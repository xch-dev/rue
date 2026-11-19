use std::{borrow::Cow, cell::RefCell, collections::HashMap};

use id_arena::Arena;

use crate::{
    BuiltinTypes, Comparison, Inference, Type, TypeId, add_expected_bounds,
    add_lambda_var_lower_bounds, compare, pass_lower_bounds, pass_upper_bounds, resolve_inference,
    resolve_inference_with_bounds, upper_bounds,
};

/// Solves the generics of a call, given the candidates its arguments contribute as they're
/// compiled and the type expected of the call. The argument loop asks for a solution before each
/// argument, to order the lambdas and to give the argument its expected type, and once at the end
/// for the mappings the call is checked with. Every rule for how the expected type is used lives
/// here, so that the compiler only decides the order the arguments are compiled in.
#[derive(Debug)]
pub struct CallSolver {
    /// The candidates inferred from the type expected of the call, if it has one.
    expected: Option<Inference>,
    /// What the expected type infers on its own, for the generics that nothing else constrains,
    /// such as `T` in `let value: Int = unchecked_cast(nil);`.
    fallback: HashMap<TypeId, TypeId>,
    /// The last inference that `CallSolver::instantiate` was given for each value of its `expected`
    /// flag, with what it gave. The argument loop solves the same inference up to three times
    /// before each argument, and a nested call in an argument solves it again, so each one is
    /// only instantiated once.
    instantiated: RefCell<[Option<(Inference, Inference)>; 2]>,
}

impl CallSolver {
    /// A solver for a call with the candidates inferred from the type expected of it, if it has
    /// one. Candidates that can't help, such as those that reference an enclosing call's generics,
    /// should already be left out of them.
    pub fn new(
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
        expected: Option<Inference>,
    ) -> Self {
        let fallback = expected
            .as_ref()
            .map(|expected| resolve_inference(arena, builtins, expected))
            .unwrap_or_default();

        Self {
            expected,
            fallback,
            instantiated: RefCell::default(),
        }
    }

    /// The solver with the type expected of the call only used for the generics that nothing else
    /// constrains (`CallSolver::fallback`), not as bounds for the others.
    #[must_use]
    pub fn fallback_only(mut self) -> Self {
        self.expected = None;
        self
    }

    /// What the arguments compiled so far infer on their own, without the type expected of the
    /// call. This decides the order lambdas are compiled in, which #265 kept independent of the
    /// expected type.
    pub fn solve_arguments(
        &self,
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
        infer: &Inference,
    ) -> HashMap<TypeId, TypeId> {
        let instantiated = self.instantiate(arena, builtins, infer, false);
        resolve_inference(arena, builtins, &instantiated)
    }

    /// The mappings that the next argument's expected type is substituted with.
    ///
    /// A generic that the arguments compiled so far map (`inferred`, from
    /// `CallSolver::solve_arguments`) takes what it's inferred as with the bounds that the type
    /// expected of the call implies added. So `T` in `let x: 5 = either(5, either(5, 5));` is `5` for
    /// the inner call, not `5` widened to `Int` (#353), and a generic in a cycle that's only mapped
    /// to another generic takes the bound too (#336).
    ///
    /// A generic they don't map falls back to what the expected type infers on its own, and failing
    /// that, takes what it's inferred as with the expected bounds passed through the others, such as
    /// `A` in `let x: 5 = map(identity, map(...));` (#331).
    ///
    /// Without `bound_mapped`, a generic the arguments map keeps what they infer, without the
    /// expected bounds. That's for an argument with a lambda in it that something can still add to
    /// the generics of: a lambda's parameter types are fixed when it's compiled, and a narrower
    /// type there, such as `7` where the arguments infer `Int`, can stop a later addition, since
    /// parameters are contravariant: `fn(x: 7) -> 5` isn't a `fn(a: 7 | 5) -> 7 | 5`.
    pub fn argument_mappings(
        &self,
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
        infer: &Inference,
        inferred: &HashMap<TypeId, TypeId>,
        bound_mapped: bool,
    ) -> HashMap<TypeId, TypeId> {
        let instantiated = self.instantiate(arena, builtins, infer, true);
        let infer: &Inference = &instantiated;
        let bounded = match &self.expected {
            Some(expected) => {
                let mut bounded = infer.clone();
                add_expected_bounds(arena, builtins, &mut bounded, expected);
                resolve_inference(arena, builtins, &bounded)
            }
            None => HashMap::new(),
        };

        let mut mappings = HashMap::new();

        for &generic in infer.generics.keys() {
            let ty = if let Some(&ty) = inferred.get(&generic) {
                if bound_mapped {
                    bounded.get(&generic).copied().unwrap_or(ty)
                } else {
                    ty
                }
            } else if let Some(&ty) = self.fallback.get(&generic) {
                ty
            } else if let Some(&ty) = bounded.get(&generic) {
                ty
            } else {
                continue;
            };

            mappings.insert(generic, ty);
        }

        mappings
    }

    /// Replaces each of the mappings that the type expected of the call bounds on its own (see
    /// `CallSolver::new`'s fallback) with that bound, where the mapping is assignable to it. This
    /// is for an argument that's a call, so it's compiled against what the expected type allows,
    /// such as `7 | 8` for `T` in `let v: 7 | 8 = either(7, id(8));`, rather than what the
    /// arguments so far narrowed it to (`7`), which the call's own value can't widen.
    pub fn widen_to_expected(
        &self,
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
        mappings: &mut HashMap<TypeId, TypeId>,
    ) {
        for (generic, ty) in mappings.iter_mut() {
            if let Some(&bound) = self.fallback.get(generic)
                && bound != *ty
                && compare(arena, builtins, *ty, bound) == Comparison::Assign
            {
                *ty = bound;
            }
        }
    }

    /// The mappings the call is checked with, once every argument has been compiled. The expected
    /// type's bounds are added to the generics whose lower bounds satisfy them, a generic that the
    /// arguments don't constrain falls back to the expected type, and one that still isn't
    /// inferred takes the upper bounds of the generics it's inferred through, so that
    /// `feed(identity, inc)` infers `A` from `inc` through `B` (#285). It takes the arguments'
    /// inference, since the expected type's bounds are added to it.
    pub fn solve(
        &self,
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
        mut infer: Inference,
    ) -> HashMap<TypeId, TypeId> {
        if infer.has_instances() {
            infer = self.instantiate(arena, builtins, &infer, true).into_owned();
        }

        if let Some(expected) = &self.expected {
            add_expected_bounds(arena, builtins, &mut infer, expected);
        }

        // After the expected type's bounds have reached the vars of untyped lambda parameters
        add_lambda_var_lower_bounds(&mut infer);

        resolve_inference_with_bounds(arena, builtins, &infer, &self.fallback)
    }

    /// The inference with the vars for passed generic functions' instances replaced with what
    /// they're instantiated as (see `InstancePolicy::Joint`), after the bounds that the rest of
    /// it implies for them are passed on to them: the upper bounds of the generics they flow into,
    /// such as `B ≤ (Int, Int)` with `B ≥ (T, T)`, including the type expected of the call if
    /// `expected` is set, and the lower bounds of the generics that flow into them, such as
    /// `A ≥ (1, nil)` with `A ≤ (T, U)`. Only the vars take these bounds, so the rest is solved as
    /// it would be if they had been instantiated right away.
    fn instantiate<'a>(
        &self,
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
        infer: &'a Inference,
        expected: bool,
    ) -> Cow<'a, Inference> {
        if !infer.has_instances() {
            return Cow::Borrowed(infer);
        }

        // Without an expected type, both values of the flag give the same result
        let expected = expected && self.expected.is_some();

        if let Some((input, output)) = &self.instantiated.borrow()[usize::from(expected)]
            && input.is_identical(infer)
        {
            return Cow::Owned(output.clone());
        }

        let mut bounded = infer.clone();

        if expected && let Some(expected) = &self.expected {
            add_expected_bounds(arena, builtins, &mut bounded, expected);
        }

        let existing = upper_bounds(&bounded);
        pass_upper_bounds(arena, builtins, &mut bounded, Vec::new(), existing, |_| {
            true
        });
        pass_lower_bounds(arena, builtins, &mut bounded, |_| true);

        let mut instantiated = infer.clone();
        instantiated.take_instance_bounds(&bounded);
        instantiated.resolve_instances(arena, builtins);

        let cached = &mut self.instantiated.borrow_mut()[usize::from(expected)];
        let (_, output) = cached.insert((infer.clone(), instantiated));

        Cow::Owned(output.clone())
    }
}
