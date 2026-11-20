use std::{borrow::Cow, cell::RefCell, collections::HashMap};

use id_arena::Arena;

use crate::{
    BuiltinTypes, Comparison, Inference, Type, TypeId, add_expected_bounds,
    add_lambda_var_lower_bounds, compare, pass_lower_bounds, pass_upper_bounds, resolve_inference,
    resolve_inference_with_bounds, substitute_with_mappings, upper_bounds,
};

use super::CandidateKind;

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
    /// such as `T` in `let value: Int = unchecked_cast(nil);`, or what a narrower expected type
    /// does, if the solver has one (see `CallSolver::with_narrower_expected`).
    fallback: HashMap<TypeId, TypeId>,
    /// What the expected type infers on its own, always, which `CallSolver::widen_to_expected`
    /// widens an argument's mappings to.
    expected_bounds: HashMap<TypeId, TypeId>,
    /// The candidates inferred from a narrower type expected of the call, which bound the generics
    /// that nothing but the expected type constrains in place of it (see
    /// `CallSolver::with_narrower_expected`).
    narrower: Option<Inference>,
    /// The last inference that `CallSolver::instantiate` was given for each value of its `expected`
    /// flag, with what it gave. The argument loop solves the same inference up to three times
    /// before each argument, and a nested call in an argument solves it again, so each one is
    /// only instantiated once.
    instantiated: RefCell<[Option<(Inference, Inference)>; 2]>,
    /// Whether the call has to be assignable to the type expected of it, rather than only taking
    /// it as a hint, so that `CallSolver::solve` can narrow the generics to satisfy it (see
    /// `CallSolver::requiring_expected`).
    required: bool,
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
            expected_bounds: fallback.clone(),
            fallback,
            narrower: None,
            instantiated: RefCell::default(),
            required: false,
        }
    }

    /// The solver with the generics that nothing but the expected type constrains bounded by what
    /// `narrower`, a narrower type expected of the call, infers, and falling back to it, rather
    /// than to the expected type, which still bounds the others. That's for a call whose expected
    /// type was widened to its enclosing call's (see `CallSolver::widen_to_expected`), where
    /// `narrower` is the one it would have had.
    #[must_use]
    pub fn with_narrower_expected(
        mut self,
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
        narrower: &Inference,
    ) -> Self {
        self.fallback = resolve_inference(arena, builtins, narrower);
        self.narrower = Some(narrower.clone());
        self
    }

    /// The solver for a call that has to be assignable to the type expected of it, as that type
    /// is (not one an enclosing call can still widen). Where the generics resolved as usual break
    /// a bound the expected type gives them outside of any union member, so that the call can't
    /// be, `CallSolver::solve` leaves out the candidates of union members that another member's
    /// implied candidates make unneeded, and takes that if it satisfies them (#362, §5, question
    /// 2). For example, `let result: 5 = first(value);` with
    /// `fn first<T>(value: T | (T, T)) -> T` and a `5 | (5, 5)`, where `T` would be `Int | (5, 5)`.
    /// Only then, so that a call that's accepted as usual is solved as it always was: a narrower
    /// generic could reject what follows, such as a type guard on the call's value.
    #[must_use]
    pub fn requiring_expected(mut self) -> Self {
        self.required = true;
        self
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
    /// `expected_bounds`) with that bound, where the mapping is assignable to it. This
    /// is for an argument that's a call, so it's compiled against what the expected type allows,
    /// such as `7 | 8` for `T` in `let v: 7 | 8 = either(7, id(8));`, rather than what the
    /// arguments so far narrowed it to (`7`), which the call's own value can't widen. A generic
    /// that the arguments bound from above (`infer`) keeps its mapping.
    pub fn widen_to_expected(
        &self,
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
        infer: &Inference,
        mappings: &mut HashMap<TypeId, TypeId>,
    ) {
        for (generic, ty) in mappings.iter_mut() {
            // A generic that an argument bounds from above, such as one in a parameter type, or
            // in both positions, can't be widened past what that argument fixes
            if infer
                .candidates(*generic)
                .is_some_and(|candidates| !candidates.upper.is_empty())
            {
                continue;
            }

            if let Some(&bound) = self.expected_bounds.get(generic)
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
        infer: Inference,
    ) -> HashMap<TypeId, TypeId> {
        if !self.required {
            return self.solve_with(arena, builtins, infer);
        }

        let mappings = self.solve_with(arena, builtins, infer.clone());

        if self.satisfies_expected(arena, builtins, &mappings) {
            return mappings;
        }

        // Only what satisfies the expected type is taken, so a call that's rejected either way
        // keeps its diagnostics (see `CallSolver::requiring_expected`)
        let mut narrow = infer;
        narrow.narrow = true;
        let narrowed = self.solve_with(arena, builtins, narrow);

        if narrowed != mappings && self.satisfies_expected(arena, builtins, &narrowed) {
            narrowed
        } else {
            mappings
        }
    }

    /// Like `CallSolver::solve`, without narrowing the generics to satisfy the expected type.
    fn solve_with(
        &self,
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
        mut infer: Inference,
    ) -> HashMap<TypeId, TypeId> {
        if infer.has_instances() {
            infer = self.instantiate(arena, builtins, &infer, true).into_owned();
        }

        // With a narrower expected type, the generics that nothing but the expected type constrains
        // are bounded by it rather than by the expected type, as they would have been before the
        // expected type was widened, and the generics bounded through them follow (see
        // `CallSolver::with_narrower_expected`)
        if let Some(expected) = &self.expected {
            if let Some(narrower) = &self.narrower {
                // A generic only takes the wide bounds where its arguments give it a lower bound
                // that the narrower type doesn't hold, such as `8` where it's `7`
                let wide_generics: Vec<TypeId> = infer
                    .generics
                    .iter()
                    .filter(|(generic, candidates)| {
                        !candidates.lower.is_empty()
                            && self.fallback.get(*generic).is_none_or(|&narrow| {
                                candidates.lower.iter().any(|&lower| {
                                    compare(arena, builtins, lower, narrow) != Comparison::Assign
                                })
                            })
                    })
                    .map(|(&generic, _)| generic)
                    .collect();
                let mut wide = expected.clone();
                wide.retain_generics(|generic| wide_generics.contains(&generic));
                let mut narrow = narrower.clone();
                narrow.retain_generics(|generic| !wide_generics.contains(&generic));
                add_expected_bounds(arena, builtins, &mut infer, &wide);
                add_expected_bounds(arena, builtins, &mut infer, &narrow);
            } else {
                add_expected_bounds(arena, builtins, &mut infer, expected);
            }
        }

        // After the expected type's bounds have reached the vars of untyped lambda parameters
        add_lambda_var_lower_bounds(&mut infer);

        resolve_inference_with_bounds(arena, builtins, &infer, &self.fallback)
    }

    /// Whether the mappings satisfy every bound that the type expected of the call gives a
    /// generic outside of any union member, which the call's type has to for it to be assignable
    /// to the expected type.
    fn satisfies_expected(
        &self,
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
        mappings: &HashMap<TypeId, TypeId>,
    ) -> bool {
        let Some(expected) = &self.expected else {
            return true;
        };

        expected
            .unconditional()
            .into_iter()
            .all(|(generic, kind, ty)| {
                let Some(&mapped) = mappings.get(&generic) else {
                    return true;
                };

                let bound = substitute_with_mappings(arena, ty, mappings);

                match kind {
                    CandidateKind::Lower => {
                        compare(arena, builtins, bound, mapped) == Comparison::Assign
                    }
                    CandidateKind::Upper => {
                        compare(arena, builtins, mapped, bound) == Comparison::Assign
                    }
                    CandidateKind::Held => true,
                }
            })
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
