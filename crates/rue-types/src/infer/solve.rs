use std::collections::HashMap;

use id_arena::Arena;

use crate::{
    BuiltinTypes, Inference, Type, TypeId, add_expected_bounds, resolve_inference,
    resolve_inference_with_bounds,
};

/// Solves the generics of a call, given the candidates its arguments contribute as they're
/// compiled and the type expected of the call. The argument loop asks for a solution before each
/// argument, to order the lambdas and to give the argument its expected type, and once at the end
/// for the mappings the call is checked with. Every rule for how the expected type is used lives
/// here, so that the compiler only decides the order the arguments are compiled in.
#[derive(Debug, Clone)]
pub struct CallSolver {
    /// The candidates inferred from the type expected of the call, if it has one.
    expected: Option<Inference>,
    /// What the expected type infers on its own, for the generics that nothing else constrains,
    /// such as `T` in `let value: Int = unchecked_cast(nil);`.
    fallback: HashMap<TypeId, TypeId>,
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

        Self { expected, fallback }
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
        resolve_inference(arena, builtins, infer)
    }

    /// The mappings that the next argument's expected type is substituted with: what the arguments
    /// compiled so far infer (`inferred`, from `CallSolver::solve_arguments`), then the expected
    /// type's fallback for the generics they don't map. A generic that's still unmapped takes what
    /// it's inferred as with the expected type's bounds passed through the others, such as `A` in
    /// `let x: 5 = map(identity, map(...));` (#331). Only unmapped generics are filled, so that the
    /// argument expected types of generics the arguments map don't change.
    pub fn argument_mappings(
        &self,
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
        infer: &Inference,
        inferred: HashMap<TypeId, TypeId>,
    ) -> HashMap<TypeId, TypeId> {
        let mut mappings = inferred;

        for (&generic, &ty) in &self.fallback {
            mappings.entry(generic).or_insert(ty);
        }

        if let Some(expected) = &self.expected
            && infer
                .generics
                .keys()
                .any(|generic| !mappings.contains_key(generic))
        {
            let mut bounded = infer.clone();
            add_expected_bounds(arena, builtins, &mut bounded, expected);

            for (generic, ty) in resolve_inference(arena, builtins, &bounded) {
                mappings.entry(generic).or_insert(ty);
            }
        }

        mappings
    }

    /// The mappings the call is checked with, once every argument has been compiled. The expected
    /// type's bounds are added to the generics whose lower bounds satisfy them, a generic that the
    /// arguments don't constrain falls back to the expected type, and one that still isn't
    /// inferred takes the upper bounds of the generics it's inferred through, so that
    /// `feed(identity, inc)` infers `A` from `inc` through `B` (#285).
    pub fn solve(
        &self,
        arena: &mut Arena<Type>,
        builtins: &BuiltinTypes,
        infer: &mut Inference,
    ) -> HashMap<TypeId, TypeId> {
        if let Some(expected) = &self.expected {
            add_expected_bounds(arena, builtins, infer, expected);
        }

        resolve_inference_with_bounds(arena, builtins, infer, &self.fallback)
    }
}
