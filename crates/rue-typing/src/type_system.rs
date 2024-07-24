use std::collections::{HashMap, HashSet};

use id_arena::{Arena, Id};

use crate::{
    check_type, compare_type, difference_type, replace_type, simplify_check, stringify_type,
    substitute_type, Alias, Check, CheckError, Comparison, ComparisonContext, Lazy, Semantics,
    StandardTypes, Type, TypePath,
};

pub type TypeId = Id<Type>;

#[derive(Debug, Clone)]
pub struct TypeSystem {
    arena: Arena<Type>,
    types: StandardTypes,
    names: HashMap<TypeId, String>,
}

impl Default for TypeSystem {
    fn default() -> Self {
        let mut arena = Arena::new();

        let unknown = arena.alloc(Type::Unknown);
        let never = arena.alloc(Type::Never);
        let atom = arena.alloc(Type::Atom);
        let bytes = arena.alloc(Type::Bytes);
        let bytes32 = arena.alloc(Type::Bytes32);
        let public_key = arena.alloc(Type::PublicKey);
        let int = arena.alloc(Type::Int);
        let true_bool = arena.alloc(Type::True);
        let false_bool = arena.alloc(Type::False);
        let nil = arena.alloc(Type::Nil);

        let bool = arena.alloc(Type::Union(vec![false_bool, true_bool]));

        let any = arena.alloc(Type::Unknown);
        let pair = arena.alloc(Type::Pair(any, any));
        arena[any] = Type::Union(vec![atom, pair]);

        let generic_list_item = arena.alloc(Type::Generic);
        let inner = arena.alloc(Type::Unknown);
        let unmapped_list = arena.alloc(Type::Alias(Alias {
            original_type_id: None,
            type_id: inner,
            generic_types: vec![generic_list_item],
        }));
        let pair = arena.alloc(Type::Pair(generic_list_item, unmapped_list));
        arena[inner] = Type::Union(vec![pair, nil]);

        let mut names = HashMap::new();
        names.insert(never, "Never".to_string());
        names.insert(atom, "Atom".to_string());
        names.insert(bytes, "Bytes".to_string());
        names.insert(bytes32, "Bytes32".to_string());
        names.insert(public_key, "PublicKey".to_string());
        names.insert(int, "Int".to_string());
        names.insert(bool, "Bool".to_string());
        names.insert(true_bool, "True".to_string());
        names.insert(false_bool, "False".to_string());
        names.insert(nil, "Nil".to_string());
        names.insert(any, "Any".to_string());
        names.insert(unmapped_list, "List".to_string());

        Self {
            arena,
            types: StandardTypes {
                unknown,
                never,
                any,
                unmapped_list,
                generic_list_item,
                atom,
                bytes,
                bytes32,
                public_key,
                int,
                bool,
                true_bool,
                false_bool,
                nil,
            },
            names,
        }
    }
}

impl TypeSystem {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn std(&self) -> StandardTypes {
        self.types
    }

    pub fn alloc(&mut self, ty: Type) -> TypeId {
        self.arena.alloc(ty)
    }

    pub fn get_raw(&self, type_id: TypeId) -> &Type {
        &self.arena[type_id]
    }

    pub fn get_raw_mut(&mut self, type_id: TypeId) -> &mut Type {
        &mut self.arena[type_id]
    }

    pub fn get(&self, type_id: TypeId) -> &Type {
        match &self.arena[type_id] {
            Type::Ref(type_id) => self.get(*type_id),
            ty => ty,
        }
    }

    pub fn get_mut(&mut self, type_id: TypeId) -> &mut Type {
        match &self.arena[type_id] {
            Type::Ref(type_id) => self.get_mut(*type_id),
            _ => &mut self.arena[type_id],
        }
    }

    pub fn get_pair(&self, type_id: TypeId) -> Option<(TypeId, TypeId)> {
        match self.get(type_id) {
            Type::Pair(first, rest) => Some((*first, *rest)),
            _ => None,
        }
    }

    pub fn get_union(&self, type_id: TypeId) -> Option<&[TypeId]> {
        match self.get(type_id) {
            Type::Union(types) => Some(types),
            _ => None,
        }
    }

    pub fn alloc_list(&mut self, type_id: TypeId) -> TypeId {
        let mut substitutions = HashMap::new();
        substitutions.insert(self.types.generic_list_item, type_id);
        self.alloc(Type::Lazy(Lazy {
            type_id: self.types.unmapped_list,
            substitutions,
        }))
    }

    pub fn stringify_named(&self, type_id: TypeId, mut names: HashMap<TypeId, String>) -> String {
        for (id, name) in &self.names {
            names.entry(*id).or_insert_with(|| name.clone());
        }
        stringify_type(self, type_id, &names, &mut HashSet::new())
    }

    pub fn stringify(&self, type_id: TypeId) -> String {
        self.stringify_named(type_id, HashMap::new())
    }

    pub fn compare(&self, lhs: TypeId, rhs: TypeId) -> Comparison {
        self.compare_with_generics(lhs, rhs, &mut Vec::new(), false)
    }

    pub fn compare_with_generics(
        &self,
        lhs: TypeId,
        rhs: TypeId,
        substitution_stack: &mut Vec<HashMap<TypeId, TypeId>>,
        infer_generics: bool,
    ) -> Comparison {
        let generic_stack_frame = if infer_generics {
            Some(substitution_stack.len() - 1)
        } else {
            None
        };
        let initial_substitution_length = substitution_stack.len();

        compare_type(
            self,
            lhs,
            rhs,
            &mut ComparisonContext {
                visited: HashSet::new(),
                substitution_stack,
                initial_substitution_length,
                generic_stack_frame,
            },
        )
    }

    pub fn substitute(
        &mut self,
        type_id: TypeId,
        mut substitutions: HashMap<TypeId, TypeId>,
        semantics: Semantics,
    ) -> TypeId {
        substitute_type(self, type_id, &mut substitutions, semantics)
    }

    pub fn check(&mut self, lhs: TypeId, rhs: TypeId) -> Result<Check, CheckError> {
        check_type(self, lhs, rhs, &mut HashSet::new()).map(simplify_check)
    }

    pub fn difference(&mut self, lhs: TypeId, rhs: TypeId) -> TypeId {
        let lhs = self.substitute(lhs, HashMap::new(), Semantics::Preserve);
        let rhs = self.substitute(rhs, HashMap::new(), Semantics::Preserve);
        difference_type(self, lhs, rhs, &mut HashSet::new())
    }

    pub fn replace(
        &mut self,
        type_id: TypeId,
        replace_type_id: TypeId,
        path: &[TypePath],
    ) -> TypeId {
        replace_type(self, type_id, replace_type_id, path)
    }
}
