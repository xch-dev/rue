use std::time::{Duration, Instant};

use id_arena::{Arena, Id};

use crate::{
    check_type, compare_type, debug_type, difference_type, replace_type, simplify_check,
    stringify_type, substitute_type, Alias, Callable, Check, CheckError, Comparison,
    ComparisonContext, HashMap, HashSet, StandardTypes, Type, TypePath,
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
        let any = arena.alloc(Type::Any);
        let bytes = arena.alloc(Type::Bytes);
        let bytes32 = arena.alloc(Type::Bytes32);
        let public_key = arena.alloc(Type::PublicKey);
        let int = arena.alloc(Type::Int);
        let true_bool = arena.alloc(Type::True);
        let false_bool = arena.alloc(Type::False);
        let nil = arena.alloc(Type::Nil);
        let bool = arena.alloc(Type::Union(vec![false_bool, true_bool]));

        let generic_list_item = arena.alloc(Type::Generic);
        let inner = arena.alloc(Type::Unknown);
        let unmapped_list = arena.alloc(Type::Unknown);
        arena[unmapped_list] = Type::Alias(Alias {
            original_type_id: unmapped_list,
            type_id: inner,
            generic_types: vec![generic_list_item],
        });
        let pair = arena.alloc(Type::Pair(generic_list_item, unmapped_list));
        arena[inner] = Type::Union(vec![pair, nil]);

        let mut names = HashMap::new();
        names.insert(never, "Never".to_string());
        names.insert(any, "Any".to_string());
        names.insert(bytes, "Bytes".to_string());
        names.insert(bytes32, "Bytes32".to_string());
        names.insert(public_key, "PublicKey".to_string());
        names.insert(int, "Int".to_string());
        names.insert(bool, "Bool".to_string());
        names.insert(true_bool, "True".to_string());
        names.insert(false_bool, "False".to_string());
        names.insert(nil, "Nil".to_string());
        names.insert(unmapped_list, "List".to_string());
        names.insert(generic_list_item, "{item}".to_string());

        Self {
            arena,
            types: StandardTypes {
                unknown,
                never,
                any,
                unmapped_list,
                generic_list_item,
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

    pub fn get_recursive(&self, type_id: TypeId) -> &Type {
        match self.get(type_id) {
            Type::Alias(ty) => self.get_recursive(ty.type_id),
            Type::Struct(ty) => self.get_recursive(ty.type_id),
            Type::Enum(ty) => self.get_recursive(ty.type_id),
            Type::Variant(ty) => self.get_recursive(ty.type_id),
            ty => ty,
        }
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

    pub fn get_callable(&self, type_id: TypeId) -> Option<&Callable> {
        match self.get(type_id) {
            Type::Callable(callable) => Some(callable),
            _ => None,
        }
    }

    pub fn get_callable_recursive(&mut self, type_id: TypeId) -> Option<&Callable> {
        match self.get_recursive(type_id) {
            Type::Callable(callable) => Some(callable),
            _ => None,
        }
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

    pub fn debug(&self, type_id: TypeId) -> String {
        debug_type(self, "", type_id, 0, &mut HashSet::new())
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
        let start = Instant::now();

        let result = compare_type(
            self,
            lhs,
            rhs,
            &mut ComparisonContext {
                visited: HashSet::new(),
                lhs_substitutions: Vec::new(),
                rhs_substitutions: Vec::new(),
                inferred: substitution_stack,
                infer_generics,
            },
        );
        let duration = start.elapsed();
        if duration > Duration::from_millis(1) {
            println!(
                "\n\n\n{duration:?} between {} => {}",
                self.stringify(lhs),
                self.stringify(rhs)
            );
            println!("LHS {}", self.debug(lhs));
            println!("RHS {}", self.debug(rhs));
        }
        result
    }

    pub fn substitute(
        &mut self,
        type_id: TypeId,
        substitutions: HashMap<TypeId, TypeId>,
    ) -> TypeId {
        substitute_type(self, type_id, &mut vec![substitutions])
    }

    pub fn check(&mut self, lhs: TypeId, rhs: TypeId) -> Result<Check, CheckError> {
        check_type(self, lhs, rhs, &mut HashSet::new()).map(simplify_check)
    }

    pub fn difference(&mut self, lhs: TypeId, rhs: TypeId) -> TypeId {
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
