use std::collections::HashSet;
use std::mem;

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};

use rue_diagnostic::Name;

use crate::{
    Alias, Atom, AtomRestriction, AtomSemantic, DISCRIMINANT_FIELD, FieldShape, Struct, Type,
    TypeId, grow_stack,
    hash::{FxHashMap, FxIndexSet},
    step, substitute,
};

pub fn stringify(arena: &mut Arena<Type>, id: TypeId) -> String {
    let id = substitute(arena, id);
    stringify_substituted(arena, id)
}

/// Like [`stringify`], for a type that's already substituted, such as the ones the operations on
/// types log.
pub(crate) fn stringify_substituted(arena: &Arena<Type>, id: TypeId) -> String {
    Stringifier::new(arena).write(id)
}

/// What's written for a type, or a part of one, that couldn't be compiled. An error has been
/// reported for whatever it was, so a diagnostic that would show this is left out instead.
pub const UNRESOLVED: &str = "{unresolved}";

/// What's written in place of the parts of a type that [`stringify_bounded`] leaves out.
pub const ELIDED: &str = "...";

/// Like [`stringify`], but only the first `parts` parts of the type are written (each type that it's
/// made of is one), and the rest are shown as `...`. A type can be far larger written out than it
/// is in memory, where its parts are shared, so this bounds the time and the length of the result
/// for types that are shown as they're inferred, such as by the language server.
///
/// Returns what was written, and whether anything was left out of it.
pub fn stringify_bounded(arena: &mut Arena<Type>, id: TypeId, parts: usize) -> (String, bool) {
    let id = substitute(arena, id);

    let mut stringifier = Stringifier::new(arena);
    stringifier.budget = Some(parts);

    let result = stringifier.write(id);

    (result, stringifier.cut)
}

/// Like [`stringify`], but every member of a union is written, including the ones that are the
/// same type as another member or that another member includes, and `true | false` as it is.
/// This is the type as inference built it, which is what a test of that is about, rather than
/// how it's displayed.
pub fn stringify_exact(arena: &mut Arena<Type>, id: TypeId) -> String {
    let id = substitute(arena, id);

    let mut stringifier = Stringifier::new(arena);
    stringifier.exact = true;
    stringifier.write(id)
}

/// The names of a union's members as they're displayed, given each member and its name: without
/// the members that are the same type as an earlier one or that another member includes, and with
/// `true` and `false` shown together as `Bool`. This is for a union that's written member by
/// member, as [`stringify`] does for the unions in a type.
pub fn distinct_member_names(arena: &Arena<Type>, members: Vec<(TypeId, String)>) -> Vec<String> {
    let members = members
        .into_iter()
        .map(|(id, name)| Member {
            id,
            name,
            cut: false,
        })
        .collect();

    distinct_members(arena, members)
}

/// Writes a type, and the types it's made of, as a string.
struct Stringifier<'a> {
    arena: &'a Arena<Type>,
    /// The types being written, and whether each one has been reached again from inside itself
    /// (in which case it's written with a label that the inner occurrences refer to).
    stack: IndexMap<TypeId, bool>,
    /// How many more parts of the type are written before the rest are left out, if that's
    /// limited (see [`stringify_bounded`]).
    budget: Option<usize>,
    /// Whether any part of the type has been left out.
    cut: bool,
    /// Whether every member of each union is written (see [`stringify_exact`]).
    exact: bool,
}

impl<'a> Stringifier<'a> {
    fn new(arena: &'a Arena<Type>) -> Self {
        Self {
            arena,
            stack: IndexMap::new(),
            budget: None,
            cut: false,
            exact: false,
        }
    }

    /// Counts a part of a type against the budget, and returns whether there was any of it left.
    fn spend(&mut self) -> bool {
        match self.budget {
            None => true,
            Some(0) => {
                self.cut = true;
                false
            }
            Some(parts) => {
                self.budget = Some(parts - 1);
                true
            }
        }
    }

    fn write(&mut self, id: TypeId) -> String {
        grow_stack(|| self.write_inner(id))
    }

    fn write_inner(&mut self, id: TypeId) -> String {
        step();

        if !self.spend() {
            return ELIDED.to_string();
        }

        let arena = self.arena;
        let len = self.stack.len();

        if self.stack.contains_key(&id) {
            self.stack.insert(id, true);
            return format!("<{}>", self.stack.get_index_of(&id).unwrap());
        }

        self.stack.insert(id, false);

        let result = match &arena[id] {
            &Type::Ref(id) => self.write(id),
            Type::Unresolved => UNRESOLVED.to_string(),
            Type::Generic(generic) => {
                if let Some(name) = &generic.name {
                    name.text().to_string()
                } else {
                    format!("{{generic {}}}", id.index())
                }
            }
            Type::Never => "Never".to_string(),
            Type::Any => "Any".to_string(),
            Type::Atom(atom) => atom.to_string(),
            Type::Pair(pair) => {
                let first = self.write(pair.first);
                let rest = self.write(pair.rest);
                format!("({first}, {rest})")
            }
            Type::Struct(ty) => {
                if let Some(name) = &ty.name {
                    if ty.narrowed {
                        self.narrowed_struct(name.text(), ty)
                    } else {
                        self.with_generics(name.text(), &ty.generics)
                    }
                } else {
                    self.write(ty.inner)
                }
            }
            Type::Alias(alias) => {
                if let Some(name) = &alias.name {
                    self.with_generics(name.text(), &alias.generics)
                } else {
                    self.write(alias.inner)
                }
            }
            Type::Apply(apply) => {
                // Order the arguments by the generic parameters they're applied to, if known.
                let (inner, parameters) = match &arena[apply.inner] {
                    Type::Alias(Alias {
                        name: Some(name),
                        generics,
                        ..
                    })
                    | Type::Struct(Struct {
                        name: Some(name),
                        generics,
                        ..
                    }) => (name.text().to_string(), generics.clone()),
                    _ => (
                        self.write(apply.inner),
                        apply.generics.keys().copied().collect(),
                    ),
                };

                let arguments = parameters
                    .iter()
                    .map(|parameter| apply.generics.get(parameter).copied().unwrap_or(*parameter))
                    .collect::<Vec<_>>();

                self.with_generics(&inner, &arguments)
            }
            Type::Function(function) => {
                let len = function.params.len();
                let params = function
                    .params
                    .iter()
                    .enumerate()
                    .map(|(i, param)| {
                        let spread = if !function.nil_terminated && i == len - 1 {
                            "..."
                        } else {
                            ""
                        };
                        format!("{spread}{}: {}", param.name, self.write(param.ty))
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret = self.write(function.ret);
                format!(
                    "{}({params}) -> {ret}",
                    self.with_generics("fn", &function.generics)
                )
            }
            // A member is shown once however many times it's in the union. References to the same
            // struct are substituted into distinct types, and inference collects the same type into
            // a union once for each argument that it came from, so members that are the same type
            // written out are only displayed once. So is a member that another one already
            // includes, such as the `0` in `Int | 0`, and `true | false` is displayed as `Bool`.
            // Members that are only shown the same are kept apart, since two structs can have the
            // same name
            Type::Union(_) => {
                // Each member takes at least one part to write, so the members that there's no
                // budget for aren't looked at, however many there are
                let limit = self.budget.unwrap_or(usize::MAX);
                let mut members = IndexSet::new();
                let mut elided = !union_variants(arena, id, limit, &mut members);

                // Whether a member was cut short is told apart from whether anything before it was
                let cut_before = mem::take(&mut self.cut);

                let members = members
                    .into_iter()
                    .map(|id| {
                        let name = self.write(id);
                        let cut = mem::take(&mut self.cut);
                        Member { id, name, cut }
                    })
                    .collect::<Vec<_>>();

                if cut_before || members.iter().any(|member| member.cut) {
                    self.cut = true;
                }

                let mut variants = if self.exact {
                    members.into_iter().map(|member| member.name).collect()
                } else {
                    distinct_members(arena, members)
                };

                // The members that were left out are shown once, after the ones that weren't
                let written = variants.len();
                variants.retain(|name| name != ELIDED);
                elided |= variants.len() < written;

                if elided {
                    self.cut = true;
                    variants.push(ELIDED.to_string());
                }

                variants.join(" | ")
            }
        };

        let recursed = self.stack.pop().unwrap().1;

        if recursed {
            if result.starts_with('(') {
                format!("{len} @ {result}")
            } else {
                format!("{len} @ ({result})")
            }
        } else {
            result
        }
    }

    fn with_generics(&mut self, name: &str, generics: &[TypeId]) -> String {
        if generics.is_empty() {
            return name.to_string();
        }

        let generics = generics
            .iter()
            .map(|&id| self.write(id))
            .collect::<Vec<_>>()
            .join(", ");

        format!("{name}<{generics}>")
    }

    /// Renders a struct whose fields have been narrowed, e.g. `Box { value: Int }`, since its
    /// generic arguments no longer describe them.
    fn narrowed_struct(&mut self, name: &str, ty: &Struct) -> String {
        let Some(variants) = self.narrowed_fields(ty, &ty.shape(), ty.inner) else {
            return self.with_generics(name, &ty.generics);
        };

        variants
            .into_iter()
            .map(|fields| {
                if fields.is_empty() {
                    name.to_string()
                } else {
                    format!("{name} {{ {} }}", fields.join(", "))
                }
            })
            .collect::<IndexSet<_>>()
            .into_iter()
            .collect::<Vec<_>>()
            .join(" | ")
    }

    /// Renders the fields of a struct in the part of its shape, where `current` is the type of
    /// that part of the struct. If the fields are correlated, each variant is rendered separately.
    /// Returns `None` if the type doesn't have the structure of the fields.
    fn narrowed_fields(
        &mut self,
        ty: &Struct,
        shape: &FieldShape,
        current: TypeId,
    ) -> Option<Vec<Vec<String>>> {
        grow_stack(|| self.narrowed_fields_inner(ty, shape, current))
    }

    fn narrowed_fields_inner(
        &mut self,
        ty: &Struct,
        shape: &FieldShape,
        mut current: TypeId,
    ) -> Option<Vec<Vec<String>>> {
        let (first_shape, rest_shape) = match shape {
            FieldShape::Nil => return Some(vec![vec![]]),
            FieldShape::Field(index) => {
                let field = &ty.fields[*index];

                // The discriminant of an enum variant is part of the variant's name, so it isn't
                // shown
                if field == DISCRIMINANT_FIELD {
                    return Some(vec![vec![]]);
                }

                // The last field of a list that isn't nil terminated is the rest of it
                let spread = if !ty.nil_terminated && *index == ty.fields.len() - 1 {
                    "..."
                } else {
                    ""
                };

                let value = self.write(current);
                return Some(vec![vec![format!("{spread}{field}: {value}")]]);
            }
            FieldShape::Pair(first, rest) => (first, rest),
        };

        let arena = self.arena;
        current = unwrap_refs(arena, current);

        match &arena[current] {
            Type::Pair(pair) => {
                let (pair_first, pair_rest) = (pair.first, pair.rest);
                let firsts = self.narrowed_fields(ty, first_shape, pair_first)?;
                let rests = self.narrowed_fields(ty, rest_shape, pair_rest)?;

                let mut variants = Vec::new();

                for first in &firsts {
                    for rest in &rests {
                        variants.push(first.iter().chain(rest).cloned().collect());
                    }
                }

                Some(variants)
            }
            Type::Union(union) => {
                let mut variants = Vec::new();

                for &id in &union.types {
                    variants.extend(self.narrowed_fields(ty, shape, id)?);
                }

                Some(variants)
            }
            _ => None,
        }
    }
}

/// Adds the variants of a union, including those of directly nested unions, each of them once,
/// until there are as many as the limit. Returns whether that was all of them, which it can say
/// it wasn't when the only ones left are written the same as ones that were added.
fn union_variants(
    arena: &Arena<Type>,
    id: TypeId,
    limit: usize,
    variants: &mut IndexSet<TypeId>,
) -> bool {
    union_variants_impl(arena, id, limit, variants, &mut HashSet::new())
}

fn union_variants_impl(
    arena: &Arena<Type>,
    id: TypeId,
    limit: usize,
    variants: &mut IndexSet<TypeId>,
    unions: &mut HashSet<TypeId>,
) -> bool {
    grow_stack(|| union_variants_inner(arena, id, limit, variants, unions))
}

fn union_variants_inner(
    arena: &Arena<Type>,
    id: TypeId,
    limit: usize,
    variants: &mut IndexSet<TypeId>,
    unions: &mut HashSet<TypeId>,
) -> bool {
    match &arena[id] {
        // A union that's nested in more than one place only adds its members once, and types
        // that share their parts can have far more places than parts
        Type::Union(_) if !unions.insert(id) => true,
        Type::Union(union) => union
            .types
            .iter()
            .all(|&id| union_variants_impl(arena, id, limit, variants, unions)),
        // A member that's there more than once is only written once
        _ if variants.contains(&id) => true,
        _ if variants.len() < limit => {
            variants.insert(id);
            true
        }
        _ => false,
    }
}

/// A member of a union as it was written.
struct Member {
    id: TypeId,
    name: String,
    /// Whether any part of it was left out, in which case it's only known by its name.
    cut: bool,
}

/// The names of a union's members as they're displayed: without the members that are the same
/// type as an earlier one or that another member includes, and with `true` and `false` shown
/// together as `Bool`.
fn distinct_members(arena: &Arena<Type>, members: Vec<Member>) -> Vec<String> {
    let mut members = members
        .into_iter()
        .map(|member| {
            let shown = if member.cut {
                Shown::Name
            } else if let Some(value) = bool_value(arena, member.id) {
                Shown::Bool(Some(value))
            } else {
                Shown::Type(unwrap_refs(arena, member.id))
            };
            (shown, member.name)
        })
        .collect::<Vec<_>>();

    // `true | false` is every `Bool`, which is shown where the first of them was
    let has = |value| {
        members
            .iter()
            .any(|(shown, _)| *shown == Shown::Bool(Some(value)))
    };

    if has(true) && has(false) {
        let mut merged = false;

        members.retain_mut(|(shown, name)| {
            if !matches!(shown, Shown::Bool(Some(_))) {
                return true;
            }

            if merged {
                return false;
            }

            merged = true;
            *shown = Shown::Bool(None);
            *name = AtomSemantic::Bool.to_string();

            true
        });
    }

    // Telling that a member is the same as another takes about as long as writing it did, so
    // the members can be compared for about as long as writing all of them took, and then some
    // for the ones that include each other
    let written = members.iter().map(|(_, name)| name.len()).sum::<usize>();
    let mut matcher = Matcher::new(MATCH_LIMIT + 2 * written);

    // A member is the same as an earlier one if it's shown the same and is the same type, which
    // is only looked into for members with the same name
    let mut kept: Vec<(Shown, String)> = Vec::new();
    let mut named: FxHashMap<String, Vec<usize>> = FxHashMap::default();

    for (shown, name) in members {
        let alike = named.entry(name.clone()).or_default();

        let repeated = !matcher.is_spent()
            && alike
                .iter()
                .any(|&earlier| shown.same_as(arena, &kept[earlier].0, &mut matcher));

        if !repeated {
            alike.push(kept.len());
            kept.push((shown, name));
        }
    }

    // A member is left out if one that's still shown includes it. They're gone through from the
    // last, so that the first of two that include each other is the one that's shown, and there's
    // always a member left: the one that the last to be left out was included in
    //
    // Only an atom that isn't a single value includes another atom, and only a pair includes
    // another pair, so those are the only members that are tried, for atoms and pairs
    let kinds = kept
        .iter()
        .map(|(shown, _)| shown.kind(arena))
        .collect::<Vec<_>>();
    let of_kind = |kind| {
        (0..kept.len())
            .filter(|&i| kinds[i] == kind)
            .collect::<Vec<_>>()
    };
    let wide_atoms = of_kind(Kind::WideAtom);
    let pairs = of_kind(Kind::Pair);
    let mut included = vec![false; kept.len()];

    for i in (0..kept.len()).rev() {
        if matcher.is_spent() {
            break;
        }

        let candidates = match kinds[i] {
            Kind::Pair => &pairs,
            Kind::Atom | Kind::WideAtom => &wide_atoms,
            Kind::Other => continue,
        };

        included[i] = candidates.iter().any(|&j| {
            i != j && !included[j] && kept[j].0.includes(arena, &kept[i].0, &mut matcher)
        });
    }

    kept.into_iter()
        .zip(included)
        .filter(|(_, included)| !included)
        .map(|((_, name), _)| name)
        .collect()
}

/// What's known about a member of a union for telling whether it's displayed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Shown {
    /// Only its name, since it wasn't written in full.
    Name,
    /// `true` or `false`, or `Bool` if the union has both.
    Bool(Option<bool>),
    Type(TypeId),
}

impl Shown {
    /// Whether two members that are shown the same are the same type.
    fn same_as(&self, arena: &Arena<Type>, other: &Self, matcher: &mut Matcher) -> bool {
        match (self, other) {
            (Self::Name, _) | (_, Self::Name) => false,
            (Self::Type(lhs), Self::Type(rhs)) => matcher.matches(arena, *lhs, *rhs, false),
            _ => self == other,
        }
    }

    /// Whether every value of the other member is a value of this one, as far as that can be told
    /// from how the types are written.
    fn includes(&self, arena: &Arena<Type>, other: &Self, matcher: &mut Matcher) -> bool {
        match (self, other) {
            (Self::Type(lhs), Self::Type(rhs)) => matcher.matches(arena, *lhs, *rhs, true),
            (Self::Type(lhs), Self::Bool(_)) => matches!(
                &arena[*lhs],
                Type::Atom(Atom {
                    semantic: AtomSemantic::Bool,
                    restriction: None,
                })
            ),
            _ => false,
        }
    }

    fn kind(&self, arena: &Arena<Type>) -> Kind {
        match self {
            Self::Name => Kind::Other,
            Self::Bool(_) => Kind::Atom,
            Self::Type(id) => match &arena[*id] {
                Type::Atom(Atom {
                    restriction: None | Some(AtomRestriction::Length(_)),
                    ..
                }) => Kind::WideAtom,
                Type::Atom(_) => Kind::Atom,
                Type::Pair(_) => Kind::Pair,
                _ => Kind::Other,
            },
        }
    }
}

/// What a member of a union is, for telling which members might include it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Kind {
    /// An atom with one value.
    Atom,
    /// An atom with more than one value, which can include other atoms.
    WideAtom,
    Pair,
    /// Anything else, which neither includes another member nor is included in one.
    Other,
}

/// Whether a type is the atom `true` or the atom `false`, and which.
fn bool_value(arena: &Arena<Type>, id: TypeId) -> Option<bool> {
    let Type::Atom(Atom {
        semantic: AtomSemantic::Bool,
        restriction: Some(AtomRestriction::Value(value)),
    }) = &arena[unwrap_refs(arena, id)]
    else {
        return None;
    };

    match value.as_ref() {
        [] => Some(false),
        [1] => Some(true),
        _ => None,
    }
}

/// The type that a reference is to.
fn unwrap_refs(arena: &Arena<Type>, mut id: TypeId) -> TypeId {
    while let Type::Ref(inner) = arena[id] {
        id = inner;
    }

    id
}

/// How many pairs of types [`Matcher`] looks at for the members of one union, beyond as many as
/// it took to write them, before it says that the rest aren't the same. That only leaves members
/// displayed that didn't need to be. It bounds the time that displaying a union takes beyond
/// writing its members, which is otherwise the square of how many of them are alike.
const MATCH_LIMIT: usize = 1 << 16;

/// Tells whether two types are the same type written out, or whether one includes the other.
struct Matcher {
    /// The pairs that are being matched, which match if nothing else in them says otherwise.
    stack: FxIndexSet<(TypeId, TypeId, bool)>,
    /// How many pairs of types and members of unions it has looked at, and how many it does.
    work: usize,
    limit: usize,
}

impl Matcher {
    fn new(limit: usize) -> Self {
        Self {
            stack: FxIndexSet::default(),
            work: 0,
            limit,
        }
    }

    /// Whether it has looked at as many pairs as it does, so nothing matches from now on.
    fn is_spent(&self) -> bool {
        self.work > self.limit
    }

    /// The members of a union, which count towards the limit like the pairs of types do.
    fn members(&mut self, arena: &Arena<Type>, id: TypeId) -> Option<IndexSet<TypeId>> {
        let mut members = IndexSet::new();
        let complete = union_variants(arena, id, self.limit, &mut members);
        self.work += members.len();
        (complete && !self.is_spent()).then_some(members)
    }

    /// Whether the types are the same: made of the same parts in the same places, where a struct
    /// is the same as another instance of its declaration with the same generic arguments.
    ///
    /// With `includes`, whether the first type includes the second instead: an atom includes the
    /// values and lengths of it, a pair includes a pair of what its parts include, and a union
    /// includes what one of its members does. This is only what can be told without comparing
    /// the types, so that it's the same wherever a type is displayed.
    fn matches(&mut self, arena: &Arena<Type>, lhs: TypeId, rhs: TypeId, includes: bool) -> bool {
        grow_stack(|| self.matches_inner(arena, lhs, rhs, includes))
    }

    fn matches_inner(
        &mut self,
        arena: &Arena<Type>,
        lhs: TypeId,
        rhs: TypeId,
        includes: bool,
    ) -> bool {
        let lhs = unwrap_refs(arena, lhs);
        let rhs = unwrap_refs(arena, rhs);

        if lhs == rhs {
            return true;
        }

        self.work += 1;

        if self.is_spent() {
            return false;
        }

        if !self.stack.insert((lhs, rhs, includes)) {
            return true;
        }

        let result = match (&arena[lhs], &arena[rhs]) {
            // A union is included if each of its members is, and includes what a member does
            (_, Type::Union(_)) if includes => self.members(arena, rhs).is_some_and(|members| {
                members
                    .iter()
                    .all(|&rhs| self.matches(arena, lhs, rhs, true))
            }),
            (Type::Union(_), _) if includes => self.members(arena, lhs).is_some_and(|members| {
                members
                    .iter()
                    .any(|&lhs| self.matches(arena, lhs, rhs, true))
            }),
            (Type::Unresolved, Type::Unresolved)
            | (Type::Never, Type::Never)
            | (Type::Any, Type::Any) => true,
            (Type::Atom(lhs), Type::Atom(rhs)) => {
                atom_shown_same(lhs, rhs) || (includes && atom_includes(lhs, rhs))
            }
            (Type::Pair(lhs), Type::Pair(rhs)) => {
                self.matches(arena, lhs.first, rhs.first, includes)
                    && self.matches(arena, lhs.rest, rhs.rest, includes)
            }
            // A struct without a name is displayed as what it's made of
            (Type::Struct(lhs), Type::Struct(rhs)) => {
                lhs.semantic == rhs.semantic
                    && lhs.narrowed == rhs.narrowed
                    && self.all_match(arena, &lhs.generics, &rhs.generics)
                    && ((!lhs.narrowed && lhs.name.is_some())
                        || self.matches(arena, lhs.inner, rhs.inner, false))
            }
            (Type::Alias(lhs), Type::Alias(rhs)) => {
                lhs.name.as_ref().map(Name::text) == rhs.name.as_ref().map(Name::text)
                    && self.all_match(arena, &lhs.generics, &rhs.generics)
                    && self.matches(arena, lhs.inner, rhs.inner, false)
            }
            (Type::Function(lhs), Type::Function(rhs)) => {
                lhs.generics == rhs.generics
                    && lhs.nil_terminated == rhs.nil_terminated
                    && lhs.params.len() == rhs.params.len()
                    && lhs.params.iter().zip(&rhs.params).all(|(lhs, rhs)| {
                        lhs.name == rhs.name && self.matches(arena, lhs.ty, rhs.ty, false)
                    })
                    && self.matches(arena, lhs.ret, rhs.ret, false)
            }
            // A union is displayed without the members that others include, so it's the same as
            // a type that it includes and that includes it, such as `Int | 0` and `Int`, or a
            // union of the same members in another order
            (Type::Union(_), _) | (_, Type::Union(_)) => {
                self.matches(arena, lhs, rhs, true) && self.matches(arena, rhs, lhs, true)
            }
            // Generics are the same if they're the same type, which has been ruled out. The types
            // that are substituted before a type is displayed are never the same
            _ => false,
        };

        self.stack.pop();

        result
    }

    fn all_match(&mut self, arena: &Arena<Type>, lhs: &[TypeId], rhs: &[TypeId]) -> bool {
        lhs.len() == rhs.len()
            && lhs
                .iter()
                .zip(rhs)
                .all(|(&lhs, &rhs)| self.matches(arena, lhs, rhs, false))
    }
}

/// Whether two atoms are displayed the same, as an `Int` and an `Int` of some length are.
fn atom_shown_same(lhs: &Atom, rhs: &Atom) -> bool {
    // A value of an `Int` or of `Bytes` is written as the value, which is only written the same
    // for the same bytes, and never the same as an atom of another kind is. This is told
    // without writing it, which takes far longer for a number
    let written_as_value = |atom: &Atom| {
        matches!(atom.semantic, AtomSemantic::Int | AtomSemantic::Bytes)
            && matches!(atom.restriction, Some(AtomRestriction::Value(_)))
    };

    let written_the_same = || lhs.to_string() == rhs.to_string();

    match (written_as_value(lhs), written_as_value(rhs)) {
        (true, true) => {
            lhs.restriction == rhs.restriction
                && (lhs.semantic == rhs.semantic || written_the_same())
        }
        (false, false) => {
            lhs.semantic == rhs.semantic
                && (lhs.restriction == rhs.restriction || written_the_same())
        }
        _ => false,
    }
}

/// The value of an atom that's a value of `Bytes`.
fn bytes_value(atom: &Atom) -> Option<&[u8]> {
    match atom {
        Atom {
            semantic: AtomSemantic::Bytes,
            restriction: Some(AtomRestriction::Value(value)),
        } => Some(value),
        _ => None,
    }
}

/// Whether every value of the second atom is a value of the first, and of the same kind.
fn atom_includes(lhs: &Atom, rhs: &Atom) -> bool {
    // `nil` is kept beside the `Bytes` that it's a value of, since it's written there to say
    // that there might be no value, as in a `T | nil` whose `T` is `Bytes`
    if lhs.semantic != rhs.semantic || bytes_value(rhs).is_some_and(<[u8]>::is_empty) {
        return false;
    }

    match (&lhs.restriction, &rhs.restriction) {
        (None, _) => true,
        (Some(_), None) => false,
        (Some(AtomRestriction::Length(length)), Some(AtomRestriction::Value(value))) => {
            value.len() == *length
        }
        (Some(lhs), Some(rhs)) => lhs == rhs,
    }
}

#[cfg(test)]
mod tests {
    use crate::{FunctionParameter, FunctionType, Generic, Pair, Union};

    use super::*;
    use crate::test_types::{literal, pair};

    fn bounded(arena: &mut Arena<Type>, id: TypeId, parts: usize) -> String {
        let (name, cut) = stringify_bounded(arena, id, parts);

        // Whether anything was left out is what the marks for it say
        assert_eq!(cut, name.contains(ELIDED), "{name}");

        name
    }

    #[test]
    fn test_stringify_bounded() {
        let mut arena = Arena::new();

        let int = arena.alloc(Type::Atom(Atom::INT));
        let bytes = arena.alloc(Type::Atom(Atom::BYTES));

        // A type that's small enough is written in full
        let pair = arena.alloc(Type::Pair(Pair::new(int, bytes)));
        assert_eq!(bounded(&mut arena, pair, 3), "(Int, Bytes)");
        assert_eq!(bounded(&mut arena, pair, 2), "(Int, ...)");
        assert_eq!(bounded(&mut arena, pair, 0), "...");

        // A union says that members were left out only if they were
        let union = arena.alloc(Type::Union(Union::new(vec![int, bytes, pair])));
        let full = "Int | Bytes | (Int, Bytes)";
        assert_eq!(bounded(&mut arena, union, 6), full);
        assert_eq!(bounded(&mut arena, union, 5), "Int | Bytes | (Int, ...)");
        assert_eq!(bounded(&mut arena, union, 3), "Int | Bytes | ...");
        assert_eq!(bounded(&mut arena, union, 2), "Int | ...");
        assert_eq!(bounded(&mut arena, union, 1), "...");

        // And so does a union inside of another type, which the union's members are part of
        let nested = arena.alloc(Type::Union(Union::new(vec![union, int])));
        let list = arena.alloc(Type::Pair(Pair::new(nested, bytes)));
        assert_eq!(
            bounded(&mut arena, list, 9),
            "(Int | Bytes | (Int, Bytes), Bytes)"
        );
        assert_eq!(bounded(&mut arena, list, 4), "(Int | Bytes | ..., ...)");

        // A member that there's no budget left for by the time it's written is left out like
        // the ones that weren't looked at, and they're all shown once
        let int_bytes = arena.alloc(Type::Pair(Pair::new(int, bytes)));
        let bytes_int = arena.alloc(Type::Pair(Pair::new(bytes, int)));
        let bytes_bytes = arena.alloc(Type::Pair(Pair::new(bytes, bytes)));
        let pairs = arena.alloc(Type::Union(Union::new(vec![
            int_bytes,
            bytes_int,
            bytes_bytes,
        ])));
        assert_eq!(bounded(&mut arena, pairs, 4), "(Int, Bytes) | ...");
        assert_eq!(
            bounded(&mut arena, pairs, 5),
            "(Int, Bytes) | (..., ...) | ..."
        );
        assert_eq!(
            bounded(&mut arena, pairs, 7),
            "(Int, Bytes) | (Bytes, Int) | ..."
        );
        assert_eq!(
            bounded(&mut arena, pairs, 10),
            "(Int, Bytes) | (Bytes, Int) | (Bytes, Bytes)"
        );

        let inner = arena.alloc(Type::Union(Union::new(vec![bytes, int_bytes])));
        let alias = arena.alloc(Type::Alias(Alias {
            name: None,
            inner,
            generics: Vec::new(),
        }));
        let mixed = arena.alloc(Type::Union(Union::new(vec![int, alias, bytes_int])));
        assert_eq!(bounded(&mut arena, mixed, 3), "Int | ...");

        // A member that's in a union more than once doesn't count as left out
        let repeated = arena.alloc(Type::Union(Union::new(vec![union, union, int])));
        assert_eq!(bounded(&mut arena, repeated, 7), full);

        let same = arena.alloc(Type::Union(Union::new(vec![int; 100])));
        assert_eq!(bounded(&mut arena, same, 3), "Int");
        assert_eq!(bounded(&mut arena, same, 2), "Int");

        // A union with far more members than there's budget for isn't walked to the end
        let mut doubled = union;

        for _ in 0..40 {
            doubled = arena.alloc(Type::Union(Union::new(vec![doubled, doubled])));
        }

        let wide = arena.alloc(Type::Union(Union::new(vec![doubled, int_bytes, bytes_int])));
        assert_eq!(
            bounded(&mut arena, wide, 4),
            "Int | Bytes | (..., ...) | ..."
        );

        // Each pair is made of the one before it twice, so the type is over a million times as
        // long written out as it is in memory
        let mut nested = int;

        for _ in 0..20 {
            nested = arena.alloc(Type::Pair(Pair::new(nested, nested)));
        }

        let short = bounded(&mut arena, nested, 8);
        assert!(short.len() < 200, "{short}");
        assert!(short.contains(ELIDED), "{short}");

        // The budget is only for the type it was given for
        assert_eq!(stringify(&mut arena, pair), "(Int, Bytes)");
    }

    fn union(arena: &mut Arena<Type>, types: &[TypeId]) -> String {
        let union = arena.alloc(Type::Union(Union::new(types.to_vec())));
        stringify(arena, union)
    }

    #[test]
    fn test_union_members_shown_once() {
        let mut arena = Arena::new();
        let arena = &mut arena;

        let int = arena.alloc(Type::Atom(Atom::INT));
        let bytes = arena.alloc(Type::Atom(Atom::BYTES));

        // Members that are the same type written out are shown once, even if they were
        // allocated separately
        let first = pair(arena, int, int);
        let second = pair(arena, int, int);
        let other = pair(arena, int, bytes);
        assert_eq!(
            union(arena, &[first, second, other, first]),
            "(Int, Int) | (Int, Bytes)"
        );

        // Whatever the order of the members of a union inside of them
        let one = literal(arena, AtomSemantic::Int, &[1]);
        let two = literal(arena, AtomSemantic::Int, &[2]);
        let one_two = arena.alloc(Type::Union(Union::new(vec![one, two])));
        let two_one = arena.alloc(Type::Union(Union::new(vec![two, one])));
        let first = pair(arena, int, one_two);
        let second = pair(arena, int, two_one);
        assert_eq!(union(arena, &[first, second]), "(Int, 1 | 2)");

        // Aliases with the same name and arguments are the same if what they're aliases of is
        let alias = |arena: &mut Arena<Type>, inner, generics: Vec<TypeId>| {
            arena.alloc(Type::Alias(Alias {
                name: Some(Name::new("Item", None)),
                inner,
                generics,
            }))
        };
        let first = alias(arena, int, vec![int]);
        let second = alias(arena, int, vec![int]);
        let different = alias(arena, bytes, vec![int]);
        assert_eq!(union(arena, &[first, second]), "Item<Int>");
        assert_eq!(union(arena, &[first, different]), "Item<Int> | Item<Int>");

        // A recursive alias is the same as another copy of it
        let list = |arena: &mut Arena<Type>| {
            let nil = arena.alloc(Type::Atom(Atom::NIL));
            let list = arena.alloc(Type::Unresolved);
            let item = arena.alloc(Type::Pair(Pair::new(int, list)));
            let inner = arena.alloc(Type::Union(Union::new(vec![nil, item])));
            arena[list] = Type::Alias(Alias {
                name: Some(Name::new("List", None)),
                inner,
                generics: vec![int],
            });
            list
        };
        let first = list(arena);
        let second = list(arena);
        assert_eq!(union(arena, &[first, second]), "List<Int>");

        // Structs are the same if they're instances of the same declaration, not if they only
        // have the same name
        let structure = |arena: &mut Arena<Type>, semantic| {
            arena.alloc(Type::Struct(Struct {
                name: Some(Name::new("Point", None)),
                semantic,
                inner: int,
                generics: Vec::new(),
                fields: IndexSet::new(),
                nil_terminated: true,
                layout: crate::StructLayout::List,
                narrowed: false,
            }))
        };
        let first = structure(arena, int);
        let second = structure(arena, int);
        let different = structure(arena, bytes);
        assert_eq!(union(arena, &[first, second]), "Point");
        assert_eq!(union(arena, &[first, different]), "Point | Point");

        let first = pair(arena, first, int);
        let different = pair(arena, different, int);
        assert_eq!(
            union(arena, &[first, different]),
            "(Point, Int) | (Point, Int)"
        );

        // Functions are the same if their parameters and return types are
        let function = |arena: &mut Arena<Type>, param, ret| {
            arena.alloc(Type::Function(FunctionType {
                generics: Vec::new(),
                params: vec![FunctionParameter::new("x", param)],
                nil_terminated: true,
                ret,
            }))
        };
        let first = function(arena, int, one_two);
        let second = function(arena, int, one_two);
        let different = function(arena, bytes, one_two);
        assert_eq!(union(arena, &[first, second]), "fn(x: Int) -> 1 | 2");
        assert_eq!(
            union(arena, &[first, different]),
            "fn(x: Int) -> 1 | 2 | fn(x: Bytes) -> 1 | 2"
        );

        // But not if they're generic over different types that have the same name
        let generic = |arena: &mut Arena<Type>| {
            arena.alloc(Type::Generic(Generic {
                name: Some(Name::new("T", None)),
            }))
        };
        let generic_function = |arena: &mut Arena<Type>, generic| {
            arena.alloc(Type::Function(FunctionType {
                generics: vec![generic],
                params: vec![FunctionParameter::new("x", int)],
                nil_terminated: true,
                ret: int,
            }))
        };
        let (t, other_t) = (generic(arena), generic(arena));
        let first = generic_function(arena, t);
        let second = generic_function(arena, t);
        let different = generic_function(arena, other_t);
        assert_eq!(union(arena, &[first, second]), "fn<T>(x: Int) -> Int");
        assert_eq!(
            union(arena, &[first, different]),
            "fn<T>(x: Int) -> Int | fn<T>(x: Int) -> Int"
        );
        assert_eq!(union(arena, &[t, other_t, t]), "T | T");

        // A function with a spread parameter and a string with dots in it are written in full,
        // although part of a type that's left out is written the same way
        let spread = |arena: &mut Arena<Type>| {
            arena.alloc(Type::Function(FunctionType {
                generics: Vec::new(),
                params: vec![FunctionParameter::new("x", int)],
                nil_terminated: false,
                ret: int,
            }))
        };
        let (first, second) = (spread(arena), spread(arena));
        assert_eq!(union(arena, &[first, second]), "fn(...x: Int) -> Int");

        let first = literal(arena, AtomSemantic::String, b"wait...");
        let second = literal(arena, AtomSemantic::String, b"wait...");
        assert_eq!(union(arena, &[first, second, int]), "\"wait...\" | Int");
        let first = pair(arena, first, int);
        let second = pair(arena, second, int);
        assert_eq!(union(arena, &[first, second]), "(\"wait...\", Int)");
    }

    #[test]
    fn test_union_structs_shown_once() {
        let mut arena = Arena::new();
        let arena = &mut arena;

        let int = arena.alloc(Type::Atom(Atom::INT));
        let bytes = arena.alloc(Type::Atom(Atom::BYTES));
        let semantic = arena.alloc(Type::Unresolved);

        let structure = |arena: &mut Arena<Type>, generic| {
            arena.alloc(Type::Struct(Struct {
                name: Some(Name::new("Box", None)),
                semantic,
                inner: generic,
                generics: vec![generic],
                fields: IndexSet::from(["value".to_string()]),
                nil_terminated: false,
                layout: crate::StructLayout::List,
                narrowed: false,
            }))
        };

        // Instances of a declaration with different generic arguments are different types
        let first = structure(arena, int);
        let second = structure(arena, int);
        let different = structure(arena, bytes);
        assert_eq!(union(arena, &[first, second]), "Box<Int>");
        assert_eq!(union(arena, &[first, different]), "Box<Int> | Box<Bytes>");

        let first = pair(arena, first, int);
        let different = pair(arena, different, int);
        assert_eq!(
            union(arena, &[first, different]),
            "(Box<Int>, Int) | (Box<Bytes>, Int)"
        );

        // A struct that's been narrowed is the same as one that's been narrowed to the same type
        let zero = literal(arena, AtomSemantic::Int, &[]);
        let one = literal(arena, AtomSemantic::Int, &[1]);
        let narrowed = |arena: &mut Arena<Type>, inner| {
            arena.alloc(Type::Struct(Struct {
                name: Some(Name::new("Box", None)),
                semantic,
                inner,
                generics: vec![int],
                fields: IndexSet::from(["value".to_string()]),
                nil_terminated: false,
                layout: crate::StructLayout::List,
                narrowed: true,
            }))
        };
        let first = narrowed(arena, zero);
        let second = narrowed(arena, zero);
        let different = narrowed(arena, one);
        assert_eq!(union(arena, &[first, second]), "Box { ...value: 0 }");
        assert_eq!(
            union(arena, &[first, different]),
            "Box { ...value: 0 } | Box { ...value: 1 }"
        );

        // As part of a pair, too, where the other part of one pair includes the other's
        let in_first = pair(arena, first, int);
        let in_second = pair(arena, second, zero);
        let in_different = pair(arena, different, zero);
        assert_eq!(
            union(arena, &[in_first, in_second]),
            "(Box { ...value: 0 }, Int)"
        );
        assert_eq!(
            union(arena, &[in_first, in_different]),
            "(Box { ...value: 0 }, Int) | (Box { ...value: 1 }, 0)"
        );

        let first = structure(arena, int);
        let second = structure(arena, int);
        let in_first = pair(arena, first, int);
        let in_second = pair(arena, second, zero);
        assert_eq!(union(arena, &[in_first, in_second]), "(Box<Int>, Int)");

        // A generic argument that's a union is the same as one that's displayed the same, since
        // the members that others include aren't displayed
        let int_or_zero = arena.alloc(Type::Union(Union::new(vec![int, zero])));
        let zero_or_one = arena.alloc(Type::Union(Union::new(vec![zero, one])));
        let with_union = structure(arena, int_or_zero);
        let other_union = structure(arena, zero_or_one);
        assert_eq!(union(arena, &[first, with_union]), "Box<Int>");
        assert_eq!(union(arena, &[with_union, first]), "Box<Int>");
        assert_eq!(union(arena, &[first, other_union]), "Box<Int> | Box<0 | 1>");
        let in_first = pair(arena, zero, first);
        let in_second = pair(arena, int, with_union);
        assert_eq!(union(arena, &[in_first, in_second]), "(Int, Box<Int>)");

        // A struct without a name is displayed as what it's made of, which is what's the same
        let anonymous = |arena: &mut Arena<Type>, inner| {
            arena.alloc(Type::Struct(Struct {
                name: None,
                semantic,
                inner,
                generics: Vec::new(),
                fields: IndexSet::new(),
                nil_terminated: true,
                layout: crate::StructLayout::List,
                narrowed: false,
            }))
        };
        let first = anonymous(arena, int);
        let second = anonymous(arena, int);
        let different = anonymous(arena, bytes);
        assert_eq!(union(arena, &[first, second]), "Int");
        assert_eq!(union(arena, &[first, different]), "Int | Bytes");
        let first = pair(arena, first, int);
        let different = pair(arena, different, int);
        assert_eq!(
            union(arena, &[first, different]),
            "(Int, Int) | (Bytes, Int)"
        );
    }

    /// Displaying a union takes about as long as writing its members, however many there are.
    /// Without the limit on the members that are compared, each of these takes minutes.
    #[test]
    fn test_union_of_many_members() {
        let mut arena = Arena::new();
        let arena = &mut arena;

        let int = arena.alloc(Type::Atom(Atom::INT));
        let bytes = arena.alloc(Type::Atom(Atom::BYTES));

        let values = (0..20_000u32)
            .map(|i| literal(arena, AtomSemantic::Bytes, &i.to_be_bytes()))
            .collect::<Vec<_>>();
        let name = union(arena, &values);
        assert_eq!(name.split(" | ").count(), values.len());

        // Every value is looked for in the atoms that have more than one, of which there's one
        let mut with_bytes = values.clone();
        with_bytes.insert(10_000, bytes);
        assert_eq!(union(arena, &with_bytes), "Bytes");

        // Pairs are compared with each other until the limit, and the rest are displayed
        let pairs = values
            .iter()
            .map(|&value| pair(arena, value, int))
            .collect::<Vec<_>>();
        let name = union(arena, &pairs);
        assert_eq!(name.split(" | ").count(), pairs.len());

        // The same pair many times over is found by its name each time, whether or not its
        // parts are the same types
        let same = (0..20_000)
            .map(|_| pair(arena, int, bytes))
            .collect::<Vec<_>>();
        assert_eq!(union(arena, &same), "(Int, Bytes)");

        let copies = (0..20_000)
            .map(|_| {
                let int = arena.alloc(Type::Atom(Atom::INT));
                let bytes = arena.alloc(Type::Atom(Atom::BYTES));
                let nil = arena.alloc(Type::Atom(Atom::NIL));
                let rest = pair(arena, bytes, nil);
                pair(arena, int, rest)
            })
            .collect::<Vec<_>>();
        assert_eq!(union(arena, &copies), "(Int, (Bytes, nil))");

        // Members that are shown the same but aren't the same type are compared with each
        // other until the limit, and all of them are displayed
        let generics = (0..20_000)
            .map(|_| {
                arena.alloc(Type::Generic(Generic {
                    name: Some(Name::new("T", None)),
                }))
            })
            .collect::<Vec<_>>();
        let name = union(arena, &generics);
        assert_eq!(name.split(" | ").count(), generics.len());

        // The members of a union inside of a member count towards the limit, so two pairs of
        // large unions aren't compared member by member for long
        let int_values = (0..20_000u32)
            .map(|i| literal(arena, AtomSemantic::Int, &(i + 256).to_be_bytes()[2..]))
            .collect::<Vec<_>>();
        let with_int = arena.alloc(Type::Union(Union::new(
            [vec![int], int_values.clone()].concat(),
        )));
        let without_int = arena.alloc(Type::Union(Union::new(int_values)));
        let first = pair(arena, with_int, bytes);
        let second = pair(arena, without_int, bytes);
        let name = union(arena, &[first, second]);
        assert!(name.starts_with("(Int, Bytes)"), "{}", &name[..40]);
    }

    /// Types that are too large to compare are displayed as they are, rather than taken to be the
    /// same, and a union always has a member left to display.
    #[test]
    fn test_union_members_too_large_to_compare() {
        let mut arena = Arena::new();
        let arena = &mut arena;

        let int = arena.alloc(Type::Atom(Atom::INT));

        // Each of these is a union of all the others' members in another order, so each
        // includes the others, which takes more comparisons to tell than the limit
        let values = (1..=400u32)
            .map(|i| literal(arena, AtomSemantic::Int, &i.to_be_bytes()[2..]))
            .collect::<Vec<_>>();
        let ascending = arena.alloc(Type::Union(Union::new(
            [vec![int], values.clone()].concat(),
        )));
        let descending = arena.alloc(Type::Union(Union::new(
            values.iter().rev().copied().chain([int]).collect(),
        )));

        let first = pair(arena, descending, int);
        let second = pair(arena, ascending, descending);
        let third = pair(arena, int, ascending);
        let name = union(arena, &[first, second, third]);
        assert!(!name.is_empty());
        assert!(
            name.split(" | ").all(|member| member == "(Int, Int)"),
            "{name}"
        );

        // Two long lists aren't taken to be the same, whether or not they are
        let list = |arena: &mut Arena<Type>, end: TypeId| {
            let mut list = end;

            for _ in 0..=MATCH_LIMIT {
                list = arena.alloc(Type::Pair(Pair::new(int, list)));
            }

            list
        };
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        let first = list(arena, nil);
        let second = list(arena, nil);
        let third = list(arena, int);

        let mut matcher = Matcher::new(MATCH_LIMIT);
        assert!(!matcher.matches(arena, first, second, false));
        assert!(matcher.is_spent());
        assert!(!Matcher::new(MATCH_LIMIT).matches(arena, first, third, false));
        assert!(!Matcher::new(MATCH_LIMIT).matches(arena, first, third, true));
    }

    #[test]
    fn test_union_members_included_in_others() {
        let mut arena = Arena::new();
        let arena = &mut arena;

        let int = arena.alloc(Type::Atom(Atom::INT));
        let bytes = arena.alloc(Type::Atom(Atom::BYTES));
        let bytes32 = arena.alloc(Type::Atom(Atom::BYTES_32));
        let nil = arena.alloc(Type::Atom(Atom::NIL));
        let zero = literal(arena, AtomSemantic::Int, &[]);
        let one = literal(arena, AtomSemantic::Int, &[1]);
        let text = literal(arena, AtomSemantic::String, b"a");
        let hash = literal(arena, AtomSemantic::Bytes, &[7; 32]);

        // A value or a length of an atom is included in the atom, whichever comes first
        assert_eq!(union(arena, &[int, zero]), "Int");
        assert_eq!(union(arena, &[zero, int, one]), "Int");
        assert_eq!(union(arena, &[bytes32, bytes, hash]), "Bytes");
        assert_eq!(union(arena, &[bytes32, hash]), "Bytes32");

        // A value of another length isn't one of the atom's
        let short = literal(arena, AtomSemantic::Bytes, &[7]);
        assert_eq!(union(arena, &[bytes32, short]), "Bytes32 | 0x07");
        let first = pair(arena, bytes32, int);
        let second = pair(arena, short, int);
        assert_eq!(
            union(arena, &[first, second]),
            "(Bytes32, Int) | (0x07, Int)"
        );

        // Other than `nil`, which says that there might be no value
        assert_eq!(union(arena, &[bytes, nil]), "Bytes | nil");
        assert_eq!(union(arena, &[bytes32, nil, nil]), "Bytes32 | nil");
        let first = pair(arena, bytes, nil);
        let second = pair(arena, nil, nil);
        assert_eq!(union(arena, &[first, second]), "(Bytes, nil) | (nil, nil)");

        // Atoms that are written the same are shown once, even if they're of different kinds,
        // as an `Int` that isn't the shortest form of its number is written like `Bytes`
        let long_int = literal(arena, AtomSemantic::Int, &[0, 1]);
        let long_bytes = literal(arena, AtomSemantic::Bytes, &[0, 1]);
        assert_eq!(union(arena, &[long_int, long_bytes, one]), "0x0001 | 1");

        // But not in an atom of another kind, which it can't be assigned to without a cast
        assert_eq!(union(arena, &[bytes, text, zero]), "Bytes | \"a\" | 0");
        assert_eq!(union(arena, &[zero, one]), "0 | 1");

        // A pair is included in a pair of what includes its parts
        let wide = pair(arena, bytes32, int);
        let narrow = pair(arena, bytes32, zero);
        let mixed = pair(arena, hash, one);
        assert_eq!(union(arena, &[wide, narrow, mixed]), "(Bytes32, Int)");
        assert_eq!(union(arena, &[narrow, mixed, wide]), "(Bytes32, Int)");

        let left = pair(arena, int, one);
        let right = pair(arena, zero, int);
        assert_eq!(union(arena, &[left, right]), "(Int, 1) | (0, Int)");

        // A union is included if all of its members are, and includes what one of them does
        let zero_one = arena.alloc(Type::Union(Union::new(vec![zero, one])));
        let int_nil = arena.alloc(Type::Union(Union::new(vec![int, nil])));
        let first = pair(arena, zero_one, bytes);
        let second = pair(arena, int_nil, bytes);
        let third = pair(arena, zero, bytes);
        assert_eq!(union(arena, &[first, second]), "(Int | nil, Bytes)");
        assert_eq!(union(arena, &[third, first]), "(0 | 1, Bytes)");

        // Of two members that include each other, the first is shown
        let int_zero = arena.alloc(Type::Union(Union::new(vec![int, zero])));
        let first = pair(arena, int_zero, bytes);
        let second = pair(arena, int, bytes);
        assert_eq!(union(arena, &[first, second]), "(Int, Bytes)");
        assert_eq!(union(arena, &[second, first]), "(Int, Bytes)");

        // The parameters of a function aren't included in wider ones
        let function = |arena: &mut Arena<Type>, param, ret| {
            arena.alloc(Type::Function(FunctionType {
                generics: Vec::new(),
                params: vec![FunctionParameter::new("x", param)],
                nil_terminated: true,
                ret,
            }))
        };
        let first = function(arena, int, int);
        let second = function(arena, zero, int);
        let third = function(arena, int, zero);
        assert_eq!(
            union(arena, &[first, second]),
            "fn(x: Int) -> Int | fn(x: 0) -> Int"
        );

        // Nor is what a function returns, as a member or as part of one
        let in_first = pair(arena, first, int);
        let in_second = pair(arena, second, int);
        let in_third = pair(arena, third, int);
        assert_eq!(
            union(arena, &[in_first, in_second]),
            "(fn(x: Int) -> Int, Int) | (fn(x: 0) -> Int, Int)"
        );
        assert_eq!(
            union(arena, &[in_first, in_third]),
            "(fn(x: Int) -> Int, Int) | (fn(x: Int) -> 0, Int)"
        );

        // A function that returns a union isn't the same as one that returns more of them
        let one = literal(arena, AtomSemantic::Int, &[1]);
        let two = literal(arena, AtomSemantic::Int, &[2]);
        let fewer = arena.alloc(Type::Union(Union::new(vec![zero, one])));
        let more = arena.alloc(Type::Union(Union::new(vec![zero, one, two])));
        let returns_fewer = function(arena, int, fewer);
        let returns_more = function(arena, int, more);
        let in_fewer = pair(arena, returns_fewer, int);
        let in_more = pair(arena, returns_more, zero);
        assert_eq!(
            union(arena, &[in_fewer, in_more]),
            "(fn(x: Int) -> 0 | 1, Int) | (fn(x: Int) -> 0 | 1 | 2, 0)"
        );
        assert_eq!(
            union(arena, &[in_more, in_fewer]),
            "(fn(x: Int) -> 0 | 1 | 2, 0) | (fn(x: Int) -> 0 | 1, Int)"
        );

        // Nor is a function that takes the rest of its arguments the same as one that doesn't
        let spread = arena.alloc(Type::Function(FunctionType {
            generics: Vec::new(),
            params: vec![FunctionParameter::new("x", int)],
            nil_terminated: false,
            ret: int,
        }));
        let in_spread = pair(arena, spread, zero);
        assert_eq!(
            union(arena, &[in_first, in_spread]),
            "(fn(x: Int) -> Int, Int) | (fn(...x: Int) -> Int, 0)"
        );
    }

    #[test]
    fn test_stringify_exact() {
        let mut arena = Arena::new();
        let arena = &mut arena;

        let int = arena.alloc(Type::Atom(Atom::INT));
        let zero = literal(arena, AtomSemantic::Int, &[]);
        let yes = arena.alloc(Type::Atom(Atom::TRUE));
        let no = arena.alloc(Type::Atom(Atom::FALSE));
        let first = pair(arena, int, int);
        let second = pair(arena, int, int);
        let inner = arena.alloc(Type::Union(Union::new(vec![int, zero])));
        let third = pair(arena, inner, yes);
        let members = arena.alloc(Type::Union(Union::new(vec![
            first, second, third, yes, no, first,
        ])));

        // Every member is written, other than one that's the very same type as another
        assert_eq!(
            stringify_exact(arena, members),
            "(Int, Int) | (Int, Int) | (Int | 0, true) | true | false"
        );

        // Which is only for that type
        assert_eq!(stringify(arena, members), "(Int, Int) | (Int, true) | Bool");

        // The names of members that were written one by one are displayed the same way
        let names = [first, second, yes, no]
            .map(|id| (id, stringify(arena, id)))
            .to_vec();
        assert_eq!(distinct_member_names(arena, names), ["(Int, Int)", "Bool"]);
    }

    #[test]
    fn test_union_bool() {
        let mut arena = Arena::new();
        let arena = &mut arena;

        let int = arena.alloc(Type::Atom(Atom::INT));
        let bool = arena.alloc(Type::Atom(Atom::new(AtomSemantic::Bool, None)));
        let yes = arena.alloc(Type::Atom(Atom::TRUE));
        let no = arena.alloc(Type::Atom(Atom::FALSE));
        let other_yes = arena.alloc(Type::Atom(Atom::TRUE));

        // Both values of `Bool` are shown as `Bool`, where the first of them was
        assert_eq!(union(arena, &[yes, no]), "Bool");
        assert_eq!(union(arena, &[no, other_yes, yes]), "Bool");
        assert_eq!(union(arena, &[int, no, yes]), "Int | Bool");
        assert_eq!(union(arena, &[yes, int, no]), "Bool | Int");

        // One of them is shown as it is
        assert_eq!(union(arena, &[yes, int, other_yes]), "true | Int");

        // And `Bool` itself includes them
        assert_eq!(union(arena, &[yes, bool]), "Bool");
        assert_eq!(union(arena, &[yes, no, bool]), "Bool");
        assert_eq!(union(arena, &[bool, no, int, yes]), "Bool | Int");

        let list = arena.alloc(Type::Union(Union::new(vec![yes, no])));
        let item = pair(arena, list, int);
        assert_eq!(stringify(arena, item), "(Bool, Int)");
    }

    #[test]
    fn test_union_members_left_out() {
        let mut arena = Arena::new();
        let arena = &mut arena;

        let int = arena.alloc(Type::Atom(Atom::INT));
        let bytes = arena.alloc(Type::Atom(Atom::BYTES));

        // Members that weren't written in full aren't known to be the same
        let first = pair(arena, int, bytes);
        let second = pair(arena, int, bytes);
        let types = arena.alloc(Type::Union(Union::new(vec![first, second])));
        assert_eq!(bounded(arena, types, 7), "(Int, Bytes)");
        assert_eq!(bounded(arena, types, 6), "(Int, Bytes) | (Int, ...)");
    }
}
