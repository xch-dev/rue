use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use indexmap::IndexMap;

use crate::grow_stack;

/// Returns the strongly connected components of the graph with Tarjan's algorithm, each one after
/// the components it has edges to. An edge to a node that isn't in the graph is ignored.
pub fn strongly_connected_components<T, E>(edges: &IndexMap<T, E>) -> Vec<Vec<T>>
where
    T: Copy + Eq + Hash,
    for<'a> &'a E: IntoIterator<Item = &'a T>,
{
    struct State<'a, T, E> {
        edges: &'a IndexMap<T, E>,
        index: HashMap<T, usize>,
        low: HashMap<T, usize>,
        stack: Vec<T>,
        on_stack: HashSet<T>,
        components: Vec<Vec<T>>,
    }

    fn visit<T, E>(state: &mut State<'_, T, E>, node: T)
    where
        T: Copy + Eq + Hash,
        for<'a> &'a E: IntoIterator<Item = &'a T>,
    {
        let index = state.index.len();
        state.index.insert(node, index);
        state.low.insert(node, index);
        state.stack.push(node);
        state.on_stack.insert(node);

        for &next in &state.edges[&node] {
            if !state.edges.contains_key(&next) {
                continue;
            }

            if !state.index.contains_key(&next) {
                // A long chain of nodes mustn't overflow the stack
                grow_stack(|| visit(state, next));
                let low = state.low[&node].min(state.low[&next]);
                state.low.insert(node, low);
            } else if state.on_stack.contains(&next) {
                let low = state.low[&node].min(state.index[&next]);
                state.low.insert(node, low);
            }
        }

        if state.low[&node] == index {
            let mut component = Vec::new();

            loop {
                let member = state.stack.pop().unwrap();
                state.on_stack.remove(&member);
                component.push(member);

                if member == node {
                    break;
                }
            }

            state.components.push(component);
        }
    }

    let mut state = State {
        edges,
        index: HashMap::new(),
        low: HashMap::new(),
        stack: Vec::new(),
        on_stack: HashSet::new(),
        components: Vec::new(),
    };

    for &node in edges.keys() {
        if !state.index.contains_key(&node) {
            grow_stack(|| visit(&mut state, node));
        }
    }

    state.components
}
