use std::fmt::Debug;
use intrusive_collections::intrusive_adapter;
use intrusive_collections::{LinkedList, LinkedListLink};
use std::collections::HashMap;
use std::ptr::null;
use std::usize;

type EdgeIter<'a> = Box<dyn Iterator<Item = usize> + 'a>;
type NodeIter<'a> = Box<dyn Iterator<Item = usize> + 'a>;

pub trait Graph {
    /* number of nodes */
    fn node_num(&self) -> usize;

    /* iterator of nodes */
    fn node_iter<'a>(&'a self) -> NodeIter<'a>;

    /* iterator of edge endpoints from node */
    fn edge_iter<'a>(&'a self, node: usize) -> EdgeIter<'a>;

    /* test if graph contains the node */
    fn contain_node(&self, node: usize) -> bool;
}

pub trait NotedGraph: Graph {
    type Note;

    /* get notes of a node */
    fn read_note<'a>(&'a self, node: usize) -> &'a Self::Note;

    /* get mutable reference to a note */
    fn get_note_mut<'a>(&'a mut self, node: usize) -> &'a mut Self::Note;

    /* put notes for a node */
    fn write_note<'a>(&'a mut self, node: usize, note: Self::Note) {
        *self.get_note_mut(node) = note;
    }
}

pub struct StaticGraph<Note> {
    edges: Vec<Vec<usize>>,
    notes: Vec<Note>,
}

impl<Note: Default + Clone> StaticGraph<Note> {
    pub fn new_from_default(node_num: usize) -> StaticGraph<Note> {
        StaticGraph {
            edges: vec![Default::default(); node_num],
            notes: vec![Default::default(); node_num],
        }
    }

    pub fn new(node_num: usize, edges: Vec<(usize, usize)>) -> StaticGraph<Note> {
        let mut graph = StaticGraph::new_from_default(node_num);
        for (x, y) in edges.iter() {
            graph.add_edge(*x, *y);
        }
        graph
    }

}

impl<Note> StaticGraph<Note> {
    pub fn new_empty() -> StaticGraph<Note> {
        StaticGraph {
            edges: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn add_node(&mut self, note: Note) -> usize {
        let num = self.node_num();
        self.edges.push(Vec::new());
        self.notes.push(note);
        num
    }

    pub fn add_edge(&mut self, node_from: usize, node_to: usize) {
        self.edges[node_from].push(node_to);
    }

    pub fn set_edges(&mut self, node: usize, edges: Vec<usize>) {
        self.edges[node] = edges;
    }
}

impl<Note> Graph for StaticGraph<Note> {
    fn node_num(&self) -> usize {
        self.edges.len()
    }

    fn edge_iter<'a>(&'a self, node: usize) -> EdgeIter<'a> {
        let edges = &self.edges[node];
        Box::new(edges.iter().map(|x| *x))
    }

    fn node_iter<'a>(&'a self) -> NodeIter<'a> {
        let n = self.node_num();
        Box::new(0..n)
    }

    fn contain_node(&self, node: usize) -> bool {
        node < self.node_num()
    }
}

impl<Note> NotedGraph for StaticGraph<Note> {
    type Note = Note;

    fn read_note(&self, node: usize) -> &Self::Note {
        &self.notes[node]
    }

    fn get_note_mut<'a>(&'a mut self, node: usize) -> &'a mut Self::Note {
        &mut self.notes[node]
    }
}

struct EditableEdge {
    link: LinkedListLink,
    dest: usize,
    rev: *const EditableEdge,
}

intrusive_adapter!(EditableEdgeAdaptor = Box<EditableEdge>: EditableEdge { link: LinkedListLink });

impl EditableEdge {
    fn new(dest: usize) -> EditableEdge {
        EditableEdge {
            link: LinkedListLink::new(),
            dest,
            rev: null(),
        }
    }
}

struct EditableNode<Note> {
    id: usize,
    link: LinkedListLink,
    note: Note,
    edge_forward: LinkedList<EditableEdgeAdaptor>,
    edge_backward: LinkedList<EditableEdgeAdaptor>,
}

intrusive_adapter!(EditableNodeAdaptor<Note> = Box<EditableNode<Note>>: EditableNode<Note> { link: LinkedListLink });

impl<Note> EditableNode<Note> {
    fn new(id: usize, note: Note) -> EditableNode<Note> {
        EditableNode {
            id,
            note,
            link: LinkedListLink::new(),
            edge_forward: LinkedList::new(EditableEdgeAdaptor::new()),
            edge_backward: LinkedList::new(EditableEdgeAdaptor::new()),
        }
    }
}

pub struct EditableGraph<Note> {
    nodes: LinkedList<EditableNodeAdaptor<Note>>,
    node_position: HashMap<usize, *const EditableNode<Note>>,
    total_node_num: usize,
}

impl<Note> EditableGraph<Note> {
    pub fn new() -> EditableGraph<Note> {
        EditableGraph {
            nodes: LinkedList::new(EditableNodeAdaptor::new()),
            node_position: HashMap::new(),
            total_node_num: 0,
        }
    }

    fn node_from_id(&self, id: usize) -> Option<*const EditableNode<Note>> {
        let node = self.node_position.get(&id)?;
        Some(*node)
    }

    fn node_from_id_mut(&self, id: usize) -> Option<*mut EditableNode<Note>> {
        let node = self.node_position.get(&id)?;
        Some(*node as *mut EditableNode<Note>)
    }

    pub fn add_node(&mut self, note: Note) -> usize {
        let num = self.total_node_num;
        self.total_node_num += 1;
        self.nodes.push_back(Box::new(EditableNode::new(num, note)));
        let cursor = self.nodes.back().get().unwrap();
        self.node_position.insert(num, cursor);
        num
    }

    pub fn del_node(&mut self, id: usize) {
        if let Some(node_ptr) = self.node_from_id(id) {
            unsafe {
                let node = node_ptr.as_ref().unwrap();
                for edge in node.edge_forward.iter() {
                    let rnode_ptr = self.node_from_id_mut(edge.dest).unwrap();
                    let rnode = rnode_ptr.as_mut().unwrap();
                    let mut rcursor = rnode.edge_backward.cursor_mut_from_ptr(edge.rev);
                    rcursor.remove();
                }
                for edge in node.edge_backward.iter() {
                    let rnode_ptr = self.node_from_id_mut(edge.dest).unwrap();
                    let rnode = rnode_ptr.as_mut().unwrap();
                    let mut rcursor = rnode.edge_forward.cursor_mut_from_ptr(edge.rev);
                    rcursor.remove();
                }
                let mut cursor = self.nodes.cursor_mut_from_ptr(node_ptr);
                cursor.remove().unwrap();
            }
            self.node_position.remove(&id);
        }
    }

    pub fn add_edge(&mut self, id_from: usize, id_to: usize) {
        unsafe {
            let node_from = match self.node_from_id_mut(id_from) {
                None => return,
                Some(ptr) => ptr.as_mut().unwrap(),
            };
            let node_to = match self.node_from_id_mut(id_to) {
                None => return,
                Some(ptr) => ptr.as_mut().unwrap(),
            };
            let mut forward_edge = Box::new(EditableEdge::new(id_to));
            let mut backward_edge = Box::new(EditableEdge::new(id_from));
            forward_edge.rev = backward_edge.as_ref();
            backward_edge.rev = forward_edge.as_ref();
            node_from.edge_forward.push_back(forward_edge);
            node_to.edge_backward.push_back(backward_edge);
        }
    }

    pub fn del_edge(&mut self, id_from: usize, id_to: usize) {
        unsafe {
            let node_from = match self.node_from_id_mut(id_from) {
                None => return,
                Some(ptr) => ptr.as_mut().unwrap(),
            };
            let node_to = match self.node_from_id_mut(id_to) {
                None => return,
                Some(ptr) => ptr.as_mut().unwrap(),
            };
            let mut cursor = node_from.edge_forward.front_mut();
            while !cursor.is_null() {
                let edge = cursor.get().unwrap();
                if edge.dest == id_to {
                    let rev = edge.rev as *mut EditableEdge;
                    let mut rcursor = node_to.edge_backward.cursor_mut_from_ptr(rev);
                    rcursor.remove().unwrap();
                    cursor.remove().unwrap();
                    break;
                }
                cursor.move_next();
            }
        }
    }

    fn edge_iter<'a>(&'a self, node: usize, forward: bool) -> EdgeIter<'a> {
        match self.node_from_id(node) {
            None => Box::new(0..0),
            Some(ptr) => unsafe {
                let node = ptr.as_ref().unwrap();
                let iter = match forward {
                    true => node.edge_forward.iter(),
                    false => node.edge_backward.iter(),
                };
                let iter = iter.map(|x| x.dest);
                Box::new(iter)
            },
        }
    }

    pub fn reverse_edge_iter<'a>(&'a self, node: usize) -> EdgeIter<'a> {
        self.edge_iter(node, false)
    }
}

impl<Note> Graph for EditableGraph<Note> {
    fn node_num(&self) -> usize {
        self.node_position.len()
    }

    fn edge_iter<'a>(&'a self, node: usize) -> EdgeIter<'a> {
        self.edge_iter(node, true)
    }

    fn node_iter<'a>(&'a self) -> NodeIter<'a> {
        let iter = self.nodes.iter();
        Box::new(iter.map(|x| x.id))
    }

    fn contain_node(&self, node: usize) -> bool {
        self.node_position.contains_key(&node)
    }
}

impl<Note> NotedGraph for EditableGraph<Note> {
    type Note = Note;

    fn read_note<'a>(&'a self, node: usize) -> &'a Self::Note {
        let node = self.node_from_id(node).unwrap();
        unsafe {
            let node = node.as_ref().unwrap();
            &node.note
        }
    }

    fn get_note_mut<'a>(&'a mut self, node: usize) -> &'a mut Self::Note {
        let node = self.node_from_id(node).unwrap();
        unsafe {
            let node = node as *mut EditableNode<Note>;
            let node = node.as_mut().unwrap();
            &mut node.note
        }
    }
}

pub fn debug_print<Note: Debug>(graph: &dyn NotedGraph<Note = Note>) {
    println!("node num: {}", graph.node_num());
    for i in graph.node_iter() {
        println!("note {}: {:?}", i, graph.read_note(i));
    }
    for i in graph.node_iter() {
        for j in graph.edge_iter(i) {
            println!("{} -> {}", i, j);
        }
    }
}

pub fn dot_graph<Note: ToString>(graph: &dyn NotedGraph<Note = Note>, entry: usize) {
    println!("digraph {{");
    println!("entry [label=\"entry\",shape=circle]");
    for node in graph.node_iter() {
        let shape = if graph.edge_iter(node).count() == 2 {
            "shape=diamond"
        } else {
            "shape=box"
        };
        let label: &str = &format!("label=\"{}\"", graph.read_note(node).to_string());
        let attr = [shape, label].join(",");
        println!("{} [{}]", node, attr);
    }
    println!("entry -> {}", entry);
    for x in graph.node_iter() {
        let edges: Vec<usize> = graph.edge_iter(x).collect();
        for y in edges.iter() {
            let attr;
            if edges.len() == 1 {
                attr = "color=black";
            } else {
                if y == edges.first().unwrap() {
                    attr = "color=red";
                } else {
                    attr = "color=blue";
                }
            }
            println!("{} -> {} [{}]", x, y, attr);
        }
    }
    println!("}}");
}