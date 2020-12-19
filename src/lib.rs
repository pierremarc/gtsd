use std::{
    cell::{RefCell, RefMut},
    cmp::min,
    fmt::{Display, Formatter},
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Op<T>
where
    T: PartialEq,
{
    Id(usize, T),
    Ins(usize, T),
    Del(usize, T),
    Sub(usize, T, T),
    Unreachable,
}

impl<T> Display for Op<T>
where
    T: Display + PartialEq,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Id(_, s) => write!(f, "[Id({})     ]", s),
            Op::Ins(_, s) => write!(f, "[Ins({})    ]", s),
            Op::Del(_, s) => write!(f, "[Del({})    ]", s),
            Op::Sub(_, s, t) => write!(f, "[Sub({}|{})]", s, t),
            Op::Unreachable => write!(f, "[          ]"),
        }
    }
}

struct Matrix<T>(Vec<Vec<T>>, T);

impl<T> Matrix<T>
where
    T: Clone,
{
    fn new(m: usize, n: usize, dflt: T) -> Matrix<T> {
        // let cache:Vec<Vec<usize>> = (0..m).map(||   ).collect();
        Matrix(vec![vec![dflt.clone(); n]; m], dflt.clone())
    }

    fn assign(&mut self, index: [usize; 2], new: T) {
        let [i, j] = index;
        self.0
            .get_mut(i)
            .and_then(|c| c.get_mut(j))
            .map(|old| *old = new);
    }

    // fn map<M, F>(&self, f: F, dflt: M) -> Matrix<M>
    // where
    //     M: Clone,
    //     F: Fn(&T) -> M,
    // {
    //     let mx: Vec<Vec<M>> = self
    //         .0
    //         .iter()
    //         .map(|row| row.iter().map(|x| f(x)).collect())
    //         .collect();
    //     Matrix(mx, dflt)
    // }
}

impl<T> std::ops::Index<[usize; 2]> for Matrix<T>
where
    T: Clone,
{
    type Output = T;

    fn index(&self, index: [usize; 2]) -> &Self::Output {
        let [i, j] = index;
        self.0
            .get(i)
            .and_then(|c| c.get(j))
            .expect(&format!("oops: i: {}, j: {}", i, j))
    }
}

impl<T> Display for Matrix<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "==========================\n")?;
        for row in self.0.iter() {
            write!(
                f,
                "{}\n",
                row.iter().map(|x| format!("{} ", x)).collect::<String>()
            )?;
        }
        write!(f, "==========================\n")
    }
}

type Diff<T> = Vec<Op<T>>;

pub fn levenshtein_ops<'a, Iter, Elem>(a: &'a Iter, b: &'a Iter) -> Diff<Elem>
where
    &'a Iter: IntoIterator<Item = Elem>,
    Elem: PartialEq<Elem> + Clone + Display,
{
    let a_len = a.into_iter().count() + 1;
    let b_len = b.into_iter().count() + 1;
    if a_len == 1 {
        return b
            .into_iter()
            .enumerate()
            .map(|(i, x)| Op::Ins(i, x))
            .collect();
    }

    let a_vec: Vec<Elem> = a.into_iter().collect();
    let b_vec: Vec<Elem> = b.into_iter().collect();
    let mut d = Matrix::new(a_len, b_len, 0);
    let mut ops: Matrix<Op<Elem>> = Matrix::new(a_len, b_len, Op::Unreachable);
    let mut result: Vec<Op<Elem>> = Vec::new();

    for i in 0..a_len {
        d.assign([i, 0], i)
    }
    for j in 0..b_len {
        d.assign([0, j], j)
    }

    for j in 1..(b_len) {
        for i in 1..(a_len) {
            let ea = &a_vec[i - 1];
            let eb = &b_vec[j - 1];
            let sub_cost = if ea == eb { 0usize } else { 1usize };
            let del = d[[i - 1, j]] + 1usize;
            let ins = d[[i, j - 1]] + 1usize;
            let sub = d[[i - 1, j - 1]] + sub_cost;
            let val = min(del, min(ins, sub));
            let op = if val == del {
                Op::Del(j, ea.clone())
            } else if val == ins {
                Op::Ins(j, eb.clone())
            } else if val == sub {
                if sub_cost == 0 {
                    Op::Id(j, eb.clone())
                } else {
                    Op::Sub(j, ea.clone(), eb.clone())
                }
            } else {
                Op::Unreachable
            };
            d.assign([i, j], val);
            ops.assign([i, j], op);
        }
    }

    // println!("{}", d);
    // println!("{}", ops);

    let mut row = a_len - 1;
    let mut column = b_len - 1;

    result.push(ops[[row, column]].clone());
    loop {
        let diag_index = [row - 1, column - 1];
        let left_index = [row, column - 1];
        let up_index = [row - 1, column];
        let diag = d[diag_index];
        let left = d[left_index];
        let up = d[up_index];
        let op = if diag < left && diag < up {
            row -= 1;
            column -= 1;
            ops[diag_index].clone()
        } else if up <= left {
            row -= 1;
            ops[up_index].clone()
        } else {
            column -= 1;
            ops[left_index].clone()
        };
        match op {
            Op::Unreachable => break,
            _ => result.push(op),
        };
    }
    result.reverse();
    result
}

type Shared<T> = Rc<RefCell<T>>;

fn shared<T>(inner: T) -> Shared<T> {
    Rc::new(RefCell::new(inner))
}

#[derive(Debug, Clone)]
enum Node {
    Element(String, Vec<Shared<Node>>),
    Attr(String, String),
    Text(String),
}

fn element(tag: impl Into<String>) -> Node {
    Node::Element(tag.into(), Vec::new())
}

fn attr(k: impl Into<String>, v: impl Into<String>) -> Node {
    Node::Attr(k.into(), v.into())
}

fn text(t: impl Into<String>) -> Node {
    Node::Text(t.into())
}

#[derive(Debug, Clone)]
struct FlattenNode {
    parent: Option<usize>,
    node: Node,
}

type FlattenNodeList = Vec<FlattenNode>;

impl Node {
    fn flatten(&self, parent: Option<usize>, ns: &mut Vec<FlattenNode>) {
        match self {
            Node::Element(tag, children) => {
                ns.push(FlattenNode {
                    parent: parent.clone(),
                    node: Node::Element(tag.clone(), Vec::new()),
                });
                let p = ns.len() - 1;
                children
                    .iter()
                    .for_each(|c| c.borrow().flatten(Some(p), ns))
            }
            Node::Attr(_, _) => ns.push(FlattenNode {
                parent: parent.clone(),
                node: self.clone(),
            }),
            Node::Text(_) => ns.push(FlattenNode {
                parent: parent.clone(),
                node: self.clone(),
            }),
        }
    }

    fn append(&self, c: Node) -> Node {
        match self {
            Node::Element(tag, cs) => {
                Node::Element(tag.into(), [cs.clone(), vec![shared(c)]].concat())
            }
            _ => self.clone(),
        }
    }

    fn append_mut(&mut self, c: Node) -> Shared<Node> {
        let child_node = shared(c);
        match self {
            Node::Element(_, cs) => cs.push(child_node.clone()),
            _ => {}
        };
        child_node
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Element(tag, cs) => write!(f, "<{} {}>", tag, cs.len()),
            Node::Attr(k, v) => write!(f, "[{} = \"{}\"]", k, v),
            Node::Text(txt) => write!(f, "«{}»", txt),
        }
    }
}
impl Display for FlattenNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.node.fmt(f)
    }
}

impl std::cmp::PartialEq<Node> for Node {
    fn eq(&self, other: &Node) -> bool {
        match (self, other) {
            (Node::Element(tag1, _), Node::Element(tag2, _)) => tag1 == tag2,
            (Node::Attr(k1, v1), Node::Attr(k2, v2)) => k1 == k2 && v1 == v2,
            (Node::Text(t1), Node::Text(t2)) => t1 == t2,
            (_, _) => false,
        }
    }
}

impl std::cmp::PartialEq<FlattenNode> for FlattenNode {
    fn eq(&self, other: &FlattenNode) -> bool {
        self.node == other.node
    }
}

struct FlatNodeList(Vec<FlattenNode>);

fn append_to_root(
    roots_ref: RefMut<Vec<(usize, Shared<Node>)>>,
    parent: Option<usize>,
    n: Node,
) -> Option<Shared<Node>> {
    if let Some((_, cell)) = parent.and_then(|id| roots_ref.iter().find(|r| r.0 == id)) {
        let mut parent_node = cell.borrow_mut();
        let appended = (*parent_node).append_mut(n);
        if let Ok(r) = appended.clone().try_borrow() {
            match *r {
                Node::Element(_, _) => {
                    return Some(appended.clone());
                }
                _ => {}
            }
        }
    };
    None
    // let result = parent
    //     .and_then(|id| roots.into_iter().find(|r| r.0 == id))
    //     .map(|(_, cell)| {
    //         let mut parent_node = cell.borrow_mut();
    //         match (*parent_node).append_mut(n) {
    //             Node::Element(_, cs) => {
    //                 cs.last().map(|last| roots.push((id, last)));
    //             }
    //             _ => {}
    //         }
    //     });
}

// fn append_to_root(
//     roots: &Vec<(usize, RefCell<Node>)>,
//     parent: Option<usize>,
//     n: Node,
// ) -> Option<&Node> {
//     let result = parent
//         .and_then(|id| roots.iter().find(|r| r.0 == id))
//         .and_then(|(_, cell)| match cell.try_borrow_mut() {
//             Ok(r) => match &*r {
//                 Node::Element(_, cs) => {
//                     cs.push(n);
//                     cs.last()
//                 }
//                 _ => None,
//             },
//             Err(err) => {
//                 println!("Could not borrow parent: {}", err);
//                 None
//             }
//         });

//     result
// }

impl FlatNodeList {
    fn as_tree(self) -> Option<Node> {
        let roots: RefCell<Vec<(usize, Shared<Node>)>> = RefCell::new(Vec::new());
        let mut iter = self.0.iter().enumerate();
        {
            if let Some((0, flat)) = iter.next() {
                roots.borrow_mut().push((0usize, shared(flat.node.clone())));
            } else {
                return None;
            }
        }

        while let (Some((idx, flat)), Ok(rs)) = (iter.next(), roots.try_borrow_mut()) {
            match &flat.node {
                Node::Text(_) => {
                    append_to_root(rs, flat.parent, flat.node.clone());
                }
                Node::Attr(_, _) => {
                    append_to_root(rs, flat.parent, flat.node.clone());
                }
                Node::Element(_, _) => {
                    if let Some(sn) = append_to_root(rs, flat.parent, flat.node.clone()) {
                        roots.borrow_mut().push((idx, sn.clone()));
                    }
                }
            }
        }
        // self.0
        //     .iter()
        //     .enumerate()
        //     .for_each(|(idx, flat)| match &flat.node {
        //         Node::Text(_) => {
        //             append_to_root(&mut roots, flat.parent, idx, flat.node.clone());
        //         }
        //         Node::Attr(_, _) => {
        //             append_to_root(&mut roots, flat.parent, idx, flat.node.clone());
        //         }
        //         Node::Element(_, _) => {
        //             append_to_root(&mut roots, flat.parent, idx, flat.node.clone());
        //         }
        //     });

        roots
            .clone()
            .borrow()
            .first()
            .map(|(_, sn)| sn.borrow().clone())
        // None
    }
}

impl Display for FlatNodeList {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.0
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{attr, element, levenshtein_ops, text, FlatNodeList, Op};

    // macro_rules! s {
    //     ($s:tt) => {
    //         &String::from($s)
    //     };
    // }
    // #[test]
    // fn levenshtein_kitten() {
    //     let a: Vec<_> = "sitting".chars().map(|c| String::from(c)).collect();
    //     let b: Vec<_> = "kitten".chars().map(|c| String::from(c)).collect();

    //     // let result = dbg!(levenshtein_ops(&a, &b));
    //     let result = levenshtein_ops(&a, &b);
    //     assert_eq!(result[0], Op::Sub(1, s!("s"), s!("k")));
    //     assert_eq!(result[1], Op::Id(2, s!("i")));
    //     assert_eq!(result[2], Op::Id(3, s!("t")));
    //     assert_eq!(result[3], Op::Id(4, s!("t")));
    //     assert_eq!(result[4], Op::Sub(5, s!("i"), s!("e")));
    //     assert_eq!(result[5], Op::Id(6, s!("n")));
    //     assert_eq!(result[6], Op::Del(6, s!("g")));
    // }

    // #[test]
    // fn flatten_nodes() {
    //     let root = element("tag1")
    //         .append(&attr("foo", "bar"))
    //         .append(&text("Hello World"))
    //         .clone();

    //     let mut ns = Vec::new();
    //     root.flatten(None, &mut ns);
    //     println!("{}", FlatNodeList(ns));
    //     // assert!(false);
    // }

    // #[test]
    // fn lev_nodes() {
    //     let root_a = element("tag1")
    //         .append(&attr("foo", "bar"))
    //         .append(&text("Hello World"))
    //         .clone();
    //     let root_b = element("tag1")
    //         .append(&attr("foo", "foo"))
    //         .append(&text("Hello World"))
    //         .clone();

    //     let mut a = Vec::new();
    //     let mut b = Vec::new();

    //     root_a.flatten(None, &mut a);
    //     root_b.flatten(None, &mut b);
    //     let result = levenshtein_ops(&a, &b);
    //     println!("{:?}", result);
    //     //         assert!(result, [
    //     //             Id(1, FlattenNode { parent: None, node: Element("tag1", []) }),
    //     //             Sub(2, FlattenNode { parent: Some(0), node: Attr("foo", "bar") }, FlattenNode { parent: Some(0), node: Attr("foo", "foo") }),
    //     //             Id(3, FlattenNode { parent: Some(0), node: Text("Hello World") })
    //     //             ]
    //     // );
    // }

    #[test]
    fn tree_node() {
        let attr_1 = attr("foo", "bar");
        let text_1 = text("Hello World");
        let root_a = element("tag1").append(attr_1).append(text_1).clone();

        let attr_2 = attr("foo", "foo");
        let text_2 = text("Hello You");
        let root_b = element("tag2")
            .append(attr_2)
            .append(text_2)
            .append(root_a)
            .clone();

        let mut a = Vec::new();

        root_b.flatten(None, &mut a);
        let mut fnl = FlatNodeList(a);
        let result = fnl.as_tree();
        println!("{:?}", result);
        assert!(false);
    }
}
