use std::str::FromStr;
use std::fmt::Display;
use std::path;

use egg::Id;

use egg::*;

use egg::FromOp;

define_language! {
    enum Expr {
        "0" = Zero,
        "succ" = Succ(Id),
        Var(Symbol),
        "+" = Add([Id; 2]),
        "*" = Mul([Id; 2]),
        "^" = Pow([Id; 2]),
    }
}

define_language! {
    enum Linalg {
        Cst(i64),
        "mat" = Mat([Id; 2]), // rows, cols
        "*" = Mul([Id; 2]),
    }
}

// set the cost of + to 10, * to 100 and ^ to 1000
struct ExprCost;
impl CostFunction<Expr> for ExprCost {
    type Cost = usize;

    fn cost<C>(&mut self, enode: &Expr, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost
    {
        let op_cost = match enode {
            Expr::Add(_) => 10,
            Expr::Mul(_) => 100,
            Expr::Pow(_) => 1000,
            _ => 1
        };
        enode.fold(op_cost, |sum, id| sum + costs(id))
    }
}

#[derive(Debug)]
#[derive(Clone)]
struct DimAndCost {
    rows: i64,
    cols: i64,
    cost: usize,
}

impl PartialEq for DimAndCost {
    fn eq(&self, other: &Self) -> bool {
        self.cost == other.cost
    }
}

impl PartialOrd for DimAndCost {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cost.cmp(&other.cost))
    }
}

struct LinalgCost; // cost is number of scalar multiplications, so for A (m x n) * B (n x p) it's m*n*p
impl CostFunction<Linalg> for LinalgCost {
    type Cost = DimAndCost;

    fn cost<C>(&mut self, enode: &Linalg, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost
    {
        match enode {
            Linalg::Cst(c) => DimAndCost { rows: *c, cols: *c, cost: 0 },
            Linalg::Mat([rows_id, cols_id]) => {
                let rows = costs(*rows_id).rows;
                let cols = costs(*cols_id).cols;
                DimAndCost { rows, cols, cost: 0 }
            },
            Linalg::Mul([a_id, b_id]) => {
                let a = costs(*a_id);
                let b = costs(*b_id);
                assert!(a.cols == b.rows);
                let cost = (a.rows * a.cols * b.cols) as usize;
                DimAndCost { rows: a.rows, cols: b.cols, cost: a.cost + b.cost + cost }
            }
        }
    }

}

fn render_egraph<L: Language + Display, N: Analysis<L>>(
    egraph: &EGraph<L, N>,
    dir: &str,
    name: &str,
) {
    // render the e-graph as a dot file
    let dot_filename = format!("{}/{}.dot", dir, name);
    let png_filename = format!("{}/{}.png", dir, name);

    let path = path::Path::new(&dot_filename);
    egraph.dot().to_dot(path).expect("Couldn't write e-graph to file");

    // render dot file into a png
    std::process::Command::new("dot")
        .arg("-Tpng")
        .arg(&dot_filename)
        .arg("-o")
        .arg(&png_filename)
        .output()
        .expect("Couldn't render dot file to png");
}

fn poly() -> (usize, RecExpr<Expr>) {
    let mut poly: RecExpr<Expr> = RecExpr::default();
    let c0 = poly.add(Expr::Zero);
    let c1 = poly.add(Expr::Succ(c0));
    let c2 = poly.add(Expr::Succ(c1));
    let c3 = poly.add(Expr::Succ(c2));
    let c4 = poly.add(Expr::Succ(c3));
    let c5 = poly.add(Expr::Succ(c4));
    let a = poly.add(Expr::Var(Symbol::from("a")));
    let b = poly.add(Expr::Var(Symbol::from("b")));
    let c = poly.add(Expr::Var(Symbol::from("c")));
    let d = poly.add(Expr::Var(Symbol::from("d")));
    let e = poly.add(Expr::Var(Symbol::from("e")));
    let f = poly.add(Expr::Var(Symbol::from("f")));
    let x = poly.add(Expr::Var(Symbol::from("x")));
    let bx = poly.add(Expr::Mul([b, x]));

    let poly1 = poly.add(Expr::Add([bx, a]));
    // print!("poly1: {}, cost: {}\n", poly, ExprCost.cost_rec(&poly));
    // assert!(ExprCost.cost_rec(&poly) == 113);

    let x2 = poly.add(Expr::Pow([x, c2]));
    let cx2 = poly.add(Expr::Mul([c, x2]));
    let poly2 = poly.add(Expr::Add([cx2, poly1]));
    // print!("poly2: {}, cost: {}\n", poly, ExprCost.cost_rec(&poly));
    // assert!(ExprCost.cost_rec(&poly) == 1228);

    let x3 = poly.add(Expr::Pow([x, c3]));
    let dx3 = poly.add(Expr::Mul([d, x3]));
    let poly3 = poly.add(Expr::Add([dx3, poly2]));
    // print!("poly3: {}, cost: {}\n", poly, ExprCost.cost_rec(&poly));

    let x4 = poly.add(Expr::Pow([x, c4]));
    let ex4 = poly.add(Expr::Mul([e, x4]));
    let poly4 = poly.add(Expr::Add([ex4, poly3]));
    // print!("poly4: {}, cost: {}\n", poly, ExprCost.cost_rec(&poly));

    let x5 = poly.add(Expr::Pow([x, c5]));
    let fx5 = poly.add(Expr::Mul([f, x5]));
    let poly5 = poly.add(Expr::Add([fx5, poly4]));
    // print!("poly5: {}, cost: {}\n", poly, ExprCost.cost_rec(&poly));
    // assert!(ExprCost.cost_rec(&poly) == 4579);

    let add_commutativity: Rewrite<Expr, ()> = rewrite!("add-comm"; "(+ ?a ?b)" => "(+ ?b ?a)");
    let mul_commutativity: Rewrite<Expr, ()> = rewrite!("mul-comm"; "(* ?a ?b)" => "(* ?b ?a)");
    let add_associativity1: Rewrite<Expr, ()> = rewrite!("add-assoc1"; "(+ (+ ?a ?b) ?c)" => "(+ ?a (+ ?b ?c))");
    let add_associativity2: Rewrite<Expr, ()> = rewrite!("add-assoc2"; "(+ ?a (+ ?b ?c))" => "(+ (+ ?a ?b) ?c)");
    let mul_associativity1: Rewrite<Expr, ()> = rewrite!("mul-assoc1"; "(* (* ?a ?b) ?c)" => "(* ?a (* ?b ?c))");
    let mul_associativity2: Rewrite<Expr, ()> = rewrite!("mul-assoc2"; "(* ?a (* ?b ?c))" => "(* (* ?a ?b) ?c)");
    let distributivity1: Rewrite<Expr, ()> = rewrite!("distrib1"; "(* ?x (+ ?a ?b))" => "(+ (* ?x ?a) (* ?x ?b))");
    let distributivity2: Rewrite<Expr, ()> = rewrite!("distrib2"; "(+ (* ?x ?a) (* ?x ?b))" => "(* ?x (+ ?a ?b))");
    let mul_id: Rewrite<Expr, ()> = rewrite!("mul-id"; "(* ?a (succ 0))" => "?a");
    let zero_exp: Rewrite<Expr, ()> = rewrite!("zero-exp"; "(^ ?a 0)" => "(succ 0)");
    let exp_def: Rewrite<Expr, ()> = rewrite!("exp-def"; "(^ ?a (succ ?n))" => "(* ?a (^ ?a ?n))");

    let rules = vec![
        add_commutativity,
        mul_commutativity,
        add_associativity1,
        add_associativity2,
        mul_associativity1,
        mul_associativity2,
        distributivity1,
        distributivity2,
        mul_id,
        zero_exp,
        exp_def,
    ];

    let mut egraph = EGraph::<Expr, ()>::default();
    let root = egraph.add_expr(&poly);
    // print!("Initial e-graph size: {}\n", egraph.total_size());
    // render_egraph(&egraph, "res", "initial");
    // egraph.rebuild();

    let runner = Runner::default()
        .with_egraph(egraph)
        .run(&rules);
    let new_egraph = runner.egraph;
    // new_egraph.rebuild();
    // print!("Final e-graph size: {}\n", new_egraph.total_size());
    // render_egraph(&new_egraph, "res", "final");

    let extractor = Extractor::new(&new_egraph, ExprCost);
    extractor.find_best(root)
}

fn mm(n: usize) -> (DimAndCost, RecExpr<Linalg>){
    let mut mm: RecExpr<Linalg> = RecExpr::default();

    let c10 = mm.add(Linalg::Cst(10));
    let mut m = mm.add(Linalg::Mat([c10, c10])); // 10x10
    for i in 1..=n {
        let mi = mm.add(Linalg::Mat([c10, c10])); // 10x10
        m = mm.add(Linalg::Mul([m, mi])); // 10x10 * ix10
    }

    let rules: Vec<Rewrite<Linalg, ()>> = rewrite!("assoc"; "(* (* ?a ?b) ?c)" <=> "(* ?a (* ?b ?c))");

    let mut egraph = EGraph::<Linalg, ()>::default();
    let root = egraph.add_expr(&mm);

    let runner = Runner::default()
        .with_egraph(egraph)
        .run(&rules);
    let new_egraph = runner.egraph;

    let extractor = Extractor::new(&new_egraph, LinalgCost);
    extractor.find_best(root)
}

fn median(v: &mut Vec<u128>) -> u128 {
    v.sort();
    let mid = v.len() / 2;
    if v.len() % 2 == 0 {
        (v[mid - 1] + v[mid]) / 2
    } else {
        v[mid]
    }
}

fn main() {
    let total_time = 60; // seconds
    {
        println!("## Benchmarking poly5 for 60 seconds.");
        let start = std::time::Instant::now();
        let mut times = vec![];
        let end = start + std::time::Duration::from_secs(total_time);
        while std::time::Instant::now() < end {
            let time_start = std::time::Instant::now();
            let (best_cost, best_expr) = poly();
            println!("Best expression: {}, cost: {}", best_expr, best_cost);
            let time_end = std::time::Instant::now();
            let duration = time_end.duration_since(time_start);
            times.push(duration.as_micros());
        }
        println!("Completed {} runs.", times.len());
        println!("Median time per iteration: {}ms", median(&mut times) as f64 / 1000.0);
    }

    for n in [3, 5, 10, 20, 40, 80] {
        println!("## Benchmarking {}mm for 60 seconds.", n);
        let mut times = vec![];
        let start = std::time::Instant::now();
        let end = start + std::time::Duration::from_secs(total_time);
        while std::time::Instant::now() < end {
            let time_start = std::time::Instant::now();
            let (_, best_expr) = mm(n);
            let time_end = std::time::Instant::now();
            let duration = time_end.duration_since(time_start);
            times.push(duration.as_micros());
        }
        println!("Completed {} runs.", times.len());
        println!("Median time per iteration: {}ms", median(&mut times) as f64 / 1000.0);
    }
}
