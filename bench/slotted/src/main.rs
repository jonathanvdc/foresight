use std::fmt::format;

use slotted_egraphs::*;

define_language! {
    enum Expr {
        Zero() = "0",
        Succ(AppliedId) = "succ",
        Var(Symbol),

        Add(AppliedId, AppliedId) = "+",
        Mul(AppliedId, AppliedId) = "*",
        Pow(AppliedId, AppliedId) = "^",
    }
}

define_language! {
    enum Linalg {
        Mat() = "mat",
        Mul(AppliedId, AppliedId) = "*",
    }
}

// set the cost of + to 10, * to 100 and ^ to 1000
struct ExprCost;
impl CostFunction<Expr> for ExprCost {
    type Cost = usize;

    fn cost<C>(&self, enode: &Expr, costs: C) -> usize
    where
        C: Fn(Id) -> usize,
    {
        match enode {
            Expr::Zero() | Expr::Var(_) => 1,
            Expr::Succ(x) => 1 + costs(x.id),
            Expr::Add(x, y) => 10 + costs(x.id) + costs(y.id),
            Expr::Mul(x, y) => 100 + costs(x.id) + costs(y.id),
            Expr::Pow(x, y) => 1000 + costs(x.id) + costs(y.id),
        }
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

impl Eq for DimAndCost {}

impl PartialOrd for DimAndCost {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cost.cmp(&other.cost))
    }
}

impl Ord for DimAndCost {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.cost.cmp(&other.cost)
    }
}

struct LinalgCost; // cost is number of scalar multiplications, so for A (m x n) * B (n x p) it's m*n*p
impl CostFunction<Linalg> for LinalgCost {
    type Cost = DimAndCost;

    fn cost<C>(&self, enode: &Linalg, costs: C) -> Self::Cost
    where
        C: Fn(Id) -> Self::Cost
    {
        match enode {
            Linalg::Mat() => {
                DimAndCost { rows: 10, cols: 10, cost: 0 }
            },
            Linalg::Mul(x, y) => {
                let a = costs(x.id);
                let b = costs(y.id);
                assert!(a.cols == b.rows);
                let cost = (a.rows * a.cols * b.cols) as usize;
                DimAndCost { rows: a.rows, cols: b.cols, cost: a.cost + b.cost + cost }
            }
        }
    }

}

fn poly() {
    let c0 = "0";
    let c1 = format!("(succ {c0})");
    let c2 = format!("(succ {c1})");
    let c3 = format!("(succ {c2})");
    let c4 = format!("(succ {c3})");
    let c5 = format!("(succ {c4})");

    let bx = format!("(* b x)");
    let poly1 = format!("(+ {bx} a)");
    // println!("poly1: {}", RecExpr::<Expr>::parse(&poly1).unwrap());
    let cx2 = format!("(* c (^ x {c2}))");
    let poly2 = format!("(+ {cx2} {poly1})");
    // println!("poly2: {}", RecExpr::<Expr>::parse(&poly2).unwrap
    let dx3 = format!("(* d (^ x {c3}))");
    let poly3 = format!("(+ {dx3} {poly2})");
    // println!("poly3: {}", RecExpr::<Expr>::parse(&poly3).unwrap
    let ex4 = format!("(* e (^ x {c4}))");
    let poly4 = format!("(+ {ex4} {poly3})");
    // println!("poly4: {}", RecExpr::<Expr>::parse(&poly4).unwrap
    let fx5 = format!("(* f (^ x {c5}))");
    let poly5 = format!("(+ {fx5} {poly4})");
    let poly = RecExpr::<Expr>::parse(&poly5).unwrap();
    // println!("poly5: {}, cost: {}", poly, ExprCost.cost_rec(&poly));
    // assert!(ExprCost.cost_rec(&poly) == 4579);

    let add_commutativity: Rewrite<Expr, ()> = Rewrite::new("add-comm", "(+ ?a ?b)", "(+ ?b ?a)");
    let mul_commutativity: Rewrite<Expr, ()> = Rewrite::new("mul-comm", "(* ?a ?b)", "(* ?b ?a)");
    let add_associativity1: Rewrite<Expr, ()> = Rewrite::new("add-assoc1", "(+ (+ ?a ?b) ?c)", "(+ ?a (+ ?b ?c))");
    let add_associativity2: Rewrite<Expr, ()> = Rewrite::new("add-assoc2", "(+ ?a (+ ?b ?c))", "(+ (+ ?a ?b) ?c)");
    let mul_associativity1: Rewrite<Expr, ()> = Rewrite::new("mul-assoc1", "(* (* ?a ?b) ?c)", "(* ?a (* ?b ?c))");
    let mul_associativity2: Rewrite<Expr, ()> = Rewrite::new("mul-assoc2", "(* ?a (* ?b ?c))", "(* (* ?a ?b) ?c)");
    let distributivity1: Rewrite<Expr, ()> = Rewrite::new("distrib1", "(* ?x (+ ?a ?b))", "(+ (* ?x ?a) (* ?x ?b))");
    let distributivity2: Rewrite<Expr, ()> = Rewrite::new("distrib2", "(+ (* ?x ?a) (* ?x ?b))", "(* ?x (+ ?a ?b))");
    let mul_id: Rewrite<Expr, ()> = Rewrite::new("mul-id", "(* ?a (succ 0))", "?a");
    let zero_exp: Rewrite<Expr, ()> = Rewrite::new("zero-exp", "(^ ?a 0)", "(succ 0)");
    let exp_def: Rewrite<Expr, ()> = Rewrite::new("exp-def", "(^ ?a (succ ?n))", "(* ?a (^ ?a ?n))");

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
    let root = egraph.add_expr(poly);
    // print!("Initial e-graph size: {}\n", egraph.total_number_of_nodes());

    let mut runner = Runner::<Expr, ()>::default().with_egraph(egraph);
    let report = runner.run(&rules);
    // print!("Final e-graph size: {}\n", runner.egraph.total_number_of_nodes());

    let extractor = Extractor::new(&runner.egraph, ExprCost);
    let best_expr = extractor.extract(&root, &runner.egraph);
    let best_cost = ExprCost.cost_rec(&best_expr);
    // print!("Best expression: {}, cost: {}\n", best_expr, best_cost);
}

fn mm(n: usize) {
    let mut m = "(mat)".to_string();
    for _ in 1..=n {
        m = format!("(* {m} (mat))");
    }
    let mm = RecExpr::<Linalg>::parse(&m).unwrap();
    // println!("MM{n}: {}, cost: {}", mm, LinalgCost.cost_rec(&mm).cost);

    let rules: Vec<Rewrite<Linalg, ()>> = vec![
        Rewrite::new("assoc1", "(* (* ?a ?b) ?c)", "(* ?a (* ?b ?c))"),
        Rewrite::new("assoc2", "(* ?a (* ?b ?c))", "(* (* ?a ?b) ?c)"),
    ];

    let mut egraph = EGraph::<Linalg, ()>::default();
    let root = egraph.add_expr(mm);
    // println!("Initial e-graph size: {}", egraph.total_number_of_nodes());

    let mut runner = Runner::<Linalg, ()>::default().with_egraph(egraph);
    let report = runner.run(&rules);
    // println!("Final e-graph size: {}", runner.egraph.total_number_of_nodes());

    let extractor = Extractor::new(&runner.egraph, LinalgCost);
    let best_expr = extractor.extract(&root, &runner.egraph);
    let best_cost = LinalgCost.cost_rec(&best_expr);
    // println!("Best expression: {}, cost: {}\n", best_expr, best_cost.cost);
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
    let total_time = 10; // seconds
    {
        println!("## Benchmarking poly5 for 60 seconds.");
        let start = std::time::Instant::now();
        let mut times = vec![];
        let end = start + std::time::Duration::from_secs(total_time);
        while std::time::Instant::now() < end {
            let time_start = std::time::Instant::now();
            poly();
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
            mm(n);
            let time_end = std::time::Instant::now();
            let duration = time_end.duration_since(time_start);
            times.push(duration.as_micros());
        }

        println!("Completed {} runs.", times.len());
        println!("Median time per iteration: {}ms", median(&mut times) as f64 / 1000.0);
    }
}
