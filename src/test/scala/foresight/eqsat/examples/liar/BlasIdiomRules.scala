package foresight.eqsat.examples.liar

import SearcherOps.SearcherOfMetadataPatternMatchOps
import foresight.eqsat.examples.liar.ApplierOps.PatternApplierOps
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.{EGraph, EGraphLike, MixedTree, Slot}
import foresight.eqsat.rewriting.{Applier, Rule}
import foresight.eqsat.rewriting.patterns.{Pattern, PatternApplier, PatternMatch}

final case class BlasIdiomRules[BaseEGraph <: EGraphLike[ArrayIR, BaseEGraph] with EGraph[ArrayIR]]() {
  type MetadataEGraph = EGraphWithMetadata[ArrayIR, BaseEGraph]
  type LiarRule = Rule[ArrayIR, PatternMatch[ArrayIR], MetadataEGraph]

  def all: Seq[LiarRule] = oneWayRules.map(typeChecked) ++ oneWayRules.map(_.tryReverse.get).map(typeChecked)

  def oneWayRules: Seq[LiarRule] = detectionRules ++ transformationRules

  def detectionRules: Seq[LiarRule] = Seq(
    detectMemsetZero,
    detectDot,
    detectAxpy,
    detectGemv,
    detectGemm,
    detectTranspose)

  def transformationRules: Seq[LiarRule] = {
    Seq(hoistLhsMulFromDot) ++ foldTransposeIntoGemv ++ foldTransposeAIntoGemm ++ foldTransposeBIntoGemm
  }

  val detectMemsetZero: LiarRule = {
    // build N (λi. 0.0) -> memset N 0.0
    val N = Pattern.Var.fresh()

    val zero = ConstDouble(0.0).toTree
    val i = Slot.fresh()

    Rule(
      "build N (λi. 0.0) -> memset N 0",
      Build(
        MixedTree.Atom(N),
        Lambda(
          i,
          Int32Type.toTree,
          ConstDouble(0.0).toTree))
        .toSearcher[MetadataEGraph],
      BlasIdioms.Memset(MixedTree.Atom(N), zero).toApplier)
  }

  val detectDot: LiarRule = {
    // ifold N (λi acc. xs[i] * ys[i] + acc) 0.0 -> ddot xs ys
    val N = Pattern.Var.fresh()
    val scalarType = Pattern.Var.fresh()

    val xs = Pattern.Var.fresh()
    val ys = Pattern.Var.fresh()
    val acc = Slot.fresh()
    val i = Slot.fresh()

    Rule(
      "ifold N (λi acc. xs[i] * ys[i] + acc) 0.0 -> ddot xs ys",
      Ifold(
        MixedTree.Atom(N),
        ConstDouble(0.0).toTree,
        Lambda(
          i,
          Int32Type.toTree,
          Lambda(
            acc,
            MixedTree.Atom(scalarType),
            Add(
              Mul(
                IndexAt(MixedTree.Atom(xs), Var(i, Int32Type.toTree)),
                IndexAt(MixedTree.Atom(ys), Var(i, Int32Type.toTree))),
              Var(acc, MixedTree.Atom(scalarType))))))
        .toSearcher[MetadataEGraph]
        .requireIndependent(xs, i, acc)
        .requireIndependent(ys, i, acc)
        .requireTypes(Map(
          xs -> ArrayType(MixedTree.Atom(scalarType), MixedTree.Atom(N)),
          ys -> ArrayType(MixedTree.Atom(scalarType), MixedTree.Atom(N)))),
      BlasIdioms.Dot(MixedTree.Atom(xs), MixedTree.Atom(ys)).toApplier)
  }

  val detectAxpy: LiarRule = {
    // build N (λi. a * xs[i] + ys[i]) -> axpy a xs ys
    val N = Pattern.Var.fresh()
    val scalarType = Pattern.Var.fresh()

    val a = Pattern.Var.fresh()
    val xs = Pattern.Var.fresh()
    val ys = Pattern.Var.fresh()
    val i = Slot.fresh()

    Rule(
      "build N (λi. a * xs[i] + ys[i]) -> axpy a xs ys",
      Build(
        MixedTree.Atom(N),
        Lambda(
          i,
          Int32Type.toTree,
          Add(
            Mul(
              MixedTree.Atom(a),
              IndexAt(MixedTree.Atom(xs), Var(i, Int32Type.toTree))),
            IndexAt(MixedTree.Atom(ys), Var(i, Int32Type.toTree)))))
        .toSearcher[MetadataEGraph]
        .requireIndependent(a, i)
        .requireIndependent(xs, i)
        .requireIndependent(ys, i)
        .requireTypes(Map(
          a -> MixedTree.Atom(scalarType),
          xs -> ArrayType(MixedTree.Atom(scalarType), MixedTree.Atom(N)),
          ys -> ArrayType(MixedTree.Atom(scalarType), MixedTree.Atom(N)))),
      BlasIdioms.Axpy(MixedTree.Atom(a), MixedTree.Atom(xs), MixedTree.Atom(ys)).toApplier)
  }

  val detectGemv: LiarRule = {
    // build N (λi. alpha * (dot a[i] x) + beta * y[i]) -> gemv alpha a x beta y
    val N = Pattern.Var.fresh()
    val K = Pattern.Var.fresh()
    val scalarType = Pattern.Var.fresh()

    val alpha = Pattern.Var.fresh()
    val a = Pattern.Var.fresh()
    val x = Pattern.Var.fresh()
    val beta = Pattern.Var.fresh()
    val y = Pattern.Var.fresh()

    val i = Slot.fresh()

    Rule(
      "build K (λi. alpha * (dot a[i] x) + beta * y[i]) -> gemv alpha a x beta y",
      Build(
        MixedTree.Atom(K),
        Lambda(
          i,
          Int32Type.toTree,
          Add(
            Mul(
              MixedTree.Atom(alpha),
              BlasIdioms.Dot(IndexAt(MixedTree.Atom(a), Var(i, Int32Type.toTree)), MixedTree.Atom(x))),
            Mul(
              MixedTree.Atom(beta),
              IndexAt(MixedTree.Atom(y), Var(i, Int32Type.toTree))))))
        .toSearcher[MetadataEGraph]
        .requireIndependent(alpha, i)
        .requireIndependent(a, i)
        .requireIndependent(x, i)
        .requireIndependent(beta, i)
        .requireIndependent(y, i)
        .requireTypes(Map(
          alpha -> MixedTree.Atom(scalarType),
          a -> ArrayType(ArrayType(MixedTree.Atom(scalarType), MixedTree.Atom(N)), MixedTree.Atom(K)),
          x -> ArrayType(MixedTree.Atom(scalarType), MixedTree.Atom(N)),
          beta -> MixedTree.Atom(scalarType),
          y -> ArrayType(MixedTree.Atom(scalarType), MixedTree.Atom(K)))),
      BlasIdioms.Gemv(false)(
        MixedTree.Atom(alpha),
        MixedTree.Atom(a),
        MixedTree.Atom(x),
        MixedTree.Atom(beta),
        MixedTree.Atom(y)).toApplier)
  }

  val detectGemm: LiarRule = {
    // build M (λi. gemvN alpha b a[i] beta c[i]) -> gemmNT alpha a b beta c
    val M = Pattern.Var.fresh()
    val N = Pattern.Var.fresh()
    val K = Pattern.Var.fresh()
    val scalarType = Pattern.Var.fresh()

    val alpha = Pattern.Var.fresh()
    val b = Pattern.Var.fresh()
    val a = Pattern.Var.fresh()
    val beta = Pattern.Var.fresh()
    val c = Pattern.Var.fresh()

    val i = Slot.fresh()

    Rule(
      "build M (λi. gemvN alpha b a[i] beta c[i]) -> gemmNT alpha a b beta c",
      Build(
        MixedTree.Atom(M),
        Lambda(
          i,
          Int32Type.toTree,
          BlasIdioms.Gemv(false)(
            MixedTree.Atom(alpha),
            MixedTree.Atom(b),
            IndexAt(MixedTree.Atom(a), Var(i, Int32Type.toTree)),
            MixedTree.Atom(beta),
            IndexAt(MixedTree.Atom(c), Var(i, Int32Type.toTree)))))
        .toSearcher[MetadataEGraph]
        .requireIndependent(alpha, i)
        .requireIndependent(a, i)
        .requireIndependent(b, i)
        .requireIndependent(beta, i)
        .requireIndependent(c, i)
        .requireTypes(Map(
          alpha -> MixedTree.Atom(scalarType),
          a -> ArrayType(ArrayType(MixedTree.Atom(scalarType), MixedTree.Atom(N)), MixedTree.Atom(M)),
          b -> ArrayType(ArrayType(MixedTree.Atom(scalarType), MixedTree.Atom(N)), MixedTree.Atom(K)),
          beta -> MixedTree.Atom(scalarType),
          c -> ArrayType(ArrayType(MixedTree.Atom(scalarType), MixedTree.Atom(K)), MixedTree.Atom(M)))),
      BlasIdioms.Gemm(aTransposed = false, bTransposed = true)(
        MixedTree.Atom(alpha),
        MixedTree.Atom(a),
        MixedTree.Atom(b),
        MixedTree.Atom(beta),
        MixedTree.Atom(c)).toApplier)
  }

  def detectTranspose: LiarRule = {
    // build N (λi. build M (λj. a[i][j])) -> transpose a

    val N = Pattern.Var.fresh()
    val M = Pattern.Var.fresh()

    val a = Pattern.Var.fresh()

    val i = Slot.fresh()
    val j = Slot.fresh()

    Rule(
      "build N (λi. build M (λj. a[i][j])) -> transpose a",
      Build(
        MixedTree.Atom(N),
        Lambda(
          i,
          Int32Type.toTree,
          Build(
            MixedTree.Atom(M),
            Lambda(
              j,
              Int32Type.toTree,
              IndexAt(IndexAt(MixedTree.Atom(a), Var(i, Int32Type.toTree)), Var(j, Int32Type.toTree))))))
        .toSearcher[MetadataEGraph]
        .requireIndependent(a, i, j)
        .requireTypes(Map(
          a -> ArrayType(ArrayType(DoubleType.toTree, MixedTree.Atom(N)), MixedTree.Atom(M)))),
      BlasIdioms.Transpose(MixedTree.Atom(a)).toApplier)
  }

  def hoistLhsMulFromDot: LiarRule = {
    // dot (build N (λi. a * xs[i])) ys -> a * (dot xs ys)
    val N = Pattern.Var.fresh()
    val scalarType = Pattern.Var.fresh()

    val xs = Pattern.Var.fresh()
    val ys = Pattern.Var.fresh()
    val a = Pattern.Var.fresh()

    val i = Slot.fresh()

    Rule(
      "dot (build N (λi. a * xs[i])) ys -> a * (dot xs ys)",
      BlasIdioms.Dot(
        Build(
          MixedTree.Atom(N),
          Lambda(
            i,
            Int32Type.toTree,
            Mul(
              MixedTree.Atom(a),
              IndexAt(MixedTree.Atom(xs), Var(i, Int32Type.toTree))))),
        MixedTree.Atom(ys))
        .toSearcher[MetadataEGraph]
        .requireIndependent(a, i)
        .requireIndependent(xs, i)
        .requireTypes(Map(
          a -> MixedTree.Atom(scalarType),
          xs -> ArrayType(MixedTree.Atom(scalarType), MixedTree.Atom(N)),
          ys -> ArrayType(MixedTree.Atom(scalarType), MixedTree.Atom(N)))),
      Mul(MixedTree.Atom(a), BlasIdioms.Dot(MixedTree.Atom(xs), MixedTree.Atom(ys))).toApplier)
  }

  def foldTransposeIntoGemv: Seq[LiarRule] = {
    // gemvX alpha (transpose a) x beta y -> gemv(not X) alpha a x beta y

    for (transposition <- Seq(true, false)) yield {
      val alpha = Pattern.Var.fresh()
      val a = Pattern.Var.fresh()
      val x = Pattern.Var.fresh()
      val beta = Pattern.Var.fresh()
      val y = Pattern.Var.fresh()

      val transposed = BlasIdioms.transpositionToString(transposition)
      val notTransposed = BlasIdioms.transpositionToString(!transposition)

      Rule(
        s"gemv$transposed alpha (transpose a) x beta y -> gemv$notTransposed alpha a x beta y",
        BlasIdioms.Gemv(transposition)(
            MixedTree.Atom(alpha),
            BlasIdioms.Transpose(MixedTree.Atom(a)),
            MixedTree.Atom(x),
            MixedTree.Atom(beta),
            MixedTree.Atom(y))
          .toSearcher[MetadataEGraph],
        BlasIdioms.Gemv(!transposition)(
          MixedTree.Atom(alpha),
          MixedTree.Atom(a),
          MixedTree.Atom(x),
          MixedTree.Atom(beta),
          MixedTree.Atom(y))
          .toApplier[MetadataEGraph])
    }
  }

  def foldTransposeAIntoGemm: Seq[LiarRule] = {
    // gemmXY alpha (transpose a) b beta c -> gemm(not X)Y alpha a b beta c
    val alpha = Pattern.Var.fresh()
    val a = Pattern.Var.fresh()
    val b = Pattern.Var.fresh()
    val beta = Pattern.Var.fresh()
    val c = Pattern.Var.fresh()

    for ((aTransposed, bTransposed) <- Seq((false, false), (false, true), (true, false), (true, true))) yield {
      val transpositionA = BlasIdioms.transpositionToString(aTransposed)
      val transpositionB = BlasIdioms.transpositionToString(bTransposed)
      val notTranspositionA = BlasIdioms.transpositionToString(!aTransposed)

      Rule(
        s"gemm$transpositionA$transpositionB alpha (transpose a) b beta c -> gemm$notTranspositionA$transpositionB alpha a b beta c",
        BlasIdioms.Gemm(aTransposed, bTransposed)(
          MixedTree.Atom(alpha),
          BlasIdioms.Transpose(MixedTree.Atom(a)),
          MixedTree.Atom(b),
          MixedTree.Atom(beta),
          MixedTree.Atom(c))
          .toSearcher[MetadataEGraph],
        BlasIdioms.Gemm(!aTransposed, bTransposed)(
          MixedTree.Atom(alpha),
          MixedTree.Atom(a),
          MixedTree.Atom(b),
          MixedTree.Atom(beta),
          MixedTree.Atom(c))
          .toApplier[MetadataEGraph])
    }
  }

  def foldTransposeBIntoGemm: Seq[LiarRule] = {
    // gemmXY alpha a (transpose b) beta c -> gemmX(not Y) alpha a b beta c
    val alpha = Pattern.Var.fresh()
    val a = Pattern.Var.fresh()
    val b = Pattern.Var.fresh()
    val beta = Pattern.Var.fresh()
    val c = Pattern.Var.fresh()

    for ((aTransposed, bTransposed) <- Seq((false, false), (false, true), (true, false), (true, true))) yield {
      val transpositionA = BlasIdioms.transpositionToString(aTransposed)
      val transpositionB = BlasIdioms.transpositionToString(bTransposed)
      val notTranspositionB = BlasIdioms.transpositionToString(!bTransposed)

      Rule(
        s"gemm$transpositionA$transpositionB alpha a (transpose b) beta c -> gemm$transpositionA$notTranspositionB alpha a b beta c",
        BlasIdioms.Gemm(aTransposed, bTransposed)(
            MixedTree.Atom(alpha),
            MixedTree.Atom(a),
            BlasIdioms.Transpose(MixedTree.Atom(b)),
            MixedTree.Atom(beta),
            MixedTree.Atom(c))
          .toSearcher[MetadataEGraph],
        BlasIdioms.Gemm(aTransposed, !bTransposed)(
            MixedTree.Atom(alpha),
            MixedTree.Atom(a),
            MixedTree.Atom(b),
            MixedTree.Atom(beta),
            MixedTree.Atom(c))
          .toApplier[MetadataEGraph])
    }
  }

  private def typeChecked(rule: LiarRule): LiarRule = {
    def addTypeCheckingToApplier(applier: Applier[ArrayIR, PatternMatch[ArrayIR], MetadataEGraph]): Applier[ArrayIR, PatternMatch[ArrayIR], MetadataEGraph] = {
      applier match {
        case p: PatternApplier[ArrayIR, MetadataEGraph] => p.typeChecked
        case filter: Applier.Filter[ArrayIR, PatternMatch[ArrayIR], MetadataEGraph] =>
          Applier.Filter(addTypeCheckingToApplier(filter.applier), filter.filter)

        case requirements: TypeRequirements.ApplierWithRequirements[BaseEGraph] =>
          TypeRequirements.ApplierWithRequirements(
            addTypeCheckingToApplier(requirements.applier),
            requirements.types)

        case _ => throw new IllegalArgumentException(
          s"Cannot add type checking to applier of type ${applier.getClass.getName}")
      }
    }

    Rule(rule.name, rule.searcher, addTypeCheckingToApplier(rule.applier))
  }
}
