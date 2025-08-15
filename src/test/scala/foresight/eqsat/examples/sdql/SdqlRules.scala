package foresight.eqsat.examples.sdql

import foresight.eqsat.{EGraph, MixedTree, Slot}
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.{Pattern, PatternMatch}
import foresight.eqsat.metadata.EGraphWithMetadata
import ApplierOps._

/**
 * This object contains a collection of rewrite rules for the SDQL language.
 */
object SdqlRules {
  type SdqlEGraph = EGraphWithMetadata[SdqlIR, EGraph[SdqlIR]]
  type SdqlRule = Rule[SdqlIR, PatternMatch[SdqlIR], SdqlEGraph]

  def allOld: Seq[SdqlRule] = Seq(
    multAssoc1, multAssoc2, subIdentity, addZero, subZero, eqComm,
    multApp1, multApp2, addApp1, addApp2, subApp1, subApp2,
    getApp1, getApp2, singApp1, singApp2, uniqueApp1, uniqueApp2,
    letBinop3, letBinop4, letApply1, letApply2, ifMult2, ifToMult,
    multToIf, beta, sumFact1, sumFact2, sumFact3, singMult1, singMult2,
    singMult3, singMult4, sumFactInv1, sumSumVertFuse1, sumSumVertFuse2,
    sumRange1, sumMerge, getToSum, sumSing, uniqueRm
  )

  def allNew: Seq[SdqlRule] = Seq(
    multAssoc1, multAssoc2, subIdentity, addZero, subZero, eqComm,
    multApp1, multApp2, addApp1, addApp2, subApp1, subApp2,
    getApp1, getApp2, singApp1, singApp2, uniqueApp1, uniqueApp2,
    letBinop3, letBinop4, letApply1, letApply2, ifMult2, ifToMult,
    multToIf, beta, sumFact1, sumFact2, sumFact3, singMult1, singMult2,
    singMult3, singMult4, sumFactInv1, sumFactInv3, sumSumVertFuse1,
    sumSumVertFuse2, sumRange1, sumMerge, getToSum, sumToGet, getRange,
    sumSing, uniqueRm
  )

  val multAssoc1: SdqlRule = {
    val a = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val b = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val c = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "mult-assoc1",
      Mult(Mult(a, b), c).toSearcher,
      Mult(a, Mult(b, c)).toApplier
    )
  }

  val multAssoc2: SdqlRule = {
    val a = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val b = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val c = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "mult-assoc2",
      Mult(a, Mult(b, c)).toSearcher,
      Mult(Mult(a, b), c).toApplier
    )
  }

  val subIdentity: SdqlRule = {
    val e = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sub-identity",
      Sub(e, e).toSearcher,
      Num(0)().toApplier
    )
  }

  val addZero: SdqlRule = {
    val e = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "add-zero",
      Add(e, Num(0)()).toSearcher,
      e.toApplier
    )
  }

  val subZero: SdqlRule = {
    val e = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sub-zero",
      Sub(e, Num(0)()).toSearcher,
      e.toApplier
    )
  }

  val eqComm: SdqlRule = {
    val a = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val b = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "eq-comm",
      Equality(a, b).toSearcher,
      Equality(b, a).toApplier
    )
  }

  val multApp1: SdqlRule = {
    val a = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val b = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "mult-app1",
      Mult(a, b).toSearcher,
      Binop(Symbol("mult")(), a, b).toApplier
    )
  }

  val multApp2: SdqlRule = {
    val a = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val b = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "mult-app2",
      Binop(Symbol("mult")(), a, b).toSearcher,
      Mult(a, b).toApplier
    )
  }

  val addApp1: SdqlRule = {
    val a = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val b = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "add-app1",
      Add(a, b).toSearcher,
      Binop(Symbol("add")(), a, b).toApplier
    )
  }

  val addApp2: SdqlRule = {
    val a = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val b = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "add-app2",
      Binop(Symbol("add")(), a, b).toSearcher,
      Add(a, b).toApplier
    )
  }

  val subApp1: SdqlRule = {
    val a = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val b = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sub-app1",
      Sub(a, b).toSearcher,
      Binop(Symbol("sub")(), a, b).toApplier
    )
  }

  val subApp2: SdqlRule = {
    val a = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val b = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sub-app2",
      Binop(Symbol("sub")(), a, b).toSearcher,
      Sub(a, b).toApplier
    )
  }

  val getApp1: SdqlRule = {
    val a = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val b = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "get-app1",
      Get(a, b).toSearcher,
      Binop(Symbol("getf")(), a, b).toApplier
    )
  }

  val getApp2: SdqlRule = {
    val a = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val b = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "get-app2",
      Binop(Symbol("getf")(), a, b).toSearcher,
      Get(a, b).toApplier
    )
  }

  val singApp1: SdqlRule = {
    val a = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val b = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sing-app1",
      Sing(a, b).toSearcher,
      Binop(Symbol("singf")(), a, b).toApplier
    )
  }

  val singApp2: SdqlRule = {
    val a = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val b = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sing-app2",
      Binop(Symbol("singf")(), a, b).toSearcher,
      Sing(a, b).toApplier
    )
  }

  val uniqueApp1: SdqlRule = {
    val a = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "unique-app1",
      Unique(a).toSearcher,
      App(Symbol("uniquef")(), a).toApplier
    )
  }

  val uniqueApp2: SdqlRule = {
    val a = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "unique-app2",
      App(Symbol("uniquef")(), a).toSearcher,
      Unique(a).toApplier
    )
  }

  val letBinop3: SdqlRule = {
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e3 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val x = Slot.fresh()
    val f = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "let-binop3",
      Let(x, e1, Binop(f, e2, e3)).toSearcher,
      Binop(f, Let(x, e1, e2), Let(x, e1, e3)).toApplier
    )
  }

  val letBinop4: SdqlRule = {
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e3 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val x = Slot.fresh()
    val f = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "let-binop4",
      Binop(f, Let(x, e1, e2), Let(x, e1, e3)).toSearcher,
      Let(x, e1, Binop(f, e2, e3)).toApplier
    )
  }

  val letApply1: SdqlRule = {
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e3 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val x = Slot.fresh()
    Rule(
      "let-apply1",
      Let(x, e1, App(e2, e3)).toSearcher,
      App(e2, Let(x, e1, e3)).toApplier
    )
  }

  val letApply2: SdqlRule = {
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e3 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val x = Slot.fresh()
    Rule(
      "let-apply2",
      App(e2, Let(x, e1, e3)).toSearcher,
      Let(x, e1, App(e2, e3)).toApplier
    )
  }

  val ifMult2: SdqlRule = {
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e3 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "if-mult2",
      Mult(e1, IfThen(e2, e3)).toSearcher,
      IfThen(e2, Mult(e1, e3)).toApplier
    )
  }

  val ifToMult: SdqlRule = {
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "if-to-mult",
      IfThen(e1, e2).toSearcher,
      Mult(e1, e2).toApplier
    )
  }

  val multToIf: SdqlRule = {
    val e1_1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e1_2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "mult-to-if",
      Mult(Equality(e1_1, e1_2), e2).toSearcher,
      IfThen(Equality(e1_1, e1_2), e2).toApplier
    )
  }

  val beta: SdqlRule = {
    val t = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val body = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val x = Slot.fresh()
    Rule(
      "beta",
      Let(x, t, body).toSearcher,
      body
        .toApplier[SdqlEGraph]
        .substitute(body.atom, x, t.atom, body.atom)
    )
  }

  val sumFact1: SdqlRule = {
    val r = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val x = Slot.fresh()
    val y = Slot.fresh()
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sum-fact-1",
      Sum(x, y, r, Mult(e1, e2))
        .toSearcher
        .filter((subst, _) => {
          val slotSet = subst(e1.atom).slotSet
          !slotSet.contains(subst(x)) && !slotSet.contains(subst(y))
        }),
      Mult(e1, Sum(x, y, r, e2)).toApplier
    )
  }

  val sumFact2: SdqlRule = {
    val r = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val x = Slot.fresh()
    val y = Slot.fresh()
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sum-fact-2",
      Sum(x, y, r, Mult(e1, e2))
        .toSearcher
        .filter((subst, _) => {
          val slotSet = subst(e2.atom).slotSet
          !slotSet.contains(subst(x)) && !slotSet.contains(subst(y))
        }),
      Mult(Sum(x, y, r, e1), e2).toApplier
    )
  }

  val sumFact3: SdqlRule = {
    val r = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val x = Slot.fresh()
    val y = Slot.fresh()
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sum-fact-3",
      Sum(x, y, r, Sing(e1, e2))
        .toSearcher
        .filter((subst, _) => {
          val slotSet = subst(e1.atom).slotSet
          !slotSet.contains(subst(x)) && !slotSet.contains(subst(y))
        }),
      Sing(e1, Sum(x, y, r, e2)).toApplier
    )
  }

  val singMult1: SdqlRule = {
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e3 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sing-mult-1",
      Sing(e1, Mult(e2, e3)).toSearcher,
      Mult(Sing(e1, e2), e3).toApplier
    )
  }

  val singMult2: SdqlRule = {
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e3 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sing-mult-2",
      Sing(e1, Mult(e2, e3)).toSearcher,
      Mult(e2, Sing(e1, e3)).toApplier
    )
  }

  val singMult3: SdqlRule = {
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e3 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sing-mult-3",
      Mult(Sing(e1, e2), e3).toSearcher,
      Sing(e1, Mult(e2, e3)).toApplier
    )
  }

  val singMult4: SdqlRule = {
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e3 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sing-mult-4",
      Mult(e2, Sing(e1, e3)).toSearcher,
      Sing(e1, Mult(e2, e3)).toApplier
    )
  }

  val sumFactInv1: SdqlRule = {
    val r = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val k = Slot.fresh()
    val v = Slot.fresh()
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sum-fact-inv-1",
      Mult(e1, Sum(k, v, r, e2)).toSearcher,
      Sum(k, v, r, Mult(e1, e2)).toApplier
    )
  }

  val sumFactInv3: SdqlRule = {
    val r = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val k = Slot.fresh()
    val v = Slot.fresh()
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val e2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sum-fact-inv-3",
      Sing(e1, Sum(k, v, r, e2)).toSearcher,
      Sum(k, v, r, Sing(e1, e2)).toApplier
    )
  }

  val sumSumVertFuse1: SdqlRule = {
    val r = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val k1 = Slot.fresh()
    val v1 = Slot.fresh()
    val k2 = Slot.fresh()
    val v2 = Slot.fresh()
    val body1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val body2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sum-sum-vert-fuse-1",
      Sum(k1, v1, Sum(k2, v2, r, Sing(Var(k2), body1)), body2).toSearcher,
      Sum(k2, v2, r, Let(k1, Var(k2), Let(v1, body1, body2))).toApplier
    )
  }

  val sumSumVertFuse2: SdqlRule = {
    val r = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val k1 = Slot.fresh()
    val v1 = Slot.fresh()
    val k2 = Slot.fresh()
    val v2 = Slot.fresh()
    val key = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val body1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val body2 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sum-sum-vert-fuse-2",
      Sum(k1, v1, Sum(k2, v2, r, Sing(Unique(key), body1)), body2).toSearcher,
      Sum(k2, v2, r, Let(k1, Unique(key), Let(v1, body1, body2))).toApplier
    )
  }

  val sumRange1: SdqlRule = {
    val st = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val en = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val k = Slot.fresh()
    val v = Slot.fresh()
    val key = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val body = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sum-range-1",
      Sum(k, v, Range(st, en), IfThen(Equality(Var(v), key), body)).toSearcher,
      Sum(
        k,
        v,
        Range(st, en),
        IfThen(Equality(Var(k), Sub(key, Sub(st, Num(1)()))), body)
      ).toApplier
    )
  }

  val sumMerge: SdqlRule = {
    val r = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val s = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val k1 = Slot.fresh()
    val v1 = Slot.fresh()
    val k2 = Slot.fresh()
    val v2 = Slot.fresh()
    val body = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sum-merge",
      Sum(k1, v1, r, Sum(k2, v2, s, IfThen(Equality(Var(v1), Var(v2)), body))).toSearcher,
      Merge(k1, k2, v1, r, s, Let(v2, Var(v1), body)).toApplier
    )
  }

  val getToSum: SdqlRule = {
    val dict = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val key = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val k = Slot.fresh()
    val v = Slot.fresh()
    Rule(
      "get-to-sum",
      Get(dict, key).toSearcher,
      Sum(k, v, dict, IfThen(Equality(Var(k), key), Var(v))).toApplier
    )
  }

  val sumToGet: SdqlRule = {
    val dict = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val key = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val k = Slot.fresh()
    val v = Slot.fresh()
    val body = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "sum-to-get",
      Sum(k, v, dict, IfThen(Equality(Var(k), key), body))
        .toSearcher
        .filter((subst, _) => {
          val keySlots = subst(key.atom).slotSet
          !keySlots.contains(subst(k)) && !keySlots.contains(subst(v))
        }),
      Let(k, key, Let(v, Get(dict, Var(k)), body)).toApplier
    )
  }

  val getRange: SdqlRule = {
    val st = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val en = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val idx = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "get-range",
      Get(Range(st, en), idx).toSearcher,
      Add(idx, Sub(st, Num(1)())).toApplier
    )
  }

  val sumSing: SdqlRule = {
    val e1 = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    val k = Slot.fresh()
    val v = Slot.fresh()
    Rule(
      "sum-sing",
      Sum(k, v, e1, Sing(Var(k), Var(v))).toSearcher,
      e1.toApplier
    )
  }

  val uniqueRm: SdqlRule = {
    val e = MixedTree.Atom(Pattern.Var.fresh[SdqlIR]())
    Rule(
      "unique-rm",
      Unique(e).toSearcher,
      e.toApplier
    )
  }
}