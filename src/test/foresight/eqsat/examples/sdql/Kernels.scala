package foresight.eqsat.examples.sdql

import foresight.eqsat.{EClassCall, MixedTree, Slot}

object Kernels {
  def mmm1: MixedTree[SdqlIR, EClassCall] = {
    val A = Slot.fresh()
    val B = Slot.fresh()
    val i_Bi_key = Slot.fresh()
    val i_Bi_val = Slot.fresh()
    val k_Bik_key = Slot.fresh()
    val k_Bik_val = Slot.fresh()
    val j_Ckj_key = Slot.fresh()
    val j_Ckj_val = Slot.fresh()

    Lam(A,
      Lam(B,
        Sum(
          i_Bi_key,
          i_Bi_val,
          Var(A),
          Sum(
            k_Bik_key,
            k_Bik_val,
            Var(i_Bi_val),
            Sum(
              j_Ckj_key,
              j_Ckj_val,
              Get(
                Var(B),
                Var(k_Bik_key)
              ),
              Sing(
                Var(i_Bi_key),
                Sing(
                  Var(j_Ckj_key),
                  Mult(
                    Var(k_Bik_val),
                    Var(j_Ckj_val)
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  def mmm2: MixedTree[SdqlIR, EClassCall] = {
    val var01 = Slot.fresh()
    val var02 = Slot.fresh()
    val var03 = Slot.fresh()
    val var04 = Slot.fresh()
    val var05 = Slot.fresh()
    val var06 = Slot.fresh()
    val var07 = Slot.fresh()
    val var08 = Slot.fresh()
    val var09 = Slot.fresh()
    val var10 = Slot.fresh()
    val var11 = Slot.fresh()
    val var12 = Slot.fresh()
    val var13 = Slot.fresh()
    val var14 = Slot.fresh()
    val var15 = Slot.fresh()

    Lam(var01,
      Lam(var02,
        Lam(var03,
          Lam(var04,
            Lam(var05,
              Lam(var06,
                Sum(
                  var07,
                  var08,
                  Sum(
                    var07,
                    var08,
                    Var(var01),
                    Let(
                      var09,
                      Get(Var(var01), Add(Var(var07), Num(1)())),
                      Sing(
                        Var(var07),
                        Sum(
                          var10,
                          var11,
                          SubArray(
                            Var(var02),
                            Var(var08),
                            Sub(Var(var09), Num(1)())
                          ),
                          Sing(
                            Unique(Var(var11)),
                            Get(Var(var03), Var(var10))
                          )
                        )
                      )
                    )
                  ),
                  Sing(
                    Var(var07),
                    Sum(
                      var09,
                      var10,
                      Var(var08),
                      Mult(
                        Var(var10),
                        Get(
                          Sum(
                            var11,
                            var12,
                            Var(var04),
                            Let(
                              var13,
                              Get(Var(var04), Add(Var(var11), Num(1)())),
                              Sing(
                                Var(var11),
                                Sum(
                                  var14,
                                  var15,
                                  SubArray(
                                    Var(var05),
                                    Var(var12),
                                    Sub(Var(var13), Num(1)())
                                  ),
                                  Sing(
                                    Unique(Var(var15)),
                                    Get(Var(var06), Var(var14))
                                  )
                                )
                              )
                            )
                          ),
                          Var(var09)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  def mmmSum1: MixedTree[SdqlIR, EClassCall] = {
    val A = Slot.fresh()
    val B = Slot.fresh()
    val k_Bk_key = Slot.fresh()
    val k_Bk_val = Slot.fresh()
    val i_Bki_key = Slot.fresh()
    val i_Bki_val = Slot.fresh()
    val j_Ckj_key = Slot.fresh()
    val j_Ckj_val = Slot.fresh()

    Lam(A,
      Lam(B,
        Sum(
          k_Bk_key,
          k_Bk_val,
          Var(A),
          Sum(
            i_Bki_key,
            i_Bki_val,
            Var(k_Bk_val),
            Sum(
              j_Ckj_key,
              j_Ckj_val,
              Get(
                Var(B),
                Var(k_Bk_key)
              ),
              Mult(
                Var(i_Bki_val),
                Var(j_Ckj_val)
              )
            )
          )
        )
      )
    )
  }

  def mmmSum2: MixedTree[SdqlIR, EClassCall] = {
    val A = Slot.fresh()
    val A1 = Slot.fresh()
    val A2 = Slot.fresh()
    val B = Slot.fresh()
    val B1 = Slot.fresh()
    val B2 = Slot.fresh()
    val k_Bk_key = Slot.fresh()
    val k_Bk_val = Slot.fresh()
    val i_p_key = Slot.fresh()
    val i_p_val = Slot.fresh()
    val q = Slot.fresh()
    val c_j_key = Slot.fresh()
    val c_j_val = Slot.fresh()
    val i_Bki_key = Slot.fresh()
    val i_Bki_val = Slot.fresh()
    val j_Ckj_key = Slot.fresh()
    val j_Ckj_val = Slot.fresh()

    Lam(A,
      Lam(A1,
        Lam(A2,
          Lam(B,
            Lam(B1,
              Lam(B2,
                Sum(
                  k_Bk_key,
                  k_Bk_val,
                  Sum(
                    i_p_key,
                    i_p_val,
                    Var(A),
                    Let(
                      q,
                      Get(Var(A), Add(Var(i_p_key), Num(1)())),
                      Sing(
                        Var(i_p_key),
                        Sum(
                          c_j_key,
                          c_j_val,
                          SubArray(
                            Var(A1),
                            Var(i_p_val),
                            Sub(Var(q), Num(1)())
                          ),
                          Sing(
                            Unique(Var(c_j_val)),
                            Get(Var(A2), Var(c_j_key))
                          )
                        )
                      )
                    )
                  ),
                  Sum(
                    i_Bki_key,
                    i_Bki_val,
                    Var(k_Bk_val),
                    Mult(
                      Var(i_Bki_val),
                      Sum(
                        j_Ckj_key,
                        j_Ckj_val,
                        Get(
                          Sum(
                            i_p_key,
                            i_p_val,
                            Var(B),
                            Let(
                              q,
                              Get(Var(B), Add(Var(i_p_key), Num(1)())),
                              Sing(
                                Var(i_p_key),
                                Sum(
                                  c_j_key,
                                  c_j_val,
                                  SubArray(
                                    Var(B1),
                                    Var(i_p_val),
                                    Sub(Var(q), Num(1)())
                                  ),
                                  Sing(
                                    Unique(Var(c_j_val)),
                                    Get(Var(B2), Var(c_j_key))
                                  )
                                )
                              )
                            )
                          ),
                          Var(k_Bk_key)
                        ),
                        Var(j_Ckj_val)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  def mttkrp1: MixedTree[SdqlIR, EClassCall] = {
    val B = Slot.fresh()
    val C = Slot.fresh()
    val D = Slot.fresh()
    val i_Bi_key = Slot.fresh()
    val i_Bi_val = Slot.fresh()
    val k_Bik_key = Slot.fresh()
    val k_Bik_val = Slot.fresh()
    val j_C_v_key = Slot.fresh()
    val j_C_v_val = Slot.fresh()
    val l_B_v_key = Slot.fresh()
    val l_B_v_val = Slot.fresh()

    Lam(B,
      Lam(C,
        Lam(D,
          Sum(
            i_Bi_key,
            i_Bi_val,
            Var(B),
            Sum(
              k_Bik_key,
              k_Bik_val,
              Var(i_Bi_val),
              Sum(
                j_C_v_key,
                j_C_v_val,
                Get(
                  Var(C),
                  Var(k_Bik_key)
                ),
                Sum(
                  l_B_v_key,
                  l_B_v_val,
                  Var(k_Bik_val),
                  Sing(
                    Var(i_Bi_key),
                    Sing(
                      Var(j_C_v_key),
                      Mult(
                        Mult(
                          Var(j_C_v_val),
                          Var(l_B_v_val)
                        ),
                        Get(
                          Get(
                            Var(D),
                            Var(j_C_v_key)
                          ),
                          Var(l_B_v_key)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  def mttkrp2: MixedTree[SdqlIR, EClassCall] = {
    val var01 = Slot.fresh()
    val var02 = Slot.fresh()
    val var03 = Slot.fresh()
    val var04 = Slot.fresh()
    val var05 = Slot.fresh()
    val var06 = Slot.fresh()
    val var07 = Slot.fresh()
    val var08 = Slot.fresh()
    val var09 = Slot.fresh()
    val var10 = Slot.fresh()
    val var11 = Slot.fresh()
    val var12 = Slot.fresh()
    val var13 = Slot.fresh()
    val var14 = Slot.fresh()
    val var15 = Slot.fresh()
    val var16 = Slot.fresh()
    val var17 = Slot.fresh()
    val var18 = Slot.fresh()
    val var19 = Slot.fresh()
    val var20 = Slot.fresh()
    val var21 = Slot.fresh()
    val var22 = Slot.fresh()
    val var23 = Slot.fresh()
    val var24 = Slot.fresh()
    val var25 = Slot.fresh()

    Lam(var01,
      Lam(var02,
        Lam(var03,
          Lam(var04,
            Lam(var05,
              Lam(var06,
                Lam(var07,
                  Lam(var08,
                    Lam(var09,
                      Lam(var10,
                        Lam(var11,
                          Lam(var12,
                            Sum(
                              var13,
                              var14,
                              Sum(
                                var13,
                                var14,
                                Var(var01),
                                Let(
                                  var15,
                                  Get(Var(var02), Var(var13)),
                                  Let(
                                    var16,
                                    Get(Var(var02), Add(Var(var13), Num(1)())),
                                    Sing(
                                      Unique(Var(var14)),
                                      Sum(
                                        var17,
                                        var18,
                                        SubArray(
                                          Var(var03),
                                          Var(var15),
                                          Sub(Var(var16), Num(1)())
                                        ),
                                        Let(
                                          var19,
                                          Get(Var(var04), Var(var17)),
                                          Let(
                                            var20,
                                            Get(Var(var04), Add(Var(var17), Num(1)())),
                                            Sing(
                                              Unique(Var(var18)),
                                              Sum(
                                                var21,
                                                var22,
                                                SubArray(
                                                  Var(var05),
                                                  Var(var19),
                                                  Sub(Var(var20), Num(1)())
                                                ),
                                                Let(
                                                  var23,
                                                  Get(Var(var06), Var(var21)),
                                                  Sing(
                                                    Unique(Var(var22)),
                                                    Var(var23)
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              ),
                              Sing(
                                Var(var13),
                                Sum(
                                  var15,
                                  var16,
                                  Var(var14),
                                  Sum(
                                    var17,
                                    var18,
                                    Get(
                                      Sum(
                                        var17,
                                        var18,
                                        Var(var07),
                                        Let(
                                          var19,
                                          Get(Var(var07), Add(Var(var17), Num(1)())),
                                          Sing(
                                            Var(var17),
                                            Sum(
                                              var20,
                                              var21,
                                              SubArray(
                                                Var(var08),
                                                Var(var18),
                                                Sub(Var(var19), Num(1)())
                                              ),
                                              Sing(
                                                Unique(Var(var21)),
                                                Get(Var(var09), Var(var20))
                                              )
                                            )
                                          )
                                        )
                                      ),
                                      Var(var15)
                                    ),
                                    Sing(
                                      Var(var17),
                                      Mult(
                                        Var(var18),
                                        Sum(
                                          var19,
                                          var20,
                                          Var(var16),
                                          Mult(
                                            Var(var20),
                                            Get(
                                              Get(
                                                Sum(
                                                  var21,
                                                  var22,
                                                  Var(var10),
                                                  Let(
                                                    var23,
                                                    Get(Var(var10), Add(Var(var21), Num(1)())),
                                                    Sing(
                                                      Var(var21),
                                                      Sum(
                                                        var24,
                                                        var25,
                                                        SubArray(
                                                          Var(var11),
                                                          Var(var22),
                                                          Sub(Var(var23), Num(1)())
                                                        ),
                                                        Sing(
                                                          Unique(Var(var25)),
                                                          Get(Var(var12), Var(var24))
                                                        )
                                                      )
                                                    )
                                                  )
                                                ),
                                                Var(var17)
                                              ),
                                              Var(var19)
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  def ttm1: MixedTree[SdqlIR, EClassCall] = {
    val A = Slot.fresh()
    val B = Slot.fresh()
    val i_Bi_key = Slot.fresh()
    val i_Bi_val = Slot.fresh()
    val j_Bij_key = Slot.fresh()
    val j_Bij_val = Slot.fresh()
    val k_Ck_key = Slot.fresh()
    val k_Ck_val = Slot.fresh()
    val l_B_v_key = Slot.fresh()
    val l_B_v_val = Slot.fresh()

    Lam(A,
      Lam(B,
        Sum(
          i_Bi_key,
          i_Bi_val,
          Var(B),
          Sum(
            j_Bij_key,
            j_Bij_val,
            Var(i_Bi_val),
            Sum(
              k_Ck_key,
              k_Ck_val,
              Var(A),
              Sum(
                l_B_v_key,
                l_B_v_val,
                Var(j_Bij_val),
                Sing(
                  Var(i_Bi_key),
                  Sing(
                    Var(j_Bij_key),
                    Sing(
                      Var(k_Ck_key),
                      Mult(
                        Var(l_B_v_val),
                        Get(
                          Var(k_Ck_val),
                          Var(l_B_v_key)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  def ttm2: MixedTree[SdqlIR, EClassCall] = {
    val var01 = Slot.fresh()
    val var02 = Slot.fresh()
    val var03 = Slot.fresh()
    val var04 = Slot.fresh()
    val var05 = Slot.fresh()
    val var06 = Slot.fresh()
    val var07 = Slot.fresh()
    val var08 = Slot.fresh()
    val var09 = Slot.fresh()
    val var10 = Slot.fresh()
    val var11 = Slot.fresh()
    val var12 = Slot.fresh()
    val var13 = Slot.fresh()
    val var14 = Slot.fresh()
    val var15 = Slot.fresh()
    val var16 = Slot.fresh()
    val var17 = Slot.fresh()
    val var18 = Slot.fresh()
    val var19 = Slot.fresh()
    val var20 = Slot.fresh()

    Lam(var01,
      Lam(var02,
        Lam(var03,
          Lam(var04,
            Lam(var05,
              Lam(var06,
                Lam(var07,
                  Lam(var08,
                    Lam(var09,
                      Sum(
                        var10,
                        var11,
                        Sum(
                          var10,
                          var11,
                          Var(var04),
                          Let(
                            var12,
                            Get(Var(var05), Var(var10)),
                            Let(
                              var13,
                              Get(Var(var05), Add(Var(var10), Num(1)())),
                              Sing(
                                Unique(Var(var11)),
                                Sum(
                                  var14,
                                  var15,
                                  SubArray(
                                    Var(var06),
                                    Var(var12),
                                    Sub(Var(var13), Num(1)())
                                  ),
                                  Let(
                                    var16,
                                    Get(Var(var07), Var(var14)),
                                    Let(
                                      var17,
                                      Get(Var(var07), Add(Var(var14), Num(1)())),
                                      Sing(
                                        Unique(Var(var15)),
                                        Sum(
                                          var18,
                                          var19,
                                          SubArray(
                                            Var(var08),
                                            Var(var16),
                                            Sub(Var(var17), Num(1)())
                                          ),
                                          Let(
                                            var20,
                                            Get(Var(var09), Var(var18)),
                                            Sing(
                                              Unique(Var(var19)),
                                              Var(var20)
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        ),
                        Sing(
                          Var(var10),
                          Sum(
                            var12,
                            var13,
                            Var(var11),
                            Sing(
                              Var(var12),
                              Sum(
                                var14,
                                var15,
                                Sum(
                                  var14,
                                  var15,
                                  Var(var01),
                                  Let(
                                    var16,
                                    Get(Var(var01), Add(Var(var14), Num(1)())),
                                    Sing(
                                      Var(var14),
                                      Sum(
                                        var17,
                                        var18,
                                        SubArray(
                                          Var(var02),
                                          Var(var15),
                                          Sub(Var(var16), Num(1)())
                                        ),
                                        Sing(
                                          Unique(Var(var18)),
                                          Get(Var(var03), Var(var17))
                                        )
                                      )
                                    )
                                  )
                                ),
                                Sing(
                                  Var(var14),
                                  Sum(
                                    var16,
                                    var17,
                                    Var(var13),
                                    Mult(
                                      Var(var17),
                                      Get(
                                        Var(var15),
                                        Var(var16)
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  def batax1: MixedTree[SdqlIR, EClassCall] = {
    val B = Slot.fresh()
    val C = Slot.fresh()
    val D = Slot.fresh()
    val i_Ai_key = Slot.fresh()
    val i_Ai_val = Slot.fresh()
    val j_a_val_key = Slot.fresh()
    val j_a_val_val = Slot.fresh()
    val k_a2_val_key = Slot.fresh()
    val k_a2_val_val = Slot.fresh()

    Lam(B,
      Lam(C,
        Lam(D,
          Sum(
            i_Ai_key,
            i_Ai_val,
            Var(C),
            Sum(
              j_a_val_key,
              j_a_val_val,
              Var(i_Ai_val),
              Sum(
                k_a2_val_key,
                k_a2_val_val,
                Var(i_Ai_val),
                Sing(
                  Var(j_a_val_key),
                  Mult(
                    Mult(
                      Mult(
                        Var(B),
                        Var(j_a_val_val)
                      ),
                      Var(k_a2_val_val)
                    ),
                    Get(
                      Var(D),
                      Var(k_a2_val_key)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  def batax2: MixedTree[SdqlIR, EClassCall] = {
    val var01 = Slot.fresh()
    val var02 = Slot.fresh()
    val var03 = Slot.fresh()
    val var04 = Slot.fresh()
    val var05 = Slot.fresh()
    val var06 = Slot.fresh()
    val var07 = Slot.fresh()
    val var08 = Slot.fresh()
    val var09 = Slot.fresh()
    val var10 = Slot.fresh()

    Lam(var01,
      Lam(var02,
        Lam(var03,
          Lam(var04,
            Lam(var05,
              Lam(var06,
                Mult(
                  Var(var01),
                  Sum(
                    var07,
                    var08,
                    Var(var02),
                    Mult(
                      Sum(
                        var09,
                        var10,
                        SubArray(
                          Var(var03),
                          Var(var08),
                          Sub(
                            Get(Var(var02), Add(Var(var07), Num(1)())),
                            Num(1)()
                          )
                        ),
                        Sing(Var(var10), Get(Var(var04), Var(var09)))
                      ),
                      Sum(
                        var09,
                        var10,
                        SubArray(
                          Var(var03),
                          Var(var08),
                          Sub(
                            Get(Var(var02), Add(Var(var07), Num(1)())),
                            Num(1)()
                          )
                        ),
                        Mult(
                          Get(Var(var04), Var(var09)),
                          Get(Var(var05), Var(var10))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }
}
