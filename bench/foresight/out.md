WARNING: A terminally deprecated method in sun.misc.Unsafe has been called
WARNING: sun.misc.Unsafe::objectFieldOffset has been called by scala.runtime.LazyVals$ (file:/Users/aziz/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.4.1/scala3-library_3-3.4.1.jar)
WARNING: Please consider reporting this to the maintainers of class scala.runtime.LazyVals$
WARNING: sun.misc.Unsafe::objectFieldOffset will be removed in a future release
# ================Using default parallel map================
## Benchmarking poly5 for 60 seconds.
Completed 325 iterations in 60 seconds
Average time per iteration: 185.137212 ms
root: 60166056750 nanoseconds (100.00%)
  add nodes: 6247421045 nanoseconds (10.38%)
  rule application: 8174589120 nanoseconds (13.59%)
    apply add-associativity1: 6969013330 nanoseconds (11.58%)
    apply add-associativity2: 6598375279 nanoseconds (10.97%)
    apply add-commutativity: 4793015186 nanoseconds (7.97%)
    apply distributivity1: 4179493989 nanoseconds (6.95%)
    apply distributivity2: 3762578879 nanoseconds (6.25%)
    apply mul-associativity1: 4703996240 nanoseconds (7.82%)
    apply mul-associativity2: 4994269687 nanoseconds (8.30%)
    apply mul-commutativity: 2182322816 nanoseconds (3.63%)
    apply multiplicative-identity: 270921001 nanoseconds (0.45%)
    apply power-null: 44214736 nanoseconds (0.07%)
    apply power-reduction: 248107377 nanoseconds (0.41%)
  rule matching: 20730974471 nanoseconds (34.46%)
    match add-associativity1: 13372011859 nanoseconds (22.23%)
    match add-associativity2: 13000512832 nanoseconds (21.61%)
    match add-commutativity: 7185302383 nanoseconds (11.94%)
    match distributivity1: 8818841460 nanoseconds (14.66%)
    match distributivity2: 18108186664 nanoseconds (30.10%)
    match mul-associativity1: 7899763635 nanoseconds (13.13%)
    match mul-associativity2: 11598893167 nanoseconds (19.28%)
    match mul-commutativity: 3734418120 nanoseconds (6.21%)
    match multiplicative-identity: 4129705575 nanoseconds (6.86%)
    match power-null: 1913263710 nanoseconds (3.18%)
    match power-reduction: 2108279208 nanoseconds (3.50%)
  union: 18611476111 nanoseconds (30.93%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 127701 iterations in 60 seconds
Average time per iteration: 0.469524 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 52285 iterations in 60 seconds
Average time per iteration: 1.147064 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 16755 iterations in 60 seconds
Average time per iteration: 3.580803 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 3645 iterations in 60 seconds
Average time per iteration: 16.463358 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 454 iterations in 60 seconds
Average time per iteration: 135.996007 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 14 iterations in 60 seconds
Average time per iteration: 4355.342458 ms
root: 429587016792 nanoseconds (100.00%)
  add nodes: 31620259566 nanoseconds (7.36%)
  rule application: 149996470635 nanoseconds (34.92%)
    apply add-associativity1: 6969013330 nanoseconds (1.62%)
    apply add-associativity2: 6598375279 nanoseconds (1.54%)
    apply add-commutativity: 4793015186 nanoseconds (1.12%)
    apply distributivity1: 4179493989 nanoseconds (0.97%)
    apply distributivity2: 3762578879 nanoseconds (0.88%)
    apply mul-associativity1: 112882509820 nanoseconds (26.28%)
    apply mul-associativity2: 102088163726 nanoseconds (23.76%)
    apply mul-commutativity: 2182322816 nanoseconds (0.51%)
    apply multiplicative-identity: 270921001 nanoseconds (0.06%)
    apply power-null: 44214736 nanoseconds (0.01%)
    apply power-reduction: 248107377 nanoseconds (0.06%)
  rule matching: 157423571668 nanoseconds (36.65%)
    match add-associativity1: 13372011859 nanoseconds (3.11%)
    match add-associativity2: 13000512832 nanoseconds (3.03%)
    match add-commutativity: 7185302383 nanoseconds (1.67%)
    match distributivity1: 8818841460 nanoseconds (2.05%)
    match distributivity2: 18108186664 nanoseconds (4.22%)
    match mul-associativity1: 104229185961 nanoseconds (24.26%)
    match mul-associativity2: 113138812797 nanoseconds (26.34%)
    match mul-commutativity: 3734418120 nanoseconds (0.87%)
    match multiplicative-identity: 4129705575 nanoseconds (0.96%)
    match power-null: 1913263710 nanoseconds (0.45%)
    match power-reduction: 2108279208 nanoseconds (0.49%)
  union: 40533166903 nanoseconds (9.44%)
# ================Using sequential map================
## Benchmarking poly5 for 60 seconds.
Completed 34 iterations in 60 seconds
Average time per iteration: 1777.135227 ms
root: 61278143750 nanoseconds (100.00%)
  add nodes: 2315142041 nanoseconds (3.78%)
  rule application: 15684625293 nanoseconds (25.60%)
    apply add-associativity1: 3716096326 nanoseconds (6.06%)
    apply add-associativity2: 4270407667 nanoseconds (6.97%)
    apply add-commutativity: 858241501 nanoseconds (1.40%)
    apply distributivity1: 1237148884 nanoseconds (2.02%)
    apply distributivity2: 1191406361 nanoseconds (1.94%)
    apply mul-associativity1: 1949951653 nanoseconds (3.18%)
    apply mul-associativity2: 1620274762 nanoseconds (2.64%)
    apply mul-commutativity: 304202625 nanoseconds (0.50%)
    apply multiplicative-identity: 14449164 nanoseconds (0.02%)
    apply power-null: 2608571 nanoseconds (0.00%)
    apply power-reduction: 14092543 nanoseconds (0.02%)
  rule matching: 34124046006 nanoseconds (55.69%)
    match add-associativity1: 3640160665 nanoseconds (5.94%)
    match add-associativity2: 3839315595 nanoseconds (6.27%)
    match add-commutativity: 770464579 nanoseconds (1.26%)
    match distributivity1: 1196108992 nanoseconds (1.95%)
    match distributivity2: 19178153626 nanoseconds (31.30%)
    match mul-associativity1: 1820961545 nanoseconds (2.97%)
    match mul-associativity2: 1559351161 nanoseconds (2.54%)
    match mul-commutativity: 620892667 nanoseconds (1.01%)
    match multiplicative-identity: 690532701 nanoseconds (1.13%)
    match power-null: 268638621 nanoseconds (0.44%)
    match power-reduction: 293095754 nanoseconds (0.48%)
  union: 6993718631 nanoseconds (11.41%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 98924 iterations in 60 seconds
Average time per iteration: 0.606381 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 22684 iterations in 60 seconds
Average time per iteration: 2.645237 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 3268 iterations in 60 seconds
Average time per iteration: 18.36076 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 384 iterations in 60 seconds
Average time per iteration: 156.406716 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 52 iterations in 60 seconds
Average time per iteration: 1173.82971 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 6 iterations in 60 seconds
Average time per iteration: 10113.015993 ms
root: 433162334125 nanoseconds (100.00%)
  add nodes: 13940863619 nanoseconds (3.22%)
  rule application: 193408995501 nanoseconds (44.65%)
    apply add-associativity1: 3716096326 nanoseconds (0.86%)
    apply add-associativity2: 4270407667 nanoseconds (0.99%)
    apply add-commutativity: 858241501 nanoseconds (0.20%)
    apply distributivity1: 1237148884 nanoseconds (0.29%)
    apply distributivity2: 1191406361 nanoseconds (0.28%)
    apply mul-associativity1: 99778405554 nanoseconds (23.03%)
    apply mul-associativity2: 75593584110 nanoseconds (17.45%)
    apply mul-commutativity: 304202625 nanoseconds (0.07%)
    apply multiplicative-identity: 14449164 nanoseconds (0.00%)
    apply power-null: 2608571 nanoseconds (0.00%)
    apply power-reduction: 14092543 nanoseconds (0.00%)
  rule matching: 161640199702 nanoseconds (37.32%)
    match add-associativity1: 3640160665 nanoseconds (0.84%)
    match add-associativity2: 3839315595 nanoseconds (0.89%)
    match add-commutativity: 770464579 nanoseconds (0.18%)
    match distributivity1: 1196108992 nanoseconds (0.28%)
    match distributivity2: 19178153626 nanoseconds (4.43%)
    match mul-associativity1: 66576564449 nanoseconds (15.37%)
    match mul-associativity2: 61536136576 nanoseconds (14.21%)
    match mul-commutativity: 620892667 nanoseconds (0.14%)
    match multiplicative-identity: 690532701 nanoseconds (0.16%)
    match power-null: 268638621 nanoseconds (0.06%)
    match power-reduction: 293095754 nanoseconds (0.07%)
  union: 23558051341 nanoseconds (5.44%)
# ================Using fixed parallel map with 1 threads================
## Benchmarking poly5 for 60 seconds.
Completed 36 iterations in 60 seconds
Average time per iteration: 1697.196319 ms
root: 64045464584 nanoseconds (100.00%)
  add nodes: 2222720907 nanoseconds (3.47%)
  rule application: 16704233048 nanoseconds (26.08%)
    apply add-associativity1: 4095398127 nanoseconds (6.39%)
    apply add-associativity2: 4435806427 nanoseconds (6.93%)
    apply add-commutativity: 845332784 nanoseconds (1.32%)
    apply distributivity1: 1513212219 nanoseconds (2.36%)
    apply distributivity2: 1225435745 nanoseconds (1.91%)
    apply mul-associativity1: 1942058215 nanoseconds (3.03%)
    apply mul-associativity2: 1689748379 nanoseconds (2.64%)
    apply mul-commutativity: 360839120 nanoseconds (0.56%)
    apply multiplicative-identity: 14895202 nanoseconds (0.02%)
    apply power-null: 2662003 nanoseconds (0.00%)
    apply power-reduction: 14535035 nanoseconds (0.02%)
  rule matching: 36661257203 nanoseconds (57.24%)
    match add-associativity1: 3877347001 nanoseconds (6.05%)
    match add-associativity2: 4219723206 nanoseconds (6.59%)
    match add-commutativity: 802620611 nanoseconds (1.25%)
    match distributivity1: 1266909034 nanoseconds (1.98%)
    match distributivity2: 20951729263 nanoseconds (32.71%)
    match mul-associativity1: 1711632785 nanoseconds (2.67%)
    match mul-associativity2: 1611661451 nanoseconds (2.52%)
    match mul-commutativity: 539957580 nanoseconds (0.84%)
    match multiplicative-identity: 783758334 nanoseconds (1.22%)
    match power-null: 328312551 nanoseconds (0.51%)
    match power-reduction: 318076840 nanoseconds (0.50%)
  union: 6494254956 nanoseconds (10.14%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 89010 iterations in 60 seconds
Average time per iteration: 0.675442 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 20483 iterations in 60 seconds
Average time per iteration: 2.929121 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 2818 iterations in 60 seconds
Average time per iteration: 21.295408 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 402 iterations in 60 seconds
Average time per iteration: 149.425945 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 51 iterations in 60 seconds
Average time per iteration: 1187.279985 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 7 iterations in 60 seconds
Average time per iteration: 9412.146875 ms
root: 443465639209 nanoseconds (100.00%)
  add nodes: 13015279932 nanoseconds (2.93%)
  rule application: 199246048951 nanoseconds (44.93%)
    apply add-associativity1: 4095398127 nanoseconds (0.92%)
    apply add-associativity2: 4435806427 nanoseconds (1.00%)
    apply add-commutativity: 845332784 nanoseconds (0.19%)
    apply distributivity1: 1513212219 nanoseconds (0.34%)
    apply distributivity2: 1225435745 nanoseconds (0.28%)
    apply mul-associativity1: 102811159417 nanoseconds (23.18%)
    apply mul-associativity2: 76135221771 nanoseconds (17.17%)
    apply mul-commutativity: 360839120 nanoseconds (0.08%)
    apply multiplicative-identity: 14895202 nanoseconds (0.00%)
    apply power-null: 2662003 nanoseconds (0.00%)
    apply power-reduction: 14535035 nanoseconds (0.00%)
  rule matching: 169799992820 nanoseconds (38.29%)
    match add-associativity1: 3877347001 nanoseconds (0.87%)
    match add-associativity2: 4219723206 nanoseconds (0.95%)
    match add-commutativity: 802620611 nanoseconds (0.18%)
    match distributivity1: 1266909034 nanoseconds (0.29%)
    match distributivity2: 20951729263 nanoseconds (4.72%)
    match mul-associativity1: 68723923688 nanoseconds (15.50%)
    match mul-associativity2: 64835707557 nanoseconds (14.62%)
    match mul-commutativity: 539957580 nanoseconds (0.12%)
    match multiplicative-identity: 783758334 nanoseconds (0.18%)
    match power-null: 328312551 nanoseconds (0.07%)
    match power-reduction: 318076840 nanoseconds (0.07%)
  union: 22703263623 nanoseconds (5.12%)
# ================Using fixed parallel map with 2 threads================
## Benchmarking poly5 for 60 seconds.
Completed 36 iterations in 60 seconds
Average time per iteration: 1675.564254 ms
root: 61692510375 nanoseconds (100.00%)
  add nodes: 3598538368 nanoseconds (5.83%)
  rule application: 18064053332 nanoseconds (29.28%)
    apply add-associativity1: 9272834418 nanoseconds (15.03%)
    apply add-associativity2: 4033451115 nanoseconds (6.54%)
    apply add-commutativity: 1817714494 nanoseconds (2.95%)
    apply distributivity1: 5070743779 nanoseconds (8.22%)
    apply distributivity2: 2337419333 nanoseconds (3.79%)
    apply mul-associativity1: 1752332551 nanoseconds (2.84%)
    apply mul-associativity2: 3297112371 nanoseconds (5.34%)
    apply mul-commutativity: 702565 microseconds (1.14%)
    apply multiplicative-identity: 31026742 nanoseconds (0.05%)
    apply power-null: 9192593 nanoseconds (0.01%)
    apply power-reduction: 35178629 nanoseconds (0.06%)
  rule matching: 28108022510 nanoseconds (45.56%)
    match add-associativity1: 17211437254 nanoseconds (27.90%)
    match add-associativity2: 13927736749 nanoseconds (22.58%)
    match add-commutativity: 22357287116 nanoseconds (36.24%)
    match distributivity1: 19798383296 nanoseconds (32.09%)
    match distributivity2: 20879457341 nanoseconds (33.84%)
    match mul-associativity1: 3624034796 nanoseconds (5.87%)
    match mul-associativity2: 14275842837 nanoseconds (23.14%)
    match mul-commutativity: 16414707120 nanoseconds (26.61%)
    match multiplicative-identity: 21986267248 nanoseconds (35.64%)
    match power-null: 16242954875 nanoseconds (26.33%)
    match power-reduction: 4515325627 nanoseconds (7.32%)
  union: 8781698872 nanoseconds (14.23%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 58265 iterations in 60 seconds
Average time per iteration: 1.029651 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 18240 iterations in 60 seconds
Average time per iteration: 3.289364 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 3695 iterations in 60 seconds
Average time per iteration: 16.238332 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 500 iterations in 60 seconds
Average time per iteration: 120.211404 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 82 iterations in 60 seconds
Average time per iteration: 732.393427 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 10 iterations in 60 seconds
Average time per iteration: 6207.783416 ms
root: 431328575083 nanoseconds (100.00%)
  add nodes: 22603326095 nanoseconds (5.24%)
  rule application: 183298501857 nanoseconds (42.50%)
    apply add-associativity1: 9272834418 nanoseconds (2.15%)
    apply add-associativity2: 4033451115 nanoseconds (0.94%)
    apply add-commutativity: 1817714494 nanoseconds (0.42%)
    apply distributivity1: 5070743779 nanoseconds (1.18%)
    apply distributivity2: 2337419333 nanoseconds (0.54%)
    apply mul-associativity1: 145267226392 nanoseconds (33.68%)
    apply mul-associativity2: 124174160718 nanoseconds (28.79%)
    apply mul-commutativity: 702565 microseconds (0.16%)
    apply multiplicative-identity: 31026742 nanoseconds (0.01%)
    apply power-null: 9192593 nanoseconds (0.00%)
    apply power-reduction: 35178629 nanoseconds (0.01%)
  rule matching: 164255672531 nanoseconds (38.08%)
    match add-associativity1: 17211437254 nanoseconds (3.99%)
    match add-associativity2: 13927736749 nanoseconds (3.23%)
    match add-commutativity: 22357287116 nanoseconds (5.18%)
    match distributivity1: 19798383296 nanoseconds (4.59%)
    match distributivity2: 20879457341 nanoseconds (4.84%)
    match mul-associativity1: 116345474699 nanoseconds (26.97%)
    match mul-associativity2: 111404198472 nanoseconds (25.83%)
    match mul-commutativity: 16414707120 nanoseconds (3.81%)
    match multiplicative-identity: 21986267248 nanoseconds (5.10%)
    match power-null: 16242954875 nanoseconds (3.77%)
    match power-reduction: 4515325627 nanoseconds (1.05%)
  union: 24658266115 nanoseconds (5.72%)
# ================Using fixed parallel map with 3 threads================
## Benchmarking poly5 for 60 seconds.
Completed 75 iterations in 60 seconds
Average time per iteration: 802.611841 ms
root: 60675055292 nanoseconds (100.00%)
  add nodes: 4111382112 nanoseconds (6.78%)
  rule application: 13237400206 nanoseconds (21.82%)
    apply add-associativity1: 7926300327 nanoseconds (13.06%)
    apply add-associativity2: 6795730129 nanoseconds (11.20%)
    apply add-commutativity: 1895589804 nanoseconds (3.12%)
    apply distributivity1: 3146644174 nanoseconds (5.19%)
    apply distributivity2: 2820496306 nanoseconds (4.65%)
    apply mul-associativity1: 2559390735 nanoseconds (4.22%)
    apply mul-associativity2: 3692640705 nanoseconds (6.09%)
    apply mul-commutativity: 1054556 microseconds (1.74%)
    apply multiplicative-identity: 107781411 nanoseconds (0.18%)
    apply power-null: 12808459 nanoseconds (0.02%)
    apply power-reduction: 47770621 nanoseconds (0.08%)
  rule matching: 27494911814 nanoseconds (45.32%)
    match add-associativity1: 13602733185 nanoseconds (22.42%)
    match add-associativity2: 17279102717 nanoseconds (28.48%)
    match add-commutativity: 21910069544 nanoseconds (36.11%)
    match distributivity1: 17925261022 nanoseconds (29.54%)
    match distributivity2: 21050381544 nanoseconds (34.69%)
    match mul-associativity1: 8115777914 nanoseconds (13.38%)
    match mul-associativity2: 15891130447 nanoseconds (26.19%)
    match mul-commutativity: 11024172170 nanoseconds (18.17%)
    match multiplicative-identity: 18719464143 nanoseconds (30.85%)
    match power-null: 15153319794 nanoseconds (24.97%)
    match power-reduction: 6045728585 nanoseconds (9.96%)
  union: 11863695746 nanoseconds (19.55%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 47850 iterations in 60 seconds
Average time per iteration: 1.25376 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 19746 iterations in 60 seconds
Average time per iteration: 3.038489 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 4359 iterations in 60 seconds
Average time per iteration: 13.811705 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 913 iterations in 60 seconds
Average time per iteration: 65.739486 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 95 iterations in 60 seconds
Average time per iteration: 633.54956 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 14 iterations in 60 seconds
Average time per iteration: 4525.313613 ms
root: 430928714375 nanoseconds (100.00%)
  add nodes: 25920033827 nanoseconds (6.01%)
  rule application: 166776577444 nanoseconds (38.70%)
    apply add-associativity1: 7926300327 nanoseconds (1.84%)
    apply add-associativity2: 6795730129 nanoseconds (1.58%)
    apply add-commutativity: 1895589804 nanoseconds (0.44%)
    apply distributivity1: 3146644174 nanoseconds (0.73%)
    apply distributivity2: 2820496306 nanoseconds (0.65%)
    apply mul-associativity1: 124819450436 nanoseconds (28.97%)
    apply mul-associativity2: 110291940168 nanoseconds (25.59%)
    apply mul-commutativity: 1054556 microseconds (0.24%)
    apply multiplicative-identity: 107781411 nanoseconds (0.03%)
    apply power-null: 12808459 nanoseconds (0.00%)
    apply power-reduction: 47770621 nanoseconds (0.01%)
  rule matching: 167988293506 nanoseconds (38.98%)
    match add-associativity1: 13602733185 nanoseconds (3.16%)
    match add-associativity2: 17279102717 nanoseconds (4.01%)
    match add-commutativity: 21910069544 nanoseconds (5.08%)
    match distributivity1: 17925261022 nanoseconds (4.16%)
    match distributivity2: 21050381544 nanoseconds (4.88%)
    match mul-associativity1: 122850550587 nanoseconds (28.51%)
    match mul-associativity2: 121653828394 nanoseconds (28.23%)
    match mul-commutativity: 11024172170 nanoseconds (2.56%)
    match multiplicative-identity: 18719464143 nanoseconds (4.34%)
    match power-null: 15153319794 nanoseconds (3.52%)
    match power-reduction: 6045728585 nanoseconds (1.40%)
  union: 28279080351 nanoseconds (6.56%)
# ================Using fixed parallel map with 4 threads================
## Benchmarking poly5 for 60 seconds.
Completed 86 iterations in 60 seconds
Average time per iteration: 711.177154 ms
root: 62644041709 nanoseconds (100.00%)
  add nodes: 4364829204 nanoseconds (6.97%)
  rule application: 13013110465 nanoseconds (20.77%)
    apply add-associativity1: 9400847708 nanoseconds (15.01%)
    apply add-associativity2: 6806078415 nanoseconds (10.86%)
    apply add-commutativity: 2941459766 nanoseconds (4.70%)
    apply distributivity1: 4046457200 nanoseconds (6.46%)
    apply distributivity2: 3598019003 nanoseconds (5.74%)
    apply mul-associativity1: 3638111325 nanoseconds (5.81%)
    apply mul-associativity2: 4581170632 nanoseconds (7.31%)
    apply mul-commutativity: 1703208960 nanoseconds (2.72%)
    apply multiplicative-identity: 66564464 nanoseconds (0.11%)
    apply power-null: 12677809 nanoseconds (0.02%)
    apply power-reduction: 66451863 nanoseconds (0.11%)
  rule matching: 26327577966 nanoseconds (42.03%)
    match add-associativity1: 14447907108 nanoseconds (23.06%)
    match add-associativity2: 17769128079 nanoseconds (28.37%)
    match add-commutativity: 21496778782 nanoseconds (34.32%)
    match distributivity1: 17146976638 nanoseconds (27.37%)
    match distributivity2: 20674963674 nanoseconds (33.00%)
    match mul-associativity1: 8213574081 nanoseconds (13.11%)
    match mul-associativity2: 14076476672 nanoseconds (22.47%)
    match mul-commutativity: 9840016233 nanoseconds (15.71%)
    match multiplicative-identity: 16495395535 nanoseconds (26.33%)
    match power-null: 13872755030 nanoseconds (22.15%)
    match power-reduction: 6784834333 nanoseconds (10.83%)
  union: 14262199294 nanoseconds (22.77%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 54014 iterations in 60 seconds
Average time per iteration: 1.110719 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 23694 iterations in 60 seconds
Average time per iteration: 2.532152 ms
## Benchmarking nmm with n=10 for 60 seconds.
