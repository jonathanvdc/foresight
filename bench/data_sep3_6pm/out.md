# ================Using default parallel map================
## Benchmarking poly5 for 60 seconds.
Completed 446 iterations in 60 seconds
Average time per iteration: 134.54038 ms
root: 59988928681 nanoseconds (100.00%)
  add nodes: 6630223503 nanoseconds (11.05%)
  rule application: 7561755188 nanoseconds (12.61%)
    apply add-associativity1: 6283079596 nanoseconds (10.47%)
    apply add-associativity2: 6210477230 nanoseconds (10.35%)
    apply add-commutativity: 4835890879 nanoseconds (8.06%)
    apply distributivity1: 3996589069 nanoseconds (6.66%)
    apply distributivity2: 3659090884 nanoseconds (6.10%)
    apply mul-associativity1: 5010482230 nanoseconds (8.35%)
    apply mul-associativity2: 4372383694 nanoseconds (7.29%)
    apply mul-commutativity: 1860058397 nanoseconds (3.10%)
    apply multiplicative-identity: 244110376 nanoseconds (0.41%)
    apply power-null: 67901946 nanoseconds (0.11%)
    apply power-reduction: 227701126 nanoseconds (0.38%)
  rule matching: 17913464374 nanoseconds (29.86%)
    match add-associativity1: 11480106952 nanoseconds (19.14%)
    match add-associativity2: 11069323688 nanoseconds (18.45%)
    match add-commutativity: 7903295190 nanoseconds (13.17%)
    match distributivity1: 10847611865 nanoseconds (18.08%)
    match distributivity2: 16638961289 nanoseconds (27.74%)
    match mul-associativity1: 7388094585 nanoseconds (12.32%)
    match mul-associativity2: 10186372447 nanoseconds (16.98%)
    match mul-commutativity: 3478216958 nanoseconds (5.80%)
    match multiplicative-identity: 4857824382 nanoseconds (8.10%)
    match power-null: 2396672517 nanoseconds (4.00%)
    match power-reduction: 2327864490 nanoseconds (3.88%)
  union: 21177964557 nanoseconds (35.30%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 180453 iterations in 60 seconds
Average time per iteration: 0.332223 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 74586 iterations in 60 seconds
Average time per iteration: 0.804163 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 22785 iterations in 60 seconds
Average time per iteration: 2.633275 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 5574 iterations in 60 seconds
Average time per iteration: 10.766063 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 883 iterations in 60 seconds
Average time per iteration: 67.982117 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 135 iterations in 60 seconds
Average time per iteration: 444.630453 ms
root: 420737726021 nanoseconds (100.00%)
  add nodes: 35443199722 nanoseconds (8.42%)
  rule application: 128027634520 nanoseconds (30.43%)
    apply add-associativity1: 6283079596 nanoseconds (1.49%)
    apply add-associativity2: 6210477230 nanoseconds (1.48%)
    apply add-commutativity: 4835890879 nanoseconds (1.15%)
    apply distributivity1: 3996589069 nanoseconds (0.95%)
    apply distributivity2: 3659090884 nanoseconds (0.87%)
    apply mul-associativity1: 96779308728 nanoseconds (23.00%)
    apply mul-associativity2: 81071449276 nanoseconds (19.27%)
    apply mul-commutativity: 1860058397 nanoseconds (0.44%)
    apply multiplicative-identity: 244110376 nanoseconds (0.06%)
    apply power-null: 67901946 nanoseconds (0.02%)
    apply power-reduction: 227701126 nanoseconds (0.05%)
  rule matching: 137918561473 nanoseconds (32.78%)
    match add-associativity1: 11480106952 nanoseconds (2.73%)
    match add-associativity2: 11069323688 nanoseconds (2.63%)
    match add-commutativity: 7903295190 nanoseconds (1.88%)
    match distributivity1: 10847611865 nanoseconds (2.58%)
    match distributivity2: 16638961289 nanoseconds (3.95%)
    match mul-associativity1: 100174089085 nanoseconds (23.81%)
    match mul-associativity2: 99873945463 nanoseconds (23.74%)
    match mul-commutativity: 3478216958 nanoseconds (0.83%)
    match multiplicative-identity: 4857824382 nanoseconds (1.15%)
    match power-null: 2396672517 nanoseconds (0.57%)
    match power-reduction: 2327864490 nanoseconds (0.55%)
  union: 53838241457 nanoseconds (12.80%)
# ================Using sequential map================
## Benchmarking poly5 for 60 seconds.
Completed 141 iterations in 60 seconds
Average time per iteration: 428.13422 ms
root: 60790060391 nanoseconds (100.00%)
  add nodes: 2177729854 nanoseconds (3.58%)
  rule application: 13710526526 nanoseconds (22.55%)
    apply add-associativity1: 3438356834 nanoseconds (5.66%)
    apply add-associativity2: 3384446034 nanoseconds (5.57%)
    apply add-commutativity: 742910998 nanoseconds (1.22%)
    apply distributivity1: 1077776918 nanoseconds (1.77%)
    apply distributivity2: 1045270491 nanoseconds (1.72%)
    apply mul-associativity1: 1595516644 nanoseconds (2.62%)
    apply mul-associativity2: 1324733490 nanoseconds (2.18%)
    apply mul-commutativity: 348248399 nanoseconds (0.57%)
    apply multiplicative-identity: 15587717 nanoseconds (0.03%)
    apply power-null: 3458728 nanoseconds (0.01%)
    apply power-reduction: 15060169 nanoseconds (0.02%)
  rule matching: 36528320381 nanoseconds (60.09%)
    match add-associativity1: 3971401132 nanoseconds (6.53%)
    match add-associativity2: 4323791784 nanoseconds (7.11%)
    match add-commutativity: 861798186 nanoseconds (1.42%)
    match distributivity1: 1256058839 nanoseconds (2.07%)
    match distributivity2: 20543959336 nanoseconds (33.79%)
    match mul-associativity1: 1747545783 nanoseconds (2.87%)
    match mul-associativity2: 1658945124 nanoseconds (2.73%)
    match mul-commutativity: 524131518 nanoseconds (0.86%)
    match multiplicative-identity: 775103398 nanoseconds (1.28%)
    match power-null: 278337253 nanoseconds (0.46%)
    match power-reduction: 274570880 nanoseconds (0.45%)
  union: 6384426271 nanoseconds (10.50%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 521176 iterations in 60 seconds
Average time per iteration: 0.115097 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 107179 iterations in 60 seconds
Average time per iteration: 0.559786 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 14236 iterations in 60 seconds
Average time per iteration: 4.214904 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 1793 iterations in 60 seconds
Average time per iteration: 33.471488 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 220 iterations in 60 seconds
Average time per iteration: 273.337161 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 26 iterations in 60 seconds
Average time per iteration: 2344.081982 ms
root: 424626333956 nanoseconds (100.00%)
  add nodes: 12697150287 nanoseconds (2.99%)
  rule application: 171113534422 nanoseconds (40.30%)
    apply add-associativity1: 3438356834 nanoseconds (0.81%)
    apply add-associativity2: 3384446034 nanoseconds (0.80%)
    apply add-commutativity: 742910998 nanoseconds (0.17%)
    apply distributivity1: 1077776918 nanoseconds (0.25%)
    apply distributivity2: 1045270491 nanoseconds (0.25%)
    apply mul-associativity1: 87448201483 nanoseconds (20.59%)
    apply mul-associativity2: 66414284787 nanoseconds (15.64%)
    apply mul-commutativity: 348248399 nanoseconds (0.08%)
    apply multiplicative-identity: 15587717 nanoseconds (0.00%)
    apply power-null: 3458728 nanoseconds (0.00%)
    apply power-reduction: 15060169 nanoseconds (0.00%)
  rule matching: 176494152264 nanoseconds (41.56%)
    match add-associativity1: 3971401132 nanoseconds (0.94%)
    match add-associativity2: 4323791784 nanoseconds (1.02%)
    match add-commutativity: 861798186 nanoseconds (0.20%)
    match distributivity1: 1256058839 nanoseconds (0.30%)
    match distributivity2: 20543959336 nanoseconds (4.84%)
    match mul-associativity1: 74452321318 nanoseconds (17.53%)
    match mul-associativity2: 65582612797 nanoseconds (15.44%)
    match mul-commutativity: 524131518 nanoseconds (0.12%)
    match multiplicative-identity: 775103398 nanoseconds (0.18%)
    match power-null: 278337253 nanoseconds (0.07%)
    match power-reduction: 274570880 nanoseconds (0.06%)
  union: 24866555238 nanoseconds (5.86%)
# ================Using fixed parallel map with 1 threads================
## Benchmarking poly5 for 60 seconds.
Completed 140 iterations in 60 seconds
Average time per iteration: 430.556109 ms
root: 60692147658 nanoseconds (100.00%)
  add nodes: 2173362921 nanoseconds (3.58%)
  rule application: 13665614509 nanoseconds (22.52%)
    apply add-associativity1: 3424733751 nanoseconds (5.64%)
    apply add-associativity2: 3371272607 nanoseconds (5.55%)
    apply add-commutativity: 748251552 nanoseconds (1.23%)
    apply distributivity1: 1059518954 nanoseconds (1.75%)
    apply distributivity2: 1055276056 nanoseconds (1.74%)
    apply mul-associativity1: 1582445658 nanoseconds (2.61%)
    apply mul-associativity2: 1315785066 nanoseconds (2.17%)
    apply mul-commutativity: 333983217 nanoseconds (0.55%)
    apply multiplicative-identity: 15738785 nanoseconds (0.03%)
    apply power-null: 3514460 nanoseconds (0.01%)
    apply power-reduction: 15121910 nanoseconds (0.02%)
  rule matching: 36425144087 nanoseconds (60.02%)
    match add-associativity1: 3979999616 nanoseconds (6.56%)
    match add-associativity2: 4324365702 nanoseconds (7.13%)
    match add-commutativity: 867708976 nanoseconds (1.43%)
    match distributivity1: 1238070467 nanoseconds (2.04%)
    match distributivity2: 20451312618 nanoseconds (33.70%)
    match mul-associativity1: 1752992997 nanoseconds (2.89%)
    match mul-associativity2: 1640205872 nanoseconds (2.70%)
    match mul-commutativity: 525654258 nanoseconds (0.87%)
    match multiplicative-identity: 756186742 nanoseconds (1.25%)
    match power-null: 281307785 nanoseconds (0.46%)
    match power-reduction: 277156524 nanoseconds (0.46%)
  union: 6399188929 nanoseconds (10.54%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 515440 iterations in 60 seconds
Average time per iteration: 0.116379 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 105324 iterations in 60 seconds
Average time per iteration: 0.569646 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 14106 iterations in 60 seconds
Average time per iteration: 4.253684 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 1788 iterations in 60 seconds
Average time per iteration: 33.56391 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 218 iterations in 60 seconds
Average time per iteration: 275.665704 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 26 iterations in 60 seconds
Average time per iteration: 2342.564235 ms
root: 424290230438 nanoseconds (100.00%)
  add nodes: 12745020556 nanoseconds (3.00%)
  rule application: 171353376373 nanoseconds (40.39%)
    apply add-associativity1: 3424733751 nanoseconds (0.81%)
    apply add-associativity2: 3371272607 nanoseconds (0.79%)
    apply add-commutativity: 748251552 nanoseconds (0.18%)
    apply distributivity1: 1059518954 nanoseconds (0.25%)
    apply distributivity2: 1055276056 nanoseconds (0.25%)
    apply mul-associativity1: 87585974701 nanoseconds (20.64%)
    apply mul-associativity2: 66490580129 nanoseconds (15.67%)
    apply mul-commutativity: 333983217 nanoseconds (0.08%)
    apply multiplicative-identity: 15738785 nanoseconds (0.00%)
    apply power-null: 3514460 nanoseconds (0.00%)
    apply power-reduction: 15121910 nanoseconds (0.00%)
  rule matching: 175824429793 nanoseconds (41.44%)
    match add-associativity1: 3979999616 nanoseconds (0.94%)
    match add-associativity2: 4324365702 nanoseconds (1.02%)
    match add-commutativity: 867708976 nanoseconds (0.20%)
    match distributivity1: 1238070467 nanoseconds (0.29%)
    match distributivity2: 20451312618 nanoseconds (4.82%)
    match mul-associativity1: 74183036916 nanoseconds (17.48%)
    match mul-associativity2: 65245928949 nanoseconds (15.38%)
    match mul-commutativity: 525654258 nanoseconds (0.12%)
    match multiplicative-identity: 756186742 nanoseconds (0.18%)
    match power-null: 281307785 nanoseconds (0.07%)
    match power-reduction: 277156524 nanoseconds (0.07%)
  union: 24787806684 nanoseconds (5.84%)
# ================Using fixed parallel map with 2 threads================
## Benchmarking poly5 for 60 seconds.
Completed 255 iterations in 60 seconds
Average time per iteration: 235.64217 ms
root: 60327636081 nanoseconds (100.00%)
  add nodes: 4146096442 nanoseconds (6.87%)
  rule application: 14379304981 nanoseconds (23.84%)
    apply add-associativity1: 6028717929 nanoseconds (9.99%)
    apply add-associativity2: 3526275659 nanoseconds (5.85%)
    apply add-commutativity: 1510431861 nanoseconds (2.50%)
    apply distributivity1: 2056168341 nanoseconds (3.41%)
    apply distributivity2: 1984630443 nanoseconds (3.29%)
    apply mul-associativity1: 1703397402 nanoseconds (2.82%)
    apply mul-associativity2: 2686727742 nanoseconds (4.45%)
    apply mul-commutativity: 662169374 nanoseconds (1.10%)
    apply multiplicative-identity: 35921025 nanoseconds (0.06%)
    apply power-null: 7650913 nanoseconds (0.01%)
    apply power-reduction: 40767200 nanoseconds (0.07%)
  rule matching: 25834863904 nanoseconds (42.82%)
    match add-associativity1: 15819390464 nanoseconds (26.22%)
    match add-associativity2: 12481814013 nanoseconds (20.69%)
    match add-commutativity: 12492377260 nanoseconds (20.71%)
    match distributivity1: 12114394649 nanoseconds (20.08%)
    match distributivity2: 19818644514 nanoseconds (32.85%)
    match mul-associativity1: 6206148731 nanoseconds (10.29%)
    match mul-associativity2: 14752768407 nanoseconds (24.45%)
    match mul-commutativity: 10877308396 nanoseconds (18.03%)
    match multiplicative-identity: 12741691901 nanoseconds (21.12%)
    match power-null: 10857309356 nanoseconds (18.00%)
    match power-reduction: 3884366052 nanoseconds (6.44%)
  union: 12110287260 nanoseconds (20.07%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 243652 iterations in 60 seconds
Average time per iteration: 0.246217 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 91089 iterations in 60 seconds
Average time per iteration: 0.658656 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 18127 iterations in 60 seconds
Average time per iteration: 3.310067 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 2843 iterations in 60 seconds
Average time per iteration: 21.105312 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 384 iterations in 60 seconds
Average time per iteration: 156.371939 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 47 iterations in 60 seconds
Average time per iteration: 1281.038125 ms
root: 422257569172 nanoseconds (100.00%)
  add nodes: 22898459542 nanoseconds (5.42%)
  rule application: 160524936468 nanoseconds (38.02%)
    apply add-associativity1: 6028717929 nanoseconds (1.43%)
    apply add-associativity2: 3526275659 nanoseconds (0.84%)
    apply add-commutativity: 1510431861 nanoseconds (0.36%)
    apply distributivity1: 2056168341 nanoseconds (0.49%)
    apply distributivity2: 1984630443 nanoseconds (0.47%)
    apply mul-associativity1: 130507695348 nanoseconds (30.91%)
    apply mul-associativity2: 106420297250 nanoseconds (25.20%)
    apply mul-commutativity: 662169374 nanoseconds (0.16%)
    apply multiplicative-identity: 35921025 nanoseconds (0.01%)
    apply power-null: 7650913 nanoseconds (0.00%)
    apply power-reduction: 40767200 nanoseconds (0.01%)
  rule matching: 161995708761 nanoseconds (38.36%)
    match add-associativity1: 15819390464 nanoseconds (3.75%)
    match add-associativity2: 12481814013 nanoseconds (2.96%)
    match add-commutativity: 12492377260 nanoseconds (2.96%)
    match distributivity1: 12114394649 nanoseconds (2.87%)
    match distributivity2: 19818644514 nanoseconds (4.69%)
    match mul-associativity1: 120660865787 nanoseconds (28.58%)
    match mul-associativity2: 114495896557 nanoseconds (27.12%)
    match mul-commutativity: 10877308396 nanoseconds (2.58%)
    match multiplicative-identity: 12741691901 nanoseconds (3.02%)
    match power-null: 10857309356 nanoseconds (2.57%)
    match power-reduction: 3884366052 nanoseconds (0.92%)
  union: 33052846790 nanoseconds (7.83%)
# ================Using fixed parallel map with 3 threads================
## Benchmarking poly5 for 60 seconds.
Completed 299 iterations in 60 seconds
Average time per iteration: 201.064226 ms
root: 60340000197 nanoseconds (100.00%)
  add nodes: 4790311885 nanoseconds (7.94%)
  rule application: 12542664776 nanoseconds (20.79%)
    apply add-associativity1: 6762736469 nanoseconds (11.21%)
    apply add-associativity2: 5864444627 nanoseconds (9.72%)
    apply add-commutativity: 1986936891 nanoseconds (3.29%)
    apply distributivity1: 2647988685 nanoseconds (4.39%)
    apply distributivity2: 2466189815 nanoseconds (4.09%)
    apply mul-associativity1: 2521710567 nanoseconds (4.18%)
    apply mul-associativity2: 3456582203 nanoseconds (5.73%)
    apply mul-commutativity: 987664147 nanoseconds (1.64%)
    apply multiplicative-identity: 51093378 nanoseconds (0.08%)
    apply power-null: 14872749 nanoseconds (0.02%)
    apply power-reduction: 55211980 nanoseconds (0.09%)
  rule matching: 23930964784 nanoseconds (39.66%)
    match add-associativity1: 13625924706 nanoseconds (22.58%)
    match add-associativity2: 13381727321 nanoseconds (22.18%)
    match add-commutativity: 12929080601 nanoseconds (21.43%)
    match distributivity1: 11923443213 nanoseconds (19.76%)
    match distributivity2: 18747204981 nanoseconds (31.07%)
    match mul-associativity1: 6767803185 nanoseconds (11.22%)
    match mul-associativity2: 14628403870 nanoseconds (24.24%)
    match mul-commutativity: 6669913581 nanoseconds (11.05%)
    match multiplicative-identity: 11694068999 nanoseconds (19.38%)
    match power-null: 10672460279 nanoseconds (17.69%)
    match power-reduction: 4144604211 nanoseconds (6.87%)
  union: 14495288445 nanoseconds (24.02%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 229464 iterations in 60 seconds
Average time per iteration: 0.261441 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 89031 iterations in 60 seconds
Average time per iteration: 0.673877 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 19836 iterations in 60 seconds
Average time per iteration: 3.024749 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 3409 iterations in 60 seconds
Average time per iteration: 17.603078 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 471 iterations in 60 seconds
Average time per iteration: 127.580497 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 59 iterations in 60 seconds
Average time per iteration: 1017.012818 ms
root: 421646410747 nanoseconds (100.00%)
  add nodes: 25694839848 nanoseconds (6.09%)
  rule application: 150476651439 nanoseconds (35.69%)
    apply add-associativity1: 6762736469 nanoseconds (1.60%)
    apply add-associativity2: 5864444627 nanoseconds (1.39%)
    apply add-commutativity: 1986936891 nanoseconds (0.47%)
    apply distributivity1: 2647988685 nanoseconds (0.63%)
    apply distributivity2: 2466189815 nanoseconds (0.58%)
    apply mul-associativity1: 105819904474 nanoseconds (25.10%)
    apply mul-associativity2: 101775935090 nanoseconds (24.14%)
    apply mul-commutativity: 987664147 nanoseconds (0.23%)
    apply multiplicative-identity: 51093378 nanoseconds (0.01%)
    apply power-null: 14872749 nanoseconds (0.00%)
    apply power-reduction: 55211980 nanoseconds (0.01%)
  rule matching: 158876112228 nanoseconds (37.68%)
    match add-associativity1: 13625924706 nanoseconds (3.23%)
    match add-associativity2: 13381727321 nanoseconds (3.17%)
    match add-commutativity: 12929080601 nanoseconds (3.07%)
    match distributivity1: 11923443213 nanoseconds (2.83%)
    match distributivity2: 18747204981 nanoseconds (4.45%)
    match mul-associativity1: 121033798866 nanoseconds (28.71%)
    match mul-associativity2: 113546095209 nanoseconds (26.93%)
    match mul-commutativity: 6669913581 nanoseconds (1.58%)
    match multiplicative-identity: 11694068999 nanoseconds (2.77%)
    match power-null: 10672460279 nanoseconds (2.53%)
    match power-reduction: 4144604211 nanoseconds (0.98%)
  union: 37887138292 nanoseconds (8.99%)
# ================Using fixed parallel map with 4 threads================
## Benchmarking poly5 for 60 seconds.
Completed 331 iterations in 60 seconds
Average time per iteration: 181.352769 ms
root: 60246876368 nanoseconds (100.00%)
  add nodes: 5197288763 nanoseconds (8.63%)
  rule application: 11132609447 nanoseconds (18.48%)
    apply add-associativity1: 8549918977 nanoseconds (14.19%)
    apply add-associativity2: 6717803507 nanoseconds (11.15%)
    apply add-commutativity: 2849025371 nanoseconds (4.73%)
    apply distributivity1: 3292702063 nanoseconds (5.47%)
    apply distributivity2: 2632566295 nanoseconds (4.37%)
    apply mul-associativity1: 3696451427 nanoseconds (6.14%)
    apply mul-associativity2: 4262835297 nanoseconds (7.08%)
    apply mul-commutativity: 1262098027 nanoseconds (2.09%)
    apply multiplicative-identity: 64281645 nanoseconds (0.11%)
    apply power-null: 20042819 nanoseconds (0.03%)
    apply power-reduction: 70221032 nanoseconds (0.12%)
  rule matching: 22737598427 nanoseconds (37.74%)
    match add-associativity1: 12969339009 nanoseconds (21.53%)
    match add-associativity2: 12566611332 nanoseconds (20.86%)
    match add-commutativity: 12695876493 nanoseconds (21.07%)
    match distributivity1: 11939847127 nanoseconds (19.82%)
    match distributivity2: 18300449826 nanoseconds (30.38%)
    match mul-associativity1: 7313459563 nanoseconds (12.14%)
    match mul-associativity2: 14558138207 nanoseconds (24.16%)
    match mul-commutativity: 4755402904 nanoseconds (7.89%)
    match multiplicative-identity: 10399775359 nanoseconds (17.26%)
    match power-null: 10010363484 nanoseconds (16.62%)
    match power-reduction: 4680547740 nanoseconds (7.77%)
  union: 16084532847 nanoseconds (26.70%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 213137 iterations in 60 seconds
Average time per iteration: 0.281468 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 86031 iterations in 60 seconds
Average time per iteration: 0.697378 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 21591 iterations in 60 seconds
Average time per iteration: 2.778877 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 3926 iterations in 60 seconds
Average time per iteration: 15.285413 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 563 iterations in 60 seconds
Average time per iteration: 106.576067 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 73 iterations in 60 seconds
Average time per iteration: 822.491023 ms
root: 421321464983 nanoseconds (100.00%)
  add nodes: 27969332637 nanoseconds (6.64%)
  rule application: 138717111044 nanoseconds (32.92%)
    apply add-associativity1: 8549918977 nanoseconds (2.03%)
    apply add-associativity2: 6717803507 nanoseconds (1.59%)
    apply add-commutativity: 2849025371 nanoseconds (0.68%)
    apply distributivity1: 3292702063 nanoseconds (0.78%)
    apply distributivity2: 2632566295 nanoseconds (0.62%)
    apply mul-associativity1: 108561507029 nanoseconds (25.77%)
    apply mul-associativity2: 92550903047 nanoseconds (21.97%)
    apply mul-commutativity: 1262098027 nanoseconds (0.30%)
    apply multiplicative-identity: 64281645 nanoseconds (0.02%)
    apply power-null: 20042819 nanoseconds (0.00%)
    apply power-reduction: 70221032 nanoseconds (0.02%)
  rule matching: 159819877438 nanoseconds (37.93%)
    match add-associativity1: 12969339009 nanoseconds (3.08%)
    match add-associativity2: 12566611332 nanoseconds (2.98%)
    match add-commutativity: 12695876493 nanoseconds (3.01%)
    match distributivity1: 11939847127 nanoseconds (2.83%)
    match distributivity2: 18300449826 nanoseconds (4.34%)
    match mul-associativity1: 123034818626 nanoseconds (29.20%)
    match mul-associativity2: 117002726477 nanoseconds (27.77%)
    match mul-commutativity: 4755402904 nanoseconds (1.13%)
    match multiplicative-identity: 10399775359 nanoseconds (2.47%)
    match power-null: 10010363484 nanoseconds (2.38%)
    match power-reduction: 4680547740 nanoseconds (1.11%)
  union: 41835592990 nanoseconds (9.93%)
# ================Using fixed parallel map with 5 threads================
## Benchmarking poly5 for 60 seconds.
Completed 351 iterations in 60 seconds
Average time per iteration: 171.040448 ms
root: 60222198009 nanoseconds (100.00%)
  add nodes: 5454597607 nanoseconds (9.06%)
  rule application: 10294045563 nanoseconds (17.09%)
    apply add-associativity1: 8608385296 nanoseconds (14.29%)
    apply add-associativity2: 7370812866 nanoseconds (12.24%)
    apply add-commutativity: 3648346073 nanoseconds (6.06%)
    apply distributivity1: 3664122186 nanoseconds (6.08%)
    apply distributivity2: 2814051935 nanoseconds (4.67%)
    apply mul-associativity1: 4077565252 nanoseconds (6.77%)
    apply mul-associativity2: 4602160764 nanoseconds (7.64%)
    apply mul-commutativity: 1437244500 nanoseconds (2.39%)
    apply multiplicative-identity: 75871820 nanoseconds (0.13%)
    apply power-null: 25658602 nanoseconds (0.04%)
    apply power-reduction: 104705236 nanoseconds (0.17%)
  rule matching: 21855596095 nanoseconds (36.29%)
    match add-associativity1: 12778858536 nanoseconds (21.22%)
    match add-associativity2: 12125475126 nanoseconds (20.13%)
    match add-commutativity: 13009045634 nanoseconds (21.60%)
    match distributivity1: 11431590722 nanoseconds (18.98%)
    match distributivity2: 18035014196 nanoseconds (29.95%)
    match mul-associativity1: 7319479352 nanoseconds (12.15%)
    match mul-associativity2: 14132168650 nanoseconds (23.47%)
    match mul-commutativity: 4466913374 nanoseconds (7.42%)
    match multiplicative-identity: 9859750535 nanoseconds (16.37%)
    match power-null: 8614036966 nanoseconds (14.30%)
    match power-reduction: 4478409969 nanoseconds (7.44%)
  union: 17204616964 nanoseconds (28.57%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 204002 iterations in 60 seconds
Average time per iteration: 0.294072 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 82617 iterations in 60 seconds
Average time per iteration: 0.726192 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 22489 iterations in 60 seconds
Average time per iteration: 2.667964 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 4282 iterations in 60 seconds
Average time per iteration: 14.012624 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 615 iterations in 60 seconds
Average time per iteration: 97.684664 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 80 iterations in 60 seconds
Average time per iteration: 752.651395 ms
root: 421485027526 nanoseconds (100.00%)
  add nodes: 29279970918 nanoseconds (6.95%)
  rule application: 136190745237 nanoseconds (32.31%)
    apply add-associativity1: 8608385296 nanoseconds (2.04%)
    apply add-associativity2: 7370812866 nanoseconds (1.75%)
    apply add-commutativity: 3648346073 nanoseconds (0.87%)
    apply distributivity1: 3664122186 nanoseconds (0.87%)
    apply distributivity2: 2814051935 nanoseconds (0.67%)
    apply mul-associativity1: 104715695073 nanoseconds (24.84%)
    apply mul-associativity2: 90571793542 nanoseconds (21.49%)
    apply mul-commutativity: 1437244500 nanoseconds (0.34%)
    apply multiplicative-identity: 75871820 nanoseconds (0.02%)
    apply power-null: 25658602 nanoseconds (0.01%)
    apply power-reduction: 104705236 nanoseconds (0.02%)
  rule matching: 157004901381 nanoseconds (37.25%)
    match add-associativity1: 12778858536 nanoseconds (3.03%)
    match add-associativity2: 12125475126 nanoseconds (2.88%)
    match add-commutativity: 13009045634 nanoseconds (3.09%)
    match distributivity1: 11431590722 nanoseconds (2.71%)
    match distributivity2: 18035014196 nanoseconds (4.28%)
    match mul-associativity1: 120502334382 nanoseconds (28.59%)
    match mul-associativity2: 115046066695 nanoseconds (27.30%)
    match mul-commutativity: 4466913374 nanoseconds (1.06%)
    match multiplicative-identity: 9859750535 nanoseconds (2.34%)
    match power-null: 8614036966 nanoseconds (2.04%)
    match power-reduction: 4478409969 nanoseconds (1.06%)
  union: 44013444680 nanoseconds (10.44%)
# ================Using fixed parallel map with 6 threads================
## Benchmarking poly5 for 60 seconds.
Completed 359 iterations in 60 seconds
Average time per iteration: 167.306454 ms
root: 60213495513 nanoseconds (100.00%)
  add nodes: 5528326162 nanoseconds (9.18%)
  rule application: 9541857900 nanoseconds (15.85%)
    apply add-associativity1: 8088531086 nanoseconds (13.43%)
    apply add-associativity2: 7316896743 nanoseconds (12.15%)
    apply add-commutativity: 3882175172 nanoseconds (6.45%)
    apply distributivity1: 3892318148 nanoseconds (6.46%)
    apply distributivity2: 3113970780 nanoseconds (5.17%)
    apply mul-associativity1: 4282554774 nanoseconds (7.11%)
    apply mul-associativity2: 4871402811 nanoseconds (8.09%)
    apply mul-commutativity: 1465793740 nanoseconds (2.43%)
    apply multiplicative-identity: 84794613 nanoseconds (0.14%)
    apply power-null: 31375362 nanoseconds (0.05%)
    apply power-reduction: 84033050 nanoseconds (0.14%)
  rule matching: 21357773900 nanoseconds (35.47%)
    match add-associativity1: 12437248603 nanoseconds (20.66%)
    match add-associativity2: 12428807272 nanoseconds (20.64%)
    match add-commutativity: 15161497668 nanoseconds (25.18%)
    match distributivity1: 10914826581 nanoseconds (18.13%)
    match distributivity2: 18059257561 nanoseconds (29.99%)
    match mul-associativity1: 7385476401 nanoseconds (12.27%)
    match mul-associativity2: 13455695303 nanoseconds (22.35%)
    match mul-commutativity: 4043641946 nanoseconds (6.72%)
    match multiplicative-identity: 9446303954 nanoseconds (15.69%)
    match power-null: 7926067258 nanoseconds (13.16%)
    match power-reduction: 4715246236 nanoseconds (7.83%)
  union: 18098536434 nanoseconds (30.06%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 180235 iterations in 60 seconds
Average time per iteration: 0.332848 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 77621 iterations in 60 seconds
Average time per iteration: 0.77293 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 22681 iterations in 60 seconds
Average time per iteration: 2.645387 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 4501 iterations in 60 seconds
Average time per iteration: 13.332206 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 673 iterations in 60 seconds
Average time per iteration: 89.24239 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 88 iterations in 60 seconds
Average time per iteration: 689.028197 ms
root: 421849070422 nanoseconds (100.00%)
  add nodes: 30070797764 nanoseconds (7.13%)
  rule application: 133298256417 nanoseconds (31.60%)
    apply add-associativity1: 8088531086 nanoseconds (1.92%)
    apply add-associativity2: 7316896743 nanoseconds (1.73%)
    apply add-commutativity: 3882175172 nanoseconds (0.92%)
    apply distributivity1: 3892318148 nanoseconds (0.92%)
    apply distributivity2: 3113970780 nanoseconds (0.74%)
    apply mul-associativity1: 101165392877 nanoseconds (23.98%)
    apply mul-associativity2: 86688407257 nanoseconds (20.55%)
    apply mul-commutativity: 1465793740 nanoseconds (0.35%)
    apply multiplicative-identity: 84794613 nanoseconds (0.02%)
    apply power-null: 31375362 nanoseconds (0.01%)
    apply power-reduction: 84033050 nanoseconds (0.02%)
  rule matching: 153924945103 nanoseconds (36.49%)
    match add-associativity1: 12437248603 nanoseconds (2.95%)
    match add-associativity2: 12428807272 nanoseconds (2.95%)
    match add-commutativity: 15161497668 nanoseconds (3.59%)
    match distributivity1: 10914826581 nanoseconds (2.59%)
    match distributivity2: 18059257561 nanoseconds (4.28%)
    match mul-associativity1: 117680328636 nanoseconds (27.90%)
    match mul-associativity2: 112967153845 nanoseconds (26.78%)
    match mul-commutativity: 4043641946 nanoseconds (0.96%)
    match multiplicative-identity: 9446303954 nanoseconds (2.24%)
    match power-null: 7926067258 nanoseconds (1.88%)
    match power-reduction: 4715246236 nanoseconds (1.12%)
  union: 46419989423 nanoseconds (11.00%)
# ================Using fixed parallel map with 7 threads================
## Benchmarking poly5 for 60 seconds.
Completed 372 iterations in 60 seconds
Average time per iteration: 161.344437 ms
root: 60165253729 nanoseconds (100.00%)
  add nodes: 5711623161 nanoseconds (9.49%)
  rule application: 9022423325 nanoseconds (15.00%)
    apply add-associativity1: 7586552792 nanoseconds (12.61%)
    apply add-associativity2: 7233789179 nanoseconds (12.02%)
    apply add-commutativity: 4072229008 nanoseconds (6.77%)
    apply distributivity1: 4104168013 nanoseconds (6.82%)
    apply distributivity2: 3181291676 nanoseconds (5.29%)
    apply mul-associativity1: 4486543782 nanoseconds (7.46%)
    apply mul-associativity2: 4782678383 nanoseconds (7.95%)
    apply mul-commutativity: 1559616248 nanoseconds (2.59%)
    apply multiplicative-identity: 94176025 nanoseconds (0.16%)
    apply power-null: 37554650 nanoseconds (0.06%)
    apply power-reduction: 96108015 nanoseconds (0.16%)
  rule matching: 20538522568 nanoseconds (34.14%)
    match add-associativity1: 12325169465 nanoseconds (20.49%)
    match add-associativity2: 12093136366 nanoseconds (20.10%)
    match add-commutativity: 14654517949 nanoseconds (24.36%)
    match distributivity1: 10263837332 nanoseconds (17.06%)
    match distributivity2: 17851002150 nanoseconds (29.67%)
    match mul-associativity1: 7513547456 nanoseconds (12.49%)
    match mul-associativity2: 12956116518 nanoseconds (21.53%)
    match mul-commutativity: 3801670066 nanoseconds (6.32%)
    match multiplicative-identity: 8355186184 nanoseconds (13.89%)
    match power-null: 6130825064 nanoseconds (10.19%)
    match power-reduction: 4321159372 nanoseconds (7.18%)
  union: 18950140617 nanoseconds (31.50%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 172444 iterations in 60 seconds
Average time per iteration: 0.347886 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 76131 iterations in 60 seconds
Average time per iteration: 0.788055 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 23057 iterations in 60 seconds
Average time per iteration: 2.602161 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 4799 iterations in 60 seconds
Average time per iteration: 12.503612 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 726 iterations in 60 seconds
Average time per iteration: 82.650269 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 95 iterations in 60 seconds
Average time per iteration: 633.611852 ms
root: 421214864657 nanoseconds (100.00%)
  add nodes: 30860612612 nanoseconds (7.33%)
  rule application: 130862265261 nanoseconds (31.07%)
    apply add-associativity1: 7586552792 nanoseconds (1.80%)
    apply add-associativity2: 7233789179 nanoseconds (1.72%)
    apply add-commutativity: 4072229008 nanoseconds (0.97%)
    apply distributivity1: 4104168013 nanoseconds (0.97%)
    apply distributivity2: 3181291676 nanoseconds (0.76%)
    apply mul-associativity1: 99414806090 nanoseconds (23.60%)
    apply mul-associativity2: 84731121380 nanoseconds (20.12%)
    apply mul-commutativity: 1559616248 nanoseconds (0.37%)
    apply multiplicative-identity: 94176025 nanoseconds (0.02%)
    apply power-null: 37554650 nanoseconds (0.01%)
    apply power-reduction: 96108015 nanoseconds (0.02%)
  rule matching: 151907353657 nanoseconds (36.06%)
    match add-associativity1: 12325169465 nanoseconds (2.93%)
    match add-associativity2: 12093136366 nanoseconds (2.87%)
    match add-commutativity: 14654517949 nanoseconds (3.48%)
    match distributivity1: 10263837332 nanoseconds (2.44%)
    match distributivity2: 17851002150 nanoseconds (4.24%)
    match mul-associativity1: 116466488975 nanoseconds (27.65%)
    match mul-associativity2: 112312374990 nanoseconds (26.66%)
    match mul-commutativity: 3801670066 nanoseconds (0.90%)
    match multiplicative-identity: 8355186184 nanoseconds (1.98%)
    match power-null: 6130825064 nanoseconds (1.46%)
    match power-reduction: 4321159372 nanoseconds (1.03%)
  union: 48003077568 nanoseconds (11.40%)
# ================Using fixed parallel map with 8 threads================
## Benchmarking poly5 for 60 seconds.
Completed 381 iterations in 60 seconds
Average time per iteration: 157.733854 ms
root: 60230548863 nanoseconds (100.00%)
  add nodes: 5860172481 nanoseconds (9.73%)
  rule application: 8573367271 nanoseconds (14.23%)
    apply add-associativity1: 7181343340 nanoseconds (11.92%)
    apply add-associativity2: 6981966106 nanoseconds (11.59%)
    apply add-commutativity: 4445946946 nanoseconds (7.38%)
    apply distributivity1: 4119001963 nanoseconds (6.84%)
    apply distributivity2: 3249703663 nanoseconds (5.40%)
    apply mul-associativity1: 4759327939 nanoseconds (7.90%)
    apply mul-associativity2: 4734858426 nanoseconds (7.86%)
    apply mul-commutativity: 1585138996 nanoseconds (2.63%)
    apply multiplicative-identity: 98437222 nanoseconds (0.16%)
    apply power-null: 42615564 nanoseconds (0.07%)
    apply power-reduction: 105616974 nanoseconds (0.18%)
  rule matching: 20342148151 nanoseconds (33.77%)
    match add-associativity1: 12044430446 nanoseconds (20.00%)
    match add-associativity2: 11620919214 nanoseconds (19.29%)
    match add-commutativity: 13630228313 nanoseconds (22.63%)
    match distributivity1: 10781196002 nanoseconds (17.90%)
    match distributivity2: 17901753935 nanoseconds (29.72%)
    match mul-associativity1: 7310734933 nanoseconds (12.14%)
    match mul-associativity2: 12767100394 nanoseconds (21.20%)
    match mul-commutativity: 3554362911 nanoseconds (5.90%)
    match multiplicative-identity: 7448080567 nanoseconds (12.37%)
    match power-null: 5220929280 nanoseconds (8.67%)
    match power-reduction: 3778553351 nanoseconds (6.27%)
  union: 19378260032 nanoseconds (32.17%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 172177 iterations in 60 seconds
Average time per iteration: 0.348428 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 76060 iterations in 60 seconds
Average time per iteration: 0.788796 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 23576 iterations in 60 seconds
Average time per iteration: 2.544919 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 4954 iterations in 60 seconds
Average time per iteration: 12.112783 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 764 iterations in 60 seconds
Average time per iteration: 78.603241 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 99 iterations in 60 seconds
Average time per iteration: 610.255793 ms
root: 421537918315 nanoseconds (100.00%)
  add nodes: 31661574491 nanoseconds (7.51%)
  rule application: 128034513278 nanoseconds (30.37%)
    apply add-associativity1: 7181343340 nanoseconds (1.70%)
    apply add-associativity2: 6981966106 nanoseconds (1.66%)
    apply add-commutativity: 4445946946 nanoseconds (1.05%)
    apply distributivity1: 4119001963 nanoseconds (0.98%)
    apply distributivity2: 3249703663 nanoseconds (0.77%)
    apply mul-associativity1: 97852276667 nanoseconds (23.21%)
    apply mul-associativity2: 82317267827 nanoseconds (19.53%)
    apply mul-commutativity: 1585138996 nanoseconds (0.38%)
    apply multiplicative-identity: 98437222 nanoseconds (0.02%)
    apply power-null: 42615564 nanoseconds (0.01%)
    apply power-reduction: 105616974 nanoseconds (0.03%)
  rule matching: 151343593314 nanoseconds (35.90%)
    match add-associativity1: 12044430446 nanoseconds (2.86%)
    match add-associativity2: 11620919214 nanoseconds (2.76%)
    match add-commutativity: 13630228313 nanoseconds (3.23%)
    match distributivity1: 10781196002 nanoseconds (2.56%)
    match distributivity2: 17901753935 nanoseconds (4.25%)
    match mul-associativity1: 115551457286 nanoseconds (27.41%)
    match mul-associativity2: 111895056689 nanoseconds (26.54%)
    match mul-commutativity: 3554362911 nanoseconds (0.84%)
    match multiplicative-identity: 7448080567 nanoseconds (1.77%)
    match power-null: 5220929280 nanoseconds (1.24%)
    match power-reduction: 3778553351 nanoseconds (0.90%)
  union: 49319643429 nanoseconds (11.70%)
# ================Using fixed parallel map with 9 threads================
## Benchmarking poly5 for 60 seconds.
Completed 390 iterations in 60 seconds
Average time per iteration: 154.066437 ms
root: 60255798949 nanoseconds (100.00%)
  add nodes: 5986738675 nanoseconds (9.94%)
  rule application: 8245770846 nanoseconds (13.68%)
    apply add-associativity1: 6940152601 nanoseconds (11.52%)
    apply add-associativity2: 6801449387 nanoseconds (11.29%)
    apply add-commutativity: 4502984737 nanoseconds (7.47%)
    apply distributivity1: 4116275245 nanoseconds (6.83%)
    apply distributivity2: 3380988723 nanoseconds (5.61%)
    apply mul-associativity1: 4833189603 nanoseconds (8.02%)
    apply mul-associativity2: 4565528923 nanoseconds (7.58%)
    apply mul-commutativity: 1635878216 nanoseconds (2.71%)
    apply multiplicative-identity: 107337828 nanoseconds (0.18%)
    apply power-null: 46805472 nanoseconds (0.08%)
    apply power-reduction: 113246862 nanoseconds (0.19%)
  rule matching: 19778284425 nanoseconds (32.82%)
    match add-associativity1: 11932761244 nanoseconds (19.80%)
    match add-associativity2: 11809591967 nanoseconds (19.60%)
    match add-commutativity: 13703045534 nanoseconds (22.74%)
    match distributivity1: 10321566440 nanoseconds (17.13%)
    match distributivity2: 17746120625 nanoseconds (29.45%)
    match mul-associativity1: 7368968140 nanoseconds (12.23%)
    match mul-associativity2: 11976049425 nanoseconds (19.88%)
    match mul-commutativity: 3549048983 nanoseconds (5.89%)
    match multiplicative-identity: 6697836771 nanoseconds (11.12%)
    match power-null: 4524445932 nanoseconds (7.51%)
    match power-reduction: 3175551271 nanoseconds (5.27%)
  union: 20018160097 nanoseconds (33.22%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 169501 iterations in 60 seconds
Average time per iteration: 0.353928 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 75675 iterations in 60 seconds
Average time per iteration: 0.7928 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 23754 iterations in 60 seconds
Average time per iteration: 2.525813 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 5174 iterations in 60 seconds
Average time per iteration: 11.597712 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 792 iterations in 60 seconds
Average time per iteration: 75.802816 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 102 iterations in 60 seconds
Average time per iteration: 589.984934 ms
root: 421294681675 nanoseconds (100.00%)
  add nodes: 32426457800 nanoseconds (7.70%)
  rule application: 125764421950 nanoseconds (29.85%)
    apply add-associativity1: 6940152601 nanoseconds (1.65%)
    apply add-associativity2: 6801449387 nanoseconds (1.61%)
    apply add-commutativity: 4502984737 nanoseconds (1.07%)
    apply distributivity1: 4116275245 nanoseconds (0.98%)
    apply distributivity2: 3380988723 nanoseconds (0.80%)
    apply mul-associativity1: 96072518837 nanoseconds (22.80%)
    apply mul-associativity2: 80613583077 nanoseconds (19.13%)
    apply mul-commutativity: 1635878216 nanoseconds (0.39%)
    apply multiplicative-identity: 107337828 nanoseconds (0.03%)
    apply power-null: 46805472 nanoseconds (0.01%)
    apply power-reduction: 113246862 nanoseconds (0.03%)
  rule matching: 149962243077 nanoseconds (35.60%)
    match add-associativity1: 11932761244 nanoseconds (2.83%)
    match add-associativity2: 11809591967 nanoseconds (2.80%)
    match add-commutativity: 13703045534 nanoseconds (3.25%)
    match distributivity1: 10321566440 nanoseconds (2.45%)
    match distributivity2: 17746120625 nanoseconds (4.21%)
    match mul-associativity1: 114552530843 nanoseconds (27.19%)
    match mul-associativity2: 110950206872 nanoseconds (26.34%)
    match mul-commutativity: 3549048983 nanoseconds (0.84%)
    match multiplicative-identity: 6697836771 nanoseconds (1.59%)
    match power-null: 4524445932 nanoseconds (1.07%)
    match power-reduction: 3175551271 nanoseconds (0.75%)
  union: 50693682782 nanoseconds (12.03%)
# ================Using fixed parallel map with 10 threads================
## Benchmarking poly5 for 60 seconds.
Completed 393 iterations in 60 seconds
Average time per iteration: 152.758482 ms
root: 60169243427 nanoseconds (100.00%)
  add nodes: 5996768396 nanoseconds (9.97%)
  rule application: 7811595261 nanoseconds (12.98%)
    apply add-associativity1: 6570558865 nanoseconds (10.92%)
    apply add-associativity2: 6462605022 nanoseconds (10.74%)
    apply add-commutativity: 4350634699 nanoseconds (7.23%)
    apply distributivity1: 3867520845 nanoseconds (6.43%)
    apply distributivity2: 3314712532 nanoseconds (5.51%)
    apply mul-associativity1: 4748681171 nanoseconds (7.89%)
    apply mul-associativity2: 4293667348 nanoseconds (7.14%)
    apply mul-commutativity: 1590736395 nanoseconds (2.64%)
    apply multiplicative-identity: 119339075 nanoseconds (0.20%)
    apply power-null: 46932701 nanoseconds (0.08%)
    apply power-reduction: 120903808 nanoseconds (0.20%)
  rule matching: 20106644625 nanoseconds (33.42%)
    match add-associativity1: 11920611199 nanoseconds (19.81%)
    match add-associativity2: 11894508685 nanoseconds (19.77%)
    match add-commutativity: 13962701345 nanoseconds (23.21%)
    match distributivity1: 10043988653 nanoseconds (16.69%)
    match distributivity2: 18166596644 nanoseconds (30.19%)
    match mul-associativity1: 7480400502 nanoseconds (12.43%)
    match mul-associativity2: 11687968928 nanoseconds (19.43%)
    match mul-commutativity: 3576202240 nanoseconds (5.94%)
    match multiplicative-identity: 6510646118 nanoseconds (10.82%)
    match power-null: 4383872290 nanoseconds (7.29%)
    match power-reduction: 3068931986 nanoseconds (5.10%)
  union: 20026234360 nanoseconds (33.28%)
## Benchmarking nmm with n=3 for 60 seconds.
Completed 167577 iterations in 60 seconds
Average time per iteration: 0.35799 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 75781 iterations in 60 seconds
Average time per iteration: 0.791694 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 24046 iterations in 60 seconds
Average time per iteration: 2.495247 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 5306 iterations in 60 seconds
Average time per iteration: 11.309693 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 816 iterations in 60 seconds
Average time per iteration: 73.607795 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 105 iterations in 60 seconds
Average time per iteration: 575.845796 ms
root: 421420910430 nanoseconds (100.00%)
  add nodes: 32536931001 nanoseconds (7.72%)
  rule application: 124493386625 nanoseconds (29.54%)
    apply add-associativity1: 6570558865 nanoseconds (1.56%)
    apply add-associativity2: 6462605022 nanoseconds (1.53%)
    apply add-commutativity: 4350634699 nanoseconds (1.03%)
    apply distributivity1: 3867520845 nanoseconds (0.92%)
    apply distributivity2: 3314712532 nanoseconds (0.79%)
    apply mul-associativity1: 95033688248 nanoseconds (22.55%)
    apply mul-associativity2: 78940046213 nanoseconds (18.73%)
    apply mul-commutativity: 1590736395 nanoseconds (0.38%)
    apply multiplicative-identity: 119339075 nanoseconds (0.03%)
    apply power-null: 46932701 nanoseconds (0.01%)
    apply power-reduction: 120903808 nanoseconds (0.03%)
  rule matching: 149862396435 nanoseconds (35.56%)
    match add-associativity1: 11920611199 nanoseconds (2.83%)
    match add-associativity2: 11894508685 nanoseconds (2.82%)
    match add-commutativity: 13962701345 nanoseconds (3.31%)
    match distributivity1: 10043988653 nanoseconds (2.38%)
    match distributivity2: 18166596644 nanoseconds (4.31%)
    match mul-associativity1: 114221711887 nanoseconds (27.10%)
    match mul-associativity2: 109681563698 nanoseconds (26.03%)
    match mul-commutativity: 3576202240 nanoseconds (0.85%)
    match multiplicative-identity: 6510646118 nanoseconds (1.54%)
    match power-null: 4383872290 nanoseconds (1.04%)
    match power-reduction: 3068931986 nanoseconds (0.73%)
  union: 51150845085 nanoseconds (12.14%)
