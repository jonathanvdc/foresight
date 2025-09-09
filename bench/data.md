# Egglog
## Benchmarking poly.egg for 60 seconds.
Median time per iteration: 11.734645ms over 4200 runs for 60.01s
## Benchmarking 3mm.egg for 60 seconds.
Median time per iteration: 1.804500ms over 13403 runs for 60.00s
## Benchmarking 5mm.egg for 60 seconds.
Median time per iteration: 2.294292ms over 12098 runs for 60.00s
## Benchmarking 10mm.egg for 60 seconds.
Median time per iteration: 3.669084ms over 9405 runs for 60.00s
## Benchmarking 20mm.egg for 60 seconds.
Median time per iteration: 7.639459ms over 5741 runs for 60.01s
## Benchmarking 40mm.egg for 60 seconds.
Median time per iteration: 22.113333ms over 2376 runs for 60.02s
## Benchmarking 80mm.egg for 60 seconds.
Median time per iteration: 106.567042ms over 539 runs for 60.01s

# Egg
## Benchmarking poly5 for 60 seconds.
Completed 912 runs.
Median time per iteration: 10.519ms
## Benchmarking 3mm for 60 seconds.
Completed 552996 runs.
Median time per iteration: 0.017ms
## Benchmarking 5mm for 60 seconds.
Completed 313982 runs.
Median time per iteration: 0.031ms
## Benchmarking 10mm for 60 seconds.
Completed 90624 runs.
Median time per iteration: 0.108ms
## Benchmarking 20mm for 60 seconds.
Completed 15537 runs.
Median time per iteration: 0.64ms
## Benchmarking 40mm for 60 seconds.
Completed 2057 runs.
Median time per iteration: 4.833ms
## Benchmarking 80mm for 60 seconds.
Completed 268 runs.
Median time per iteration: 37.155ms

# Slotted-egraphs
## Benchmarking poly5 for 60 seconds.
Completed 42 runs.
Median time per iteration: 239.018ms
## Benchmarking 3mm for 60 seconds.
Completed 75021 runs.
Median time per iteration: 0.131ms
## Benchmarking 5mm for 60 seconds.
Completed 19438 runs.
Median time per iteration: 0.511ms
## Benchmarking 10mm for 60 seconds.
Completed 3013 runs.
Median time per iteration: 3.3ms
## Benchmarking 20mm for 60 seconds.
Completed 421 runs.
Median time per iteration: 23.733ms
## Benchmarking 40mm for 60 seconds.
Completed 54 runs.
Median time per iteration: 188.032ms
## Benchmarking 80mm for 60 seconds.
Completed 7 runs.
Median time per iteration: 1557.796ms

# hegg
## benchmarking rewrite poly5
time                 340.0 ms   (314.5 ms .. 363.6 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 346.2 ms   (339.0 ms .. 357.8 ms)
std dev              10.98 ms   (3.046 ms .. 13.71 ms)
variance introduced by outliers: 19% (moderately inflated)
## benchmarking rewrite mm3
time                 40.93 μs   (40.86 μs .. 41.02 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 40.94 μs   (40.89 μs .. 41.07 μs)
std dev              260.6 ns   (133.6 ns .. 515.3 ns)

## benchmarking rewrite mm5
time                 172.4 μs   (172.1 μs .. 172.8 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 172.2 μs   (172.0 μs .. 172.5 μs)
std dev              783.1 ns   (567.6 ns .. 1.038 μs)

## benchmarking rewrite mm10
time                 1.328 ms   (1.323 ms .. 1.332 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.325 ms   (1.323 ms .. 1.326 ms)
std dev              5.559 μs   (4.356 μs .. 7.192 μs)

## benchmarking rewrite mm20
time                 12.82 ms   (12.76 ms .. 12.89 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.79 ms   (12.77 ms .. 12.82 ms)
std dev              61.09 μs   (47.76 μs .. 81.98 μs)

## benchmarking rewrite mm40
time                 115.4 ms   (113.7 ms .. 117.1 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 116.4 ms   (115.1 ms .. 119.5 ms)
std dev              2.949 ms   (535.9 μs .. 4.580 ms)
variance introduced by outliers: 11% (moderately inflated)

## benchmarking rewrite mm80
time                 1.054 s    (1.001 s .. 1.136 s)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.084 s    (1.068 s .. 1.102 s)
std dev              18.90 ms   (9.762 ms .. 26.57 ms)
variance introduced by outliers: 19% (moderately inflated)

# Foresight
## Benchmarking poly5 for 60 seconds.
Completed 478 iterations in 60 seconds
Median time per iteration: 119.005333 ms
## Benchmarking nmm with n=3 for 60 seconds.
Completed 153818 iterations in 60 seconds
Median time per iteration: 0.378334 ms
## Benchmarking nmm with n=5 for 60 seconds.
Completed 72741 iterations in 60 seconds
Median time per iteration: 0.813709 ms
## Benchmarking nmm with n=10 for 60 seconds.
Completed 25746 iterations in 60 seconds
Median time per iteration: 2.277209 ms
## Benchmarking nmm with n=20 for 60 seconds.
Completed 6685 iterations in 60 seconds
Median time per iteration: 8.74725 ms
## Benchmarking nmm with n=40 for 60 seconds.
Completed 1028 iterations in 60 seconds
Median time per iteration: 57.752583 ms
## Benchmarking nmm with n=80 for 60 seconds.
Completed 122 iterations in 60 seconds
Median time per iteration: 485.799833 ms