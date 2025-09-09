import subprocess
import time
import statistics
import os

EGGLOG_RELEASE = "/Users/aziz/dev/lib/egglog/target/release/egglog"
EGGLOG_EXP_RELEASE = "/Users/aziz/dev/lib/egglog-experimental/target/release/egglog-experimental"

def run_benchmark(executable, filename, duration=60):
    print(f"## Benchmarking {filename} for {duration} seconds.")
    start = time.time()
    times = []
    while time.time() - start < duration:
        out = subprocess.run([executable, filename],
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True
        ).stdout

        last_line = out.strip().split('\n')[-1]
        time_ms = float(last_line.split(': ')[1].replace('ms', ''))
        times.append(time_ms)
    
    median = statistics.median(times)
    print(f"Median time per iteration: {median:.6f}ms over {len(times)} runs for {time.time() - start:.2f}s")

def main():
    # Benchmark poly.egg with EGGLOG_RELEASE
    run_benchmark(EGGLOG_RELEASE, "poly.egg")

    # Run benchmarks
    for filename in [3, 5, 10, 20, 40, 80]:
        filename = f"{filename}mm.egg"
        if not os.path.exists(filename):
            print(f"File {filename} does not exist, skipping.")
            continue
        run_benchmark(EGGLOG_EXP_RELEASE, filename)

if __name__ == "__main__":
    main()