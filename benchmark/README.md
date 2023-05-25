# Benchmarking and Profiling

## Scala JS

Open in [index.html](./index.html) a browser. 

Use devtool profiling ...

## Scala JVM

Run from `sbt` using 
```
benchmark/jmh:run -i 3 -wi 3 -f1 -t1
benchmark/jmh:run
benchmark/jmh:run -h
```

Profiling hints:
- Java Flight Recorder
```
benchmark/jmh:run -prof jfr:help 
benchmark/jmh:run -i 2 -wi 2 -f1 -t1 -prof jfr
```
- async-profiler (needs to be installed separately)
```
benchmark/jmh:run -prof async:help
benchmark/jmh:run -i 2 -wi 2 -f1 -t1 -prof async
```

## Adding a benchmark case

- add a function to `diesel.benchmark.BenchmarkCases` in _shared_ sources.
- integrate with JS in _js_ sources: `diesel.benchmark.DieselBenchmark`
- integrate with JVM in _jvm_ sources: `diesel.benchmark.DieselBenchmark`

## Links
- https://github.com/japgolly/scalajs-benchmark
- https://github.com/sbt/sbt-jmh