scala-stratify
==============
Cumulative Square Root Frequency Stratification in Scala. Computes an approximately
optimal stratification assuming a stratified random sampling design with Neyman allocation.
This method was originally proposed by Dalenius and Hodges (1959).

Use
===
```
val table = stratify(data, 4, 23)
```
stratifies `data` into 4 strata with 23-1 equal-sized intervals used to aggregate
the cumulative square root frequency.

See `stratify.scala` for example use.
Use `sbt run` to run tests.