# metricTester 1.0.2

* In metricTester, any plot that contains fewer than 2 species is cut from analysis. In this update, I added a check to see whether all plots contain fewer than 2 species, and would cause the analysis to fail. Currently the function still just throws an error, with a more descriptive error message. Could easily change to resample new plots within the arena, but the truth is that if this happens once, it's likely that it will happen on another iteration. It's instead better to stop the simulation and restart with an increased density of individuals in the arena.

# metricTester 1.0.1

* Added code to run all parallel functions sequentially by default. User must now explicitly invoke parallel processing, to avoid issues on Windows machines.

# metricTester 1.0.0

* This is the first submission of metricTester to CRAN. No other news to report.