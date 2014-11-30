library(metricTester)
context("Simple community simulation")

tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)

sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1))

cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)

results <- allMetrics(tree, cdm)

#this will change if you ever add more metrics
test_that("results have correct dimensions",{
	expect_true(dim(results)[1] == 16)
	expect_true(dim(results)[2] == 21)
})

test_that("results are class data frame",
{
	expect_is(results, "data.frame")
})

test_that("results do not have NA",
{
	expect_true(sum(is.na(results)) == 0)
})
