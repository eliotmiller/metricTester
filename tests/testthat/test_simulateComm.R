library(metricTester)
context("Simple community simulation")

tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)

sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1))

cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)

test_that("cdm has correct dimensions",{
	expect_true(dim(cdm)[1] == 16)
	expect_true(dim(cdm)[2] == 50)
})

test_that("cdm is class data frame",
{
	expect_is(cdm, "data.frame")
})

test_that("cdm properly filled",
{
	expect_true(sum(cdm, na.rm=TRUE) != 0)
})

test_that("row and column names are correct",
{
	expect_true(sum(row.names(cdm) == paste("quadrat",1:dim(cdm)[1], sep=""))==16)
	expect_true(sum(names(cdm) == tree$tip.label)==50)
})
