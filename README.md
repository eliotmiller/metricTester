# metricTester
## A package to explore and test phylogenetic community structure metrics and null models

Many of these functions are detailed in [our Ecography paper](http://onlinelibrary.wiley.com/doi/10.1111/ecog.02070/abstract) that reviews phylogenetic community structure metrics and null models.

#### Why should I use metricTester?
metricTester allows users to define their own spatial simulations, null models, and metrics. A variety of functions then allow users to explore the behavior of these methods (e.g., across varying species richness), and their statistical performance (e.g., ability to detect a pattern in the spatial simulations). Although originally programmed specifically for phylogenetic community structure methods, the package is flexible enough that simulations, models, and metrics can be defined on the fly, and it can harness multiple cores to quickly generate expectations (see examples below). Thus, beyond phylogenetic community structure methods, some may find this package useful for exploring the behavior of any user-defined row- or column-wise matrix calculations as the matrix is repeatedly shuffled according to any user-defined algorithm.

#### How do I use metricTester?
All exported functions are documented and illustrated with examples. To illustrate some of the general functionality of metricTester, here's how you would install (from GitHub), generate a community data matrix (where plots are rows and species are columns), then calculate an arbitrary row-wise metric as the community data matrix is repeatedly randomized according to an arbitrary null model.

```r
library(devtools)
install_github("metricTester/eliotmiller")
library(metricTester)

# simulate a phylogenetic tree with birth-death process. although not needed for this
# example per se, many metricTester functions anticipate a tree, and it's often easier to
# just create one and pass it along
tree <- geiger::sim.bdtree(b = 0.1, d = 0, stop = "taxa", n = 50)

# simulate a log-normal abundance distribution
sim.abundances <- round(rlnorm(5000, meanlog = 2, sdlog = 1)) + 1

# sample from that distribution to create a community data matrix of varying species 
# richness
cdm <- simulateComm(tree, richness.vector = 10:25, abundances = sim.abundances)

# define a new metric. here we will simply calculate the total abundance (i.e. the total
# number of individuals of any species) per plot.
tempMetric <- function(input.vector)
{
	nonZeros <- input.vector[input.vector != 0]
	return(sum(nonZeros))
}

# write a quick wrapper to apply the new metric over a community data matrix. note that the 
# metrics in metricTester expect a prepped object of class metrics.input. downstream
# functions are going to convert your inputs into a prepped metrics.input object, then the
# function is going to run over the $picante.cdm element within the prepped metrics.input
# object. ensure the metric is calculated over this $picante.cdm element
dummyMetric <- function(metrics.input)
{
	results <- apply(metrics.input$picante.cdm, 1, tempMetric)
	results
}

# define a new null model. here we will simply completely shuffle the contents of the
# community data matrix. like the above, your inputs will be automatically converted into
# a prepped object of nulls.input. so, make sure the null model runs over 
# nulls.input$picante.cdm
dummyNull <- function(nulls.input)
{
	results <- matrix(nrow = dim(nulls.input$picante.cdm)[1],
	ncol=dim(nulls.input$picante.cdm)[2], sample(unlist(nulls.input$picante.cdm)))
	rownames(results) <- row.names(nulls.input$picante.cdm)
	colnames(results) <- names(nulls.input$picante.cdm)
	results
}

# see what we expect plot-level total abundance to be after repeatedly randomizing the
# matrix 100 times. note that metricTester always expects richness to be included, so this
# example wouldn't work if we did not include it as a metric.
expectations(tree = tree, picante.cdm = cdm, nulls = list("fullShuffle" = dummyNull),
metrics = list("richness" = metricTester:::my_richness, "totalAbund" = dummyMetric),
randomizations = 100, concat.by = "plot", cores = 8)
```

#### How do I get metricTester?
metricTester is available on [CRAN](https://cran.r-project.org/web/packages/metricTester/index.html). Updates may also be available more frequently/sooner via the [GitHub site](https://github.com/eliotmiller/metricTester/). See above for how to install directly from GitHub.

#### The software DOI released in conjunction with our Ecography paper is available [here](https://zenodo.org/badge/latestdoi/21050/eliotmiller/metricTester).

The phylogenetic and trait field functions (`phyloField`, `traitField`, `sesPhyloField`, and `sesTraitField`) are explained in a paper with Sarah Wagner, Luke Harmon, and Robert Ricklefs on [honeyeater ecomorphology](http://www.biorxiv.org/content/early/2015/12/14/034389). However, these functions are being deprecated. They have been replaced by `prepFieldData`, `calcField`, and `sesField`. These functions automatically detect whether the user is interested in calculating phylogenetic or trait field, and allow for a much wider use of metrics and null models in the calculations. Eventually the functions used in our paper will be entirely removed from metricTester.
