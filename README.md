# metricTester
## A package to explore and test phylogenetic community structure metrics and null models

Many of these functions are detailed in [our Ecography paper](http://onlinelibrary.wiley.com/doi/10.1111/ecog.02070/abstract) that reviews phylogenetic community structure metrics and null models.

metricTester allows users to define their own spatial simulations, null models, and metrics. A variety of functions then allow users to explore the behavior of these methods (e.g., across varying species richness), and their statistical performance (e.g., ability to detect a pattern in the spatial simulations). Although originally programmed specifically for phylogenetic community structure methods, the package is flexible enough that simulations, models, and metrics can be defined on the fly, and it can harness multiple cores to quickly generate expectations. Thus, beyond phylogenetic community structure methods, some may find this package useful for exploring the behavior of any user-defined row- or column-wise matrix calculations as the matrix is repeatedly shuffled according to any user-defined algorithm.

All exported functions are carefully documented and illustrated with examples. Additional examples will be included here soon.

metricTester will soon be available on CRAN. Updates may also be available more frequently/sooner via the [GitHub site](https://github.com/eliotmiller/metricTester/).

#### The software DOI released in conjunction with our Ecography paper is available [here](https://zenodo.org/badge/latestdoi/21050/eliotmiller/metricTester).

The phylogenetic and trait field functions (`phyloField`, `traitField`, `sesPhyloField`, and `sesTraitField`) are explained in a paper with Sarah Wagner, Luke Harmon, and Robert Ricklefs on [honeyeater ecomorphology](http://www.biorxiv.org/content/early/2015/12/14/034389).

**Note** that these field functions  are currently limited to three metrics and two null models. I am working on a more general adaptation of these approaches to the metricTester framework. Specifically, users will prep their data like they already do with `prepData`, then calculate the actual metric(s) and null model(s) of their choice on the prepped object with a function like `calcMetrics`. This soon-to-come function will detect whether to calculate the phylogenetic or trait field based on the inputs in the prepped object. **These updates are liable to break the current field functions**, although I intend to deprecate the current functions for some period of time to minimize conflicts.