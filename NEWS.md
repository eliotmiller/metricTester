# metricTester 1.2.0

* Functional dispersion functions, sensu Laliberte and Legendre (2010). The two new relevant functions are FDis and centers. They take similar inputs to dbFD from the FD package, but can be calculated across a variety of ordination spaces (PCA, PCoA, NMDS, etc), instead of only with a PCoA. These functions and the theoretical background are still experimental, and should be treated as such.

# metricTester 1.1.0

* Added prepFieldData, calcField, and sesField. Calculation of phylogenetic and trait fields is now a two-step process, where the data are prepped, then the metrics and nulls of interest are calculated over the prepped object. phyloField, traitField, sesPhyloField, and sesTraitField will be deprecated in future versions of the package.

# metricTester 1.0.3

* Added mean root distance (MRD) and mean distance to the most recent common ancestor of a sample (distMRCA). For MRD, added both presence-absence and abundance-weighted forms, and for distMRCA, added ability to calculate both the distance to the overall MRCA of a sample, and the distance to the average of the pairwise MRCAs.

# metricTester 1.0.2

* Added more informative error message for instances where the specified simulation parameters yield community data matrices with insufficient individuals/species.

# metricTester 1.0.1

* Added code to run all parallel functions sequentially by default. User must now explicitly invoke parallel processing, to avoid issues on Windows machines.

# metricTester 1.0.0

* This is the first submission of metricTester to CRAN. No other news to report.