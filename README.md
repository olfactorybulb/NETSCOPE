# NETSCOPE

An R package for information theoretical analysis of molecular networks.
This is an R port of the original [NETSCOPE MATLAB/Octave/Python
toolbox](https://github.com/DepartmentofNeurophysiology/NETSCOPE), ported
from the toolbox's master branch with help from Claude.

## Introduction

NETSCOPE can be used for network construction and analysis from a wide
variety of biological data. Applications range from constructing gene
co-expression networks and using topological patterns to identify genes
or pathways of interest, to identifying functional links in fMRI- and
EEG-based networks. The pipeline computes pairwise mutual information
between variables, sparsifies the resulting network using a Data
Processing Inequality (DPI)-based approach, and provides a suite of
network analysis tools (centrality, shortest paths, connected components,
clustering coefficients, and more).

This package is plug-and-play — install it and start importing your data.

## Installation

Install directly from GitHub:

```r
# install.packages("devtools")
devtools::install_github("olfactorybulb/NETSCOPE")
```

## Documentation

![Workflow](https://github.com/DepartmentofNeurophysiology/NETSCOPE/blob/master/Documentation/workflow.png)

Use `?<function>` or `help(<function>)` in R to see detailed
documentation for any function in the package.

## Supporting data

### Network construction from synthetic data

`synthetic_data.R` contains everything necessary to recreate the
synthetic-data network construction analysis from the original manuscript
(Figure 3). The code generates synthetic expression data from a ground
truth network, constructs an MI-based network, and compares it to the
ground truth network. See the `synthetic-data-validation` vignette for a
narrative walkthrough with plots, and `plot_synthetic_data.R` for
standalone plotting.

### Network construction from yeast expression data

As in the original toolbox, `yeast_data.mat` contains the gene expression
data from Ziemann et al., and the co-expression network data from
YeastNet v3 (see Table 1 of the manuscript for details on the network
data). This package includes `R.matlab` as a dependency so this `.mat`
file can be read directly into R.

## Manuscript

Bergmans T, Jamal T, Rezeika A, Hsing C, Celikel T. 2026. "NETSCOPE:
Information-Theory Based Network Discovery and Analysis" *(in
submission)*

## Author

This R port is maintained at the [Georgia Institute of Technology, School
of Psychology and Brain Sciences](https://github.com/olfactorybulb).

---

<!-- This R port was developed with assistance from Claude (Anthropic).  -->
