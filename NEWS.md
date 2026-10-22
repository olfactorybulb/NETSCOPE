# NETSCOPE 0.1.0

* Initial CRAN release of NETSCOPE.

## Differences from the MATLAB implementation

* `matrix_distribution()` uses disk-backed matrices created with
  the R package `bigmemory`, rather than MATLAB `.mat` files.
  This allows large matrices to be processed in batches without
  loading the entire matrix into memory. The MATLAB `varname`
  argument is omitted because each `bigmemory` backing file
  contains a single matrix.
* `get_pathlength()` and `shortestpath()` now share a common MI-to-distance
  transformation that clamps MI values to [0, 1], preventing negative
  distances from out-of-range MI values.
* `shuffle_data()` now defaults to independently permuting observations
  within each variable, preserving variable-wise marginal distributions.
  This differs from the original MATLAB implementation, which permutes
  variables within each sample. The original behavior remains available
  through `margin = "samples"`.
