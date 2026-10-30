# NETSCOPE 0.1.0

* Initial release of the NETSCOPE R package.

## Differences from the MATLAB implementation

* `matrix_distribution()` uses disk-backed matrices created with
  the R package `bigmemory`, rather than MATLAB `.mat` files.
  This allows large matrices to be processed in batches without
  loading the entire matrix into memory. The MATLAB `varname`
  argument is omitted because each `bigmemory` backing file
  contains a single matrix.
* `get_pathlength()` and `shortestpath()` share a common MI-to-distance
  transformation that clamps MI values to [0, 1], preventing negative
  distances from out-of-range MI values.
* `shuffle_data()` defaults to independently permuting observations
  within each variable, preserving variable-wise marginal distributions.
  This differs from the original MATLAB implementation, which permutes
  variables within each sample. The original behavior remains available
  through `margin = "samples"`.
* `sort_matrix()` may produce a different node ordering from the MATLAB
  implementation because PCA component signs are not uniquely determined.
  Although the PCA solutions are mathematically equivalent, differences
  in score signs can affect the sorting order.
