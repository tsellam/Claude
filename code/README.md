Implementation of Claude in R.

* `MakeViews.R` is the main file, which calls all the other scripts in the repository.
* The `Rlib` directory contains fast C functions to compute  information theoretic estimators (e.g., entropy, mutual information, variation of information)
* The `Java` directory contains an implementation of beam search, for comparison with R. This is legacy code, to be removed.
* The `4S` directory contains binaries for the 4S subspace search algorithm, also used for experiments. Provided by Hoang Vu Nguyen and Emmanuel Muller.