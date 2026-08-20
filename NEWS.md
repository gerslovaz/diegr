# diegr 0.3.1
* Fixed bug in checks for empty input data.

# diegr 0.3.0

* Added functions for creating interactive surface plots and checking data structures.
* Added new sensor templates: BioSemi 128, BioSemi 256, and the international 10-05 system.
* Updated graphical functions to ensure compatibility with the new templates.
* Added metadata to computational and graphical functions.
* Added validation in `point_mesh()` and `make_triangulation()` to detect duplicate sensor coordinates.
* Added exit checks for empty input data across all core computational and plotting functions.
* Changed default palette in `create_scale()` to a perceptually uniform `"redblue"` scale and added range symmetrization around zero.
* Improved handling of `NA` values within `compute_mean()`, `outliers_epoch()`, and `baseline_correction()`.
* Added a fallback to `MASS::ginv()` for ill-conditioned interpolation matrices.
* Rewrote `point_mesh()` using `sf` instead of the `sp` package.
* Fixed bugs in `outliers_epoch()`, animation timeline rendering, and the baseline ranges used in examples and documentation.
* Added static versions of boxplot outputs.
* Updated the installation instructions for the development version in the README and revised the vignette's troubleshooting section. 


# diegr 0.2.0

* Added support for multiple conditions and groups in the input data to improve flexibility and generality.
* Added the option to calculate the weighted mean in `compute_mean()`.
* Added functions for summary statistics computation.
* Improved code readability through the use of internal helper functions.
* Internal refactoring of source files.
* Fixed sensor order in animations and average plotting in `interactive_waveforms()`.
* Added a `NEWS.md` file to track changes to the package.

## Breaking changes
- Removed arguments `subject` and `channel` from `interactive_waveforms()` and `boxplot_epoch()`, removed arguments `epoch` and `channel` from `boxplot_subject()`.
- Removed arguments `subject` and `sensor` from `outliers_epoch()`.
- `compute_mean()`: renamed argument `group` to `domain` (to distinguish it from the data column), removed arguments `subject`, `channel`, `time`, `ex_epoch`.


# diegr 0.1.0

* Initial release to CRAN.
