# diegr 0.3.0

* Added new sensor templates: BioSemi 128, BioSemi 256, and system 10-05.
* Updated graphical functions to ensure compatibility with the new templates.
* Added a fallback to `MASS::ginv()` for ill-conditioned interpolation matrices.
* Rewrote `point_mesh()` using `sf` instead of the `sp` package.
* Fixed bugs in `outliers_epoch()`, timeline rendering in animations and baseline range in examples and documentation.
* Added static version of outputs for boxplots.
* Updated the installation method for the development version in the README, edited troubleshooting section in vignette. 


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
