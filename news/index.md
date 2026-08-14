# Changelog

## diegr 0.3.0

- Added functions for creating interactive surface plots and checking
  data structure.
- Added new sensor templates: BioSemi 128, BioSemi 256, and system
  10-05.
- Updated graphical functions to ensure compatibility with the new
  templates.
- Added metadata to computing and graphic functions.
- Changed default palette in
  [`create_scale()`](https://gerslovaz.github.io/diegr/reference/create_scale.md)
  to a perceptually uniform `"redblue"` scale and added range
  symmetrization around zero.
- Updated dealing with `NA` values within
  [`compute_mean()`](https://gerslovaz.github.io/diegr/reference/compute_mean.md),
  [`outliers_epoch()`](https://gerslovaz.github.io/diegr/reference/outliers_epoch.md)
  and
  [`baseline_correction()`](https://gerslovaz.github.io/diegr/reference/baseline_correction.md).
- Added a fallback to
  [`MASS::ginv()`](https://rdrr.io/pkg/MASS/man/ginv.html) for
  ill-conditioned interpolation matrices.
- Rewrote
  [`point_mesh()`](https://gerslovaz.github.io/diegr/reference/point_mesh.md)
  using `sf` instead of the `sp` package.
- Fixed bugs in
  [`outliers_epoch()`](https://gerslovaz.github.io/diegr/reference/outliers_epoch.md),
  timeline rendering in animations and baseline range in examples and
  documentation.
- Added static version of outputs for boxplots.
- Updated the installation method for the development version in the
  README, edited troubleshooting section in vignette.

## diegr 0.2.0

CRAN release: 2026-01-24

- Added support for multiple conditions and groups in the input data to
  improve flexibility and generality.
- Added the option to calculate the weighted mean in
  [`compute_mean()`](https://gerslovaz.github.io/diegr/reference/compute_mean.md).
- Added functions for summary statistics computation.
- Improved code readability through the use of internal helper
  functions.
- Internal refactoring of source files.
- Fixed sensor order in animations and average plotting in
  [`interactive_waveforms()`](https://gerslovaz.github.io/diegr/reference/interactive_waveforms.md).
- Added a `NEWS.md` file to track changes to the package.

### Breaking changes

- Removed arguments `subject` and `channel` from
  [`interactive_waveforms()`](https://gerslovaz.github.io/diegr/reference/interactive_waveforms.md)
  and
  [`boxplot_epoch()`](https://gerslovaz.github.io/diegr/reference/boxplot_epoch.md),
  removed arguments `epoch` and `channel` from
  [`boxplot_subject()`](https://gerslovaz.github.io/diegr/reference/boxplot_subject.md).
- Removed arguments `subject` and `sensor` from
  [`outliers_epoch()`](https://gerslovaz.github.io/diegr/reference/outliers_epoch.md).
- [`compute_mean()`](https://gerslovaz.github.io/diegr/reference/compute_mean.md):
  renamed argument `group` to `domain` (to distinguish it from the data
  column), removed arguments `subject`, `channel`, `time`, `ex_epoch`.

## diegr 0.1.0

CRAN release: 2025-09-23

- Initial release to CRAN.
