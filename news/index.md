# Changelog

## diegr 0.2.0

CRAN release: 2026-01-24

- Added support for multiple conditions and groups in the input data to
  improve flexibility and generality.
- Added the option to calculate the weighted mean in `compute_mean`.
- Added functions for summary statistics computation.
- Improved code readability through the use of internal helper
  functions.
- Internal refactoring of source files.
- Fixed sensor order in animations and average plotting in
  interactive_waveforms().
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
