# Example high-density (HD-EEG) epoched data

This dataset is a short slice of a HD-EEG dataset from a study
investigating the impact of deep brain stimulation on patients with
advanced Parkinson's disease (Madetko-Alster, 2025). During the
experiment subjects performed a simple visual motor task (pressing the
response button in case of target visual stimulus presentation). The
data was measured by 256-channel HydroCel Geodesic Sensor Net and
sampling frequency is 250 Hz. The study was carried out by Central
European Institute of Technology in Brno and was supported by Czech
Health Research Council AZV NU21-04-00445.

Example dataset contains amplitude values measured on chosen 204
channels in 50 time points (with the stimulus in the time point 10) for
2 representative subjects (one patient and one healthy control subject).
From the total number of 50 epochs for each subject, 14 (or 15) epochs
were selected for the sample dataset. This data is intended for testing
EEG preprocessing and visualization methods.

## Usage

``` r
data("epochdata")
```

## Format

The data frame consist of 295 800 rows (50 time points x 204 sensors x
29 epochs) and five columns:

- time:

  Number of time point. Time point 10 corresponds to stimulus onset (0
  ms) and the interval between two time points corresponds to the time
  period 4 ms.

- signal:

  HD-EEG signal amplitude, in microvolts.

- epoch:

  Factor variable with epoch number, 14 epochs for subject 1, 15 epochs
  for subject 2.

- sensor:

  Sensor label, according to labeling used in the EGI Geodesic Sensor
  Net Technical Manual.

- subject:

  Factor variable with subject ID, 1 - representative healthy control
  subject, 2 - representative patient subject.

## Source

Central European Institute of Technology, Masaryk University, Brno,
Czech Republic.

## References

EGI Geodesic Sensor Net Technical Manual (2024)
<https://www.egi.com/knowledge-center>

Madetko-Alster N., Alster P., Lamoš M., Šmahovská L., Boušek T., Rektor
I. and Bočková M. The role of the somatosensory cortex in self-paced
movement impairment in Parkinson’s disease. Clinical Neurophysiology.
2025, vol. 171, 11-17.

## Examples

``` r
# Data preview
head(epochdata)
#>   time     signal epoch sensor subject
#> 1    1  13.112123     1     E1       1
#> 2    1  16.480782     1     E2       1
#> 3    1  12.497802     1     E3       1
#> 4    1 -18.158045     1     E4       1
#> 5    1   6.519290     1     E5       1
#> 6    1   7.407604     1     E6       1
```
