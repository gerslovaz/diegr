# Database workflows for HD-EEG data in R

    #> Warning in rgl.init(initValue, onlyNULL): RGL: unable to open X11 display
    #> Warning: 'rgl.init' failed, will use the null device.
    #> See '?rgl.useNULL' for ways to avoid this warning.

This article introduces basic principles of working with databases in R
and demonstrates how to convert HD-EEG data stored in BrainVision or
HDF5 file format into database tables compatible with the `diegr`
package.

## Database-oriented approach in `diegr`

Because of its versatility, EEG is used in a wide range of applications,
from clinical practice to research and commercial use. As a result, the
market offers a large variety of EEG systems produced by different
manufacturers. These systems often rely on distinct hardware designs,
accompanying software, and proprietary data formats. This diversity
poses a major challenge when developing R packages for working with EEG
and high-density EEG (HD-EEG) data.

To address this challenge, `diegr` adopts a database-oriented approach
and consistently builds on `dplyr` throughout the package. EEG data can
be handled either as in-memory `data.frame`s or as database tables,
using the same unified, memory-efficient workflow. Existing datasets can
be converted to database tables using standard R database interfaces
(concrete examples are shown in further sections) or external tools.

### Why a database-oriented approach?

- **Memory efficiency**  
  Large EEG and HD-EEG datasets do not need to be fully loaded into
  memory.

- **Consistent dplyr-based workflows**  
  All data manipulation in diegr is built on `dplyr`, making operations
  identical for in-memory `data.frame`s and database tables.

- **Unified data representation**  
  Database tables follow the same tabular structure as `data.frame`s,
  avoiding the need to support and maintain multiple
  manufacturer-specific input formats.

- **Long-term stability**  
  Proprietary EEG file formats may change or become obsolete over time,
  while relational databases rely on stable, well-established standards.

- **Scalability**  
  The same analysis code can be used for small example datasets and for
  large EEG recordings without modification.

### Required data structure

Functions in the `diegr` package expect the input data to be stored in a
*long-format* table (e.g. a `data.frame`, `tibble`, or a database table
with the same structure), where each row corresponds to a single
observation of the EEG signal at a given time point.

The dataset should contain the following columns:

- `group` Identifier of the experimental group (e.g. *control*,
  *patient*).

- `subject` Identifier of the subject.

- `sensor` Sensor (electrode) label (e.g. *E1*, *Fz*).

- `epoch` Epoch number.

- `condition` Label of the experimental condition.

- `time` Time points expressed as sampling indices (not in
  milliseconds).

- `signal` EEG signal amplitude, typically in microvolts (µV).  
  Note: In most functions, the name of this column can be specified via
  an argument, so it does not always have to be strictly called
  `signal`.

**Minimal requirements**

Not all columns listed above are required for every function, also the
order of the columns is arbitrary. However, **if a variable is present
in the dataset, it must follow the naming convention described above**.
This ensures that the functions can correctly identify and process the
data.

**Example**

An example of a valid data structure might look like this:

``` r
head(data)
#>   subject sensor epoch time signal
#> 1      1     E1     1    1   2.34
#> 2      1     E1     1    2   2.10
#> 3      1     E1     1    3   1.98
```

## Database basics in R

Throughout this article, we rely on the `DBI`, `dbplyr`, and `RSQLite`
packages for working with databases in R. Rather than covering database
concepts in depth, we focus on a small set of practical commands for
connecting to a database, reading tables into R, and creating database
tables directly from R.

Readers who are new to working with databases in R may find it helpful
to consult, for example, the chapter *Databases* in Hadley Wickham’s *R
for Data Science*, available online at
<https://r4ds.hadley.nz/databases.html>, and *SQLite in R* tutorial from
*Datacamp*: <https://www.datacamp.com/tutorial/sqlite-in-r>.  
A wide range of additional resources on working with databases in R is
also available.

**Basic database commands**

Below is a brief overview of basic database commands in R with `DBI`,
`dbplyr`, and `RSQLite`:

| Command                                                                 | Description                                                                                                                                  |
|-------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------|
| [`dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)         | Establishes a connection to a database. User specifies the database driver and location.                                                     |
| [`dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html)   | Closes the connection to the database.                                                                                                       |
| [`dbListTables()`](https://dbi.r-dbi.org/reference/dbListTables.html)   | Lists all tables available in the connected database.                                                                                        |
| [`dbListFields()`](https://dbi.r-dbi.org/reference/dbListFields.html)   | Lists all columns available in the selected database table.                                                                                  |
| [`dbReadTable()`](https://dbi.r-dbi.org/reference/dbReadTable.html)     | Reads a table from the database into R as a `data.frame` or `tibble`.                                                                        |
| [`dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html)   | Writes a `data.frame` or `tibble` from R into the database as a new table.                                                                   |
| [`dbRemoveTable()`](https://dbi.r-dbi.org/reference/dbRemoveTable.html) | Deletes a database table from database.                                                                                                      |
| [`dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html)         | Executes a SQL command that does not return a result, e.g., creating or deleting tables.                                                     |
| `tbl()`                                                                 | Creates a `dplyr`-friendly reference to a database table. Allows using `dplyr` verbs (`filter`, `select`, etc.) directly on database tables. |

## Converting HD-EEG data into database tables

As mentioned above, EEG (HD-EEG) data can be stored in a variety of file
formats. Such data can be converted into a database table using any
database software the user is familiar with, including tools and
packages available in R.

In this article, we demonstrate the data preparation workflow using two
file formats that are very common in the HD-EEG area: *BrainVision* and
*HDF5* (the latter being a frequent export format from MATLAB). For
other data formats, the overall procedure is analogous. Moreover, in
many cases these formats can be converted to one of the two mentioned
formats prior to importing the data into R.

The workflow from an EEG data file to a database table can be divided
into four main steps: connecting to the database, importing the data
into R, converting the data to long format, and writing the data to a
database table.  
Each step is demonstrated in the following section with example code.

### Step 1 – Connecting to the database

The first step is **to establish a connection to the database**. Assume
we have a database named *mydatabase.db*. In R, you can connect to it
using the `DBI` and `RSQLite` packages as follows:

``` r
library(DBI)
con <- DBI::dbConnect(RSQLite::SQLite(),
                      dbname = "mydatabase.db")
```

Note: **When using `RSQLite`, the database file is automatically created
if it does not exist.** For other database backends (e.g., MySQL,
PostgreSQL), the database must usually exist beforehand, or you need to
create it explicitly using
[`dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html) before
connecting.

### Step 2 – Import data into R

**BrainVision file format**

An EEG recording in the BrainVision Core Data Format consists of three
separate files:

- **Header file (.vhdr)**: A text file containing recording parameters
  and further meta-information.

- **Marker file (.vmrk)**: A text file describing the events that have
  been collected during the EEG data recording.

- **Raw EEG data file (.eeg)**: A binary file containing the EEG data as
  well as additional signals recorded along with the EEG (e.g. EOG).

BrainVision files can be imported into R, for example, using the
`import_raw()` function from the `eegUtils` package (available on
GitHub):

``` r
# Install eegUtils from GitHub if not already installed
# install.packages("remotes")
# remotes::install_github("craddm/eegUtils")
library(eegUtils)

# Import data from a BrainVision .vhdr file
FILE_VHDR <- "name_of_the_eeg_data_file.vhdr"
PATH <- "the complete path to the file"
data_eeg <- eegUtils::import_raw(FILE_VHDR, file_path = PATH)
```

For more details on the `eegUtils` package, visit the official
documentation: <https://craddm.github.io/eegUtils/>.

**HDF5 file format**

HD-EEG data exported from MATLAB are often stored in the HDF5
(Hierarchical Data Format version 5) file format. It is a binary,
platform-independent format designed for efficient storage and access to
large and complex datasets. Data are organized in a hierarchical
structure, similar to a file system. In the R environment, the `rhdf5`
and `hdf5r` packages are available for working with HDF5 files.

In the following, we present an example using the `H5Fopen()` function
from the `rhdf5` package.

``` r
# Install rhdf5 if not already installed
# install.packages("BiocManager")
# BiocManager::install("rhdf5")
library("rhdf5")

# Import data from a HDF5 file
PATH <- "the complete path to the file"
data_eeg <- rhdf5::H5Fopen(PATH)
```

### Step 3 – Convert data to a long-format table

For the purposes of the `diegr` package, the data must be available in
so-called *long format*, where each row represents a single
observation.  
Imported EEG data are often stored in *wide format* and distributed
across multiple tables; moreover, individual subjects are typically
imported separately. It is therefore necessary to convert the data to
long format. The exact procedure depends on the specific structure of
the data, but functions such as `pivot_longer()` and `add_column()` can
be particularly useful in this process.

A more detailed discussion of long and wide data formats can be found in
the *Tidy Data* chapter of Hadley Wickham’s *R for Data Science*:
<https://r4ds.hadley.nz/data-tidy.html>.

Note: When working with raw (unprocessed) EEG data, preprocessing needs
to be carried out using other software or R packages (such as
`eegUtils`), since `diegr` assumes that the input data have already been
preprocessed.

**Example for HDF5 files**

Assume that, using the procedure described in Step 2, we have imported
an HDF5 file containing EEG data. The stimulus-locked signal is stored
in a component denoted as `dataT`, which is a three-dimensional array
with dimensions *number of sensors × number of time points × number of
epochs*. Sensor labels are stored in `data$hdr$label`.

Under these assumptions, the procedure for converting the
stimulus-locked data for one epoch into a long-format table may proceed
as follows:

``` r
signal_array <- data$dataT # array with signal values
n_epoch <- dim(signal_array)[3] # number of epochs
n_time <- dim(signal_array)[2] # number of time points
n_sensor <- dim(signal_array)[1] # number of sensors
sensornames <- data$hdr$label # sensor labels

# create long-format table for epoch 01
df_stimul <- as.data.frame(t(signal_array[, , 1]))
colnames(df_stimul) <- sensornames
data_epoch_01 <- tidyr::pivot_longer(
  data = df_stimul,
  cols = everything(),
  values_to = "signal",
  names_to = "sensor"
)
n1 <- nrow(data_epoch_01)

# add epoch, subject and time numbers/IDs to the table
data_epoch_01 <- data_epoch_01 |>
  tibble::add_column(
    subject = rep("subject_id", n1),
    epoch = rep(1, n1),
    time = rep(1:n_time, each = n_sensor)
  )
```

Note: The example above follows the *subject–sensor–epoch–time–signal*
structure. However, the code can be readily adapted to accommodate
additional variables, such as different groups or experimental
conditions, if they are present in the data.

### Step 4 – Writing data into a database table

When creating a database table, two main approaches can be used:

1.  The first option is to convert the entire dataset into a long-format
    table in R and then write it to the database using
    [`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html).

2.  Alternatively, the database table can be built incrementally. For
    epoched data, first epoch can be converted to long format separately
    and written to the database by
    [`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html)
    as an initial table. The remaining epochs are then converted to long
    format one by one in a `for` loop, and appended to the existing
    table using argument `append = TRUE` in
    [`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html)
    function, as shown in the example below.

The second approach can be advantageous for HD-EEG data, as it avoids
holding a single, very large long-format `data.frame` in memory.

``` r
# Create database table with the first epoch
DBI::dbWriteTable(con, "table_name", data_epoch_01)

# Add other epochs to the table using for cycle
# n_epoch denotes the number of epochs
for (i in 2:n_epoch) {
  data_epoch <- ... # create long-format table as in step 3
  DBI::dbWriteTable(con, "table_name", data_epoch, append = TRUE)
}
```

## Using the database with diegr

Most functions in the package are programmed to work directly with input
in the form of a database table. Thanks to the use of `dplyr`, the usage
is the same as for `data.frame` inputs and is demonstrated in the
following example.

Note: The SQLite database in the example is created in a temporary
directory and exists only for the duration of the R session. In real
applications, the database would typically be stored on disk or on a
database server.

``` r
data(epochdata, package = "diegr")
# create connection to the database in a temporary directory
con <- DBI::dbConnect(
  RSQLite::SQLite(),
  tempfile(fileext = ".sqlite")
)
# create a database table from epochdata
DBI::dbWriteTable(con, "epochdb", epochdata)
# read the table
epochdb_table <- dplyr::tbl(con, "epochdb")
head(epochdb_table)
#> Source:   SQL [?? x 5]
#> Database: sqlite 3.51.1
#>   time signal epoch sensor subject
#>  <int>  <dbl> <chr> <chr>  <chr>  
#> 1     1  13.1  1     E1     1      
#> 2     1  16.5  1     E2     1 
```

From the output of [`head()`](https://rdrr.io/r/utils/head.html), it is
clear that the `epoch` and `subject` columns are of type character.
These columns therefore need to be handled as character variables or
converted to an appropriate type (numeric/integer) before being used in
`diegr` or `dplyr` functions.

``` r
# convert the type of the columns
epochdb_table <- epochdb_table |>
  dplyr::mutate(
    subject = as.integer(subject),
    epoch = as.integer(epoch)
  )

# pick data directly from the database table
pick_data(epochdb_table,
  subject_rg = 1,
  sensor_rg = c("E1", "E65"),
  epoch_rg = 1:2,
  time_rg = 10
)
#>  Source:   SQL [?? x 5]
#>  Database: sqlite 3.51.1 
#>    time signal epoch sensor subject
#>   <int>  <dbl> <int> <chr>    <int>
#> 1    10  19.6      1 E1           1
#> 2    10  -2.66     1 E65          1
#> 3    10   1.99     2 E1           1
#> 4    10   4.90     2 E65          1
```

``` r
library(plotly)
# plot interactive epoch-boxplot for subject 1 and electrode E65 directly from the database
pick_data(epochdb_table,
  subject_rg = 1,
  sensor_rg = "E65"
) |>
  boxplot_epoch(amplitude = "signal",
                time_lim = 10:20) |>
  layout( # rewrite yaxis title due to html render
    yaxis = list(title = "μV") 
  )
```

``` r
# close the database connection (at the end of work)
DBI::dbDisconnect(con)
```

#### References

Wickham H., Cetinkaya-Rundel M., Grolemund G. *R for Data Science:
Import, Tidy, Transform, Visualize, and Model Data (2nd Edition).*
O’Reilly Media; 2023. Available online at <https://r4ds.hadley.nz>.
