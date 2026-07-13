# Evaluate whether the data argument is properly formatted.

Will most often be called from within a metab_model constructor.

## Usage

``` r
mm_validate_data(
  data = mm_data(NULL),
  data_daily = mm_data(NULL),
  metab_class,
  data_tests = c("missing_cols", "extra_cols", "na_times", "units")
)
```

## Arguments

- data:

  data.frame (not a tbl_df) of input data at the temporal resolution of
  raw observations (unit-value). Columns must have the same names,
  units, and format as the default. The solar.time column must also have
  a timezone code ('tzone' attribute) of 'UTC'. See the **'Formatting
  `data`'** section below for a full description.

- data_daily:

  data.frame containing inputs with a daily timestep. See the
  **'Formatting `data_daily`'** section below for a full description.

- metab_class:

  character the class name of the metab_model constructor

- data_tests:

  list of tests to conduct to determine whether the input data.frames
  are properly formatted to allow modeling to begin

## Examples

``` r
if (FALSE) { # \dontrun{
mm_validate_data(dplyr::select(mm_data(),-temp.water), metab_class="metab_mle")
} # }
```
