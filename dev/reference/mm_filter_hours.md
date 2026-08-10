# Limit to a specific time range on each date

Within each date (as labeled by the 'date' column of `data`, select the
values of solar.time that are within the time range specified by
day_start and day_end). This function only removes rows and cannot add
them; to add overlap starting from a continuous time series, see
[`mm_model_by_ply()`](https://connorb.github.io/streamMetabolizer/dev/reference/mm_model_by_ply.md).

## Usage

``` r
mm_filter_hours(data, day_start, day_end)
```

## Arguments

- data:

  A data.frame containing date and solar.time columns (POSIXct).

- day_start:

  The start time of each day, inclusive, in hours.

- day_end:

  The end time of each day, exclusive, in hours.
