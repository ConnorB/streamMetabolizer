# Convert a date to a day of year (1-366) with decimal hours

Inspired by / copied from LakeMetabolizer date2doy

## Usage

``` r
convert_date_to_doyhr(date)
```

## Arguments

- date:

  A datetime object as POSIXct or POSIXt

## Value

Numeric value expressing the date as the number of days, with decimal
hours, since 00:00 of December 31 of the preceding year (i.e., January
1st at 00:01 is ~1.01)

## Examples

``` r
streamMetabolizer:::convert_date_to_doyhr(as.POSIXct("2015-02-03 12:01:00 UTC"))
#> [1] 34.50069
```
