# Convert a a day of year (1-366) with decimal hours to a date

Convert a a day of year (1-366) with decimal hours to a date

## Usage

``` r
convert_doyhr_to_date(
  doyhr,
  year,
  tz = "UTC",
  origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"),
  ...
)
```

## Arguments

- doyhr:

  Numeric value expressing the date as the number of days, with decimal
  hours, since 00:00 of December 31 of the preceding year

- year:

  Numeric 4-digit year

- tz:

  The time zone to pass to as.POSIXct()

- origin:

  The origin to pass to as.POSIXct()

- ...:

  Other arguments to pass to as.POSIXct()

## Value

A datetime object as POSIXct

## Examples

``` r
streamMetabolizer:::convert_doyhr_to_date(34.500695, 2015)
#> [1] "2015-02-03 12:01:00 UTC"
```
