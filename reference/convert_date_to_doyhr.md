# Convert a date to a day of year (1-366) with decimal hours

Inspired by / copied from LakeMetabolizer date2doy

## Usage

``` r
convert_date_to_doyhr(date)
```

## Arguments

- date:

  A datetime object as POSIXct or POSIXt.

## Value

A number expressing the date as days, including fractional days, since
00:00 on December 31 of the preceding year (that is, January 1 at 00:01
is approximately 1.01).

## Examples

``` r
streamMetabolizer:::convert_date_to_doyhr(as.POSIXct("2015-02-03 12:01:00 UTC"))
#> [1] 34.50069
```
