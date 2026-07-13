# Determine the local time zone from the coordinates

Uses the `lutz` package to determine the local timezone name, standard
offset, and DST offset of a site from its coordinates.

## Usage

``` r
lookup_timezone(latitude, longitude)
```

## Arguments

- latitude:

  degrees latitude (positive for north) of the location to look up.

- longitude:

  degrees longitude (positive for east) of the location to look up.

## Examples

``` r
lookup_timezone(41.33, -106.3)
#> $tz
#> [1] "America/Denver"
#> 
#> $dst_offset
#> [1] 0
#> 
#> $std_offset
#> [1] -7
#> 
#> $retry
#> [1] 0
#> 
```
