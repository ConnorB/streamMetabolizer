# Determine the local time zone from the coordinates

Uses the `lutz` package to determine the local timezone name, standard
offset, and DST offset of a site from its coordinates.

## Usage

``` r
lookup_timezone(latitude, longitude)
```

## Arguments

- latitude:

  Degrees latitude (positive for north) of the location to look up.

- longitude:

  Degrees longitude (positive for east) of the location to look up.

## Value

A list containing the IANA time zone name (`tz`), daylight-saving offset
(`dst_offset`), standard UTC offset (`std_offset`), and retry count.

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
