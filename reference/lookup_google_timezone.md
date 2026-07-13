# Use Google API to determine local time zone

**\[deprecated\]**

This function has been replaced by
[`lookup_timezone()`](https://connorb.github.io/streamMetabolizer/reference/lookup_timezone.md),
which uses the `lutz` package for offline timezone lookup instead of the
Google API.

## Usage

``` r
lookup_google_timezone(latitude, longitude, timestamp = NULL)
```

## Arguments

- latitude:

  degrees latitude (positive for north) of the location to look up.

- longitude:

  degrees longitude (positive for east) of the location to look up.

- timestamp:

  Ignored. Kept for backward compatibility.
