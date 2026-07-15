# Use USGS API (USGS Elevation Point Query Service) to determine approximate local elevation

This is meant to supply an APPROXIMATE elevation, with no guarantees on
precision or on the lifetime of the API service used by the function.
The lookup uses the JSON response from the service.

## Usage

``` r
lookup_usgs_elevation(
  latitude,
  longitude,
  units = "m",
  timeout = 30,
  max_tries = 3
)
```

## Arguments

- latitude:

  Degrees latitude (positive for north) of the location to look up.

- longitude:

  Degrees longitude (positive for east) of the location to look up.

- units:

  A single string specifying the elevation units. Accepts `"m"`,
  `"meters"`, `"ft"`, or `"feet"`, case-insensitively.

- timeout:

  A single positive number giving the request timeout in seconds.

- max_tries:

  A single positive integer giving the maximum number of request
  attempts.

## Value

The numeric elevation in the requested units.

## References

https://epqs.nationalmap.gov/v1/docs

## Examples

``` r
if (FALSE) { # interactive()
elevation_m <- lookup_usgs_elevation(
  latitude = 39.102075,
  longitude = -96.594689
)
elevation_m

elevation_ft <- lookup_usgs_elevation(
  latitude = 39.102075,
  longitude = -96.594689,
  units = "ft"
)
elevation_ft
}
```
