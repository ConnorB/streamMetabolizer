# Use USGS API (USGS Elevation Point Query Service) to determine approximate local elevation

This is meant to supply an APPROXIMATE elevation, with no guarantees on
precision or on the lifetime of the API service used by the function.
The lookup uses the JSON response from the service.

## Usage

``` r
lookup_usgs_elevation(latitude, longitude, units = c("Meters", "Feet"))
```

## Arguments

- latitude:

  degrees latitude (positive for north) of the location to look up.

- longitude:

  degrees longitude (positive for east) of the location to look up.

- units:

  character, one of Meters or Feet, specifying the units in which to
  return the elevation

## Value

The numeric elevation in the requested units.

## References

https://epqs.nationalmap.gov/v1/docs

## Examples

``` r
if (FALSE) { # \dontrun{
elevation_m <- lookup_usgs_elevation(
  latitude = 39.102075,
  longitude = -96.594689
)
elevation_m

elevation_ft <- lookup_usgs_elevation(
  latitude = 39.102075,
  longitude = -96.594689,
  units = "Feet"
)
elevation_ft
} # }
```
