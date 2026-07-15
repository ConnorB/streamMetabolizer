# Calculate timesteps in days

Calculate timesteps in days

## Usage

``` r
mm_get_timestep(
  datetimes,
  format = c("mean", "unique", "modal"),
  require_unique = FALSE,
  tol = 60/(24 * 60 * 60)
)
```

## Arguments

- datetimes:

  A `POSIXct` vector from which to calculate timesteps.

- format:

  A string specifying the output format. `"mean"` always returns one
  value; `"unique"` may return more than one, depending on the variation
  in timesteps and the value of `tol`.

- require_unique:

  A logical. Should exactly one unique timestep (within the tolerance
  `tol`) be required?

- tol:

  If `format = "unique"`, the minimum difference in days for two
  timesteps to be considered distinct. For example, `1 / (24 * 60 * 60)`
  is one second.

## Value

A numeric vector of timesteps in days. Its length depends on `format`.

## Examples

``` r
datetimes <- Sys.time() +
  as.difftime(c(0, 304, 600, 900.2, 1200, 1500, 1800), units = "secs")
mm_get_timestep(datetimes, "unique", tol = 1 / (24 * 60 * 60))
#> [1] 0.003425926 0.003469907 0.003518519
mm_get_timestep(datetimes, "unique", tol = 5 / (24 * 60 * 60))
#> [1] 0.003425926 0.003518519
mm_get_timestep(datetimes, "mean")
#> [1] 0.003472222
mm_get_timestep(
  datetimes,
  "mean",
  require_unique = TRUE,
  tol = 300 / (24 * 60 * 60)
)
#> [1] 0.003425926

datetimes <- Sys.time() +
  as.difftime(c(-1, 0, 2, 4, 5, 6, 8, 10), units = "days")
mm_get_timestep(datetimes, "modal")
#> [1] 2
mm_get_timestep(c(), "mean")
#> [1] NA

try(mm_get_timestep(datetimes, "mean", require_unique = TRUE))
#> Error in mm_get_timestep(datetimes, format = "unique", require_unique = TRUE,  : 
#>   Found 2 unique timesteps; expected exactly one.
try(mm_get_timestep(c(), "unique", require_unique = TRUE))
#> Error in mm_get_timestep(c(), "unique", require_unique = TRUE) : 
#>   Could not determine exactly one unique timestep.
```
