# Return the average timestep in days

Return the average timestep in days

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

  a vector of date-times in POSIXct format from which to compute the
  average timestep

- format:

  the format in which to return the timestep. 'mean' always returns one
  value; 'unique' may return more than one depending on the variation in
  timesteps and the value of `digits`.

- require_unique:

  logical. should it be required that there is exactly one unique
  timestep (within the given tolerance `tol`)?

- tol:

  if `format == 'unique'`, unique values are first calculated to machine
  precision, but then subsetted to those that differ from one another by
  at least tol, where tol is a time difference in units of days (and
  thus 1/(24*60*60) is one second).

## Examples

``` r
{
datetimes <- Sys.time()+ as.difftime(c(0,304,600,900.2,1200,1500,1800), units='secs')
mm_get_timestep(datetimes, 'unique', tol=1/(24*60*60))
mm_get_timestep(datetimes, 'unique', tol=5/(24*60*60))
mm_get_timestep(datetimes, 'unique', tol=10/(24*60*60))
mm_get_timestep(datetimes, 'unique', tol=300/(24*60*60))
mm_get_timestep(datetimes, 'mean')
mm_get_timestep(datetimes, 'mean', require_unique=TRUE, tol=300/(24*60*60))
datetimes <- Sys.time()+ as.difftime(c(-1,0,2,4,5,6,8,10), units='days')
mm_get_timestep(datetimes, 'modal')
mm_get_timestep(c(), 'mean')
mm_get_timestep(c(), 'unique')
mm_get_timestep(c(), 'modal')
if (FALSE) { # \dontrun{
# all of these should and do give errors:
mm_get_timestep(datetimes, 'mean', require_unique=TRUE, tol=1/(24*60*60))
mm_get_timestep(datetimes, 'unique', tol=5/(24*60*60), require_unique=TRUE)
mm_get_timestep(c(), 'mean', require_unique=TRUE)
mm_get_timestep(c(), 'unique', require_unique=TRUE)
mm_get_timestep(c(), 'modal', require_unique=TRUE)
} # }
}
```
