# Deprecated Functions in package streamMetabolizer

These functions are provided for compatibility with older versions of
`streamMetabolizer` only, and may be defunct as soon as the next
release.

## Details

- [`calc_DO_deficit()`](https://connorb.github.io/streamMetabolizer/reference/calc_DO_deficit.md) -
  instead, subtract `DO.obs` from output of
  [`calc_DO_sat()`](https://connorb.github.io/streamMetabolizer/reference/calc_DO_sat.md)

- `calc_DO_at_sat` - use
  [`calc_DO_sat()`](https://connorb.github.io/streamMetabolizer/reference/calc_DO_sat.md)
  instead

- [`calc_is_daytime()`](https://connorb.github.io/streamMetabolizer/reference/calc_is_daytime.md) -
  if you like and want this function, submit a GitHub issue to keep it

- [`calc_sun_rise_set()`](https://connorb.github.io/streamMetabolizer/reference/calc_sun_rise_set.md) -
  if you like and want this function, submit a GitHub issue to keep it

These functions are defunct and will error when called:

- `lookup_google_timezone` - use
  [`lookup_timezone()`](https://connorb.github.io/streamMetabolizer/reference/lookup_timezone.md)
  instead
