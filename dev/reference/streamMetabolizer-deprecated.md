# Deprecated functions in streamMetabolizer

These functions are provided for compatibility with older versions of
streamMetabolizer only, and may be defunct as soon as the next release.

## Details

- [`calc_DO_deficit()`](https://connorb.github.io/streamMetabolizer/dev/reference/calc_DO_deficit.md)
  is superseded by subtracting `DO.obs` from the output of
  [`calc_DO_sat()`](https://connorb.github.io/streamMetabolizer/dev/reference/calc_DO_sat.md).

- [`calc_DO_at_sat()`](https://connorb.github.io/streamMetabolizer/dev/reference/calc_DO_at_sat.md)
  is superseded by
  [`calc_DO_sat()`](https://connorb.github.io/streamMetabolizer/dev/reference/calc_DO_sat.md).

- [`calc_is_daytime()`](https://connorb.github.io/streamMetabolizer/dev/reference/calc_is_daytime.md)
  is deprecated.

- [`calc_sun_rise_set()`](https://connorb.github.io/streamMetabolizer/dev/reference/calc_sun_rise_set.md)
  is deprecated.

These functions are defunct and will error when called:

- [`lookup_google_timezone()`](https://connorb.github.io/streamMetabolizer/dev/reference/lookup_google_timezone.md)
  is superseded by
  [`lookup_timezone()`](https://connorb.github.io/streamMetabolizer/dev/reference/lookup_timezone.md).
