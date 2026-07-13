# Make daily reaeration estimates from input parameters

Called from metab_night().

## Usage

``` r
nightreg_1ply(
  data_ply,
  data_daily_ply,
  day_start,
  day_end,
  ply_date,
  ...,
  night_tests = TRUE,
  specs = specs(mm_name("night"))
)
```

## Arguments

- data_ply:

  a data.frame containing all relevant, validated modeling data for a
  single ply of data. (1 ply ~= 1 date, although the day length has been
  specified by day_start and day_end and may not be exactly 24 hours)

- data_daily_ply:

  NULL or a data.frame containing inputs with a daily timestep.

- day_start:

  start time (inclusive) of a day's data in number of hours from the
  midnight that begins the date. For example, day_start=-1.5 indicates
  that data describing 2006-06-26 begin at 2006-06-25 22:30, or at the
  first observation time that occurs after that time if day_start
  doesn't fall exactly on an observation time.

- day_end:

  end time (exclusive) of a day's data in number of hours from the
  midnight that begins the date. For example, day_end=30 indicates that
  data describing 2006-06-26 end at the last observation time that
  occurs before 2006-06-27 06:00.

- ply_date:

  the modal date of this ply of data and data_daily, and the date by
  which this ply should be referred topresent.

- ...:

  other args that were passed untouched from the function calling
  mm_model_by_ply, through mm_model_by_ply, and finally to this
  function.

- night_tests:

  character vector of validity tests to conduct on the data after
  subsetting to just nighttime

- specs:

  a list of model specifications and parameters for a model. Although
  this may be specified manually (it's just a list), it is easier and
  safer to use
  [`specs()`](https://connorb.github.io/streamMetabolizer/reference/specs.md)
  to generate the list, because the set of required parameters and their
  defaults depends on the model given in the `model_name` argument to
  `specs`. The help file for
  [`specs()`](https://connorb.github.io/streamMetabolizer/reference/specs.md)
  lists the necessary parameters, describes them in detail, and gives
  default values.

## Value

data.frame of estimates and
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) model diagnostics

## References

Hornberger, George M., and Mahlon G. Kelly. Atmospheric Reaeration in a
River Using Productivity Analysis. Journal of the Environmental
Engineering Division 101, no. 5 (October 1975): 729-39.

Raymond, Peter A., Christopher J. Zappa, David Butman, Thomas L. Bott,
Jody Potter, Patrick Mulholland, Andrew E. Laursen, William H. McDowell,
and Denis Newbold. Scaling the gas transfer velocity and hydraulic
geometry in streams and small rivers. Limnology & Oceanography: Fluids &
Environments 2 (2012); 41:53.
