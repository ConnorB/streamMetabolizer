# converting between UTC and solar time works

    Code
      convert_UTC_to_solartime(adate, longitude = 0, time.type = "not a type")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "apparent solar", "mean solar"

---

    Code
      convert_solartime_to_UTC(adate, longitude = 0, time.type = "not a type")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "apparent solar", "mean solar"

# converting between UTC and local time works

    Code
      convert_UTC_to_localtime(adate, latitude = 51.48, longitude = 0, time.type = "not a type")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "standard local", "daylight local"

# common use-case conversions (calc_solar_time) works

    Code
      calc_solar_time(lubridate::with_tz(adate, "UTC"), -120)
    Condition
      Warning:
      The longitude does not appear to use the UTC time zone.
      i Check that `local.time` contains local clock time.
    Output
      [1] "2014-02-01 00:01:19 UTC"
