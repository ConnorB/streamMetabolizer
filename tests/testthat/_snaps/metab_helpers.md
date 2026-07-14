# mm_data works

    Code
      mm_data(solar.time, DO.obs, optional = "DO.sat")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "all", "none", "solar.time", "DO.obs"

---

    Code
      mm_data(solar.time, DO.obs, optional = c("DO.obs", "all"))
    Condition
      Error in `mm_data()`:
      ! `optional` must have length 1 when it contains "all" or "none".

# mm_validate_data works

    Code
      mm_validate_data(NULL, NULL, "metab_mle")
    Condition
      Error:
      ! `data` is required and cannot be "NULL".

---

    Code
      mm_validate_data(data.frame(), NULL, "metab_mle")
    Condition
      Error:
      ! `data` is missing column: `solar.time`, `DO.obs`, `DO.sat`, `depth`, `temp.water`, and `light`.

---

    Code
      mm_validate_data(ok_data[names(ok_data) != "temp.water"], NULL, "metab_mle")
    Condition
      Error:
      ! `data` is missing column: `temp.water`.

---

    Code
      mm_validate_data(dplyr::mutate(ok_data, temp.air = 9), mm_data("temp.air"),
      "metab_mle")
    Condition
      Error:
      ! `data` must omit extra column: `temp.air`.

---

    Code
      mm_validate_data(ok_data, data.frame(), "metab_mle")
    Condition
      Error:
      ! Found 0 possible timestamp columns in `data_daily`; expected exactly one.

---

    Code
      mm_validate_data(ok_data, dplyr::mutate(ok_data_daily, temp.air = 9),
      "metab_mle")
    Condition
      Error:
      ! `data_daily` must omit extra column: `temp.air`.

# mm_filter_valid_days works

    Code
      mm_filter_valid_days(french, day_start = 10, day_end = 12)
    Condition
      Error in `mm_model_by_ply()`:
      ! Timestamps in `data` must increase.
      x Minimum timestep is 0 days from 2012-09-20 12:05:58.158 (row 109) to 2012-09-20 12:05:58.158 (row 110).
