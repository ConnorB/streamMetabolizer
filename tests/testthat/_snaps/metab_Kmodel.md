# metab_Kmodel predictions (predict_metab, predict_DO) make sense

    Code
      lm(log(K600.daily.obs) ~ log(discharge.daily), data = data.frame(date = as.Date(
        "2012-08-24"), K600.daily.obs = 20, discharge.daily = NA, weights = 1))
    Condition
      Error in `lm.fit()`:
      ! 0 (non-NA) cases

---

    Code
      loess(log(K600.daily.obs) ~ as.numeric(date) + log(discharge.daily), data = data.frame(
        date = as.Date("2012-08-24"), K600.daily.obs = 20, discharge.daily = NA,
        weights = 1))
    Condition
      Error in `simpleLoess()`:
      ! invalid 'x'

---

    Code
      suppressMessages(metab_Kmodel(data = dat, data_daily = ddat1, specs = specs(
        mm_name("Kmodel", engine = "lm"))))
    Condition
      Error in `lm.wfit()`:
      ! 0 (non-NA) cases

---

    Code
      suppressMessages(metab_Kmodel(data = dat, data_daily = ddat1, specs = specs(
        mm_name("Kmodel", engine = "loess"))))
    Condition
      Error in `simpleLoess()`:
      ! invalid 'x'

---

    Code
      predict_metab(mm)
    Condition
      Error in `predict_metab()`:
      ! <metab_Kmodel> objects cannot predict metabolism.
      i Use `get_params()` to retrieve predicted `K600.daily` values.

---

    Code
      predict_DO(mm)
    Condition
      Error in `predict_DO()`:
      ! <metab_Kmodel> objects cannot predict dissolved oxygen.
      i Use `get_params()` to retrieve predicted K values.
