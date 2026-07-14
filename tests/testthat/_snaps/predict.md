# predict_metab works as expected for bad inputs

    Code
      predict_metab(mm, day_start = 20, day_end = 28, use_saved = FALSE)
    Message
      Daily metabolism predictions cover hours 20 to 28 on each date.
      i The model-fitting range was 2 to 28 hours.
    Condition
      Error in `predict_metab()`:
      ! `day_end` - `day_start` must equal 24 hours except for <metab_night> models.

---

    Code
      predict_metab(mm, day_start = 2, day_end = 28, use_saved = FALSE)
    Condition
      Error in `predict_metab()`:
      ! `day_end` - `day_start` cannot exceed 24 hours for metabolism prediction.

---

    Code
      predict_metab(mm, day_start = 7, day_end = 36, use_saved = FALSE)
    Condition
      Error in `predict_metab()`:
      ! `day_end` - `day_start` cannot exceed 24 hours for metabolism prediction.

---

    Code
      mp <- predict_metab(mm, day_start = 7, day_end = 36)
    Condition
      Warning:
      Using saved daily metabolism values; new `day_start` or `day_end` values are ignored.
