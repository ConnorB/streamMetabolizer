# mm_model_by_ply creates intuitive ply_dates from day_start and day_end

    Code
      mm_model_by_ply(mm_model_by_ply_prototype, data = dat, day_start = 25, day_end = 30)
    Condition
      Error in `mm_model_by_ply()`:
      ! `day_start` must be between -24 and 24.

---

    Code
      mm_model_by_ply(mm_model_by_ply_prototype, data = dat, day_start = -12,
        day_end = -2)
    Condition
      Error in `mm_model_by_ply()`:
      ! `day_end` must be between 0 and 48.

---

    Code
      mm_model_by_ply(mm_model_by_ply_prototype, data = dat, day_start = -22,
        day_end = 28)
    Condition
      Error in `mm_model_by_ply()`:
      ! `day_end` - `day_start` cannot exceed 48 hours.

---

    Code
      mm_model_by_ply(mm_model_by_ply_prototype, data = dat, day_start = -26,
        day_end = 4)
    Condition
      Error in `mm_model_by_ply()`:
      ! `day_start` must be between -24 and 24.

---

    Code
      mm_model_by_ply(mm_model_by_ply_prototype, data = dat, day_start = 22, day_end = 49)
    Condition
      Error in `mm_model_by_ply()`:
      ! `day_end` must be between 0 and 48.
