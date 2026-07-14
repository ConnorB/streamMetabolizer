# metab_inputs uses cli for text guidance

    Code
      metab_inputs("night", "specs")
    Message
      i Use `specs(mm_name('night'))`.
      * See `mm_name()` (`?streamMetabolizer::mm_name()`) and `specs()` (`?streamMetabolizer::specs()`) for more options.

---

    Code
      metab_inputs("night", "data_daily")
    Message
      i "night" models do not use `data_daily`.
      * Set `data_daily` to `NULL`.

---

    Code
      metab_inputs("mle", "info")
    Message
      i `info` is optional metadata stored in the returned `metab_model()` (`?streamMetabolizer::metab_model()`).
      * Use `NULL` (the default) or any R object, then retrieve it with `get_info()` (`?streamMetabolizer::get_info()`).
