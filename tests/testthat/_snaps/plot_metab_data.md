# plot_metab_data validates its inputs

    Code
      plot_metab_data(1)
    Condition
      Error in `plot_metab_data()`:
      ! `data` must be a data frame.

---

    Code
      plot_metab_data(data, cols = 1)
    Condition
      Error in `plot_metab_data()`:
      ! `cols` must be a character vector without missing values.

---

    Code
      plot_metab_data(data, cols = character())
    Condition
      Error in `plot_metab_data()`:
      ! `cols` must name at least one column.

---

    Code
      plot_metab_data(data, cols = "unknown")
    Condition
      Error in `plot_metab_data()`:
      ! `cols` contains unknown columns: unknown.
      i Valid columns are: DO.obs, DO.sat, depth, temp.water, light.

---

    Code
      plot_metab_data(data, cols = "DO.sat")
    Condition
      Error in `plot_metab_data()`:
      ! `data` is missing required columns: DO.sat.

---

    Code
      plot_metab_data(data, cols = "DO.obs")
    Condition
      Error in `plot_metab_data()`:
      ! `data` measurement columns must be numeric: DO.obs.
