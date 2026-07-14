# CLI helpers support semantic messages

    Code
      .cli_abort(c("Invalid input", x = "{.arg {input}} must be numeric", i = "Found {n} invalid value{?s}"),
      call = NULL)
    Condition
      Error:
      ! Invalid input
      x `temperature` must be numeric
      i Found 2 invalid values

---

    Code
      .cli_warn("Column {.var {input}} contains missing values")
    Condition
      Warning:
      Column `temperature` contains missing values

---

    Code
      .cli_inform(c(v = "Processed {n} row{?s}"))
    Message
      v Processed 2 rows
