# mm_name can generate names

    Code
      mm_name("bayes", err_proc_acor_light = TRUE)
    Condition
      Error in `mm_name()`:
      ! `err_proc_acor_light` requires `err_proc_acor` = `TRUE`.

---

    Code
      mm_name("b", pool_K600 = "none", err_proc_acor = TRUE, engine = "nlm")
    Condition
      Error in `mm_name()`:
      ! Model type "bayes" is incompatible with engine "nlm".

---

    Code
      mm_name("m", err_proc_iid = TRUE)
    Condition
      Error in `mm_validate_name()`:
      ! `model_name` is not valid for type "mle".
      x Received "m_np_oipi_tr_plrckm.nlm".
      i See `mm_valid_names()` (`?streamMetabolizer::mm_valid_names()`) for valid names.

---

    Code
      mm_name("s", err_proc_iid = FALSE)
    Condition
      Error in `mm_validate_name()`:
      ! `model_name` is not valid for type "sim".
      x Received "s_np_oipc_tr_plrckm.rnorm".
      i See `mm_valid_names()` (`?streamMetabolizer::mm_valid_names()`) for valid names.

---

    Code
      mm_name("n", ode_method = "trapezoid")
    Condition
      Error in `mm_validate_name()`:
      ! `model_name` is not valid for type "night".
      x Received "n_np_pi_tr_rckf.lm".
      i See `mm_valid_names()` (`?streamMetabolizer::mm_valid_names()`) for valid names.
