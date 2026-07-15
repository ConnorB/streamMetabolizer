# Fit a metabolism model to data

Runs the metabolism model specified by the `specs` argument. Returns a
fitted model.

## Usage

``` r
metab(
  specs = specs(mm_name()),
  data = mm_data(NULL),
  data_daily = mm_data(NULL),
  info = NULL
)
```

## Arguments

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

- data:

  A data frame or tibble of input data at the temporal resolution of raw
  observations (unit-value). Columns must have the same names, units,
  and format as the default. The solar.time column must also have a
  timezone code ('tzone' attribute) of 'UTC'. See the **'Formatting
  `data`'** section below for a full description.

- data_daily:

  A data frame or tibble containing inputs with a daily timestep. See
  the **'Formatting `data_daily`'** section below for a full
  description.

- info:

  any information, in any format, that you would like to store within
  the metab_model object

## Value

An object inheriting from metab_model and containing the fitted model.
This object can be inspected with the functions in the
[`metab_model_interface()`](https://connorb.github.io/streamMetabolizer/reference/metab_model_interface.md).

## Author

Alison Appling

## Formatting `data`

Unit-value model inputs passed via the `data` argument should be
formatted as a data.frame with column names and values that depend on
the model `type`, as follows. (If all columns are optional, `data` may
equal `NULL`.)

- `mle` or `night`:

  |             |                |                        |          |
  |-------------|----------------|------------------------|----------|
  | **colname** | **class**      | **units**              | **need** |
  | solar.time  | POSIXct,POSIXt |                        | required |
  | DO.obs      | numeric        | \\mgO_2 L^{-1}\\       | required |
  | DO.sat      | numeric        | \\mgO_2 L^{-1}\\       | required |
  | depth       | numeric        | \\m\\                  | required |
  | temp.water  | numeric        | \\°C\\                 | required |
  | light       | numeric        | \\umol m^{-2} s^{-1}\\ | required |
  | discharge   | numeric        | \\m^{3} s^{-1}\\       | optional |

  **Example**:

  |  |  |  |  |  |  |  |
  |----|----|----|----|----|----|----|
  | `solar.time ` | `DO.obs` | `DO.sat` | `depth` | `temp.water` | `light` | `discharge` |
  | `2050-03-14 15:10:00` | `10.1 ` | `14.2 ` | `0.5 ` | `21.8 ` | `300.9` | `9 ` |

- `bayes`:

  |             |                |                        |          |
  |-------------|----------------|------------------------|----------|
  | **colname** | **class**      | **units**              | **need** |
  | solar.time  | POSIXct,POSIXt |                        | required |
  | DO.obs      | numeric        | \\mgO_2 L^{-1}\\       | required |
  | DO.sat      | numeric        | \\mgO_2 L^{-1}\\       | required |
  | depth       | numeric        | \\m\\                  | required |
  | temp.water  | numeric        | \\°C\\                 | required |
  | light       | numeric        | \\umol m^{-2} s^{-1}\\ | required |
  | discharge   | numeric        | \\m^{3} s^{-1}\\       | optional |

  **Example**:

  |  |  |  |  |  |  |  |
  |----|----|----|----|----|----|----|
  | `solar.time ` | `DO.obs` | `DO.sat` | `depth` | `temp.water` | `light` | `discharge` |
  | `2050-03-14 15:10:00` | `10.1 ` | `14.2 ` | `0.5 ` | `21.8 ` | `300.9` | `9 ` |

- `Kmodel`:

  |             |                |                  |          |
  |-------------|----------------|------------------|----------|
  | **colname** | **class**      | **units**        | **need** |
  | solar.time  | POSIXct,POSIXt |                  | optional |
  | discharge   | numeric        | \\m^{3} s^{-1}\\ | optional |
  | velocity    | numeric        | \\m s^{-1}\\     | optional |

  **Example**:

  |                       |             |            |
  |-----------------------|-------------|------------|
  | `solar.time `         | `discharge` | `velocity` |
  | `2050-03-14 15:10:00` | `9 `        | `2 `       |

- `sim`:

  |             |                |                        |          |
  |-------------|----------------|------------------------|----------|
  | **colname** | **class**      | **units**              | **need** |
  | solar.time  | POSIXct,POSIXt |                        | required |
  | DO.obs      | numeric        | \\mgO_2 L^{-1}\\       | optional |
  | DO.sat      | numeric        | \\mgO_2 L^{-1}\\       | required |
  | depth       | numeric        | \\m\\                  | required |
  | temp.water  | numeric        | \\°C\\                 | required |
  | light       | numeric        | \\umol m^{-2} s^{-1}\\ | required |

  **Example**:

  |                       |          |          |         |              |         |
  |-----------------------|----------|----------|---------|--------------|---------|
  | `solar.time `         | `DO.obs` | `DO.sat` | `depth` | `temp.water` | `light` |
  | `2050-03-14 15:10:00` | `10.1 `  | `14.2 `  | `0.5 `  | `21.8 `      | `300.9` |

## Formatting `data_daily`

Daily-value model inputs passed via the `data_daily` argument should be
formatted as a data.frame with column names and values that depend on
the model `type`, as follows. (If all columns are optional, `data_daily`
may equal `NULL`.)

- `night`:

  `NULL`

- `mle`:

  |                 |           |                             |          |
  |-----------------|-----------|-----------------------------|----------|
  | **colname**     | **class** | **units**                   | **need** |
  | date            | Date      |                             | optional |
  | K600.daily      | numeric   | \\d^{-1}\\                  | optional |
  | init.GPP.daily  | numeric   | \\gO_2 m^{-2} d^{-1}\\      | optional |
  | init.Pmax       | numeric   | \\gO_2 m^{-2} d^{-1}\\      | optional |
  | init.alpha      | numeric   | \\gO_2 s d^{-1} umol^{-1}\\ | optional |
  | init.ER.daily   | numeric   | \\gO_2 m^{-2} d^{-1}\\      | optional |
  | init.ER20       | numeric   | \\gO_2 m^{-2} d^{-1}\\      | optional |
  | init.K600.daily | numeric   | \\d^{-1}\\                  | optional |

  **Example**:

  |  |  |  |  |  |  |  |  |
  |----|----|----|----|----|----|----|----|
  | `date ` | `K600.daily` | `init.GPP.daily` | `init.Pmax` | `init.alpha` | `init.ER.daily` | `init.ER20` | `init.K600.daily` |
  | `2050-03-14` | `10 ` | `5 ` | `10 ` | `1e-04 ` | `-10 ` | `-10 ` | `10 ` |

- `bayes`:

  |                 |           |                  |          |
  |-----------------|-----------|------------------|----------|
  | **colname**     | **class** | **units**        | **need** |
  | date            | Date      |                  | optional |
  | discharge.daily | numeric   | \\m^{3} s^{-1}\\ | optional |

  **Example**:

  |              |                   |
  |--------------|-------------------|
  | `date `      | `discharge.daily` |
  | `2050-03-14` | `9 `              |

- `Kmodel`:

  |                  |           |                  |          |
  |------------------|-----------|------------------|----------|
  | **colname**      | **class** | **units**        | **need** |
  | date             | Date      |                  | required |
  | K600.daily       | numeric   | \\d^{-1}\\       | required |
  | K600.daily.lower | numeric   | \\d^{-1}\\       | optional |
  | K600.daily.upper | numeric   | \\d^{-1}\\       | optional |
  | discharge.daily  | numeric   | \\m^{3} s^{-1}\\ | optional |
  | velocity.daily   | numeric   | \\m s^{-1}\\     | optional |

  **Example**:

  |  |  |  |  |  |  |
  |----|----|----|----|----|----|
  | `date ` | `K600.daily` | `K600.daily.lower` | `K600.daily.upper` | `discharge.daily` | `velocity.daily` |
  | `2050-03-14` | `10 ` | `4.5 ` | `15.6 ` | `9 ` | `2 ` |

- `sim`:

  |                 |           |                             |          |
  |-----------------|-----------|-----------------------------|----------|
  | **colname**     | **class** | **units**                   | **need** |
  | date            | Date      |                             | optional |
  | discharge.daily | numeric   | \\m^{3} s^{-1}\\            | optional |
  | DO.mod.1        | numeric   | \\mgO_2 L^{-1}\\            | optional |
  | K600.daily      | numeric   | \\d^{-1}\\                  | optional |
  | GPP.daily       | numeric   | \\gO_2 m^{-2} d^{-1}\\      | optional |
  | Pmax            | numeric   | \\gO_2 m^{-2} d^{-1}\\      | optional |
  | alpha           | numeric   | \\gO_2 s d^{-1} umol^{-1}\\ | optional |
  | ER.daily        | numeric   | \\gO_2 m^{-2} d^{-1}\\      | optional |
  | ER20            | numeric   | \\gO_2 m^{-2} d^{-1}\\      | optional |
  | err.obs.sigma   | numeric   | \\mgO_2 L^{-1}\\            | optional |
  | err.obs.phi     | numeric   |                             | optional |
  | err.proc.sigma  | numeric   | \\gO_2 m^{-2} d^{-1}\\      | optional |
  | err.proc.phi    | numeric   |                             | optional |

  **Example**:

  |  |  |  |  |  |  |  |  |  |  |  |  |  |
  |----|----|----|----|----|----|----|----|----|----|----|----|----|
  | `date ` | `discharge.daily` | `DO.mod.1` | `K600.daily` | `GPP.daily` | `Pmax` | `alpha` | `ER.daily` | `ER20` | `err.obs.sigma` | `err.obs.phi` | `err.proc.sigma` | `err.proc.phi` |
  | `2050-03-14` | `9 ` | `7.5 ` | `10 ` | `5 ` | `10 ` | `1e-04` | `-10 ` | `-10 ` | `0.01 ` | `0 ` | `5 ` | `0 ` |

## Examples

``` r
dat <- data_metab(num_days='3')

# fit a basic MLE model
mm <- metab(specs(mm_name('mle')), data=dat, info='my info')
predict_metab(mm)
#> # A tibble: 3 × 10
#>   date         GPP GPP.lower GPP.upper    ER ER.lower ER.upper msgs.fit warnings
#>   <date>     <dbl>     <dbl>     <dbl> <dbl>    <dbl>    <dbl> <chr>    <chr>   
#> 1 2012-09-18  2.81      2.44      3.19 -2.10    -2.41    -1.80 "      … ""      
#> 2 2012-09-19  3.28      2.87      3.69 -2.47    -2.81    -2.13 "    W … ""      
#> 3 2012-09-20  2.58      2.31      2.85 -1.71    -1.91    -1.50 "      … ""      
#> # ℹ 1 more variable: errors <chr>
get_info(mm)
#> [1] "my info"
get_fitting_time(mm)
#>    user  system elapsed 
#>   0.571   0.000   0.570 

# with chaining & customization
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
mm <- mm_name('mle', ode_method='euler') |>
  specs(init.GPP.daily=40) |>
  metab(data=dat)
predict_metab(mm)
#> # A tibble: 3 × 10
#>   date         GPP GPP.lower GPP.upper    ER ER.lower ER.upper msgs.fit warnings
#>   <date>     <dbl>     <dbl>     <dbl> <dbl>    <dbl>    <dbl> <chr>    <chr>   
#> 1 2012-09-18  2.81      2.40      3.22 -2.10    -2.43    -1.77 "    W … ""      
#> 2 2012-09-19  3.27      2.84      3.70 -2.46    -2.82    -2.11 "    W … ""      
#> 3 2012-09-20  2.57      2.29      2.85 -1.70    -1.91    -1.48 "    W … ""      
#> # ℹ 1 more variable: errors <chr>
if (FALSE) { # \dontrun{
plot_DO_preds(predict_DO(mm))
plot_DO_preds(predict_DO(mm), y_var='pctsat', style='dygraphs')
} # }
```
