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

## Formatting `data`

Unit-value model inputs passed via the `data` argument should be
formatted as a data.frame with column names and values that depend on
the model `type`, as follows. (If all columns are optional, `data` may
equal `NULL`.)

- `mle` or `night`:

  |             |                |                |          |
  |-------------|----------------|----------------|----------|
  | **colname** | **class**      | **units**      | **need** |
  | solar.time  | POSIXct,POSIXt |                | required |
  | DO.obs      | numeric        | mgO2 L^-1      | required |
  | DO.sat      | numeric        | mgO2 L^-1      | required |
  | depth       | numeric        | m              | required |
  | temp.water  | numeric        | degC           | required |
  | light       | numeric        | umol m^-2 s^-1 | required |
  | discharge   | numeric        | m^3 s^-1       | optional |

      \strong{Example}:
      \tabular{lrrrrrr}{
        \code{solar.time         } \tab \code{DO.obs} \tab \code{DO.sat} \tab \code{depth} \tab \code{temp.water} \tab \code{light} \tab \code{discharge}\cr
        \code{2050-03-14 15:10:00} \tab \code{10.1  } \tab \code{14.2  } \tab \code{0.5  } \tab \code{21.8      } \tab \code{300.9} \tab \code{9        }
      }

- `bayes`:

  |             |                |                |          |
  |-------------|----------------|----------------|----------|
  | **colname** | **class**      | **units**      | **need** |
  | solar.time  | POSIXct,POSIXt |                | required |
  | DO.obs      | numeric        | mgO2 L^-1      | required |
  | DO.sat      | numeric        | mgO2 L^-1      | required |
  | depth       | numeric        | m              | required |
  | temp.water  | numeric        | degC           | required |
  | light       | numeric        | umol m^-2 s^-1 | required |
  | discharge   | numeric        | m^3 s^-1       | optional |

      \strong{Example}:
      \tabular{lrrrrrr}{
        \code{solar.time         } \tab \code{DO.obs} \tab \code{DO.sat} \tab \code{depth} \tab \code{temp.water} \tab \code{light} \tab \code{discharge}\cr
        \code{2050-03-14 15:10:00} \tab \code{10.1  } \tab \code{14.2  } \tab \code{0.5  } \tab \code{21.8      } \tab \code{300.9} \tab \code{9        }
      }

- `Kmodel`:

  |             |                |           |          |
  |-------------|----------------|-----------|----------|
  | **colname** | **class**      | **units** | **need** |
  | solar.time  | POSIXct,POSIXt |           | optional |
  | discharge   | numeric        | m^3 s^-1  | optional |
  | velocity    | numeric        | m s^-1    | optional |

      \strong{Example}:
      \tabular{lrr}{
        \code{solar.time         } \tab \code{discharge} \tab \code{velocity}\cr
        \code{2050-03-14 15:10:00} \tab \code{9        } \tab \code{2       }
      }

- `sim`:

  |             |                |                |          |
  |-------------|----------------|----------------|----------|
  | **colname** | **class**      | **units**      | **need** |
  | solar.time  | POSIXct,POSIXt |                | required |
  | DO.obs      | numeric        | mgO2 L^-1      | optional |
  | DO.sat      | numeric        | mgO2 L^-1      | required |
  | depth       | numeric        | m              | required |
  | temp.water  | numeric        | degC           | required |
  | light       | numeric        | umol m^-2 s^-1 | required |

      \strong{Example}:
      \tabular{lrrrrr}{
        \code{solar.time         } \tab \code{DO.obs} \tab \code{DO.sat} \tab \code{depth} \tab \code{temp.water} \tab \code{light}\cr
        \code{2050-03-14 15:10:00} \tab \code{10.1  } \tab \code{14.2  } \tab \code{0.5  } \tab \code{21.8      } \tab \code{300.9}
      }

## Formatting `data_daily`

Daily-value model inputs passed via the `data_daily` argument should be
formatted as a data.frame with column names and values that depend on
the model `type`, as follows. (If all columns are optional, `data_daily`
may equal `NULL`.)

- `night`:

  `NULL`

- `mle`:

  |                 |           |                    |          |
  |-----------------|-----------|--------------------|----------|
  | **colname**     | **class** | **units**          | **need** |
  | date            | Date      |                    | optional |
  | K600.daily      | numeric   | d^-1               | optional |
  | init.GPP.daily  | numeric   | gO2 m^-2 d^-1      | optional |
  | init.Pmax       | numeric   | gO2 m^-2 d^-1      | optional |
  | init.alpha      | numeric   | gO2 s d^-1 umol^-1 | optional |
  | init.ER.daily   | numeric   | gO2 m^-2 d^-1      | optional |
  | init.ER20       | numeric   | gO2 m^-2 d^-1      | optional |
  | init.K600.daily | numeric   | d^-1               | optional |

      \strong{Example}:
      \tabular{lrrrrrrr}{
        \code{date      } \tab \code{K600.daily} \tab \code{init.GPP.daily} \tab \code{init.Pmax} \tab \code{init.alpha} \tab \code{init.ER.daily} \tab \code{init.ER20} \tab \code{init.K600.daily}\cr
        \code{2050-03-14} \tab \code{10        } \tab \code{5             } \tab \code{10       } \tab \code{1e-04     } \tab \code{-10          } \tab \code{-10      } \tab \code{10             }
      }

- `bayes`:

  |                 |           |           |          |
  |-----------------|-----------|-----------|----------|
  | **colname**     | **class** | **units** | **need** |
  | date            | Date      |           | optional |
  | discharge.daily | numeric   | m^3 s^-1  | optional |

      \strong{Example}:
      \tabular{lr}{
        \code{date      } \tab \code{discharge.daily}\cr
        \code{2050-03-14} \tab \code{9              }
      }

- `Kmodel`:

  |                  |           |           |          |
  |------------------|-----------|-----------|----------|
  | **colname**      | **class** | **units** | **need** |
  | date             | Date      |           | required |
  | K600.daily       | numeric   | d^-1      | required |
  | K600.daily.lower | numeric   | d^-1      | optional |
  | K600.daily.upper | numeric   | d^-1      | optional |
  | discharge.daily  | numeric   | m^3 s^-1  | optional |
  | velocity.daily   | numeric   | m s^-1    | optional |

      \strong{Example}:
      \tabular{lrrrrr}{
        \code{date      } \tab \code{K600.daily} \tab \code{K600.daily.lower} \tab \code{K600.daily.upper} \tab \code{discharge.daily} \tab \code{velocity.daily}\cr
        \code{2050-03-14} \tab \code{10        } \tab \code{4.5             } \tab \code{15.6            } \tab \code{9              } \tab \code{2             }
      }

- `sim`:

  |                 |           |                    |          |
  |-----------------|-----------|--------------------|----------|
  | **colname**     | **class** | **units**          | **need** |
  | date            | Date      |                    | optional |
  | discharge.daily | numeric   | m^3 s^-1           | optional |
  | DO.mod.1        | numeric   | mgO2 L^-1          | optional |
  | K600.daily      | numeric   | d^-1               | optional |
  | GPP.daily       | numeric   | gO2 m^-2 d^-1      | optional |
  | Pmax            | numeric   | gO2 m^-2 d^-1      | optional |
  | alpha           | numeric   | gO2 s d^-1 umol^-1 | optional |
  | ER.daily        | numeric   | gO2 m^-2 d^-1      | optional |
  | ER20            | numeric   | gO2 m^-2 d^-1      | optional |
  | err.obs.sigma   | numeric   | mgO2 L^-1          | optional |
  | err.obs.phi     | numeric   |                    | optional |
  | err.proc.sigma  | numeric   | gO2 m^-2 d^-1      | optional |
  | err.proc.phi    | numeric   |                    | optional |

      \strong{Example}:
      \tabular{lrrrrrrrrrrrr}{
        \code{date      } \tab \code{discharge.daily} \tab \code{DO.mod.1} \tab \code{K600.daily} \tab \code{GPP.daily} \tab \code{Pmax} \tab \code{alpha} \tab \code{ER.daily} \tab \code{ER20} \tab \code{err.obs.sigma} \tab \code{err.obs.phi} \tab \code{err.proc.sigma} \tab \code{err.proc.phi}\cr
        \code{2050-03-14} \tab \code{9              } \tab \code{7.5     } \tab \code{10        } \tab \code{5        } \tab \code{10  } \tab \code{1e-04} \tab \code{-10     } \tab \code{-10 } \tab \code{0.01         } \tab \code{0          } \tab \code{5             } \tab \code{0           }
      }

## Author

Alison Appling

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
#>   0.868   0.000   0.867 

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
