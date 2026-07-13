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

  data.frame (not a tbl_df) of input data at the temporal resolution of
  raw observations (unit-value). Columns must have the same names,
  units, and format as the default. The solar.time column must also have
  a timezone code ('tzone' attribute) of 'UTC'. See the **'Formatting
  `data`'** section below for a full description.

- data_daily:

  data.frame containing inputs with a daily timestep. See the
  **'Formatting `data_daily`'** section below for a full description.

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

  |             |                |           |          |
  |-------------|----------------|-----------|----------|
  | **colname** | **class**      | **units** | **need** |
  | solar.time  | POSIXct,POSIXt | NA        | required |
  | DO.obs      | numeric        | NA        | required |
  | DO.sat      | numeric        | NA        | required |
  | depth       | numeric        | NA        | required |
  | temp.water  | numeric        | NA        | required |
  | light       | numeric        | NA        | required |
  | discharge   | numeric        | NA        | optional |

      \strong{Example}:
      \tabular{lrrrrrr}{
        \code{solar.time         } \tab \code{DO.obs} \tab \code{DO.sat} \tab \code{depth} \tab \code{temp.water} \tab \code{light} \tab \code{discharge}\cr
        \code{2050-03-14 15:10:00} \tab \code{10.1  } \tab \code{14.2  } \tab \code{0.5  } \tab \code{21.8      } \tab \code{300.9} \tab \code{9        }
      }

- `bayes`:

  |             |                |           |          |
  |-------------|----------------|-----------|----------|
  | **colname** | **class**      | **units** | **need** |
  | solar.time  | POSIXct,POSIXt | NA        | required |
  | DO.obs      | numeric        | NA        | required |
  | DO.sat      | numeric        | NA        | required |
  | depth       | numeric        | NA        | required |
  | temp.water  | numeric        | NA        | required |
  | light       | numeric        | NA        | required |
  | discharge   | numeric        | NA        | optional |

      \strong{Example}:
      \tabular{lrrrrrr}{
        \code{solar.time         } \tab \code{DO.obs} \tab \code{DO.sat} \tab \code{depth} \tab \code{temp.water} \tab \code{light} \tab \code{discharge}\cr
        \code{2050-03-14 15:10:00} \tab \code{10.1  } \tab \code{14.2  } \tab \code{0.5  } \tab \code{21.8      } \tab \code{300.9} \tab \code{9        }
      }

- `Kmodel`:

  |             |                |           |          |
  |-------------|----------------|-----------|----------|
  | **colname** | **class**      | **units** | **need** |
  | solar.time  | POSIXct,POSIXt | NA        | optional |
  | discharge   | numeric        | NA        | optional |
  | velocity    | numeric        | NA        | optional |

      \strong{Example}:
      \tabular{lrr}{
        \code{solar.time         } \tab \code{discharge} \tab \code{velocity}\cr
        \code{2050-03-14 15:10:00} \tab \code{9        } \tab \code{2       }
      }

- `sim`:

  |             |                |           |          |
  |-------------|----------------|-----------|----------|
  | **colname** | **class**      | **units** | **need** |
  | solar.time  | POSIXct,POSIXt | NA        | required |
  | DO.obs      | numeric        | NA        | optional |
  | DO.sat      | numeric        | NA        | required |
  | depth       | numeric        | NA        | required |
  | temp.water  | numeric        | NA        | required |
  | light       | numeric        | NA        | required |

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

  |                 |           |           |          |
  |-----------------|-----------|-----------|----------|
  | **colname**     | **class** | **units** | **need** |
  | date            | Date      | NA        | optional |
  | K600.daily      | numeric   | NA        | optional |
  | init.GPP.daily  | numeric   | NA        | optional |
  | init.Pmax       | numeric   | NA        | optional |
  | init.alpha      | numeric   | NA        | optional |
  | init.ER.daily   | numeric   | NA        | optional |
  | init.ER20       | numeric   | NA        | optional |
  | init.K600.daily | numeric   | NA        | optional |

      \strong{Example}:
      \tabular{lrrrrrrr}{
        \code{date      } \tab \code{K600.daily} \tab \code{init.GPP.daily} \tab \code{init.Pmax} \tab \code{init.alpha} \tab \code{init.ER.daily} \tab \code{init.ER20} \tab \code{init.K600.daily}\cr
        \code{2050-03-14} \tab \code{10        } \tab \code{5             } \tab \code{10       } \tab \code{1e-04     } \tab \code{-10          } \tab \code{-10      } \tab \code{10             }
      }

- `bayes`:

  |                 |           |           |          |
  |-----------------|-----------|-----------|----------|
  | **colname**     | **class** | **units** | **need** |
  | date            | Date      | NA        | optional |
  | discharge.daily | numeric   | NA        | optional |

      \strong{Example}:
      \tabular{lr}{
        \code{date      } \tab \code{discharge.daily}\cr
        \code{2050-03-14} \tab \code{9              }
      }

- `Kmodel`:

  |                  |           |           |          |
  |------------------|-----------|-----------|----------|
  | **colname**      | **class** | **units** | **need** |
  | date             | Date      | NA        | required |
  | K600.daily       | numeric   | NA        | required |
  | K600.daily.lower | numeric   | NA        | optional |
  | K600.daily.upper | numeric   | NA        | optional |
  | discharge.daily  | numeric   | NA        | optional |
  | velocity.daily   | numeric   | NA        | optional |

      \strong{Example}:
      \tabular{lrrrrr}{
        \code{date      } \tab \code{K600.daily} \tab \code{K600.daily.lower} \tab \code{K600.daily.upper} \tab \code{discharge.daily} \tab \code{velocity.daily}\cr
        \code{2050-03-14} \tab \code{10        } \tab \code{4.5             } \tab \code{15.6            } \tab \code{9              } \tab \code{2             }
      }

- `sim`:

  |                 |           |           |          |
  |-----------------|-----------|-----------|----------|
  | **colname**     | **class** | **units** | **need** |
  | date            | Date      | NA        | optional |
  | discharge.daily | numeric   | NA        | optional |
  | DO.mod.1        | numeric   | NA        | optional |
  | K600.daily      | numeric   | NA        | optional |
  | GPP.daily       | numeric   | NA        | optional |
  | Pmax            | numeric   | NA        | optional |
  | alpha           | numeric   | NA        | optional |
  | ER.daily        | numeric   | NA        | optional |
  | ER20            | numeric   | NA        | optional |
  | err.obs.sigma   | numeric   | NA        | optional |
  | err.obs.phi     | numeric   | NA        | optional |
  | err.proc.sigma  | numeric   | NA        | optional |
  | err.proc.phi    | numeric   | NA        | optional |

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
#>         date      GPP GPP.lower GPP.upper        ER  ER.lower  ER.upper
#> 1 2012-09-18 2.812955  2.435230  3.190681 -2.103521 -2.410113 -1.796930
#> 2 2012-09-19 3.277526  2.866958  3.688094 -2.470115 -2.808856 -2.131375
#> 3 2012-09-20 2.582700  2.313734  2.851666 -1.709653 -1.914700 -1.504606
#>   msgs.fit warnings errors
#> 1                         
#> 2      W                  
#> 3                         
get_info(mm)
#> [1] "my info"
get_fitting_time(mm)
#>    user  system elapsed 
#>   0.462   0.000   0.461 

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
mm <- mm_name('mle', ode_method='euler') %>%
  specs(init.GPP.daily=40) %>%
  metab(data=dat)
predict_metab(mm)
#>         date      GPP GPP.lower GPP.upper        ER  ER.lower  ER.upper
#> 1 2012-09-18 2.809444  2.402234  3.216654 -2.098404 -2.427638 -1.769169
#> 2 2012-09-19 3.271351  2.838930  3.703772 -2.463138 -2.818887 -2.107388
#> 3 2012-09-20 2.568592  2.286376  2.850807 -1.697389 -1.911571 -1.483207
#>   msgs.fit warnings errors
#> 1      W                  
#> 2      W                  
#> 3      W                  
if (FALSE) { # \dontrun{
plot_DO_preds(predict_DO(mm))
plot_DO_preds(predict_DO(mm), y_var='pctsat', style='dygraphs')
} # }
```
