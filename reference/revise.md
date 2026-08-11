# Change or add named elements of a list

Primary use case is for revising a list of specifications as originally
created by specs()

## Usage

``` r
revise(specs, ..., delete)
```

## Arguments

- specs:

  A list of specifications to revise.

- ...:

  Named values to replace in or add to `specs`.

- delete:

  A vector of names of elements to remove from the specs list.

## Value

The revised list.

## Examples

``` r
sp <- specs(mm_name('bayes'))
sp <- revise(sp,
  model_name='b_np_oipi_tr_plrckm_mynewmodel.stan',
  params_in=c(params_in,'my_new_param'), my_new_param=4)
```
