# Load a short dataset from French Creek using Bob Hall's code

This function requires `chron`, a package that is not formally required
by the `streamMetabolizer` package overall. Ensure that you have `chron`
installed or install it with `install.packages('chron')`.

## Usage

``` r
load_french_creek_std(attach.units = deprecated())
```

## Arguments

- attach.units:

  (deprecated, effectively FALSE in future) logical, default TRUE for
  backward compatibility. Should units be attached to the data.frame?

## Value

a data.frame, unitted if attach.units==TRUE

## Details

This function produces a version of the French Creek dataset that agrees
as much as possible with raw code from Bob Hall, for comparison to
functions written in streamMetabolizer. Line numbers in comments (L22,
etc.) refer to those in 'stream_metab_usa/starter_files/One station
metab code.R'
