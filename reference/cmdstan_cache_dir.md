# Locate the persistent compilation cache for a CmdStan model

Locate the persistent compilation cache for a CmdStan model

## Usage

``` r
cmdstan_cache_dir(model_path, cmdstan_version)
```

## Arguments

- model_path:

  Path to a Stan program.

- cmdstan_version:

  The configured CmdStan version.

## Value

A writable cache directory unique to the model contents and CmdStan
version.
