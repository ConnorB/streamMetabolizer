# Locate the persistent compilation cache for a CmdStan model

Locate the persistent compilation cache for a CmdStan model

## Usage

``` r
cmdstan_cache_dir(
  model_path,
  cmdstan_version,
  cmdstanr_version = package_version_for_cache("cmdstanr"),
  platform = R.version$platform
)
```

## Arguments

- model_path:

  Path to a Stan program.

- cmdstan_version:

  The configured CmdStan version.

- cmdstanr_version:

  The installed CmdStanR version.

- platform:

  The platform on which the model executable will run.

## Value

A writable cache directory unique to the model contents and CmdStan
toolchain.
