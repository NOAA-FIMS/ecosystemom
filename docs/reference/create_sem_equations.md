# Create SEM Equations from a Tibble

This helper function generates a character string of SEM equations
formatted for the `dsem` package from a tibble of SEM specifications.

## Usage

``` r
create_sem_equations(sem_tibble)
```

## Arguments

- sem_tibble:

  A tibble with columns `driver`, `target`, `lag`, and `param_name`.

## Value

A character string with each SEM equation on a new line.

## Examples

``` r
sem_tibble <- tibble::tibble(
  driver = "sst", target = "menhaden_0yr", lag = 12, param_name = "sst_menhaden_0yr"
)
create_sem_equations(sem_tibble)
#> [1] "sst -> menhaden_0yr, 12, sst_menhaden_0yr"
```
