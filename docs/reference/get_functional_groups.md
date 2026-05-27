# Get the functional groups from an EwE file

Functional group names are useful for the column names of output data
from an EwE model, and thus, this function is a way to get them
automatically from the EwE output. The functional group names come from
the basic estimates file.

## Usage

``` r
get_functional_groups(file_path)
```

## Arguments

- file_path:

  The path to the EwE file.

## Value

A tibble with the following columns:

- function_group. The full functional group name from the model. This
  column is helpful for error checking and debugging.

- species. The species name from the functional group name. This column
  should only contain text strings and no numbers or special characters.
  There will potentially be multiple rows with the same species name
  because there can be multiple age groups for a given species.

- group. The group name as a string. This column can contain digits,
  special characters, and text strings. It is used to delineate the
  group within a species. Not all species will have multiple groups.

- functional_group_snake_case. A standardized version of the functional
  group name. The functional_group is converted to lowercase and swaps
  spaces or hyphens for underscores.

## Examples

``` r
get_functional_groups(
  file_path = fs::path(
    system.file("extdata", package = "ecosystemom"),
    "ewe_ecosim_with_environmental_data_nwatlantic", "basic_estimates.csv"
  )
)
#> # A tibble: 22 × 4
#>    functional_group species      group functional_group_snake_case
#>    <chr>            <chr>        <chr> <chr>                      
#>  1 striped bass 0   striped bass 0     striped_bass_0             
#>  2 striped bass 2-5 striped bass 2-5   striped_bass_2_5           
#>  3 striped bass 6+  striped bass 6+    striped_bass_6_plus        
#>  4 menhaden 0       menhaden     0     menhaden_0                 
#>  5 menhaden 1       menhaden     1     menhaden_1                 
#>  6 menhaden 2       menhaden     2     menhaden_2                 
#>  7 menhaden 3       menhaden     3     menhaden_3                 
#>  8 menhaden 4       menhaden     4     menhaden_4                 
#>  9 menhaden 5       menhaden     5     menhaden_5                 
#> 10 menhaden 6+      menhaden     6+    menhaden_6_plus            
#> # ℹ 12 more rows
```
