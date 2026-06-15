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

- `functional_group`: The exact functional group name used by EwE. These
  values should match the names and order expected by the EwE output
  files that you will load later. Keep this column exact to the original
  EwE text as possible because it is the audit trail back to the model.

- `species`: The biological or conceptual species/group name after
  removing any age, stage, size, or other within-species suffix. For
  example, if EwE has `"King mackerel (0-1yr)"` and
  `"King mackerel (1+yr)"`, both rows should usually have
  `"King mackerel"` in `species`.

- `group`: the age, stage, size, or other suffix within `species`. Use a
  character value such as `"0-1yr"`, `"1+yr"`, `"juvenile"`, or
  `"adult"` when a functional group is one of several groups for the
  same species. Use `NA_character_` when the functional group does not
  have a within-species suffix. It is used to delineate the group within
  a species. Not all species will have multiple groups.

- `functional_group_snake_case`: A unique, syntactically convenient name
  that can be used as a column name in downstream output. These names
  should be lowercase, should not contain spaces or parentheses, and
  should be unique across rows.

## Details

This function is for convenience only, it is not required by the
package. Using it to generate the functional group names from a
`basic_estimates.csv` file lessens the burden on you but we realize that
the function will not work for every ecosystem model. It does work for
many common naming conventions, including species names followed by
numeric age groups, plus groups, ranges, and a small number of text
suffixes such as `"juv"` and `"adult"`. However, functional group names
in real EwE models are not always consistent. If your model uses names
that cannot be parsed correctly, you can create the functional group
tibble yourself and pass that tibble to functions such as
[`load_csv_ewe()`](load_csv_ewe.md).

A custom functional group tibble must contain one row for each
functional group in the model and the same four columns returned by this
function. See the return section for more details on the columns. To
create the last column, `functional_group_snake_case`, consider calling
[`split_functional_groups()`](split_functional_groups.md) on the
original names and then manually edit `species` and `group` where
needed.

The custom tibble should be checked before it is used in model-loading
functions. In particular, confirm that `functional_group` has no missing
values, that every functional group name is unique, that
`functional_group_snake_case` is unique, and that the number of rows
equals the number of living and non-living groups in the EwE output
files you are about to load. If those columns are misaligned, downstream
data can be read into the wrong functional group. When in doubt, start
with the output of `get_functional_groups()` or
[`split_functional_groups()`](split_functional_groups.md), inspect the
result, and then replace only the rows that were parsed incorrectly.

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

# If the automatic parser does not work for your naming convention, create
# the functional group tibble yourself. This object can be supplied anywhere
# ecosystemom asks for `functional_groups`.
functional_groups <- tibble::tibble(
  functional_group = c(
    "King mackerel_(0-1yr)",
    "King mackerel_(1+yr)",
    "Detritus"
  ),
  species = c("King mackerel", "King mackerel", "Detritus"),
  group = c("0-1yr", "1+yr", NA_character_),
  functional_group_snake_case = c(
    "king_mackerel_0_1yr",
    "king_mackerel_1_plusyr",
    "detritus"
  )
)
```
