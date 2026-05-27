# Split the functional groups into species and group

Split strings containing functional groups into species and group names.
Functional groups can contain age ranges, plus groups, or sub
categories, e.g., juvenile. This information is extracted from the
functional group name and returned as the group name. The
`functional_group_snake_case` column is created by converting the
`functional_group` name to lowercase and substituting spaces or hyphens
with underscores.

## Usage

``` r
split_functional_groups(x)
```

## Arguments

- x:

  A character vector of functional group names. This is most often
  created by running [`unique()`](https://rdrr.io/r/base/unique.html) on
  the functional group column of the data.

## Value

A tibble with the following three columns: functional_group, species,
group, and functional_group_snake_case is returned.

## Author

Kelli F. Johnson

## Examples

``` r
# A hypothetical example of functional groups
split_functional_groups(
  c("spiny dogfish 0", "spiny dogfish 1-2", "spiny dogfish +")
)
#> # A tibble: 3 × 4
#>   functional_group  species       group functional_group_snake_case
#>   <chr>             <chr>         <chr> <chr>                      
#> 1 spiny dogfish 0   spiny dogfish 0     spiny_dogfish_0            
#> 2 spiny dogfish 1-2 spiny dogfish 1-2   spiny_dogfish_1_2          
#> 3 spiny dogfish +   spiny dogfish +     spiny_dogfish_plus         
split_functional_groups(c("spiny dogfish", "spiny dogfish juvenile"))
#> # A tibble: 2 × 4
#>   functional_group       species       group    functional_group_snake_case
#>   <chr>                  <chr>         <chr>    <chr>                      
#> 1 spiny dogfish          spiny dogfish NA       spiny_dogfish              
#> 2 spiny dogfish juvenile spiny dogfish juvenile spiny_dogfish_juvenile     
data("ewe_ecosim_base_nwatlantic", package = "ecosystemom")
split_functional_groups(unique(ewe_ecosim_base_nwatlantic[["functional_group"]]))
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
