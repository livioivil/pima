# Hurricane names and damage data

A data set used to study the association between hurricane name
femininity and hurricane fatalities.

## Usage

``` r
data(hurricane)
```

## Format

A data frame with 94 rows and 13 variables:

- year:

  Year of the hurricane.

- name:

  Hurricane name.

- masfem:

  Name femininity score, from 1 masculine to 11 feminine.

- min:

  Minimum pressure.

- gender_mf:

  Name gender, male or female.

- category:

  Hurricane category.

- alldeaths:

  Total deaths.

- ndam:

  Normalized damage amount.

- elapsedyrs:

  Years elapsed since the hurricane.

- source:

  Data source.

- masfem_mturk:

  Name femininity score from MTurk ratings.

- wind:

  Highest wind speed.

- ndam15:

  Damage amount normalized to 2015.

## Source

Original analysis from Jung et al. (2014) .

## References

Jung K, Shavitt S, Viswanathan M, Hilbe JM (2014). “Female Hurricanes
Are Deadlier than Male Hurricanes.” *Proceedings of the National Academy
of Sciences*, **111**(24), 8782–8787.
[doi:10.1073/pnas.1402786111](https://doi.org/10.1073/pnas.1402786111) .

## Examples

``` r
data("hurricane", package = "pima")
head(hurricane)
#>   year     name   masfem min gender_mf category alldeaths  ndam elapsedyrs
#> 1 1950     Easy 6.777778 958    Female        3         2  1590         63
#> 2 1950     King 1.388889 955      Male        3         4  5350         63
#> 3 1952     Able 3.833333 985      Male        1         3   150         61
#> 4 1953  Barbara 9.833333 987    Female        1         1    58         60
#> 5 1953 Florence 8.333333 985    Female        1         0    15         60
#> 6 1954    Carol 8.111111 960    Female        3        60 19321         59
#>   source masfem_mturk wind ndam15
#> 1    MWR      5.40625  120   1870
#> 2    MWR      1.59375  130   6030
#> 3    MWR      2.96875   85    170
#> 4    MWR      8.62500   85     65
#> 5    MWR      7.87500   85     18
#> 6    MWR      8.53125  120  21375
```
