# Pima Indians diabetes data

A data set of diabetes outcomes and clinical predictors for women from
the Pima Indian population.

## Usage

``` r
data(pimads)
```

## Format

A data frame with 768 rows and 8 variables:

- npreg:

  Number of pregnancies.

- glucose:

  Plasma glucose concentration from an oral glucose tolerance test.

- pressure:

  Diastolic blood pressure in mm Hg.

- skin:

  Triceps skin fold thickness in mm.

- bmi:

  Body mass index.

- pedigree:

  Diabetes pedigree function.

- age:

  Age in years.

- diabetes:

  Diabetes diagnosis, with levels `neg` and `pos`.

## References

Smith JW, Everhart JE, Dickson WC, Knowler WC, Johannes RS (1988).
“Using the ADAP Learning Algorithm to Forecast the Onset of Diabetes
Mellitus.” In *Proceedings of the Symposium on Computer Applications in
Medical Care*.

## Examples

``` r
data("pimads", package = "pima")
head(pimads)
#>   npreg glucose pressure skin  bmi pedigree age diabetes
#> 1     6     148       72   35 33.6    0.627  50      pos
#> 2     1      85       66   29 26.6    0.351  31      neg
#> 3     8     183       64   NA 23.3    0.672  32      pos
#> 4     1      89       66   23 28.1    0.167  21      neg
#> 5     0     137       40   35 43.1    2.288  33      pos
#> 6     5     116       74   NA 25.6    0.201  30      neg
```
