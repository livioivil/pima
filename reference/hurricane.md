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

Original analysis from Jung et al. (2014),
<https://www.pnas.org/content/111/24/8782>.
