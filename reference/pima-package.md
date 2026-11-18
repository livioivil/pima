# Post-selection Inference in Multiverse Analysis

Runs resampling-based tests jointly to allow for multiple testing in
multiverse analysis. As main feature, it applies the PIMA procedure, a
general and flexible framework based on the sign-flip score test that
provides control of the Family-Wise Error Rate (FWER) and confidence
bounds for True Discovery Proportions (TDPs).

## Details

A typical analysis has four steps:

1.  build and fit the multiverse of specifications with
    [`create_multi()`](https://livioivil.github.io/pima/reference/create_multi.md);

2.  collect the results in a table with
    [`spec_table()`](https://livioivil.github.io/pima/reference/spec_table.md)
    and plot them with
    [`spec_curve()`](https://livioivil.github.io/pima/reference/spec_curve.md)
    and
    [`spec_scatter()`](https://livioivil.github.io/pima/reference/spec_scatter.md);

3.  describe the vibration of effects with
    [`voe()`](https://livioivil.github.io/pima/reference/voe.md),
    [`janus()`](https://livioivil.github.io/pima/reference/janus.md) and
    [`volcano()`](https://livioivil.github.io/pima/reference/volcano.md);

4.  test the effects jointly across the specifications with
    [`pima()`](https://livioivil.github.io/pima/reference/pima.md),
    controlling the FWER.

## References

Girardi P, Vesely A, Lakens D, Altoè G, Pastore M, Calcagnì A, Finos L
(2024). “Post-selection Inference in Multiverse Analysis (PIMA): An
Inferential Framework Based on the Sign Flipping Score Test.”
*Psychometrika*, **89**(2), 542–568.
[doi:10.1007/s11336-024-09973-6](https://doi.org/10.1007/s11336-024-09973-6)
.

Hemerik J, Goeman JJ, Finos L (2020). “Robust Testing in Generalized
Linear Models by Sign Flipping Score Contributions.” *Journal of the
Royal Statistical Society Series B: Statistical Methodology*, **82**(3),
841–864. [doi:10.1111/rssb.12369](https://doi.org/10.1111/rssb.12369) .

De Santis R, Goeman JJ, Hemerik J, Davenport S, Finos L (2025).
“Inference in Generalized Linear Models with Robustness to Misspecified
Variances.” *Journal of the American Statistical Association*,
**120**(552), 2762–2771.
[doi:10.1080/01621459.2025.2491775](https://doi.org/10.1080/01621459.2025.2491775)
.

## See also

Useful links:

- <https://livioivil.github.io/pima>

- Report bugs at <https://github.com/livioivil/pima/issues>

## Author

**Maintainer**: Filippo Gambarota <filippo.gambarota@unipd.it>

Authors:

- Filippo Gambarota <filippo.gambarota@unipd.it>

- Livio Finos

- Paolo Girardi

- Anna Vesely

- Giulia Calignano

- Massimiliano Pastore

- Gianmarco Altoè
