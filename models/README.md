# Models

Models with regressors have month, day of the week, and [Italian holidays](https://en.wikipedia.org/wiki/Public_holidays_in_Italy) as predictors.

- [Simple](simple.stan)
  - no accounting for correlation, intervention effect moves intercept
  - very bad model
- General [autoregressive moving average](https://en.wikipedia.org/wiki/Autoregressive%E2%80%93moving-average_model) models:
  - [AR(p)](ar.stan)
  - [AR(p) w/ regressors](ar_regression.stan)
  - [ARMA(p,q)](arma.stan)
  - [ARMA(p,q) w/ regressors](arma_regression.stan)
- AR/ARMA models with levelling-off change
  - [AR(p)](ar_p3e.stan)
  - [AR(p) w/ regressors](ar_regression_p3e.stan)
  - [ARMA(p,q)](arma_p3e.stan)
  - [ARMA(p,q) w/ regressors](arma_regression_p3e.stan)
- AR/ARMA models with [Gompertz](https://en.wikipedia.org/wiki/Gompertz_function)-modeled change (slowly ramps up, then slowly levels off)
  - [AR(p)](ar_p5e.stan)
  - [AR(p) w/ regressors](ar_regression_p5e.stan)
  - [ARMA(p,q)](arma_p5e.stan)
  - [ARMA(p,q) w/ regressors](arma_regression_p5e.stan)
