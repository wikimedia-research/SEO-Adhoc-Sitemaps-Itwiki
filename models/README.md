# Stan models

This is a repository of the Stan code for the various models explored during model search.

- General [autoregressive moving average](https://en.wikipedia.org/wiki/Autoregressive%E2%80%93moving-average_model) models with instant, permanent change:
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
  - [ARMA(p,q) w/ regressors (v2)](arma_regression_p5e.stan): reparameterization with categorical predictors as random intercepts
