data {
  int<lower = 1> N; // number of observations
  real y[N];        // observed time series
  int<lower = 1> T; // time of sitemaps deployment
  int<lower = 1> p; // number of AR terms
}
parameters {
  real<lower = 0> sigma;              // standard deviation of noise
  real delta0;                        // impact of intervention
  real mu;                            // intercept
  real<lower = -1, upper = 1> phi[p]; // AR(p) coefficients
}
model {
  // priors
  mu ~ normal(8, 4); // the average is 8 mil pvs/day
  sigma ~ cauchy(0, 5);
  delta0 ~ normal(0, 10);
  phi ~ cauchy(0, 1);
  // likelihood
  for (t in (p + 1):N) {
    real nu = mu + delta0 * (t >= T ? 1 : 0);
    for (i in 1:p) {
      nu += phi[i] * y[t - i];
    }
    y[t] ~ normal(nu, sigma);
  }
}
generated quantities {
  real yhat[N];
  for (t in 1:p) {
    yhat[t] = normal_rng(mu + delta0 * (t >= T ? 1 : 0), sigma);
  }
  for (t in (p + 1):N) {
    real nu = mu + delta0 * (t >= T ? 1 : 0);
    for (i in 1:min(t - 1, p)) {
      nu += phi[i] * y[t - i];
    }
    yhat[t] = normal_rng(nu, sigma);
  }
}
