data {
  int<lower = 1> N; // number of observations
  real y[N];        // observed time series
  int<lower = 1> T; // time of sitemaps deployment
  int<lower = 1> p; // number of AR terms
  int<lower = 1> q; // number of MA terms
}
parameters {
  real<lower = 0> sigma;                // standard deviation of noise
  real delta0;                          // impact of intervention
  real mu;                              // intercept
  real<lower = -1, upper = 1> phi[p];   // AR(p) coefficients
  real<lower = -1, upper = 1> theta[q]; // MA(q) coefficients
  real<lower = 0, upper = 1> omega1;    // gradation coefficient
  real beta0;                           // trend coefficient
}
model {
  vector[N] epsilon; // error/noise at time t
  // priors
  mu ~ normal(0, 10);
  sigma ~ cauchy(0, 5);
  delta0 ~ normal(0, 10);
  phi ~ cauchy(0, 1);
  theta ~ cauchy(0, 1);
  omega1 ~ beta(6, 3);
  beta0 ~ normal(0, 10);
  // likelihood
  for (t in 1:max(p, q)) {
    epsilon[t] = 0;
  }
  for (t in (max(p, q) + 1):N) {
    real z = 0;
    real nu = mu + beta0 * t;
    if (t >= T) {
      z += delta0 * (1 - (omega1 ^ (t - T + 1))) / (1 - omega1);
    }
    nu += z;
    for (i in 1:p) {
      nu += phi[i] * y[t - i];
    }
    for (j in 1:q) {
      nu += theta[j] * epsilon[t - j];
    }
    epsilon[t] = y[t] - nu;
  }
  epsilon ~ normal(0, sigma);
}
generated quantities {
  real noise[N];
  real z[N];
  real yhat[N];
  for (t in 1:N) {
    noise[t] = normal_rng(0, sigma);
    if (t >= T) {
      z[t] = delta0 * (1 - (omega1 ^ (t - T + 1))) / (1 - omega1);
    } else {
      z[t] = 0;
    }
  }
  for (t in 1:max(p, q)) {
    yhat[t] = mu + z[t] + noise[t] + beta0 * t;
  }
  for (t in (max(p, q) + 1):N) {
    real nu = mu + z[t];
    for (i in 1:p) {
      nu += phi[i] * y[t - i];
    }
    for (j in 1:q) {
      nu += theta[j] * noise[t - j];
    }
    yhat[t] = nu + noise[t];
  }
}
