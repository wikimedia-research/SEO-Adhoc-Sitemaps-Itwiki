functions {
    real gompertz(real t, real a, real b, real c) {
        return (a * exp(-b * exp(-c * t)));
    }
}
data {
  int<lower = 1> N; // number of observations
  real y[N];        // observed time series
  int<lower = 1> T; // time of sitemaps deployment
  int<lower = 1> p; // number of AR terms
  int<lower = 1> q; // number of MA terms
  int<lower = 1> K; // number of predictors
  matrix[N, K] x;   // predictor matrix
}
parameters {
  real<lower = 0> sigma;                // standard deviation of noise
  real delta0;                          // impact of intervention
  real mu;                              // intercept
  real<lower = -1, upper = 1> phi[p];   // AR(p) coefficients
  real<lower = -1, upper = 1> theta[q]; // MA(q) coefficients
  vector[K] beta;                       // coefficients for predictors
  real<lower = 0> lambda;               // Gompertz growth rate
  real<lower = 1> d;                    // Gompertz displacement along t
}
model {
  vector[N] epsilon; // error/noise at time t
  // priors
  mu ~ normal(8, 4); // the average is 8 mil pvs/day
  sigma ~ cauchy(0, 5);
  delta0 ~ normal(0, 10);
  beta ~ normal(0, 10);
  phi ~ cauchy(0, 1);
  theta ~ cauchy(0, 1);
  lambda ~ cauchy(0, 5);
  d ~ normal(7, 5);
  // likelihood
  for (t in 1:max(p, q)) {
    epsilon[t] = 0;
  }
  for (t in (max(p, q) + 1):N) {
    real z = 0;
    real nu = mu + x[t, ] * beta;
    if (t >= T) {
      z += gompertz(t - T, delta0, d, lambda);
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
      z[t] = gompertz(t - T, delta0, d, lambda);
    } else {
      z[t] = 0;
    }
  }
  for (t in 1:max(p, q)) {
    yhat[t] = mu + x[t, ] * beta + z[t];
  }
  for (t in (max(p, q) + 1):N) {
    real nu = mu + x[t, ] * beta + z[t];
    for (i in 1:p) {
      nu += phi[i] * y[t - i];
    }
    for (j in 1:q) {
      nu += theta[j] * noise[t - j];
    }
    yhat[t] = nu + noise[t];
  }
}
