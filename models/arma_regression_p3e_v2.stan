data {
  int<lower = 1> N;               // number of observations
  real y[N];                      // observed time series
  int<lower = 1> T;               // time of sitemaps deployment
  int<lower = 1> p;               // number of AR terms
  int<lower = 1> q;               // number of MA terms
  int<lower = 1> K;               // number of continuous predictors
  matrix[N, K] x;                 // predictor matrix of continuous variables, incl. t for linear trend
  int<lower = 1> M;               // number of random intercepts
  int<lower = 1, upper = M> c[N]; // which of M intercepts to use for t-th observation
}
parameters {
  real<lower = 0> sigma;                // standard deviation of noise
  real delta0;                          // impact of intervention
  real<lower = 0> tau;                  // std dev of delta0
  real<lower = -1, upper = 1> phi[p];   // AR(p) coefficients
  real<lower = -1, upper = 1> theta[q]; // MA(q) coefficients
  vector[K] beta;                       // coefficients for continuous predictors
  real<lower = 0> alpha_mean;           // group mean for the categorical predictor
  real alpha_offset[M];                 // differentials
  real<lower = 0> alpha_stddev;         // standard deviation of the random intercepts
  real<lower = 0, upper = 1> omega1;    // gradation coefficient
}
transformed parameters {
  real alpha[M]; // random intercepts
  for (m in 1:M) {
    alpha[m] = alpha_mean + alpha_stddev * alpha_offset[m];
  }
}
model {
  vector[N] epsilon; // error/noise at time t
  // priors
  alpha_mean ~ normal(0, 10);
  for (m in 1:M) {
    alpha_offset[m] ~ normal(0, 1);
  }
  alpha_stddev ~ cauchy(0, 5);
  sigma ~ cauchy(0, 5);
  tau ~ cauchy(0, 5);
  delta0 ~ normal(0, tau);
  beta ~ normal(0, 10);
  phi ~ cauchy(0, 1);
  theta ~ cauchy(0, 1);
  omega1 ~ beta(9, 3); // expected value around 0.75, with most of prob 0.6-1.0
  // likelihood
  for (t in 1:max(p, q)) {
    epsilon[t] = 0;
  }
  for (t in (max(p, q) + 1):N) {
    real z = 0;
    real nu = alpha[c[t]] + x[t, ] * beta;
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
    real nu = alpha[c[t]] + x[t, ] * beta;
    yhat[t] = nu + z[t];
  }
  for (t in (max(p, q) + 1):N) {
    real nu = alpha[c[t]] + x[t, ] * beta + z[t];
    for (i in 1:p) {
      nu += phi[i] * y[t - i];
    }
    for (j in 1:q) {
      nu += theta[j] * noise[t - j];
    }
    yhat[t] = nu + noise[t];
  }
}
