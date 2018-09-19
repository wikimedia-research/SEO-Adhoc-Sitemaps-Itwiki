functions {
    real gompertz(real t, real a, real b, real c) {
        return (a * exp(-b * exp(-c * t)));
    }
}
data {
  int<lower = 1> N;       // number of observations
  real y[N];              // observed time series
  int<lower = 1> T;       // time of sitemaps deployment
  int<lower = 1> p;       // number of AR terms
  int<lower = 1> q;       // number of MA terms
  int<lower = 1> K;       // number of continuous predictors
  matrix[N, K] x;         // predictor matrix of continuous variables, incl. t for linear trend
  int<lower = 1> M;       // number of categorical predictors
  int<lower = 1> J[M];    // number of levels of each m-th categorical predictor
  int<lower = 1> J_max;   // largest number of categories among M predictors
  int<lower = 1> c[N, M]; // predictor matrix of categorical variables
}
parameters {
  real<lower = 0> sigma;                // standard deviation of noise
  real delta0;                          // impact of intervention
  real<lower = 0> tau;                  // std dev of delta0
  real<lower = -1, upper = 1> phi[p];   // AR(p) coefficients
  real<lower = -1, upper = 1> theta[q]; // MA(q) coefficients
  vector[K] beta;                       // coefficients for continuous predictors
  vector[M] alpha0;                     // centers of the M predictors, for better sampling
  matrix[M, J_max] alpha_change;        // differentials for M categorical predictors
  real<lower = 0, upper = 5> lambda;    // Gompertz growth rate
  real<lower = 1> d;                    // Gompertz displacement along t
}
transformed parameters {
  matrix[M, J_max] alpha; // coefficients for M categorical predictors
  for (m in 1:M) {
    for (j in 1:(J[m])) {
      alpha[m, j] = alpha0[m] + alpha_change[m, j];
    }
  }
}
model {
  vector[N] epsilon; // error/noise at time t
  // priors
  for (m in 1:M) {
    alpha0[m] ~ normal(0, 10);
    for (j in 1:(J[m])) {
      alpha_change[m, j] ~ normal(0, 10);
    }
  }
  sigma ~ cauchy(0, 5);
  tau ~ cauchy(0, 5);
  delta0 ~ normal(0, tau);
  beta ~ normal(0, 10);
  phi ~ cauchy(0, 1);
  theta ~ cauchy(0, 1);
  lambda ~ normal(0, 2);
  d ~ normal(7, 5);
  // likelihood
  for (t in 1:max(p, q)) {
    epsilon[t] = 0;
  }
  for (t in (max(p, q) + 1):N) {
    real z = 0;
    real nu = x[t, ] * beta;
    for (m in 1:M) {
      nu += alpha[m, c[t, m]];
    }
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
    real nu = x[t, ] * beta;
    for (m in 1:M) {
      nu += alpha[m, c[t, m]];
    }
    yhat[t] = nu + z[t];
  }
  for (t in (max(p, q) + 1):N) {
    real nu = x[t, ] * beta + z[t];
    for (m in 1:M) {
      nu += alpha[m, c[t, m]];
    }
    for (i in 1:p) {
      nu += phi[i] * y[t - i];
    }
    for (j in 1:q) {
      nu += theta[j] * noise[t - j];
    }
    yhat[t] = nu + noise[t];
  }
}
