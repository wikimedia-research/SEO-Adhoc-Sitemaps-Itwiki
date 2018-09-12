data {
  int<lower = 1> N;     // number of observations
  real y[N];          // observed (and differenced!) time series
  int<lower = 1> T;     // time of sitemaps deployment
}
parameters {
  real<lower = 0> sigma;
  real mu;
  real delta0;
}
model {
  // setup
  vector[N] diff;    // lag-1 differenced y
  vector[N] nu;      // predictions at t
  vector[N] z;       // change at t
  vector[N] epsilon; // errors at t
  // priors
  sigma ~ cauchy(0, 5);
  mu ~ normal(0, 5);
  delta0 ~ normal(0, 5);
  // likelihood
  z[1] = 0;
  epsilon[1] = 0;
  for (t in 2:N) {
    z[t] = delta0 * (t >= T ? 1 : 0);
    nu[t] = mu + z[t];
    epsilon[t] = y[t] - nu[t];
  }
  epsilon ~ normal(0, sigma);
}
generated quantities {
  real yhat[N]; // change at t
  yhat[1] = mu;
  for (t in 2:N) {
    yhat[t] = mu + delta0 * (t >= T ? 1 : 0);
  }
}
