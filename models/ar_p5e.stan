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
}
parameters {
  real<lower = 0> sigma;              // standard deviation of noise
  real delta0;                        // impact of intervention
  real mu;                            // intercept
  real<lower = -1, upper = 1> phi[p]; // AR(p) coefficients
  real<lower = 0, upper = 5> lambda;  // Gompertz growth rate
  real<lower = 1> d;                  // Gompertz displacement along t
}
model {
  // priors
  mu ~ normal(8, 4); // the average is 8 mil pvs/day
  sigma ~ cauchy(0, 5);
  delta0 ~ normal(0, 10);
  phi ~ cauchy(0, 1);
  lambda ~ normal(0, 2);
  d ~ normal(7, 5);
  // likelihood
  for (t in (p + 1):N) {
    real z = 0;
    real nu = mu;
    if (t >= T) {
      z += gompertz(t - T, delta0, d, lambda);
    }
    nu += z;
    for (i in 1:p) {
      nu += phi[i] * y[t - i];
    }
    y[t] ~ normal(nu, sigma);
  }
}
generated quantities {
  real z[N];
  real yhat[N];
  for (t in 1:N) {
    if (t >= T) {
      z[t] = gompertz(t - T, delta0, d, lambda);
    } else {
      z[t] = 0;
    }
  }
  for (t in 1:p) {
    yhat[t] = normal_rng(mu + z[t], sigma);
  }
  for (t in (p + 1):N) {
    real nu = mu + z[t];
    for (i in 1:min(t - 1, p)) {
      nu += phi[i] * y[t - i];
    }
    yhat[t] = normal_rng(nu, sigma);
  }
}
