functions {
  // in R: cumsum(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  int day2month(int day) {
    if (day >= 1 && day <= 31) {
      return 1; // first 31 days are January
    } else if (day > 31 && day <= 59) {
      return 2; // next 28 days are Febrary
    } else if (day > 59 && day <= 90) {
      return 3; // next 31 days are March
    } else if (day > 90 && day <= 120) {
      return 4; // next 30 days are April
    } else if (day > 120 && day <= 151) {
      return 5; // next 31 days are May
    } else if (day > 151 && day <= 181) {
      return 6; // next 30 days are June
    } else if (day > 181 && day <= 212) {
      return 7; // next 31 days are July
    } else if (day > 212 && day <= 243) {
      return 8; // next 31 days are August
    } else if (day > 243 && day <= 273) {
      return 9; // next 30 days are September
    } else if (day > 273 && day <= 304) {
      return 10; // next 31 days are October
    } else if (day > 304 && day <= 334) {
      return 11; // next 30 days are November
    } else if (day > 334 && day <= 365) {
      return 12; // next 31 days are December
    } else {
      return 0; // shouldn't be the case
    }
  }
}
data {
  int<lower = 1> N;                // number of observations
  real y[N];                       // observed time series
  int<lower = 1> T;                // time of sitemaps deployment
  int<lower = 1> p;                // number of AR terms
  int<lower = 1> q;                // number of MA terms
  int<lower = 1> K;                // number of continuous predictors
  matrix[N, K] x;                  // incl. t for linear trend and 6 dummies for days of the week
  int<lower = 1, upper = 365> D;   // maximum number of days of the year observed, up to 365
  int<lower = 1, upper = 12> M;    // maximum number of months of the year observed, up to 12
  int<lower = 1, upper = D> yd[N]; // day of the year (use day2month for mapping yd to md)
}
parameters {
  real<lower = 0> sigma;                // standard deviation of noise
  real delta0;                          // impact of intervention
  real<lower = 0> tau;                  // std dev of delta0
  real<lower = -1, upper = 1> phi[p];   // AR(p) coefficients
  real<lower = -1, upper = 1> theta[q]; // MA(q) coefficients
  vector[K] beta;                       // coefficients for continuous predictors
  real<lower = 0> gamma_mean;           // overall mean around which the month means are centered
  real gamma_offset[M];                 // differentials
  real<lower = 0> gamma_stddev;         // standard deviation of the month intercepts
  real<lower = 0> alpha[D];             // day-of-year means, sharing info w/ other days in same month
  real<lower = 0> alpha_stddev[M];      // standard deviation of day-of-year intercepts, per month
  real<lower = 0, upper = 1> omega1;    // gradation coefficient
}
transformed parameters {
  real gamma[M]; // month means, sharing information with other months
  for (i in 1:M) {
    gamma[i] = gamma_mean + gamma_stddev * gamma_offset[i];
  }
}
model {
  vector[N] epsilon; // error/noise at time t
  // priors
  sigma ~ cauchy(0, 5);
  tau ~ cauchy(0, 5);
  delta0 ~ normal(0, tau);
  beta ~ normal(0, 10);
  phi ~ cauchy(0, 1);
  theta ~ cauchy(0, 1);
  omega1 ~ beta(9, 3); // expected value around 0.75, with most of prob 0.6-1.0
  gamma_mean ~ normal(0, 10);
  gamma_stddev ~ cauchy(0, 5);
  alpha_stddev ~ cauchy(0, 5);
  for (d in 1:D) {
    alpha[d] ~ normal(gamma[day2month(d)], alpha_stddev[day2month(d)]);
  }
  for (m in 1:M) {
    gamma_offset[m] ~ normal(0, 1);
  }
  // likelihood
  for (t in 1:max(p, q)) {
    epsilon[t] = 0;
  }
  for (t in (max(p, q) + 1):N) {
    real z = 0;
    real nu = alpha[yd[t]] + x[t, ] * beta;
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
  real limz = delta0 / (1 - omega1);
  for (t in 1:N) {
    noise[t] = normal_rng(0, sigma);
    if (t >= T) {
      z[t] = delta0 * (1 - (omega1 ^ (t - T + 1))) / (1 - omega1);
    } else {
      z[t] = 0;
    }
  }
  for (t in 1:max(p, q)) {
    real nu = alpha[yd[t]] + x[t, ] * beta;
    yhat[t] = nu + z[t];
  }
  for (t in (max(p, q) + 1):N) {
    real nu = alpha[yd[t]] + x[t, ] * beta + z[t];
    for (i in 1:p) {
      nu += phi[i] * y[t - i];
    }
    for (j in 1:q) {
      nu += theta[j] * noise[t - j];
    }
    yhat[t] = nu + noise[t];
  }
}
