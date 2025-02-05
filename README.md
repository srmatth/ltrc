
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ltrc

R package for fitting left-truncated right-censored regression models
using a semiparametric spline approach.

## Software Package

### Installation

To install the `ltrc` R package, run the following code:

``` r
# install.packages("devtools")
devtools::install_github("srmatth/ltrc")
```

Currently the package is only available through GitHub, but we have
plans to make it available on the CRAN in the future.

### Basic Usage

The main function in the `ltrc` package is the `ltrc()` function, which
fits a semiparametric linear model on data subject to left-truncation
and right-censoring. See the example below:

``` r
## Load the libraries
library(ltrc)

## Simulate Data
true_beta <- 1
n <- 300
x <- matrix(runif(length(true_beta)*2*n, -3, 3), ncol = length(true_beta))
eps <- rnorm(2*n)
y <- x %*% true_beta + eps

## Induce left-truncation and censoring into our data
t <- runif(2*n, -6, 1)
obs_indx <- which(y > t)
sampled_indx <- sample(obs_indx, size = n)
c <- runif(2*n, 1, 7)
delta <- as.numeric(y < c)
y_cens <- ifelse(y < c, y, c)

y_obs <- y_cens[sampled_indx]
x_obs <- x[sampled_indx,]
t_obs <- t[sampled_indx]
delta_obs <- delta[sampled_indx]
c_obs <- c[sampled_indx]
true_y_obs <- y[sampled_indx]

dat <- data.frame(
  y_obs, delta_obs, x_obs
)
```

``` r
## Fit the Model
res <- ltrc(
  formula = survival::Surv(y_obs, delta_obs) ~ x_obs, 
  t_obs, 
  data = dat, 
  n_start = 10, 
  int_knots = 1,
  sd_multiplier = 3, 
  verbose = FALSE, 
  diag_only = TRUE, 
  return_all = FALSE
)
```

``` r
## Clean and Summarize the Model
clean_res <- get_clean_model(res)
summary(clean_res)
#> Summary of ltrc_mod object
#> --------------------------
#> Model Parameters:
#>   Beta Coefficients:
#> [1] 0.992372
#>   Gamma Coefficients:
#> [1] -61.9380272  -5.4016057  -1.6649926   1.0856196   0.6504792
#> 
#> Interior Knots:
#> [1] 0.08582515
#> Boundary Knots:
#> [1] -8.652591  2.958868
#> 
#> Model Metrics:
#>   Log-Likelihood: -389.7480
#>   Log-Likelihood (Start): -679.0375
#>   Number of Iterations: 22
#>   Converged: Yes
#> 
#> Data Summary:
#>   Number of Predictors: 1
#>   Number of Observed Responses: 300
#> 
#> Residuals Summary:
#>   Min: -2.9755, 1st Qu.: -0.5987, Median: 0.0877, Mean: 0.0889, 3rd Qu.: 0.8060, Max: 0.8060
#>   Standard Deviation: 1.0288
```

``` r
## Predict on New Data
predict(clean_res, newdata = data.frame(x = c(1.5, 0.8, 2.4)))
#> [1] 1.4885581 0.7938976 2.3816929
```

``` r
## Put a 95% confidence interval on the parameter estimate
## Using the square of the efficient score function (Theoretically supported)
se_beta_1 <- vcov(clean_res, type = "efficient_score") %>% diag() %>% sqrt()
clean_res$parameters$beta + c(-1, 1) * qnorm(0.975) * se_beta_1
#> [1] 0.911397 1.073347

## Using the inverse of the information matrix for all the parameters (empirically supported)
se_beta_1 <- vcov(clean_res, type = "inverse_information") %>% diag() %>% sqrt() %>% `[`(1)
clean_res$parameters$beta + c(-1, 1) * qnorm(0.975) * se_beta_1
#> [1] 0.9004055 1.0843386
```

Note that the current output the the `ltrc()` function is quite raw. We
have left it as such in order to maintain the reproducibility of the
code used for the paper. However, using the `get_clean_model()` function
will solve this problem and allows the user to pass the newly cleaned
object (of class `"ltrc_mod"`) to common R functions such as `predict()`
and `summary()`. This process will be refined as the package is prepared
for submission to CRAN.

Further note that any transformations applied to the response variable
must also be applied to the left truncation time BEFORE being passed to
the function.

## Paper Code

The `inst/script` directory holds all the code used for simulation
studies, plots, and data analysis in the paper *Regression for
Left-Truncated and Right-Censored Data: A Semiparametric Sieve
Likelihood Approach*, by Spencer Matthews and Bin Nan. Simulations can
be run on a server using the file `05_simulation_on_server.R`, assuming
the necessary packages are installed. In general, most file names in
this directory are self-explanatory.

Some scripts will not run due to the lack of data in this GitHub
repository. If you wish to access the data from the 90+ study as used in
the paper, please reach out to the authors and we can direct you to the
correct point of contact. And of course running scripts meant to analyze
simulation results will not run unless a simulation has been performed
and stored in the appropriate directory.
