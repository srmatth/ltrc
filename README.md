
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
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

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

## Clean and Summarize the Model
clean_res <- get_clean_model(res)
class(clean_res)
#> [1] "ltrc_mod"
summary(clean_res)
#> Summary of ltrc_mod object
#> --------------------------
#> Model Parameters:
#>   Beta Coefficients:
#> [1] 1.015835
#>   Gamma Coefficients:
#> [1] -46.658054  -5.658228  -1.865497   1.209199   1.469397
#> 
#> Interior Knots:
#> [1] 0.1203628
#> Boundary Knots:
#> [1] -8.827919  3.138155
#> 
#> Model Metrics:
#>   Log-Likelihood: -386.5675
#>   Log-Likelihood (Start): -1146.0154
#>   Number of Iterations: 11
#>   Converged: Yes
#> 
#> Data Summary:
#>   Number of Predictors: 1
#>   Number of Observed Responses: 300
#> 
#> Residuals Summary:
#>   Min: -2.9382, 1st Qu.: -0.5653, Median: 0.1346, Mean: 0.1327, 3rd Qu.: 0.8942, Max: 0.8942
#>   Standard Deviation: 1.0218

## Predict on New Data
predict(clean_res, newdata = data.frame(x = c(1.5, 0.8, 2.4)))
#> [1] 1.523752 0.812668 2.438004
```

Note that the current output the the `ltrc()` function is quite raw. We
have left it as such in order to maintain the reproducibility of the
code used for the paper. However, using the `get_clean_model()` function
will solve this problem and allows the user to pass the newly cleaned
object (of class `"ltrc_mod"`) to common R functions such as `predict()`
and `summary()`. This process will be refined as the package is prepared
for submission to CRAN.

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
