## getting variance and such for the other distributions
library(extRemes)

samp <- revd(n = 10000, loc = 0.87, scale = 0.2250791)
mean(samp)
var(samp)


error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/12))
  ep2 <- rnorm(x, 0, sqrt(3/4))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1
  return(err_vec)
}
samp <- error_dist(10000)
mean(samp)
var(samp)

error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/12))
  ep2 <- rnorm(x, -0.5, sqrt(1/48))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1.25
  return(err_vec)
}
samp <- error_dist(10000)
mean(samp)
var(samp)
plot(density(samp))


# For NEW simulation
data.frame(
  error = rep(c("b", "c", "d"), each = 3),
  cens_pct = rep(c("50%", "30%", "15%"), 3),
  C_UB = c(11, 17, 32, 13, 24, 50, 11, 18, 32)
) %>%
  dplyr::arrange(desc(cens_pct), error) %>%
  View()


