# For UPDATED simulation
data.frame(
  error = rep(c("a", "b", "c", "d", "e"), each = 3),
  cens_pct = rep(c("50%", "30%", "15%"), 5),
  C_UB = c(11, 17, 30, 11, 17, 30, 15, 60, 200, 50, 190, 800, 17, 45, 100)
) %>%
  dplyr::arrange(desc(cens_pct), error) %>%
  View()

# For NEW simulation
data.frame(
  error = rep(c("a", "b", "c", "d"), each = 3),
  cens_pct = rep(c("50%", "30%", "15%"), 4),
  C_UB = c(30, 100, 300, 75, 600, 10000, 200, 20000, 20000, 21, 70, 250)
) %>%
  dplyr::arrange(desc(cens_pct), error) %>%
  View()
