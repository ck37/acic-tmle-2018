#------------- bound function --------------
bound <- function(x, bound_var) {
  max_val = max(bound_var, na.rm = TRUE)
  min_val = min(bound_var, na.rm = TRUE)
  x[x > max_val] <- max_val
  x[x < min_val] <- min_val
  x
}
