# Screener to select certain variables.
screen.select_vars = function(Y, X, vars = NULL, ...)  {
  return(colnames(X) %in% vars)
}