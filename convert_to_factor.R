convert_to_factor <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.numeric(x)) {
      return(as.factor(x))
    } else {
      return(x)
    }
  })
  return(df)
}