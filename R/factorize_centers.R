factorize_centers <- function(x) {
  factor(x, levels = centers_table[["id"]], labels = centers_table[["center"]])
}
