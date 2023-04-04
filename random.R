random_in_unit_sphere <- function() {
  while (TRUE) {
    p <- runif(3)
    if (sum(p^2) < 1) return(p)
  }
}

random_unit_vector <- function() {
  vec_unit(random_in_unit_sphere())
}
