random_in_unit_sphere <- function() {
  while (TRUE) {
    v <- runif(3, -1, 1)
    if (sum(v^2) < 1) return(v)
  }
}

random_in_unit_disk <- function() {
  while (TRUE) {
    v <- c(runif(1, -1, 1), runif(1, -1, 1), 0)
    if (sum(v^2) < 1) return(v)
  }
}

random_unit_vector <- function() {
  vec_unit(random_in_unit_sphere())
}
