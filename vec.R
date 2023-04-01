vec_length <- function(v) {
  sqrt(sum(v^2))
}

vec_unit <- function(v) {
  v/vec_length(v)
}
