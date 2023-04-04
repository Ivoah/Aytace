to_rgb <- function(v) {
  gamma <- 3
  v <- v^(1/gamma)
  rgb(v[1], v[2], v[3])
}
