new_ray <- function(origin, direction) {
  list(orig=origin, dir=direction)
}

ray_at <- function(ray, t) {
  ray$orig + t*ray$dir
}

hit_sphere <- function(center, radius, ray) {
  oc <- ray$orig - center
  a <- (ray$dir %*% ray$dir)[1]
  b <- 2.0 * (oc %*% ray$dir)[1]
  c <- (oc %*% oc)[1] - radius*radius
  discriminant <- b*b - 4*a*c
  if (discriminant < 0) -1
  else (-b - sqrt(discriminant) ) / (2.0*a)
}
