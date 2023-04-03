new_ray <- function(origin, direction) {
  list(origin=origin, dir=direction)
}

ray_at <- function(ray, t) {
  ray$origin + t*ray$dir
}

hit_sphere <- function(center, radius, ray) {
  oc <- ray$origin - center
  a <- sum(ray$dir^2)
  half_b <- (oc %*% ray$dir)[1]
  c <- sum(oc^2) - radius^2
  discriminant <- half_b^2 - a*c
  if (discriminant < 0) -1
  else (-half_b - sqrt(discriminant)) / a
}
