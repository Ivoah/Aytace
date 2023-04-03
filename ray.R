new_ray <- function(origin, direction) {
  list(origin=origin, dir=direction)
}

ray_at <- function(ray, t) {
  ray$origin + t*ray$dir
}
