new_camera <- function(origin, target, vup, vfov, aspect_ratio, aperture, background) {
  viewport_height <- 2.0*tan(deg2rad(vfov)/2)
  viewport_width <- aspect_ratio*viewport_height

  w <- vec_unit(origin - target)
  u <- vec_unit(cross(vup, w))
  v <- vec_unit(cross(w, u))

  focus <- vec_length(target - origin)
  horizontal <- focus*viewport_width*u
  vertical <- focus*viewport_height*v
  lower_left_corner <- origin - horizontal/2 - vertical/2 - focus*w

  lens_radius <- aperture/2
  function(s, t) {
    rd <- lens_radius*random_in_unit_disk()
    offset <- u*rd[1] + v*rd[2]
    new_ray(origin + offset, lower_left_corner + s*horizontal + t*vertical - origin - offset)
  }
}
