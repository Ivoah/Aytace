new_hit <- function(point, outward_normal, t, ray) {
  front_face <- (ray$dir %*% outward_normal)[1] < 0
  list(
    p=point,
    normal=if (front_face) outward_normal else -outward_normal,
    t=t,
    front_face=front_face
  )
}

hittable_list <- function(...) {
  function(ray, t_min, t_max) {
    hit <- NULL
    closest_so_far <- t_max

    for (hit_fn in list(...)) {
      temp <- hit_fn(ray, t_min, closest_so_far)
      if (!is.null(temp)) {
        closest_so_far <- temp$t
        hit <- temp
      }
    }

    hit
  }
}

sphere <- function(center, radius) {
  function(ray, t_min, t_max) {
    oc <- ray$origin - center
    a <- sum(ray$dir^2)
    half_b <- (oc %*% ray$dir)[1]
    c <- sum(oc^2) - radius^2

    discriminant <- half_b^2 - a*c
    if (discriminant < 0) return(NULL)
    sqrtd <- sqrt(discriminant)

    # Find the nearest root that lies in the acceptable range.
    root <- (-half_b - sqrtd) / a
    if (root < t_min || t_max < root) {
      root <- (-half_b + sqrtd) / a
      if (root < t_min || t_max < root)
        return(NULL)
    }

    point <- ray_at(ray, root)
    new_hit(point, (point - center)/radius, root, ray)
  }
}
