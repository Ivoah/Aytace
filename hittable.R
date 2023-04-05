new_hit <- function(point, outward_normal, t, ray, color) {
  front_face <- (ray$dir %*% outward_normal)[1] < 0
  list(
    point=point,
    normal=if (front_face) outward_normal else -outward_normal,
    t=t,
    front_face=front_face,
    color=color
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

sphere <- function(center, radius, color) {
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
    new_hit(point, (point - center)/radius, root, ray, color)
  }
}

triangle <- function(v1, v2, v3, color) {
  function(ray, t_min, t_max) {
    edgeAB <- v2 - v1
    edgeAC <- v3 - v1
    normalVector <- cross(edgeAB, edgeAC)
    ao <- ray$origin - v1
    dao <- cross(ao, ray$dir)
    determinant <- -ray$dir %*% normalVector
    
    t <- (ao %*% normalVector)/determinant
    u <- (edgeAC %*% dao)/determinant
    v <- -(edgeAB %*% dao)/determinant
    w <- 1 - u - v
    
    if (determinant > 1e-6 && t >= t_min && t <= t_max && u >= 0 && v >= 0 && w >= 0) {
      new_hit(ray_at(ray, t[1]), normalVector, t[1], ray, color)
    } else NULL
  }
}
