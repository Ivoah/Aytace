source("vec.R")
source("ray.R")
source("color.R")

# Image
aspect_ratio <- 16.0 / 9.0
image_width <- 400
image_height <- image_width / aspect_ratio

# Camera
viewport_height <- 2.0
viewport_width <- aspect_ratio * viewport_height
focal_length <- 1.0

origin <- c(0, 0, 0)
horizontal <- c(viewport_width, 0, 0)
vertical <- c(0, viewport_height, 0)
lower_left_corner <- origin - horizontal/2 - vertical/2 - c(0, 0, focal_length)

img <- matrix(nrow=image_height - 1, ncol=image_width - 1)

ray_color <- function(ray) {
  t <- hit_sphere(c(0,0,-1), 0.5, ray)
  if (t > 0) {
    n <- vec_unit(ray_at(ray, t) - c(0, 0, -1))
    return(to_rgb(0.5*(n + 1)))
  }
  unit_direction <- vec_unit(ray$dir)
  t = 0.5*(unit_direction[2] + 1.0)
  col <- (1.0-t)*c(1.0, 1.0, 1.0) + t*c(0.5, 0.7, 1.0)
  to_rgb(col)
}

for (j in seq(image_height-1, 0, -1)) {
  cat("\rScanlines remaining:", j, "")
  for (i in 0:(image_width-1)) {
    u <- i / (image_width-1)
    v <- j / (image_height-1)
    img[image_height - 1 - j,i] <- ray_color(new_ray(origin, lower_left_corner + u*horizontal + v*vertical - origin))
  }
}

grid::grid.raster(img)
