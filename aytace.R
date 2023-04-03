source("vec.R")
source("ray.R")
source("color.R")
source("hittable.R")
source("camera.R")

# Image
aspect_ratio <- 16.0 / 9.0
image_width <- 400
image_height <- image_width / aspect_ratio

# Camera
viewport_height <- 2.0
viewport_width <- aspect_ratio * viewport_height
focal_length <- 1.0
samples <- 25

origin <- c(0, 0, 0)
horizontal <- c(viewport_width, 0, 0)
vertical <- c(0, viewport_height, 0)
lower_left_corner <- origin - horizontal/2 - vertical/2 - c(0, 0, focal_length)

img <- matrix(nrow=image_height - 1, ncol=image_width - 1)

world <- hittable_list(
  sphere(c(0, 0, -1), 0.5),
  sphere(c(0, -100.5, -1), 100)
)

ray_color <- function(ray) {
  hit <- world(ray, 0, Inf)
  if (!is.null(hit <- world(ray, 0, Inf))) {
      0.5 * (hit$normal + c(1, 1, 1))
  } else {
    unit_direction <- vec_unit(ray$dir)
    t <- 0.5*(unit_direction[2] + 1.0)
    (1.0-t)*c(1.0, 1.0, 1.0) + t*c(0.5, 0.7, 1.0)
  }
}

for (j in seq(image_height-1, 0, -1)) {
  cat("\rScanlines remaining:", j, "")
  for (i in 0:(image_width-1)) {
    pixel <- c(0, 0, 0)
    for (s in 1:samples) {
      u <- (i + runif(1)) / (image_width-1)
      v <- (j + runif(1)) / (image_height-1)
      pixel <- pixel + ray_color(new_ray(origin, lower_left_corner + u*horizontal + v*vertical - origin))
    }
    img[image_height - 1 - j,i] <- to_rgb(pixel/samples)
  }
}

grid::grid.raster(img)
