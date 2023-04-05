library(pracma)

source("vec.R")
source("ray.R")
source("color.R")
source("hittable.R")
source("random.R")

# Image
aspect_ratio <- 16.0 / 9.0
image_width <- 128
image_height <- image_width / aspect_ratio

# Camera
viewport_height <- 2.0
viewport_width <- aspect_ratio * viewport_height
focal_length <- 1.0
samples <- 10
max_depth <- 5

origin <- c(0, 0, 0)
horizontal <- c(viewport_width, 0, 0)
vertical <- c(0, viewport_height, 0)
lower_left_corner <- origin - horizontal/2 - vertical/2 - c(0, 0, focal_length)

img <- matrix(nrow=image_height - 1, ncol=image_width - 1)

# world <- hittable_list(
#   sphere(c(0, 0, -1), 0.5, c(1, 0, 0)),
#   sphere(c(0, -100.5, -1), 100, c(0, 1, 0))
# )
red <- c(0.8, 0, 0)
green <- c(0, 0.8, 0)
black <- c(0, 0, 0)

v2 <- c(1, 0, -2)
v1 <- c(-1, 0, -2)
v3 <- c(0, 1, -2)

world <- hittable_list(
  sphere(c(0, -100.5, -1), 100, green),
  sphere(v1, 0.1, black),
  sphere(v2, 0.1, black),
  sphere(v3, 0.1, black),
  triangle(v1, v2, v3, red)
)

ray_color <- function(ray, world, depth = max_depth) {
  if (depth <= 0) return(c(0, 0, 0))
  
  hit <- world(ray, 0.001, Inf)
  if (!is.null(hit <- world(ray, 0, Inf))) {
    target <- hit$point + hit$normal + random_unit_vector()
    hit$color * ray_color(new_ray(hit$point, target - hit$point), world, depth - 1);
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
      ray <- new_ray(origin, lower_left_corner + u*horizontal + v*vertical - origin)
      pixel <- pixel + ray_color(ray, world)
    }
    img[image_height - 1 - j,i] <- to_rgb(pixel/samples)
  }
}

grid::grid.raster(img)
