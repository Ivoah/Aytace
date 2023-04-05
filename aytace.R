library(pracma)
library(stringr)

source("vec.R")
source("ray.R")
source("color.R")
source("hittable.R")
source("random.R")
source("camera.R")

# Image
aspect_ratio <- 16.0 / 9.0
image_width <- 400
image_height <- image_width / aspect_ratio

samples <- 16
max_depth <- 10

camera_ray <- new_camera(
  origin=c(-12, 4, 12),
  target=c(0, 2, 0),
  vup=c(0, 1, 0),
  vfov=20,
  aspect_ratio=aspect_ratio,
  aperture=0.1,
  background=c(0.7, 0.8, 1)
)

img <- matrix(nrow=image_height - 1, ncol=image_width - 1)

red <- c(0.8, 0, 0)
green <- c(0, 0.8, 0)
black <- c(0, 0, 0)

world <- hittable_list(
  sphere(c(0, -1000, 0), 1000, green),
  stl("dragon.stl", red)
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
    (1.0-t)*c(1.0, 1.0, 1.0) + t*c(0.7, 0.8, 1.0)
  }
}

print(Sys.time())
for (j in seq(image_height-1, 0, -1)) {
  cat("\rScanlines remaining:", j, "")
  for (i in 0:(image_width-1)) {
    pixel <- c(0, 0, 0)
    for (s in 1:samples) {
      u <- (i + runif(1)) / (image_width-1)
      v <- (j + runif(1)) / (image_height-1)
      ray <- camera_ray(u, v)
      pixel <- pixel + ray_color(ray, world)
    }
    img[image_height - 1 - j,i] <- to_rgb(pixel/samples)
  }
}
print(Sys.time())

grid::grid.raster(img)
