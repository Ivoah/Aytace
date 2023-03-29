source("ray.R")

filename <- "output.ppm"
image_width <- 256
image_height <- 256

write_color <- function(color) {
  cat(as.integer(color[1]*255.999), as.integer(color[2]*255.999), as.integer(color[3]*255.999), "\n", file=filename, append=TRUE)
}

cat("P3\n", image_width, ' ', image_height, "\n255\n", file=filename)

for (j in seq(image_height-1, 0, by=-1)) {
  cat("\rScanlines remaining:", j, "")
  for (i in 0:(image_height-1)) {
    r <- i/(image_width - 1)
    g <- j/(image_height-1)
    b <- 0.25
    write_color(c(r, g, b))
  }
}
cat("\nDone\n")
