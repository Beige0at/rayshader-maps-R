library(sf)
library(tidyverse)
library(stars)
library(rayshader)
library(MetBrewer)
library(colorspace)
library(units)
library(rgl)

# load Kontur Data
data <- st_read("data/kontur_population_IN_20220630.gpkg")

# load GADM data For State
state_level_map <- raster::getData("GADM", country = "India", level = 1) %>%
  st_as_sf() %>%
  filter(NAME_1 == "NCT of Delhi")

# fix crs for intersection
state_level_map <- st_transform(state_level_map, crs= st_crs(data))

# Plot to check map
state_level_map %>%
  ggplot()+
  geom_sf()

# Intersect data
st_intersected <- st_intersection(data, state_level_map)

# Define aspect ratio based on bounding box
bb <- st_bbox(state_level_map)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) %>% 
  st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(data))

# Check my plotting
state_level_map %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red")

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)

# handle conditions of width or height being the longer side

if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- width / height
}

# convert to raster so we can then convert to matrix

size <- 1000

w_ratio <- drop_units(w_ratio)
h_ratio <- drop_units(h_ratio)

state_rast <- st_rasterize(data,
                           nx = floor(size * w_ratio),
                           ny = floor(size * h_ratio))

mat <- matrix(state_rast$population, 
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))

# create color palette

c1 <- met.brewer("OKeeffe2")
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 2)(256)
swatchplot(texture)


# plot that 3d thing!

mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          zscale = 300,
          solid = TRUE,
          solidcolor = "beige",
          shadowdepth = 0)

render_camera(theta = -5, phi = 50, zoom = 0.6)

outfile <- "images/India2.png"


{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  render_highquality(
    filename = outfile,
    interactive = FALSE,
    lightdirection = 95,
    lightaltitude = c(45, 80),
    lightcolor = c(c1[2], "beige"),
    lightintensity = c(600, 100),
    samples = 450,
    width = 6000,
    height = 6000
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}
