library(data.table)
library(ggplot2)
library(scattermore)
library(sf)
library(tictoc)
source('bbox.R')

# set a region of interest ----
d_bbox_kn <- bbox_d(121, 134, 37, 43.5)
bbox_kn <- st_bbox(d_bbox_kn$st_sfc_poly)

# load boundary dataset - natural earth ----
d_sf_ne <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")

# create equidistant dataset ~5 million lat/lons ----
tic()
n_points <- 5*1e6 # 5 million
n_lon <- (180*2/10)+1
n_lat <- (90*2/10)+1
n_grid_10deg <- n_lon*n_lat
mult_5mil <- ceiling(sqrt(n_points/n_grid_10deg))
lon <- seq(bbox_kn$xmin, bbox_kn$xmax, length.out = n_lon*mult_5mil)
lat <- seq(bbox_kn$ymin, bbox_kn$ymax, length.out = n_lat*mult_5mil)
d_sf_big <- st_as_sf(
  expand.grid(lon=lon, lat=lat), remove = FALSE,
  coords = c("lon", "lat"), crs = 4326
)
d_sf_big$id <- 1:nrow(d_sf_big)
toc()

# validate boundary dataset ----
tic()
d_sf_ne_valid <- d_sf_ne |> st_make_valid()
toc()

# gather only countries within region of interest ----
tic()
d_sf_kn_bbox <- d_sf_ne_valid %>%
  filter(
    st_intersects(x = ., y = d_bbox_kn$st_sfc_poly, sparse = FALSE)
  ) |>
  select(geounit, geometry)
toc()

# calculate distance to border ----
tic()
d_sf_big$dist_to_border <- st_distance(
  d_sf_big, d_sf_kn_bbox |> st_cast("MULTILINESTRING")
) |> apply(1, min)
toc()

map_kn_bb <- ggplot() +
  geom_sf(data = d_sf_ne) +
  coord_sf(
    xlim = bbox_kn[c("xmin", "xmax")], ylim = bbox_kn[c("ymin", "ymax")]
  )

# colored by distance - log scale base 10 ----
tic()
ggplot() +
  geom_sf(data = d_sf_ne) +
  coord_sf(
    xlim = bbox_kn[c("xmin", "xmax")], ylim = bbox_kn[c("ymin", "ymax")]
  ) +
  geom_scattermore(
    data = d_sf_big,
    aes(x = lon, y = lat, color = dist_to_border), pointsize = 0.25
  ) +
  scale_color_viridis_c(
    trans = "log10", direction = -1, 
    guide = guide_colorbar(label.theme = element_text(angle = 0))
  ) +
  labs(title = "Distance to Nearest Border - Log Scale", color = "Meters - Log Scale") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.90, 0.20)
  )
toc()

# colored by distance - linear ----
tic()
ggplot() +
  geom_sf(data = d_sf_ne) +
  coord_sf(
    xlim = bbox_kn[c("xmin", "xmax")], ylim = bbox_kn[c("ymin", "ymax")]
  ) +
  geom_scattermore(
    data = d_sf_big,
    aes(x = lon, y = lat, color = dist_to_border), pointsize = 0.25
  ) +
  scale_color_viridis_c(
    direction = -1, 
    guide = guide_colorbar(label.theme = element_text(angle = 0))
  ) +
  labs(title = "Distance to Nearest Border - Linear Scale", color = "Meters") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.90, 0.20)
  )
toc()