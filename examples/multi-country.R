library(sf)
library(tictoc)
source('bbox.R')

d_bbox_world <- bbox_d(-180, 180, -90, 90)
d_bbox_canada <- bbox_d(-145, -50, 40, 89)

# Actual polygon object:
d_bbox_world$st_sfc_poly[[1]][[1]]
class(d_bbox_world$st_sfc_poly[[1]][[1]])
# POLYGON ((-90 180, 90 180, 90 -180, -90 -180, -90 180))
# [1] "XY"      "POLYGON" "sfg"

sf::st_bbox(d_bbox_world$st_sfc_poly)
# xmin ymin xmax ymax 
# -90 -180   90  180 

# filter with bbox ----
tic()
d_sf_ne <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
toc()

d_canada_germany <- d_sf_ne %>% filter(geounit %in% c("Canada", "Germany"))

# works
tic()
i_bbox_canada <- d_canada_germany$geometry %>% 
  st_intersects(x = ., y = d_bbox_canada$st_sfc_poly, sparse = FALSE)
d_canada_bb <- d_canada_germany[i_bbox_canada, ]
toc()
# does not work - works for POINTs
tic()
d_canada_bb <- d_canada_germany %>% 
  filter(
    st_within(x = ., y = d_bbox_canada$st_sfc_poly[[1]][[1]], sparse = FALSE)
  )
toc()