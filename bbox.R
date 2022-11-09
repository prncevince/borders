library(data.table)
library(dplyr)
library(sf)
library(tictoc)

#' @param lon1 bottom latitude [-180, 180]
#' @param lon2 top latitude [-180, 180]
#' @param lat1 bottom latitude [-90, 90]
#' @param lat2 top latitude [-90, 90]
bbox_d <- function(lon1, lon2, lat1, lat2, crs = 4326) {
  d_bbox <- data.table(
    st_p_tl = st_sfc(list(st_point(c(lon1, lat2)))),
    st_p_tr = st_sfc(list(st_point(c(lon2, lat2)))),
    st_p_br = st_sfc(list(st_point(c(lon2, lat1)))),
    st_p_bl = st_sfc(list(st_point(c(lon1, lat1))))
  )
  d_bbox[,
    st_sfc_mp_box := st_sfc(
      list(c(st_p_tl[[1]], st_p_tr[[1]], st_p_br[[1]], st_p_bl[[1]], st_p_tl[[1]])),
      crs = crs
    )
  ]
  d_bbox[,
    st_sfc_ls_box := st_sfc(
      list(st_linestring(st_sfc_mp_box[[1]])), crs = crs
    )
  ]
  d_bbox[, st_sfc_poly := list(st_polygonize(st_sfc_ls_box))]
  return(d_bbox)
}

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

# filter  with bbox ----
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