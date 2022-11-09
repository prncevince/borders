library(data.table)
library(sf)

#' @param lat1 bottom latitude [-90, 90]
#' @param lat2 top latitude [-90, 90]
#' @param lon1 bottom latitude [-180, 180]
#' @param lon2 top latitude [-180, 180]
bbox_d <- function(lat1, lat2, lon1, lon2, crs = 4326) {
  d_bbox <- data.table(
    st_p_tl = st_sfc(list(st_point(c(lat1, lon2)))),
    st_p_tr = st_sfc(list(st_point(c(lat2, lon2)))),
    st_p_br = st_sfc(list(st_point(c(lat2, lon1)))),
    st_p_bl = st_sfc(list(st_point(c(lat1, lon1))))
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

d_bbox_world <- bbox_d(-90, 90, -180, 180)

# Actual polygon object:
d_bbox_world$st_sfc_poly[[1]][[1]]
class(d_bbox_world$st_sfc_poly[[1]][[1]])
# POLYGON ((-90 180, 90 180, 90 -180, -90 -180, -90 180))
# [1] "XY"      "POLYGON" "sfg"

sf::st_bbox(d_bbox_world$st_sfc_poly)
# xmin ymin xmax ymax 
# -90 -180   90  180 