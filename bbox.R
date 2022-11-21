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
