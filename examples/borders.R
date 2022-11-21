library(ggplot2)
library(scattermore)
# source('bbox.R')
source('borders.R')
source('plot.R')
source('polygons.R')

# inner & outer border polygons ----
tic()
l_sfc_kn_10km_inout <- buffers_inner_outer(sfc_kn_poly, dist = 10000, max_cells = 10000)
toc()

tic()
l_sfc_bb_kn_10km_inout <- buffers_inner_outer(d_kn_bb_poly$geometry, dist = 10000, max_cells = 20000)
toc()

# regular plot function ----
plot_buffers(l_sfc_kn_10km_inout, d_sf_kn)
plot_buffers(l_sfc_bb_kn_10km_inout, d_sf_kn)

# ggplot - no borders ----
bbox_kn <- st_bbox(d_bbox_kn$st_sfc_poly)
map_kn_bb <- ggplot() +
  geom_sf(data = d_sf_ne) +
  coord_sf(
    xlim = bbox_kn[c("xmin", "xmax")], ylim = bbox_kn[c("ymin", "ymax")]
  )
# Large point observation ----
map_kn_bb +
  geom_scattermore(
    data = d_sf_point,
    aes(x = lon, y = lat, color = by), pointsize = 0.25
  )
# ggplot - borders ----
map_kn_bb_buf <- map_kn_bb
for (i in names(l_sfc_bb_kn_10km_inout)) {
  color <- "purple"
  if (i != "outer") color = "yellow"
  for (j in 1:length(l_sfc_bb_kn_10km_inout[[i]])) {
    if(! st_is_empty(l_sfc_bb_kn_10km_inout[[i]][j])) {
      map_kn_bb_buf <- map_kn_bb_buf +
        geom_sf(data = l_sfc_bb_kn_10km_inout[[i]][j], fill = "transparent", color = color)
    }
  }
}
# `coord_sf` must be added last
map_kn_bb_buf <- map_kn_bb_buf +
  coord_sf(
    xlim = bbox_kn[c("xmin", "xmax")], ylim = bbox_kn[c("ymin", "ymax")]
  )

