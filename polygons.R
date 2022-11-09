library(dplyr)
library(sf)

source('bbox.R')

# load boundary dataset - natural earth ----
d_sf_ne <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")

d_sf_kn <- d_sf_ne |>
  filter(geounit == "North Korea") %>%
  select(geometry)

d_bbox_kn <- bbox_d(121, 134, 37, 43.5)

# validate boundary dataset ----
tic()
d_sf_ne_valid <- d_sf_ne |> st_make_valid()
toc()

# filter MULTIPOLYGONs - 1 per country ----
sfc_kn_poly <- d_sf_kn$geometry |>
  lapply(function(l) lapply(l, st_polygon)) |>
  unlist(recursive = F) |> st_sfc(crs = 4326)

d_sf_kn_bb <- d_sf_ne_valid %>%
  filter(
    st_intersects(x = ., y = d_bbox_kn$st_sfc_poly, sparse = FALSE)
  ) |>
  select(geounit, geometry)

# create & filter individual POLYGONs - multiple per country ----
tic()
d_kn_bb_poly <- data.frame()
n <- d_sf_kn_bb |> select(geometry, geounit) |> nrow()
for(i in 1:n) {
  r <- d_sf_kn_bb |> select(geometry, geounit) %>% `[`(i, )
  d_kn_bb_poly <- rbind(
    d_kn_bb_poly,
    data.frame(
      geounit = r$geounit,
      geometry = lapply(r$geometry[[1]], st_polygon) |> st_sfc(crs = 4326),
      stringsAsFactors = F
    )
  )
}
i_bb <- d_kn_bb_poly$geometry %>%
  st_intersects(x = ., y = d_bbox_kn$st_sfc_poly, sparse = FALSE)
d_kn_bb_poly <- d_kn_bb_poly[i_bb,]
st_geometry(d_kn_bb_poly) <- "geometry"
toc()

