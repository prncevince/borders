#' Taking an sf object of class sfc_POLYGON
#' returns corresponding inner & outer buffers of specified length and of
#' class sfc for each in a list
buffers_inner_outer <- function(sfc_poly, dist, max_cells, crs = NULL) {
  if (is.null(crs)) crs <- st_crs(sfc_poly)$epsg
  l <- list()
  for (i in 1:length(sfc_poly)) {
    l[[i]] <- sfc_poly[i] %>% poly2openlinestring()
  }
  sfc_ls <- st_sfc(crs = crs, l)
  sfc_buff <- sfc_ls %>% st_buffer(dist = dist, max_cells = max_cells)
  l_sfc_buff <- buff2inoutpolylist(sfc_buff)
  x <- lapply(l_sfc_buff, function(i) sum(st_area(i))) %>% unlist()
  ix <- order(x, decreasing = T)
  l <- list()
  l$inner <- inners(l_sfc_buff[ix]) %>% st_sfc(crs = crs)
  l$outer <- outers(l_sfc_buff[ix]) %>% st_sfc(crs = crs)
  return(l)
}

#' when we convert our polygon to a linestring - then we get both inside & outside
#' buffers with the CRS ... however - it seems that we cannot specify single sides
#' so manipulation is necessary to get both inner & outer buffers separately
poly2openlinestring <- function(sfc_poly) {
  p <- sfc_poly %>% st_cast("MULTIPOINT") %>% st_cast("POINT")
  i <- which.min(st_distance(p[-length(p)], p[-1], by_element = T))
  l <- sfc_poly[[1]][[1]][-i,] %>% st_linestring()
  return(l)
}

#' Also, by default, each buffer is within 1 polygon class object.
#' We can convert the buffer object to a list, where each item/index links to an
#' individual polygon, and for each item there are corresponding separate inner &
#' outer polygon buffers. 
#' Of course, depending on the length / region will depend if there are multiple buffer polygons.
#' For example, North Korea is made up of 1 country & 1 small island. 
#' When taking a 10km buffer, only an outer polygon is returned for the small island.
#' An inner is not created for such a small area. In addition, for some reason,
#' a small dot of a polygon is returned in the larger country buffer call, so 3 
#' polygon items are returned in the list.
buff2inoutpolylist <- function(sfc_buff) {
  crs <- st_crs(sfc_buff)$epsg
  lapply(
    sfc_buff,
    function(j) {
      lapply(j, function(i) st_polygon(list(i))) %>% st_sfc(crs = crs)
    }
  )
}

#' Get the outer buffers from the buffer sfc class list
outers <- function(l_sfc_buff) {
  lapply(l_sfc_buff, function(i) i[which.max(st_area(i))][[1]])
}

#' Get the inner buffers from the buffer sfc class list
inners <- function(l_sfc_buff) {
  lapply(
    l_sfc_buff,
    function(i) {
      x <- st_area(i)
      # 2nd largest should be the inner
      i[order(x, decreasing = T)[2]][[1]]
    }
  )
}
