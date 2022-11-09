plot_buffers <- function(l_sfc_inout, sf) {
  plot(l_sfc_inout$outer, axes = TRUE)
  plot(l_sfc_inout$inner, add = TRUE)
  plot(sf, add = TRUE)
}