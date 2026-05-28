#' Combined plot: Equity curve + Position ratio
#'
#' @param perf_result List, output from performance_analysis() function
#' @return ggplot object (arranged using ggpubr)
#' @export
#'
#' @examples
#' \dontrun{
#' perf <- performance_analysis(result)
#' plot_asset_position_combined(perf)
#' }
plot_asset_position_combined <- function(perf_result) {
  p1 <- plot_equity_curve(perf_result)
  p2 <- plot_position_ratio(perf_result)

  if (is.null(p2)) {
    return(p1)
  }

  ggpubr::ggarrange(p1, p2, ncol = 1, heights = c(2, 1), align = "v")
}
