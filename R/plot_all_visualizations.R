#' Plot all visualizations in a grid
#'
#' @param perf_result List, output from performance_analysis() function
#' @return ggplot object (arranged grid)
#' @export
#'
#' @examples
#' \dontrun{
#' perf <- performance_analysis(result)
#' plot_all_visualizations(perf)
#' }
plot_all_visualizations <- function(perf_result) {
  p1 <- plot_equity_curve(perf_result)
  p2 <- plot_cumulative_return(perf_result)
  p3 <- plot_drawdown(perf_result)
  p4 <- plot_daily_return(perf_result)

  ggpubr::ggarrange(
    p1, p2, p3, p4,
    ncol = 2, nrow = 2,
    labels = c("A", "B", "C", "D"),
    common.legend = FALSE
  )
}
