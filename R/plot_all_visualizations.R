#' Plot All Strategy Visualizations
#'
#' Assembles a dashboard of six panels covering the full picture of a
#' strategy's performance: NAV, cumulative return, drawdown, daily returns,
#' monthly return heatmap, and return distribution. Requires \pkg{patchwork}.
#'
#' @param perf_result List returned by \code{performance_analysis()}.
#'
#' @return A patchwork composite ggplot, or (if \pkg{patchwork} is not
#'   installed) a 2×2 grid via \pkg{ggpubr} with the first four panels only.
#' @export
#'
#' @examples
#' \dontrun{
#' perf <- performance_analysis(res)
#' plot_all_visualizations(perf)
#' }
plot_all_visualizations <- function(perf_result) {
  p_nav <- plot_equity_curve(perf_result, unit = "nav")
  p_cum <- plot_equity_curve(perf_result, unit = "pct")
  p_dd <- plot_drawdown(perf_result)
  p_dr <- plot_daily_return(perf_result)
  p_monthly <- plot_monthly_return(perf_result)
  p_dist <- plot_return_dist(perf_result)

  if (requireNamespace("patchwork", quietly = TRUE)) {
    (p_nav | p_cum) /
      (p_dd | p_dr) /
      (p_monthly | p_dist) +
      patchwork::plot_annotation(
        title = "Strategy Performance Dashboard",
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
        )
      )
  } else {
    message("Install the 'patchwork' package to enable the full six-panel layout; the first four panels will be rendered via ggpubr for now.")
    ggpubr::ggarrange(
      p_nav, p_cum, p_dd, p_dr,
      ncol = 2, nrow = 2,
      labels = c("A", "B", "C", "D"),
      common.legend = FALSE
    )
  }
}
