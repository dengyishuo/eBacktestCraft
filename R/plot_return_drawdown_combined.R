#' Combined Plot: Cumulative Return + Drawdown (Two-Panel)
#'
#' Stacks the cumulative return curve (upper panel) and the drawdown curve
#' (lower panel) with independent y-axes. Requires the \pkg{patchwork} package.
#'
#' @param perf_result List returned by \code{performance_analysis()}, or a
#'   data frame containing columns \code{date}, \code{cum_return}, and
#'   \code{drawdown}.
#' @param height_ratio Numeric vector of length 2 controlling the relative
#'   height of the two panels. Default \code{c(3, 1)}.
#' @param title Character. Overall plot title. Default
#'   \code{"Cumulative Return & Drawdown"}.
#'
#' @return A patchwork composite ggplot object, or (if \pkg{patchwork} is not
#'   installed) only the cumulative return panel with a message.
#' @export
#' @importFrom ggplot2 ggplot aes geom_area geom_hline scale_y_continuous labs
#' @importFrom scales percent_format
#'
#' @examples
#' \dontrun{
#' perf <- performance_analysis(res)
#' plot_return_drawdown_combined(perf)
#' }
plot_return_drawdown_combined <- function(perf_result,
                                          height_ratio = c(3, 1),
                                          title = "Cumulative Return & Drawdown") {
  df <- if (is.data.frame(perf_result)) {
    perf_result
  } else {
    perf_result$daily_details
  }
  df$date <- as.Date(df$date)
  df$pct <- df$cum_return - 1

  p_ret <- ggplot2::ggplot(df, ggplot2::aes(x = .data$date, y = .data$pct)) +
    ggplot2::geom_area(fill = "#E63946", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(title = title, x = NULL, y = "Cumulative Return") +
    theme_quant()

  p_dd <- ggplot2::ggplot(df, ggplot2::aes(x = .data$date, y = .data$drawdown)) +
    ggplot2::geom_area(fill = "#2A9D8F", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(x = NULL, y = "Drawdown") +
    theme_quant()

  if (requireNamespace("patchwork", quietly = TRUE)) {
    p_ret / p_dd + patchwork::plot_layout(heights = height_ratio)
  } else {
    message("Install the 'patchwork' package to enable the two-panel plot layout; only the cumulative return curve will be returned for now.")
    p_ret
  }
}
