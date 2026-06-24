#' Plot Equity Curve or Cumulative Return
#'
#' Visualise the strategy's NAV or percentage cumulative return over time.
#'
#' @param perf_result List returned by \code{performance_analysis()} (must
#'   contain \code{$daily_details}), or a tibble returned by
#'   \code{performance_analysis(result, what = "daily_details")}.
#' @param unit Character. \code{"nav"} (default) plots the total asset value;
#'   \code{"pct"} plots the cumulative return as a percentage (NAV - 1).
#' @param color Character. Line / fill colour. Default \code{"#E63946"}.
#' @param title Character. Plot title. \code{NULL} uses a sensible default.
#'
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_area scale_y_continuous labs
#' @importFrom scales comma_format percent_format
#'
#' @examples
#' \dontrun{
#' perf <- performance_analysis(res)
#' plot_equity_curve(perf) # total asset (NAV)
#' plot_equity_curve(perf, unit = "pct") # cumulative return %
#' }
plot_equity_curve <- function(perf_result,
                              unit = c("nav", "pct"),
                              color = "#E63946",
                              title = NULL) {
  unit <- match.arg(unit)

  df <- if (is.data.frame(perf_result)) {
    perf_result
  } else {
    perf_result$daily_details
  }
  df$date <- as.Date(df$date)

  if (unit == "nav") {
    ttl <- if (is.null(title)) "Equity Curve" else title
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$date, y = .data$total_asset)) +
      ggplot2::geom_line(color = color, linewidth = 1) +
      ggplot2::scale_y_continuous(labels = scales::comma_format()) +
      ggplot2::labs(title = ttl, x = NULL, y = "Total Asset")
  } else {
    df$pct <- df$cum_return - 1
    ttl <- if (is.null(title)) "Cumulative Return" else title
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$date, y = .data$pct)) +
      ggplot2::geom_area(fill = color, alpha = 0.7) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      ggplot2::labs(title = ttl, x = NULL, y = "Cumulative Return")
  }

  p + theme_quant()
}
