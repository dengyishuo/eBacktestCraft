#' Plot Monthly Return Heatmap
#'
#' Calendar heatmap of monthly returns. Each cell shows the return for one
#' month; rows are years, columns are months (Jan–Dec). Cells are coloured on
#' a red–white–green diverging scale centred at zero.
#'
#' @param perf_result List returned by \code{performance_analysis()}, or a
#'   data frame containing columns \code{date} and \code{daily_return}.
#' @param color_up Character. Colour for positive months.
#'   Default \code{"#E63946"}.
#' @param color_down Character. Colour for negative months.
#'   Default \code{"#2A9D8F"}.
#' @param title Character. Plot title. Default \code{"Monthly Return Heatmap"}.
#' @param digits Integer. Decimal places shown in each cell. Default \code{1}.
#'
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient2 scale_x_continuous scale_y_continuous labs theme element_text element_blank element_rect margin
#' @importFrom scales percent_format
#'
#' @examples
#' \dontrun{
#' perf <- performance_analysis(res)
#' plot_monthly_return(perf)
#' }
plot_monthly_return <- function(perf_result,
                                color_up = "#E63946",
                                color_down = "#2A9D8F",
                                title = "Monthly Return Heatmap",
                                digits = 1L) {
  df <- if (is.data.frame(perf_result)) {
    perf_result
  } else {
    perf_result$daily_details
  }
  df$date <- as.Date(df$date)

  # ── Aggregate to monthly returns ────────────────────────────────────────────
  df$year <- as.integer(format(df$date, "%Y"))
  df$month <- as.integer(format(df$date, "%m"))

  monthly <- stats::aggregate(
    daily_return ~ year + month,
    data = df,
    FUN  = function(x) prod(1 + x, na.rm = TRUE) - 1
  )

  # Label formatting: "2.3%" or "-1.4%"
  monthly$label <- paste0(
    ifelse(monthly$daily_return >= 0, "+", ""),
    round(monthly$daily_return * 100, digits), "%"
  )

  month_abbr <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
  monthly$month_label <- factor(month_abbr[monthly$month],
    levels = month_abbr
  )

  all_years <- sort(unique(monthly$year), decreasing = TRUE)
  monthly$year_label <- factor(monthly$year, levels = all_years)

  lim <- max(abs(monthly$daily_return), na.rm = TRUE)

  ggplot2::ggplot(
    monthly,
    ggplot2::aes(
      x = .data$month_label,
      y = .data$year_label,
      fill = .data$daily_return
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$label),
      size = 3, color = "white", fontface = "bold"
    ) +
    ggplot2::scale_fill_gradient2(
      low      = color_down,
      mid      = "white",
      high     = color_up,
      midpoint = 0,
      limits   = c(-lim, lim),
      labels   = scales::percent_format(accuracy = 0.1),
      name     = "Return"
    ) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    theme_quant() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(face = "bold"),
      axis.text.y = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
}
