# ==============================================
# Visualization functions for BacktestCraft
# ==============================================

#' Professional quant theme for ggplot2
#'
#' @param base_size Base font size
#' @return ggplot2 theme object
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = date, y = value)) +
#'   geom_line() +
#'   theme_quant()
#' }
theme_quant <- function(base_size = 10) {
  # Font configuration for better display on Windows
  if (Sys.info()["sysname"] == "Windows") {
    windowsFonts(`Microsoft YaHei` = windowsFont("Microsoft YaHei"))
    default_font <- "Microsoft YaHei"
  } else {
    default_font <- ""
  }

  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(family = default_font),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 12),
      panel.grid.major.y = ggplot2::element_line(linetype = "dashed", color = "#eeeeee"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, color = "#cccccc")
    )
}
