#' Umbrella Weight Generator
#'
#' A single entry point for all weight allocation methods. Dispatches to the
#' corresponding \code{add_*_weight()} function based on \code{type}. All
#' additional arguments are forwarded via \code{...}.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param type Character. Weight type — one of:
#' \describe{
#'   \item{\code{"equal"}}{Equal weight (1/n) across selected stocks per day.
#'     → \code{\link{add_equal_weight}}}
#'   \item{\code{"fixed"}}{Pre-defined fixed weights per stock, optionally
#'     normalized daily. → \code{\link{add_fixed_weight}}}
#'   \item{\code{"norm"}}{Factor-proportional weights via linear or softmax
#'     normalization. → \code{\link{add_norm_weight}}}
#' }
#' @param ... Arguments forwarded to the underlying \code{add_*_weight()} function.
#'   See the corresponding function's documentation for available parameters.
#'
#' @return \code{mkt_data} with one appended weight column, identical to the
#'   output of the dispatched function.
#'
#' @examples
#' \dontrun{
#' data(style, package = "eBacktestCraft")
#' df <- eClassic::add_mom(style, close_col = "adjusted", n = 20)
#'
#' # Step 1: generate a signal
#' df <- add_signal(df, type = "threshold",
#'                  indicator_cols = "mom_20",
#'                  threshold = 0, compare_op = ">")
#'
#' # Equal weight
#' df <- add_weight(df, type = "equal",
#'                  signal_col = "signal_mom_20_gt_0")
#'
#' # Fixed weight
#' fw <- c("000001.SZ" = 0.6, "000002.SZ" = 0.4)
#' df <- add_weight(df, type = "fixed",
#'                  signal_col    = "signal_mom_20_gt_0",
#'                  fixed_weights = fw,
#'                  strict_check  = FALSE)
#'
#' # Norm weight (linear)
#' df <- add_weight(df, type = "norm",
#'                  weight_col  = "mom_20",
#'                  signal_col  = "signal_mom_20_gt_0",
#'                  norm_method = "linear")
#'
#' # Norm weight (softmax) — pipeline-compatible
#' df |>
#'   add_signal(type = "rank", rank_col = "mom_20", top_n = 10) |>
#'   add_weight(type = "norm",
#'              weight_col  = "mom_20",
#'              signal_col  = "signal_rank_mom_20_top10",
#'              norm_method = "softmax")
#' }
#'
#' @family weight
#' @seealso
#'   \code{\link{add_equal_weight}},
#'   \code{\link{add_fixed_weight}},
#'   \code{\link{add_norm_weight}}
#' @export
add_weight <- function(mkt_data, type, ...) {
  dispatch <- list(
    equal = add_equal_weight,
    fixed = add_fixed_weight,
    norm  = add_norm_weight
  )

  valid_types <- names(dispatch)
  if (!type %in% valid_types) {
    stop(
      "Unknown weight type: '", type, "'. ",
      "Must be one of: ", paste(valid_types, collapse = ", "), "."
    )
  }

  dispatch[[type]](mkt_data, ...)
}
