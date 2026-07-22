#' Umbrella Signal Generator
#'
#' A single entry point for all signal types. Dispatches to the corresponding
#' \code{add_*_signal()} function based on \code{type}. All additional arguments
#' are forwarded via \code{...}.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param type Character. Signal type — one of:
#' \describe{
#'   \item{\code{"threshold"}}{Fixed-value comparison. → \code{\link{add_threshold_signal}}}
#'   \item{\code{"crossover"}}{Band crossover detection. → \code{\link{add_crossover_signal}}}
#'   \item{\code{"multi_condition"}}{AND/OR of multiple indicator columns. → \code{\link{add_multi_condition_signal}}}
#'   \item{\code{"between"}}{Value inside a closed interval. → \code{\link{add_between_signal}}}
#'   \item{\code{"constant"}}{Fixed value for all rows. → \code{\link{add_constant_signal}}}
#'   \item{\code{"rank"}}{Cross-sectional top-N ranking. → \code{\link{add_rank_signal}}}
#'   \item{\code{"quantile"}}{Cross-sectional quantile selection. → \code{\link{add_quantile_signal}}}
#'   \item{\code{"percentile"}}{Cross-sectional top/bottom percentile. → \code{\link{add_percentile_signal}}}
#'   \item{\code{"zscore"}}{Cross-sectional Z-score threshold. → \code{\link{add_zscore_signal}}}
#'   \item{\code{"rolling"}}{Rolling-window percentile/SD exceedance. → \code{\link{add_rolling_signal}}}
#'   \item{\code{"consecutive"}}{Consecutive-bar condition. → \code{\link{add_consecutive_signal}}}
#'   \item{\code{"ma_cross"}}{Moving average crossover (golden/death cross). → \code{\link{add_ma_cross_signal}}}
#'   \item{\code{"breakout"}}{N-day high/low breakout (Donchian). → \code{\link{add_breakout_signal}}}
#'   \item{\code{"mean_reversion"}}{Deviation beyond k-sigma band. → \code{\link{add_mean_reversion_signal}}}
#'   \item{\code{"regime"}}{Bull/bear regime via long-term MA. → \code{\link{add_regime_signal}}}
#'   \item{\code{"score"}}{Multi-factor weighted composite score. → \code{\link{add_score_signal}}}
#'   \item{\code{"and"}}{AND combination of existing signal columns. → \code{\link{add_and_signal}}}
#'   \item{\code{"or"}}{OR combination of existing signal columns. → \code{\link{add_or_signal}}}
#'   \item{\code{"vote"}}{Majority-vote combination of existing signal columns. → \code{\link{add_vote_signal}}}
#'   \item{\code{"td_setup"}}{TD Sequential Setup=9 completion. → \code{\link{add_td_setup_signal}}}
#'   \item{\code{"earnings"}}{Window around earnings announcement dates. → \code{\link{add_earnings_signal}}}
#'   \item{\code{"index_rebalance"}}{Window around index constituent change dates. → \code{\link{add_index_rebalance_signal}}}
#'   \item{\code{"vol_regime"}}{High/low realised volatility regime. → \code{\link{add_volatility_regime_signal}}}
#'   \item{\code{"macro"}}{Macro indicator threshold/trend/change. → \code{\link{add_macro_signal}}}
#'   \item{\code{"window"}}{Extend an existing signal over N lookforward bars. → \code{\link{add_window_signal}}}
#' }
#' @param ... Arguments forwarded to the underlying \code{add_*_signal()} function.
#'   See the corresponding function's documentation for available parameters.
#'
#' @return \code{mkt_data} with one appended signal column, identical to the
#'   output of the dispatched function.
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' df <- add_indicator(style, "mom", close_col = "adjusted", n = 20)
#'
#' # threshold
#' add_signal(df, type = "threshold", indicator_cols = "mom_20",
#'            threshold = 0, compare_op = ">")
#'
#' # rank
#' add_signal(df, type = "rank", rank_col = "mom_20", top_n = 3)
#'
#' # constant (buy-and-hold baseline)
#' add_signal(style, type = "constant", value = 1L)
#'
#' @family signal
#' @seealso
#' \code{\link{add_threshold_signal}}, \code{\link{add_crossover_signal}},
#' \code{\link{add_multi_condition_signal}}, \code{\link{add_between_signal}},
#' \code{\link{add_constant_signal}}, \code{\link{add_rank_signal}},
#' \code{\link{add_quantile_signal}}, \code{\link{add_percentile_signal}},
#' \code{\link{add_zscore_signal}}, \code{\link{add_rolling_signal}},
#' \code{\link{add_consecutive_signal}}, \code{\link{add_ma_cross_signal}},
#' \code{\link{add_breakout_signal}}, \code{\link{add_mean_reversion_signal}},
#' \code{\link{add_regime_signal}}, \code{\link{add_score_signal}},
#' \code{\link{add_and_signal}}, \code{\link{add_or_signal}},
#' \code{\link{add_vote_signal}}
#' @export
add_signal <- function(mkt_data, type, ...) {
  dispatch <- list(
    threshold       = add_threshold_signal,
    crossover       = add_crossover_signal,
    multi_condition = add_multi_condition_signal,
    between         = add_between_signal,
    constant        = add_constant_signal,
    rank            = add_rank_signal,
    quantile        = add_quantile_signal,
    percentile      = add_percentile_signal,
    zscore          = add_zscore_signal,
    rolling         = add_rolling_signal,
    consecutive     = add_consecutive_signal,
    ma_cross        = add_ma_cross_signal,
    breakout        = add_breakout_signal,
    mean_reversion  = add_mean_reversion_signal,
    regime          = add_regime_signal,
    score           = add_score_signal,
    and                = add_and_signal,
    or                 = add_or_signal,
    vote               = add_vote_signal,
    td_setup           = add_td_setup_signal,
    earnings           = add_earnings_signal,
    index_rebalance    = add_index_rebalance_signal,
    vol_regime         = add_volatility_regime_signal,
    macro              = add_macro_signal,
    window             = add_window_signal
  )

  if (!type %in% names(dispatch))
    stop("Unknown signal type '", type, "'. Available types: ",
         paste(names(dispatch), collapse = ", "))

  dispatch[[type]](mkt_data, ...)
}
