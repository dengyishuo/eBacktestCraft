#' Add Earnings Event Signal
#'
#' Generates a signal within a window around earnings announcement dates.
#' Useful for earnings drift strategies (PEAD), pre-announcement positioning,
#' or post-earnings momentum/reversal.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param earnings_dates A data frame with columns \code{code} and \code{earnings_date}
#'   (Date or character coercible to Date). Each row is one earnings announcement.
#'   Multiple announcements per code are supported.
#' @param pre_window Integer >= 0. Number of trading days before the earnings date
#'   to activate the signal. Default \code{0L} (no pre-window).
#' @param post_window Integer >= 0. Number of trading days after the earnings date
#'   to keep the signal active. Default \code{5L}.
#' @param mode Character.
#'   \code{"pre"} = signal active only in the pre-window;
#'   \code{"post"} (default) = signal active only in the post-window;
#'   \code{"both"} = signal active in both windows (pre and post).
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column (0/1).
#'
#' @examples
#' \dontrun{
#' data(style, package = "eBacktestCraft")
#' earnings <- data.frame(
#'   code          = c("510300.SS", "510300.SS", "513100.SS"),
#'   earnings_date = as.Date(c("2021-04-30", "2021-10-31", "2021-04-28"))
#' )
#' result <- add_earnings_signal(style, earnings_dates = earnings,
#'                               pre_window = 2, post_window = 5)
#' }
#'
#' @family signal-event
#' @importFrom tibble as_tibble
#' @export
add_earnings_signal <- function(mkt_data,
                                earnings_dates,
                                pre_window  = 0L,
                                post_window = 5L,
                                mode        = c("post", "pre", "both"),
                                signal_name = NULL,
                                output      = c("tibble", "data.frame")) {
  output <- match.arg(output)
  mode   <- match.arg(mode)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  if (!all(c("code", "earnings_date") %in% colnames(earnings_dates)))
    stop("'earnings_dates' must contain 'code' and 'earnings_date' columns.")

  col <- if (is.null(signal_name))
    paste0("signal_earnings_", mode, "_pre", pre_window, "_post", post_window)
  else signal_name

  earnings_dates$earnings_date <- as.Date(earnings_dates$earnings_date)
  mkt_data$date                <- as.Date(mkt_data$date)

  sig <- integer(nrow(mkt_data))

  for (cd in unique(mkt_data$code)) {
    idx      <- which(mkt_data$code == cd)
    dates_cd <- sort(mkt_data$date[idx])
    ann_dates <- earnings_dates$earnings_date[earnings_dates$code == cd]
    if (length(ann_dates) == 0) next

    active <- logical(length(dates_cd))
    for (ann in ann_dates) {
      ann <- as.Date(ann)
      pos <- which(dates_cd >= ann)[1]   # first trading day on or after announcement
      if (is.na(pos)) next

      if (mode %in% c("post", "both")) {
        end_pos <- min(length(dates_cd), pos + post_window)
        active[pos:end_pos] <- TRUE
      }
      if (mode %in% c("pre", "both") && pre_window > 0L) {
        start_pos <- max(1L, pos - pre_window)
        active[start_pos:(pos - 1L)] <- TRUE
      }
    }
    sig[idx[order(mkt_data$date[idx])]] <- as.integer(active)
  }

  mkt_data[[col]] <- sig
  message("Generated signal column: ", col,
          ", valid signals: ", sum(mkt_data[[col]], na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(mkt_data) else mkt_data
}
