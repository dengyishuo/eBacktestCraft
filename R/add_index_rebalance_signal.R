#' Add Index Rebalance Event Signal
#'
#' Generates a signal around index constituent change dates. Stocks being added
#' to an index tend to be bought up before the rebalance date (inclusion effect);
#' stocks being removed tend to be sold. This function activates a window around
#' each rebalance event date for the affected codes.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param rebalance_events A data frame with columns:
#'   \describe{
#'     \item{\code{code}}{Asset code.}
#'     \item{\code{rebalance_date}}{Date of index constituent change (Date or character).}
#'     \item{\code{direction}}{Character: \code{"add"} (inclusion) or \code{"remove"} (exclusion).}
#'   }
#' @param pre_window Integer >= 0. Trading days before the rebalance date to
#'   activate the signal. Default \code{5L}.
#' @param post_window Integer >= 0. Trading days after the rebalance date to
#'   keep the signal active. Default \code{2L}.
#' @param direction_filter Character. \code{"add"} = only inclusion events;
#'   \code{"remove"} = only exclusion events; \code{"both"} (default) = all events.
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column (0/1).
#'
#' @examples
#' \dontrun{
#' data(style, package = "eBacktestCraft")
#' events <- data.frame(
#'   code           = c("510300.SS", "513100.SS"),
#'   rebalance_date = as.Date(c("2022-06-10", "2022-12-09")),
#'   direction      = c("add", "add")
#' )
#' result <- add_index_rebalance_signal(style, rebalance_events = events,
#'                                      pre_window = 5, post_window = 2)
#' }
#'
#' @family signal-event
#' @importFrom tibble as_tibble
#' @export
add_index_rebalance_signal <- function(mkt_data,
                                       rebalance_events,
                                       pre_window       = 5L,
                                       post_window      = 2L,
                                       direction_filter = c("both", "add", "remove"),
                                       signal_name      = NULL,
                                       output           = c("tibble", "data.frame")) {
  output           <- match.arg(output)
  direction_filter <- match.arg(direction_filter)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  required_ev <- c("code", "rebalance_date", "direction")
  missing_ev  <- setdiff(required_ev, colnames(rebalance_events))
  if (length(missing_ev) > 0)
    stop("'rebalance_events' missing columns: ", paste(missing_ev, collapse = ", "))

  col <- if (is.null(signal_name))
    paste0("signal_idxreb_pre", pre_window, "_post", post_window)
  else signal_name

  rebalance_events$rebalance_date <- as.Date(rebalance_events$rebalance_date)
  mkt_data$date                   <- as.Date(mkt_data$date)

  if (direction_filter != "both")
    rebalance_events <- rebalance_events[
      rebalance_events$direction == direction_filter, ]

  sig <- integer(nrow(mkt_data))

  for (cd in unique(mkt_data$code)) {
    idx      <- which(mkt_data$code == cd)
    dates_cd <- sort(mkt_data$date[idx])
    events_cd <- rebalance_events$rebalance_date[rebalance_events$code == cd]
    if (length(events_cd) == 0) next

    active <- logical(length(dates_cd))
    for (ev in events_cd) {
      ev  <- as.Date(ev)
      pos <- which(dates_cd >= ev)[1]
      if (is.na(pos)) next
      start_pos <- max(1L, pos - pre_window)
      end_pos   <- min(length(dates_cd), pos + post_window)
      active[start_pos:end_pos] <- TRUE
    }
    sig[idx[order(mkt_data$date[idx])]] <- as.integer(active)
  }

  mkt_data[[col]] <- sig
  message("Generated signal column: ", col,
          ", valid signals: ", sum(mkt_data[[col]], na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(mkt_data) else mkt_data
}
