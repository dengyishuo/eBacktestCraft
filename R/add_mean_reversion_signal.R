#' Add Mean Reversion Signal
#'
#' Generates a signal when the indicator deviates from its rolling mean by more
#' than \code{k} standard deviations. Useful for pairs trading, statistical
#' arbitrage, and overbought/oversold detection.
#'
#' Signal = 1 when value is below mean - k*SD (oversold / long entry);
#' Signal = -1 when value is above mean + k*SD (overbought / short entry);
#' Signal = 0 within the band.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param indicator_col Character. Indicator column to test.
#' @param n Integer. Rolling window for mean and SD. Default \code{20L}.
#' @param k Numeric. Number of standard deviations that defines the band.
#'   Default \code{2.0}.
#' @param mode Character.
#'   \code{"long_only"} (default) = signal 1 only when below lower band;
#'   \code{"short_only"} = signal 1 only when above upper band;
#'   \code{"both"} = 1 below lower band, -1 above upper band.
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column.
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' df <- eClassic::add_rps(style, close_col = "adjusted", n = 60)
#' result <- add_mean_reversion_signal(df, indicator_col = "rps_60", n = 60, k = 1.5)
#'
#' @family signal-timeseries
#' @importFrom tibble as_tibble
#' @export
add_mean_reversion_signal <- function(mkt_data,
                                      indicator_col,
                                      n           = 20L,
                                      k           = 2.0,
                                      mode        = c("long_only", "short_only", "both"),
                                      signal_name = NULL,
                                      output      = c("tibble", "data.frame")) {
  output <- match.arg(output)
  mode   <- match.arg(mode)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  if (!indicator_col %in% colnames(mkt_data))
    stop("Column '", indicator_col, "' not found in mkt_data.")

  col <- if (is.null(signal_name))
    paste0("signal_meanrev_", indicator_col, "_n", n, "_k", gsub("\\.", "", k))
  else signal_name

  result_list <- lapply(unique(mkt_data$code), function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    v   <- sub[[indicator_col]]
    nr  <- nrow(sub)
    sig <- integer(nr)

    for (i in seq_len(nr)) {
      if (i <= n) next
      w   <- v[(i - n + 1):i]
      mu  <- mean(w, na.rm = TRUE)
      sd_ <- sd(w,   na.rm = TRUE)
      if (is.na(v[i]) || is.na(sd_) || sd_ == 0) next
      sig[i] <- switch(mode,
        long_only  = if (v[i] < mu - k * sd_)  1L else 0L,
        short_only = if (v[i] > mu + k * sd_)  1L else 0L,
        both       = if (v[i] < mu - k * sd_)  1L else
                     if (v[i] > mu + k * sd_) -1L else 0L
      )
    }

    sub[[col]] <- sig
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  message("Generated signal column: ", col,
          ", valid signals: ", sum(abs(res[[col]]), na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(res) else res
}
