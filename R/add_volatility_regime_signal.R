#' Add Volatility Regime Signal
#'
#' Identifies high/low volatility regimes by comparing the rolling realised
#' volatility of each asset (or a reference index) against a threshold or its
#' own historical percentile. Analogous to a VIX-based risk-on/risk-off filter.
#'
#' Signal = 1 in low-volatility regime (risk-on); 0 in high-volatility regime
#' (risk-off). Use \code{mode = "high"} to signal the high-vol regime instead.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param close_col Character. Price column used to compute returns and vol.
#'   Default \code{"close"}.
#' @param vol_n Integer. Rolling window for realised volatility (annualised).
#'   Default \code{20L}.
#' @param method Character.
#'   \code{"threshold"} (default) = compare vol against a fixed \code{vol_threshold};
#'   \code{"percentile"} = compare vol against its own rolling historical percentile
#'   (lookback = \code{hist_n} days).
#' @param vol_threshold Numeric. Annualised volatility threshold used when
#'   \code{method = "threshold"}. Default \code{0.20} (20\%).
#' @param hist_n Integer. Lookback window for historical percentile when
#'   \code{method = "percentile"}. Default \code{252L}.
#' @param pct_threshold Numeric in (0,1). Percentile cutoff when
#'   \code{method = "percentile"}. Default \code{0.5} (median).
#' @param mode Character. \code{"low"} (default) = signal 1 in low-vol regime;
#'   \code{"high"} = signal 1 in high-vol regime.
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column (0/1).
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' result <- add_volatility_regime_signal(style, close_col = "adjusted",
#'                                        vol_n = 20, vol_threshold = 0.25)
#'
#' @family signal-timeseries
#' @importFrom tibble as_tibble
#' @export
add_volatility_regime_signal <- function(mkt_data,
                                         close_col     = "close",
                                         vol_n         = 20L,
                                         method        = c("threshold", "percentile"),
                                         vol_threshold = 0.20,
                                         hist_n        = 252L,
                                         pct_threshold = 0.5,
                                         mode          = c("low", "high"),
                                         signal_name   = NULL,
                                         output        = c("tibble", "data.frame")) {
  output <- match.arg(output)
  method <- match.arg(method)
  mode   <- match.arg(mode)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  if (!close_col %in% colnames(mkt_data))
    stop("Column '", close_col, "' not found in mkt_data.")

  col <- if (is.null(signal_name))
    paste0("signal_volregime_", method, "_", mode) else signal_name

  result_list <- lapply(unique(mkt_data$code), function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    px  <- sub[[close_col]]
    nr  <- nrow(sub)

    ret <- c(NA_real_, diff(log(px)))       # log returns
    vol <- rep(NA_real_, nr)                # rolling annualised vol

    for (i in seq_len(nr)) {
      if (i <= vol_n) next
      w      <- ret[(i - vol_n + 1):i]
      vol[i] <- sd(w, na.rm = TRUE) * sqrt(252)
    }

    sig <- integer(nr)
    for (i in seq_len(nr)) {
      if (is.na(vol[i])) next
      in_low_vol <- switch(method,
        threshold  = vol[i] <= vol_threshold,
        percentile = {
          start_h <- max(1L, i - hist_n + 1L)
          hist_vol <- vol[start_h:i]
          hist_vol <- hist_vol[!is.na(hist_vol)]
          if (length(hist_vol) < 5) next
          cutoff <- quantile(hist_vol, pct_threshold, na.rm = TRUE)
          vol[i] <= cutoff
        }
      )
      sig[i] <- as.integer(if (mode == "low") in_low_vol else !in_low_vol)
    }

    sub[[col]] <- sig
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  message("Generated signal column: ", col,
          ", valid signals: ", sum(res[[col]], na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(res) else res
}
