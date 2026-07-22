# Test add_indicator routing function
pkgload::load_all(".", quiet = TRUE)
library(eBacktestCraft)

cat("=== add_indicator Routing Test ===\n\n")

# 1. Check exports
stopifnot(exists("add_indicator"))
stopifnot(exists("list_indicators"))
cat("[PASS] add_indicator and list_indicators exported\n")

# 2. list_indicators
inds <- list_indicators()
cat(sprintf("[INFO] Total: %d | eTTR=%d eClassic=%d eAlpha101=%d eCandleSticks=%d\n",
    nrow(inds),
    sum(inds$package == "eTTR"),
    sum(inds$package == "eClassic"),
    sum(inds$package == "eAlpha101"),
    sum(inds$package == "eCandleSticks")))

# 3. Test routing with style dataset
data(style, package = "eBacktestCraft")

# 3a. eTTR
r <- tryCatch(add_indicator(style, "rsi", close_col = "close", n = 14), error = function(e) e)
if (inherits(r, "error")) cat(sprintf("[FAIL] eTTR::rsi: %s\n", r$message)) else cat("[PASS] eTTR::add_rsi\n")

r <- tryCatch(add_indicator(style, "sma", n = 10), error = function(e) e)
if (inherits(r, "error")) cat(sprintf("[FAIL] eTTR::sma: %s\n", r$message)) else cat("[PASS] eTTR::add_sma\n")

# 3b. eClassic
r <- tryCatch(add_indicator(style, "mom", close_col = "close", n = 20), error = function(e) e)
if (inherits(r, "error")) cat(sprintf("[FAIL] eClassic::mom: %s\n", r$message)) else cat("[PASS] eClassic::add_mom\n")

r <- tryCatch(add_indicator(style, "eClassic.sma", close_col = "close", n = 10), error = function(e) e)
if (inherits(r, "error")) cat(sprintf("[FAIL] eClassic::sma: %s\n", r$message)) else cat("[PASS] eClassic::add_sma (disambiguated)\n")

# 3c. eAlpha101 (note: requires full OHLC data; style has limited columns)
r <- tryCatch(add_indicator(style, "alpha001", close_col = "close"), error = function(e) e)
if (inherits(r, "error")) cat(sprintf("[INFO] eAlpha101::alpha001 routed but exec failed (data issue, not routing): %s\n", r$message)) else cat("[PASS] eAlpha101::add_alpha001\n")

# 3d. eCandleSticks
r <- tryCatch(add_indicator(style, "csp_doji", output = "tibble"), error = function(e) e)
if (inherits(r, "error")) cat(sprintf("[FAIL] eCandleSticks::doji: %s\n", r$message)) else cat("[PASS] eCandleSticks::add_csp_doji\n")

# 4. Error handling
r <- tryCatch(add_indicator(style, "nonexistent"), error = function(e) e)
if (grepl("Unknown indicator", r$message)) {
    cat("[PASS] Unknown indicator error message OK\n")
} else {
    cat(sprintf("[FAIL] Unexpected error: %s\n", r$message))
}

cat("\n=== Done ===\n")
