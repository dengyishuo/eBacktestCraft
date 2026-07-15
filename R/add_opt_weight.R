#' Optimisation-based portfolio weights
#'
#' For each trading date, fits portfolio weights by minimising / maximising
#' a risk-return objective computed over a rolling window of past returns.
#'
#' Solvers used:
#' \itemize{
#'   \item \code{min_variance} — exact QP via \pkg{quadprog}.
#'   \item All others — unconstrained \code{stats::optim} with softmax
#'     reparameterisation (guarantees \eqn{w \ge 0}, \eqn{\sum w = 1}).
#' }
#'
#' @param mkt_data Data frame with \code{date} and \code{code} columns.
#' @param signal_col Column where value = 1 marks the investable universe.
#' @param return_col Per-asset return column (already computed).
#' @param opt_type Objective. One of:
#'   \code{"min_variance"}, \code{"risk_parity"},
#'   \code{"max_sharpe"}, \code{"min_es"},
#'   \code{"min_mdd"}, \code{"max_calmar"} / \code{"max_kama"},
#'   \code{"max_treynor"} / \code{"max_terino"}.
#' @param window Rolling lookback periods. Default 60.
#' @param alpha Tail probability for ES (default 0.05).
#' @param rf Annual risk-free rate for Sharpe / Treynor. Default 0.
#' @param benchmark_col Benchmark return column required for \code{max_treynor}.
#' @param annual_factor Annualisation factor (252 / 52 / 12). Default 252.
#' @param weight_name Output column name. Auto-generated if NULL.
#' @param fallback Fallback when optimisation fails: \code{"equal"} or \code{"zero"}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#' @return \code{mkt_data} with one appended weight column.
#' @family weight
#' @importFrom quadprog solve.QP
#' @importFrom dplyr arrange
#' @importFrom tibble as_tibble
#' @export
add_opt_weight <- function(
  mkt_data,
  signal_col,
  return_col,
  opt_type      = "min_variance",
  window        = 60L,
  alpha         = 0.05,
  rf            = 0.0,
  benchmark_col = NULL,
  annual_factor = 252L,
  weight_name   = NULL,
  fallback      = "equal",
  output        = c("tibble", "data.frame")
) {
  valid <- c("min_variance", "risk_parity", "max_sharpe", "min_es",
             "min_mdd", "max_calmar", "max_kama", "max_treynor", "max_terino")
  if (!opt_type %in% valid)
    stop("opt_type must be one of: ", paste(valid, collapse = ", "))
  if (opt_type == "max_kama")   opt_type <- "max_calmar"
  if (opt_type == "max_terino") opt_type <- "max_treynor"
  if (opt_type == "max_treynor" && is.null(benchmark_col))
    stop("max_treynor requires benchmark_col")

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("mkt_data must contain 'date' and 'code' columns")
  for (col in c(signal_col, return_col))
    if (!col %in% colnames(mkt_data)) stop("Column not found: ", col)
  if (!is.null(benchmark_col) && !benchmark_col %in% colnames(mkt_data))
    stop("Benchmark column not found: ", benchmark_col)

  output <- match.arg(output)
  wname  <- if (is.null(weight_name))
    paste0("weight_", opt_type, "_", signal_col) else weight_name

  result <- mkt_data %>% dplyr::arrange(.data$date, .data$code)
  result[[wname]] <- 0
  dates  <- sort(unique(result$date))

  for (i in seq_along(dates)) {
    dt       <- dates[i]
    day_rows <- which(result$date == dt)
    sel_rows <- day_rows[result[[signal_col]][day_rows] == 1 &
                           !is.na(result[[signal_col]][day_rows])]
    if (length(sel_rows) == 0L) next

    codes <- result$code[sel_rows]
    past_dates <- dates[max(1L, i - window):(i - 1L)]
    n_eq <- if (fallback == "equal" && length(sel_rows) > 0)
      rep(1 / length(sel_rows), length(sel_rows)) else rep(0, length(sel_rows))

    if (length(past_dates) < 5L) {
      result[[wname]][sel_rows] <- n_eq; next
    }

    past <- result[result$date %in% past_dates & result$code %in% codes,
                   c("date", "code", return_col)]
    R_wide <- tryCatch(
      stats::reshape(past, idvar = "date", timevar = "code", direction = "wide"),
      error = function(e) NULL
    )
    if (is.null(R_wide)) { result[[wname]][sel_rows] <- n_eq; next }

    col_names <- sub(paste0(return_col, "."), "", colnames(R_wide)[-1], fixed = TRUE)
    R_mat <- as.matrix(R_wide[, -1, drop = FALSE])
    colnames(R_mat) <- col_names
    R_mat <- R_mat[, as.character(codes), drop = FALSE]
    R_mat[is.na(R_mat)] <- 0
    if (nrow(R_mat) < 5L || ncol(R_mat) < 1L) {
      result[[wname]][sel_rows] <- n_eq; next
    }

    bm <- NULL
    if (!is.null(benchmark_col)) {
      bm_sub <- result[result$date %in% past_dates, ]
      bm <- tapply(bm_sub[[benchmark_col]], bm_sub$date, mean, na.rm = TRUE)
      bm <- as.numeric(bm[as.character(past_dates)])
      bm[is.na(bm)] <- 0
    }

    w_opt <- .run_opt(R_mat, opt_type, alpha, rf, annual_factor, bm)
    if (is.null(w_opt)) {
      result[[wname]][sel_rows] <- n_eq
    } else {
      names(w_opt) <- col_names
      for (r in sel_rows)
        result[[wname]][r] <- w_opt[as.character(result$code[r])]
    }
  }

  result[[wname]][is.na(result[[wname]])] <- 0
  .diag_weight(result, wname, opt_type)
  if (output == "tibble") tibble::as_tibble(result) else result
}


# ── solvers ───────────────────────────────────────────────────────────────────

.run_opt <- function(R, opt_type, alpha, rf, annual_factor, bm) {
  n   <- ncol(R)
  w0  <- rep(1 / n, n)

  if (opt_type == "min_variance") {
    return(.qp_min_var(R))
  }

  # softmax reparameterisation: theta -> w = exp(theta)/sum(exp(theta))
  # guarantees w >= 0, sum(w) = 1; optimise over unconstrained theta
  obj <- .make_obj(opt_type, R, alpha, rf, annual_factor, bm)
  theta0 <- rep(0, n)
  res <- tryCatch(
    stats::optim(theta0, function(th) obj(.softmax(th)),
                 method = "BFGS",
                 control = list(maxit = 500, reltol = 1e-9)),
    error = function(e) NULL
  )
  if (is.null(res) || res$convergence > 1) return(NULL)
  w <- .softmax(res$par)
  w / sum(w)
}

.softmax <- function(th) {
  th <- th - max(th)        # numerical stability
  e  <- exp(th)
  e  / sum(e)
}

.qp_min_var <- function(R) {
  n   <- ncol(R)
  cov_mat <- cov(R)
  # regularise to ensure positive-definite
  cov_mat <- cov_mat + diag(1e-8, n)
  Dmat <- 2 * cov_mat
  dvec <- rep(0, n)
  # sum(w) = 1  →  Amat col 1; w_i >= 0 → cols 2..n+1
  Amat <- cbind(rep(1, n), diag(n))
  bvec <- c(1, rep(0, n))
  res  <- tryCatch(
    quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 1L),
    error = function(e) NULL
  )
  if (is.null(res)) return(NULL)
  w <- pmax(res$solution, 0)
  w / sum(w)
}

.make_obj <- function(opt_type, R, alpha, rf, annual_factor, bm) {
  switch(opt_type,

    risk_parity = {
      cov_mat <- cov(R) + diag(1e-8, ncol(R))
      function(w) {
        port_var <- as.numeric(t(w) %*% cov_mat %*% w)
        if (port_var < 1e-20) return(0)
        rc     <- w * as.numeric(cov_mat %*% w) / sqrt(port_var)
        target <- sqrt(port_var) / length(w)
        sum((rc - target)^2)
      }
    },

    max_sharpe = {
      mu      <- colMeans(R) * annual_factor
      cov_mat <- cov(R) + diag(1e-8, ncol(R))
      function(w) {
        ret <- sum(mu * w) - rf
        vol <- sqrt(as.numeric(t(w) %*% cov_mat %*% w) * annual_factor)
        if (vol < 1e-10) return(1e10)
        -ret / vol
      }
    },

    min_es = {
      function(w) {
        pr     <- as.numeric(R %*% w)
        cutoff <- quantile(pr, alpha)
        tail   <- pr[pr <= cutoff]
        if (length(tail) == 0) return(0)
        -mean(tail)
      }
    },

    min_mdd = {
      function(w) {
        pr   <- as.numeric(R %*% w)
        nav  <- cumprod(1 + pr)
        peak <- cummax(nav)
        max((peak - nav) / pmax(peak, 1e-10))
      }
    },

    max_calmar = {
      function(w) {
        pr      <- as.numeric(R %*% w)
        ann_ret <- mean(pr) * annual_factor
        nav     <- cumprod(1 + pr)
        peak    <- cummax(nav)
        mdd     <- max((peak - nav) / pmax(peak, 1e-10))
        if (mdd < 1e-10) return(1e10)
        -(ann_ret / mdd)
      }
    },

    max_treynor = {
      rf_p <- rf / annual_factor
      function(w) {
        pr        <- as.numeric(R %*% w)
        excess    <- pr - rf_p
        bm_excess <- bm - rf_p
        var_bm    <- var(bm_excess)
        if (var_bm < 1e-15) return(1e10)
        beta <- cov(excess, bm_excess) / var_bm
        if (abs(beta) < 1e-10) return(1e10)
        -(mean(excess) * annual_factor / beta)
      }
    },

    stop("Unknown opt_type: ", opt_type)
  )
}


# .diag_weight and .fill_fallback are defined in weight_utils.R
