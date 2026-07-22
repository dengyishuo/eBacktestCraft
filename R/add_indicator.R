# ==============================================================================
# add_indicator — Unified Indicator Routing Interface
# ==============================================================================
# Routes indicator computation to the appropriate eQuant-R package:
#   eTTR, eClassic, eAlpha101, eCandleSticks
# ==============================================================================

#' Add Indicator via Unified Routing Interface
#'
#' A unified routing function that dispatches indicator computation to the
#' appropriate eQuant-R package (eTTR, eClassic, eAlpha101, eCandleSticks).
#'
#' @description
#' `add_indicator` provides a single entry point for all indicator computations
#' across the eQuant ecosystem. It maps short indicator names to their
#' corresponding `add_*` functions in the correct package.
#'
#' @section Indicator routing by package:
#' * **eTTR** (58 technical indicators) — sma, ema, rsi, macd, bbands, atr, cci,
#'   kdj, obv, sar, stoch, adx, aroon, cmf, mfi, ultosc, vwap, zigzag, pivots,
#'   tdi, trix, tsi, kst, dpo, cmo, wpr, roc, willr, chaikin_volatility,
#'   keltner_channels, donchian_channel, ultimate_oscillator, td_setup,
#'   td_countdown, and more.
#' * **eClassic** (13 classic factors) — mom, beta, ram, size, value,
#'   volatility, return, benchmark, investment, profitability, rps, slope, sma.
#'   Use `"eClassic.sma"`, `"eClassic.volatility"`, `"eClassic.mom"` to
#'   disambiguate from eTTR.
#' * **eAlpha101** (100 WorldQuant formulas) — alpha001 through alpha101.
#' * **eCandleSticks** (63 candle patterns) — csp_doji, csp_engulfing,
#'   csp_hammer, csp_harami, csp_star, csp_marubozu, and more.
#'
#' @section Disambiguation:
#' Use `"Pkg.name"` syntax when the same indicator name exists in multiple
#' packages. Example: `"eClassic.sma"` routes to eClassic instead of eTTR.
#'
#' @param data A data.frame or tibble. Required columns vary by indicator.
#'   Typically needs OHLCV columns for technical indicators and candle patterns.
#' @param indicator Character. Short name of the indicator, e.g. `"rsi"`,
#'   `"macd"`, `"alpha001"`, `"csp_doji"`. Use `list_indicators()` to browse.
#' @param ... Additional arguments passed through to the underlying `add_*`
#'   function (e.g. `n`, `close_col`, `append`, `output`).
#'
#' @return A data.frame or tibble with the indicator column(s) appended (when
#'   `append = TRUE`, the default in most `add_*` functions).
#'
#' @examples
#' \dontrun{
#' library(eBacktestCraft)
#' library(eTTR)
#' library(eClassic)
#'
#' df <- data.frame(
#'   date = rep(Sys.Date() - 100:1, 2),
#'   code = rep(c("A", "B"), each = 100),
#'   close = runif(200, 10, 100)
#' )
#'
#' # Technical indicator (eTTR)
#' df <- add_indicator(df, "rsi", close_col = "close", n = 14)
#'
#' # Classic factor (eClassic)
#' df <- add_indicator(df, "mom", close_col = "close", n = 20)
#'
#' # Alpha factor (eAlpha101)
#' df <- add_indicator(df, "alpha001", close_col = "close", returns_col = "returns")
#'
#' # Candle pattern (eCandleSticks)
#' signals <- add_indicator(df, "csp_doji", output = "tibble")
#'
#' # List available indicators
#' list_indicators("eClassic")
#' }
#'
#' @export
add_indicator <- function(data, indicator, ...) {
  indicator_map <- build_indicator_map()

  entry <- indicator_map[[indicator]]
  if (is.null(entry)) {
    stop(
      "Unknown indicator: '", indicator, "'.\n",
      "Use `list_indicators()` to see all available indicators.\n",
      'Use `"Pkg.name"` for disambiguation, e.g. `"eClassic.sma"`.'
    )
  }

  fun <- tryCatch(
    getFromNamespace(entry$fun, entry$pkg),
    error = function(e) {
      stop(
        "Failed to load '", entry$pkg, "::", entry$fun, "'.\n",
        "Please install '", entry$pkg, "' first: ",
        'pak::pak("dengyishuo/', entry$pkg, '")'
      )
    }
  )

  fun(data, ...)
}


#' List All Available Indicators
#'
#' Returns a data.frame of all indicators registered in the `add_indicator`
#' routing system, with their source package and function name.
#'
#' @param package Optional character. Filter by source package
#'   (`"eTTR"`, `"eClassic"`, `"eAlpha101"`, `"eCandleSticks"`).
#'   If `NULL`, returns all 230+ indicators.
#'
#' @return A tibble with columns: `indicator`, `package`, `func`.
#'
#' @examples
#' \dontrun{
#' # All indicators
#' list_indicators()
#'
#' # Classic factors only
#' list_indicators("eClassic")
#' }
#'
#' @export
list_indicators <- function(package = NULL) {
  indicator_map <- build_indicator_map()

  out <- do.call(rbind, lapply(names(indicator_map), function(nm) {
    data.frame(
      indicator = nm,
      package   = indicator_map[[nm]]$pkg,
      func      = indicator_map[[nm]]$fun,
      stringsAsFactors = FALSE
    )
  }))

  if (!is.null(package)) {
    out <- out[out$package == package, ]
  }

  tibble::as_tibble(out)
}


# ==============================================================================
# Internal: build the complete indicator routing map
# ==============================================================================

build_indicator_map <- function() {

  list(

    # ============================================================ eTTR (58)
    alma                = list(pkg = "eTTR", fun = "add_alma"),
    atr                 = list(pkg = "eTTR", fun = "add_atr"),
    bbands              = list(pkg = "eTTR", fun = "add_bbands"),
    cci                 = list(pkg = "eTTR", fun = "add_cci"),
    clv                 = list(pkg = "eTTR", fun = "add_clv"),
    cmf                 = list(pkg = "eTTR", fun = "add_cmf"),
    cmo                 = list(pkg = "eTTR", fun = "add_cmo"),
    cti                 = list(pkg = "eTTR", fun = "add_cti"),
    dema                = list(pkg = "eTTR", fun = "add_dema"),
    dpo                 = list(pkg = "eTTR", fun = "add_dpo"),
    dvi                 = list(pkg = "eTTR", fun = "add_dvi"),
    donchian_channel    = list(pkg = "eTTR", fun = "add_donchian_channel"),
    ema                 = list(pkg = "eTTR", fun = "add_ema"),
    emv                 = list(pkg = "eTTR", fun = "add_emv"),
    evwma               = list(pkg = "eTTR", fun = "add_evwma"),
    gmma                = list(pkg = "eTTR", fun = "add_gmma"),
    hma                 = list(pkg = "eTTR", fun = "add_hma"),
    kdj                 = list(pkg = "eTTR", fun = "add_kdj"),
    kst                 = list(pkg = "eTTR", fun = "add_kst"),
    macd                = list(pkg = "eTTR", fun = "add_macd"),
    mfi                 = list(pkg = "eTTR", fun = "add_mfi"),
    obv                 = list(pkg = "eTTR", fun = "add_obv"),
    pbands              = list(pkg = "eTTR", fun = "add_pbands"),
    po                  = list(pkg = "eTTR", fun = "add_po"),
    roc                 = list(pkg = "eTTR", fun = "add_roc"),
    rsi                 = list(pkg = "eTTR", fun = "add_rsi"),
    rvi                 = list(pkg = "eTTR", fun = "add_rvi"),
    sar                 = list(pkg = "eTTR", fun = "add_sar"),
    sma                 = list(pkg = "eTTR", fun = "add_sma"),
    smi                 = list(pkg = "eTTR", fun = "add_smi"),
    snr                 = list(pkg = "eTTR", fun = "add_snr"),
    tdi                 = list(pkg = "eTTR", fun = "add_tdi"),
    tr                  = list(pkg = "eTTR", fun = "add_tr"),
    trix                = list(pkg = "eTTR", fun = "add_trix"),
    tsi                 = list(pkg = "eTTR", fun = "add_tsi"),
    vhf                 = list(pkg = "eTTR", fun = "add_vhf"),
    vwap                = list(pkg = "eTTR", fun = "add_vwap"),
    vwma                = list(pkg = "eTTR", fun = "add_vwma"),
    wma                 = list(pkg = "eTTR", fun = "add_wma"),
    wpr                 = list(pkg = "eTTR", fun = "add_wpr"),
    zlema               = list(pkg = "eTTR", fun = "add_zlema"),
    zigzag              = list(pkg = "eTTR", fun = "add_zig_zag"),
    adj_ratios          = list(pkg = "eTTR", fun = "add_adj_ratios"),
    adx                 = list(pkg = "eTTR", fun = "add_adx"),
    aroon               = list(pkg = "eTTR", fun = "add_aroon"),
    chaikin_ad          = list(pkg = "eTTR", fun = "add_chaikin_ad"),
    chaikin_volatility  = list(pkg = "eTTR", fun = "add_chaikin_volatility"),
    growth              = list(pkg = "eTTR", fun = "add_growth"),
    keltner_channels    = list(pkg = "eTTR", fun = "add_keltner_channels"),
    momentum            = list(pkg = "eTTR", fun = "add_momentum"),
    pivots              = list(pkg = "eTTR", fun = "add_pivots"),
    roll_sfm            = list(pkg = "eTTR", fun = "add_roll_sfm"),
    stoch               = list(pkg = "eTTR", fun = "add_stoch"),
    td_setup            = list(pkg = "eTTR", fun = "add_td_setup"),
    td_countdown        = list(pkg = "eTTR", fun = "add_td_countdown"),
    ultosc              = list(pkg = "eTTR", fun = "add_ultimate_oscillator"),
    volatility          = list(pkg = "eTTR", fun = "add_volatility"),
    willr               = list(pkg = "eTTR", fun = "add_williams_ad"),

    # ============================================================ eClassic (13)
    benchmark       = list(pkg = "eClassic", fun = "add_benchmark"),
    beta            = list(pkg = "eClassic", fun = "add_beta"),
    investment      = list(pkg = "eClassic", fun = "add_investment"),
    profitability   = list(pkg = "eClassic", fun = "add_profitability"),
    ram             = list(pkg = "eClassic", fun = "add_ram"),
    return          = list(pkg = "eClassic", fun = "add_return"),
    rps             = list(pkg = "eClassic", fun = "add_rps"),
    size            = list(pkg = "eClassic", fun = "add_size"),
    slope           = list(pkg = "eClassic", fun = "add_slope"),
    value           = list(pkg = "eClassic", fun = "add_value"),
    mom             = list(pkg = "eClassic", fun = "add_mom"),

    # Disambiguated eClassic versions
    "eClassic.sma"        = list(pkg = "eClassic", fun = "add_sma"),
    "eClassic.volatility" = list(pkg = "eClassic", fun = "add_volatility"),
    "eClassic.mom"        = list(pkg = "eClassic", fun = "add_mom"),

    # ============================================================ eAlpha101 (100)
    alpha001 = list(pkg = "eAlpha101", fun = "add_alpha001"),
    alpha002 = list(pkg = "eAlpha101", fun = "add_alpha002"),
    alpha003 = list(pkg = "eAlpha101", fun = "add_alpha003"),
    alpha004 = list(pkg = "eAlpha101", fun = "add_alpha004"),
    alpha005 = list(pkg = "eAlpha101", fun = "add_alpha005"),
    alpha006 = list(pkg = "eAlpha101", fun = "add_alpha006"),
    alpha007 = list(pkg = "eAlpha101", fun = "add_alpha007"),
    alpha008 = list(pkg = "eAlpha101", fun = "add_alpha008"),
    alpha009 = list(pkg = "eAlpha101", fun = "add_alpha009"),
    alpha010 = list(pkg = "eAlpha101", fun = "add_alpha010"),
    alpha011 = list(pkg = "eAlpha101", fun = "add_alpha011"),
    alpha012 = list(pkg = "eAlpha101", fun = "add_alpha012"),
    alpha013 = list(pkg = "eAlpha101", fun = "add_alpha013"),
    alpha014 = list(pkg = "eAlpha101", fun = "add_alpha014"),
    alpha015 = list(pkg = "eAlpha101", fun = "add_alpha015"),
    alpha016 = list(pkg = "eAlpha101", fun = "add_alpha016"),
    alpha017 = list(pkg = "eAlpha101", fun = "add_alpha017"),
    alpha018 = list(pkg = "eAlpha101", fun = "add_alpha018"),
    alpha019 = list(pkg = "eAlpha101", fun = "add_alpha019"),
    alpha020 = list(pkg = "eAlpha101", fun = "add_alpha020"),
    alpha021 = list(pkg = "eAlpha101", fun = "add_alpha021"),
    alpha022 = list(pkg = "eAlpha101", fun = "add_alpha022"),
    alpha023 = list(pkg = "eAlpha101", fun = "add_alpha023"),
    alpha024 = list(pkg = "eAlpha101", fun = "add_alpha024"),
    alpha025 = list(pkg = "eAlpha101", fun = "add_alpha025"),
    alpha026 = list(pkg = "eAlpha101", fun = "add_alpha026"),
    alpha027 = list(pkg = "eAlpha101", fun = "add_alpha027"),
    alpha028 = list(pkg = "eAlpha101", fun = "add_alpha028"),
    alpha029 = list(pkg = "eAlpha101", fun = "add_alpha029"),
    alpha030 = list(pkg = "eAlpha101", fun = "add_alpha030"),
    alpha031 = list(pkg = "eAlpha101", fun = "add_alpha031"),
    alpha032 = list(pkg = "eAlpha101", fun = "add_alpha032"),
    alpha033 = list(pkg = "eAlpha101", fun = "add_alpha033"),
    alpha034 = list(pkg = "eAlpha101", fun = "add_alpha034"),
    alpha035 = list(pkg = "eAlpha101", fun = "add_alpha035"),
    alpha036 = list(pkg = "eAlpha101", fun = "add_alpha036"),
    alpha037 = list(pkg = "eAlpha101", fun = "add_alpha037"),
    alpha038 = list(pkg = "eAlpha101", fun = "add_alpha038"),
    alpha039 = list(pkg = "eAlpha101", fun = "add_alpha039"),
    alpha040 = list(pkg = "eAlpha101", fun = "add_alpha040"),
    alpha041 = list(pkg = "eAlpha101", fun = "add_alpha041"),
    alpha042 = list(pkg = "eAlpha101", fun = "add_alpha042"),
    alpha043 = list(pkg = "eAlpha101", fun = "add_alpha043"),
    alpha044 = list(pkg = "eAlpha101", fun = "add_alpha044"),
    alpha045 = list(pkg = "eAlpha101", fun = "add_alpha045"),
    alpha046 = list(pkg = "eAlpha101", fun = "add_alpha046"),
    alpha047 = list(pkg = "eAlpha101", fun = "add_alpha047"),
    alpha049 = list(pkg = "eAlpha101", fun = "add_alpha049"),
    alpha050 = list(pkg = "eAlpha101", fun = "add_alpha050"),
    alpha051 = list(pkg = "eAlpha101", fun = "add_alpha051"),
    alpha052 = list(pkg = "eAlpha101", fun = "add_alpha052"),
    alpha053 = list(pkg = "eAlpha101", fun = "add_alpha053"),
    alpha054 = list(pkg = "eAlpha101", fun = "add_alpha054"),
    alpha055 = list(pkg = "eAlpha101", fun = "add_alpha055"),
    alpha056 = list(pkg = "eAlpha101", fun = "add_alpha056"),
    alpha057 = list(pkg = "eAlpha101", fun = "add_alpha057"),
    alpha058 = list(pkg = "eAlpha101", fun = "add_alpha058"),
    alpha059 = list(pkg = "eAlpha101", fun = "add_alpha059"),
    alpha060 = list(pkg = "eAlpha101", fun = "add_alpha060"),
    alpha061 = list(pkg = "eAlpha101", fun = "add_alpha061"),
    alpha062 = list(pkg = "eAlpha101", fun = "add_alpha062"),
    alpha063 = list(pkg = "eAlpha101", fun = "add_alpha063"),
    alpha064 = list(pkg = "eAlpha101", fun = "add_alpha064"),
    alpha065 = list(pkg = "eAlpha101", fun = "add_alpha065"),
    alpha066 = list(pkg = "eAlpha101", fun = "add_alpha066"),
    alpha067 = list(pkg = "eAlpha101", fun = "add_alpha067"),
    alpha068 = list(pkg = "eAlpha101", fun = "add_alpha068"),
    alpha069 = list(pkg = "eAlpha101", fun = "add_alpha069"),
    alpha070 = list(pkg = "eAlpha101", fun = "add_alpha070"),
    alpha071 = list(pkg = "eAlpha101", fun = "add_alpha071"),
    alpha072 = list(pkg = "eAlpha101", fun = "add_alpha072"),
    alpha073 = list(pkg = "eAlpha101", fun = "add_alpha073"),
    alpha074 = list(pkg = "eAlpha101", fun = "add_alpha074"),
    alpha075 = list(pkg = "eAlpha101", fun = "add_alpha075"),
    alpha076 = list(pkg = "eAlpha101", fun = "add_alpha076"),
    alpha077 = list(pkg = "eAlpha101", fun = "add_alpha077"),
    alpha078 = list(pkg = "eAlpha101", fun = "add_alpha078"),
    alpha079 = list(pkg = "eAlpha101", fun = "add_alpha079"),
    alpha080 = list(pkg = "eAlpha101", fun = "add_alpha080"),
    alpha081 = list(pkg = "eAlpha101", fun = "add_alpha081"),
    alpha082 = list(pkg = "eAlpha101", fun = "add_alpha082"),
    alpha083 = list(pkg = "eAlpha101", fun = "add_alpha083"),
    alpha084 = list(pkg = "eAlpha101", fun = "add_alpha084"),
    alpha085 = list(pkg = "eAlpha101", fun = "add_alpha085"),
    alpha086 = list(pkg = "eAlpha101", fun = "add_alpha086"),
    alpha087 = list(pkg = "eAlpha101", fun = "add_alpha087"),
    alpha088 = list(pkg = "eAlpha101", fun = "add_alpha088"),
    alpha089 = list(pkg = "eAlpha101", fun = "add_alpha089"),
    alpha090 = list(pkg = "eAlpha101", fun = "add_alpha090"),
    alpha091 = list(pkg = "eAlpha101", fun = "add_alpha091"),
    alpha092 = list(pkg = "eAlpha101", fun = "add_alpha092"),
    alpha093 = list(pkg = "eAlpha101", fun = "add_alpha093"),
    alpha094 = list(pkg = "eAlpha101", fun = "add_alpha094"),
    alpha095 = list(pkg = "eAlpha101", fun = "add_alpha095"),
    alpha096 = list(pkg = "eAlpha101", fun = "add_alpha096"),
    alpha097 = list(pkg = "eAlpha101", fun = "add_alpha097"),
    alpha098 = list(pkg = "eAlpha101", fun = "add_alpha098"),
    alpha099 = list(pkg = "eAlpha101", fun = "add_alpha099"),
    alpha100 = list(pkg = "eAlpha101", fun = "add_alpha100"),
    alpha101 = list(pkg = "eAlpha101", fun = "add_alpha101"),

    # ============================================================ eCandleSticks (63)
    csp_abandoned_baby               = list(pkg = "eCandleSticks", fun = "add_csp_abandoned_baby"),
    csp_advance_block                = list(pkg = "eCandleSticks", fun = "add_csp_advance_block"),
    csp_belt_hold                    = list(pkg = "eCandleSticks", fun = "add_csp_belt_hold"),
    csp_breakaway                    = list(pkg = "eCandleSticks", fun = "add_csp_breakaway"),
    csp_closing_marubozu             = list(pkg = "eCandleSticks", fun = "add_csp_closing_marubozu"),
    csp_conceal_baby_swallow         = list(pkg = "eCandleSticks", fun = "add_csp_conceal_baby_swallow"),
    csp_counter_attack               = list(pkg = "eCandleSticks", fun = "add_csp_counter_attack"),
    csp_dark_cloud_cover             = list(pkg = "eCandleSticks", fun = "add_csp_dark_cloud_cover"),
    csp_doji                         = list(pkg = "eCandleSticks", fun = "add_csp_doji"),
    csp_doji_star                    = list(pkg = "eCandleSticks", fun = "add_csp_doji_star"),
    csp_engulfing                    = list(pkg = "eCandleSticks", fun = "add_csp_engulfing"),
    csp_gap                          = list(pkg = "eCandleSticks", fun = "add_csp_gap"),
    csp_gap_side_side_white          = list(pkg = "eCandleSticks", fun = "add_csp_gap_side_side_white"),
    csp_hammer                       = list(pkg = "eCandleSticks", fun = "add_csp_hammer"),
    csp_hanging_man                  = list(pkg = "eCandleSticks", fun = "add_csp_hanging_man"),
    csp_harami                       = list(pkg = "eCandleSticks", fun = "add_csp_harami"),
    csp_high_wave                    = list(pkg = "eCandleSticks", fun = "add_csp_high_wave"),
    csp_homing_pigeon                = list(pkg = "eCandleSticks", fun = "add_csp_homing_pigeon"),
    csp_identical_3_crows            = list(pkg = "eCandleSticks", fun = "add_csp_identical_3_crows"),
    csp_in_neck                      = list(pkg = "eCandleSticks", fun = "add_csp_in_neck"),
    csp_inside_day                   = list(pkg = "eCandleSticks", fun = "add_csp_inside_day"),
    csp_inverted_hammer              = list(pkg = "eCandleSticks", fun = "add_csp_inverted_hammer"),
    csp_kicking                      = list(pkg = "eCandleSticks", fun = "add_csp_kicking"),
    csp_ladder_bottom                = list(pkg = "eCandleSticks", fun = "add_csp_ladder_bottom"),
    csp_long_candle                  = list(pkg = "eCandleSticks", fun = "add_csp_long_candle"),
    csp_long_candle_body             = list(pkg = "eCandleSticks", fun = "add_csp_long_candle_body"),
    csp_long_legged_doji             = list(pkg = "eCandleSticks", fun = "add_csp_long_legged_doji"),
    csp_marubozu                     = list(pkg = "eCandleSticks", fun = "add_csp_marubozu"),
    csp_matching_low                 = list(pkg = "eCandleSticks", fun = "add_csp_matching_low"),
    csp_n_blended                    = list(pkg = "eCandleSticks", fun = "add_csp_n_blended"),
    csp_n_higher_close               = list(pkg = "eCandleSticks", fun = "add_csp_n_higher_close"),
    csp_n_long_black_candle_bodies   = list(pkg = "eCandleSticks", fun = "add_csp_n_long_black_candle_bodies"),
    csp_n_long_black_candles         = list(pkg = "eCandleSticks", fun = "add_csp_n_long_black_candles"),
    csp_n_long_white_candle_bodies   = list(pkg = "eCandleSticks", fun = "add_csp_n_long_white_candle_bodies"),
    csp_n_long_white_candles         = list(pkg = "eCandleSticks", fun = "add_csp_n_long_white_candles"),
    csp_n_lower_close                = list(pkg = "eCandleSticks", fun = "add_csp_n_lower_close"),
    csp_on_neck                      = list(pkg = "eCandleSticks", fun = "add_csp_on_neck"),
    csp_outside_day                  = list(pkg = "eCandleSticks", fun = "add_csp_outside_day"),
    csp_piercing_pattern             = list(pkg = "eCandleSticks", fun = "add_csp_piercing_pattern"),
    csp_rickshaw_man                 = list(pkg = "eCandleSticks", fun = "add_csp_rickshaw_man"),
    csp_separating_lines             = list(pkg = "eCandleSticks", fun = "add_csp_separating_lines"),
    csp_shooting_star                = list(pkg = "eCandleSticks", fun = "add_csp_shooting_star"),
    csp_short_candle                 = list(pkg = "eCandleSticks", fun = "add_csp_short_candle"),
    csp_short_candle_body            = list(pkg = "eCandleSticks", fun = "add_csp_short_candle_body"),
    csp_spinning_top                 = list(pkg = "eCandleSticks", fun = "add_csp_spinning_top"),
    csp_stalled_pattern              = list(pkg = "eCandleSticks", fun = "add_csp_stalled_pattern"),
    csp_star                         = list(pkg = "eCandleSticks", fun = "add_csp_star"),
    csp_stick_sandwich               = list(pkg = "eCandleSticks", fun = "add_csp_stick_sandwich"),
    csp_stomach                      = list(pkg = "eCandleSticks", fun = "add_csp_stomach"),
    csp_takuri                       = list(pkg = "eCandleSticks", fun = "add_csp_takuri"),
    csp_tasuki_gap                   = list(pkg = "eCandleSticks", fun = "add_csp_tasuki_gap"),
    csp_three_black_crows            = list(pkg = "eCandleSticks", fun = "add_csp_three_black_crows"),
    csp_three_inside                 = list(pkg = "eCandleSticks", fun = "add_csp_three_inside"),
    csp_three_line_strike            = list(pkg = "eCandleSticks", fun = "add_csp_three_line_strike"),
    csp_three_methods                = list(pkg = "eCandleSticks", fun = "add_csp_three_methods"),
    csp_three_outside                = list(pkg = "eCandleSticks", fun = "add_csp_three_outside"),
    csp_three_stars_in_south         = list(pkg = "eCandleSticks", fun = "add_csp_three_stars_in_south"),
    csp_three_white_soldiers         = list(pkg = "eCandleSticks", fun = "add_csp_three_white_soldiers"),
    csp_thrusting                    = list(pkg = "eCandleSticks", fun = "add_csp_thrusting"),
    csp_tristar                      = list(pkg = "eCandleSticks", fun = "add_csp_tristar"),
    csp_two_crows                    = list(pkg = "eCandleSticks", fun = "add_csp_two_crows"),
    csp_unique_3_river               = list(pkg = "eCandleSticks", fun = "add_csp_unique_3_river"),
    csp_upside_gap_2_crows           = list(pkg = "eCandleSticks", fun = "add_csp_upside_gap_2_crows")
  )
}
