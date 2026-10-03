# ============================================================================
# RSM 応答曲面法分析アプリケーション
# Response Surface Methodology Analysis
# ============================================================================
# Optimized & Refactored Version
# - Modular architecture with extracted utility functions
# - Vectorized operations for performance
# - Memoization for expensive computations
# - Enhanced error handling and validation
# - Statistical computation improvements
# ============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(DT)
  library(plotly)
  library(bslib)
})

# ============================================================================
# CONSTANTS & CONFIGURATION
# ============================================================================

# Numeric thresholds
COEF_ZERO_THRESHOLD <- 1e-8
DISPLAY_THRESHOLD <- 1e-6
MIN_EXPLANATORY_VARS <- 2
MAX_RECOMMENDED_VARS <- 15
DEFAULT_SAMPLE_SIZE <- 200
RANDOM_SEED <- 42
MIN_SAMPLE_SIZE <- 10  # Minimum observations for reliable analysis

# Null coalescing operator (for R < 4.1 compatibility)
`%||%` <- function(x, y) if (is.null(x)) y else x

# Color palette (GitHub-inspired dark theme)
COLORS <- list(
  bg_primary   = "#0d1117",
  bg_secondary = "#161b22",
  bg_tertiary  = "#21262d",
  border       = "#30363d",
  text_primary = "#c9d1d9",
  text_muted   = "#8b949e",
  accent_blue  = "#58a6ff",
  accent_green = "#3fb950",
  accent_red   = "#f85149",
  accent_orange = "#d29922",
  accent_purple = "#a371f7",
  info_blue    = "#388bfd",
  link_blue    = "#79c0ff"
)

# R² thresholds for color coding
R2_EXCELLENT <- 0.8
R2_GOOD <- 0.6

# ============================================================================
# THEME & STYLING
# ============================================================================

custom_theme <- bs_theme(

  version = 5,
  bg = COLORS$bg_primary,
  fg = COLORS$text_primary,
  primary = COLORS$accent_blue,
  secondary = COLORS$bg_tertiary,
  success = COLORS$accent_green,
  warning = COLORS$accent_orange,

  danger = COLORS$accent_red,
  # local = FALSE: fonts are linked from Google Fonts (see <head>) instead of
  # being downloaded at startup, so the app also starts offline
  base_font = font_google("IBM Plex Sans", local = FALSE),
  heading_font = font_google("IBM Plex Mono", local = FALSE),
  code_font = font_google("IBM Plex Mono", local = FALSE)
)

custom_css <- sprintf("
/* Base theme */
body {
  background: linear-gradient(135deg, %s 0%%, %s 50%%, %s 100%%);
  min-height: 100vh;
  font-family: 'IBM Plex Sans', sans-serif;
}

/* Header */
.main-header {
  background: linear-gradient(90deg, %s 0%%, %s 100%%);
  border-bottom: 1px solid %s;
  padding: 1.5rem 2rem;
  margin-bottom: 2rem;
}

.app-title {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 1.8rem;
  font-weight: 700;
  color: %s;
  letter-spacing: -0.5px;
  margin: 0;
}

.app-subtitle {
  font-size: 0.9rem;
  color: %s;
  margin-top: 0.25rem;
}

/* Cards */
.analysis-card {
  background: rgba(22, 27, 34, 0.95);
  border: 1px solid %s;
  border-radius: 12px;
  padding: 1.5rem;
  margin-bottom: 1.5rem;
  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.3);
  backdrop-filter: blur(10px);
  transition: all 0.3s ease;
}

.analysis-card:hover {
  border-color: %s;
  box-shadow: 0 8px 32px rgba(88, 166, 255, 0.15);
}

.card-header {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.85rem;
  font-weight: 600;
  color: %s;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  margin-bottom: 1rem;
  padding-bottom: 0.75rem;
  border-bottom: 1px solid %s;
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.card-header::before {
  content: '▸';
  color: %s;
}

/* Form controls */
.form-control, .selectize-input {
  background: %s !important;
  border: 1px solid %s !important;
  border-radius: 8px !important;
  color: %s !important;
  font-family: 'IBM Plex Sans', sans-serif !important;
  transition: all 0.2s ease !important;
}

.form-control:focus, .selectize-input.focus {
  border-color: %s !important;
  box-shadow: 0 0 0 3px rgba(88, 166, 255, 0.15) !important;
}

.selectize-dropdown {
  background: %s !important;
  border: 1px solid %s !important;
  border-radius: 8px !important;
}

.selectize-dropdown-content .option {
  color: %s !important;
  padding: 10px 12px !important;
}

.selectize-dropdown-content .option:hover,
.selectize-dropdown-content .option.active {
  background: %s !important;
  color: %s !important;
}

/* Button */
.btn-analysis {
  background: linear-gradient(135deg, #238636 0%%, #2ea043 100%%);
  border: none;
  border-radius: 8px;
  color: #ffffff;
  font-family: 'IBM Plex Mono', monospace;
  font-weight: 600;
  font-size: 0.9rem;
  padding: 12px 24px;
  letter-spacing: 0.5px;
  transition: all 0.3s ease;
  width: 100%%;
  margin-top: 1rem;
}

.btn-analysis:hover {
  background: linear-gradient(135deg, #2ea043 0%%, #3fb950 100%%);
  transform: translateY(-2px);
  box-shadow: 0 4px 20px rgba(46, 160, 67, 0.4);
}

.btn-analysis:active { transform: translateY(0); }

/* Inline radio buttons for scale toggle */
.card-header .shiny-input-radiogroup {
  margin: 0;
}

.card-header .shiny-input-radiogroup .shiny-options-group {
  display: flex;
  gap: 0.5rem;
}

.card-header .shiny-input-radiogroup label.radio-inline {
  padding: 0.25rem 0.75rem;
  margin: 0;
  font-size: 0.75rem;
  font-weight: 500;
  background: %s;
  border: 1px solid %s;
  border-radius: 4px;
  cursor: pointer;
  transition: all 0.2s ease;
}

.card-header .shiny-input-radiogroup label.radio-inline:hover {
  border-color: %s;
}

.card-header .shiny-input-radiogroup input[type='radio']:checked + span {
  color: %s;
}

.card-header .shiny-input-radiogroup label.radio-inline:has(input:checked) {
  background: %s;
  border-color: %s;
}

/* Result section */
.result-section {
  background: linear-gradient(135deg, %s 0%%, %s 100%%);
  border: 1px solid %s;
  border-radius: 12px;
  padding: 1.5rem;
  margin-bottom: 1.5rem;
}

.equation-display {
  background: %s;
  border: 1px solid %s;
  border-radius: 8px;
  padding: 1.25rem;
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.95rem;
  color: %s;
  overflow-x: auto;
  white-space: pre-wrap;
  word-break: break-all;
  line-height: 1.8;
}

/* Metrics grid */
.metric-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
  gap: 1rem;
  margin-top: 1rem;
}

.metric-box {
  background: linear-gradient(135deg, %s 0%%, %s 100%%);
  border: 1px solid %s;
  border-radius: 10px;
  padding: 1.25rem;
  text-align: center;
  transition: all 0.3s ease;
}

.metric-box:hover {
  border-color: %s;
  transform: translateY(-3px);
}

.metric-label {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.7rem;
  color: %s;
  text-transform: uppercase;
  letter-spacing: 1px;
  margin-bottom: 0.5rem;
}

.metric-value {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 1.5rem;
  font-weight: 700;
  color: %s;
}

.metric-value.success { color: %s; }
.metric-value.warning { color: %s; }
.metric-value.danger { color: %s; }

/* Tables */
.dataTables_wrapper { font-family: 'IBM Plex Sans', sans-serif; }

table.dataTable {
  background: transparent !important;
  border-collapse: separate !important;
  border-spacing: 0 4px !important;
}

table.dataTable thead th {
  background: %s !important;
  color: %s !important;
  font-family: 'IBM Plex Mono', monospace !important;
  font-size: 0.8rem !important;
  font-weight: 600 !important;
  text-transform: uppercase !important;
  letter-spacing: 1px !important;
  border: none !important;
  padding: 12px 16px !important;
}

table.dataTable tbody td {
  background: %s !important;
  color: %s !important;
  border: none !important;
  padding: 12px 16px !important;
}

table.dataTable tbody tr:hover td { background: %s !important; }

/* Notifications */
.shiny-notification {
  background: %s !important;
  border: 1px solid %s !important;
  border-radius: 8px !important;
  color: %s !important;
}

/* Scrollbar */
::-webkit-scrollbar { width: 8px; height: 8px; }
::-webkit-scrollbar-track { background: %s; }
::-webkit-scrollbar-thumb { background: %s; border-radius: 4px; }
::-webkit-scrollbar-thumb:hover { background: #484f58; }

/* Plot container */
.plot-container {
  background: %s;
  border: 1px solid %s;
  border-radius: 8px;
  padding: 1rem;
}

/* Tabs */
.nav-tabs { border-bottom: 1px solid %s; }

.nav-tabs .nav-link {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.85rem;
  color: %s;
  border: none;
  padding: 0.75rem 1.25rem;
  transition: all 0.2s ease;
}

.nav-tabs .nav-link:hover {
  color: %s;
  border-bottom: 2px solid #484f58;
}

.nav-tabs .nav-link.active {
  background: transparent;
  color: %s;
  border-bottom: 2px solid %s;
}

/* Labels */
.control-label {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.8rem;
  color: %s;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  margin-bottom: 0.5rem;
}

/* Responsive */
@media (max-width: 768px) {
  .metric-grid { grid-template-columns: repeat(2, 1fr); }
  .app-title { font-size: 1.4rem; }
}
",
  # Template arguments in order of appearance
  COLORS$bg_primary, COLORS$bg_secondary, COLORS$bg_primary,
  COLORS$bg_secondary, COLORS$bg_tertiary, COLORS$border,
  COLORS$accent_blue, COLORS$text_muted, COLORS$border, COLORS$accent_blue,
  COLORS$accent_blue, COLORS$border, COLORS$accent_green,
  COLORS$bg_primary, COLORS$border, COLORS$text_primary, COLORS$accent_blue,
  COLORS$bg_secondary, COLORS$border, COLORS$text_primary,
  COLORS$bg_tertiary, COLORS$accent_blue,
  # Radio button scale toggle
  COLORS$bg_tertiary, COLORS$border, COLORS$accent_blue,
  COLORS$accent_blue, COLORS$accent_blue, COLORS$accent_blue,
  COLORS$bg_secondary, COLORS$bg_primary, COLORS$border,
  COLORS$bg_primary, COLORS$border, COLORS$link_blue,
  COLORS$bg_tertiary, COLORS$bg_secondary, COLORS$border, COLORS$accent_blue,
  COLORS$text_muted, COLORS$accent_blue,
  COLORS$accent_green, COLORS$accent_orange, COLORS$accent_red,
  COLORS$bg_tertiary, COLORS$text_muted,
  COLORS$bg_secondary, COLORS$text_primary, COLORS$bg_tertiary,
  COLORS$bg_tertiary, COLORS$border, COLORS$text_primary,
  COLORS$bg_primary, COLORS$border,
  COLORS$bg_primary, COLORS$border, COLORS$border,
  COLORS$text_muted, COLORS$text_primary, COLORS$accent_blue, COLORS$accent_blue,
  COLORS$text_muted
)

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Encode categorical (character/factor) columns as dummy variables
#' Uses one-hot encoding with first level dropped as reference.
#' Numeric columns with <= max_levels unique values and all integers
#' are NOT encoded (treated as numeric but detected later by BO as discrete).
#' @param df Data frame (may contain character, factor, and numeric columns)
#' @param max_levels Maximum number of unique levels to encode (higher → skip)
#' @return List with:
#'   - data: Modified data frame (original categorical columns removed, dummies added)
#'   - mapping: Named list of { dummy_cols, dummy_levels, reference, levels } per encoded column
#'   - messages: Human-readable description of what was done
encode_categorical <- function(df, max_levels = 10) {
  mapping <- list()
  messages <- character(0)
  cols_to_remove <- character(0)

  for (col in names(df)) {
    vals <- df[[col]]
    # Only process non-numeric columns (character or factor)
    if (is.numeric(vals)) next

    levels_raw <- unique(as.character(vals))
    levels_raw <- levels_raw[!is.na(levels_raw)]
    n_levels <- length(levels_raw)

    if (n_levels < 2) {
      # Constant or single-level → remove
      cols_to_remove <- c(cols_to_remove, col)
      messages <- c(messages, sprintf("「%s」: 水準1個のため除外", col))
      next
    }

    if (n_levels > max_levels) {
      # Too many levels → remove
      cols_to_remove <- c(cols_to_remove, col)
      messages <- c(messages, sprintf("「%s」: 水準%d個(上限%d)のため除外", col, n_levels, max_levels))
      next
    }

    # Sort levels for deterministic ordering
    levels_sorted <- sort(levels_raw)
    reference <- levels_sorted[1]
    dummy_names <- character(0)
    chr_vals <- as.character(vals)

    # Create one dummy per non-reference level
    for (lvl in levels_sorted[-1]) {
      dummy_name <- paste0(col, "_", lvl)
      # Avoid name collision
      while (dummy_name %in% names(df)) {
        dummy_name <- paste0(dummy_name, "_d")
      }
      df[[dummy_name]] <- as.integer(chr_vals == lvl)
      dummy_names <- c(dummy_names, dummy_name)
    }

    mapping[[col]] <- list(
      dummy_cols = dummy_names,
      dummy_levels = levels_sorted[-1],  # level per dummy column (names may be de-duplicated)
      reference = reference,
      levels = levels_sorted
    )
    cols_to_remove <- c(cols_to_remove, col)
    messages <- c(messages,
      sprintf("「%s」→ ダミー変数化 (%s, 基準水準: %s)",
              col, paste(dummy_names, collapse = ", "), reference))
  }

  # Remove original categorical columns
  if (length(cols_to_remove) > 0) {
    df <- df[, !(names(df) %in% cols_to_remove), drop = FALSE]
  }

  list(data = df, mapping = mapping, messages = messages)
}

#' Make column names usable as unique display/input keys
#' Names may contain any characters (they never reach a formula), but they must
#' be non-empty and unique.
#' @param df Data frame
#' @return Data frame with cleaned names
sanitize_column_names <- function(df) {
  nm <- trimws(names(df))
  nm[is.na(nm) | nm == ""] <- paste0("V", which(is.na(nm) | nm == ""))
  names(df) <- make.unique(nm, sep = "_")
  df
}

#' Validate numeric data for RSM analysis
#' @param X Matrix of predictors
#' @param y Response vector
#' @return List with validated data and diagnostics
validate_data <- function(X, y) {
  issues <- character(0)
  if (!is.matrix(X)) X <- as.matrix(X)
  storage.mode(X) <- "double"
  y <- suppressWarnings(as.numeric(y))

  # Check for complete cases (Inf/-Inf/NaN are treated as missing)
  complete_idx <- rowSums(!is.finite(X)) == 0 & is.finite(y)
  n_missing <- sum(!complete_idx)

  if (n_missing > 0) {
    issues <- c(issues, sprintf("%d行の欠損値を除外", n_missing))
    X <- X[complete_idx, , drop = FALSE]
    y <- y[complete_idx]
  }

  # Check for constant columns (handle NA from single observation)
  const_cols <- vapply(seq_len(ncol(X)), function(i) {
    col <- X[, i]
    length(col) < 2 || all(col == col[1])
  }, logical(1))
  if (any(const_cols)) {
    const_names <- colnames(X)[const_cols]
    issues <- c(issues, sprintf("定数列を検出: %s", paste(const_names, collapse = ", ")))
    X <- X[, !const_cols, drop = FALSE]
  }

  # Check minimum sample size
  if (nrow(X) < MIN_SAMPLE_SIZE) {
    issues <- c(issues, sprintf("サンプルサイズが小さすぎます（n=%d, 推奨: n≥%d）", nrow(X), MIN_SAMPLE_SIZE))
  }

  # Check for highly correlated columns
  if (ncol(X) > 1 && nrow(X) > 1) {
    cor_mat <- tryCatch(suppressWarnings(cor(X)), error = function(e) NULL)
    if (!is.null(cor_mat)) {
      diag(cor_mat) <- 0
      # Handle NaN from singular columns
      if (any(!is.finite(cor_mat))) {
        issues <- c(issues, "完全共線性の変数ペアを検出")
      } else {
        high_cor <- which(abs(cor_mat) > 0.99, arr.ind = TRUE)
        if (nrow(high_cor) > 0) {
          issues <- c(issues, "高相関変数ペアを検出（|r| > 0.99）")
        }
      }
    }
  }

  list(
    X = X,
    y = y,
    n = nrow(X),
    p = ncol(X),
    issues = issues,
    valid = nrow(X) > 0 && ncol(X) >= MIN_EXPLANATORY_VARS
  )
}

#' Extract interaction and quadratic coefficients as a tidy data frame (vectorized)
#' @param int_mat Interaction matrix from RSM (diagonal = quadratic, off-diagonal = interaction)
#' @param var_names Variable names
#' @param threshold Minimum absolute value to include
#' @return Data frame with columns: var1, var2, coefficient, type
#'         type is "quadratic" (var1==var2) or "interaction" (var1!=var2)
extract_interactions <- function(int_mat, var_names, threshold = DISPLAY_THRESHOLD) {
  # Return properly structured empty data.frame
  empty_result <- data.frame(
    var1 = character(0),
    var2 = character(0),
    coefficient = numeric(0),
    type = character(0),
    stringsAsFactors = FALSE
  )

  n <- nrow(int_mat)
  if (is.null(n) || n < 1) return(empty_result)

  # Get upper triangle indices INCLUDING diagonal (for quadratic terms)
  idx <- which(upper.tri(int_mat, diag = TRUE), arr.ind = TRUE)
  coefficients <- int_mat[upper.tri(int_mat, diag = TRUE)]

  # Filter by threshold
  keep <- abs(coefficients) > threshold

  if (!any(keep)) return(empty_result)

  # Determine type: diagonal = quadratic, off-diagonal = interaction
  is_quadratic <- idx[keep, 1] == idx[keep, 2]

  data.frame(
    var1 = var_names[idx[keep, 1]],
    var2 = var_names[idx[keep, 2]],
    coefficient = coefficients[keep],
    type = ifelse(is_quadratic, "quadratic", "interaction"),
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(desc(abs(coefficient)))
}

#' Format a number with appropriate precision based on its magnitude
#' @param x Numeric value to format
#' @param sig_digits Number of significant digits (default: 4)
#' @return Formatted string
smart_format <- function(x, sig_digits = 4) {
  if (is.na(x) || !is.finite(x)) return("NA")
  if (x == 0) return("0")

  # Determine order of magnitude
  magnitude <- floor(log10(abs(x)))

  # Calculate decimal places needed for sig_digits significant figures
  if (magnitude >= sig_digits - 1) {
    # Large numbers: use integer or 1 decimal
    decimals <- max(0, sig_digits - magnitude - 1)
  } else if (magnitude >= 0) {
    # Numbers >= 1: show sig_digits - (magnitude + 1) decimals
    decimals <- sig_digits - magnitude - 1
  } else {
    # Numbers < 1: need more decimals to show significant digits
    decimals <- sig_digits - magnitude - 1
  }

  # Clamp decimals to reasonable range
 decimals <- max(0, min(decimals, 10))

  sprintf(paste0("%.", decimals, "f"), x)
}

#' Build regression equation string
#' @param intercept Intercept value
#' @param main_effects Named vector of main effects
#' @param int_mat Interaction matrix
#' @param var_names Variable names
#' @param target_name Target variable name
#' @return Character string of equation
build_equation <- function(intercept, main_effects, int_mat, var_names, target_name,
                          scale = "original", mx = NULL, is_dummy = NULL) {
  # Original-scale factor as used by the model: dummies enter interactions
  # raw (0/1), continuous variables centered as (X - mean)
  centered_factor <- function(v) {
    if (!is.null(is_dummy) && isTRUE(is_dummy[v])) return(v)
    m <- mx[[v]]
    if (m < 0) sprintf("(%s + %s)", v, smart_format(abs(m))) else sprintf("(%s - %s)", v, smart_format(m))
  }

  # For standardized scale, intercept is 0 (Y is centered), so don't display it
  # For original scale, always show intercept
  if (abs(intercept) < DISPLAY_THRESHOLD) {
    terms <- ""
    first_term <- TRUE
  } else {
    terms <- sprintf("%.4f", intercept)
    first_term <- FALSE
  }

  # Main effects (NOT centered for original scale)
  active_main <- abs(main_effects) > DISPLAY_THRESHOLD
  for (i in which(active_main)) {
    coef <- main_effects[i]
    var_name <- names(main_effects)[i]
    # Main effects: always just β × X (no centering)
    if (first_term) {
      # First term: no leading sign for positive, "-" for negative
      if (coef < 0) {
        terms <- paste0("-", sprintf("%.4f", abs(coef)), " × ", var_name)
      } else {
        terms <- paste0(sprintf("%.4f", coef), " × ", var_name)
      }
      first_term <- FALSE
    } else {
      sign <- if (coef > 0) " + " else " - "
      terms <- paste0(terms, sign, sprintf("%.4f", abs(coef)), " × ", var_name)
    }
  }

  # Interactions and quadratic terms
  # Model uses CENTERED terms: (Xi - X̄i)(Xj - X̄j) and (Xi - X̄i)²
  interactions <- extract_interactions(int_mat, var_names, DISPLAY_THRESHOLD)
  for (i in seq_len(nrow(interactions))) {
    coef <- interactions$coefficient[i]
    v1 <- interactions$var1[i]
    v2 <- interactions$var2[i]
    is_quadratic <- (interactions$type[i] == "quadratic")

    # Show centered form for quadratic terms (matching the actual model)
    if (scale == "original" && !is.null(mx)) {
      # Original scale: show (Xi - mean) form with actual mean values
      # Use smart_format to handle different data scales (e.g., 0.001 vs 1000)
      # Avoid double minus: (X - -2.5) → (X + 2.5)
      if (is_quadratic) {
        term <- paste0(centered_factor(v1), "²")
      } else {
        f1 <- centered_factor(v1)
        f2 <- centered_factor(v2)
        # Join with "·" when a raw dummy name is involved, for readability
        term <- if (identical(f1, v1) || identical(f2, v2)) paste0(f1, "·", f2) else paste0(f1, f2)
      }
    } else {
      # Standardized scale: X is already centered (mean=0), so just show X·X or X²
      if (is_quadratic) {
        term <- paste0(v1, "²")
      } else {
        term <- paste0(v1, "·", v2)
      }
    }

    if (first_term) {
      if (coef < 0) {
        terms <- paste0("-", sprintf("%.4f", abs(coef)), " × ", term)
      } else {
        terms <- paste0(sprintf("%.4f", coef), " × ", term)
      }
      first_term <- FALSE
    } else {
      sign <- if (coef > 0) " + " else " - "
      terms <- paste0(terms, sign, sprintf("%.4f", abs(coef)), " × ", term)
    }
  }

  # Handle edge case: no terms selected
  if (terms == "") {
    terms <- "0"
  }

  paste0(target_name, " = ", terms)
}

#' Compute model metrics with robust edge case handling
#' @param y Actual values
#' @param predictions Predicted values
#' @param n_params Number of parameters (excluding intercept)
#' @return List of metrics
compute_metrics <- function(y, predictions, n_params) {
  n <- length(y)
  residuals <- y - predictions

  ss_res <- sum(residuals^2)
  ss_tot <- sum((y - mean(y))^2)

  # Handle zero variance in response
  if (ss_tot < .Machine$double.eps) {
    warning("目的変数の分散がゼロです")
    r_squared <- NA_real_
  } else {
    r_squared <- 1 - ss_res / ss_tot
    # Note: R² can be negative (model worse than mean), do NOT clamp
  }

  # Adjusted R² with proper degrees of freedom
  df_res <- n - n_params - 1
  adj_r_squared <- if (df_res > 0 && !is.na(r_squared)) {
    1 - (1 - r_squared) * (n - 1) / df_res
  } else {
    NA_real_
  }

  # MAPE: handle individual zeros gracefully
  non_zero_idx <- y != 0
  mape <- if (sum(non_zero_idx) > 0) {
    mean(abs(residuals[non_zero_idx] / y[non_zero_idx])) * 100
  } else {
    NA_real_
  }

  list(
    r_squared = r_squared,
    adj_r_squared = adj_r_squared,
    rmse = sqrt(mean(residuals^2)),
    mae = mean(abs(residuals)),
    mape = mape,
    max_error = max(abs(residuals)),
    residuals = residuals,
    n = n,
    df_residual = df_res
  )
}

#' Count active coefficients
#' @param main_effects Main effect vector
#' @param int_mat Interaction matrix (diagonal = quadratic, off-diagonal = interaction)
#' @return List with counts:
#'   - n_main: number of main effects
#'   - n_interaction: number of interaction terms (Xi*Xj, i≠j)
#'   - n_quadratic: number of quadratic terms (Xi²)
#'   - n_higher_order: n_interaction + n_quadratic (for UI display)
count_active_coefficients <- function(main_effects, int_mat) {
  n_main <- sum(abs(main_effects) > COEF_ZERO_THRESHOLD)
  # Interaction terms (off-diagonal, upper triangle only to avoid double counting)
  n_interaction <- sum(abs(int_mat[upper.tri(int_mat, diag = FALSE)]) > COEF_ZERO_THRESHOLD)
  # Quadratic terms (diagonal)
  n_quadratic <- sum(abs(diag(int_mat)) > COEF_ZERO_THRESHOLD)

  list(
    n_main = n_main,
    n_interaction = n_interaction,
    n_quadratic = n_quadratic,
    n_higher_order = n_interaction + n_quadratic  # For UI: "交互作用/二次項数"
  )
}

#' Get R² color class
#' @param r2 R-squared value
#' @return CSS class name
get_r2_class <- function(r2) {
  if (is.na(r2) || r2 >= R2_EXCELLENT) "success"
  else if (r2 >= R2_GOOD) "warning"
  else "danger"
}

#' Create ggplot theme for RSM
#' @return ggplot theme object
theme_rsm <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_line(color = COLORS$bg_tertiary),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = COLORS$text_muted),
      axis.title = element_text(color = COLORS$text_primary),
      legend.background = element_rect(fill = COLORS$bg_secondary, color = COLORS$border),
      legend.text = element_text(color = COLORS$text_primary),
      legend.title = element_text(color = COLORS$text_primary)
    )
}

#' Configure plotly layout for RSM
#' @param p plotly object
#' @return Configured plotly object
layout_rsm <- function(p) {
  p |>
    layout(
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent",
      font = list(color = COLORS$text_primary)
    ) |>
    config(displayModeBar = FALSE)
}

# ============================================================================
# DATA GENERATION
# ============================================================================

#' Generate sample data with specified pattern
#' @param pattern One of: main_linear, strong_interact, weak_interact, quadratic, low_signal_noise, categorical
#' @param n Number of observations
#' @param seed Random seed
#' @return Data frame
generate_sample_data <- function(
    pattern = c("main_linear", "strong_interact", "weak_interact", "quadratic", "low_signal_noise", "categorical"),
    n = DEFAULT_SAMPLE_SIZE,
    seed = RANDOM_SEED
) {
  pattern <- match.arg(pattern)
  set.seed(seed)

  # Generate predictors
  X <- matrix(rnorm(n * 5), ncol = 5)
  colnames(X) <- c("X1", "X2", "X3", "X4", "X5")
  eps <- rnorm(n, 0, 1)

  # Categorical pattern: demonstrates a categorical (character) column that
  # encode_categorical() will dummy-encode, with a real main effect AND an
  # interaction with a continuous variable (slope differs per category).
  if (pattern == "categorical") {
    cat_levels <- c("A", "B", "C")
    catalyst <- sample(cat_levels, n, replace = TRUE)
    cat_intercept <- c(A = 0, B = 2.5, C = -1.8)
    cat_slope_mod <- c(A = 0, B = 0.5, C = -0.8)  # modifies 温度(X1) slope per level

    Y <- 5 +
      2.0 * X[, 1] + 1.2 * X[, 2] + 0.5 * X[, 3] +
      cat_intercept[catalyst] +
      cat_slope_mod[catalyst] * X[, 1] +
      rnorm(n, 0, 1)

    return(data.frame(
      パターン = pattern,
      品質スコア = round(as.vector(Y), 2),
      温度 = round(X[, 1], 3),
      圧力 = round(X[, 2], 3),
      速度 = round(X[, 3], 3),
      触媒 = catalyst,
      湿度 = round(X[, 5], 3),
      stringsAsFactors = FALSE
    ))
  }

  # Define coefficients based on pattern
  coeffs <- switch(
    pattern,
    main_linear = list(
      intercept = 5,
      main = c(2.0, 1.5, 0.8, 0.3, 0.0),
      int12 = 0, int23 = 0, quad1 = 0, quad2 = 0,
      noise_sd = 1
    ),
    strong_interact = list(
      intercept = 5,
      main = c(1.5, 1.5, 0.5, 0.0, 0.0),
      int12 = 1.8, int23 = 0.8, quad1 = 0, quad2 = 0,
      noise_sd = 1
    ),
    weak_interact = list(
      intercept = 5,
      main = c(0.0, 1.5, 0.0, 0.0, 0.0),
      int12 = 2.0, int23 = 0, quad1 = 0, quad2 = 0,
      noise_sd = 1
    ),
    quadratic = list(
      intercept = 10,
      main = c(0.5, -0.5, 0.0, 0.0, 0.0),
      int12 = 1.0, int23 = 0, quad1 = 2.0, quad2 = -1.5,
      noise_sd = 1
    ),
    low_signal_noise = list(
      intercept = 50,
      main = c(0.1, -0.1, 0.05, 0.0, 0.0),
      int12 = 0, int23 = 0, quad1 = 0, quad2 = 0,
      noise_sd = 5
    )
  )

  # Compute response (explicit vectorization for matrix multiplication)
  Y <- coeffs$intercept +
    as.vector(X %*% coeffs$main) +
    coeffs$int12 * X[, 1] * X[, 2] +
    coeffs$int23 * X[, 2] * X[, 3] +
    coeffs$quad1 * X[, 1]^2 +
    coeffs$quad2 * X[, 2]^2 +
    rnorm(n, 0, coeffs$noise_sd)

  data.frame(
    パターン = pattern,
    品質スコア = round(as.vector(Y), 2),
    温度 = round(X[, 1], 3),
    圧力 = round(X[, 2], 3),
    速度 = round(X[, 3], 3),
    材料硬度 = round(X[, 4], 3),
    湿度 = round(X[, 5], 3)
  )
}

# ============================================================================
# MODEL TERMS (design matrix)
# ============================================================================
# Every model term is a single numeric column computed directly from the
# predictor matrix. Variables are referenced by column INDEX, never by name,
# so arbitrary user column names (spaces, symbols, "x" next to "x_c", ...)
# can never break a formula or collide with helper columns.
#   main : X_i                      (not centered)
#   int  : Z_i * Z_j                (Z = X - mean for continuous, raw 0/1 for dummies)
#   quad : (X_i - mean_i)^2         (continuous variables only)

#' Detect 0/1 dummy columns
#' @param X Numeric matrix
#' @return Named logical vector
detect_dummies <- function(X) {
  out <- vapply(seq_len(ncol(X)), function(i) {
    vals <- unique(X[, i])
    vals <- vals[!is.na(vals)]
    length(vals) <= 2 && all(vals %in% c(0, 1))
  }, logical(1))
  names(out) <- colnames(X)
  out
}

#' Enumerate candidate model terms
#' Order matches the classic layout: main effects, then interactions (i-major),
#' then quadratic terms.
#' @param var_names Variable names (used for display labels only)
#' @param is_dummy Logical vector: 0/1 dummy columns
#' @param is_binary Logical vector: columns with <= 2 distinct values
#'   (their square is collinear with the main effect, so no quadratic term)
#' @param dummy_group Character vector: categorical group per variable (or NA)
#' @param model_type "interaction" or "quadratic"
#' @return data.frame(type, i, j, label)
build_term_specs <- function(var_names, is_dummy, is_binary, dummy_group, model_type) {
  p <- length(var_names)
  main <- data.frame(type = rep("main", p), i = seq_len(p), j = seq_len(p),
                     stringsAsFactors = FALSE)
  int <- NULL
  if (p >= 2) {
    ii <- rep(seq_len(p - 1), (p - 1):1)
    jj <- unlist(lapply(seq_len(p - 1), function(i) (i + 1):p))
    # Dummies of the SAME categorical variable are mutually exclusive:
    # their product is identically 0, so that interaction is skipped
    same_grp <- !is.na(dummy_group[ii]) & !is.na(dummy_group[jj]) &
      dummy_group[ii] == dummy_group[jj]
    int <- data.frame(type = "int", i = ii[!same_grp], j = jj[!same_grp],
                      stringsAsFactors = FALSE)
  }
  quad <- NULL
  if (model_type == "quadratic") {
    qi <- which(!is_binary)
    quad <- data.frame(type = rep("quad", length(qi)), i = qi, j = qi,
                       stringsAsFactors = FALSE)
  }
  specs <- rbind(main, int, quad)
  specs$label <- ifelse(
    specs$type == "main", var_names[specs$i],
    ifelse(specs$type == "quad", paste0(var_names[specs$i], "²"),
           paste0(var_names[specs$i], " × ", var_names[specs$j]))
  )
  rownames(specs) <- NULL
  specs
}

#' Build the term (design) matrix, without intercept
#' @param X Numeric matrix of predictors (n x p)
#' @param specs Term specification from build_term_specs()
#' @param mx Column means used for centering
#' @param is_dummy Logical vector of dummy columns (not centered in interactions)
#' @return n x nrow(specs) numeric matrix
build_term_matrix <- function(X, specs, mx, is_dummy) {
  Xc <- sweep(X, 2, mx, "-", check.margin = FALSE)
  Z <- Xc
  if (any(is_dummy)) Z[, is_dummy] <- X[, is_dummy]
  M <- matrix(0, nrow(X), nrow(specs))
  m <- specs$type == "main"
  t <- specs$type == "int"
  q <- specs$type == "quad"
  if (any(m)) M[, m] <- X[, specs$i[m], drop = FALSE]
  if (any(t)) M[, t] <- Z[, specs$i[t], drop = FALSE] * Z[, specs$j[t], drop = FALSE]
  if (any(q)) M[, q] <- Xc[, specs$i[q], drop = FALSE]^2
  M
}

# ============================================================================
# STEPWISE VARIABLE SELECTION (逐次減増法)
# ============================================================================
# Partial F-tests are computed directly from a QR decomposition of the design
# matrix. Results are identical to drop1(fit, test = "F") / add1(fit, test = "F")
# (Type II SS, same rank tolerance 1e-7 as lm), but avoid re-parsing formulas
# and refitting the model once per term.

QR_TOL <- 1e-7

#' Partial F-ratios for removing each selected term (≡ drop1(test = "F"))
#' @param M Full term matrix
#' @param y Response
#' @param sel Indices (columns of M) currently in the model
#' @return Numeric vector of F-ratios aligned with sel (NA/NaN → 0)
partial_f_drop <- function(M, y, sel) {
  k <- length(sel)
  Xd <- cbind(1, M[, sel, drop = FALSE])
  q <- qr(Xd, tol = QR_TOL)
  rss <- sum(qr.resid(q, y)^2)
  rms <- rss / (length(y) - q$rank)

  if (q$rank == k + 1) {
    # Full rank: RSS increase from dropping term j = b_j^2 / [(X'X)^-1]_jj
    R <- qr.R(q)
    Rinv <- backsolve(R, diag(k + 1))
    b_piv <- backsolve(R, qr.qty(q, y)[seq_len(k + 1)])
    dev <- numeric(k + 1)
    dev[q$pivot] <- b_piv^2 / rowSums(Rinv^2)
    dev <- dev[-1]
  } else {
    # Rank deficient: refit without each term (dropping an aliased term → NA)
    dev <- vapply(seq_len(k), function(t) {
      q2 <- qr(Xd[, -(t + 1), drop = FALSE], tol = QR_TOL)
      d_rank <- q$rank - q2$rank
      if (d_rank < 1e-4) return(NA_real_)
      (sum(qr.resid(q2, y)^2) - rss) / d_rank
    }, numeric(1))
  }
  f <- dev / rms
  f[is.na(f)] <- 0
  f
}

#' Partial F-ratios for adding each candidate term (≡ add1(test = "F"))
#' @param M Full term matrix
#' @param y Response
#' @param sel Indices currently in the model
#' @param cand Candidate indices
#' @return Numeric vector of F-ratios aligned with cand (aliased/NA → 0)
partial_f_add <- function(M, y, sel, cand) {
  q <- qr(cbind(1, M[, sel, drop = FALSE]), tol = QR_TOL)
  e <- qr.resid(q, y)
  rss0 <- sum(e^2)
  rdf <- length(y) - q$rank
  C <- M[, cand, drop = FALSE]
  Rc <- qr.resid(q, C)
  rr <- colSums(Rc^2)
  # Same aliasing rule as lm's QR: residual norm < tol * original norm
  aliased <- sqrt(rr) < QR_TOL * sqrt(colSums(C^2))
  dev <- colSums(Rc * e)^2 / rr
  f <- dev / ((rss0 - dev) / (rdf - 1))
  f[aliased | is.na(f)] <- 0
  f
}

#' Perform stepwise variable selection (逐次減増法)
#' Starts from the full model; each iteration removes the weakest term
#' (F <= f_out) and then adds back removed terms while F > f_in.
#' Optionally enforces the hierarchy principle.
#' @param M Full term matrix (n x T)
#' @param y Response vector
#' @param specs Term specifications (nrow = T)
#' @param model_type "interaction" or "quadratic"
#' @param f_in F-ratio threshold for entry
#' @param f_out F-ratio threshold for removal
#' @param enforce_hierarchy Whether to enforce hierarchy principle
#' @param max_iter Safety cap on iterations (oscillation detection also stops the loop)
#' @return List with selected (term indices), removed, history, n_iterations
stepwise_selection <- function(
    M, y, specs,
    model_type = "quadratic",
    f_in = 2,
    f_out = 2,
    enforce_hierarchy = TRUE,
    max_iter = max(100, 3 * nrow(specs))
) {
  n_terms <- nrow(specs)
  labels <- specs$label
  is_main <- specs$type == "main"
  # Main-effect term index for each variable (main terms are rows 1..p)
  main_of <- function(v) v

  selected <- seq_len(n_terms)
  removed <- integer(0)
  history <- list()
  iteration <- 0
  state_history <- new.env(hash = TRUE, parent = emptyenv())

  add_history <- function(action, term, f) {
    history[[length(history) + 1]] <<- list(
      iteration = iteration, action = action, term = labels[term], f_ratio = f
    )
  }

  # A main effect is required if any selected higher-order term contains it
  is_required <- function(v, sel) {
    hi <- sel[!is_main[sel]]
    any(specs$i[hi] == v | specs$j[hi] == v)
  }

  message(sprintf("=== 逐次減増法 開始 (部分F検定使用): %d 項 ===", n_terms))

  while (iteration < max_iter) {
    iteration <- iteration + 1
    changed <- FALSE

    # Oscillation detection (same term set seen before)
    state <- paste(sort(selected), collapse = ",")
    if (!is.null(state_history[[state]])) {
      message(sprintf("  振動検出 - 終了 (反復 %d 回)", iteration))
      break
    }
    state_history[[state]] <- TRUE

    # --- BACKWARD STEP: remove the weakest removable term ---
    if (length(selected) > 1) {
      f <- partial_f_drop(M, y, selected)
      elig <- which(f <= f_out)
      if (length(elig) > 0) {
        # Stable ascending order of F (ties keep model order)
        elig <- elig[order(f[elig], method = "radix")]
        for (e in elig) {
          term <- selected[e]
          if (enforce_hierarchy && is_main[term] && is_required(specs$i[term], selected)) {
            next
          }
          selected <- selected[-e]
          removed <- c(removed, term)
          changed <- TRUE
          message(sprintf("  [%d] 削除: %s (部分F=%.3f)", iteration, labels[term], f[e]))
          add_history("remove", term, f[e])
          break
        }
      }
    }

    # --- FORWARD STEP: re-add removed terms while F > f_in ---
    if (iteration >= 2 && length(removed) > 0 && length(selected) > 0) {
      repeat {
        if (length(removed) == 0) break
        f <- partial_f_add(M, y, selected, removed)
        if (!any(f > f_in)) break
        best_pos <- which.max(ifelse(f > f_in, f, -Inf))
        best <- removed[best_pos]
        best_f <- f[best_pos]

        # Hierarchy: bring back main effects needed by the added term
        additions <- integer(0)
        if (enforce_hierarchy && !is_main[best]) {
          for (v in unique(c(specs$i[best], specs$j[best]))) {
            mt <- main_of(v)
            if (!(mt %in% selected) && mt %in% removed) additions <- c(additions, mt)
          }
        }

        to_add <- c(best, additions)
        selected <- c(selected, to_add)
        removed <- setdiff(removed, to_add)
        changed <- TRUE
        message(sprintf("  [%d] 追加: %s (部分F=%.3f)", iteration, labels[best], best_f))
        add_history("add", best, best_f)
        for (h in additions) add_history("hierarchy_add", h, NA_real_)
      }
    }

    if (!changed) {
      message(sprintf("  収束 (反復 %d 回)", iteration))
      break
    }
  }

  # Final hierarchy check (safety net)
  if (enforce_hierarchy) {
    if (model_type == "quadratic") {
      for (term in which(specs$type == "quad")) {
        mt <- main_of(specs$i[term])
        if (term %in% selected && !(mt %in% selected)) {
          selected <- c(mt, selected)
          removed <- setdiff(removed, mt)
          history[[length(history) + 1]] <- list(
            iteration = iteration + 1, action = "hierarchy_final",
            term = labels[mt], f_ratio = NA_real_
          )
        }
      }
    }
    for (term in selected[specs$type[selected] == "int"]) {
      for (v in c(specs$i[term], specs$j[term])) {
        mt <- main_of(v)
        if (!(mt %in% selected)) {
          selected <- c(mt, selected)
          removed <- setdiff(removed, mt)
          message(sprintf("  [最終階層] 強制追加: %s", labels[mt]))
        }
      }
    }
  }

  message(sprintf("=== 逐次減増法 終了: %d 項選択 ===", length(selected)))

  list(
    selected = selected,
    removed = removed,
    history = history,
    n_iterations = iteration
  )
}

# ============================================================================
# RSM ANALYSIS WRAPPER
# ============================================================================

#' Build a fast prediction function from a fitted model
#' Numerically identical to predict(fit, se.fit = TRUE), but works directly
#' on a numeric matrix of predictors (no model.frame / formula overhead).
#' @param fit lm object fitted on the term columns
#' @param specs Specs of the terms in the model (same order as the fit)
#' @param mx Training column means
#' @param is_dummy Dummy flags
#' @param var_names Variable names (column order of the input)
#' @return function(newX) -> list(mean, se, residual_se)
make_rsm_predictor <- function(fit, specs, mx, is_dummy, var_names) {
  b <- coef(fit)
  r <- fit$rank
  piv <- fit$qr$pivot[seq_len(r)]
  b_piv <- b[piv]
  R <- fit$qr$qr[seq_len(r), seq_len(r), drop = FALSE]
  R[lower.tri(R)] <- 0
  Rinv <- backsolve(R, diag(r))
  rdf <- fit$df.residual
  sigma <- if (rdf > 0) sqrt(sum(fit$residuals^2) / rdf) else NaN

  function(newX) {
    if (!is.matrix(newX)) newX <- as.matrix(newX)
    if (!is.null(colnames(newX))) newX <- newX[, var_names, drop = FALSE]
    storage.mode(newX) <- "double"
    D <- cbind(1, build_term_matrix(newX, specs, mx, is_dummy))[, piv, drop = FALSE]
    list(
      mean = drop(D %*% b_piv),
      se = sqrt(rowSums((D %*% Rinv)^2)) * sigma,
      residual_se = sigma
    )
  }
}

#' Perform RSM (Response Surface Methodology) analysis with stepwise selection
#' @param X Predictor matrix
#' @param y Response vector
#' @param model_type Type of model: "interaction" or "quadratic"
#' @param use_stepwise Whether to use stepwise variable selection
#' @param f_in F-ratio threshold for entry (default 2)
#' @param f_out F-ratio threshold for removal (default 2)
#' @param enforce_hierarchy Whether to enforce hierarchy principle (default TRUE)
#' @param categorical_mapping Mapping from encode_categorical()
#' @return List with fit, predictor and extracted results
run_rsm_analysis <- function(
    X, y,
    model_type = "quadratic",
    use_stepwise = TRUE,
    f_in = 2,
    f_out = 2,
    enforce_hierarchy = TRUE,
    categorical_mapping = NULL
) {
  if (!is.matrix(X)) X <- as.matrix(X)
  storage.mode(X) <- "double"
  y <- as.numeric(y)

  var_names <- colnames(X)
  p <- ncol(X)
  n <- nrow(X)

  if (n < p + 1) {
    stop(sprintf("サンプル数(%d)が変数数(%d)に対して不足しています", n, p))
  }

  # Scaling info
  mx <- colMeans(X)
  sx <- apply(X, 2, sd)
  sx[!is.finite(sx) | sx < .Machine$double.eps] <- 1
  my <- mean(y)
  sy <- sd(y)
  if (!is.finite(sy) || sy < .Machine$double.eps) sy <- 1

  X_scaled <- sweep(sweep(X, 2, mx, "-"), 2, sx, "/")
  y_scaled <- (y - my) / sy

  # Dummy detection and categorical groups
  is_dummy <- detect_dummies(X)
  is_binary <- apply(X, 2, function(col) length(unique(col)) <= 2)
  dummy_group <- rep(NA_character_, p)
  names(dummy_group) <- var_names
  for (cat_name in names(categorical_mapping)) {
    dv <- intersect(categorical_mapping[[cat_name]]$dummy_cols, var_names)
    dummy_group[dv] <- cat_name
  }

  specs <- build_term_specs(var_names, is_dummy, is_binary, dummy_group, model_type)
  M <- build_term_matrix(X, specs, mx, is_dummy)

  if (use_stepwise) {
    sw <- stepwise_selection(
      M, y, specs,
      model_type = model_type,
      f_in = f_in, f_out = f_out,
      enforce_hierarchy = enforce_hierarchy
    )
    sel <- sw$selected
    selection_history <- sw$history
  } else {
    sel <- seq_len(nrow(specs))
    selection_history <- list()
  }

  # Final fit on the selected term columns (columns named T<index>)
  term_ids <- paste0("T", sel)
  df_fit <- as.data.frame(M[, sel, drop = FALSE])
  names(df_fit) <- term_ids
  df_fit$Y <- y
  model_formula <- as.formula(
    if (length(sel) == 0) "Y ~ 1" else paste("Y ~", paste(term_ids, collapse = " + "))
  )
  fit_orig <- tryCatch(
    lm(model_formula, data = df_fit),
    error = function(e) stop(sprintf("モデルフィッティング失敗: %s", e$message))
  )

  # Standardized coefficients: same terms on fully standardized X and Y
  Ms <- build_term_matrix(X_scaled, specs[sel, , drop = FALSE], rep(0, p), is_dummy)
  qs <- qr(cbind(1, Ms), tol = QR_TOL)
  coef_scaled <- qr.coef(qs, y_scaled)

  coef_orig <- coef(fit_orig)
  intercept_orig <- unname(coef_orig[1])
  if (is.na(intercept_orig)) intercept_orig <- 0
  intercept_std <- unname(coef_scaled[1])
  if (is.na(intercept_std)) intercept_std <- 0

  b_orig <- unname(coef_orig[-1])
  b_std <- unname(coef_scaled[-1])
  b_orig[is.na(b_orig)] <- 0
  b_std[is.na(b_std)] <- 0

  sel_specs <- specs[sel, , drop = FALSE]
  main_effects_orig <- setNames(rep(0, p), var_names)
  main_effects_std <- main_effects_orig
  interaction_matrix_orig <- matrix(0, p, p, dimnames = list(var_names, var_names))
  interaction_matrix_std <- interaction_matrix_orig

  m <- sel_specs$type == "main"
  main_effects_orig[sel_specs$i[m]] <- b_orig[m]
  main_effects_std[sel_specs$i[m]] <- b_std[m]
  h <- !m
  if (any(h)) {
    idx <- cbind(sel_specs$i[h], sel_specs$j[h])
    interaction_matrix_orig[idx] <- b_orig[h]
    interaction_matrix_orig[idx[, 2:1, drop = FALSE]] <- b_orig[h]
    interaction_matrix_std[idx] <- b_std[h]
    interaction_matrix_std[idx[, 2:1, drop = FALSE]] <- b_std[h]
  }

  predictions <- unname(fitted(fit_orig))
  model_summary <- summary(fit_orig)
  coef_table <- model_summary$coefficients
  rn <- rownames(coef_table)
  lab <- c(`(Intercept)` = "(Intercept)", setNames(sel_specs$label, term_ids))
  rownames(coef_table) <- unname(lab[rn])

  list(
    fit = fit_orig,
    predictor = make_rsm_predictor(fit_orig, sel_specs, mx, is_dummy, var_names),
    model_summary = model_summary,
    # Standardized coefficients
    main_effects_std = main_effects_std,
    interaction_matrix_std = interaction_matrix_std,
    intercept_std = intercept_std,
    # Original scale coefficients
    main_effects_orig = main_effects_orig,
    interaction_matrix_orig = interaction_matrix_orig,
    intercept_orig = intercept_orig,
    # Scaling info (X and Y)
    mx = mx,
    sx = sx,
    my = my,
    sy = sy,
    is_dummy = is_dummy,
    # Legacy/fallback fields (used with %||% operator throughout UI code)
    main_effects = main_effects_std,
    interaction_matrix = interaction_matrix_std,
    intercept = intercept_orig,
    predictions = predictions,
    r_squared = model_summary$r.squared,
    adj_r_squared = model_summary$adj.r.squared,
    f_statistic = model_summary$fstatistic,
    coefficients_table = coef_table,
    n_params = fit_orig$rank - 1,
    # Stepwise selection info
    term_specs = specs,
    selected_index = sel,
    selected_terms = sel_specs$label,
    all_terms = specs$label,
    selection_history = selection_history
  )
}

# ============================================================================
# BAYESIAN OPTIMIZATION
# ============================================================================

#' Detect variable types: continuous, dummy (0/1), or discrete (few integer levels)
#' @param X Predictor matrix
#' @return Named character vector: "dummy", "discrete", or "continuous" per variable
detect_variable_types <- function(X) {
  sapply(colnames(X), function(v) {
    vals <- unique(X[, v])
    vals <- vals[!is.na(vals)]
    n_unique <- length(vals)
    # Dummy: exactly {0,1} or {-1,1}
    if (n_unique <= 2 && (all(vals %in% c(0, 1)) || all(vals %in% c(-1, 1)))) {
      return("dummy")
    }
    # Discrete: few unique integer values (e.g., factor levels 1,2,3)
    if (n_unique <= 5 && all(vals == round(vals))) {
      return("discrete")
    }
    "continuous"
  }, USE.NAMES = TRUE)
}

#' Enforce mutual exclusivity for categorical dummy groups
#' For each group of dummies from the same category, at most one can be 1.
#' @param x Named numeric vector
#' @param cat_groups List of integer vectors (column indices per group)
#' @param var_names Variable names for indexing
#' @return Corrected numeric vector
enforce_categorical_constraints <- function(x, cat_groups, var_names) {
  for (grp_idx in cat_groups) {
    grp_vals <- x[var_names[grp_idx]]
    if (sum(grp_vals >= 0.5) > 1) {
      winner <- grp_idx[which.max(grp_vals)]
      x[var_names[grp_idx]] <- 0
      x[var_names[winner]] <- 1
    }
  }
  x
}

#' Level name of each dummy column of a categorical variable
#' @param info One entry of categorical_mapping
#' @param dc Dummy column name(s)
dummy_level <- function(info, dc) {
  info$dummy_levels[match(dc, info$dummy_cols)]
}

#' Decode the active level of a categorical variable from a named 0/1 vector
#' @param info One entry of categorical_mapping
#' @param x Named numeric vector (or one-row list) containing the dummy columns
#' @return Level name (reference level when no dummy is 1)
decode_category <- function(info, x) {
  for (k in seq_along(info$dummy_cols)) {
    v <- x[[info$dummy_cols[k]]]
    if (!is.null(v) && !is.na(v) && v == 1) return(info$dummy_levels[k])
  }
  info$reference
}

#' Keep only categorical variables (and their dummies) used in the model
#' @param cm categorical_mapping
#' @param var_names Variables in the model
restrict_categorical_mapping <- function(cm, var_names) {
  out <- list()
  for (cat_name in names(cm)) {
    info <- cm[[cat_name]]
    keep <- info$dummy_cols %in% var_names
    if (!any(keep)) next
    info$dummy_cols <- info$dummy_cols[keep]
    info$dummy_levels <- info$dummy_levels[keep]
    info$levels <- c(info$reference, info$dummy_levels)
    out[[cat_name]] <- info
  }
  out
}

#' Partition a set of "other" (non-axis) variable names into complete
#' categorical groups vs. leftover raw variables (continuous/discrete/
#' partial-group dummies). Used by the Response Surface tab to render one
#' category selector per categorical variable instead of one raw 0/1 slider
#' per dummy column.
#' @param other_vars Character vector of variable names to partition
#' @param cm categorical_mapping list: cat_name -> list(dummy_cols, reference, levels)
#' @return List with:
#'   - cat_groups: named list, cat_name -> dummy_cols (all present in other_vars)
#'   - leftover: character vector of other_vars not covered by a complete group
partition_surface_vars <- function(other_vars, cm) {
  cat_groups <- list()
  covered <- character(0)
  for (cat_name in names(cm)) {
    dcols <- cm[[cat_name]]$dummy_cols
    if (length(dcols) > 0 && all(dcols %in% other_vars)) {
      cat_groups[[cat_name]] <- dcols
      covered <- c(covered, dcols)
    }
  }
  list(cat_groups = cat_groups, leftover = setdiff(other_vars, covered))
}

#' If var_name is a dummy column belonging to a categorical group, return
#' plotly tick positions/labels (0 -> reference level, 1 -> encoded level)
#' so Response Surface axes show category names instead of raw 0/1.
#' @param var_name Variable (column) name to check
#' @param cm categorical_mapping list
#' @return List(tickvals, ticktext) or NULL if var_name is not a dummy column
surface_axis_ticks <- function(var_name, cm) {
  for (cat_name in names(cm)) {
    info <- cm[[cat_name]]
    if (var_name %in% info$dummy_cols) {
      return(list(tickvals = c(0, 1), ticktext = c(info$reference, dummy_level(info, var_name))))
    }
  }
  NULL
}

#' Expected Improvement acquisition function
#' @param mu Predicted mean
#' @param sigma Predicted standard error (model uncertainty)
#' @param best_y Current best value
#' @param minimize If TRUE, minimize Y; if FALSE, maximize Y
#' @param target_value If not NULL, minimize (Y - target)^2 instead
#' @return EI values (always non-negative) or acquisition scores for target mode
expected_improvement <- function(mu, sigma, best_y, minimize = FALSE, target_value = NULL) {
  # Non-finite SE (e.g. no residual degrees of freedom) → pure exploitation
  sigma[!is.finite(sigma) | sigma < 1e-10] <- 1e-10

  if (!is.null(target_value)) {
    # Target mode: minimize (Y - target)^2
    # Point estimate of score + exploration bonus from uncertainty
    point_score <- (mu - target_value)^2
    improvement <- best_y - point_score  # best_y = best_score
    pmax(improvement + sigma^2, 0)
  } else {
    improvement <- if (minimize) best_y - mu else mu - best_y
    z <- improvement / sigma
    pmax(improvement * pnorm(z) + sigma * dnorm(z), 0)
  }
}

#' Run Bayesian Optimization using RSM regression as surrogate
#' @param predictor Prediction function from make_rsm_predictor()
#' @param X Original predictor matrix
#' @param var_names Variable names
#' @param n_iter Number of BO iterations
#' @param n_random Number of random candidates per iteration
#' @param minimize If TRUE, seek minimum; if FALSE, seek maximum
#' @param bounds 2 x p matrix (row 1 = lower, row 2 = upper). NULL = data range
#' @param categorical_mapping Categorical mapping (mutual exclusivity of dummies)
#' @param target_value If not NULL, minimize (Y - target)^2
#' @param patience Stop after this many consecutive iterations with negligible EI
#' @return List with best_x, best_y, history, convergence
run_bayesian_optimization <- function(
    predictor, X, var_names,
    n_iter = 30,
    n_random = 500,
    minimize = FALSE,
    bounds = NULL,
    categorical_mapping = NULL,
    target_value = NULL,
    patience = 3
) {
  use_target <- !is.null(target_value)
  X <- X[, var_names, drop = FALSE]
  p <- length(var_names)

  # Variable types (dummy / discrete / continuous)
  var_types <- detect_variable_types(X)
  is_disc <- var_types %in% c("dummy", "discrete")
  cont_idx <- which(!is_disc)
  disc_idx <- which(is_disc)
  levels_list <- lapply(seq_len(p), function(j) if (is_disc[j]) sort(unique(X[, j])) else NULL)

  # Categorical groups (column indices) for mutual exclusivity
  cat_groups <- list()
  for (cat_name in names(categorical_mapping)) {
    grp_idx <- which(var_names %in% categorical_mapping[[cat_name]]$dummy_cols)
    if (length(grp_idx) >= 2) cat_groups[[cat_name]] <- grp_idx
  }

  if (is.null(bounds)) {
    bounds <- rbind(apply(X, 2, min), apply(X, 2, max))
    colnames(bounds) <- var_names
  }

  # Score: lower is better in every mode
  score_of <- function(mu) {
    if (use_target) (mu - target_value)^2 else if (minimize) mu else -mu
  }
  predict_one <- function(x) predictor(matrix(x, nrow = 1, dimnames = list(NULL, var_names)))
  snap <- function(x) {
    for (j in disc_idx) {
      lv <- levels_list[[j]]
      x[j] <- lv[which.min(abs(lv - x[j]))]
    }
    enforce_categorical_constraints(x, cat_groups, var_names)
  }

  # Start from the best model prediction over the observed points
  pred_init <- predictor(X)
  scores_init <- score_of(pred_init$mean)
  best_idx <- which.min(scores_init)
  best_score <- scores_init[best_idx]
  best_y <- pred_init$mean[best_idx]
  best_x <- setNames(as.numeric(X[best_idx, ]), var_names)

  history_list <- list(list(
    iteration = 0, x = best_x, predicted_y = best_y,
    ei = NA_real_, se = pred_init$se[best_idx], type = "initial"
  ))
  convergence_y <- best_y
  ei_ref <- function() if (use_target) best_score else best_y
  stall <- 0
  n_done <- 0

  for (iter in seq_len(n_iter)) {
    n_done <- iter
    # Random candidates respecting variable types
    candidates <- matrix(0, n_random, p, dimnames = list(NULL, var_names))
    for (j in cont_idx) candidates[, j] <- runif(n_random, bounds[1, j], bounds[2, j])
    for (j in disc_idx) {
      lv <- levels_list[[j]]
      candidates[, j] <- lv[sample.int(length(lv), n_random, replace = TRUE)]
    }
    # Categorical groups: exactly one level active per row (all 0 = reference)
    for (grp_idx in cat_groups) {
      active <- sample(c(0L, grp_idx), n_random, replace = TRUE)
      candidates[, grp_idx] <- 0
      on <- which(active > 0)
      candidates[cbind(on, active[on])] <- 1
    }

    pred <- predictor(candidates)
    ei <- expected_improvement(pred$mean, pred$se, ei_ref(), minimize, target_value)
    ei[!is.finite(ei)] <- -Inf
    if (all(ei == -Inf)) break

    bi <- which.max(ei)
    new_x <- candidates[bi, ]
    new_ei <- ei[bi]

    # L-BFGS-B refinement of EI over continuous variables (discrete fixed)
    if (length(cont_idx) > 0) {
      base_x <- new_x
      ref <- ei_ref()
      opt <- tryCatch(optim(
        par = new_x[cont_idx],
        fn = function(xc) {
          x <- base_x
          x[cont_idx] <- xc
          pr <- predict_one(x)
          v <- -expected_improvement(pr$mean, pr$se, ref, minimize, target_value)
          if (is.finite(v)) v else 0
        },
        method = "L-BFGS-B",
        lower = bounds[1, cont_idx],
        upper = bounds[2, cont_idx]
      ), error = function(e) NULL)
      if (!is.null(opt) && is.finite(opt$value) && -opt$value > new_ei) {
        new_x[cont_idx] <- opt$par
        new_ei <- -opt$value
      }
    }

    new_x <- snap(new_x)
    new_pred <- predict_one(new_x)
    new_y <- new_pred$mean[1]
    new_score <- score_of(new_y)

    improved <- is.finite(new_score) && new_score < best_score
    if (improved) {
      best_score <- new_score
      best_y <- new_y
      best_x <- new_x
    }

    history_list[[length(history_list) + 1]] <- list(
      iteration = iter, x = new_x, predicted_y = new_y,
      ei = new_ei, se = new_pred$se[1],
      type = if (improved) "improved" else "explored"
    )
    convergence_y <- c(convergence_y, best_y)

    # Early stop after several consecutive iterations with negligible EI
    stall <- if (new_ei < 1e-12) stall + 1 else 0
    if (stall >= patience) break
  }

  # Final exploitation: locally optimize the predicted value itself around the
  # incumbent (discrete levels fixed), so the reported optimum is a true local
  # optimum of the response surface rather than the last sampled point.
  if (length(cont_idx) > 0) {
    base_x <- best_x
    opt <- tryCatch(optim(
      par = best_x[cont_idx],
      fn = function(xc) {
        x <- base_x
        x[cont_idx] <- xc
        v <- score_of(predict_one(x)$mean)
        if (is.finite(v)) v else .Machine$double.xmax
      },
      method = "L-BFGS-B",
      lower = bounds[1, cont_idx],
      upper = bounds[2, cont_idx]
    ), error = function(e) NULL)
    if (!is.null(opt) && is.finite(opt$value) &&
        opt$value < best_score - 1e-12 * max(1, abs(best_score))) {
      cand_x <- best_x
      cand_x[cont_idx] <- opt$par
      cand_pred <- predict_one(cand_x)
      best_score <- score_of(cand_pred$mean[1])
      best_y <- cand_pred$mean[1]
      best_x <- cand_x
      history_list[[length(history_list) + 1]] <- list(
        iteration = length(history_list), x = best_x, predicted_y = best_y,
        ei = NA_real_, se = cand_pred$se[1], type = "improved"
      )
      convergence_y <- c(convergence_y, best_y)
    }
  }

  # Tidy data frames
  hx <- do.call(rbind, lapply(history_list, function(h) h$x))
  history_df <- data.frame(
    iteration = vapply(history_list, function(h) h$iteration, numeric(1)),
    stringsAsFactors = FALSE
  )
  history_df <- cbind(history_df, as.data.frame(hx, stringsAsFactors = FALSE, optional = TRUE))
  names(history_df) <- c("iteration", var_names)
  history_df$predicted_y <- vapply(history_list, function(h) h$predicted_y, numeric(1))
  history_df$EI <- vapply(history_list, function(h) h$ei, numeric(1))
  history_df$SE <- vapply(history_list, function(h) h$se, numeric(1))
  history_df$type <- vapply(history_list, function(h) h$type, character(1))

  convergence_df <- data.frame(
    iteration = seq(0, length(convergence_y) - 1),
    best_y = convergence_y
  )

  list(
    best_x = best_x,
    best_y = best_y,
    history = history_df,
    convergence = convergence_df,
    bounds = bounds,
    minimize = minimize,
    var_types = var_types,
    n_iter_actual = n_done,
    target_value = target_value,
    categorical_mapping = categorical_mapping
  )
}

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- fluidPage(
  theme = custom_theme,
  tags$head(
    tags$style(HTML(custom_css)),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600;700&family=IBM+Plex+Sans:wght@400;500;600&display=swap",
      rel = "stylesheet"
    )
  ),

  # Header
  div(
    class = "main-header",
    div(
      class = "container-fluid",
      h1(
        class = "app-title",
        tags$span(style = sprintf("color: %s;", COLORS$accent_purple), "◆"),
        " RSM 応答曲面法"
      ),
      p(
        class = "app-subtitle",
        "Response Surface Methodology | 回帰分析 + ベイズ最適化"
      )
    )
  ),

  # Main content
  div(
    class = "container-fluid",
    fluidRow(
      # Left panel - Controls (with tabs)
      column(
        4,
        tabsetPanel(
          type = "tabs",
          id = "settings_tabs",

          # Data settings tab
          tabPanel(
            title = "データ設定",
            value = "data_settings",
            div(
              style = "margin-top: 1rem;",

              # Data input
              div(
                class = "analysis-card",
                div(class = "card-header", "データ入力"),
                tabsetPanel(
                  id = "data_input_mode",
                  type = "pills",
                  tabPanel(
                    title = "CSVファイル",
                    value = "csv",
                    br(),
                    fileInput(
                      "data_file",
                      label = "CSVファイルをアップロード",
                      accept = c(".csv", ".CSV"),
                      buttonLabel = "選択",
                      placeholder = "ファイル未選択"
                    ),
                    selectInput(
                      "encoding", "文字エンコーディング",
                      choices = c("UTF-8" = "UTF-8", "Shift-JIS" = "CP932", "EUC-JP" = "EUC-JP"),
                      selected = "UTF-8"
                    )
                  ),
                  tabPanel(
                    title = "表データ貼り付け",
                    value = "paste",
                    br(),
                    textAreaInput(
                      "data_paste",
                      label = "表データを貼り付け（CSV または タブ区切り）",
                      placeholder = "例：\n品質スコア,温度,圧力,速度\n80,0.12,-0.55,1.02",
                      rows = 6
                    )
                  )
                ),
                hr(style = sprintf("border-color: %s;", COLORS$border)),
                checkboxInput("header", "1行目をヘッダー行として扱う", value = TRUE)
              ),

              # Variable selection
              div(
                class = "analysis-card",
                div(class = "card-header", "変数設定"),
                selectInput("target_var", label = "目的変数 (Y)", choices = NULL),
                selectInput("explanatory_vars", label = "説明変数 (X)", choices = NULL, multiple = TRUE),
                p(
                  style = sprintf("color: %s; font-size: 0.8rem;", COLORS$accent_orange),
                  sprintf("変数は%d個以下を推奨", MAX_RECOMMENDED_VARS)
                )
              ),

              # Sample data pattern
              div(
                class = "analysis-card",
                div(class = "card-header", "サンプルデータ"),
                selectInput(
                  "sample_pattern",
                  "パターン選択（データ未指定時に使用）",
                  choices = c(
                    "主効果のみ" = "main_linear",
                    "強い交互作用" = "strong_interact",
                    "弱い階層の交互作用" = "weak_interact",
                    "二次モデル" = "quadratic",
                    "低寄与・ほぼノイズ" = "low_signal_noise",
                    "カテゴリ変数を含む" = "categorical"
                  ),
                  selected = "strong_interact"
                )
              )
            )
          ),

          # Model settings tab
          tabPanel(
            title = "モデル設定",
            value = "model_settings",
            div(
              style = "margin-top: 1rem;",
              div(
                class = "analysis-card",
                div(class = "card-header", "RSMパラメータ"),
                selectInput(
                  "model_type",
                  "モデルタイプ",
                  choices = c(
                    "交互作用モデル (FO + TWI)" = "interaction",
                    "二次モデル (SO)" = "quadratic"
                  ),
                  selected = "quadratic"
                ),
                p(
                  style = sprintf("color: %s; font-size: 0.75rem; margin-top: 0.5rem;", COLORS$text_muted),
                  "FO: 一次項のみ、TWI: 二因子交互作用、SO: 二次項を含む完全モデル"
                )
              ),
              div(
                class = "analysis-card",
                div(class = "card-header", "変数選択 (逐次減増法)"),
                checkboxInput(
                  "use_stepwise",
                  "逐次減増法を使用する",
                  value = TRUE
                ),
                p(
                  style = sprintf("color: %s; font-size: 0.75rem;", COLORS$text_muted),
                  "全変数からスタートし、分散比(F値)に基づいて変数を選択・除外"
                ),
                hr(style = sprintf("border-color: %s;", COLORS$border)),
                sliderInput(
                  "f_threshold",
                  "分散比(F値)閾値",
                  min = 1, max = 4, value = 2, step = 0.5
                ),
                p(
                  style = sprintf("color: %s; font-size: 0.75rem;", COLORS$text_muted),
                  "F値 ≤ 閾値: 変数を除外、F値 > 閾値: 変数を追加"
                ),
                hr(style = sprintf("border-color: %s;", COLORS$border)),
                checkboxInput(
                  "enforce_hierarchy",
                  "階層原則を適用する",
                  value = TRUE
                ),
                p(
                  style = sprintf("color: %s; font-size: 0.75rem;", COLORS$text_muted),
                  "2次項・交互作用が選択された場合、対応する1次項を強制選択"
                )
              ),
              div(
                class = "analysis-card",
                div(class = "card-header", "最適化方向"),
                radioButtons(
                  "bo_direction",
                  NULL,
                  choices = c("最大化" = "maximize", "最小化" = "minimize", "目標値" = "target"),
                  selected = "maximize",
                  inline = TRUE
                ),
                conditionalPanel(
                  condition = "input.bo_direction == 'target'",
                  numericInput("bo_target_value", "目標値:", value = 0, step = 0.1)
                )
              ),
              div(
                class = "analysis-card",
                actionButton("run_analysis", "分析・最適化 実行", class = "btn-analysis", icon = icon("play"))
              )
            )
          )
        )
      ),

      # Right panel - Results
      column(
        8,
        tabsetPanel(
          type = "tabs",
          id = "result_tabs",

          # Data preview tab
          tabPanel(
            title = "データプレビュー",
            value = "data_tab",
            div(
              class = "result-section", style = "margin-top: 1rem;",
              div(class = "card-header", "アップロードデータ / サンプルデータ"),
              DTOutput("data_preview")
            )
          ),

          # Results tab
          tabPanel(
            title = "回帰結果",
            value = "result_tab",
            div(
              style = "margin-top: 1rem;",
              div(
                class = "result-section",
                div(class = "card-header", "モデル評価指標"),
                div(class = "metric-grid", uiOutput("metrics_display"))
              ),
              div(
                class = "result-section",
                div(
                  class = "card-header",
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  span("回帰式・係数"),
                  radioButtons(
                    "coef_scale",
                    label = NULL,
                    choices = c("元単位" = "original", "標準化" = "standardized"),
                    selected = "original",
                    inline = TRUE
                  )
                ),
                div(class = "equation-display", uiOutput("equation_display")),
                p(
                  style = sprintf("color: %s; font-size: 0.75rem; margin: 0.5rem 0;", COLORS$text_muted),
                  uiOutput("scale_description")
                )
              ),
              div(
                class = "result-section",
                div(class = "card-header", "係数一覧 (主効果・交互作用)"),
                DTOutput("coef_table")
              ),
              div(
                class = "result-section",
                div(
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  div(class = "card-header", style = "margin-bottom: 0;", "最適解 (ベイズ最適化)"),
                  downloadButton("bo_download_csv", "CSV保存",
                                 class = "btn-sm",
                                 style = "font-size: 0.75rem; padding: 4px 12px;")
                ),
                uiOutput("bo_best_display")
              ),
              fluidRow(
                column(
                  6,
                  div(
                    class = "result-section",
                    div(class = "card-header", "収束プロット"),
                    div(class = "plot-container", plotlyOutput("bo_convergence_plot", height = "350px"))
                  )
                ),
                column(
                  6,
                  div(
                    class = "result-section",
                    div(class = "card-header", "EI（期待改善量）推移"),
                    div(class = "plot-container", plotlyOutput("bo_ei_plot", height = "350px"))
                  )
                )
              ),
              div(
                class = "result-section",
                div(class = "card-header", "探索履歴"),
                DTOutput("bo_history_table")
              ),
              div(
                class = "result-section",
                div(
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  div(class = "card-header", style = "margin-bottom: 0;", "予測シミュレーション"),
                  div(
                    style = "display: flex; gap: 6px;",
                    actionButton("pred_fill_bo", "最適値セット",
                                 class = "btn-sm",
                                 style = "font-size: 0.7rem; padding: 3px 10px;"),
                    actionButton("pred_fill_mean", "平均値セット",
                                 class = "btn-sm",
                                 style = "font-size: 0.7rem; padding: 3px 10px;")
                  )
                ),
                uiOutput("pred_input_ui"),
                uiOutput("pred_result_display")
              )
            )
          ),

          # Plots tab
          tabPanel(
            title = "予測判定グラフ",
            value = "plot_tab",
            div(
              style = "margin-top: 1rem;",
              div(
                class = "result-section",
                div(class = "card-header", "実測値 vs 予測値"),
                div(class = "plot-container", plotlyOutput("prediction_plot", height = "500px"))
              ),
              fluidRow(
                column(
                  6,
                  div(
                    class = "result-section",
                    div(class = "card-header", "残差分布"),
                    div(class = "plot-container", plotlyOutput("residual_plot", height = "350px"))
                  )
                ),
                column(
                  6,
                  div(
                    class = "result-section",
                    div(class = "card-header", "残差 vs 予測値"),
                    div(class = "plot-container", plotlyOutput("residual_vs_fitted_plot", height = "350px"))
                  )
                )
              )
            )
          ),

          # Coefficient structure tab
          tabPanel(
            title = "係数構造",
            value = "structure_tab",
            div(
              style = "margin-top: 1rem;",
              fluidRow(
                column(
                  6,
                  div(
                    class = "result-section",
                    div(class = "card-header", "主効果係数"),
                    div(class = "plot-container", plotlyOutput("main_effect_plot", height = "400px"))
                  )
                ),
                column(
                  6,
                  div(
                    class = "result-section",
                    div(class = "card-header", "交互作用ヒートマップ"),
                    div(class = "plot-container", plotlyOutput("interaction_heatmap", height = "400px"))
                  )
                )
              ),
              div(
                class = "result-section",
                div(class = "card-header", "選択された交互作用"),
                DTOutput("interaction_table")
              )
            )
          ),

          # Response Surface tab (応答曲面)
          tabPanel(
            title = "応答曲面",
            value = "surface_tab",
            div(
              style = "margin-top: 1rem;",
              fluidRow(
                column(
                  3,
                  div(
                    class = "analysis-card",
                    div(class = "card-header", "曲面設定"),
                    selectInput(
                      "surface_var1",
                      "X軸変数",
                      choices = NULL
                    ),
                    selectInput(
                      "surface_var2",
                      "Y軸変数",
                      choices = NULL
                    ),
                    hr(style = sprintf("border-color: %s;", COLORS$border)),
                    uiOutput("surface_slider_ui"),
                    hr(style = sprintf("border-color: %s;", COLORS$border)),
                    sliderInput(
                      "surface_resolution",
                      "グリッド解像度",
                      min = 10, max = 50, value = 25, step = 5
                    ),
                    checkboxInput(
                      "surface_show_points",
                      "実測値を表示",
                      value = TRUE
                    ),
                    hr(style = sprintf("border-color: %s;", COLORS$border)),
                    checkboxInput(
                      "surface_allow_extrapolation",
                      "外挿を許可（データ範囲外）",
                      value = FALSE
                    ),
                    uiOutput("extrapolation_warning")
                  )
                ),
                column(
                  9,
                  div(
                    class = "result-section",
                    div(class = "card-header", "3D応答曲面 (Response Surface)"),
                    div(class = "plot-container", plotlyOutput("surface_plot", height = "600px"))
                  ),
                  div(
                    class = "result-section",
                    div(class = "card-header", "等高線プロット (Contour)"),
                    div(class = "plot-container", plotlyOutput("contour_plot", height = "450px"))
                  )
                )
              )
            )
          ),

        )
      )
    )
  )
)

# ============================================================================
# SERVER DEFINITION
# ============================================================================

server <- function(input, output, session) {

  # Reactive values
  run_counter <- 0  # Incremented per analysis; makes dynamic input IDs unique per run

  rv <- reactiveValues(
    data = NULL,
    analysis = NULL  # Combined fit, cv_fit, and results
  )

  # -------------------------------------------------------------------------
  # Data loading
  # -------------------------------------------------------------------------

  parsed_data <- reactive({
    mode <- input$data_input_mode %||% "csv"
    pattern <- input$sample_pattern %||% "strong_interact"

    # CSV mode
    if (mode == "csv" && !is.null(input$data_file)) {
      tryCatch({
        enc <- input$encoding %||% "UTF-8"
        # "UTF-8-BOM" also reads plain UTF-8 and strips an Excel BOM
        if (enc == "UTF-8") enc <- "UTF-8-BOM"
        df <- read.csv(
          input$data_file$datapath,
          header = isTRUE(input$header),
          fileEncoding = enc,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          comment.char = "",
          strip.white = TRUE
        )
        df <- sanitize_column_names(df)
        showNotification("CSVファイルを読み込みました", type = "message", duration = 3)
        df
      }, error = function(e) {
        showNotification(
          sprintf("CSV読み込みエラー: %s（サンプルデータを使用します）", e$message),
          type = "error", duration = 8
        )
        generate_sample_data(pattern = pattern)
      })
    }
    # Paste mode
    else if (mode == "paste") {
      txt <- input$data_paste
      if (!is.null(txt) && nzchar(trimws(txt))) {
        sep <- if (grepl("\t", txt)) "\t" else ","
        tryCatch({
          df <- as.data.frame(read.table(
            text = txt,
            sep = sep,
            header = isTRUE(input$header),
            stringsAsFactors = FALSE,
            check.names = FALSE,
            quote = "\"",
            comment.char = "",
            strip.white = TRUE,
            blank.lines.skip = TRUE
          ))
          df <- sanitize_column_names(df)
          showNotification("貼り付けデータを読み込みました", type = "message", duration = 3)
          df
        }, error = function(e) {
          showNotification(
            sprintf("貼り付けデータの読み込みエラー: %s（サンプルデータを使用します）", e$message),
            type = "error", duration = 8
          )
          generate_sample_data(pattern = pattern)
        })
      } else {
        generate_sample_data(pattern = pattern)
      }
    }
    # Default: sample data
    else {
      generate_sample_data(pattern = pattern)
    }
  })

  # Update variable selectors when data changes
  observe({
    df <- parsed_data()

    # Auto-encode categorical (character/factor) columns as dummy variables
    encoded <- encode_categorical(df)
    df <- encoded$data

    # Show encoding notifications
    if (length(encoded$messages) > 0) {
      for (msg in encoded$messages) {
        showNotification(msg, type = "message", duration = 6)
      }
    }

    rv$data <- df
    rv$categorical_mapping <- encoded$mapping

    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]

    if (length(numeric_cols) == 0) {
      updateSelectInput(session, "target_var", choices = character(0))
      updateSelectInput(session, "explanatory_vars", choices = character(0))
      showNotification("数値列が見つかりませんでした", type = "error")
      return()
    }

    # Check minimum columns for RSM (target + at least 2 explanatory)
    if (length(numeric_cols) < 3) {
      showNotification(
        sprintf("RSMには最低3つの数値列が必要です（現在: %d列）", length(numeric_cols)),
        type = "warning"
      )
    }

    # Check sample size vs number of variables
    n_obs <- nrow(df)
    n_vars <- length(numeric_cols) - 1  # excluding target
    if (n_obs < 10 * n_vars) {
      showNotification(
        sprintf("サンプルサイズ(%d)が変数数(%d)に対して小さい可能性があります（推奨: n≥10p）",
                n_obs, n_vars),
        type = "warning"
      )
    }

    updateSelectInput(session, "target_var", choices = numeric_cols, selected = numeric_cols[1])

    explanatory_choices <- if (length(numeric_cols) > 1) numeric_cols[-1] else numeric_cols
    updateSelectInput(
      session, "explanatory_vars",
      choices = explanatory_choices,
      selected = explanatory_choices
    )
  })

  # Update explanatory variables when target changes (exclude target from choices)
  observeEvent(input$target_var, {
    req(rv$data)
    numeric_cols <- names(rv$data)[vapply(rv$data, is.numeric, logical(1))]
    explanatory_choices <- setdiff(numeric_cols, input$target_var)

    # Keep current selections that are still valid
    current_selected <- input$explanatory_vars
    valid_selected <- intersect(current_selected, explanatory_choices)

    # If no valid selections, select all available
    if (length(valid_selected) == 0) {
      valid_selected <- explanatory_choices
    }

    updateSelectInput(
      session, "explanatory_vars",
      choices = explanatory_choices,
      selected = valid_selected
    )
  })

  # -------------------------------------------------------------------------
  # Data preview
  # -------------------------------------------------------------------------

  output$data_preview <- renderDT({
    req(rv$data)
    datatable(
      rv$data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "frtip",
        language = list(
          search = "検索:",
          lengthMenu = "表示: _MENU_ 件",
          info = "_TOTAL_ 件中 _START_ - _END_ 件表示",
          paginate = list(previous = "前", `next` = "次")
        )
      ),
      class = "stripe hover",
      rownames = FALSE
    )
  })

  # -------------------------------------------------------------------------
  # Analysis execution
  # -------------------------------------------------------------------------

  observeEvent(input$run_analysis, {
    req(rv$data, input$target_var, input$explanatory_vars)

    # Validation: target must not be in explanatory variables
    if (input$target_var %in% input$explanatory_vars) {
      showNotification(
        "目的変数と説明変数に同じ変数が選択されています",
        type = "error"
      )
      return()
    }

    # Validation: minimum explanatory variables
    if (length(input$explanatory_vars) < MIN_EXPLANATORY_VARS) {
      showNotification(
        sprintf("RSMには%d個以上の説明変数が必要です", MIN_EXPLANATORY_VARS),
        type = "error"
      )
      return()
    }

    if (length(input$explanatory_vars) > MAX_RECOMMENDED_VARS) {
      showNotification(
        sprintf("変数が多すぎます。%d個以下を推奨します", MAX_RECOMMENDED_VARS),
        type = "warning"
      )
    }

    withProgress(message = "RSM分析実行中...", value = 0, {
      tryCatch({
        incProgress(0.1, detail = "データ準備中")

        y <- rv$data[[input$target_var]]
        X <- as.matrix(rv$data[, input$explanatory_vars, drop = FALSE])

        # Validate data
        validated <- validate_data(X, y)

        if (length(validated$issues) > 0) {
          for (issue in validated$issues) {
            showNotification(issue, type = "warning")
          }
        }

        if (!validated$valid) {
          showNotification("有効なデータがありません", type = "error")
          return()
        }

        X <- validated$X
        y <- validated$y

        # Get stepwise parameters
        use_stepwise <- isTRUE(input$use_stepwise)
        f_threshold <- input$f_threshold %||% 2
        enforce_hierarchy <- isTRUE(input$enforce_hierarchy)

        if (use_stepwise) {
          incProgress(0.2, detail = "逐次減増法で変数選択中...")
        } else {
          incProgress(0.4, detail = "モデルフィッティング中")
        }

        # Run RSM analysis with stepwise selection
        analysis_result <- run_rsm_analysis(
          X = X,
          y = y,
          model_type = input$model_type,
          use_stepwise = use_stepwise,
          f_in = f_threshold,
          f_out = f_threshold,
          enforce_hierarchy = enforce_hierarchy,
          categorical_mapping = rv$categorical_mapping
        )

        incProgress(0.3, detail = "評価指標計算中")

        # Compute metrics
        active_counts <- count_active_coefficients(
          analysis_result$main_effects,
          analysis_result$interaction_matrix
        )

        # n_params = estimable coefficients (excluding intercept)
        metrics <- compute_metrics(y, analysis_result$predictions, analysis_result$n_params)

        incProgress(0.15, detail = "結果整理中")

        # Count selected terms
        n_selected <- length(analysis_result$selected_terms)
        n_total <- length(analysis_result$all_terms)

        # Categorical variables actually present in this model
        cm_used <- restrict_categorical_mapping(rv$categorical_mapping, colnames(X))
        run_counter <<- run_counter + 1

        # Store results
        rv$analysis <- list(
          run_id = run_counter,
          target_name = input$target_var,
          cm = cm_used,
          var_types = detect_variable_types(X),
          is_dummy = analysis_result$is_dummy,
          predictor = analysis_result$predictor,
          y = y,
          X = X,
          var_names = colnames(X),
          predictions = analysis_result$predictions,
          # Original scale coefficients
          intercept_orig = analysis_result$intercept_orig,
          main_effects_orig = analysis_result$main_effects_orig,
          interaction_matrix_orig = analysis_result$interaction_matrix_orig,
          # Standardized coefficients
          intercept_std = analysis_result$intercept_std,
          main_effects_std = analysis_result$main_effects_std,
          interaction_matrix_std = analysis_result$interaction_matrix_std,
          # Scaling info
          mx = analysis_result$mx,
          sx = analysis_result$sx,
          # Legacy fields
          intercept = analysis_result$intercept,
          main_effects = analysis_result$main_effects,
          interaction_matrix = analysis_result$interaction_matrix,
          # Model info
          fit = analysis_result$fit,
          model_summary = analysis_result$model_summary,
          metrics = metrics,
          n_main = active_counts$n_main,
          n_interaction = active_counts$n_interaction,
          n_quadratic = active_counts$n_quadratic,
          n_higher_order = active_counts$n_higher_order,  # For UI: "交互作用/二次項数"
          model_type = input$model_type,
          # Stepwise selection info
          selected_terms = analysis_result$selected_terms,
          all_terms = analysis_result$all_terms,
          selection_history = analysis_result$selection_history,
          use_stepwise = use_stepwise,
          f_threshold = f_threshold
        )

        incProgress(0.05, detail = "最適化実行中...")

        # Auto-run Bayesian Optimization
        bo_dir <- input$bo_direction
        minimize <- (bo_dir == "minimize")
        target_val <- NULL
        if (bo_dir == "target") {
          if (!is.null(input$bo_target_value) && !is.na(input$bo_target_value)) {
            target_val <- input$bo_target_value
          } else {
            showNotification("目標値が未入力のため、最大化モードで実行します", type = "warning", duration = 5)
          }
        }
        set.seed(RANDOM_SEED)
        bo_result <- tryCatch({
          run_bayesian_optimization(
            predictor = analysis_result$predictor,
            X = X,
            var_names = colnames(X),
            n_iter = 30,
            n_random = 500,
            minimize = minimize,
            bounds = NULL,
            categorical_mapping = cm_used,
            target_value = target_val
          )
        }, error = function(e) {
          message("BO auto-run error: ", e$message)
          NULL
        })
        rv_bo$result <- bo_result

        incProgress(0.05, detail = "完了")

        updateTabsetPanel(session, "result_tabs", selected = "result_tab")

        # Show notification
        direction_text <- if (!is.null(target_val)) {
          sprintf("目標(%s)", smart_format(target_val))
        } else if (minimize) "最小" else "最大"
        bo_info <- if (!is.null(bo_result)) {
          sprintf(", %s値: %s", direction_text, smart_format(bo_result$best_y))
        } else ""

        showNotification(
          sprintf("分析・最適化完了（R²=%s%s）",
                  if (is.na(metrics$r_squared)) "NA" else sprintf("%.3f", metrics$r_squared), bo_info),
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        # User-friendly error messages
        err_msg <- e$message
        user_msg <- if (grepl("singular|collinear", err_msg, ignore.case = TRUE)) {
          "変数間に完全共線性があります。変数を見直してください"
        } else if (grepl("memory|allocate", err_msg, ignore.case = TRUE)) {
          "メモリ不足です。変数数を減らしてください"
        } else {
          sprintf("分析エラー: %s", err_msg)
        }
        showNotification(user_msg, type = "error", duration = 10)
        message("RSM Error: ", err_msg)
      })
    })
  })

  # -------------------------------------------------------------------------
  # Metrics display
  # -------------------------------------------------------------------------

  output$metrics_display <- renderUI({
    req(rv$analysis)
    res <- rv$analysis
    m <- res$metrics

    r2_class <- get_r2_class(m$r_squared)

    # Helper for formatting potentially NA values
    fmt <- function(x, digits = 4) {
      if (is.null(x) || is.na(x)) "N/A" else sprintf("%.*f", digits, x)
    }

    # Selection info
    n_selected <- length(res$selected_terms %||% character(0))
    n_total <- length(res$all_terms %||% character(0))
    selection_text <- if (isTRUE(res$use_stepwise) && n_total > 0) {
      sprintf("%d / %d", n_selected, n_total)
    } else {
      sprintf("%d", n_selected)
    }

    tagList(
      div(class = "metric-box",
          div(class = "metric-label", "決定係数 R²"),
          div(class = paste("metric-value", r2_class), fmt(m$r_squared))),
      div(class = "metric-box",
          div(class = "metric-label", "調整済み R²"),
          div(class = "metric-value", fmt(m$adj_r_squared))),
      div(class = "metric-box",
          div(class = "metric-label", "RMSE"),
          div(class = "metric-value", fmt(m$rmse))),
      div(class = "metric-box",
          div(class = "metric-label", "MAE"),
          div(class = "metric-value", fmt(m$mae))),
      div(class = "metric-box",
          div(class = "metric-label", "選択項数"),
          div(class = "metric-value", style = sprintf("color: %s;", COLORS$accent_green), selection_text)),
      div(class = "metric-box",
          div(class = "metric-label", "主効果数"),
          div(class = "metric-value", res$n_main)),
      div(class = "metric-box",
          div(class = "metric-label", "交互作用/二次項数"),
          div(class = "metric-value", style = sprintf("color: %s;", COLORS$accent_purple), res$n_higher_order)),
      div(class = "metric-box",
          div(class = "metric-label", "サンプルサイズ"),
          div(class = "metric-value", m$n)),
      div(class = "metric-box",
          div(class = "metric-label", "モデルタイプ"),
          div(class = "metric-value", style = "font-size: 0.9rem;",
              if (res$model_type == "interaction") "FO+TWI" else "SO (二次)")),
      if (isTRUE(res$use_stepwise)) {
        div(class = "metric-box",
            div(class = "metric-label", "F閾値"),
            div(class = "metric-value", style = "font-size: 1rem;", res$f_threshold %||% 2))
      }
    )
  })

  # -------------------------------------------------------------------------
  # Scale description
  # -------------------------------------------------------------------------

  output$scale_description <- renderUI({
    scale <- input$coef_scale %||% "original"
    if (scale == "original") {
      "元単位: 主効果β×Xi、交互作用θ×(Xi-X̄i)(Xj-X̄j)、二乗項γ×(Xi-X̄i)²。予測式として使用可能。"
    } else {
      HTML(paste0(
        "標準化: X,Yともに完全標準化（平均0,SD=1）。Xが1SD変化時のYのSD変化量を表す。",
        "<br><span style='color: ", COLORS$text_muted, ";'>",
        "※二次項は「標準化Xの二乗」であり、元単位の二次項とはスケールが異なります。</span>"
      ))
    }
  })

  # -------------------------------------------------------------------------
  # Equation display
  # -------------------------------------------------------------------------

  output$equation_display <- renderUI({
    req(rv$analysis)
    res <- rv$analysis
    scale <- input$coef_scale %||% "original"

    # Select coefficients based on scale (with fallback)
    if (scale == "original") {
      intercept <- res$intercept_orig %||% res$intercept %||% 0
      main_effects <- res$main_effects_orig %||% res$main_effects
      interaction_matrix <- res$interaction_matrix_orig %||% res$interaction_matrix
    } else {
      intercept <- res$intercept_std %||% res$intercept %||% 0
      main_effects <- res$main_effects_std %||% res$main_effects
      interaction_matrix <- res$interaction_matrix_std %||% res$interaction_matrix
    }

    # Ensure intercept is numeric
    intercept <- as.numeric(intercept)
    if (is.na(intercept)) intercept <- 0

    equation <- build_equation(
      intercept,
      main_effects,
      interaction_matrix,
      res$var_names,
      res$target_name %||% "Y",
      scale = scale,
      mx = res$mx,
      is_dummy = res$is_dummy
    )
    # Variable names come from user data: escape before rendering as HTML
    HTML(htmltools::htmlEscape(equation))
  })

  # -------------------------------------------------------------------------
  # Coefficient table
  # -------------------------------------------------------------------------

  output$coef_table <- renderDT({
    req(rv$analysis)
    res <- rv$analysis
    scale <- input$coef_scale %||% "original"

    # Select coefficients based on scale (with fallback)
    if (scale == "original") {
      main_effects <- res$main_effects_orig %||% res$main_effects
      interaction_matrix <- res$interaction_matrix_orig %||% res$interaction_matrix
    } else {
      main_effects <- res$main_effects_std %||% res$main_effects
      interaction_matrix <- res$interaction_matrix_std %||% res$interaction_matrix
    }

    # Ensure numeric
    main_effects <- as.numeric(main_effects)
    names(main_effects) <- res$var_names

    # Main effects
    main_df <- data.frame(
      変数 = names(main_effects),
      係数 = main_effects,
      タイプ = "主効果",
      選択 = ifelse(abs(main_effects) > DISPLAY_THRESHOLD, "✓", ""),
      stringsAsFactors = FALSE
    )

    # Interactions (using utility function - now includes type column)
    interactions <- extract_interactions(interaction_matrix, res$var_names, 0)

    if (nrow(interactions) > 0) {
      # Use type column from extract_interactions()
      is_quadratic <- interactions$type == "quadratic"
      int_df <- data.frame(
        変数 = ifelse(is_quadratic,
                     paste0(interactions$var1, "²"),
                     paste0(interactions$var1, " × ", interactions$var2)),
        係数 = interactions$coefficient,
        タイプ = ifelse(is_quadratic, "2次項", "交互作用"),
        選択 = ifelse(abs(interactions$coefficient) > DISPLAY_THRESHOLD, "✓", ""),
        stringsAsFactors = FALSE
      )
      coef_df <- rbind(main_df, int_df)
    } else {
      coef_df <- main_df
    }

    # Sort by absolute coefficient
    coef_df <- coef_df[order(-abs(coef_df$係数)), ]
    rownames(coef_df) <- NULL

    datatable(
      coef_df,
      options = list(pageLength = 15, dom = "frtip", ordering = TRUE),
      rownames = FALSE,
      selection = "none"
    ) |>
      formatRound(columns = "係数", digits = 4) |>
      formatStyle(columns = "選択", color = COLORS$accent_green, fontWeight = "bold") |>
      formatStyle(
        columns = "タイプ",
        color = styleEqual(c("主効果", "交互作用", "2次項"),
                          c(COLORS$accent_blue, COLORS$accent_purple, COLORS$accent_orange))
      )
  })

  # -------------------------------------------------------------------------
  # Prediction plot
  # -------------------------------------------------------------------------

  output$prediction_plot <- renderPlotly({
    req(rv$analysis)
    req(rv$analysis$metrics)
    res <- rv$analysis

    # Prepare data with pre-computed tooltip text
    df <- data.frame(
      actual = res$y,
      predicted = res$predictions,
      residual = res$metrics$residuals,
      stringsAsFactors = FALSE
    )
    df$tooltip <- sprintf("実測: %.2f<br>予測: %.2f<br>残差: %.2f",
                          df$actual, df$predicted, df$residual)

    # Handle edge case: single observation
    if (nrow(df) < 2) {
      p <- ggplot(df, aes(x = actual, y = predicted)) +
        geom_point(aes(text = tooltip), color = COLORS$accent_purple, size = 5) +
        labs(x = "実測値", y = "予測値", title = "（データ点が少なすぎます）") +
        theme_rsm()
      return(ggplotly(p, tooltip = "text") |> layout_rsm())
    }

    # Pad by 5% of the data range (multiplying by 0.95/1.05 would clip points
    # when values are negative)
    range_min <- min(c(df$actual, df$predicted))
    range_max <- max(c(df$actual, df$predicted))
    pad <- 0.05 * (range_max - range_min)
    range_min <- range_min - pad
    range_max <- range_max + pad

    # Handle identical values (zero range)
    if (abs(range_max - range_min) < .Machine$double.eps) {
      range_min <- range_min - 1
      range_max <- range_max + 1
    }

    # Safe regression line calculation
    reg_coef <- tryCatch({
      fit <- lm(predicted ~ actual, data = df)
      coef(fit)
    }, error = function(e) c(0, 1))

    r2_display <- if (is.na(res$metrics$r_squared)) "NA" else sprintf("%.3f", res$metrics$r_squared)
    ann_text <- sprintf("R² = %s\nRMSE = %.3f", r2_display, res$metrics$rmse)

    p <- ggplot(df, aes(x = actual, y = predicted)) +
      geom_abline(intercept = 0, slope = 1, color = COLORS$border, linetype = "dashed", linewidth = 1) +
      geom_ribbon(
        data = data.frame(x = seq(range_min, range_max, length.out = 100)),
        aes(x = x, ymin = pmin(x * 0.9, x * 1.1), ymax = pmax(x * 0.9, x * 1.1)),
        inherit.aes = FALSE,
        fill = COLORS$accent_purple, alpha = 0.08
      )

    # Only add regression line if coefficients are valid
    if (!any(is.na(reg_coef))) {
      p <- p + geom_abline(intercept = reg_coef[1], slope = reg_coef[2],
                           color = COLORS$accent_green, linewidth = 1.0)
    }

    p <- p +
      geom_point(aes(text = tooltip), color = COLORS$accent_purple, alpha = 0.7, size = 3) +
      annotate(
        "text",
        x = range_min + 0.03 * (range_max - range_min),
        y = range_max - 0.03 * (range_max - range_min),
        label = ann_text, hjust = 0, vjust = 1, size = 3.5, color = COLORS$text_primary
      ) +
      labs(x = "実測値", y = "予測値") +
      theme_rsm() +
      coord_fixed(ratio = 1, xlim = c(range_min, range_max), ylim = c(range_min, range_max))

    ggplotly(p, tooltip = "text") |>
      layout_rsm() |>
      layout(legend = list(orientation = "h"))
  })

  # -------------------------------------------------------------------------
  # Residual plot
  # -------------------------------------------------------------------------

  output$residual_plot <- renderPlotly({
    req(rv$analysis)

    df <- data.frame(residual = rv$analysis$metrics$residuals)

    p <- ggplot(df, aes(x = residual)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = COLORS$accent_purple, alpha = 0.6, color = COLORS$bg_primary) +
      geom_density(color = COLORS$accent_green, linewidth = 1) +
      geom_vline(xintercept = 0, color = COLORS$accent_red, linetype = "dashed") +
      labs(x = "残差", y = "密度") +
      theme_rsm()

    ggplotly(p) |> layout_rsm()
  })

  # -------------------------------------------------------------------------
  # Residual vs Fitted plot
  # -------------------------------------------------------------------------

  output$residual_vs_fitted_plot <- renderPlotly({
    req(rv$analysis)
    res <- rv$analysis

    df <- data.frame(
      fitted = res$predictions,
      residual = res$metrics$residuals
    )

    p <- ggplot(df, aes(x = fitted, y = residual)) +
      geom_hline(yintercept = 0, color = COLORS$accent_red, linetype = "dashed", linewidth = 1) +
      geom_point(color = COLORS$accent_purple, alpha = 0.7, size = 3) +
      geom_smooth(method = "loess", se = FALSE, color = COLORS$accent_green, linewidth = 1) +
      labs(x = "予測値", y = "残差") +
      theme_rsm()

    ggplotly(p) |> layout_rsm()
  })

  # -------------------------------------------------------------------------
  # Main effect plot
  # -------------------------------------------------------------------------

  output$main_effect_plot <- renderPlotly({
    req(rv$analysis)
    res <- rv$analysis
    scale <- input$coef_scale %||% "original"

    # Select coefficients based on scale (with fallback)
    if (scale == "original") {
      main_effects <- res$main_effects_orig %||% res$main_effects
    } else {
      main_effects <- res$main_effects_std %||% res$main_effects
    }
    x_label <- if (scale == "original") "係数 (元単位)" else "係数 (標準化)"

    # Ensure numeric and named
    coef_values <- as.numeric(main_effects)
    var_names <- names(main_effects) %||% res$var_names

    df <- data.frame(
      variable = var_names,
      coefficient = coef_values,
      stringsAsFactors = FALSE
    ) |>
      dplyr::mutate(abs_coef = abs(coefficient)) |>
      dplyr::arrange(abs_coef) |>
      dplyr::mutate(variable = factor(variable, levels = variable))

    p <- ggplot(df, aes(x = coefficient, y = variable)) +
      geom_col(aes(fill = coefficient > 0), alpha = 0.8, width = 0.7) +
      geom_vline(xintercept = 0, color = COLORS$border, linewidth = 0.5) +
      scale_fill_manual(values = c("TRUE" = COLORS$accent_green, "FALSE" = COLORS$accent_red), guide = "none") +
      labs(x = x_label, y = "") +
      theme_rsm() +
      theme(panel.grid.major.y = element_blank(), axis.text = element_text(color = COLORS$text_primary, size = 11))

    ggplotly(p) |> layout_rsm()
  })

  # -------------------------------------------------------------------------
  # Interaction heatmap
  # -------------------------------------------------------------------------

  output$interaction_heatmap <- renderPlotly({
    req(rv$analysis)
    res <- rv$analysis
    scale <- input$coef_scale %||% "original"

    # Select coefficients based on scale (with fallback)
    if (scale == "original") {
      int_mat <- res$interaction_matrix_orig %||% res$interaction_matrix
    } else {
      int_mat <- res$interaction_matrix_std %||% res$interaction_matrix
    }
    var_names <- res$var_names
    legend_title <- if (scale == "original") "係数\n(元単位)" else "係数\n(標準化)"

    # Validate matrix
    if (is.null(int_mat) || !is.matrix(int_mat)) {
      p <- ncol(res$X) %||% length(var_names)
      int_mat <- matrix(0, nrow = p, ncol = p)
    }

    # Create grid for heatmap
    # expand.grid: var1 varies fastest (rows), var2 varies slowest (columns)
    # as.vector(t(int_mat)): transpose to match expand.grid order
    # This ensures cell (var1[i], var2[j]) shows int_mat[i, j]
    df <- expand.grid(var1 = var_names, var2 = var_names, stringsAsFactors = FALSE)
    df$coef_value <- as.numeric(as.vector(t(int_mat)))

    p <- ggplot(df, aes(x = var1, y = var2, fill = coef_value)) +
      geom_tile(color = COLORS$bg_tertiary, linewidth = 0.5) +
      geom_text(
        aes(label = ifelse(abs(coef_value) > DISPLAY_THRESHOLD, sprintf("%.2f", coef_value), "")),
        color = COLORS$text_primary, size = 3
      ) +
      scale_fill_gradient2(
        low = COLORS$accent_red, mid = COLORS$bg_primary, high = COLORS$accent_purple,
        midpoint = 0, name = legend_title
      ) +
      labs(x = "", y = "") +
      theme_rsm() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_text(color = COLORS$text_primary, size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      coord_fixed()

    ggplotly(p) |> layout_rsm()
  })

  # -------------------------------------------------------------------------
  # Interaction table
  # -------------------------------------------------------------------------

  output$interaction_table <- renderDT({
    req(rv$analysis)
    res <- rv$analysis
    scale <- input$coef_scale %||% "original"

    # Select coefficients based on scale (with fallback)
    if (scale == "original") {
      interaction_matrix <- res$interaction_matrix_orig %||% res$interaction_matrix
    } else {
      interaction_matrix <- res$interaction_matrix_std %||% res$interaction_matrix
    }

    int_df <- extract_interactions(interaction_matrix, res$var_names, DISPLAY_THRESHOLD)

    if (nrow(int_df) == 0) {
      return(datatable(
        data.frame(メッセージ = "選択された交互作用・2次項はありません"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }

    # Use type column from extract_interactions()
    is_quadratic <- int_df$type == "quadratic"
    int_df$タイプ <- ifelse(is_quadratic, "2次項", "交互作用")

    # Format variable display
    int_df$項 <- ifelse(is_quadratic,
                       paste0(int_df$var1, "²"),
                       paste0(int_df$var1, " × ", int_df$var2))

    col_name <- if (scale == "original") "係数(元単位)" else "係数(標準化)"
    result_df <- data.frame(
      項 = int_df$項,
      タイプ = int_df$タイプ,
      係数 = int_df$coefficient,
      stringsAsFactors = FALSE
    )
    names(result_df)[3] <- col_name

    datatable(
      result_df,
      options = list(pageLength = 10, dom = "t", ordering = TRUE),
      rownames = FALSE
    ) |>
      formatRound(columns = col_name, digits = 4) |>
      formatStyle(
        columns = "タイプ",
        color = styleEqual(c("交互作用", "2次項"), c(COLORS$accent_purple, COLORS$accent_orange))
      )
  })

  # -------------------------------------------------------------------------
  # Response Surface Plot (応答曲面)
  # -------------------------------------------------------------------------

  # Update surface variable selectors when analysis is done
  observe({
    req(rv$analysis)
    res <- rv$analysis
    var_names <- res$var_names
    int_mat <- abs(res$interaction_matrix_std)

    # Total importance: |main| + |quadratic| + Σ|interactions involving the variable|
    # (the matrix is symmetric, so the row sum covers each interaction once)
    importance <- abs(res$main_effects_std[var_names]) + rowSums(int_mat)
    names(importance) <- var_names
    ranked_vars <- names(sort(importance, decreasing = TRUE))

    updateSelectInput(session, "surface_var1",
                      choices = ranked_vars,
                      selected = if (length(ranked_vars) >= 1) ranked_vars[1] else NULL)
    updateSelectInput(session, "surface_var2",
                      choices = ranked_vars,
                      selected = if (length(ranked_vars) >= 2) ranked_vars[2] else NULL)
  })

  # Dynamic input IDs are index based (user column names may contain any
  # character) and include the analysis run id, so values left over from a
  # previous analysis are never applied to a different variable.
  surface_slider_id <- function(res, v) {
    sprintf("surface_slider_%d_%d", res$run_id, match(v, res$var_names))
  }
  surface_cat_id <- function(res, cat_name) {
    sprintf("surface_cat_%d_%d", res$run_id, match(cat_name, names(res$cm)))
  }

  # Decimal places appropriate for a variable's data range
  range_decimals <- function(x_col) {
    x_range <- diff(range(x_col))
    if (x_range > 0) max(0, min(2 - floor(log10(x_range)), 6)) else 2
  }

  # Dynamic slider UI for 3rd+ variables
  output$surface_slider_ui <- renderUI({
    req(rv$analysis)
    res <- rv$analysis
    var_names <- res$var_names
    cm <- res$cm

    var1 <- input$surface_var1
    var2 <- input$surface_var2
    if (is.null(var1) || is.null(var2)) return(NULL)

    other_vars <- setdiff(var_names, c(var1, var2))
    if (length(other_vars) == 0) {
      return(p(style = sprintf("color: %s; font-size: 0.8rem;", COLORS$text_muted),
               "他の変数なし（2変数モデル）"))
    }

    # Split into complete categorical groups (one dropdown per category)
    # vs. leftover numeric/discrete vars (sliders)
    parts <- partition_surface_vars(other_vars, cm)

    cat_controls <- lapply(names(parts$cat_groups), function(cat_name) {
      info <- cm[[cat_name]]
      tagList(
        div(
          style = "margin-bottom: 0.5rem;",
          span(style = sprintf("color: %s; font-size: 0.8rem; font-weight: 600;", COLORS$text_primary),
               cat_name),
          tags$span(style = sprintf("font-size: 0.6rem; color: %s;", COLORS$accent_purple), " [カテゴリ]")
        ),
        selectInput(
          inputId = surface_cat_id(res, cat_name),
          label = NULL,
          choices = info$levels,
          selected = info$levels[1]
        )
      )
    })

    preset_button <- function(idx, kind, label, color) {
      # One shared input event for all preset buttons (no per-variable observers)
      tags$button(
        type = "button",
        class = "btn btn-default btn-xs",
        style = sprintf("padding: 2px 6px; font-size: 0.65rem; background: %s; border: 1px solid %s; color: %s;",
                        COLORS$bg_tertiary, COLORS$border, color),
        onclick = sprintf(
          "Shiny.setInputValue('surface_preset', {run: %d, idx: %d, kind: '%s'}, {priority: 'event'})",
          res$run_id, idx, kind
        ),
        label
      )
    }

    slider_list <- lapply(parts$leftover, function(v) {
      idx <- match(v, var_names)
      x_col <- res$X[, v]
      x_min <- min(x_col)
      x_max <- max(x_col)
      x_mean <- mean(x_col)
      decimals <- range_decimals(x_col)
      step_val <- round((x_max - x_min) / 20, decimals + 1)
      if (step_val == 0) step_val <- 10^(-decimals)

      tagList(
        div(
          style = "margin-bottom: 0.5rem;",
          span(style = sprintf("color: %s; font-size: 0.8rem; font-weight: 600;", COLORS$text_primary), v),
          div(
            style = "display: inline-flex; gap: 4px; margin-left: 8px;",
            preset_button(idx, "low", "Low", COLORS$accent_blue),
            preset_button(idx, "center", "Center", COLORS$accent_green),
            preset_button(idx, "high", "High", COLORS$accent_red)
          )
        ),
        sliderInput(
          inputId = surface_slider_id(res, v),
          label = NULL,
          # floor/ceiling so rounding never cuts off the data range
          min = floor(x_min * 10^decimals) / 10^decimals,
          max = ceiling(x_max * 10^decimals) / 10^decimals,
          value = round(x_mean, decimals),
          step = step_val
        ),
        p(style = sprintf("color: %s; font-size: 0.65rem; margin-top: -10px;", COLORS$text_muted),
          sprintf("Low: %s | Center: %s | High: %s",
                  smart_format(x_min), smart_format(x_mean), smart_format(x_max)))
      )
    })

    tagList(
      p(style = sprintf("color: %s; font-size: 0.8rem; margin-bottom: 0.5rem;", COLORS$accent_orange),
        sprintf("他の%d変数を固定:", length(parts$cat_groups) + length(parts$leftover))),
      cat_controls,
      slider_list
    )
  })

  # Low/Center/High preset buttons (single observer for all variables)
  observeEvent(input$surface_preset, {
    req(rv$analysis)
    res <- rv$analysis
    ev <- input$surface_preset
    # Ignore clicks from UI of a previous analysis
    if (!identical(as.integer(ev$run), as.integer(res$run_id))) return()
    idx <- as.integer(ev$idx)
    if (is.na(idx) || idx < 1 || idx > length(res$var_names)) return()
    v <- res$var_names[idx]
    x_col <- res$X[, v]
    decimals <- range_decimals(x_col)
    value <- switch(ev$kind, low = min(x_col), center = mean(x_col), high = max(x_col), NULL)
    if (is.null(value)) return()
    updateSliderInput(session, surface_slider_id(res, v), value = round(value, decimals))
  })

  # Extrapolation warning message
  output$extrapolation_warning <- renderUI({
    if (isTRUE(input$surface_allow_extrapolation)) {
      div(
        style = paste0(
          "background-color: ", COLORS$accent_red, ";",
          "color: white;",
          "padding: 8px 12px;",
          "border-radius: 4px;",
          "margin-top: 8px;",
          "font-size: 0.85em;"
        ),
        tags$strong("⚠ 外挿警告"),
        tags$br(),
        "データ範囲外を表示中。",
        tags$br(),
        "予測精度が低下する可能性があります。"
      )
    } else {
      NULL
    }
  })

  # Shared prediction grid for the 3D surface and contour plots
  # (computed once per input change instead of once per plot)
  surface_data <- reactive({
    req(rv$analysis, input$surface_var1, input$surface_var2)
    res <- rv$analysis
    var1 <- input$surface_var1
    var2 <- input$surface_var2
    var_names <- res$var_names
    req(var1 %in% var_names, var2 %in% var_names)
    if (var1 == var2) return(list(error = "X軸とY軸に異なる変数を選択してください"))

    cm <- res$cm
    resolution <- input$surface_resolution %||% 25

    x1_data <- res$X[, var1]
    x2_data <- res$X[, var2]
    x1_range <- range(x1_data)
    x2_range <- range(x2_data)

    # Expand range only if extrapolation is allowed (default: data range only)
    allow_extrap <- isTRUE(input$surface_allow_extrapolation)
    x1_expand <- if (allow_extrap) diff(x1_range) * 0.1 else 0
    x2_expand <- if (allow_extrap) diff(x2_range) * 0.1 else 0

    # Dummy/discrete axes are shown at their actual levels (e.g. {0,1})
    var_types <- res$var_types
    x1_seq <- if (var_types[[var1]] %in% c("dummy", "discrete")) sort(unique(x1_data)) else
      seq(x1_range[1] - x1_expand, x1_range[2] + x1_expand, length.out = resolution)
    x2_seq <- if (var_types[[var2]] %in% c("dummy", "discrete")) sort(unique(x2_data)) else
      seq(x2_range[1] - x2_expand, x2_range[2] + x2_expand, length.out = resolution)
    n1 <- length(x1_seq)
    n2 <- length(x2_seq)

    # Fixed values for the other variables
    fixed <- setNames(numeric(length(var_names)), var_names)
    other_vars <- setdiff(var_names, c(var1, var2))
    parts <- partition_surface_vars(other_vars, cm)
    for (v in parts$leftover) {
      val <- input[[surface_slider_id(res, v)]]
      fixed[v] <- if (is.null(val) || is.na(val)) mean(res$X[, v]) else val
    }
    for (cat_name in names(parts$cat_groups)) {
      info <- cm[[cat_name]]
      sel_level <- input[[surface_cat_id(res, cat_name)]] %||% info$levels[1]
      dcols <- parts$cat_groups[[cat_name]]
      fixed[dcols] <- as.numeric(dummy_level(info, dcols) == sel_level)
    }

    # Prediction grid (x1 varies fastest)
    grid <- matrix(rep(fixed, each = n1 * n2), nrow = n1 * n2,
                   dimnames = list(NULL, var_names))
    grid[, var1] <- rep(x1_seq, times = n2)
    grid[, var2] <- rep(x2_seq, each = n1)

    predictions <- tryCatch(res$predictor(grid)$mean, error = function(e) rep(NA_real_, nrow(grid)))

    # z_matrix[i, j] = f(x1[j], x2[i]) as expected by plotly (x → columns)
    z_matrix <- t(matrix(predictions, nrow = n1, ncol = n2))

    list(
      var1 = var1, var2 = var2,
      x1_seq = x1_seq, x2_seq = x2_seq, z = z_matrix,
      x1_data = x1_data, x2_data = x2_data,
      target = res$target_name %||% "Y",
      x1_ticks = surface_axis_ticks(var1, cm),
      x2_ticks = surface_axis_ticks(var2, cm)
    )
  })

  # Hover text for observed points / BO optimum
  surface_point_text <- function(sd, y) {
    sprintf("%s=%s<br>%s=%s<br>%s=%s",
            htmltools::htmlEscape(sd$var1), formatC(sd$x1_data, digits = 4, format = "g"),
            htmltools::htmlEscape(sd$var2), formatC(sd$x2_data, digits = 4, format = "g"),
            htmltools::htmlEscape(sd$target), formatC(y, digits = 4, format = "g"))
  }
  bo_point <- function(sd) {
    bo <- rv_bo$result
    if (is.null(bo) || !(sd$var1 %in% names(bo$best_x)) || !(sd$var2 %in% names(bo$best_x))) return(NULL)
    list(
      x = unname(bo$best_x[sd$var1]), y = unname(bo$best_x[sd$var2]), z = bo$best_y,
      text = sprintf("★ BO最適解<br>%s=%.3f<br>%s=%.3f<br>Y=%.3f",
                     htmltools::htmlEscape(sd$var1), bo$best_x[sd$var1],
                     htmltools::htmlEscape(sd$var2), bo$best_x[sd$var2], bo$best_y)
    )
  }
  surface_colorscale <- list(
    c(0, COLORS$accent_blue),
    c(0.5, COLORS$accent_purple),
    c(1, COLORS$accent_red)
  )

  # 3D Surface plot
  output$surface_plot <- renderPlotly({
    sd <- surface_data()
    if (!is.null(sd$error)) return(plotly_empty() |> layout(title = sd$error))

    p <- plot_ly() |>
      add_surface(
        x = sd$x1_seq,
        y = sd$x2_seq,
        z = sd$z,
        colorscale = surface_colorscale,
        opacity = 0.85,
        name = "応答曲面",
        showscale = TRUE,
        colorbar = list(title = sd$target)
      )

    if (isTRUE(input$surface_show_points)) {
      p <- p |>
        add_markers(
          x = sd$x1_data,
          y = sd$x2_data,
          z = rv$analysis$y,
          marker = list(
            size = 5,
            color = COLORS$accent_green,
            line = list(color = COLORS$text_primary, width = 1)
          ),
          name = "実測値",
          text = surface_point_text(sd, rv$analysis$y),
          hoverinfo = "text"
        )
    }

    bp <- bo_point(sd)
    if (!is.null(bp)) {
      p <- p |>
        add_markers(
          x = bp$x, y = bp$y, z = bp$z,
          marker = list(
            size = 14, color = "#FFD700", symbol = "diamond",
            line = list(color = "#FF4500", width = 2)
          ),
          name = "BO最適解",
          text = bp$text,
          hoverinfo = "text"
        )
    }

    # Dummy/categorical axes get tick labels showing level names
    xaxis_cfg <- c(list(title = sd$var1, color = COLORS$text_primary, gridcolor = COLORS$border), sd$x1_ticks)
    yaxis_cfg <- c(list(title = sd$var2, color = COLORS$text_primary, gridcolor = COLORS$border), sd$x2_ticks)
    p |>
      layout(
        scene = list(
          xaxis = xaxis_cfg,
          yaxis = yaxis_cfg,
          zaxis = list(title = sd$target, color = COLORS$text_primary, gridcolor = COLORS$border),
          bgcolor = "transparent",
          camera = list(eye = list(x = 1.5, y = 1.5, z = 1.2))
        ),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        font = list(color = COLORS$text_primary),
        legend = list(x = 0.02, y = 0.98, bgcolor = "rgba(22,27,34,0.8)")
      )
  })

  # Contour plot
  output$contour_plot <- renderPlotly({
    sd <- surface_data()
    if (!is.null(sd$error)) return(plotly_empty() |> layout(title = sd$error))

    p <- plot_ly() |>
      add_contour(
        x = sd$x1_seq,
        y = sd$x2_seq,
        z = sd$z,
        colorscale = surface_colorscale,
        contours = list(
          showlabels = TRUE,
          labelfont = list(color = COLORS$text_primary, size = 10)
        ),
        name = "等高線",
        showscale = TRUE,
        colorbar = list(title = sd$target)
      )

    if (isTRUE(input$surface_show_points)) {
      p <- p |>
        add_markers(
          x = sd$x1_data,
          y = sd$x2_data,
          marker = list(
            size = 8,
            color = rv$analysis$y,
            colorscale = surface_colorscale,
            line = list(color = COLORS$text_primary, width = 1),
            showscale = FALSE
          ),
          name = "実測値",
          text = surface_point_text(sd, rv$analysis$y),
          hoverinfo = "text"
        )
    }

    bp <- bo_point(sd)
    if (!is.null(bp)) {
      p <- p |>
        add_markers(
          x = bp$x, y = bp$y,
          marker = list(
            size = 16, color = "#FFD700", symbol = "star",
            line = list(color = "#FF4500", width = 2)
          ),
          name = "BO最適解",
          text = bp$text,
          hoverinfo = "text"
        )
    }

    xaxis_cfg <- c(list(title = sd$var1, color = COLORS$text_primary, gridcolor = COLORS$border), sd$x1_ticks)
    yaxis_cfg <- c(list(title = sd$var2, color = COLORS$text_primary, gridcolor = COLORS$border), sd$x2_ticks)
    p |>
      layout(
        xaxis = xaxis_cfg,
        yaxis = yaxis_cfg,
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        font = list(color = COLORS$text_primary),
        legend = list(x = 0.02, y = 0.98, bgcolor = "rgba(22,27,34,0.8)")
      )
  })

  # =========================================================================
  # BAYESIAN OPTIMIZATION SERVER LOGIC
  # =========================================================================

  # Reactive value for BO results
  rv_bo <- reactiveValues(result = NULL)

  # Best solution display
  output$bo_best_display <- renderUI({
    bo <- rv_bo$result

    if (is.null(bo)) {
      return(div(
        style = sprintf("text-align: center; padding: 1rem; color: %s;", COLORS$text_muted),
        p(style = "font-size: 0.9rem;", "「分析・最適化 実行」ボタンを押すと最適解が表示されます")
      ))
    }

    use_target <- !is.null(bo$target_value)
    direction_text <- if (use_target) {
      sprintf("目標値(%s)近傍", smart_format(bo$target_value))
    } else if (bo$minimize) "最小値" else "最大値"

    # Best Y metric box
    y_label <- if (use_target) "予測値 (目標近傍)" else paste0("予測 ", direction_text)
    y_box <- div(
      class = "metric-box",
      style = sprintf("border-color: %s;", COLORS$accent_green),
      div(class = "metric-label", y_label),
      div(class = "metric-value success", smart_format(bo$best_y))
    )
    # For target mode, also show deviation
    if (use_target) {
      y_box <- tagList(
        y_box,
        div(
          class = "metric-box",
          style = sprintf("border-color: %s;", COLORS$accent_orange),
          div(class = "metric-label", "目標との偏差"),
          div(class = "metric-value", smart_format(abs(bo$best_y - bo$target_value)))
        )
      )
    }

    cm <- bo$categorical_mapping
    vt <- bo$var_types
    var_boxes <- list()

    # [1] Categorical variables -- reconstructed from dummies
    for (cat_name in names(cm)) {
      var_boxes <- c(var_boxes, list(div(
        class = "metric-box",
        div(class = "metric-label", cat_name,
            tags$span(style = sprintf("font-size: 0.6rem; color: %s;", COLORS$accent_purple), " [カテゴリ]")),
        div(class = "metric-value", style = sprintf("color: %s;", COLORS$accent_blue),
            decode_category(cm[[cat_name]], bo$best_x))
      )))
    }

    # [2] Non-dummy variables -- continuous, discrete
    dummy_cols <- unlist(lapply(cm, function(x) x$dummy_cols))
    for (v in names(bo$best_x)) {
      if (v %in% dummy_cols) next
      val <- bo$best_x[[v]]
      is_discrete <- !is.null(vt) && !is.na(vt[v]) && vt[v] == "discrete"
      type_tag <- if (is_discrete) {
        tags$span(style = sprintf("font-size: 0.6rem; color: %s;", COLORS$accent_orange), " [離散]")
      } else NULL
      display_val <- if (is_discrete) as.character(round(val)) else smart_format(val)
      var_boxes <- c(var_boxes, list(div(
        class = "metric-box",
        div(class = "metric-label", v, type_tag),
        div(class = "metric-value", style = sprintf("color: %s;", COLORS$accent_blue), display_val)
      )))
    }

    dir_display <- if (use_target) {
      sprintf("目標値: %s", smart_format(bo$target_value))
    } else direction_text
    info_boxes <- tagList(
      div(class = "metric-box",
          div(class = "metric-label", "探索反復数"),
          div(class = "metric-value", bo$n_iter_actual)),
      div(class = "metric-box",
          div(class = "metric-label", "最適化方向"),
          div(class = "metric-value", style = "font-size: 1rem;", dir_display))
    )

    tagList(div(class = "metric-grid", y_box, var_boxes, info_boxes))
  })

  # Convergence plot
  output$bo_convergence_plot <- renderPlotly({
    req(rv_bo$result)
    bo <- rv_bo$result

    direction_label <- if (!is.null(bo$target_value)) {
      sprintf("最良値 (目標: %s)", smart_format(bo$target_value))
    } else if (bo$minimize) "最良値 (最小)" else "最良値 (最大)"

    p <- ggplot(bo$convergence, aes(x = iteration, y = best_y)) +
      geom_line(color = COLORS$accent_green, linewidth = 1.2) +
      geom_point(color = COLORS$accent_green, size = 2, alpha = 0.7) +
      labs(x = "反復回数", y = direction_label) +
      theme_rsm()

    if (!is.null(bo$target_value)) {
      p <- p + geom_hline(yintercept = bo$target_value, linetype = "dashed",
                          color = COLORS$accent_orange, linewidth = 0.8)
    }

    ggplotly(p) |> layout_rsm()
  })

  # EI plot
  output$bo_ei_plot <- renderPlotly({
    req(rv_bo$result)
    bo <- rv_bo$result

    # Remove rows without EI (initial point / final local refinement)
    hist_ei <- bo$history[!is.na(bo$history$EI), ]

    if (nrow(hist_ei) == 0) {
      return(plotly_empty() |> layout(title = "EIデータなし"))
    }

    p <- ggplot(hist_ei, aes(x = iteration, y = EI)) +
      geom_col(fill = COLORS$accent_purple, alpha = 0.7, width = 0.8) +
      geom_line(color = COLORS$accent_blue, linewidth = 0.8) +
      labs(x = "反復回数", y = "Expected Improvement") +
      theme_rsm()

    ggplotly(p) |> layout_rsm()
  })

  # Search history table
  output$bo_history_table <- renderDT({
    req(rv_bo$result)
    bo <- rv_bo$result

    df <- bo$history
    type_labels <- c("initial" = "初期最良", "improved" = "改善", "explored" = "探索")
    matched <- type_labels[df$type]
    df$type <- ifelse(is.na(matched), df$type, matched)

    # Reconstruct categorical variables from dummy columns
    cm <- bo$categorical_mapping
    for (cat_name in names(cm)) {
      info <- cm[[cat_name]]
      existing <- intersect(info$dummy_cols, names(df))
      if (length(existing) == 0) next
      cat_col <- rep(info$reference, nrow(df))
      for (dc in existing) {
        active_rows <- !is.na(df[[dc]]) & df[[dc]] == 1
        cat_col[active_rows] <- dummy_level(info, dc)
      }
      # Replace the dummy columns by one categorical column at the same position
      first_pos <- min(which(names(df) %in% existing))
      df[existing] <- NULL
      new_col <- setNames(data.frame(cat_col, stringsAsFactors = FALSE), cat_name)
      after_idx <- if (first_pos <= ncol(df)) first_pos:ncol(df) else integer(0)
      df <- cbind(df[, seq_len(first_pos - 1), drop = FALSE], new_col,
                  df[, after_idx, drop = FALSE])
    }

    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    for (col in num_cols) df[[col]] <- round(df[[col]], 6)

    col_rename <- c(
      "iteration" = "反復",
      "predicted_y" = "予測Y",
      "EI" = "EI",
      "SE" = "SE",
      "type" = "ステータス"
    )
    for (old_name in names(col_rename)) {
      idx <- which(names(df) == old_name)
      if (length(idx) == 1) names(df)[idx] <- col_rename[old_name]
    }

    datatable(
      df,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "frtip",
        order = list(list(0, "desc"))
      ),
      rownames = FALSE,
      selection = "none"
    ) |>
      formatStyle(
        columns = "ステータス",
        color = styleEqual(
          c("初期最良", "改善", "探索"),
          c(COLORS$accent_blue, COLORS$accent_green, COLORS$text_muted)
        ),
        fontWeight = styleEqual(
          c("初期最良", "改善", "探索"),
          c("bold", "bold", "normal")
        )
      )
  })

  # =========================================================================
  # PREDICTION SIMULATOR
  # =========================================================================

  # Index-based input IDs (see surface_slider_id)
  pred_var_id <- function(res, v) sprintf("pred_%d_v%d", res$run_id, match(v, res$var_names))
  pred_cat_id <- function(res, cat_name) sprintf("pred_%d_c%d", res$run_id, match(cat_name, names(res$cm)))

  # Dynamic input UI for prediction
  output$pred_input_ui <- renderUI({
    req(rv$analysis)
    res <- rv$analysis
    cm <- res$cm
    dummy_cols <- unlist(lapply(cm, function(x) x$dummy_cols))

    box <- function(...) div(style = "display: inline-block; width: 140px; margin-right: 8px; vertical-align: top;", ...)
    inputs <- list()

    # Categorical variable selectors
    for (cat_name in names(cm)) {
      lv <- cm[[cat_name]]$levels
      inputs <- c(inputs, list(box(
        selectInput(pred_cat_id(res, cat_name), cat_name, choices = lv, selected = lv[1], width = "100%")
      )))
    }

    # Continuous/discrete variable inputs
    for (v in res$var_names) {
      if (v %in% dummy_cols) next
      x_col <- res$X[, v]
      x_step <- diff(range(x_col)) / 20
      if (x_step == 0) x_step <- 1
      inputs <- c(inputs, list(box(
        numericInput(pred_var_id(res, v), v,
                     value = signif(mean(x_col), 6), step = signif(x_step, 2), width = "100%")
      )))
    }

    div(style = "display: flex; flex-wrap: wrap; gap: 4px; padding: 0.5rem 0;", inputs)
  })

  # Fill with BO optimal values
  observeEvent(input$pred_fill_bo, {
    req(rv$analysis, rv_bo$result)
    res <- rv$analysis
    bo <- rv_bo$result
    cm <- res$cm

    for (cat_name in names(cm)) {
      updateSelectInput(session, pred_cat_id(res, cat_name),
                        selected = decode_category(cm[[cat_name]], bo$best_x))
    }
    dummy_cols <- unlist(lapply(cm, function(x) x$dummy_cols))
    for (v in intersect(names(bo$best_x), res$var_names)) {
      if (v %in% dummy_cols) next
      updateNumericInput(session, pred_var_id(res, v), value = signif(bo$best_x[[v]], 8))
    }
  })

  # Fill with mean values
  observeEvent(input$pred_fill_mean, {
    req(rv$analysis)
    res <- rv$analysis
    cm <- res$cm

    for (cat_name in names(cm)) {
      updateSelectInput(session, pred_cat_id(res, cat_name), selected = cm[[cat_name]]$levels[1])
    }
    dummy_cols <- unlist(lapply(cm, function(x) x$dummy_cols))
    for (v in res$var_names) {
      if (v %in% dummy_cols) next
      updateNumericInput(session, pred_var_id(res, v), value = signif(mean(res$X[, v]), 6))
    }
  })

  # Reactive prediction result
  output$pred_result_display <- renderUI({
    req(rv$analysis)
    res <- rv$analysis
    var_names <- res$var_names
    cm <- res$cm
    dummy_cols <- unlist(lapply(cm, function(x) x$dummy_cols))

    # Build new data row from inputs
    x_new <- setNames(numeric(length(var_names)), var_names)
    for (v in setdiff(var_names, dummy_cols)) {
      val <- input[[pred_var_id(res, v)]]
      if (is.null(val) || !is.numeric(val) || !is.finite(val)) return(NULL)
      x_new[v] <- val
    }
    for (cat_name in names(cm)) {
      info <- cm[[cat_name]]
      selected_level <- input[[pred_cat_id(res, cat_name)]]
      if (is.null(selected_level)) return(NULL)
      x_new[info$dummy_cols] <- as.numeric(info$dummy_levels == selected_level)
    }

    pred <- tryCatch(
      res$predictor(matrix(x_new, nrow = 1, dimnames = list(NULL, var_names))),
      error = function(e) NULL
    )
    if (is.null(pred)) {
      return(p(style = sprintf("color: %s;", COLORS$text_muted), "予測計算中..."))
    }

    y_hat <- pred$mean[1]
    se <- pred$se[1]
    res_se <- pred$residual_se
    # 95% prediction interval: y_hat +/- t * sqrt(se^2 + residual_se^2)
    df_resid <- res$fit$df.residual
    pi_text <- if (df_resid > 0 && is.finite(res_se)) {
      pi_half <- qt(0.975, df_resid) * sqrt(se^2 + res_se^2)
      sprintf("%s ~ %s", smart_format(y_hat - pi_half), smart_format(y_hat + pi_half))
    } else {
      "N/A (残差自由度なし)"
    }

    div(
      style = sprintf("background: %s; border: 1px solid %s; border-radius: 8px; padding: 1rem; margin-top: 0.5rem;",
                      COLORS$bg_secondary, COLORS$border),
      div(style = "display: flex; gap: 1.5rem; align-items: center; flex-wrap: wrap;",
          div(
            span(style = sprintf("color: %s; font-size: 0.75rem;", COLORS$text_muted), "予測値"),
            div(style = sprintf("font-size: 1.5rem; font-weight: 700; color: %s;", COLORS$accent_green),
                smart_format(y_hat))
          ),
          div(
            span(style = sprintf("color: %s; font-size: 0.75rem;", COLORS$text_muted), "95%予測区間"),
            div(style = sprintf("font-size: 1rem; color: %s;", COLORS$text_primary), pi_text)
          ),
          div(
            span(style = sprintf("color: %s; font-size: 0.75rem;", COLORS$text_muted), "SE"),
            div(style = sprintf("font-size: 0.9rem; color: %s;", COLORS$text_muted),
                smart_format(se))
          )
      )
    )
  })

  # CSV download handler for BO results
  output$bo_download_csv <- downloadHandler(
    filename = function() {
      paste0("bo_optimal_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      bo <- rv_bo$result
      if (is.null(bo)) {
        write.csv(data.frame(message = "no optimization result"), file, row.names = FALSE)
        return(invisible())
      }

      cm <- bo$categorical_mapping
      dummy_cols <- unlist(lapply(cm, function(x) x$dummy_cols))
      best_row <- list()

      # Categorical variables first (decoded from dummies)
      for (cat_name in names(cm)) {
        best_row[[cat_name]] <- decode_category(cm[[cat_name]], bo$best_x)
      }
      for (v in names(bo$best_x)) {
        if (v %in% dummy_cols) next
        best_row[[v]] <- unname(bo$best_x[v])
      }
      best_row[["predicted_Y"]] <- bo$best_y

      if (!is.null(bo$target_value)) {
        best_row[["optimization"]] <- paste0("target=", bo$target_value)
        best_row[["deviation"]] <- abs(bo$best_y - bo$target_value)
      } else {
        best_row[["optimization"]] <- if (bo$minimize) "minimize" else "maximize"
      }

      df <- as.data.frame(best_row, stringsAsFactors = FALSE, check.names = FALSE)
      # UTF-8 with BOM so Excel opens Japanese headers correctly
      con <- file(file, open = "wb")
      on.exit(close(con))
      writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
      write.csv(df, con, row.names = FALSE, fileEncoding = "")
    }
  )
}

# ============================================================================
# RUN APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)
