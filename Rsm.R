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
  library(shinydashboard)
  library(shinyWidgets)
  # Note: rsm package loaded for potential future enhancements (contour plots, canonical analysis)
  # Current implementation uses standard lm() for flexibility with stepwise selection
  library(rsm)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(DT)
  library(plotly)
  library(scales)
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
  base_font = font_google("IBM Plex Sans"),
  heading_font = font_google("IBM Plex Mono"),
  code_font = font_google("IBM Plex Mono")
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
#'   - mapping: Named list of { dummy_cols, reference, levels } per encoded column
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

    # Create one dummy per non-reference level
    for (lvl in levels_sorted[-1]) {
      dummy_name <- paste0(col, "_", lvl)
      # Avoid name collision
      while (dummy_name %in% names(df)) {
        dummy_name <- paste0(dummy_name, "_d")
      }
      df[[dummy_name]] <- as.integer(as.character(vals) == lvl)
      dummy_names <- c(dummy_names, dummy_name)
    }

    mapping[[col]] <- list(
      dummy_cols = dummy_names,
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

#' Validate numeric data for RSM analysis
#' @param X Matrix of predictors
#' @param y Response vector
#' @return List with validated data and diagnostics
validate_data <- function(X, y) {
  issues <- character(0)

  # Check for complete cases
  complete_idx <- complete.cases(X, y)
  n_missing <- sum(!complete_idx)

  if (n_missing > 0) {
    issues <- c(issues, sprintf("%d行の欠損値を除外", n_missing))
    X <- X[complete_idx, , drop = FALSE]
    y <- y[complete_idx]
  }

  # Check for constant columns (handle NA from single observation)
  const_cols <- apply(X, 2, function(col) {
    v <- var(col, na.rm = TRUE)
    is.na(v) || v < .Machine$double.eps
  })
  if (any(const_cols, na.rm = TRUE)) {
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
    cor_mat <- tryCatch(cor(X), error = function(e) NULL)
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
                          scale = "original", mx = NULL) {
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
      m1 <- mx[v1]
      if (is_quadratic) {
        if (m1 < 0) {
          term <- sprintf("(%s + %s)²", v1, smart_format(abs(m1)))
        } else {
          term <- sprintf("(%s - %s)²", v1, smart_format(m1))
        }
      } else {
        m2 <- mx[v2]
        part1 <- if (m1 < 0) sprintf("(%s + %s)", v1, smart_format(abs(m1))) else sprintf("(%s - %s)", v1, smart_format(m1))
        part2 <- if (m2 < 0) sprintf("(%s + %s)", v2, smart_format(abs(m2))) else sprintf("(%s - %s)", v2, smart_format(m2))
        term <- paste0(part1, part2)
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
# STEPWISE VARIABLE SELECTION (逐次減増法)
# ============================================================================

#' Get partial F-ratios using drop1() - Type II/III SS (順序非依存)
#' @param fit lm object
#' @return Named vector of partial F-ratios for each term
get_drop1_f_ratios <- function(fit) {
  tryCatch({
    # drop1() gives partial F-test for each term (Type II SS)
    # This tests "what happens if we remove this term, given all others"
    drop_result <- drop1(fit, test = "F")

    # Extract F values (column name is "F value")
    f_col <- grep("^F", names(drop_result), value = TRUE)
    if (length(f_col) == 0) {
      return(numeric(0))
    }

    f_values <- drop_result[[f_col[1]]]
    term_names <- rownames(drop_result)

    # Remove <none> row (represents not dropping anything)
    none_idx <- which(term_names == "<none>")
    if (length(none_idx) > 0) {
      f_values <- f_values[-none_idx]
      term_names <- term_names[-none_idx]
    }

    # Handle NA values
    f_values[is.na(f_values)] <- 0
    names(f_values) <- term_names
    f_values
  }, error = function(e) {
    numeric(0)
  })
}

#' Get partial F-ratios for candidate terms using add1()
#' @param fit Current lm object
#' @param candidates Character vector of candidate terms to test
#' @param data Data frame for fitting
#' @return Named vector of partial F-ratios for each candidate
get_add1_f_ratios <- function(fit, candidates, data) {
  tryCatch({
    # Build scope formula with all candidates
    scope_formula <- as.formula(paste("~ . +", paste(candidates, collapse = " + ")))

    # add1() gives partial F-test for each candidate term
    # This tests "what happens if we add this term, given current model"
    add_result <- add1(fit, scope = scope_formula, test = "F", data = data)

    # Extract F values
    f_col <- grep("^F", names(add_result), value = TRUE)
    if (length(f_col) == 0) {
      return(numeric(0))
    }

    f_values <- add_result[[f_col[1]]]
    term_names <- rownames(add_result)

    # Remove <none> row
    none_idx <- which(term_names == "<none>")
    if (length(none_idx) > 0) {
      f_values <- f_values[-none_idx]
      term_names <- term_names[-none_idx]
    }

    # Handle NA values
    f_values[is.na(f_values)] <- 0
    names(f_values) <- term_names
    f_values
  }, error = function(e) {
    numeric(0)
  })
}

#' Build interaction term name, respecting dummy variable rules
#' Dummy variables use original name; continuous variables use centered (_c) name
#' @param v1 Variable 1 name
#' @param v2 Variable 2 name
#' @param is_dummy Named logical vector indicating which variables are dummies
#' @return Formula term string, e.g., "I(X1_c * Color_B)"
make_interaction_term <- function(v1, v2, is_dummy) {
  t1 <- if (is_dummy[v1]) v1 else paste0(v1, "_c")
  t2 <- if (is_dummy[v2]) v2 else paste0(v2, "_c")
  sprintf("I(%s * %s)", t1, t2)
}

#' Check if a main effect is required by hierarchy (has dependent higher-order terms)
#' Uses pattern matching to detect any interaction or quadratic term containing
#' the variable, regardless of whether it was centered or not.
#' @param main_var The main effect variable name
#' @param selected_terms Currently selected terms
#' @param var_names All variable names
#' @return TRUE if this main effect cannot be removed due to hierarchy
is_required_by_hierarchy <- function(main_var, selected_terms, var_names) {
  # Check if any quadratic term depends on this variable
  quad_term <- sprintf("I(%s_c^2)", main_var)
  if (quad_term %in% selected_terms) {
    return(TRUE)
  }

  # Check if any interaction term in selected_terms contains this variable
  # Matches both centered (var_c) and original (var) forms
  # Pattern: I(... * ...) where either side contains this variable
  esc_var <- gsub("([.+?^${}()|\\[\\]\\\\])", "\\\\\\1", main_var)
  pattern <- sprintf("^I\\((%s_c|%s) \\* |\\* (%s_c|%s)\\)$", esc_var, esc_var, esc_var, esc_var)
  if (any(grepl(pattern, selected_terms))) {
    return(TRUE)
  }

  return(FALSE)
}

#' Match term name to F-ratio names (handles I() wrapper and : notation)
#' @param term Term name from formula (e.g., "I(X1 * X2)")
#' @param f_ratio_names Names from drop1/add1 result
#' @return Matched name or NA if not found
match_term_to_f_name <- function(term, f_ratio_names) {
  # Try exact match first
  if (term %in% f_ratio_names) {
    return(term)
  }
  # Try without I() wrapper
  clean_term <- gsub("^I\\((.*)\\)$", "\\1", term)
  if (clean_term %in% f_ratio_names) {
    return(clean_term)
  }
  # Try converting * to : (R's interaction notation)
  colon_term <- gsub(" \\* ", ":", clean_term)
  if (colon_term %in% f_ratio_names) {
    return(colon_term)
  }
  return(NA_character_)
}

#' Perform stepwise variable selection (逐次減増法)
#' Uses drop1()/add1() for partial F-tests (Type II SS - 順序非依存)
#' Optionally enforces hierarchy during selection
#' @param df Data frame with predictors and response Y
#' @param all_terms All possible terms (main, interaction, quadratic)
#' @param var_names Original variable names (for hierarchy)
#' @param model_type "interaction" or "quadratic"
#' @param f_in F-ratio threshold for entry (default 2)
#' @param f_out F-ratio threshold for removal (default 2)
#' @param enforce_hierarchy Whether to enforce hierarchy principle (default TRUE)
#' @param max_iter Maximum iterations (default 100)
#' @return List with selected terms and selection history
stepwise_selection <- function(
    df,
    all_terms,
    var_names,
    model_type = "quadratic",
    f_in = 2,
    f_out = 2,
    enforce_hierarchy = TRUE,
    max_iter = 100
) {
  # Start with full model (all terms selected)
  selected_terms <- all_terms
  removed_terms <- character(0)
  history <- list()
  iteration <- 0

  # Track previous states to detect oscillation
  state_history <- character(0)

  message("=== 逐次減増法 開始 (部分F検定使用) ===")
  message(sprintf("初期モデル: %d 項", length(selected_terms)))

  while (iteration < max_iter) {
    iteration <- iteration + 1
    changed <- FALSE

    # Check for oscillation (same state seen before)
    current_state <- paste(sort(selected_terms), collapse = ",")
    if (current_state %in% state_history) {
      message(sprintf("  振動検出 - 終了 (反復 %d 回)", iteration))
      break
    }
    state_history <- c(state_history, current_state)

    # --- BACKWARD STEP (後退ステップ) using drop1() ---
    if (length(selected_terms) > 1) {  # Need at least 1 term to drop
      # Fit current model
      formula_str <- paste("Y ~", paste(selected_terms, collapse = " + "))
      fit <- tryCatch(
        lm(as.formula(formula_str), data = df),
        error = function(e) NULL
      )

      if (!is.null(fit)) {
        # Get partial F-ratios using drop1() (Type II SS)
        f_ratios <- get_drop1_f_ratios(fit)

        if (length(f_ratios) > 0) {
          # Match term names using helper function
          matched_f <- sapply(selected_terms, function(term) {
            matched_name <- match_term_to_f_name(term, names(f_ratios))
            if (!is.na(matched_name)) f_ratios[matched_name] else Inf
          })
          names(matched_f) <- selected_terms

          # Find terms eligible for removal (F <= f_out)
          # BUT: check hierarchy - don't remove main effects needed by higher-order terms
          eligible_for_removal <- matched_f[matched_f <= f_out]

          if (length(eligible_for_removal) > 0) {
            # Sort by F-ratio (ascending) and try to remove
            sorted_candidates <- names(sort(eligible_for_removal))

            for (candidate in sorted_candidates) {
              # Check if this is a main effect required by hierarchy
              if (enforce_hierarchy && candidate %in% var_names) {
                if (is_required_by_hierarchy(candidate, selected_terms, var_names)) {
                  message(sprintf("  [%d] スキップ: %s (階層原則により削除不可, F=%.3f)",
                                iteration, candidate, eligible_for_removal[candidate]))
                  next
                }
              }

              # Can remove this term
              selected_terms <- setdiff(selected_terms, candidate)
              removed_terms <- c(removed_terms, candidate)
              changed <- TRUE

              message(sprintf("  [%d] 削除: %s (部分F=%.3f)",
                            iteration, candidate, eligible_for_removal[candidate]))

              history[[length(history) + 1]] <- list(
                iteration = iteration,
                action = "remove",
                term = candidate,
                f_ratio = eligible_for_removal[candidate]
              )
              break  # Remove one term per backward step
            }
          }
        }
      }
    }

    # --- FORWARD STEP (前進ステップ) using add1() ---
    # Only from iteration 2 onwards
    # Keep adding terms while any removed term has F > f_in
    if (iteration >= 2 && length(removed_terms) > 0 && length(selected_terms) > 0) {
      forward_continue <- TRUE

      while (forward_continue && length(removed_terms) > 0) {
        # Fit current model
        formula_str <- paste("Y ~", paste(selected_terms, collapse = " + "))
        fit <- tryCatch(
          lm(as.formula(formula_str), data = df),
          error = function(e) NULL
        )

        if (is.null(fit)) {
          forward_continue <- FALSE
          next
        }

        # Get partial F-ratios for candidates using add1()
        f_ratios_add <- get_add1_f_ratios(fit, removed_terms, df)

        if (length(f_ratios_add) == 0) {
          forward_continue <- FALSE
          next
        }

        # Match term names using helper function
        matched_f <- sapply(removed_terms, function(term) {
          matched_name <- match_term_to_f_name(term, names(f_ratios_add))
          if (!is.na(matched_name)) f_ratios_add[matched_name] else 0
        })
        names(matched_f) <- removed_terms

        # Find best term to add (F > f_in)
        eligible_for_add <- matched_f[matched_f > f_in]

        if (length(eligible_for_add) > 0) {
          # Add the term with highest F-ratio
          best_term <- names(which.max(eligible_for_add))
          best_f <- eligible_for_add[best_term]

          # Check hierarchy: if adding interaction/quadratic, ensure main effects are present
          terms_to_add <- best_term
          hierarchy_additions <- character(0)

          if (enforce_hierarchy) {
            # Check if this is an interaction term
            if (grepl("^I\\(.*\\*.*\\)$", best_term)) {
              # Extract variable names from interaction (e.g., "I(X1_c * Color_B)" -> ["X1_c", "Color_B"])
              inner <- gsub("^I\\((.*)\\)$", "\\1", best_term)
              vars <- trimws(strsplit(inner, "\\*")[[1]])
              for (v in vars) {
                # Map to original variable name:
                # If the part is directly in var_names, use as-is (e.g., dummy "Color_B")
                # Otherwise strip _c suffix (e.g., "X1_c" → "X1")
                base_v <- if (v %in% var_names) v else sub("_c$", "", v)
                if (!(base_v %in% selected_terms) && !(base_v %in% terms_to_add)) {
                  if (base_v %in% removed_terms) {
                    hierarchy_additions <- c(hierarchy_additions, base_v)
                  }
                }
              }
            }

            # Check if this is a quadratic term
            if (grepl("^I\\(.*\\^2\\)$", best_term)) {
              inner <- gsub("^I\\((.*)\\^2\\)$", "\\1", best_term)
              base_v <- if (inner %in% var_names) inner else sub("_c$", "", inner)
              if (!(base_v %in% selected_terms) && !(base_v %in% terms_to_add)) {
                if (base_v %in% removed_terms) {
                  hierarchy_additions <- c(hierarchy_additions, base_v)
                }
              }
            }
          }

          # Add all terms (best term + hierarchy requirements if enabled)
          all_to_add <- c(terms_to_add, hierarchy_additions)
          selected_terms <- c(selected_terms, all_to_add)
          removed_terms <- setdiff(removed_terms, all_to_add)
          changed <- TRUE

          message(sprintf("  [%d] 追加: %s (部分F=%.3f)",
                        iteration, best_term, best_f))

          history[[length(history) + 1]] <- list(
            iteration = iteration,
            action = "add",
            term = best_term,
            f_ratio = best_f
          )

          # Log hierarchy additions
          for (h_term in hierarchy_additions) {
            message(sprintf("  [%d] 階層追加: %s (%s のため)",
                          iteration, h_term, best_term))
            history[[length(history) + 1]] <- list(
              iteration = iteration,
              action = "hierarchy_add",
              term = h_term,
              f_ratio = NA
            )
          }
        } else {
          # No more terms to add
          forward_continue <- FALSE
        }
      }
    }

    # Check convergence
    if (!changed) {
      message(sprintf("  収束 (反復 %d 回)", iteration))
      break
    }
  }

  # Final hierarchy check (safety net, only if enforce_hierarchy is TRUE)
  # Note: Quadratic terms use centered names (_c suffix)
  hierarchy_added <- FALSE
  if (enforce_hierarchy) {
    if (model_type == "quadratic") {
      for (var in var_names) {
        quad_term <- sprintf("I(%s_c^2)", var)
        if (quad_term %in% selected_terms && !(var %in% selected_terms)) {
          selected_terms <- c(var, selected_terms)
          removed_terms <- setdiff(removed_terms, var)
          message(sprintf("  [最終階層] 強制追加: %s (2次項 %s が選択されたため)", var, quad_term))
          hierarchy_added <- TRUE

          history[[length(history) + 1]] <- list(
            iteration = iteration + 1,
            action = "hierarchy_final",
            term = var,
            f_ratio = NA
          )
        }
      }
    }

    # Check all interaction terms in selected_terms:
    # Extract variable names from I(... * ...) patterns and ensure their main effects are present
    int_selected <- grep("^I\\(.*\\*.*\\)$", selected_terms, value = TRUE)
    for (int_term in int_selected) {
      # Extract the two operands from "I(A * B)"
      inner <- gsub("^I\\((.*)\\)$", "\\1", int_term)
      parts <- trimws(strsplit(inner, "\\*")[[1]])
      # Map back to original variable names:
      # If part is directly in var_names, use as-is (dummy variable)
      # Otherwise strip _c suffix (centered continuous variable)
      for (part in parts) {
        orig_var <- if (part %in% var_names) part else sub("_c$", "", part)
        if (orig_var %in% var_names && !(orig_var %in% selected_terms)) {
          selected_terms <- c(orig_var, selected_terms)
          removed_terms <- setdiff(removed_terms, orig_var)
          message(sprintf("  [最終階層] 強制追加: %s (交互作用 %s が選択されたため)", orig_var, int_term))
          hierarchy_added <- TRUE
        }
      }
    }

    # If hierarchy was enforced at the end, just log it
    # (coefficients will be correctly calculated in run_rsm_analysis)
    if (hierarchy_added) {
      message("  [最終階層] 階層原則により主効果を追加")
    }
  }

  message(sprintf("=== 逐次減増法 終了: %d 項選択 ===", length(selected_terms)))

  list(
    selected_terms = selected_terms,
    removed_terms = removed_terms,
    history = history,
    n_iterations = iteration
  )
}

# ============================================================================
# RSM ANALYSIS WRAPPER
# ============================================================================

#' Perform RSM (Response Surface Methodology) analysis with stepwise selection
#' @param X Predictor matrix
#' @param y Response vector
#' @param model_type Type of model: "interaction" or "quadratic"
#' @param use_stepwise Whether to use stepwise variable selection
#' @param f_in F-ratio threshold for entry (default 2)
#' @param f_out F-ratio threshold for removal (default 2)
#' @param enforce_hierarchy Whether to enforce hierarchy principle (default TRUE)
#' @return List with fit and extracted results
#' @throws Error if analysis fails
run_rsm_analysis <- function(
    X, y,
    model_type = "quadratic",
    use_stepwise = TRUE,
    f_in = 2,
    f_out = 2,
    enforce_hierarchy = TRUE,
    categorical_mapping = NULL
) {
  # Input validation
  if (!is.matrix(X)) X <- as.matrix(X)

  var_names <- colnames(X)
  p <- ncol(X)
  n <- nrow(X)

  if (n < p + 1) {
    stop(sprintf("サンプル数(%d)が変数数(%d)に対して不足しています", n, p))
  }

  # Compute scaling info
  mx <- colMeans(X, na.rm = TRUE)
  sx <- apply(X, 2, sd, na.rm = TRUE)
  sx[sx < .Machine$double.eps] <- 1  # Avoid division by zero

  # Compute Y scaling info
  my <- mean(y, na.rm = TRUE)
  sy <- sd(y, na.rm = TRUE)
  if (is.na(sy) || sy < .Machine$double.eps) sy <- 1  # Avoid division by zero

  # Center and scale X for standardized coefficients
  X_centered <- sweep(X, 2, mx, "-")
  X_scaled <- sweep(X_centered, 2, sx, "/")

  # Create data frame for model fitting
  df_orig <- as.data.frame(X)
  df_orig$Y <- y

  # Add centered columns for quadratic terms (Xi_c = Xi - mean(Xi))
  # Main effects use original Xi, but interactions and quadratics use centered Xi_c
  centered_names <- paste0(var_names, "_c")
  for (i in seq_along(var_names)) {
    df_orig[[centered_names[i]]] <- X[, i] - mx[i]
  }

  # Full standardization: (Y - mean(Y)) / sd(Y)
  df_scaled <- as.data.frame(X_scaled)
  df_scaled$Y <- (y - my) / sy

  # Add centered columns for scaled data (already centered, so just copy)
  for (i in seq_along(var_names)) {
    df_scaled[[centered_names[i]]] <- X_centered[, i] / sx[i]
  }

  # ---------------------------------------------------------------------------
  # Detect dummy variables and categorical groups
  # Dummy: binary 0/1 → D² = D, so quadratic term is collinear with main effect
  # Same-category dummies: mutually exclusive → their product is always 0
  # ---------------------------------------------------------------------------
  is_dummy <- sapply(var_names, function(v) {
    vals <- unique(X[, v])
    vals <- vals[!is.na(vals)]
    length(vals) <= 2 && all(vals %in% c(0, 1))
  })

  # Build a lookup: variable name → original categorical group name (or NA)
  dummy_group <- rep(NA_character_, p)
  names(dummy_group) <- var_names
  if (!is.null(categorical_mapping)) {
    for (cat_name in names(categorical_mapping)) {
      for (dv in categorical_mapping[[cat_name]]$dummy_cols) {
        if (dv %in% var_names) {
          dummy_group[dv] <- cat_name
        }
      }
    }
  }

  # Build all possible terms based on model type
  # First order terms (use original Xi - NOT centered)
  fo_terms <- var_names

  # Interaction terms:
  #   continuous × continuous → (Xi_c * Xj_c)  [both centered]
  #   continuous × dummy      → (Xi_c * Dj)    [only continuous centered]
  #   dummy × dummy (diff cat) → (Di * Dj)     [neither centered]
  # SKIP interactions between dummies from the SAME categorical variable
  # (mutually exclusive → product is always 0 → zero-variance column)
  int_terms <- character(0)
  if (p >= 2) {
    for (i in 1:(p-1)) {
      for (j in (i+1):p) {
        # Check if both belong to the same categorical group
        gi <- dummy_group[var_names[i]]
        gj <- dummy_group[var_names[j]]
        if (!is.na(gi) && !is.na(gj) && gi == gj) {
          next  # Skip: same category → mutually exclusive → product always 0
        }
        int_terms <- c(int_terms, make_interaction_term(var_names[i], var_names[j], is_dummy))
      }
    }
  }

  # Quadratic terms: (Xi - X̄i)² — continuous variables only
  # SKIP for dummy variables: D ∈ {0,1} → D² = D → perfectly collinear with main effect
  quad_terms <- character(0)
  if (model_type == "quadratic") {
    for (i in seq_len(p)) {
      if (!is_dummy[var_names[i]]) {
        quad_terms <- c(quad_terms, sprintf("I(%s_c^2)", var_names[i]))
      }
    }
  }

  # Combine all terms
  all_terms <- c(fo_terms, int_terms, quad_terms)

  # Apply stepwise selection if enabled
  if (use_stepwise) {
    stepwise_result <- stepwise_selection(
      df = df_orig,
      all_terms = all_terms,
      var_names = var_names,
      model_type = model_type,
      f_in = f_in,
      f_out = f_out,
      enforce_hierarchy = enforce_hierarchy
    )
    selected_terms <- stepwise_result$selected_terms
    selection_history <- stepwise_result$history
  } else {
    selected_terms <- all_terms
    selection_history <- list()
  }

  # Build final formula with selected terms
  if (length(selected_terms) == 0) {
    # If no terms selected, use intercept only model
    formula_str <- "Y ~ 1"
  } else {
    formula_str <- paste("Y ~", paste(selected_terms, collapse = " + "))
  }
  model_formula <- as.formula(formula_str)

  # Fit model on original scale
  fit_orig <- tryCatch(
    lm(model_formula, data = df_orig),
    error = function(e) {
      stop(sprintf("モデルフィッティング失敗: %s", e$message))
    }
  )

  # Fit model on scaled data for standardized coefficients
  fit_scaled <- tryCatch(
    lm(model_formula, data = df_scaled),
    error = function(e) {
      stop(sprintf("標準化モデルフィッティング失敗: %s", e$message))
    }
  )

  # Extract coefficients
  coef_orig <- coef(fit_orig)
  coef_scaled <- coef(fit_scaled)

  # Intercept
  intercept_orig <- coef_orig["(Intercept)"]
  if (is.na(intercept_orig)) intercept_orig <- 0
  intercept_std <- 0  # Standardized model has intercept 0 by design

  # Main effects (only for selected terms)
  main_effects_orig <- rep(0, p)
  names(main_effects_orig) <- var_names
  main_effects_std <- rep(0, p)
  names(main_effects_std) <- var_names

  for (var in var_names) {
    if (var %in% selected_terms && var %in% names(coef_orig)) {
      main_effects_orig[var] <- coef_orig[var]
      if (var %in% names(coef_scaled)) {
        main_effects_std[var] <- coef_scaled[var]
      }
    }
  }
  main_effects_orig[is.na(main_effects_orig)] <- 0
  main_effects_std[is.na(main_effects_std)] <- 0

  # Build interaction matrix
  interaction_matrix_orig <- matrix(0, nrow = p, ncol = p)
  rownames(interaction_matrix_orig) <- var_names
  colnames(interaction_matrix_orig) <- var_names

  interaction_matrix_std <- matrix(0, nrow = p, ncol = p)
  rownames(interaction_matrix_std) <- var_names
  colnames(interaction_matrix_std) <- var_names

  # Fill interaction terms (only for selected terms)
  # Term names built by make_interaction_term: continuous uses _c, dummy uses original
  if (p >= 2) {
    for (i in 1:(p-1)) {
      for (j in (i+1):p) {
        term_name <- make_interaction_term(var_names[i], var_names[j], is_dummy)
        if (term_name %in% selected_terms && term_name %in% names(coef_orig)) {
          val_orig <- coef_orig[term_name]
          val_std <- coef_scaled[term_name]
          if (!is.na(val_orig)) {
            interaction_matrix_orig[i, j] <- val_orig
            interaction_matrix_orig[j, i] <- val_orig
          }
          if (!is.na(val_std)) {
            interaction_matrix_std[i, j] <- val_std
            interaction_matrix_std[j, i] <- val_std
          }
        }
      }
    }
  }

  # Fill quadratic terms (on diagonal, only for selected terms)
  # Note: Uses centered term names (_c suffix)
  if (model_type == "quadratic") {
    for (i in 1:p) {
      term_name <- sprintf("I(%s_c^2)", var_names[i])
      if (term_name %in% selected_terms && term_name %in% names(coef_orig)) {
        val_orig <- coef_orig[term_name]
        val_std <- coef_scaled[term_name]
        if (!is.na(val_orig)) {
          interaction_matrix_orig[i, i] <- val_orig
        }
        if (!is.na(val_std)) {
          interaction_matrix_std[i, i] <- val_std
        }
      }
    }
  }

  # Compute predictions
  predictions <- predict(fit_orig, newdata = df_orig)

  # Model summary for diagnostics
  model_summary <- summary(fit_orig)

  list(
    fit = fit_orig,
    fit_scaled = fit_scaled,
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
    # Legacy/fallback fields (used with %||% operator throughout UI code)
    main_effects = main_effects_std,
    interaction_matrix = interaction_matrix_std,
    intercept = intercept_orig,
    # Predictions (original scale)
    predictions = as.vector(predictions),
    # RSM specific
    r_squared = model_summary$r.squared,
    adj_r_squared = model_summary$adj.r.squared,
    f_statistic = model_summary$fstatistic,
    coefficients_table = model_summary$coefficients,
    # Stepwise selection info
    selected_terms = selected_terms,
    all_terms = all_terms,
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

#' Snap values to nearest valid level for non-continuous variables
#' @param x Named numeric vector of variable values
#' @param var_types Named character vector from detect_variable_types()
#' @param X Original data matrix (to extract unique levels for discrete vars)
#' @return Snapped numeric vector
snap_to_valid <- function(x, var_types, X) {
  for (v in names(x)) {
    vtype <- var_types[v]
    if (is.na(vtype)) next
    if (vtype == "dummy") {
      levels <- sort(unique(X[, v]))
      x[v] <- levels[which.min(abs(levels - x[v]))]
    } else if (vtype == "discrete") {
      levels <- sort(unique(X[, v]))
      x[v] <- levels[which.min(abs(levels - x[v]))]
    }
  }
  x
}

#' Enforce mutual exclusivity for categorical dummy groups
#' For each group of dummies from the same category, at most one can be 1.
#' If L-BFGS-B produced multiple 1s, keep the one closest to 1, set rest to 0.
#' @param x Named numeric vector
#' @param cat_groups List of integer vectors (column indices per group)
#' @param var_names Variable names for indexing
#' @return Corrected numeric vector
enforce_categorical_constraints <- function(x, cat_groups, var_names) {
  for (grp_idx in cat_groups) {
    grp_vals <- x[var_names[grp_idx]]
    # After snapping, each should be 0 or 1. If multiple are 1, keep highest raw value.
    n_active <- sum(grp_vals >= 0.5)
    if (n_active > 1) {
      # Pick the one with highest raw value (most "confident")
      winner <- grp_idx[which.max(grp_vals)]
      x[var_names[grp_idx]] <- 0
      x[var_names[winner]] <- 1
    }
  }
  x
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
  if (!is.null(cm)) {
    for (cat_name in names(cm)) {
      dcols <- cm[[cat_name]]$dummy_cols
      if (length(dcols) > 0 && all(dcols %in% other_vars)) {
        cat_groups[[cat_name]] <- dcols
        covered <- c(covered, dcols)
      }
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
  if (is.null(cm)) return(NULL)
  for (cat_name in names(cm)) {
    info <- cm[[cat_name]]
    if (var_name %in% info$dummy_cols) {
      lvl <- sub(paste0("^", cat_name, "_"), "", var_name)
      return(list(tickvals = c(0, 1), ticktext = c(info$reference, lvl)))
    }
  }
  NULL
}

#' Predict from RSM model with standard error at new points
#' @param fit lm object from RSM analysis
#' @param newdata Data frame of new points (original scale)
#' @param mx Named vector of variable means (for centering)
#' @param var_names Variable names
#' @return List with mean predictions and standard errors
rsm_predict_with_se <- function(fit, newdata, mx, var_names) {
  df <- as.data.frame(newdata)
  for (v in var_names) {
    df[[paste0(v, "_c")]] <- df[[v]] - mx[v]
  }
  pred <- predict(fit, newdata = df, se.fit = TRUE)
  list(
    mean = as.numeric(pred$fit),
    se = as.numeric(pred$se.fit),
    residual_se = pred$residual.scale
  )
}

#' Expected Improvement acquisition function
#' @param mu Predicted mean
#' @param sigma Predicted standard error (model uncertainty)
#' @param best_y Current best value
#' @param minimize If TRUE, minimize Y; if FALSE, maximize Y
#' @param target_value If not NULL, minimize (Y - target)^2 instead
#' @return EI values (always non-negative) or acquisition scores for target mode
expected_improvement <- function(mu, sigma, best_y, minimize = FALSE, target_value = NULL) {
  sigma[sigma < 1e-10] <- 1e-10

  if (!is.null(target_value)) {
    # Target mode: minimize (Y - target)^2
    # Point estimate of score + exploration bonus from uncertainty
    point_score <- (mu - target_value)^2
    improvement <- best_y - point_score  # best_y = best_score
    acq <- improvement + sigma^2         # sigma^2 as exploration bonus
    pmax(acq, 0)
  } else {
    if (minimize) {
      improvement <- best_y - mu
    } else {
      improvement <- mu - best_y
    }

    z <- improvement / sigma
    ei <- improvement * pnorm(z) + sigma * dnorm(z)
    ei[sigma < 1e-10] <- 0
    ei
  }
}

#' Run Bayesian Optimization using RSM regression as surrogate
#' @param fit lm object from RSM
#' @param X Original predictor matrix
#' @param y Original response vector
#' @param mx Named vector of variable means
#' @param var_names Variable names
#' @param n_iter Number of BO iterations
#' @param n_random Number of random candidates per iteration
#' @param minimize If TRUE, seek minimum; if FALSE, seek maximum
#' @param bounds 2 x p matrix (row 1 = lower, row 2 = upper). NULL = data range
#' @param target_value If not NULL, minimize (Y - target)^2
#' @return List with best_x, best_y, history, convergence
run_bayesian_optimization <- function(
    fit, X, y, mx, var_names,
    n_iter = 30,
    n_random = 500,
    minimize = FALSE,
    bounds = NULL,
    categorical_mapping = NULL,
    target_value = NULL
) {
  # Target mode forces minimize on (Y - target)^2
  use_target <- !is.null(target_value)
  p <- length(var_names)

  # Detect variable types (dummy / discrete / continuous)
  var_types <- detect_variable_types(X)

  # Pre-compute unique levels for discrete/dummy variables
  discrete_levels <- lapply(var_names, function(v) {
    if (var_types[v] %in% c("dummy", "discrete")) {
      sort(unique(X[, v]))
    } else {
      NULL
    }
  })
  names(discrete_levels) <- var_names

  # Build categorical group information for mutual exclusivity
  # cat_groups: list of lists, each with dummy column indices in var_names
  cat_groups <- list()
  if (!is.null(categorical_mapping)) {
    for (cat_name in names(categorical_mapping)) {
      grp_cols <- categorical_mapping[[cat_name]]$dummy_cols
      grp_idx <- which(var_names %in% grp_cols)
      if (length(grp_idx) >= 2) {
        cat_groups[[cat_name]] <- grp_idx
      }
    }
  }

  # Default bounds from data range
  if (is.null(bounds)) {
    bounds <- rbind(
      apply(X, 2, min, na.rm = TRUE),
      apply(X, 2, max, na.rm = TRUE)
    )
    colnames(bounds) <- var_names
  }

  # Evaluate model at all observed data points
  pred_init <- rsm_predict_with_se(fit, as.data.frame(X), mx, var_names)
  if (use_target) {
    scores_init <- (pred_init$mean - target_value)^2
    best_idx <- which.min(scores_init)
    best_score <- scores_init[best_idx]
  } else if (minimize) {
    best_idx <- which.min(pred_init$mean)
  } else {
    best_idx <- which.max(pred_init$mean)
  }
  best_y <- pred_init$mean[best_idx]
  best_x <- as.numeric(X[best_idx, ])
  names(best_x) <- var_names

  # History tracking
  history_list <- list()
  history_list[[1]] <- list(
    iteration = 0, x = best_x, predicted_y = best_y,
    ei = NA_real_, se = pred_init$se[best_idx], type = "initial"
  )
  convergence_y <- best_y
  ei_best <- if (use_target) best_score else best_y

  for (iter in seq_len(n_iter)) {
    # Random candidate generation - respecting variable types
    candidates <- matrix(nrow = n_random, ncol = p)
    for (j in seq_len(p)) {
      v <- var_names[j]
      if (var_types[v] %in% c("dummy", "discrete")) {
        lvls <- discrete_levels[[v]]
        candidates[, j] <- sample(lvls, n_random, replace = TRUE)
      } else {
        candidates[, j] <- runif(n_random, bounds[1, j], bounds[2, j])
      }
    }
    colnames(candidates) <- var_names

    # Enforce mutual exclusivity for categorical groups
    # For each group of dummies from the same original category,
    # at most one dummy can be 1 (the rest must be 0)
    for (grp_idx in cat_groups) {
      for (row in seq_len(n_random)) {
        # Randomly choose: either one dummy is 1, or all are 0 (= reference level)
        active <- sample(c(0, grp_idx), 1)  # 0 means reference (all dummies = 0)
        candidates[row, grp_idx] <- 0L
        if (active > 0) {
          candidates[row, active] <- 1L
        }
      }
    }

    # Predict and compute EI at candidates
    pred <- rsm_predict_with_se(fit, as.data.frame(candidates), mx, var_names)
    ei <- expected_improvement(pred$mean, pred$se, ei_best, minimize, target_value)

    # Best random candidate
    best_cand_idx <- which.max(ei)
    new_x <- candidates[best_cand_idx, ]
    new_ei <- ei[best_cand_idx]

    # L-BFGS-B refinement (only for continuous variables)
    # For mixed problems: optimize continuous vars, snap discrete after
    tryCatch({
      obj_fn <- function(x_vec) {
        names(x_vec) <- var_names
        # Snap discrete/dummy variables and enforce categorical constraints
        x_vec <- snap_to_valid(x_vec, var_types, X)
        x_vec <- enforce_categorical_constraints(x_vec, cat_groups, var_names)
        df_tmp <- as.data.frame(t(x_vec))
        p_tmp <- rsm_predict_with_se(fit, df_tmp, mx, var_names)
        -expected_improvement(p_tmp$mean, p_tmp$se, ei_best, minimize, target_value)
      }

      opt_result <- optim(
        par = new_x,
        fn = obj_fn,
        method = "L-BFGS-B",
        lower = bounds[1, ],
        upper = bounds[2, ]
      )

      if (-opt_result$value > new_ei) {
        new_x <- opt_result$par
        names(new_x) <- var_names
        new_ei <- -opt_result$value
      }
    }, error = function(e) NULL)

    # Snap discrete/dummy variables to valid levels, then enforce mutual exclusivity
    names(new_x) <- var_names
    new_x <- snap_to_valid(new_x, var_types, X)
    new_x <- enforce_categorical_constraints(new_x, cat_groups, var_names)
    new_pred <- rsm_predict_with_se(fit, as.data.frame(t(new_x)), mx, var_names)
    new_y <- new_pred$mean[1]
    new_se <- new_pred$se[1]

    # Update best
    if (use_target) {
      new_score <- (new_y - target_value)^2
      improved <- new_score < best_score
      if (improved) {
        best_score <- new_score
        best_y <- new_y
        best_x <- new_x
        ei_best <- best_score
      }
    } else {
      improved <- if (minimize) (new_y < best_y) else (new_y > best_y)
      if (improved) {
        best_y <- new_y
        best_x <- new_x
        ei_best <- best_y
      }
    }

    history_list[[iter + 1]] <- list(
      iteration = iter, x = new_x, predicted_y = new_y,
      ei = new_ei, se = new_se,
      type = if (improved) "improved" else "explored"
    )
    convergence_y <- c(convergence_y, best_y)

    # Early stop if EI negligible
    if (new_ei < 1e-12) break
  }

  # Build tidy data frames
  history_df <- do.call(rbind, lapply(history_list, function(h) {
    x_vals <- as.list(h$x)
    names(x_vals) <- var_names
    c(
      list(iteration = h$iteration),
      x_vals,
      list(
        predicted_y = h$predicted_y,
        EI = h$ei,
        SE = h$se,
        type = h$type
      )
    ) |> as.data.frame(stringsAsFactors = FALSE, check.names = FALSE)
  }))

  convergence_df <- data.frame(
    iteration = seq(0, length(convergence_y) - 1),
    best_y = convergence_y,
    stringsAsFactors = FALSE
  )

  list(
    best_x = best_x,
    best_y = best_y,
    history = history_df,
    convergence = convergence_df,
    bounds = bounds,
    minimize = minimize,
    var_types = var_types,
    n_iter_actual = length(history_list) - 1,
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
        df <- read.csv(
          input$data_file$datapath,
          header = isTRUE(input$header),
          fileEncoding = input$encoding %||% "UTF-8",
          stringsAsFactors = FALSE
        )
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
            check.names = FALSE
          ))
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

        # n_params = main effects + interaction terms + quadratic terms
        n_params <- active_counts$n_main + active_counts$n_higher_order
        metrics <- compute_metrics(y, analysis_result$predictions, n_params)

        incProgress(0.15, detail = "結果整理中")

        # Count selected terms
        n_selected <- length(analysis_result$selected_terms)
        n_total <- length(analysis_result$all_terms)

        # Store results
        rv$analysis <- list(
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
            fit = analysis_result$fit,
            X = X,
            y = y,
            mx = analysis_result$mx,
            var_names = colnames(X),
            n_iter = 30,
            n_random = 500,
            minimize = minimize,
            bounds = NULL,
            categorical_mapping = rv$categorical_mapping,
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
          sprintf("分析・最適化完了（R²=%.3f%s）",
                  metrics$r_squared %||% 0, bo_info),
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
      input$target_var,
      scale = scale,
      mx = res$mx
    )
    HTML(equation)
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

    range_min <- min(c(df$actual, df$predicted)) * 0.95
    range_max <- max(c(df$actual, df$predicted)) * 1.05

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
        aes(x = x, ymin = x * 0.9, ymax = x * 1.1),
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
    int_mat <- res$interaction_matrix_std

    # Calculate total importance: |main| + Σ|interactions| + |quadratic|
    importance <- sapply(var_names, function(v) {
      # Main effect
      main_contrib <- abs(res$main_effects_std[v])

      # Quadratic term (diagonal)
      idx <- which(var_names == v)
      quad_contrib <- if (length(idx) == 1) abs(int_mat[idx, idx]) else 0

      # Interaction terms (row + column, avoiding double count)
      int_contrib <- 0
      if (length(idx) == 1 && !is.null(int_mat)) {
        # Sum of absolute interaction coefficients involving this variable
        row_vals <- int_mat[idx, ]
        col_vals <- int_mat[, idx]
        # Upper triangle only to avoid double counting, exclude diagonal
        for (j in seq_along(var_names)) {
          if (j != idx) {
            int_contrib <- int_contrib + abs(int_mat[min(idx, j), max(idx, j)])
          }
        }
      }

      main_contrib + quad_contrib + int_contrib
    })
    names(importance) <- var_names
    ranked_vars <- names(sort(importance, decreasing = TRUE))

    # Update selectors with ranked variables
    updateSelectInput(session, "surface_var1",
                      choices = ranked_vars,
                      selected = if (length(ranked_vars) >= 1) ranked_vars[1] else NULL)
    updateSelectInput(session, "surface_var2",
                      choices = ranked_vars,
                      selected = if (length(ranked_vars) >= 2) ranked_vars[2] else NULL)
  })

  # Dynamic slider UI for 3rd+ variables
  output$surface_slider_ui <- renderUI({
    req(rv$analysis)
    res <- rv$analysis
    var_names <- res$var_names
    cm <- rv$categorical_mapping

    # Get selected X and Y axis variables
    var1 <- input$surface_var1
    var2 <- input$surface_var2

    if (is.null(var1) || is.null(var2)) return(NULL)

    # Other variables (not on axes)
    other_vars <- setdiff(var_names, c(var1, var2))

    if (length(other_vars) == 0) {
      return(p(style = sprintf("color: %s; font-size: 0.8rem;", COLORS$text_muted),
               "他の変数なし（2変数モデル）"))
    }

    # Split into complete categorical groups (rendered as one dropdown per
    # category) vs. leftover numeric/discrete vars (rendered as sliders)
    parts <- partition_surface_vars(other_vars, cm)

    # One category selector per complete categorical group
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
          inputId = paste0("surface_cat_", make.names(cat_name)),
          label = NULL,
          choices = info$levels,
          selected = info$levels[1]
        )
      )
    })

    # Create sliders with Low/Center/High presets for each leftover variable
    slider_list <- lapply(parts$leftover, function(v) {
      # Get range from original data
      x_col <- res$X[, v]
      x_min <- min(x_col, na.rm = TRUE)
      x_max <- max(x_col, na.rm = TRUE)
      x_mean <- mean(x_col, na.rm = TRUE)

      # Determine appropriate decimal places based on data scale
      x_range <- x_max - x_min
      if (x_range > 0) {
        # Calculate decimals needed to show meaningful step changes
        magnitude <- floor(log10(x_range))
        decimals <- max(0, 2 - magnitude)  # More decimals for smaller ranges
        decimals <- min(decimals, 6)  # Cap at 6 decimal places
      } else {
        decimals <- 2
      }
      round_val <- function(x) round(x, decimals)
      step_val <- round(x_range / 20, decimals + 1)
      if (step_val == 0) step_val <- 10^(-decimals)

      tagList(
        div(
          style = "margin-bottom: 0.5rem;",
          span(style = sprintf("color: %s; font-size: 0.8rem; font-weight: 600;", COLORS$text_primary),
               v),
          # Low/Center/High preset buttons
          div(
            style = "display: inline-flex; gap: 4px; margin-left: 8px;",
            actionButton(
              inputId = paste0("surface_preset_low_", v),
              label = "Low",
              class = "btn-xs",
              style = sprintf("padding: 2px 6px; font-size: 0.65rem; background: %s; border: 1px solid %s; color: %s;",
                            COLORS$bg_tertiary, COLORS$border, COLORS$accent_blue)
            ),
            actionButton(
              inputId = paste0("surface_preset_center_", v),
              label = "Center",
              class = "btn-xs",
              style = sprintf("padding: 2px 6px; font-size: 0.65rem; background: %s; border: 1px solid %s; color: %s;",
                            COLORS$bg_tertiary, COLORS$border, COLORS$accent_green)
            ),
            actionButton(
              inputId = paste0("surface_preset_high_", v),
              label = "High",
              class = "btn-xs",
              style = sprintf("padding: 2px 6px; font-size: 0.65rem; background: %s; border: 1px solid %s; color: %s;",
                            COLORS$bg_tertiary, COLORS$border, COLORS$accent_red)
            )
          )
        ),
        sliderInput(
          inputId = paste0("surface_slider_", v),
          label = NULL,
          min = round_val(x_min),
          max = round_val(x_max),
          value = round_val(x_mean),
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

  # Observers for Low/Center/High preset buttons
  # Use once=TRUE to prevent multiple observer registration (Shiny common pitfall)
  # Observers are created once for all variables; values are computed dynamically
  observe({
    req(rv$analysis)
    var_names <- rv$analysis$var_names

    # Create observers only once for each variable
    lapply(var_names, function(v) {
      # Helper to get appropriate decimal places for this variable's data range
      get_decimals <- function(x_col) {
        x_range <- diff(range(x_col, na.rm = TRUE))
        if (x_range > 0) {
          magnitude <- floor(log10(x_range))
          decimals <- max(0, min(2 - magnitude, 6))
        } else {
          decimals <- 2
        }
        decimals
      }

      # Low button - get current values dynamically inside handler
      observeEvent(input[[paste0("surface_preset_low_", v)]], {
        req(rv$analysis, v %in% colnames(rv$analysis$X))
        x_col <- rv$analysis$X[, v]
        decimals <- get_decimals(x_col)
        x_min <- min(x_col, na.rm = TRUE)
        updateSliderInput(session, paste0("surface_slider_", v), value = round(x_min, decimals))
      }, ignoreInit = TRUE)

      # Center button
      observeEvent(input[[paste0("surface_preset_center_", v)]], {
        req(rv$analysis, v %in% colnames(rv$analysis$X))
        x_col <- rv$analysis$X[, v]
        decimals <- get_decimals(x_col)
        x_mean <- mean(x_col, na.rm = TRUE)
        updateSliderInput(session, paste0("surface_slider_", v), value = round(x_mean, decimals))
      }, ignoreInit = TRUE)

      # High button
      observeEvent(input[[paste0("surface_preset_high_", v)]], {
        req(rv$analysis, v %in% colnames(rv$analysis$X))
        x_col <- rv$analysis$X[, v]
        decimals <- get_decimals(x_col)
        x_max <- max(x_col, na.rm = TRUE)
        updateSliderInput(session, paste0("surface_slider_", v), value = round(x_max, decimals))
      }, ignoreInit = TRUE)
    })
  }) |> bindEvent(rv$analysis, once = TRUE)  # Run only once when first analysis completes

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

  # 3D Surface plot
  output$surface_plot <- renderPlotly({
    req(rv$analysis, input$surface_var1, input$surface_var2)
    res <- rv$analysis

    var1 <- input$surface_var1
    var2 <- input$surface_var2

    # Validation
    if (var1 == var2) {
      return(plotly_empty() |> layout(title = "X軸とY軸に異なる変数を選択してください"))
    }

    var_names <- res$var_names
    cm <- rv$categorical_mapping
    resolution <- input$surface_resolution %||% 25
    show_points <- isTRUE(input$surface_show_points)

    # Get data ranges for selected variables
    x1_data <- res$X[, var1]
    x2_data <- res$X[, var2]
    x1_range <- range(x1_data, na.rm = TRUE)
    x2_range <- range(x2_data, na.rm = TRUE)

    # Expand range only if extrapolation is allowed (default: within data range only)
    allow_extrap <- isTRUE(input$surface_allow_extrapolation)
    x1_expand <- if (allow_extrap) diff(x1_range) * 0.1 else 0
    x2_expand <- if (allow_extrap) diff(x2_range) * 0.1 else 0

    # Dummy/discrete axes are shown at their actual levels (e.g. {0,1}) instead
    # of an interpolated continuous grid, which would be meaningless for them
    var_types <- detect_variable_types(res$X)
    x1_is_snapped <- !is.na(var_types[var1]) && var_types[var1] %in% c("dummy", "discrete")
    x2_is_snapped <- !is.na(var_types[var2]) && var_types[var2] %in% c("dummy", "discrete")
    x1_seq <- if (x1_is_snapped) sort(unique(x1_data)) else
      seq(x1_range[1] - x1_expand, x1_range[2] + x1_expand, length.out = resolution)
    x2_seq <- if (x2_is_snapped) sort(unique(x2_data)) else
      seq(x2_range[1] - x2_expand, x2_range[2] + x2_expand, length.out = resolution)
    n1 <- length(x1_seq)
    n2 <- length(x2_seq)

    # Create prediction grid
    grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)
    names(grid) <- c(var1, var2)

    # Set other variables: complete categorical groups from their dropdown,
    # leftover continuous/discrete variables from their slider (or mean)
    other_vars <- setdiff(var_names, c(var1, var2))
    parts <- partition_surface_vars(other_vars, cm)
    for (v in parts$leftover) {
      slider_id <- paste0("surface_slider_", v)
      slider_val <- input[[slider_id]]
      if (is.null(slider_val)) {
        slider_val <- mean(res$X[, v], na.rm = TRUE)
      }
      grid[[v]] <- slider_val
    }
    for (cat_name in names(parts$cat_groups)) {
      dcols <- parts$cat_groups[[cat_name]]
      info <- cm[[cat_name]]
      sel_level <- input[[paste0("surface_cat_", make.names(cat_name))]] %||% info$levels[1]
      for (dc in dcols) {
        lvl <- sub(paste0("^", cat_name, "_"), "", dc)
        grid[[dc]] <- as.integer(identical(lvl, sel_level))
      }
    }

    # Reorder columns to match original variable order
    grid <- grid[, var_names, drop = FALSE]

    # Add centered columns for quadratic terms (Xi_c = Xi - mean(Xi))
    # Uses original training data means from res$mx
    for (v in var_names) {
      centered_col <- paste0(v, "_c")
      grid[[centered_col]] <- grid[[v]] - res$mx[v]
    }

    # Predict using fitted model
    predictions <- tryCatch({
      predict(res$fit, newdata = as.data.frame(grid))
    }, error = function(e) {
      rep(NA, nrow(grid))
    })

    # Reshape for surface plot
    # expand.grid order: x1 varies fastest (rows 1:n1 have x2[1])
    # matrix(byrow=FALSE) fills column-by-column: z[i,j] = f(x1[i], x2[j])
    # plotly add_surface expects: z[i,j] at position (x[j], y[i])
    # So we need z[i,j] = f(x1[j], x2[i]), which requires transpose
    z_matrix <- matrix(predictions, nrow = n1, ncol = n2, byrow = FALSE)
    z_matrix <- t(z_matrix)

    # Create 3D surface plot
    p <- plot_ly() |>
      add_surface(
        x = x1_seq,
        y = x2_seq,
        z = z_matrix,
        colorscale = list(
          c(0, COLORS$accent_blue),
          c(0.5, COLORS$accent_purple),
          c(1, COLORS$accent_red)
        ),
        opacity = 0.85,
        name = "応答曲面",
        showscale = TRUE,
        colorbar = list(title = input$target_var %||% "Y")
      )

    # Add actual data points if requested
    if (show_points) {
      p <- p |>
        add_markers(
          x = x1_data,
          y = x2_data,
          z = res$y,
          marker = list(
            size = 5,
            color = COLORS$accent_green,
            line = list(color = COLORS$text_primary, width = 1)
          ),
          name = "実測値",
          text = sprintf("%s=%.2f<br>%s=%.2f<br>%s=%.2f",
                        var1, x1_data, var2, x2_data,
                        input$target_var %||% "Y", res$y),
          hoverinfo = "text"
        )
    }

    # Add BO optimal point (star marker)
    bo <- rv_bo$result
    if (!is.null(bo) && var1 %in% names(bo$best_x) && var2 %in% names(bo$best_x)) {
      opt_x1 <- bo$best_x[var1]
      opt_x2 <- bo$best_x[var2]
      p <- p |>
        add_markers(
          x = opt_x1, y = opt_x2, z = bo$best_y,
          marker = list(
            size = 14, color = "#FFD700", symbol = "diamond",
            line = list(color = "#FF4500", width = 2)
          ),
          name = "BO最適解",
          text = sprintf("★ BO最適解<br>%s=%.3f<br>%s=%.3f<br>Y=%.3f",
                         var1, opt_x1, var2, opt_x2, bo$best_y),
          hoverinfo = "text"
        )
    }

    # Layout (dummy/categorical axes get tick labels showing level names)
    x1_ticks <- surface_axis_ticks(var1, cm)
    x2_ticks <- surface_axis_ticks(var2, cm)
    xaxis_cfg <- c(list(title = var1, color = COLORS$text_primary, gridcolor = COLORS$border), x1_ticks)
    yaxis_cfg <- c(list(title = var2, color = COLORS$text_primary, gridcolor = COLORS$border), x2_ticks)
    p |>
      layout(
        scene = list(
          xaxis = xaxis_cfg,
          yaxis = yaxis_cfg,
          zaxis = list(title = input$target_var %||% "Y", color = COLORS$text_primary, gridcolor = COLORS$border),
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
    req(rv$analysis, input$surface_var1, input$surface_var2)
    res <- rv$analysis

    var1 <- input$surface_var1
    var2 <- input$surface_var2

    if (var1 == var2) {
      return(plotly_empty() |> layout(title = "X軸とY軸に異なる変数を選択してください"))
    }

    var_names <- res$var_names
    cm <- rv$categorical_mapping
    resolution <- input$surface_resolution %||% 25
    show_points <- isTRUE(input$surface_show_points)

    # Get data ranges
    x1_data <- res$X[, var1]
    x2_data <- res$X[, var2]
    x1_range <- range(x1_data, na.rm = TRUE)
    x2_range <- range(x2_data, na.rm = TRUE)

    # Expand range only if extrapolation is allowed (default: within data range only)
    allow_extrap <- isTRUE(input$surface_allow_extrapolation)
    x1_expand <- if (allow_extrap) diff(x1_range) * 0.1 else 0
    x2_expand <- if (allow_extrap) diff(x2_range) * 0.1 else 0

    # Dummy/discrete axes are shown at their actual levels (e.g. {0,1}) instead
    # of an interpolated continuous grid, which would be meaningless for them
    var_types <- detect_variable_types(res$X)
    x1_is_snapped <- !is.na(var_types[var1]) && var_types[var1] %in% c("dummy", "discrete")
    x2_is_snapped <- !is.na(var_types[var2]) && var_types[var2] %in% c("dummy", "discrete")
    x1_seq <- if (x1_is_snapped) sort(unique(x1_data)) else
      seq(x1_range[1] - x1_expand, x1_range[2] + x1_expand, length.out = resolution)
    x2_seq <- if (x2_is_snapped) sort(unique(x2_data)) else
      seq(x2_range[1] - x2_expand, x2_range[2] + x2_expand, length.out = resolution)
    n1 <- length(x1_seq)
    n2 <- length(x2_seq)

    # Create prediction grid
    grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)
    names(grid) <- c(var1, var2)

    # Set other variables: complete categorical groups from their dropdown,
    # leftover continuous/discrete variables from their slider (or mean)
    other_vars <- setdiff(var_names, c(var1, var2))
    parts <- partition_surface_vars(other_vars, cm)
    for (v in parts$leftover) {
      slider_id <- paste0("surface_slider_", v)
      slider_val <- input[[slider_id]]
      if (is.null(slider_val)) {
        slider_val <- mean(res$X[, v], na.rm = TRUE)
      }
      grid[[v]] <- slider_val
    }
    for (cat_name in names(parts$cat_groups)) {
      dcols <- parts$cat_groups[[cat_name]]
      info <- cm[[cat_name]]
      sel_level <- input[[paste0("surface_cat_", make.names(cat_name))]] %||% info$levels[1]
      for (dc in dcols) {
        lvl <- sub(paste0("^", cat_name, "_"), "", dc)
        grid[[dc]] <- as.integer(identical(lvl, sel_level))
      }
    }

    grid <- grid[, var_names, drop = FALSE]

    # Add centered columns for quadratic terms (Xi_c = Xi - mean(Xi))
    # Uses original training data means from res$mx
    for (v in var_names) {
      centered_col <- paste0(v, "_c")
      grid[[centered_col]] <- grid[[v]] - res$mx[v]
    }

    predictions <- tryCatch({
      predict(res$fit, newdata = as.data.frame(grid))
    }, error = function(e) {
      rep(NA, nrow(grid))
    })

    # Reshape for contour plot (same logic as surface plot)
    # expand.grid order: x1 varies fastest -> matrix column-by-column -> transpose for plotly
    z_matrix <- matrix(predictions, nrow = n1, ncol = n2, byrow = FALSE)
    z_matrix <- t(z_matrix)

    # Create contour plot
    p <- plot_ly() |>
      add_contour(
        x = x1_seq,
        y = x2_seq,
        z = z_matrix,
        colorscale = list(
          c(0, COLORS$accent_blue),
          c(0.5, COLORS$accent_purple),
          c(1, COLORS$accent_red)
        ),
        contours = list(
          showlabels = TRUE,
          labelfont = list(color = COLORS$text_primary, size = 10)
        ),
        name = "等高線",
        showscale = TRUE,
        colorbar = list(title = input$target_var %||% "Y")
      )

    # Add actual data points
    if (show_points) {
      p <- p |>
        add_markers(
          x = x1_data,
          y = x2_data,
          marker = list(
            size = 8,
            color = res$y,
            colorscale = list(
              c(0, COLORS$accent_blue),
              c(0.5, COLORS$accent_purple),
              c(1, COLORS$accent_red)
            ),
            line = list(color = COLORS$text_primary, width = 1),
            showscale = FALSE
          ),
          name = "実測値",
          text = sprintf("%s=%.2f<br>%s=%.2f<br>%s=%.2f",
                        var1, x1_data, var2, x2_data,
                        input$target_var %||% "Y", res$y),
          hoverinfo = "text"
        )
    }

    # Add BO optimal point (star marker)
    bo <- rv_bo$result
    if (!is.null(bo) && var1 %in% names(bo$best_x) && var2 %in% names(bo$best_x)) {
      opt_x1 <- bo$best_x[var1]
      opt_x2 <- bo$best_x[var2]
      p <- p |>
        add_markers(
          x = opt_x1, y = opt_x2,
          marker = list(
            size = 16, color = "#FFD700", symbol = "star",
            line = list(color = "#FF4500", width = 2)
          ),
          name = "BO最適解",
          text = sprintf("★ BO最適解<br>%s=%.3f<br>%s=%.3f<br>Y=%.3f",
                         var1, opt_x1, var2, opt_x2, bo$best_y),
          hoverinfo = "text"
        )
    }

    x1_ticks <- surface_axis_ticks(var1, cm)
    x2_ticks <- surface_axis_ticks(var2, cm)
    xaxis_cfg <- c(list(title = var1, color = COLORS$text_primary, gridcolor = COLORS$border), x1_ticks)
    yaxis_cfg <- c(list(title = var2, color = COLORS$text_primary, gridcolor = COLORS$border), x2_ticks)
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
      deviation <- abs(bo$best_y - bo$target_value)
      y_box <- tagList(
        y_box,
        div(
          class = "metric-box",
          style = sprintf("border-color: %s;", COLORS$accent_orange),
          div(class = "metric-label", "目標との偏差"),
          div(class = "metric-value", smart_format(deviation))
        )
      )
    }

    # Build reverse mapping: dummy_col -> { cat_name, level }
    cm <- bo$categorical_mapping
    dummy_to_cat <- list()
    if (!is.null(cm)) {
      for (cat_name in names(cm)) {
        info <- cm[[cat_name]]
        for (dc in info$dummy_cols) {
          # Extract level name from dummy column: "Material_B" -> "B"
          lvl <- sub(paste0("^", cat_name, "_"), "", dc)
          dummy_to_cat[[dc]] <- list(cat_name = cat_name, level = lvl, reference = info$reference)
        }
      }
    }

    # Reconstruct categorical variables from dummy groups, then show non-dummy vars
    vt <- bo$var_types
    var_boxes <- list()

    # [1] Categorical variables -- reconstructed from dummies
    if (!is.null(cm)) {
      for (cat_name in names(cm)) {
        info <- cm[[cat_name]]
        # Find which dummy is 1 (if any); otherwise reference level
        active_level <- info$reference
        for (dc in info$dummy_cols) {
          if (!is.na(bo$best_x[dc]) && bo$best_x[dc] == 1) {
            active_level <- sub(paste0("^", cat_name, "_"), "", dc)
            break
          }
        }
        var_boxes <- c(var_boxes, list(div(
          class = "metric-box",
          div(class = "metric-label", cat_name,
              tags$span(style = sprintf("font-size: 0.6rem; color: %s;", COLORS$accent_purple), " [カテゴリ]")),
          div(class = "metric-value", style = sprintf("color: %s;", COLORS$accent_blue),
              active_level)
        )))
      }
    }

    # [2] Non-dummy variables -- continuous, discrete
    dummy_cols <- unlist(lapply(cm, function(x) x$dummy_cols))
    for (v in names(bo$best_x)) {
      if (v %in% dummy_cols) next  # Skip individual dummies
      val <- bo$best_x[v]
      type_tag <- if (!is.null(vt) && !is.na(vt[v]) && vt[v] == "discrete") {
        tags$span(style = sprintf("font-size: 0.6rem; color: %s;", COLORS$accent_orange), " [離散]")
      } else NULL
      display_val <- if (!is.null(vt) && !is.na(vt[v]) && vt[v] == "discrete") {
        as.character(as.integer(val))
      } else {
        smart_format(val)
      }
      var_boxes <- c(var_boxes, list(div(
        class = "metric-box",
        div(class = "metric-label", v, type_tag),
        div(class = "metric-value", style = sprintf("color: %s;", COLORS$accent_blue),
            display_val)
      )))
    }

    # Info boxes
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

    tagList(
      div(class = "metric-grid",
          y_box,
          var_boxes,
          info_boxes
      )
    )
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

    # Add target line if in target mode
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

    # Remove initial row (EI = NA)
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
    # Format type column in Japanese (base R for dplyr version compatibility)
    type_labels <- c("initial" = "初期最良", "improved" = "改善", "explored" = "探索")
    matched <- type_labels[df$type]
    df$type <- ifelse(is.na(matched), df$type, matched)

    # Reconstruct categorical variables from dummy columns
    cm <- bo$categorical_mapping
    if (!is.null(cm)) {
      for (cat_name in names(cm)) {
        info <- cm[[cat_name]]
        dummies <- info$dummy_cols
        existing <- intersect(dummies, names(df))
        if (length(existing) == 0) next
        # Determine active level per row
        cat_col <- rep(info$reference, nrow(df))
        for (dc in existing) {
          lvl <- sub(paste0("^", cat_name, "_"), "", dc)
          active_rows <- !is.na(df[[dc]]) & df[[dc]] == 1
          cat_col[active_rows] <- lvl
        }
        # Remove dummy columns, insert categorical column
        first_pos <- min(which(names(df) %in% existing))
        df[existing] <- NULL
        # Insert at the position where first dummy was
        if (first_pos > ncol(df)) {
          df[[cat_name]] <- cat_col
        } else {
          before <- df[, seq_len(first_pos - 1), drop = FALSE]
          after <- df[, seq(first_pos, ncol(df)), drop = FALSE]
          df <- cbind(before, setNames(data.frame(cat_col, stringsAsFactors = FALSE), cat_name), after)
        }
      }
    }

    # Round numeric columns
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    for (col in num_cols) {
      df[[col]] <- round(df[[col]], 6)
    }

    # Rename columns for display
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

  # Dynamic input UI for prediction
  output$pred_input_ui <- renderUI({
    req(rv$analysis)
    res <- rv$analysis
    var_names <- res$var_names
    cm <- rv$categorical_mapping
    dummy_cols <- unlist(lapply(cm, function(x) x$dummy_cols))

    inputs <- list()

    # Categorical variable selectors
    if (!is.null(cm)) {
      for (cat_name in names(cm)) {
        info <- cm[[cat_name]]
        all_levels <- info$levels
        inputs <- c(inputs, list(
          div(style = "display: inline-block; width: 140px; margin-right: 8px; vertical-align: top;",
              selectInput(
                paste0("pred_", cat_name), cat_name,
                choices = all_levels, selected = all_levels[1],
                width = "100%"
              ))
        ))
      }
    }

    # Continuous/discrete variable inputs
    for (v in var_names) {
      if (v %in% dummy_cols) next
      x_col <- res$X[, v]
      x_mean <- mean(x_col, na.rm = TRUE)
      x_step <- diff(range(x_col, na.rm = TRUE)) / 20
      if (x_step == 0) x_step <- 1
      inputs <- c(inputs, list(
        div(style = "display: inline-block; width: 140px; margin-right: 8px; vertical-align: top;",
            numericInput(
              paste0("pred_", v), v,
              value = round(x_mean, 4), step = round(x_step, 4),
              width = "100%"
            ))
      ))
    }

    div(style = "display: flex; flex-wrap: wrap; gap: 4px; padding: 0.5rem 0;", inputs)
  })

  # Fill with BO optimal values
  observeEvent(input$pred_fill_bo, {
    req(rv$analysis, rv_bo$result)
    bo <- rv_bo$result
    cm <- rv$categorical_mapping

    # Set categorical selectors
    if (!is.null(cm)) {
      for (cat_name in names(cm)) {
        info <- cm[[cat_name]]
        active_level <- info$reference
        for (dc in info$dummy_cols) {
          if (!is.na(bo$best_x[dc]) && bo$best_x[dc] == 1) {
            active_level <- sub(paste0("^", cat_name, "_"), "", dc)
            break
          }
        }
        updateSelectInput(session, paste0("pred_", cat_name), selected = active_level)
      }
    }

    # Set numeric inputs
    dummy_cols <- unlist(lapply(cm, function(x) x$dummy_cols))
    for (v in names(bo$best_x)) {
      if (v %in% dummy_cols) next
      updateNumericInput(session, paste0("pred_", v), value = round(bo$best_x[v], 4))
    }
  })

  # Fill with mean values
  observeEvent(input$pred_fill_mean, {
    req(rv$analysis)
    res <- rv$analysis
    cm <- rv$categorical_mapping

    if (!is.null(cm)) {
      for (cat_name in names(cm)) {
        updateSelectInput(session, paste0("pred_", cat_name), selected = cm[[cat_name]]$levels[1])
      }
    }

    dummy_cols <- unlist(lapply(cm, function(x) x$dummy_cols))
    for (v in res$var_names) {
      if (v %in% dummy_cols) next
      updateNumericInput(session, paste0("pred_", v),
                         value = round(mean(res$X[, v], na.rm = TRUE), 4))
    }
  })

  # Reactive prediction result
  output$pred_result_display <- renderUI({
    req(rv$analysis)
    res <- rv$analysis
    var_names <- res$var_names
    cm <- rv$categorical_mapping
    dummy_cols <- unlist(lapply(cm, function(x) x$dummy_cols))

    # Build new data row from inputs
    new_row <- list()
    for (v in var_names) {
      if (v %in% dummy_cols) next
      val <- input[[paste0("pred_", v)]]
      if (is.null(val) || is.na(val)) return(NULL)
      new_row[[v]] <- val
    }

    # Reconstruct dummy columns from categorical selectors
    if (!is.null(cm)) {
      for (cat_name in names(cm)) {
        info <- cm[[cat_name]]
        selected_level <- input[[paste0("pred_", cat_name)]]
        if (is.null(selected_level)) return(NULL)
        for (dc in info$dummy_cols) {
          lvl <- sub(paste0("^", cat_name, "_"), "", dc)
          new_row[[dc]] <- if (lvl == selected_level) 1 else 0
        }
      }
    }

    # Reorder to match var_names
    newdata <- as.data.frame(new_row[var_names], stringsAsFactors = FALSE, check.names = FALSE)

    # Predict
    pred <- tryCatch({
      rsm_predict_with_se(res$fit, newdata, res$mx, var_names)
    }, error = function(e) NULL)

    if (is.null(pred)) {
      return(p(style = sprintf("color: %s;", COLORS$text_muted), "予測計算中..."))
    }

    y_hat <- pred$mean[1]
    se <- pred$se[1]
    res_se <- pred$residual_se
    # 95% prediction interval: y_hat +/- t * sqrt(se^2 + residual_se^2)
    n <- nrow(res$X)
    p_terms <- length(coef(res$fit))
    df_resid <- n - p_terms
    t_val <- if (df_resid > 0) qt(0.975, df_resid) else 1.96
    pi_half <- t_val * sqrt(se^2 + res_se^2)

    # Display
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
            div(style = sprintf("font-size: 1rem; color: %s;", COLORS$text_primary),
                sprintf("%s ~ %s", smart_format(y_hat - pi_half), smart_format(y_hat + pi_half)))
          ),
          div(
            span(style = sprintf("color: %s; font-size: 0.75rem;", COLORS$text_muted), "SE"),
            div(style = sprintf("font-size: 0.9rem; color: %s;", COLORS$text_secondary),
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
      if (is.null(bo)) return()

      cm <- bo$categorical_mapping

      # Build optimal solution row with reverse-mapped categories
      best_row <- list()
      dummy_cols <- unlist(lapply(cm, function(x) x$dummy_cols))

      # Categorical variables first
      if (!is.null(cm)) {
        for (cat_name in names(cm)) {
          info <- cm[[cat_name]]
          active_level <- info$reference
          for (dc in info$dummy_cols) {
            if (!is.na(bo$best_x[dc]) && bo$best_x[dc] == 1) {
              active_level <- sub(paste0("^", cat_name, "_"), "", dc)
              break
            }
          }
          best_row[[cat_name]] <- active_level
        }
      }

      # Continuous/discrete variables
      for (v in names(bo$best_x)) {
        if (v %in% dummy_cols) next
        best_row[[v]] <- bo$best_x[v]
      }

      best_row[["predicted_Y"]] <- bo$best_y

      # Direction info
      if (!is.null(bo$target_value)) {
        best_row[["optimization"]] <- paste0("target=", bo$target_value)
        best_row[["deviation"]] <- abs(bo$best_y - bo$target_value)
      } else {
        best_row[["optimization"]] <- if (bo$minimize) "minimize" else "maximize"
      }

      df <- as.data.frame(best_row, stringsAsFactors = FALSE, check.names = FALSE)
      write.csv(df, file, row.names = FALSE)
    }
  )
}

# ============================================================================
# RUN APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)
