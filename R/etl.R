# =====================================================================
# R/etl.R  -  Extract / Transform / Load
#   load_transactions()   read a CSV and check the columns
#   clean_transactions()  fix types, drop bad rows, report what was dropped
#   compute_rfm()         one row per customer: Recency, Frequency, Monetary...
#   add_rfm_scores()      1-5 quintile scores (used for the heatmap)
#
# NOTE: files in R/ are sourced by Shiny BEFORE app.R runs, so we write
# package::function() everywhere to be independent of library() order.
# =====================================================================

REQUIRED_COLS <- c("CustomerID", "InvoiceDate", "TransactionID", "OrderValue")
# ProductCategory is used if present, otherwise filled with "Uncategorised".

validate_columns <- function(dt) {
  missing_cols <- setdiff(REQUIRED_COLS, names(dt))
  if (length(missing_cols) > 0) {
    stop("Your file is missing required column(s): ",
         paste(missing_cols, collapse = ", "),
         ". Expected: CustomerID, InvoiceDate, TransactionID, OrderValue, ProductCategory.",
         call. = FALSE)
  }
  invisible(TRUE)
}

load_transactions <- function(path) {
  dt <- data.table::fread(path, na.strings = c("", "NA", "NULL"))
  validate_columns(dt)
  dt
}

clean_transactions <- function(dt) {
  dt <- data.table::copy(data.table::as.data.table(dt))  # never modify the caller's data
  validate_columns(dt)
  if (!"ProductCategory" %in% names(dt)) dt[, ProductCategory := "Uncategorised"]

  keep <- c("CustomerID", "InvoiceDate", "TransactionID", "OrderValue", "ProductCategory")
  dt <- dt[, ..keep]
  rows_in <- nrow(dt)

  # --- types ---------------------------------------------------------
  dt[, CustomerID      := trimws(as.character(CustomerID))]
  dt[, TransactionID   := trimws(as.character(TransactionID))]
  dt[, ProductCategory := trimws(as.character(ProductCategory))]
  dt[is.na(ProductCategory) | ProductCategory == "", ProductCategory := "Uncategorised"]
  dt[, OrderValue := suppressWarnings(as.numeric(OrderValue))]

  if (!inherits(dt$InvoiceDate, "Date")) {
    # ISO dates (2025-03-31) are safest; dmy / mdy are tried as fallbacks.
    parsed <- lubridate::parse_date_time(
      as.character(dt$InvoiceDate),
      orders = c("Ymd HMS", "Ymd HM", "Ymd", "dmY HMS", "dmY", "mdY"),
      quiet = TRUE
    )
    dt[, InvoiceDate := as.Date(parsed)]
  }

  # --- drop bad rows --------------------------------------------------
  bad_missing <- is.na(dt$CustomerID) | dt$CustomerID == "" |
    is.na(dt$TransactionID) | is.na(dt$InvoiceDate) | is.na(dt$OrderValue)
  n_missing <- sum(bad_missing)
  dt <- dt[!bad_missing]

  n_nonpos <- sum(dt$OrderValue <= 0)   # refunds / zero-value rows
  dt <- dt[OrderValue > 0]

  before <- nrow(dt)
  dt <- unique(dt)                      # exact duplicate rows
  n_dup <- before - nrow(dt)

  list(
    data   = dt[order(InvoiceDate)],
    report = list(rows_in = rows_in, rows_out = nrow(dt),
                  dropped_missing = n_missing,
                  dropped_nonpositive = n_nonpos,
                  dropped_duplicates = n_dup)
  )
}

# ---------------------------------------------------------------------
# RFM: "as of" analysis_date, using only transactions BEFORE that date.
#   Recency   = days since last purchase
#   Frequency = number of DISTINCT transactions
#   Monetary  = total spend;  AOV = Monetary / Frequency
# AvgGapDays (average days between orders) feeds the churn-risk estimate.
# ---------------------------------------------------------------------
compute_rfm <- function(transactions, analysis_date = NULL) {
  dt <- data.table::as.data.table(transactions)
  if (is.null(analysis_date)) analysis_date <- max(dt$InvoiceDate) + 1
  analysis_date <- as.Date(analysis_date)

  dt <- dt[InvoiceDate < analysis_date]

  rfm <- dt[, .(
    FirstPurchase = min(InvoiceDate),
    LastPurchase  = max(InvoiceDate),
    Frequency     = data.table::uniqueN(TransactionID),
    Monetary      = sum(OrderValue)
  ), by = CustomerID]

  rfm[, Recency    := as.numeric(analysis_date - LastPurchase)]
  rfm[, AOV        := Monetary / Frequency]
  rfm[, AvgGapDays := data.table::fifelse(
    Frequency > 1,
    as.numeric(LastPurchase - FirstPurchase) / (Frequency - 1),
    NA_real_)]

  # Favourite category = category with the highest spend per customer
  cat_spend <- dt[, .(Spend = sum(OrderValue)), by = .(CustomerID, ProductCategory)]
  cat_spend <- cat_spend[order(CustomerID, -Spend)]
  top_cat   <- cat_spend[!duplicated(CustomerID), .(CustomerID, TopCategory = ProductCategory)]

  rfm <- merge(rfm, top_cat, by = "CustomerID", all.x = TRUE)
  data.table::setcolorder(rfm, c("CustomerID", "Recency", "Frequency", "Monetary", "AOV"))
  rfm[]
}

# Quintile score 1-5 based on percentile rank (ties get the same score)
score_quintile <- function(x) {
  pmin(5L, as.integer(floor(dplyr::percent_rank(x) * 5)) + 1L)
}

# For every score, 5 = best. Recency is reversed (fewer days = better).
add_rfm_scores <- function(rfm) {
  dplyr::mutate(
    rfm,
    R_Score = 6L - score_quintile(Recency),
    F_Score = score_quintile(Frequency),
    M_Score = score_quintile(Monetary)
  )
}
