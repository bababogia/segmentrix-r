# =====================================================================
# R/migration.R  -  Cohort migration / lifecycle vectors
#
# Idea: take two "snapshots" of every customer, `lookback_days` apart:
#   NOW      : RFM using all history up to the analysis date
#   EARLIER  : RFM using all history up to (analysis date - lookback_days)
# Both snapshots are assigned to the SAME clusters (same scaling, nearest
# centre), so "Champions" means the same thing in both. The pair
# (earlier segment -> current segment) is the customer's Migration Vector.
# =====================================================================

NEW_LABEL <- "New (no prior history)"

build_migration <- function(transactions, model, analysis_date, lookback_days = 90) {
  analysis_date <- as.Date(analysis_date)
  prev_date     <- analysis_date - lookback_days

  # If the lookback reaches before the first transaction, nobody existed yet.
  has_history <- any(transactions$InvoiceDate < prev_date)
  rfm_prev <- if (has_history) compute_rfm(transactions, analysis_date = prev_date) else NULL

  prev_tbl <- if (!is.null(rfm_prev) && nrow(rfm_prev) > 0) {
    data.frame(CustomerID   = as.character(rfm_prev$CustomerID),
               Prev_Segment = predict_segments(model, rfm_prev),
               stringsAsFactors = FALSE)
  } else {
    data.frame(CustomerID = character(0), Prev_Segment = character(0),
               stringsAsFactors = FALSE)
  }

  rank_lookup <- stats::setNames(model$profile$value_rank, model$profile$Segment)

  model$data |>
    dplyr::select(CustomerID, Segment, SegmentType, Monetary) |>
    dplyr::left_join(prev_tbl, by = "CustomerID") |>
    dplyr::mutate(
      Segment      = as.character(Segment),
      Prev_Segment = ifelse(is.na(Prev_Segment), NEW_LABEL, Prev_Segment),
      prev_rank    = unname(rank_lookup[Prev_Segment]),   # NA for new customers
      curr_rank    = unname(rank_lookup[Segment]),        # 1 = best segment
      Movement = dplyr::case_when(
        Prev_Segment == NEW_LABEL ~ "New",
        curr_rank <  prev_rank    ~ "Upgraded",
        curr_rank >  prev_rank    ~ "Downgraded",
        TRUE                      ~ "Stable"
      ),
      Vector = paste(Prev_Segment, "\u2192", Segment)
    ) |>
    dplyr::select(-prev_rank, -curr_rank)
}

# Counts table: rows = where customers WERE, columns = where they ARE now
migration_matrix <- function(migration, segment_levels) {
  table(
    factor(migration$Prev_Segment, levels = c(segment_levels, NEW_LABEL)),
    factor(migration$Segment,      levels = segment_levels)
  )
}
