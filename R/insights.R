# =====================================================================
# R/insights.R  -  The automated "insight engine"
#   compute_churn_risk()          churn probability per customer
#   build_segment_summary()       KPIs per segment
#   get_playbook()                recommended marketing action per segment type
#   generate_segment_narrative()  plain-English summary of one segment
#   generate_migration_insights() alerts about customers changing segments
#
# This engine is rule-based (templates + business rules): deterministic,
# free, and easy to explain. You can later swap the text-building step for
# an LLM call without changing the rest of the app.
# =====================================================================

# --- Churn risk -----------------------------------------------------------
# A transparent heuristic: compare how long a customer has been silent
# (Recency) with how often THEY normally buy (AvgGapDays).
#   ratio = Recency / typical gap between orders
# ratio 1 = "just on schedule", 2 = twice overdue (50% risk), 4 = very likely gone.
# A logistic curve turns the ratio into a 0-1 probability.
# Customers with one order have no personal gap: we use the median of others.
# (A production system would train a model on historic churn labels.)
compute_churn_risk <- function(df, steepness = 1.5, midpoint = 2) {
  typical_gap <- stats::median(df$AvgGapDays, na.rm = TRUE)
  if (is.na(typical_gap)) typical_gap <- 30
  gap   <- ifelse(is.na(df$AvgGapDays), typical_gap, df$AvgGapDays)
  gap   <- pmax(gap, 7)                      # avoid tiny gaps exploding the ratio
  ratio <- df$Recency / gap
  df$ChurnRisk   <- stats::plogis(steepness * (ratio - midpoint))
  df$ValueAtRisk <- df$Monetary * df$ChurnRisk   # lifetime spend x churn probability
  df
}

# --- Segment summary ------------------------------------------------------
build_segment_summary <- function(df) {
  total_rev <- sum(df$Monetary)
  n_total   <- nrow(df)

  df |>
    dplyr::group_by(Segment, SegmentType) |>
    dplyr::summarise(
      Customers     = dplyr::n(),
      Revenue       = sum(Monetary),
      AvgSpend      = mean(Monetary),
      AvgOrderValue = sum(Monetary) / sum(Frequency),
      AvgFrequency  = mean(Frequency),
      AvgRecency    = mean(Recency),
      ChurnRisk     = mean(ChurnRisk),
      ValueAtRisk   = sum(ValueAtRisk),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      CustomerShare = Customers / n_total,
      RevenueShare  = Revenue / total_rev,
      Action        = vapply(SegmentType, function(t) get_playbook(t)$action, character(1))
    ) |>
    dplyr::arrange(Segment)   # Segment is an ordered factor: best segment first
}

# --- Playbook -------------------------------------------------------------
SEGMENT_PLAYBOOK <- list(
  "Champions" = list(
    action = "Offer VIP loyalty perk",
    detail = "Early access to launches, tiered rewards and referral bonuses. Avoid blanket discounts: these customers already buy at full price.",
    kpi    = "Retention rate, referrals, share of revenue"),
  "Loyal Regulars" = list(
    action = "Grow share of wallet with cross-sell",
    detail = "Recommend complementary categories and invite them into the loyalty programme to move them towards Champions.",
    kpi    = "Items per order, upgrade rate to Champions"),
  "At-Risk High-Spenders" = list(
    action = "Launch personalised win-back outreach",
    detail = "Highest ROI per customer rescued: personal email or call, a time-limited incentive and a 'we miss you' message built around their favourite category.",
    kpi    = "Reactivation rate, recovered revenue"),
  "Low-Value Frequent" = list(
    action = "Raise basket size with bundles and thresholds",
    detail = "Free-shipping thresholds, multi-buy bundles and premium add-ons to lift average order value without hurting frequency.",
    kpi    = "Average order value, margin per order"),
  "New & Promising" = list(
    action = "Run an onboarding journey for the 2nd and 3rd purchase",
    detail = "Welcome series plus a small second-purchase incentive within 30 days. The second order is the strongest predictor of long-term loyalty.",
    kpi    = "Second-purchase rate within 30 days"),
  "Occasional Shoppers" = list(
    action = "Nurture with seasonal and category reminders",
    detail = "Low-cost, content-led campaigns (new arrivals, seasonal reminders). Test one incentive to see whether frequency responds.",
    kpi    = "Purchase frequency, email click-through"),
  "Hibernating" = list(
    action = "Run a re-engagement discount sequence",
    detail = "A 3-step email/SMS sequence over about 30 days with escalating incentive. Suppress customers who never respond to protect sender reputation and budget.",
    kpi    = "Reactivation rate, cost per reactivation")
)

get_playbook <- function(segment_type) {
  pb <- SEGMENT_PLAYBOOK[[as.character(segment_type)]]
  if (is.null(pb)) {
    pb <- list(action = "Monitor and A/B test",
               detail = "No specific playbook for this segment yet. Test a small campaign and measure the response.",
               kpi    = "Conversion rate")
  }
  pb
}

# --- Narratives ------------------------------------------------------------
risk_label <- function(p) {
  as.character(cut(p, breaks = c(-Inf, 0.25, 0.55, Inf), labels = c("low", "moderate", "high")))
}

# `row` = one row of build_segment_summary()
generate_segment_narrative <- function(row) {
  pb <- get_playbook(row$SegmentType)
  sprintf(
    paste0("%s customers (%s of the base) make up this segment and generate %s, ",
           "or %s of total revenue. On average each has spent %s over their lifetime ",
           "(average order value %s), orders %.1f times and last bought %.0f days ago. ",
           "Estimated churn risk is %s (%s). Recommended action: %s."),
    scales::comma(row$Customers),
    scales::percent(row$CustomerShare, accuracy = 0.1),
    scales::dollar(row$Revenue, accuracy = 1),
    scales::percent(row$RevenueShare, accuracy = 0.1),
    scales::dollar(row$AvgSpend, accuracy = 1),
    scales::dollar(row$AvgOrderValue, accuracy = 1),
    row$AvgFrequency,
    row$AvgRecency,
    scales::percent(row$ChurnRisk, accuracy = 1),
    risk_label(row$ChurnRisk),
    tolower(substr(pb$action, 1, 1)) |> paste0(substr(pb$action, 2, nchar(pb$action)))
  )
}

add_narratives <- function(summary_tbl) {
  summary_tbl$Narrative <- vapply(
    seq_len(nrow(summary_tbl)),
    function(i) generate_segment_narrative(summary_tbl[i, ]),
    character(1)
  )
  summary_tbl
}

add_recommended_action <- function(customers) {
  customers$Action <- vapply(customers$SegmentType,
                             function(t) get_playbook(t)$action, character(1))
  customers
}

# --- Migration alerts ------------------------------------------------------
generate_migration_insights <- function(migration, lookback_days) {
  n  <- nrow(migration)
  mv <- table(factor(migration$Movement,
                     levels = c("Upgraded", "Stable", "Downgraded", "New")))
  pct <- function(x) scales::percent(x / n, accuracy = 0.1)

  overview <- sprintf(
    "Over the last %d days, %s customers (%s) moved to a more valuable segment, %s (%s) slipped to a less valuable one, %s stayed in the same segment and %s are new.",
    as.integer(lookback_days),
    scales::comma(mv[["Upgraded"]]),   pct(mv[["Upgraded"]]),
    scales::comma(mv[["Downgraded"]]), pct(mv[["Downgraded"]]),
    scales::comma(mv[["Stable"]]), scales::comma(mv[["New"]])
  )

  flows <- migration |>
    dplyr::filter(Movement %in% c("Downgraded", "Upgraded")) |>
    dplyr::group_by(Movement, Prev_Segment, Segment, SegmentType) |>
    dplyr::summarise(Customers = dplyr::n(), Spend = sum(Monetary), .groups = "drop")

  risks <- flows |> dplyr::filter(Movement == "Downgraded") |>
    dplyr::arrange(dplyr::desc(Spend)) |> utils::head(4)
  wins  <- flows |> dplyr::filter(Movement == "Upgraded") |>
    dplyr::arrange(dplyr::desc(Spend)) |> utils::head(2)

  risk_txt <- vapply(seq_len(nrow(risks)), function(i) {
    sprintf("%s customers moved %s \u2192 %s, carrying %s of lifetime spend. Suggested response: %s.",
            scales::comma(risks$Customers[i]), risks$Prev_Segment[i], risks$Segment[i],
            scales::dollar(risks$Spend[i], accuracy = 1),
            tolower(get_playbook(risks$SegmentType[i])$action))
  }, character(1))

  win_txt <- vapply(seq_len(nrow(wins)), function(i) {
    sprintf("%s customers moved %s \u2192 %s (%s lifetime spend). Reinforce with a thank-you or reward to lock the gain in.",
            scales::comma(wins$Customers[i]), wins$Prev_Segment[i], wins$Segment[i],
            scales::dollar(wins$Spend[i], accuracy = 1))
  }, character(1))

  list(overview = overview, risks = risk_txt, wins = win_txt)
}
