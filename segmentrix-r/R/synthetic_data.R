# =====================================================================
# R/synthetic_data.R
# Generates a fake (but realistic) transaction log so the app works
# out-of-the-box. Output columns match what the real CSV must contain:
#   CustomerID, InvoiceDate, TransactionID, OrderValue, ProductCategory
# =====================================================================

generate_synthetic_transactions <- function(n_customers = 1500,
                                            end_date    = Sys.Date(),
                                            seed        = 42) {
  set.seed(seed)  # same seed -> same data every time (reproducible)

  # Each "archetype" is a type of shopper. Day ranges mean "days before end_date".
  archetypes <- data.frame(
    archetype   = c("champion", "loyal_low_spend", "big_spender_lapsing",
                    "occasional", "hibernating", "new"),
    share       = c(0.10, 0.20, 0.08, 0.27, 0.25, 0.10),  # must sum to 1
    first_min   = c(300, 250, 300, 120, 300,  5),   # earliest/latest FIRST purchase
    first_max   = c(540, 540, 540, 540, 540, 60),
    last_min    = c(  1,   1,  90,  30, 220,  1),   # earliest/latest LAST purchase
    last_max    = c( 25,  40, 200, 160, 500, 30),
    mean_orders = c( 18,  14,   9,   3,   2,  2),   # average number of orders
    meanlog     = c(5.0, 3.4, 5.2, 4.0, 3.6, 4.2),  # order value ~ log-normal
    sdlog       = c(0.35, 0.30, 0.40, 0.45, 0.45, 0.40),
    stringsAsFactors = FALSE
  )

  categories <- c("Electronics", "Home & Garden", "Fashion", "Beauty", "Sports", "Books")

  # Assign every customer an archetype according to the shares above
  archetype_idx <- sample(seq_len(nrow(archetypes)), n_customers,
                          replace = TRUE, prob = archetypes$share)

  # Build the order history of ONE customer (returns a small data.table)
  one_customer <- function(i) {
    a         <- archetypes[archetype_idx[i], ]
    first_ago <- runif(1, a$first_min, a$first_max)
    last_ago  <- min(runif(1, a$last_min, a$last_max), first_ago)
    n_orders  <- 1 + rpois(1, a$mean_orders - 1)

    days_ago <- if (n_orders == 1) {
      last_ago
    } else {
      c(first_ago, last_ago, runif(n_orders - 2, last_ago, first_ago))
    }

    favourite <- sample(categories, 1)  # each customer has a favourite category
    data.table::data.table(
      CustomerID      = sprintf("C%05d", i),
      InvoiceDate     = end_date - round(days_ago),
      OrderValue      = round(rlnorm(n_orders, a$meanlog, a$sdlog), 2),
      ProductCategory = ifelse(runif(n_orders) < 0.6, favourite,
                               sample(categories, n_orders, replace = TRUE))
    )
  }

  tx <- data.table::rbindlist(lapply(seq_len(n_customers), one_customer))
  tx[, TransactionID := sprintf("T%07d", .I)]

  # Sprinkle in some real-world mess so the cleaning step has work to do
  n_rows  <- nrow(tx)
  refunds <- sample(n_rows, ceiling(0.003 * n_rows))
  tx[refunds, OrderValue := -OrderValue]            # negative = refund
  dups    <- tx[sample(n_rows, 10)]                 # exact duplicate rows
  tx      <- rbind(tx, dups)
  tx[sample(nrow(tx), 5), CustomerID := NA_character_]  # missing customer IDs

  tx[sample(nrow(tx))]  # shuffle row order
}
