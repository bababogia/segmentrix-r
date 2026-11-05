# =====================================================================
# R/clustering.R
#   build_feature_matrix()  log1p + standardise Recency/Frequency/Monetary
#   evaluate_k()            silhouette + within-cluster SS for K = 3..8
#   select_k()              pick K by "silhouette" or "elbow"
#   fit_segments()          the main function: K-means + names + PCA
#   predict_segments()      assign NEW customers to existing clusters
# =====================================================================

# --- 1. Features -------------------------------------------------------
# log1p() = log(1 + x) tames the long right tail of spend/frequency.
# Standardising (mean 0, sd 1) stops one metric from dominating K-means.
# We return the centre/scale so the SAME transformation can be re-applied
# to another time window (needed for migration analysis).
build_feature_matrix <- function(rfm, scaling = NULL) {
  raw <- cbind(
    Recency   = log1p(rfm$Recency),
    Frequency = log1p(rfm$Frequency),
    Monetary  = log1p(rfm$Monetary)
  )
  if (is.null(scaling)) {
    sds <- apply(raw, 2, stats::sd)
    sds[is.na(sds) | sds == 0] <- 1
    scaling <- list(center = colMeans(raw), scale = sds)
  }
  scaled <- sweep(sweep(raw, 2, scaling$center, "-"), 2, scaling$scale, "/")
  list(x = scaled, scaling = scaling)
}

# --- 2. Choosing K -----------------------------------------------------
# Silhouette is computed on a random sample (max 3000 rows) because the
# distance matrix grows with n^2.
evaluate_k <- function(x, k_range = 3:8, nstart = 25, sil_sample = 3000, seed = 123) {
  set.seed(seed)
  k_range <- k_range[k_range < nrow(x)]
  idx <- if (nrow(x) > sil_sample) sample.int(nrow(x), sil_sample) else seq_len(nrow(x))
  d   <- stats::dist(x[idx, , drop = FALSE])

  res <- lapply(k_range, function(k) {
    km  <- stats::kmeans(x, centers = k, nstart = nstart, iter.max = 50)
    sil <- cluster::silhouette(km$cluster[idx], d)
    data.frame(k = k,
               tot_withinss   = km$tot.withinss,
               avg_silhouette = mean(sil[, "sil_width"]))
  })
  do.call(rbind, res)
}

# Elbow = the point farthest from the straight line joining first and last point
find_elbow_k <- function(k, wss) {
  if (length(k) < 3) return(k[1])
  x  <- (k - min(k)) / (max(k) - min(k))
  y  <- (wss - min(wss)) / (max(wss) - min(wss))
  x1 <- x[1]; y1 <- y[1]; x2 <- x[length(x)]; y2 <- y[length(y)]
  d  <- abs((y2 - y1) * x - (x2 - x1) * y + x2 * y1 - y2 * x1) /
    sqrt((y2 - y1)^2 + (x2 - x1)^2)
  k[which.max(d)]
}

select_k <- function(k_eval, method = c("silhouette", "elbow")) {
  method <- match.arg(method)
  if (method == "silhouette") {
    k_eval$k[which.max(k_eval$avg_silhouette)]
  } else {
    find_elbow_k(k_eval$k, k_eval$tot_withinss)
  }
}

# --- 3. Naming clusters --------------------------------------------------
# `centers` are the cluster centres in standardised space, so a value of
# 0 = average customer, +1 = one standard deviation above average.
# Remember: LOW recency is GOOD (bought recently).
name_segments <- function(centers) {
  z <- as.data.frame(centers)
  z$cluster     <- seq_len(nrow(z))
  z$value_score <- -z$Recency + z$Frequency + z$Monetary   # higher = better customer

  # At most ONE cluster is crowned "Champions": the best active, frequent, high-spend one.
  cand  <- which(z$Recency < 0 & z$Frequency > 0 & z$Monetary > 0)
  champ <- if (length(cand) > 0) cand[which.max(z$value_score[cand])] else NA_integer_

  z$SegmentType <- vapply(seq_len(nrow(z)), function(i) {
    if (!is.na(champ) && i == champ) return("Champions")
    r <- z$Recency[i]; f <- z$Frequency[i]; m <- z$Monetary[i]
    if (r > 0.25) {                                 # lapsed
      if (m > 0) "At-Risk High-Spenders" else "Hibernating"
    } else if (f > 0) {                             # active + frequent
      if (m > 0) "Loyal Regulars" else "Low-Value Frequent"
    } else if (r < -0.25) {                         # very recent but not frequent
      "New & Promising"
    } else {
      "Occasional Shoppers"
    }
  }, character(1))

  # If two clusters get the same label, add A / B ... (best first)
  z$Segment <- z$SegmentType
  for (lbl in names(which(table(z$SegmentType) > 1))) {
    idx <- which(z$SegmentType == lbl)
    idx <- idx[order(-z$value_score[idx])]
    z$Segment[idx] <- paste(lbl, LETTERS[seq_along(idx)])
  }
  z$value_rank <- rank(-z$value_score, ties.method = "first")  # 1 = best
  z
}

# --- 4. Main entry point -----------------------------------------------
fit_segments <- function(rfm, k = NULL, k_method = c("silhouette", "elbow"),
                         k_range = 3:8, nstart = 25, seed = 123) {
  k_method <- match.arg(k_method)

  feats <- build_feature_matrix(rfm)
  x     <- feats$x

  k_eval   <- evaluate_k(x, k_range, nstart, seed = seed)
  chosen_k <- if (is.null(k)) select_k(k_eval, k_method) else as.integer(k)

  set.seed(seed)
  km      <- stats::kmeans(x, centers = chosen_k, nstart = 50, iter.max = 100)
  profile <- name_segments(km$centers)

  out <- tibble::as_tibble(rfm)
  out$Cluster     <- km$cluster
  out$SegmentType <- profile$SegmentType[km$cluster]
  out$Segment     <- factor(profile$Segment[km$cluster],
                            levels = profile$Segment[order(profile$value_rank)])

  # PCA: squeezes 3 correlated features into axes we can plot
  pca    <- stats::prcomp(x)
  scores <- as.data.frame(pca$x)
  out    <- dplyr::bind_cols(out, tibble::as_tibble(scores))

  # Quality check of the FINAL clustering (sampled for speed)
  set.seed(seed)
  idx    <- if (nrow(x) > 3000) sample.int(nrow(x), 3000) else seq_len(nrow(x))
  sil    <- cluster::silhouette(km$cluster[idx], stats::dist(x[idx, , drop = FALSE]))
  widths <- sil[, "sil_width"]
  by_cl  <- tapply(widths, km$cluster[idx], mean)
  sil_by_segment <- data.frame(
    Segment       = profile$Segment[as.integer(names(by_cl))],
    AvgSilhouette = as.numeric(by_cl),
    stringsAsFactors = FALSE
  )

  list(
    data = out, km = km, k = chosen_k, k_method = k_method,
    k_eval = k_eval,
    k_silhouette = select_k(k_eval, "silhouette"),
    k_elbow      = select_k(k_eval, "elbow"),
    scaling = feats$scaling, profile = profile, pca = pca,
    silhouette_by_segment = sil_by_segment,
    overall_silhouette    = mean(widths)
  )
}

# --- 5. Score customers from ANOTHER time window ------------------------
# Re-uses the original scaling and assigns each customer to the NEAREST
# cluster centre, so segments mean the same thing across time.
predict_segments <- function(model, rfm_new) {
  if (nrow(rfm_new) == 0) return(character(0))
  x       <- build_feature_matrix(rfm_new, model$scaling)$x
  centers <- model$km$centers
  d2 <- vapply(seq_len(nrow(centers)),
               function(j) rowSums(sweep(x, 2, centers[j, ], "-")^2),
               numeric(nrow(x)))
  if (!is.matrix(d2)) d2 <- matrix(d2, nrow = nrow(x))
  cl <- max.col(-d2, ties.method = "first")
  as.character(model$profile$Segment[cl])
}
