# =====================================================================
# export_figures.R
# Saves the main charts as PNG files in docs/figures/ so you can
# show them in the README and on GitHub.
#
# Run from the project folder:  source("export_figures.R")
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(cluster)
  library(factoextra)
})

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

out_dir <- file.path("docs", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Run the same pipeline the app uses ----------------------------
tx      <- generate_synthetic_transactions(n_customers = 1500, seed = 42)
cleaned <- clean_transactions(tx)
ad      <- max(cleaned$data$InvoiceDate) + 1
rfm     <- compute_rfm(cleaned$data, ad)
model   <- fit_segments(rfm)

model$data <- model$data |>
  add_rfm_scores() |>
  compute_churn_risk() |>
  add_recommended_action()

migration <- build_migration(cleaned$data, model, ad, lookback_days = 90)

seg_levels <- levels(model$data$Segment)
base_cols  <- c("#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd",
                "#8c564b", "#17becf", "#7f7f7f", "#bcbd22")
pal        <- stats::setNames(base_cols[seq_along(seg_levels)], seg_levels)

save_plot <- function(p, name, w = 9, h = 5.5) {
  path <- file.path(out_dir, paste0(name, ".png"))
  ggsave(path, p, width = w, height = h, dpi = 150, bg = "white")
  message("Saved ", path)
}

# ---- 1. Spend distribution -------------------------------------------
p_spend <- ggplot(model$data, aes(x = Monetary, fill = Segment)) +
  geom_histogram(bins = 40, colour = NA) +
  scale_x_log10(labels = scales::dollar) +
  scale_fill_manual(values = pal) +
  labs(title = "Lifetime spend distribution by segment",
       x = "Lifetime spend (log scale)", y = "Customers", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
save_plot(p_spend, "01_spend_distribution")

# ---- 2. RFM heatmap --------------------------------------------------
heat <- model$data |>
  group_by(R_Score, F_Score) |>
  summarise(AvgSpend = mean(Monetary), .groups = "drop")

p_heat <- ggplot(heat, aes(x = F_Score, y = R_Score, fill = AvgSpend)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = scales::dollar(AvgSpend, accuracy = 1)), size = 3) +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(breaks = 1:5) +
  labs(title = "RFM heatmap: average spend by score",
       x = "Frequency score (5 = most frequent)",
       y = "Recency score (5 = most recent)", fill = "Avg spend") +
  theme_minimal(base_size = 12)
save_plot(p_heat, "02_rfm_heatmap", w = 8, h = 6)

# ---- 3. PCA cluster map ----------------------------------------------
ve <- round(100 * model$pca$sdev^2 / sum(model$pca$sdev^2), 1)
p_pca <- ggplot(model$data, aes(PC1, PC2, colour = Segment)) +
  geom_point(alpha = 0.6, size = 1.6) +
  scale_colour_manual(values = pal) +
  labs(title = "Customer segments in PCA space",
       x = sprintf("PC1 (%.1f%% of variance)", ve[1]),
       y = sprintf("PC2 (%.1f%% of variance)", ve[2]), colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
save_plot(p_pca, "03_pca_clusters")

# ---- 4. Choosing K ---------------------------------------------------
ke <- model$k_eval
p_k <- ggplot(ke, aes(k, avg_silhouette)) +
  geom_line() + geom_point(size = 2) +
  geom_point(data = filter(ke, k == model$k), colour = "red", size = 4) +
  scale_x_continuous(breaks = ke$k) +
  labs(title = sprintf("Silhouette score by K (selected K = %d)", model$k),
       x = "Number of clusters (K)", y = "Average silhouette") +
  theme_minimal(base_size = 12)
save_plot(p_k, "04_choosing_k", w = 7, h = 4.5)

# ---- 5. Migration matrix ---------------------------------------------
mat <- as.data.frame(migration_matrix(migration, seg_levels))
names(mat) <- c("From", "To", "Customers")
p_mig <- ggplot(mat, aes(To, From, fill = Customers)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = Customers), size = 3) +
  scale_fill_gradient(low = "white", high = "#2171b5") +
  scale_y_discrete(limits = rev) +
  labs(title = "Cohort migration: segment 90 days ago (rows) to now (columns)",
       x = "Segment now", y = "Segment 90 days ago", fill = "Customers") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_plot(p_mig, "05_migration_matrix", w = 10, h = 6.5)

message("\nAll figures saved to ", normalizePath(out_dir))
