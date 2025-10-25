



# ---------------------------------------------------------------------
# 4. Plot 2: Faceted Proportion within Tolerance
# ---------------------------------------------------------------------
p_facet_prop <- ggplot(relMSE_summary, aes(x = size, y = prop_within_tol)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "darkred") +
  facet_wrap(~ method, scales = "free_y") +
  labs(
    x = "Model size (# variables)",
    y = paste0("Proportion with relMSE ≤ ", tol),
    title = "Reliability vs. Sparsity (Faceted)",
    subtitle = "Fraction of repeats within tolerance threshold"
  ) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold"))

# ---------------------------------------------------------------------
# 5. Plot 3: Combined Overlay for All Methods
# ---------------------------------------------------------------------
p_overlay <- ggplot(relMSE_summary, aes(x = size, color = method, fill = method)) +
  geom_ribbon(aes(ymin = mean_relMSE - sd_relMSE,
                  ymax = mean_relMSE + sd_relMSE),
              alpha = 0.2, color = NA) +
  geom_line(aes(y = mean_relMSE), size = 1.2) +
  geom_point(aes(y = mean_relMSE), size = 2) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "darkred") +
  labs(
    x = "Model size (# variables)",
    y = "Relative MSE (vs full model)",
    title = "Prediction Loss vs. Model Size (Overlay)",
    subtitle = "Comparison across selection methods",
    caption = paste0("Shaded = ±SD; tolerance = ", tol)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16)
  )

# ---------------------------------------------------------------------
# 6. Identify smallest acceptable model per method (mean_relMSE ≤ tol)
# ---------------------------------------------------------------------
best_models <- relMSE_summary %>%
  group_by(method) %>%
  filter(mean_relMSE <= tol) %>%
  slice_min(size, n = 1, with_ties = FALSE) %>%
  select(method, size, mean_relMSE, sd_relMSE, prop_within_tol)

print(best_models)

# ---------------------------------------------------------------------
# 7. Export section: Save everything neatly
# ---------------------------------------------------------------------
if (!dir.exists(output_dir)) dir.create(output_dir)

# (a) Tables
write_csv(relMSE_summary_print, file.path(output_dir, "relMSE_summary.csv"))
write_csv(best_models, file.path(output_dir, "best_models.csv"))

# (b) Plots
ggsave(file.path(output_dir, "relMSE_faceted_meanSD.png"),
       plot = p_facet_meanSD, width = 10, height = 6, dpi = 300)

ggsave(file.path(output_dir, "relMSE_faceted_tolerance.png"),
       plot = p_facet_prop, width = 10, height = 6, dpi = 300)

ggsave(file.path(output_dir, "relMSE_overlay.png"),
       plot = p_overlay, width = 9, height = 6, dpi = 300)

message("✅ All summary tables and plots saved to: ", normalizePath(output_dir))
