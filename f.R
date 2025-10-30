








# ---------------------------------------------------------------------
# 7. Export section: Save everything neatly
# ---------------------------------------------------------------------
if (!dir.exists(output_dir)) dir.create(output_dir)


# (b) Plots
ggsave(file.path(output_dir, "relMSE_faceted_meanSD.png"),
       plot = p_facet_meanSD, width = 10, height = 6, dpi = 300)

ggsave(file.path(output_dir, "relMSE_faceted_tolerance.png"),
       plot = p_facet_prop, width = 10, height = 6, dpi = 300)

ggsave(file.path(output_dir, "relMSE_overlay.png"),
       plot = p_overlay, width = 9, height = 6, dpi = 300)

message("✅ All summary tables and plots saved to: ", normalizePath(output_dir))
