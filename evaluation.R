library(dplyr)
library(readODS)

# ---------------------------------------------------------------------
# 1. Combine all methods into one tidy long data frame
# ---------------------------------------------------------------------

eval.lsDF2long <- function(lsDF) {
  df_list <- lapply(seq_along(lsDF), function(i) {
    df <- as.data.frame(lsDF[[i]])
    n <- nrow(df)
    m <- ncol(df)
    
    # Create long format manually
    data.frame(
      rep = rep(seq_len(n), times = m),
      size = rep(seq_len(m), each = n),
      value = as.vector(as.matrix(df)),
      method = names(lsDF)[i],
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, df_list)
}

# ---------------------------------------------------------------------
# 2. Compute summary statistics per method × model size
# ---------------------------------------------------------------------

eval.Summary<-function(dfLong){
  dfLong %>%
  group_by(method, size) %>%
  summarise(
        n = n(),
        mean = mean(value, na.rm = TRUE),
        sd= sd(value, na.rm = TRUE),
        q25 = quantile(value, 0.25, na.rm = TRUE),
        q50 = quantile(value, 0.50, na.rm = TRUE),
        q75 = quantile(value, 0.75, na.rm = TRUE),
        tolM10 = mean(value <= -0.10, na.rm = TRUE),
        tolM05 = mean(value <= -0.05, na.rm = TRUE),
        tolP00 = mean(value <= 0, na.rm = TRUE),
        tolP05 = mean(value <= 0.05, na.rm = TRUE),
        tolP10 = mean(value <= 0.10, na.rm = TRUE),
        .groups  = "drop"
  ) %>%
  ungroup()
}

# ---------------------------------------------------------------------
# 3. Plot 1: Faceted Mean ± SD and Median
# ---------------------------------------------------------------------
p_facet_meanSD <-function(df){
  ggplot(df, aes(x = size)) +
  geom_ribbon(aes(ymin = mean - sd,
                  ymax = mean + sd),
              fill = "skyblue", alpha = 0.25) +
  geom_line(aes(y = mean), color = "blue", linewidth = 1) +
  geom_line(aes(y = q50), color = "darkorange",
            linetype = "dashed", size = 1) +
  geom_point(aes(y = mean), color = "blue", size = 2) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "darkred") +
  facet_wrap(~ method, scales = "free_y") +
  labs(
    x = "Model size (# variables)",
    y = "Relative MSE (vs full model)",
    title = "Prediction Loss vs. Model Size (Faceted)",
    subtitle = "Blue = mean ± SD, Orange = median"
  ) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold"))
}
