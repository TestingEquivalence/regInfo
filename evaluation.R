library(dplyr)
library(readODS)
library(ggplot2)

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

freq_min_sizes <- function(dfLong, tol = 0) {
  dfLong %>%
    group_by(rep, method) %>%                      # per repeat and method
    filter(value <= tol) %>%                       # only models meeting threshold
    slice_min(size, n = 1, with_ties = FALSE) %>% # pick smallest size per repeat
    ungroup() %>%
    group_by(method, size) %>%                     # count how many repeats picked each size
    summarise(count = n(), .groups = "drop") %>%
    group_by(method) %>%
    mutate(freq = count / sum(count)) %>%          # compute frequency
    select(method, size, freq)                     # keep only relevant columns
}


# ---------------------------------------------------------------------
# 3. Plot 1: Faceted Mean ± SD and Median
# ---------------------------------------------------------------------
p_facet_meanSD <- function(df, free_y = TRUE, base_size = 14) {
  ggplot(df, aes(x = size)) +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd),
                fill = "skyblue", alpha = 0.25) +
    geom_line(aes(y = mean), color = "blue", linewidth = 1) +
    geom_line(aes(y = q50), color = "darkorange",
              linetype = "dashed", linewidth = 1) +
    geom_point(aes(y = mean), color = "blue", size = 2) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "darkred") +  # baseline at 0
    facet_wrap(~method, scales = if (free_y) "free_y" else "fixed") +
    scale_x_continuous(breaks = sort(unique(df$size))) +
    labs(
      x = "Model size",
      y = "Relative MSE",
      title = "Relative MSE vs. Model Size",
      subtitle = "Blue = mean ± SD, Orange = median"
    ) +
    theme_minimal(base_size = base_size) +
    theme(strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank())
}


# ---------------------------------------------------------------------
# 4. Plot 2: Faceted Proportion within Tolerance
# ---------------------------------------------------------------------
p_facet_prop <- function(df){
  ggplot(df, aes(x = size, y = tolP00)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "darkred") +
  facet_wrap(~ method, scales = "free_y") +
  scale_x_continuous(breaks = sort(unique(df$size))) +  
  labs(
    x = "Model size",
    y = "Proportion with relative MSE ≤ 0",
    title = "Proportion with relative MSE ≤ 0"
  ) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())
}

# ---------------------------------------------------------------------
# 5. Plot 3: Combined Overlay for All Methods
# ---------------------------------------------------------------------
p_overlay_mean <- function(df){
  ggplot(df, aes(x = size, color = method, fill = method)) +
  geom_ribbon(aes(ymin = mean - sd,
                  ymax = mean + sd),
              alpha = 0.2, color = NA) +
  geom_line(aes(y = mean), linewidth = 1.2) +
  geom_point(aes(y = mean), size = 2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "darkred") +
  scale_x_continuous(breaks = sort(unique(df$size))) + 
  labs(
    x = "Model size",
    y = "Relative MSE",
    title = "Mean of relative MSE across selection methods",
    subtitle="Shaded = ±SD"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )
}

p_overlay_median <- function(df) {
  ggplot(df, aes(x = size, color = method)) +
    geom_line(aes(y = q50), linewidth = 1.2) +
    geom_point(aes(y = q50), size = 2) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "darkred") +
    scale_x_continuous(breaks = sort(unique(df$size))) + 
    labs(
      x = "Model size",
      y = "Relative MSE",
      title = "Median of relative MSE across selection methods"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank()
    )
}

p_overlay_prop <- function(df) {
  ggplot(df, aes(x = size, color = method)) +
    geom_line(aes(y = tolP00), linewidth = 1.2) +
    geom_point(aes(y = tolP00), size = 2) +
    geom_hline(yintercept = 0.5, linetype = "dotted", color = "darkred") +
    scale_x_continuous(breaks = sort(unique(df$size))) + 
    labs(
      x = "Model size",
      y = "Proportion with relative MSE ≤ 0",
      title = "Proportion with relative MSE ≤ 0 across selection methods"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank()
    )
}

library(ggplot2)

p_overlay_freq <- function(df) {
  ggplot(df, aes(x = size, color = method)) +
    geom_line(aes(y = freq), linewidth = 1.2) +   # use frequency
    geom_point(aes(y = freq), size = 2) +
    geom_hline(yintercept = 0.5, linetype = "dotted", color = "darkred") +
    scale_x_continuous(breaks = sort(unique(df$size))) + 
    labs(
      x = "Model size",
      y = "Frequency of the minimal model size",
      title = "Distribution of selected minimal model sizes across methods"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank()
    )
}



