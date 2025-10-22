library(dplyr)

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


# # Rename 'col' -> 'size' for consistency
# colnames(relMSE_summary)[colnames(relMSE_summary) == "col"] <- "size"
# 
# # Round numeric columns
# is_num <- sapply(relMSE_summary, is.numeric)
# relMSE_summary[is_num] <- lapply(relMSE_summary[is_num], function(x) round(x, 3))
# 
# # Print all rows
# print(relMSE_summary, row.names = FALSE)
