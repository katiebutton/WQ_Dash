plot_metric <- function(df, metric) {
  
  library(tidyverse)
  
  # -------------------------------------------------------
  # METRIC SETUP (your logic preserved)
  # -------------------------------------------------------
  
  if (metric == "DO") {
    cond_col   <- "Condition_DO"
    depth_keep <- 2
    legend_lbl <- c(
      Good = "Good (>5 mg/L)",
      Fair = "Fair (2–5 mg/L)",
      Poor = "Poor (<2 mg/L)",
      Missing = "Missing"
    )
    plot_title <- "Percent of Estuarine Area — Dissolved Oxygen (Bottom)"
    
  } else if (metric == "Kd") {
    cond_col   <- "Condition_Kd"
    depth_keep <- 0
    legend_lbl <- c(
      Good = "Good (<0.92)",
      Fair = "Fair (0.92–1.61)",
      Poor = "Poor (>1.61)",
      Missing = "Missing"
    )
    plot_title <- "Percent of Estuarine Area — Kd (Surface)"
    
  } else { # CHLA
    cond_col   <- "Condition_CHLA"
    depth_keep <- 0
    legend_lbl <- c(
      Good = "Good (<5)",
      Fair = "Fair (5–20)",
      Poor = "Poor (>20)",
      Missing = "Missing"
    )
    plot_title <- "Percent of Estuarine Area — Chlorophyll-a (Surface)"
  }
  
  # -------------------------------------------------------
  # DATA PREP
  # -------------------------------------------------------
  
  df_sub <- df %>%
    filter(Depth_type == depth_keep) %>%
    mutate(condition = .data[[cond_col]])
  
  summ <- df_sub %>%
    group_by(Sample_Year, condition) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Sample_Year) %>%
    mutate(percent = n / sum(n) * 100) %>%
    ungroup()
  
  # Order + highlight
  summ <- summ %>%
    mutate(year_num = as.numeric(as.character(Sample_Year))) %>%
    arrange(desc(year_num)) %>%
    mutate(
      Sample_Year = factor(year_num, levels = unique(year_num)),
      highlight = year_num == max(year_num)
    )
  
  # Ensure condition order
  summ$condition <- factor(
    summ$condition,
    levels = c("Good", "Fair", "Poor", "Missing")
  )
  
  # -------------------------------------------------------
  # OUTLINE DATA
  # -------------------------------------------------------
  
  latest_year <- max(summ$year_num)
  
  outline_df <- summ %>%
    group_by(Sample_Year) %>%
    summarise(total = sum(percent), .groups = "drop") %>%
    filter(as.numeric(as.character(Sample_Year)) == latest_year)
  
  # -------------------------------------------------------
  # PLOT
  # -------------------------------------------------------
  
  p <- ggplot(summ, aes(x = percent, y = Sample_Year, fill = condition)) +
    
    geom_col(aes(alpha = highlight), width = 0.65) +
    
    geom_col(
      data = outline_df,
      aes(x = total, y = Sample_Year),
      fill = NA,
      color = "black",
      linewidth = 1.2,
      inherit.aes = FALSE
    ) +
    
    scale_fill_manual(
      values = c(
        "Good" = "#7FBF3F",
        "Fair" = "#F2A900",
        "Poor" = "#D73027",
        "Missing" = "#BDBDBD"
      ),
      labels = legend_lbl   # 🔥 THIS is the key piece
    ) +
    
    scale_alpha_manual(
      values = c(`TRUE` = 1, `FALSE` = 0.4),
      guide = "none"
    ) +
    
    scale_x_continuous(
      limits = c(0, 101),
      breaks = c(0, 25, 50, 75, 100),
      labels = function(x) paste0(x, "%")
    ) +
    
    labs(
      title = plot_title,
      subtitle = paste0("Most recent year (", latest_year, ") highlighted"),
      fill = NULL
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey85"),
      axis.title = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )
  
  return(p)
}
p_CHLA <- plot_metric(df, "CHLA")
p_DO   <- plot_metric(df, "DO")
p_Kd   <- plot_metric(df, "Kd")

print(p_CHLA)
print(p_DO)
print(p_Kd)
