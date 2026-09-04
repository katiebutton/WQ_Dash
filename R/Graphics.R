# ============================================================
# ESTUARINE AREA CONDITION PLOTS
# ============================================================

library(tidyverse)
library(openxlsx)


# ------------------------------------------------------------
# 1) SETTINGS
# ------------------------------------------------------------

park_to_plot <- "ASIS"

final_path <- "\\\\files.nps.doi.net/NPS/WASO/Programs/IMD/NCBN/Files/MONITORING/Estuarine_Eutrophication/02_MASTER/Database/Water_quality_database/current/03_Certified_LoggerData/Certified_Spatial_Data/2024/2024_Final_Master_ENE_Dashboard.xlsx"


# ------------------------------------------------------------
# 2) OUTPUT FOLDER
# ------------------------------------------------------------

output_folder <- paste0("Bar_Graph_Figs_", park_to_plot)

if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}


# ------------------------------------------------------------
# 3) LOAD DATA
# ------------------------------------------------------------

df <- read.xlsx(
  final_path,
  detectDates = TRUE,
  sep.names = "_",
  check.names = FALSE
) %>%
  filter(Park == park_to_plot)


# ------------------------------------------------------------
# 4) PLOT FUNCTION
# ------------------------------------------------------------

plot_metric <- function(df, metric, save = FALSE) {
  
  # ---- Metric-specific settings ----
  
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
    
  } else if (metric == "CHLA") {
    
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
  
  
  # ----------------------------------------------------------
  # Filter and summarize
  # ----------------------------------------------------------
  
  summ <- df %>%
    filter(Depth_type == depth_keep) %>%
    mutate(
      Sample_Year = as.numeric(Sample_Year),
      condition = .data[[cond_col]],
      condition = ifelse(
        is.na(condition) | condition == "",
        "Missing",
        condition
      ),
      condition = str_trim(condition)
    ) %>%
    group_by(Sample_Year, condition) %>%
    summarise(
      percent = sum(Percent_Tot, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    # Make sure every year has all four categories
    complete(
      Sample_Year = sort(
        unique(Sample_Year),
        decreasing = TRUE
      ),
      condition = c(
        "Missing",
        "Poor",
        "Fair",
        "Good"
      ),
      fill = list(percent = 0)
    ) %>%
    
    # Normalize each year to 100%
    group_by(Sample_Year) %>%
    mutate(
      percent = percent / sum(percent) * 100
    ) %>%
    ungroup() %>%
    
    # Round for cleaner plotting
    mutate(
      percent = round(percent, 1),
      Sample_Year = factor(
        Sample_Year,
        levels = sort(
          unique(Sample_Year),
          decreasing = TRUE
        )
      )
    )
  
  
  # ----------------------------------------------------------
  # Highlight most recent year
  # ----------------------------------------------------------
  
  latest_year <- max(
    as.numeric(as.character(summ$Sample_Year)),
    na.rm = TRUE
  )
  
  summ <- summ %>%
    mutate(
      highlight =
        as.numeric(as.character(Sample_Year)) == latest_year
    )
  
  
  # ----------------------------------------------------------
  # Stack order
  # ----------------------------------------------------------
  
  summ$condition <- factor(
    summ$condition,
    levels = c(
      "Missing",
      "Poor",
      "Fair",
      "Good"
    )
  )
  
  
  # ----------------------------------------------------------
  # Outline around latest year
  # ----------------------------------------------------------
  
  outline_df <- summ %>%
    group_by(Sample_Year) %>%
    summarise(
      total = sum(percent),
      .groups = "drop"
    ) %>%
    filter(
      as.numeric(as.character(Sample_Year)) ==
        latest_year
    )
  
  
  # ----------------------------------------------------------
  # Make plot
  # ----------------------------------------------------------
  
  p <- ggplot(
    summ,
    aes(
      x = percent,
      y = Sample_Year,
      fill = condition
    )
  ) +
    
    # Highlight most recent year
    geom_tile(
      data = tibble(
        Sample_Year = factor(
          latest_year,
          levels = levels(summ$Sample_Year)
        ),
        x = 50
      ),
      aes(
        x = x,
        y = Sample_Year
      ),
      width = 100,
      height = 0.9,
      fill = "#B2DFDB",
      alpha = 0.6,
      inherit.aes = FALSE
    ) +
    
    # Stacked bars
    geom_col(
      aes(alpha = highlight),
      width = 0.65
    ) +
    
    # Black outline around latest year
    geom_col(
      data = outline_df,
      aes(
        x = total,
        y = Sample_Year
      ),
      fill = NA,
      color = "black",
      linewidth = 1.2,
      inherit.aes = FALSE
    ) +
    
    # Colors
    scale_fill_manual(
      values = c(
        "Good" = "#7FBF3F",
        "Fair" = "#F2A900",
        "Poor" = "#D73027",
        "Missing" = "#BDBDBD"
      ),
      labels = legend_lbl,
      breaks = c(
        "Missing",
        "Poor",
        "Fair",
        "Good"
      )
    ) +
    
    # Reverse legend so it reads Good → Missing
    guides(
      fill = guide_legend(reverse = TRUE)
    ) +
    
    # Transparency for older years
    scale_alpha_manual(
      values = c(
        `TRUE` = 1,
        `FALSE` = 0.4
      ),
      guide = "none"
    ) +
    
    # X axis
    scale_x_continuous(
      limits = c(0, 101),
      breaks = c(0, 25, 50, 75, 100),
      labels = function(x) paste0(x, "%")
    ) +
    
    # Labels
    labs(
      title = plot_title,
      subtitle = paste0(
        "Most recent year (",
        latest_year,
        ") highlighted"
      ),
      fill = NULL
    ) +
    
    # Theme
    theme_minimal(
      base_size = 12
    ) +
    
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(
        color = "grey85"
      ),
      
      axis.title = element_blank(),
      
      axis.text = element_text(
        size = 14,
        face = "bold"
      ),
      
      plot.title = element_text(
        face = "bold",
        size = 18
      ),
      
      plot.subtitle = element_text(
        size = 14
      ),
      
      legend.text = element_text(
        size = 12
      ),
      
      legend.position = "bottom"
    )
  
  
  # ----------------------------------------------------------
  # Save plot
  # ----------------------------------------------------------
  
  if (save) {
    
    ggsave(
      filename = file.path(
        output_folder,
        paste0(
          park_to_plot,
          "_",
          metric,
          ".png"
        )
      ),
      plot = p,
      width = 7,
      height = 6,
      dpi = 300
    )
  }
  
  
  return(p)
}


# ------------------------------------------------------------
# 5) CREATE AND SAVE ALL THREE PLOTS
# ------------------------------------------------------------

p_CHLA <- plot_metric(
  df,
  "CHLA",
  save = TRUE
)

p_DO <- plot_metric(
  df,
  "DO",
  save = TRUE
)

p_Kd <- plot_metric(
  df,
  "Kd",
  save = TRUE
)


# ------------------------------------------------------------
# 6) DISPLAY PLOTS IN RSTUDIO
# ------------------------------------------------------------

print(p_CHLA)
print(p_DO)
print(p_Kd)
