library(tidyverse)
library(openxlsx)

# -------------------------------------------------------
# LOAD DATA
# -------------------------------------------------------
final_path <- "\\\\files.nps.doi.net/NPS/WASO/Programs/IMD/NCBN/Files/MONITORING/Estuarine_Eutrophication/02_MASTER/Database/Water_quality_database/current/03_Certified_LoggerData/Certified_Spatial_Data/2024/2024_Final_Master_ENE_Dashboard.xlsx"

df <- read.xlsx(final_path, detectDates = TRUE, sep.names = "_", check.names = FALSE) %>%
  filter(Park == "FIIS")

# -------------------------------------------------------
# AGG FUNCTION — builds % area by year + condition for a metric
# -------------------------------------------------------
agg_by_year <- function(data, metric = c("DO","Kd","CHLA")) {
  
  metric <- match.arg(metric)
  
  # Metric setup
  if (metric == "DO") {
    cond_col   <- "Condition_DO"
    depth_keep <- 2
    legend_lbl <- c(Good="Good (>5 mg/L)", Fair="Fair (2–5 mg/L)",
                    Poor="Poor (<2 mg/L)", Missing="Missing")
    plot_title <- "Percent of Estuarine Area — Dissolved Oxygen (Bottom)"
    
  } else if (metric == "Kd") {
    cond_col   <- "Condition_Kd"
    depth_keep <- 0
    legend_lbl <- c(Good="Good (<0.92)", Fair="Fair (0.92–1.61)",
                    Poor="Poor (>1.61)", Missing="Missing")
    plot_title <- "Percent of Estuarine Area — Kd (Surface)"
    
  } else { # CHLA
    cond_col   <- "Condition_CHLA"
    depth_keep <- 0
    legend_lbl <- c(Good="Good (<5)", Fair="Fair (5–20)",
                    Poor="Poor (>20)", Missing="Missing")
    plot_title <- "Percent of Estuarine Area — Chlorophyll‑a (Surface)"
  }
  
  # Compute year levels for THIS metric/depth (newest → oldest)
  year_levels <- data %>%
    filter(Depth_type == depth_keep) %>%
    pull(Sample_Year) %>%
    unique() %>%
    sort(decreasing = TRUE)
  
  # Aggregate percent by year + condition
  summ <- data %>%
    filter(Depth_type == depth_keep) %>%
    mutate(
      Sample_Year = as.numeric(Sample_Year),
      condition   = .data[[cond_col]]
    ) %>%
    group_by(Sample_Year, condition) %>%
    summarise(percent = sum(Percent_Tot, na.rm = TRUE), .groups = "drop") %>%
    # ensure all condition categories exist in every year (fill zeros)
    complete(
      Sample_Year = year_levels,
      condition = factor(c("Good","Fair","Poor","Missing"),
                         levels = c("Good","Fair","Poor","Missing")),
      fill = list(percent = 0)
    ) %>%
    mutate(
      percent = round(percent, 1),
      # lock factor order newest → oldest on the y-axis
      Sample_Year = factor(Sample_Year, levels = year_levels)
    )
  
  list(summary = summ,
       legend  = legend_lbl,
       title   = plot_title,
       year_levels = year_levels)
}

# -------------------------------------------------------
# COLORS
# -------------------------------------------------------
pal <- c(
  Good    = "#8BC34A",
  Fair    = "#F6B10C",
  Poor    = "#E53935",
  Missing = "#9E9E9E"
)

# -------------------------------------------------------
# PLOT FUNCTION — reverses years (newest top) and highlights a target year
# -------------------------------------------------------
plot_metric <- function(metric = c("DO","Kd","CHLA"),
                        highlight_year = NULL,  # e.g., "2023"
                        save = FALSE) {
  
  metric <- match.arg(metric)
  meta   <- agg_by_year(df, metric)
  summ   <- meta$summary
  
  # If no highlight year provided, default to newest available
  if (is.null(highlight_year)) {
    highlight_year <- as.character(meta$year_levels[1])
  } else {
    highlight_year <- as.character(highlight_year)
  }
  
  # If requested highlight year isn't present, fall back to newest
  if (!highlight_year %in% levels(summ$Sample_Year)) {
    highlight_year <- levels(summ$Sample_Year)[1]
  }
  
  # y position index for highlight (discrete y → integer positions 1..n)
  y_levels   <- levels(summ$Sample_Year)
  recent_pos <- which(y_levels == highlight_year)
  
  # Background highlight rectangle (inside x-scale, so no warnings)
  highlight_df <- tibble(
    ymin = recent_pos - 0.45,
    ymax = recent_pos + 0.45
  )
  
  p <- ggplot() +
    # Highlight band behind selected year
    geom_rect(
      data = highlight_df,
      aes(xmin = 0, xmax = 100, ymin = ymin, ymax = ymax),
      fill = "#B2DFDB", alpha = 0.6
    ) +
    # Stacked 100% horizontal bars
    geom_col(
      data = summ,
      aes(x = percent, y = Sample_Year, fill = condition),
      width = 0.7
    ) +
    scale_x_continuous(
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      labels = function(x) paste0(x, "%")
    ) +
    scale_fill_manual(values = pal, labels = meta$legend, name = NULL) +
    labs(
      title = meta$title,
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      axis.text.y = element_text(size = 12),
      plot.title  = element_text(size = 16, face = "bold")
    ) +
    # Left bracket (kept inside 0–100 range so it never warns)
    annotate(
      "segment",
      x = 0.2, xend = 0.2,
      y = recent_pos - 0.32,
      yend = recent_pos + 0.32,
      size = 1.4, colour = "black"
    ) +
    annotate(
      "segment",
      x = 0.2, xend = 2,
      y = recent_pos + 0.32,
      yend = recent_pos + 0.32,
      size = 1.4, colour = "black"
    )
  
  if (save) {
    out_png <- paste0("FIIS_", metric, "_", highlight_year, "_", Sys.Date(), ".png")
    ggsave(out_png, p, width = 7, height = 6, dpi = 300)
    message("Saved: ", out_png)
  }
  
  return(p)
}

# -------------------------------------------------------
# RUN (highlight 2023; falls back to newest if 2023 missing for a metric)
# -------------------------------------------------------
p_DO   <- plot_metric("DO",   highlight_year = "2023")
p_Kd   <- plot_metric("Kd",   highlight_year = "2023")
p_CHLA <- plot_metric("CHLA", highlight_year = "2023")

# View one:
print(p_DO)
