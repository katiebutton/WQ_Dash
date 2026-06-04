library(tidyverse) 
library(openxlsx)


# GENERAL WORKFLOW --------------------------------------------------------

# * Note: certified data for each year should be stored on the Z drive as xlsx files: Z:\Files\MONITORING\Estuarine_Eutrophication\02_MASTER\Database\Water_quality_database\current\03_Certified_LoggerData\Certified_Spatial_Data
# 1. Import the certified data file into R and do calculations (using the code below)
# 2. Export calculated values to an xlsx file on the Z drive: Z:\Files\MONITORING\Estuarine_Eutrophication\02_MASTER\Database\Water_quality_database\current\03_Certified_LoggerData\Certified_Spatial_Data
# 3. Go into AGOL, open the group "NCBN Water Quality Monitoring Dashboard", and then open the hosted feature layer named "NCBN Water Quality Monitoring Dashboard" 
# 4. Save a backup copy of the hosted feature layer (use the Export Data button on the right side of AGOL)
# 5. Still in AGOL click "update data" on the right side menu, select "Add features" and upload the xlsx file with the new calculated values, then match up the fields and update the layer
# 6. Open the dashboard "NCBN Water Quality Monitoring Dashboard" and make sure all the components are updated - in particular add the new year to the year filter at the top
# * Note - It looks like the excel file "Final_Master_ENE_Dashboard" in AGOL is not actually used for anything, so not sure if we need to continue updating it?

# -------------------------------------------------------------------------

sample_year <- 2022 # change this each time you update calculations

# Load data ---------------------------------------------------------------
df <- read.xlsx(
  paste0(
    "\\\\files.nps.doi.net/NPS/WASO/Programs/IMD/NCBN/Files/MONITORING/Estuarine_Eutrophication/02_MASTER/Database/Water_quality_database/current/03_Certified_LoggerData/Certified_Spatial_Data/",
    sample_year, "/NCBN_", sample_year, "_WQ_Certified.xlsx"
  ),
  check.names = FALSE,
  sep.names   = "_",
  detectDates = TRUE
) %>%
  rename_with(~trimws(.)) %>% # Trim whitespace from column names (if any)
  # Rename columns to match NCBN Water Quality Monitoring Dashboard hosted feature layer on AGOL
  rename_with(
    ~case_when( # Column name normalization (kept as-is)
      str_detect(., regex("park", ignore_case = TRUE)) ~ "Park",
      str_detect(., regex("sample", ignore_case = TRUE)) ~ "Sample_Year",
      str_detect(., regex("stratum", ignore_case = TRUE)) ~ "Stratum",
      str_detect(., regex("event", ignore_case = TRUE)) ~ "Event_ID",
      str_detect(., regex("num", ignore_case = TRUE)) ~ "Hex_num",
      str_detect(., regex("area", ignore_case = TRUE)) ~ "Hex_Area",
      str_detect(., regex("site", ignore_case = TRUE)) ~ "Site_Type",
      str_detect(., regex("^date", ignore_case = TRUE)) ~ "Date",
      str_detect(., regex("time", ignore_case = TRUE)) ~ "Time",
      str_detect(., regex("depth", ignore_case = TRUE)) & str_detect(., regex("type", ignore_case = TRUE)) ~ "Depth_type",
      str_detect(., regex("kd$", ignore_case = TRUE)) ~ "Kd",
      str_detect(., regex("squared", ignore_case = TRUE)) ~ "Kd_R_Squared",
      str_detect(., regex("qualif", ignore_case = TRUE)) ~ "Kd_Data_Qualifier",
      str_detect(., regex("temp", ignore_case = TRUE)) ~ "Temp_deg_C",
      str_detect(., regex("cond", ignore_case = TRUE)) ~ "Sp_Conductance_mS_cm",
      str_detect(., regex("sali", ignore_case = TRUE)) ~ "Salinity_ppt",
      str_detect(., regex("sat", ignore_case = TRUE)) ~ "DO_PercentSat",
      str_detect(., regex("mg", ignore_case = TRUE)) ~ "DO_mg_L",
      str_detect(., regex("depth", ignore_case = TRUE)) & str_detect(., regex("m", ignore_case = TRUE)) ~ "Depth__m_",
      str_detect(., regex("turb", ignore_case = TRUE)) ~ "Turbidity_NTU",
      str_detect(., regex("chl", ignore_case = TRUE)) ~ "CHl_A_ug_l",
      str_detect(., regex("latitude", ignore_case = TRUE)) ~ "Latitude",
      str_detect(., regex("longitude", ignore_case = TRUE)) ~ "Longitude",
      str_detect(., regex("ph", ignore_case = TRUE)) ~ "pH",
      str_detect(., regex("certified", ignore_case = TRUE)) & str_detect(., regex("date", ignore_case = TRUE)) ~ "Certified_Date",
      str_detect(., regex("certified", ignore_case = TRUE)) & str_detect(., regex("by", ignore_case = TRUE)) ~ "Certified_By",
      str_detect(., regex("qc", ignore_case = TRUE)) ~ "QC_Notes",
      str_detect(., regex("spatial", ignore_case = TRUE)) ~ "Spatial_Analysis"
    )
  ) %>%
  # Keep only rows where Spatial_Analysis is TRUE
  filter(Spatial_Analysis) %>%
  
  # Main calculations and conditions (unchanged except removal of old Weighted_* definitions)
  mutate(
    Park_Area = case_when(
      Park == "ASIS" ~ 37319.36952,
      Park == "CACO" ~ 3179.542435,
      Park == "COLO" ~ 291.8907769,
      Park == "FIIS" ~ 4531.925619,
      Park == "GATE" ~ 5871.890953,
      Park == "GEWA" ~ 142.3594358,
      TRUE ~ NA_real_
    ),
    Percent_Tot = if_else(!is.na(Hex_Area) & !is.na(Park_Area),
                          (Hex_Area / Park_Area) * 100,
                          NA_real_),
    
    # --- CONDITIONS (kept as-is) ---
    Condition_CHLA = case_when(
      is.na(CHl_A_ug_l) ~ "",
      CHl_A_ug_l == -9999 ~ "Missing",
      CHl_A_ug_l < 5 ~ "Good",
      CHl_A_ug_l <= 20 ~ "Fair",
      CHl_A_ug_l > 20 ~ "Poor"
    ),
    Condition_DO = case_when(
      is.na(DO_mg_L) ~ "",
      DO_mg_L == -9999 ~ "Missing",
      DO_mg_L > 5 ~ "Good",
      DO_mg_L >= 2 ~ "Fair",
      DO_mg_L < 2 ~ "Poor"
    ),
    Condition_Kd = case_when(
      is.na(Kd) ~ "",
      Kd == -9999 ~ "Missing",
      Kd < 0.92 ~ "Good",
      Kd <= 1.61 ~ "Fair",
      Kd > 1.61 ~ "Poor"
    ),
    
    # --- STRAT LOGIC (kept as-is) ---
    Weighted_Strat = case_when(
      Park %in% c("ASIS", "FIIS", "GATE", "GEWA") ~ Park_Area, 
      Park == "CACO" & Stratum == 1 ~ 2466.449597,
      Park == "CACO" & Stratum == 2 ~ 559.3187087,
      Park == "CACO" & Stratum == 3 ~ 153.77413,
      Park == "COLO" & Stratum == 1 ~ 223.9309087,
      Park == "COLO" & Stratum == 2 ~ 67.95986818
    ),
    ParkStrat_area = case_when(
      Park %in% c("ASIS", "FIIS", "GATE", "GEWA") ~ Weighted_Strat,
      Park == "CACO" & Stratum == 1 ~ 2466.449597,
      Park == "CACO" & Stratum == 2 ~ 559.3187087,
      Park == "CACO" & Stratum == 3 ~ 153.77413,
      Park == "COLO" & Stratum == 1 ~ 223.9309087,
      Park == "COLO" & Stratum == 2 ~ 67.95986818
    )
  )

# -------------------------------------------------------------------------
# UPDATED: Weighted mean denominators (match trend/AWA logic)
#   - Kd: all depths
#   - CHl_A_ug_l: surface only (Depth_type == 0)
#   - DO_mg_L: bottom only (Depth_type == 2)
#   Denominators are per Park + Year (for Weighted_*)
# -------------------------------------------------------------------------
denoms <- df %>%
  group_by(Park, Sample_Year) %>%
  summarise(
    denom_Kd   = sum(if_else(!is.na(Kd) & !is.na(Hex_Area), Hex_Area, 0), na.rm = TRUE),
    denom_CHYA = sum(if_else(Depth_type == 0 & !is.na(CHl_A_ug_l) & !is.na(Hex_Area), Hex_Area, 0), na.rm = TRUE),
    denom_DO   = sum(if_else(Depth_type == 2 & !is.na(DO_mg_L)    & !is.na(Hex_Area), Hex_Area, 0), na.rm = TRUE),
    .groups = "drop"
  )

df <- df %>%
  left_join(denoms, by = c("Park","Sample_Year")) %>%
  # -----------------------------------------------------------------------
# UPDATED: Weighted_* using denominators above (names unchanged)
#   Each row stores its weighted contribution:
#      value * area / sum(area of rows with value in the group)
#   Summing these per Park+Year yields the weighted mean.
# -----------------------------------------------------------------------
mutate(
  Weighted_Kd   = if_else(!is.na(Kd) & !is.na(Hex_Area) & coalesce(denom_Kd, 0) > 0,
                          Kd * Hex_Area / denom_Kd,
                          NA_real_),
  Weighted_CHYA = if_else(Depth_type == 0 & !is.na(CHl_A_ug_l) & !is.na(Hex_Area) & coalesce(denom_CHYA, 0) > 0,
                          CHl_A_ug_l * Hex_Area / denom_CHYA,
                          NA_real_),
  Weighted_DO   = if_else(Depth_type == 2 & !is.na(DO_mg_L) & !is.na(Hex_Area) & coalesce(denom_DO, 0) > 0,
                          DO_mg_L * Hex_Area / denom_DO,
                          NA_real_)
)

# -------------------------------------------------------------------------
# UPDATED: Stratified denominators and *_StratInc (names unchanged)
#   Denominators are per Park + Year + Stratum
#   Same depth rules apply.
# -------------------------------------------------------------------------
denoms_strat <- df %>%
  group_by(Park, Sample_Year, Stratum) %>%
  summarise(
    denom_Kd_s   = sum(if_else(!is.na(Kd) & !is.na(Hex_Area), Hex_Area, 0), na.rm = TRUE),
    denom_CHYA_s = sum(if_else(Depth_type == 0 & !is.na(CHl_A_ug_l) & !is.na(Hex_Area), Hex_Area, 0), na.rm = TRUE),
    denom_DO_s   = sum(if_else(Depth_type == 2 & !is.na(DO_mg_L)    & !is.na(Hex_Area), Hex_Area, 0), na.rm = TRUE),
    .groups = "drop"
  )

df <- df %>%
  left_join(denoms_strat, by = c("Park","Sample_Year","Stratum")) %>%
  mutate(
    Weighted_Kd_StratInc   = if_else(!is.na(Kd) & !is.na(Hex_Area) & coalesce(denom_Kd_s, 0) > 0,
                                     Kd * Hex_Area / denom_Kd_s,
                                     NA_real_),
    Weighted_CHYA_StratInc = if_else(Depth_type == 0 & !is.na(CHl_A_ug_l) & !is.na(Hex_Area) & coalesce(denom_CHYA_s, 0) > 0,
                                     CHl_A_ug_l * Hex_Area / denom_CHYA_s,
                                     NA_real_),
    Weighted_DO_StratInc   = if_else(Depth_type == 2 & !is.na(DO_mg_L) & !is.na(Hex_Area) & coalesce(denom_DO_s, 0) > 0,
                                     DO_mg_L * Hex_Area / denom_DO_s,
                                     NA_real_)
  )

# -------------------------------------------------------------------------
# Add the Percent_tot_Strat column based on park type (kept as-is)
# -------------------------------------------------------------------------
df <- df %>%
  mutate(
    Percent_tot_Strat = case_when(
      Park %in% c("ASIS", "FIIS", "GATE", "GEWA") & !is.na(Hex_Area) & !is.na(Park_Area) ~
        (Hex_Area / Park_Area) * 100,
      Park %in% c("CACO", "COLO") & !is.na(Hex_Area) & !is.na(ParkStrat_area) ~
        (Hex_Area / ParkStrat_area) * 100,
      TRUE ~ NA_real_
    )
  ) %>%
  
  # Manipulate column types to match the hosted feature layer on AGOL (kept as-is)
  mutate(
    across(c(Park, Event_ID, Kd_Data_Qualifier, Certified_By, QC_Notes, Spatial_Analysis,
             Condition_CHLA, Condition_Kd, Condition_DO), as.character),
    across(c(Date, Certified_Date), ~as.Date(.x, format = "%m/%d/%Y")),
    Time = format(as.POSIXct("1899-12-31") + as.difftime(Time, units = "days"), "%m/%d/%Y %r"),
    across(-c(Park, Event_ID, Kd_Data_Qualifier, Certified_By, QC_Notes, Spatial_Analysis,
              Condition_CHLA, Condition_Kd, Condition_DO, Date, Certified_Date, Time), as.numeric),
    NumRep = ""
  ) %>%
  
  # Final column selection (names unchanged)
  select(
    Park, Sample_Year, Stratum, Event_ID, Hex_num, Hex_Area, Site_Type,
    Date, Time, Depth_type, Kd, Kd_R_Squared, Kd_Data_Qualifier, Temp_deg_C,
    Sp_Conductance_mS_cm, Salinity_ppt, DO_PercentSat, DO_mg_L, Depth__m_,
    Turbidity_NTU, CHl_A_ug_l, Latitude, Longitude, pH, Certified_Date,
    Certified_By, QC_Notes, Spatial_Analysis,
    Weighted_Kd, Weighted_DO, Weighted_CHYA,        # UPDATED logic, same names
    Park_Area, Condition_CHLA, Condition_Kd, Percent_Tot, Condition_DO,
    Weighted_Strat, ParkStrat_area,
    Weighted_Kd_StratInc, Weighted_DO_StratInc, Weighted_CHYA_StratInc,  # UPDATED logic, same names
    Percent_tot_Strat, NumRep
  )

# Save the output xlsx without NAs showing as NA --------------------------
write.xlsx(
  df,
  paste0(
    "\\\\files.nps.doi.net/NPS/WASO/Programs/IMD/NCBN/Files/MONITORING/Estuarine_Eutrophication/02_MASTER/Database/Water_quality_database/current/03_Certified_LoggerData/Certified_Spatial_Data/",
    sample_year, "/", sample_year, "_Final_Master_ENE_Dashboard.xlsx"
  ),
  keepNA = FALSE
)
