# ==================================================
# Pediatric Asthma Hospitalizations in Texas, 2019
# Figures 1–3
# install.packages("gt")
# ==================================================

rm(list = ls())
cat("\014")
graphics.off()

library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(tidyr)
library(sf)
library(tigris)
library(viridis)
library(patchwork)
library(gt)

options(tigris_use_cache = TRUE)

# -------------------------------
# Step 0: Read PUDF quarterly files
# -------------------------------
files <- list.files(
  path = "C:/Abhinav/Asthama/Source",
  pattern = "^PUDF_base1_.*q2019_tab\\.txt$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No PUDF 2019 quarterly files found. Check the folder path and file names.")
}

pudf_all <- lapply(files, function(f) {
  read_delim(f, delim = "\t", col_types = cols(.default = "c")) %>%
    mutate(
      QUARTER = case_when(
        grepl("_1q2019_", f) ~ "Q1",
        grepl("_2q2019_", f) ~ "Q2",
        grepl("_3q2019_", f) ~ "Q3",
        grepl("_4q2019_", f) ~ "Q4",
        TRUE ~ "Unknown"
      )
    )
}) %>%
  bind_rows()

# -------------------------------
# Step 1: Clean variables
# -------------------------------
pudf_all <- pudf_all %>%
  mutate(
    PAT_AGE = as.numeric(PAT_AGE),
    PAT_COUNTY = sprintf("%03d", as.integer(PAT_COUNTY)),
    PRINC_DIAG_CODE = toupper(PRINC_DIAG_CODE)
  )

# -------------------------------
# Step 2: County lookup
# -------------------------------
county_lookup <- read_csv(
  "C:/Abhinav/Asthama/Source/texas_county_lookup.csv",
  col_types = cols(.default = "c")
) %>%
  filter(`State FIPS` == "48") %>%
  transmute(
    PAT_COUNTY = sprintf("%03d", as.integer(`County FIPS`)),
    COUNTY_NAME = `County Name`
  )

# -------------------------------
# Step 3: Pediatric asthma cases
# -------------------------------
asthma_peds <- pudf_all %>%
  filter(
    !is.na(PAT_AGE),
    PAT_AGE < 18,
    substr(PRINC_DIAG_CODE, 1, 3) %in% c("J45", "J46")
  ) %>%
  left_join(county_lookup, by = "PAT_COUNTY") %>%
  filter(!is.na(COUNTY_NAME))

# -------------------------------
# Step 4: All-age asthma cases
# -------------------------------
asthma_all <- pudf_all %>%
  filter(
    substr(PRINC_DIAG_CODE, 1, 3) %in% c("J45", "J46")
  ) %>%
  left_join(county_lookup, by = "PAT_COUNTY") %>%
  filter(!is.na(COUNTY_NAME))

# ==================================================
# Figure 1: Texas county map
# ==================================================
pediatric_county <- asthma_peds %>%
  group_by(PAT_COUNTY, COUNTY_NAME) %>%
  summarise(
    Pediatric_Hospitalizations = n(),
    .groups = "drop"
  )

tx_counties <- counties(state = "TX", cb = TRUE, class = "sf") %>%
  mutate(PAT_COUNTY = COUNTYFP)

map_data <- tx_counties %>%
  left_join(pediatric_county, by = "PAT_COUNTY") %>%
  mutate(
    Pediatric_Hospitalizations = ifelse(
      is.na(Pediatric_Hospitalizations),
      0,
      Pediatric_Hospitalizations
    )
  )

# Create county centroids for labels

label_points <- map_data %>%
  slice_max(Pediatric_Hospitalizations, n = 10) %>%
  st_point_on_surface()

p_map <- ggplot(map_data) +
  geom_sf(aes(fill = Pediatric_Hospitalizations),
          color = "white",
          size = 0.1) +
  geom_sf_text(
    data = label_points,
    aes(label = paste0(COUNTY_NAME, "\n", Pediatric_Hospitalizations)),
    size = 2,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Figure 1. Top 10 Pediatric Asthma Hospitalizations by Texas County",
    subtitle = "Patients under 18 years old, 2019",
    fill = "Hospitalizations"
  ) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# p_map  # display in Rmd, not while sourcing

top10_table <- map_data %>%
  st_drop_geometry() %>%
  arrange(desc(Pediatric_Hospitalizations)) %>%
  select(COUNTY_NAME, Pediatric_Hospitalizations) %>%
  slice(1:10)


# gt(top10_table)  # display in Rmd, not while sourcing

# ==================================================
# Figure 2: Quarterly pediatric hospitalizations
# ==================================================

county_quarter_counts <- asthma_peds %>%
  group_by(COUNTY_NAME, QUARTER) %>%
  summarise(
    hospitalizations = n(),
    .groups = "drop"
  )

top10_counties <- county_quarter_counts %>%
  group_by(COUNTY_NAME) %>%
  summarise(
    Total = sum(hospitalizations),
    .groups = "drop"
  ) %>%
  arrange(desc(Total)) %>%
  slice_head(n = 10)

top10_quarter_counts <- county_quarter_counts %>%
  filter(COUNTY_NAME %in% top10_counties$COUNTY_NAME) %>%
  mutate(
    COUNTY_NAME = factor(
      COUNTY_NAME,
      levels = rev(top10_counties$COUNTY_NAME)
    ),
    QUARTER = factor(
      QUARTER,
      levels = c("Q1", "Q2", "Q3", "Q4")
    )
  )


p_quarter <- ggplot(
  top10_quarter_counts,
  aes(
    x = COUNTY_NAME,
    y = hospitalizations,
    fill = QUARTER
  )
) +
  geom_col(position = position_dodge(width = 0.9)) +
  
  geom_text(
    aes(label = hospitalizations),
    position = position_dodge(width = 0.9),
    hjust = -0.2,
    size = 3
  ) +
  
  coord_flip() +
  
  scale_fill_viridis_d(option = "plasma") +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1))
  ) +
  
  theme_minimal(base_size = 12) +
  
  labs(
    title = "Figure 2. Quarterly Pediatric Asthma Hospitalizations",
    subtitle = "Top 10 Texas Counties, 2019",
    x = "County",
    y = "Hospitalizations",
    fill = "Quarter"
  )

# p_quarter  # display in Rmd, not while sourcing

# ==================================================
# Figure 3 / Table 1: Validation table
# Pediatric counts vs all-age counts
# ==================================================

all_age_county <- asthma_all %>%
  group_by(COUNTY_NAME) %>%
  summarise(
    All_Age_Hospitalizations = n(),
    .groups = "drop"
  )

pediatric_county_simple <- asthma_peds %>%
  group_by(COUNTY_NAME) %>%
  summarise(
    Pediatric_Hospitalizations = n(),
    .groups = "drop"
  )

validation_table <- all_age_county %>%
  left_join(pediatric_county_simple, by = "COUNTY_NAME") %>%
  mutate(
    Pediatric_Hospitalizations = replace_na(Pediatric_Hospitalizations, 0),
    Pediatric_Percent = round(
      100 * Pediatric_Hospitalizations / All_Age_Hospitalizations,
      1
    ),
    Validation_Check = ifelse(
      Pediatric_Hospitalizations <= All_Age_Hospitalizations,
      "Pass",
      "Fail"
    )
  ) %>%
  arrange(desc(All_Age_Hospitalizations))

top10_validation <- validation_table %>%
  slice_head(n = 10) %>%
  select(
    County = COUNTY_NAME,
    `All-Age Asthma Hospitalizations` = All_Age_Hospitalizations,
    `Pediatric Asthma Hospitalizations` = Pediatric_Hospitalizations,
    `Pediatric Share (%)` = Pediatric_Percent,
    `Validation Check` = Validation_Check
  )

# gt(top10_validation)  # display in Rmd, not while sourcing
