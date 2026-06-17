
# -------------------------------
# 0. Install packages (run ONCE)
# -------------------------------
# install.packages(c("readr","dplyr","sf","ggplot2","tigris","viridis","plotly"))

# -------------------------------
# 1. Load libraries
# -------------------------------
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(tigris)
library(viridis)
library(plotly)

options(tigris_use_cache = TRUE)

# -------------------------------
# 2. Set working directory
# -------------------------------
setwd("C:/Abhinav/Asthama/Source")

# -------------------------------
# 3. Load Texas county geometries
# -------------------------------
texas_counties <- counties(state = "TX", cb = TRUE, class = "sf")

# -------------------------------
# 4. Load county FIPS lookup
# -------------------------------
county_lookup <- read_csv("texas_county_lookup.csv", col_types = cols(.default = "c")) %>%
  filter(`State FIPS` == "48") %>%
  transmute(
    PAT_COUNTY = sprintf("%03d", as.integer(`County FIPS`)),
    COUNTY_NAME = `County Name`
  )

stopifnot(nrow(county_lookup) == 254)

# -------------------------------
# 5. Read & combine all 2019 PUDF quarterly files
# -------------------------------
files <- list.files(
  path = "C:/Abhinav/Asthama/Source",
  pattern = "^PUDF_base1_.*q2019_tab\\.txt$",
  full.names = TRUE
)

pudf_all <- lapply(files, function(f) {
  read_delim(f, delim = "\t", col_types = cols(.default = "c"))
}) %>% bind_rows()

# -------------------------------
# 6. Prepare PUDF variables
# -------------------------------
pudf_all <- pudf_all %>%
  mutate(
    PAT_AGE = as.numeric(PAT_AGE),
    PAT_COUNTY = sprintf("%03d", as.integer(PAT_COUNTY))
  )

# -------------------------------
# 7. Filter pediatric asthma hospitalizations (J45/J46)
# -------------------------------
asthma_peds <- pudf_all %>%
  filter(
    !is.na(PAT_AGE),
    PAT_AGE < 18,
    substr(PRINC_DIAG_CODE, 1, 3) %in% c("J45", "J46")
  )

# -------------------------------
# 8. Join county names
# -------------------------------
asthma_peds <- asthma_peds %>%
  left_join(county_lookup, by = "PAT_COUNTY")

# -------------------------------
# 9. Aggregate total hospitalizations by county
# -------------------------------
county_counts_named <- asthma_peds %>%
  group_by(PAT_COUNTY, COUNTY_NAME) %>%
  summarise(hospitalizations = n(), .groups = "drop")

# Prepare GEOID for mapping
asthma_map <- county_counts_named %>%
  mutate(GEOID = paste0("48", PAT_COUNTY)) %>%
  group_by(GEOID) %>%
  summarise(Total = sum(hospitalizations, na.rm = TRUE))

# -------------------------------
# 10. Join asthma data to Texas geometry
# -------------------------------
texas_map <- texas_counties %>%
  left_join(asthma_map, by = "GEOID")

# -------------------------------
# 11. Top 10 counties
# -------------------------------
top10_asthma <- county_counts_named %>%
  group_by(COUNTY_NAME) %>%
  summarise(Total = sum(hospitalizations, na.rm = TRUE)) %>%
  arrange(desc(Total)) %>%
  slice_head(n = 10)

# -------------------------------
# 12. Interactive Texas Map
# -------------------------------
p_map <- ggplot(texas_map) +
  geom_sf(
    aes(
      fill = Total,
      text = paste0("County: ", NAME, "<br>Total Hospitalizations: ", Total)
    ),
    color = "white",
    linewidth = 0.1
  ) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey85") +
  coord_sf(xlim = c(-107, -93), ylim = c(25.5, 36.5)) +
  theme_minimal() +
  labs(
    title = "Total Pediatric Asthma Hospitalizations by County",
    subtitle = "Texas, 2019 (All Quarters)",
    fill = "Hospitalizations",
    caption = "Data source: Texas PUDF 2019 Q1–Q4; Top counties highlight disparities."
  )

ggplotly(p_map, tooltip = "text")

# -------------------------------
# 13. Interactive Top 10 Bar Chart
# -------------------------------
p_bar <- ggplot(top10_asthma, aes(
  x = reorder(COUNTY_NAME, Total),
  y = Total,
  text = paste0("County: ", COUNTY_NAME, "<br>Total: ", Total)
)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Texas Counties by Pediatric Asthma Hospitalizations",
    x = "County",
    y = "Hospitalizations"
  )

ggplotly(p_bar, tooltip = "text")
