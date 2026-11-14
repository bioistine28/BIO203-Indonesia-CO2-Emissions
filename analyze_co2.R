# ==========================================
# BIO 203: INDONESIA CO2 EMISSIONS ANALYSIS
# ==========================================

# 1. Install and load packages
install.packages("tidyverse")  # Run only once

library(tidyverse)

# 2. Create project folders if they do not exist
dir.create("data", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

# 3. Download the official dataset
dataset_url <- paste0(
  "https://catalog.ourworldindata.org/",
  "emissions/owid_co2/owid_co2.csv"
)

download.file(
  dataset_url,
  "data/owid-co2-data.csv",
  mode = "wb"
)

# 4. Import the dataset
co2_data <- read_csv("data/owid-co2-data.csv")

# 5. Check the dataset
glimpse(co2_data)
names(co2_data)

# 6. Filter Indonesia (1990–2024)
indonesia <- co2_data %>%
  filter(
    country == "Indonesia",
    year >= 1990,
    year <= 2024
  ) %>%
  select(
    country,
    year,
    co2,
    co2_per_capita,
    population
  ) %>%
  drop_na(co2, co2_per_capita)

# 7. Display the cleaned data
print(indonesia)

# 8. Save the cleaned dataset
write_csv(
  indonesia,
  "data/indonesia_co2_data.csv"
)

# 9. Summary statistics
summary(indonesia)

# 10. Save summary statistics
summary_table <- indonesia %>%
  summarise(
    First_Year = min(year),
    Last_Year = max(year),
    Average_CO2 = mean(co2),
    Highest_CO2 = max(co2),
    Lowest_CO2 = min(co2),
    Average_CO2_Per_Capita = mean(co2_per_capita)
  )

print(summary_table)

write_csv(
  summary_table,
  "results/summary_statistics.csv"
)

# ==========================================
# 11. GRAPH 1: TOTAL CO2 EMISSIONS
# ==========================================

library(ggplot2)

graph1 <- ggplot(indonesia, aes(x = year, y = co2)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 2, color = "darkblue") +
  labs(
    title = "Annual CO2 Emissions in Indonesia (1990–2024)",
    x = "Year",
    y = "CO2 Emissions (Million Tonnes)"
  ) +
  theme_minimal()

print(graph1)

# Save graph
ggsave(
  "figures/total_co2_emissions.png",
  plot = graph1,
  width = 10,
  height = 6,
  dpi = 300
)

# ==========================================
# 12. GRAPH 2: CO2 EMISSIONS PER CAPITA
# ==========================================

graph2 <- ggplot(indonesia, aes(x = year, y = co2_per_capita)) +
  geom_line(linewidth = 1, color = "darkgreen") +
  geom_point(size = 2, color = "forestgreen") +
  labs(
    title = "Annual CO2 Emissions Per Capita in Indonesia (1990–2024)",
    x = "Year",
    y = "CO2 Emissions Per Capita (Tonnes/Person)"
  ) +
  theme_minimal()

print(graph2)

# Save graph
ggsave(
  "figures/co2_per_capita.png",
  plot = graph2,
  width = 10,
  height = 6,
  dpi = 300
)

# ==========================================
# 13. ANALYSIS OF EMISSION CHANGES
# ==========================================

# Arrange data by year
indonesia <- indonesia %>%
  arrange(year)

# Calculate annual changes
annual_changes <- indonesia %>%
  mutate(
    CO2_Change = co2 - lag(co2),
    CO2_Percent_Change = (co2 - lag(co2)) / lag(co2) * 100,
    Per_Capita_Change = co2_per_capita - lag(co2_per_capita)
  )

# Display annual changes
print(annual_changes)

# Save annual changes
write_csv(
  annual_changes,
  "results/annual_emission_changes.csv"
)

# ==========================================
# 14. KEY FINDINGS
# ==========================================

key_findings <- indonesia %>%
  summarise(
    Starting_CO2 = first(co2),
    Ending_CO2 = last(co2),
    Total_Change = last(co2) - first(co2),
    Percentage_Change =
      ((last(co2) - first(co2)) / first(co2)) * 100,
    Starting_Per_Capita = first(co2_per_capita),
    Ending_Per_Capita = last(co2_per_capita),
    Highest_Emission_Year = year[which.max(co2)],
    Lowest_Emission_Year = year[which.min(co2)]
  )

print(key_findings)

write_csv(
  key_findings,
  "results/key_findings.csv"
)
