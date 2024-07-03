## -----------------------------------------------------------------------------
#| label: required-packages1
#| echo: true
#| message: false
#| warning: false

library(tidyverse)


## -----------------------------------------------------------------------------
#| label: dataset-part-e-iii-3

pc_ghg <-
  read.csv("dataset/per_capita_ghg_dataset.csv") |>
  as_tibble()

pc_ghg


## -----------------------------------------------------------------------------
#| label: dataset-part-e-ii-2

analysis_ghg <-
  read.csv("dataset/ghg_emission_analysis_dataset.csv") |>
  as_tibble()

analysis_ghg


## -----------------------------------------------------------------------------
#| label: dataset-part-e-v-1

population <-
  read.csv("dataset/total_population_1960_2022.csv", skip = 4) |>
  as_tibble()

population


## -----------------------------------------------------------------------------
#| label: clean-dataset-i

# Rename columns
colnames(pc_ghg)[colnames(pc_ghg) == "Entity"] <- "country"
colnames(pc_ghg)[colnames(pc_ghg) == "Per.capita.greenhouse.gas.emissions.in.CO..equivalents"] <- "pc_ghg_emissions_t"
colnames(pc_ghg) <- tolower(colnames(pc_ghg))

# Remove NAs from emissions column
pc_ghg_cleaned <- drop_na(pc_ghg, pc_ghg_emissions_t)

pc_ghg_cleaned


## -----------------------------------------------------------------------------
#| label: clean-dataset-ii

colnames(population)[colnames(population) == "Country.Name"] <- "country"
colnames(population)[colnames(population) == "Country.Code"] <- "code"
colnames(population)[colnames(population) == "X2022"] <- "population"

print(population)


## -----------------------------------------------------------------------------
#| label: process-dataset-i

pc_ghg_cleaned_2022 <-
  filter(pc_ghg_cleaned, year == 2022) |>
  mutate(
    code = if_else(country == "European Union (27)", "EU27", code)
  )

pc_ghg_cleaned_2022


## -----------------------------------------------------------------------------
#| label: process-dataset-ii

analysis_ghg_projected_2030 <-
  filter(
    analysis_ghg,
    grepl("1.5", scenario),
    indicator == "Equity boundaries (absolute)",
    year == 2030
  ) |>
  select(region, value) |>
  mutate(value = value * 1000000) |>
  rename(projected_ghg_emissions_t = value)

analysis_ghg_projected_2030


## -----------------------------------------------------------------------------
#| label: process-dataset-iii

population_cleaned_2022 <- select(.data = population, code, population)

population_cleaned_2022


## -----------------------------------------------------------------------------
#| label: process-dataset-iv

ghg_2022 <-
  left_join(pc_ghg_cleaned_2022, population_cleaned_2022, by = "code") |>
  select(country, code, pc_ghg_emissions_t, population)

ghg_2022


## -----------------------------------------------------------------------------
#| label: process-dataset-v

ghg_2022_projected_2030 <- ghg_2022 |>
  left_join(analysis_ghg_projected_2030, by = c("code" = "region")) |>
  mutate(
    country = ifelse(country == "United States", "United States of America", country),
    pc_projected_ghg_emissions_t = projected_ghg_emissions_t / population
  ) |>
  select(country, code, pc_ghg_emissions_t, population, pc_projected_ghg_emissions_t)

ghg_2022_projected_2030

