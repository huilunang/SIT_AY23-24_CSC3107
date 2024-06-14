## -----------------------------------------------------------------------------
#| label: required-packages
#| echo: true
#| message: false
#| warning: false

library(ggplot2)
library(ggrepel)
library(ggtext)
library(htmltools)
library(rnaturalearth)
library(sf)
library(tidyverse)


## -----------------------------------------------------------------------------
#| label: dataset-part-e-iii-3
#|
# Read the CSV file
per_capita_ghg <- read.csv("per_capita_ghg_dataset.csv")

# Convert to tibble
per_capita_ghg <- as_tibble(per_capita_ghg)

print(per_capita_ghg)


## -----------------------------------------------------------------------------
#| label: dataset-part-e-ii-2

analysis_ghg <- read.csv("ghg_emission_analysis_dataset.csv")

analysis_ghg <- as_tibble(analysis_ghg)

print(analysis_ghg)


## -----------------------------------------------------------------------------
#| label: dataset-part-e-v-1

population <- read.csv("total_population_1960_2022.csv", skip=4)

population <- as_tibble(population)

print(population)


## -----------------------------------------------------------------------------
# Rename the per capita emissions column
colnames(per_capita_ghg)[colnames(per_capita_ghg) == "Entity"] <- "country"
colnames(per_capita_ghg)[colnames(per_capita_ghg) == "Per.capita.greenhouse.gas.emissions.in.CO..equivalents"] <- "per_capita_ghg_emissions_t"
colnames(per_capita_ghg) <- tolower(colnames(per_capita_ghg))

# Display total GHG emission column as a whole number
# per_capita_ghg$per_capita_ghg_emissions_t <- as.double(per_capita_ghg$per_capita_ghg_emissions_t)

# Remove NAs from per capita emissions column
per_capita_ghg_cleaned <- drop_na(per_capita_ghg, per_capita_ghg_emissions_t)

print(per_capita_ghg_cleaned)


## -----------------------------------------------------------------------------
colnames(population)[colnames(population) == "Country.Name"] <- "country"
colnames(population)[colnames(population) == "Country.Code"] <- "code"
colnames(population)[colnames(population) == "X2022"] <- "population"

print(population)


## -----------------------------------------------------------------------------
per_capita_ghg_cleaned_2022 <- filter(per_capita_ghg_cleaned, year == 2022)
per_capita_ghg_cleaned_2022 <- mutate(per_capita_ghg_cleaned_2022, code = if_else(country == "European Union (27)", "EU27", code))

print(per_capita_ghg_cleaned_2022)


## -----------------------------------------------------------------------------
analysis_ghg_projected_2030 <- filter(analysis_ghg, grepl("1.5", scenario), indicator == "Equity boundaries (absolute)", year == 2030)

analysis_ghg_projected_2030 <- analysis_ghg_projected_2030 %>%
  select(region, value) %>%
  mutate(value = value * 1000000) %>%
  rename(projected_ghg_emissions_t = value)

print(analysis_ghg_projected_2030)


## -----------------------------------------------------------------------------
population_cleaned_2022 <- population %>%
  select(code, population)

print(population_cleaned_2022)


## -----------------------------------------------------------------------------
ghg_2022 <-
  left_join(per_capita_ghg_cleaned_2022, population_cleaned_2022, by = "code") |>
  select(country, code, per_capita_ghg_emissions_t, population)

print(ghg_2022)


## -----------------------------------------------------------------------------
ghg_2022_projected_2030 <- left_join(ghg_2022, analysis_ghg_projected_2030, by = c("code" = "region"))
ghg_2022_projected_2030 <-
  mutate(ghg_2022_projected_2030, country = case_when(
    country == "United States" ~ "United States of America",
    TRUE ~ country
  )) |>
  mutate(pc_projected_ghg_emissions_t = projected_ghg_emissions_t / population) |>
  select(country, code, per_capita_ghg_emissions_t, population, pc_projected_ghg_emissions_t)

print(ghg_2022_projected_2030)


## -----------------------------------------------------------------------------
# Get world map data
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Merge emissions data with world map
world <- left_join(world, ghg_2022_projected_2030, by = c("name" = "country"))

# Sort world data by per capita emissions
world_pc <- world %>%
  arrange(desc(per_capita_ghg_emissions_t)) %>%
  mutate(ranking = row_number())

# Add ranking and labels
world_pc$label <- lapply(
  sprintf(
    "<strong>%d. %s</strong><br/>%.2f t",
    world_pc$ranking, 
    world_pc$iso_a3_eh, 
    world_pc$per_capita_ghg_emissions_t
  ), 
  HTML
)

# Create simplified dataframe
world_simplified <- world_pc %>%
  select(
    ranking, 
    name,
    code,
    geometry,
    per_capita_ghg_emissions_t, 
    pc_projected_ghg_emissions_t, 
    label, 
  )


## -----------------------------------------------------------------------------
# Individual label offsets
label_offsets <- data.frame(
  country = c("Qatar", "Bahrain", "Brunei", "Kuwait", "Trinidad and Tobago", 
              "United Arab Emirates", "Oman", "Mongolia", "Saudi Arabia", "Australia"),
  offset_x = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  offset_y = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)

# Merge offsets data
world_simplified <- left_join(world_simplified, label_offsets, by = c("name" = "country"))

# Extract centroid coordinates to offset labels
world_simplified <- world_simplified %>%
  mutate(centroid = st_centroid(geometry),
         x = st_coordinates(centroid)[,1],
         y = st_coordinates(centroid)[,2])


## -----------------------------------------------------------------------------
#| fig.height: 7
#| fig.width: 9

# Set colour pallete
ylOrRd <- c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026")

# Set seed to reproduce same settings
set.seed(1234567890)

# Create choropleth map
ggplot(data = world_simplified) +
  annotate(
    "rect", 
    xmin = -180, xmax = 180, ymin = -90, ymax = 90, 
    fill = "lightblue",  
    color = NA
  ) +
  geom_sf(
    aes(fill = per_capita_ghg_emissions_t), 
    color = "black", 
    size = 0.2
  ) +
  scale_fill_gradientn(
    colors = ylOrRd, 
    name = "C02 Tonnes(t) equivalent"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    text = element_text(size = 12),
    plot.title = element_markdown(),
    plot.subtitle = element_markdown()
  ) +
  geom_label_repel(
    data = subset(world_simplified, ranking <= 10),
    aes(
      x = x + offset_x,
      y = y + offset_y,
      label = sprintf("%d. %s\n%.2f t", ranking, code, per_capita_ghg_emissions_t),
      fontface = "bold"
    ),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.5,
    max.overlaps = Inf,
    seed = 1234567890
  ) +
  geom_point(
    data = subset(world_simplified, ranking <= 10),
    aes(
      x = x,
      y = y,
      size = pc_projected_ghg_emissions_t
    ),
    color = ylOrRd[9],
    alpha = 0.5
  ) +
  scale_size(range = c(5, 15), name = "Projected Reduction") +
  guides(
    fill = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,  
      label.position = "bottom" 
    )
  ) +
  labs(
    title = "<b>Per Capita Greenhouse Gas Emissions in 2022</b>", 
    subtitle = "<b>Top 10 Emitters</b>",
    caption = "Source: Our World in Data",
    x = NULL,
    y = NULL
  )

