library(tidyverse)
library(maps)
library(sf)
library(glue)

## 1. Load the datasets
map_data <- read_csv("data_search_volumen_country.csv")
year_data <- read_csv("data_search_volume_year.csv")
country_year_data <- read_csv("data_search_volume_country_year.csv")

## 2. Filter the species of interest
species_selected <- "Thunnus thynnus"
country_selected <- "Spain"


## 3. PLOT 1 - Map: Scaled GSV by country

# Load world map data for plotting
world <- map_data("world")

# Filter data for the target species
species_map_data <- map_data %>% 
  filter(species == species_selected)

# Get list of study countries (countries that appear in your dataset)
study_countries <- unique(map_data$country)

# Create the map plot data by joining world map with your scaled data
map_plot_data <- world %>%
  left_join(species_map_data, by = c("region" = "country")) %>%
  mutate(
    # Create fill_color: use scaled data where available, 
    # -0.1 for study countries with no data (will be gray),
    # NA for countries not in study (will be white)
    fill_color = case_when(
      !is.na(sum_gsv_scaled) ~ sum_gsv_scaled,
      region %in% study_countries ~ -0.1,
      TRUE ~ NA_real_
    )
  )

# Create the map plot
ggplot(map_plot_data, aes(x = long, y = lat, group = group, fill = fill_color)) +
  geom_polygon(color = "black", linewidth = 0.2) +
  scale_fill_distiller(
    palette = "Spectral", 
    name = "Google Search Volume (%)", 
    na.value = "white", 
    values = c(0, 1), 
    limits = c(0, 100)
  ) +
  # Add gray layer for study countries with no data
  geom_polygon(
    data = map_plot_data %>% filter(fill_color == -0.1),
    aes(x = long, y = lat, group = group),
    fill = "lightgray", color = "black", linewidth = 0.2
  ) +
  expand_limits(x = world$long, y = world$lat) +
  coord_sf(xlim = c(-10, 40), ylim = c(30, 50)) +  # Mediterranean region coordinates
  labs(
    x = "Longitude", 
    y = "Latitude"
  ) +
  theme(
    axis.title    = element_blank(), 
    axis.ticks    = element_blank(),
    axis.text     = element_blank(),
    panel.background = element_rect(fill = "white"), 
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.key.width  = unit(40, "point"),
    legend.key.height = unit(10, "point"),
    legend.title      = element_text(hjust = 0.5, size = 11, face = "bold"),
    legend.text       = element_text(size = 10)
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "bottom",
      title.hjust     = 0.5,
      barwidth        = unit(200, "points"),
      barheight       = unit(10, "points")
    )
  )

## 4. PLOT 2 - Bar plot: Scaled GSV by year

# Filter data for the target species
species_yearly_data <- year_data %>% 
  filter(species == species_selected) %>%
  filter(!is.na(mean_gsv_scaled)) %>%
  # Convert year to factor for discrete x-axis labels
  mutate(year = as_factor(year))

# Create the bar plot
species_yearly_data %>%
  ggplot(aes(x = year, y = mean_gsv_scaled)) + 
  geom_bar(stat = "identity", fill = "#00b4d8") +
  labs(
    x = "Year", 
    y = "Google Search Volume (%)"
  ) +
  theme_minimal() +
  theme(
    # panel and grid elements
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "#c4cfcf"),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    
    # text elements
    axis.title = element_text(size = 11, face = "bold", colour = "black"),
    axis.text = element_text(size = 10, colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2, size = 10)
  )

## 5. PLOT 3 - Bar plot: Scaled GSV by year for one country

# Filter data for target species and country
species_country_data <- country_year_data %>% 
  filter(species == species_selected, country == country_selected) %>%
  filter(!is.na(gsv_scaled)) %>%
  # Convert year to factor for discrete x-axis labels
  mutate(year = as_factor(year))

# Create the bar plot
species_country_data %>%
  ggplot(aes(x = year, y = gsv_scaled)) + 
  geom_bar(stat = "identity", fill = "#0077b6") +
  labs(
    title = country_selected,
    x = "Year", 
    y = "Google Search Volume (%)"
  ) + 
  theme_minimal() +
  theme(
    # panel and grid elements
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "#c4cfcf"),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    
    # text elements
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold", colour = "black"),
    axis.title = element_text(size = 11, face = "bold", colour = "black"),
    axis.text = element_text(size = 10, colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2, size = 10)
  )