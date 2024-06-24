# Load necessary libraries
library(dplyr)
library(ggplot2)
library(sf)
library(tidycensus)
library(lubridate)

# Set API key
options(tigris_use_cache = TRUE)
census_api_key("ff223a3f832673daf9f56bbd4b0f924eea70dd36")

# Fetch median household income by ZIP code area
income_data <- get_acs(
  geography = "zcta",
  variables = "B19013_001",
  year = 2020,
  survey = "acs5"
)

# Simulated data frame of housing transactions
set.seed(123)
housing_data <- data.frame(
  date = seq(as.Date('2020-01-01'), as.Date('2020-12-31'), by = "day"),
  price = runif(366, 300000, 2000000),
  zipcode = sample(c("10025", "10026", "10027"), 366, replace = TRUE),
  longitude = runif(366, -74.010, -73.980),
  latitude = runif(366, 40.75, 40.80)
)

# Convert to sf object for spatial analysis
coordinates <- st_as_sf(housing_data, coords = c("longitude", "latitude"), crs = 4326)

# Clean and prepare data
cleaned_data <- coordinates %>%
  filter(!is.na(price)) %>%
  mutate(month = month(date, label = TRUE))

# Join with income data based on ZIP code
enriched_data <- left_join(cleaned_data, income_data, by = c("zipcode" = "GEOID"))

# Spatial join to calculate proximity to amenities (e.g., subway stations)
# For simplicity, assume a data frame 'subway_stations' exists with similar structure
stations <- data.frame(
  longitude = c(-74.005, -73.990),
  latitude = c(40.77, 40.76),
  station_name = c("Station A", "Station B")
)
stations_sf <- st_as_sf(stations, coords = c("longitude", "latitude"), crs = 4326)
closest_stations <- st_join(enriched_data, stations_sf, join = st_nearest_feature)

# Analysis: Average price by income level and proximity to subway
avg_price_analysis <- closest_stations %>%
  group_by(zipcode, station_name) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = 'drop')

# Visualization: Map of housing prices and subway station proximity
ggplot() +
  geom_sf(data = enriched_data, aes(color = price)) +
  geom_sf(data = stations_sf, shape = 21, fill = "red", size = 5, show.legend = "point") +
  scale_color_viridis_c() +
  labs(title = "Housing Prices and Proximity to Subway Stations",
       color = "Housing Price") +
  theme_minimal()

# Print results
print(avg_price_analysis)
