# Required library installations
install.packages(c("ggmap", "dplyr", "readxl", "geosphere", "writexl", "TSP"))

# Load libraries
library(ggmap)
library(dplyr)
library(readxl)
library(geosphere)
library(TSP)
library(writexl)

# Set API Key and read the Excel file
Sys.setenv("GEOCODE_API_KEY" = "AIzaSyD8aN9R94D_FecjkhFINmTeUMpOk0I-eSk")
register_google("AIzaSyD8aN9R94D_FecjkhFINmTeUMpOk0I-eSk")
setwd("/Users/ahmedehtisham/Desktop/LUMS/DISC 335/Project")
excel_data <- read_excel("LHE&ISB OCT.xlsx", sheet = "Finance (Original)")

# Data Processing and Geocoding
filtered_data <- excel_data %>%
  filter(`Current Status` == "Delivered", `Destination City` == "Islamabad", `Origin City` == "Karachi") 

geocoded_data <- filtered_data %>%
  rowwise() %>%
  mutate(location = geocode(Address, output = "latlona", source = "google"),
         lat = location$lat, lon = location$lon) %>%
  select(-location)

# Warehouse coordinates
warehouse_coords <- c(73.06793488465854, 33.653789448775996)

# Function to calculate the TSP route for a group of deliveries
calc_tsp_route <- function(deliveries, warehouse_coords) {
  # Filter out rows with NA in lat or lon
  deliveries <- deliveries %>% filter(!is.na(lat) & !is.na(lon))
  
  # Combine warehouse and delivery coordinates
  coords <- rbind(warehouse_coords, as.matrix(deliveries[c("lon", "lat")]))
  
  # Create a TSP object
  tsp_obj <- TSP(distm(coords))
  
  # Solve the TSP
  tsp_solution <- solve_TSP(tsp_obj)
  
  # Extract the order of locations from the TSP solution
  ordered_coords <- coords[tsp_solution, ]
  
  return(ordered_coords)
}

# Calculate routes and distances for each rider
total_distances <- data.frame(rider = character(), distance = numeric())
all_routes <- data.frame(
  Rider_Code = character(), Rider_Name = character(),
  Lat = numeric(), Lon = numeric(), Address = character(),
  Total_Distance = numeric(), stringsAsFactors = FALSE
)

for (rider_task in unique(geocoded_data$`Dispatch Rider Task`)) {
  rider_data <- geocoded_data %>% filter(`Dispatch Rider Task` == rider_task)
  if (nrow(rider_data) > 0) {
    tsp_route <- calc_tsp_route(rider_data, warehouse_coords)
    rider_total_distance <- sum(distHaversine(tsp_route, lag(tsp_route, default = tsp_route[1])))
    
    rider_data$Rider_Code <- rider_task
    rider_data$Rider_Name <- unique(rider_data$`Dispatch Rider`)[1]
    rider_data$Total_Distance <- rider_total_distance
    
    total_distances <- rbind(total_distances, data.frame(rider = rider_task, distance = rider_total_distance))
    all_routes <- rbind(all_routes, rider_data)
  }
}

# Write the combined routes to an Excel file
write_xlsx(list("All_Routes" = all_routes), path = "All_Rider_Routes.xlsx")
print("All rider routes have been written to All_Rider_Routes.xlsx")
