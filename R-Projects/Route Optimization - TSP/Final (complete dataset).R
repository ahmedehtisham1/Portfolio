### Sequential method ####




install.packages(c("ggmap", "dplyr"))

install.packages(c("ggmap", "dplyr", "readxl"))

install.packages("geosphere")

library(ggmap)
library(dplyr)
library(readxl)

Sys.setenv("GEOCODE_API_KEY" = "AIzaSyD8aN9R94D_FecjkhFINmTeUMpOk0I-eSk")
register_google("AIzaSyD8aN9R94D_FecjkhFINmTeUMpOk0I-eSk")

# Read the Excel file

setwd("/Users/ahmedehtisham/Desktop/LUMS/DISC 335/Project")

excel_data <- read_excel("LHE&ISB OCT.xlsx", 
                         sheet = "Finance (Original)")


#Data Processing
table(excel_data$`Destination City`)
table(excel_data$`Current Status`)
table(excel_data$`Current Status Verified`)
table(excel_data$`Area Type`)

# Filter for "Delivered" in "Current Status" and "Lahore" in "Destination City"
filtered_data <- excel_data %>%
  filter(`Current Status` == "Delivered") %>% 
  filter(`Destination City` %in% "Islamabad") %>% 
  filter(`Origin City` %in% "Karachi") 

# Get the total count
total_count <- nrow(filtered_data)
print(total_count)

# Specify the column with addresses
address_column <- "Address"

# Geocode addresses to get coordinates
geocoded_data <- filtered_data %>%
  mutate(location = geocode(get(address_column))) 


# Checking for missing values
table(is.na(geocoded_data))
colSums(is.na(geocoded_data$location)) #63 

# Print the resulting data frame with coordinates
print(geocoded_data)


# Distance 
library(ggmap)
library(dplyr)
library(geosphere)


# Assuming your data is already read and geocoded
# Assuming 'geocoded_data' is your data frame with the necessary columns

# Warehouse coordinates
warehouse_coords <- c(73.06793488465854, 33.653789448775996)

# Function to calculate the distance matrix for a group of deliveries
calc_distance_matrix <- function(deliveries, warehouse_coords) {
  # Ensure warehouse_coords is a one-row matrix with two columns
  warehouse_matrix <- matrix(warehouse_coords, nrow = 1, byrow = TRUE)
  
  # Extract coordinates from deliveries and convert them into a matrix
  delivery_coords <- as.matrix(deliveries[c("lon", "lat")])
  
  # Combine warehouse coordinates with delivery coordinates into one matrix
  coords <- rbind(warehouse_matrix, delivery_coords, warehouse_matrix)
  
  # Calculate the distance matrix using the combined coordinates
  dist_matrix <- as.data.frame(distm(coords, coords, fun = distHaversine))
  
  # Add meaningful column names to the distance matrix
  colnames(dist_matrix) <- c("Warehouse_Start", paste0("Delivery_", seq_len(nrow(deliveries))), "Warehouse_End")
  
  return(dist_matrix)
}

# Initialize a data frame to store total distances for each rider
total_distances <- data.frame(rider = character(), distance = numeric())



# Assuming you have a column 'Address' in your dataframe which contains the addresses to geocode.
# This will add 'lat' and 'lon' columns to your dataframe.
geocoded_data <- geocoded_data %>%
  rowwise() %>%
  mutate(geocode_result = list(geocode(Address, output = "latlona", source = "google")),
         lat = geocode_result$lat,
         lon = geocode_result$lon) %>%
  select(-geocode_result)  # remove the list column to clean up the dataframe




# Function to calculate the distance matrix for a group of deliveries
calc_distance_matrix <- function(deliveries, warehouse_coords) {
  # Ensure warehouse_coords is a one-row matrix with two columns
  warehouse_matrix <- matrix(warehouse_coords, nrow = 1, byrow = TRUE)
  
  # Extract coordinates from deliveries and convert them into a matrix
  delivery_coords <- as.matrix(deliveries[c("lon", "lat")])
  
  # Combine warehouse coordinates with delivery coordinates into one matrix
  coords <- rbind(warehouse_matrix, delivery_coords, warehouse_matrix)
  
  # Calculate the distance matrix using the combined coordinates
  dist_matrix <- as.data.frame(distm(coords, coords, fun = distHaversine))
  
  # Add meaningful column names to the distance matrix
  colnames(dist_matrix) <- c("Warehouse_Start", paste0("Delivery_", seq_len(nrow(deliveries))), "Warehouse_End")
  
  return(dist_matrix)
}


# Calculate distance matrices and routes for each rider
unique_riders <- unique(geocoded_data$`Dispatch Rider Task`)
total_distances <- data.frame(rider = character(), distance = numeric())

for (rider_task in unique_riders) {
  rider_data <- geocoded_data %>%
    filter(`Dispatch Rider Task` == rider_task)
  
  # Skip if no deliveries are assigned to this rider
  if (nrow(rider_data) == 0) {
    next
  }
  
  # Calculate distance matrix for rider
  dist_matrix <- calc_distance_matrix(rider_data, warehouse_coords)
  
  # Assuming a sequential visit to all points
  rider_route <- seq_len(nrow(rider_data) + 1)
  
  # Calculate total distance for rider's route
  rider_distance <- sum(dist_matrix[rider_route, rider_route + 1])
  
  # Store the total distance for the rider
  total_distances <- rbind(total_distances, data.frame(rider = rider_task, distance = rider_distance))
}

# Drop NA values from total_distances
total_distances <- total_distances %>% filter(!is.na(distance))

# Calculate collective distance for all riders, excluding NA values
collective_distance <- sum(total_distances$distance, na.rm = TRUE)

# Print out the total distances for each rider and collectively
print(total_distances)
print(paste("Collective distance for all riders:", collective_distance))





library(writexl)

# Initialize a data frame to store all routes sequentially
all_routes <- data.frame(
  Rider_Code = character(),
  Rider_Name = character(),
  Lat = numeric(),
  Lon = numeric(),
  Address = character(),
  Total_Distance = numeric(), # Add column for total distance
  stringsAsFactors = FALSE # To keep character columns as characters
)

# ... (rest of your existing code)

# Calculate routes for each rider and combine them into one data frame
for (rider_task in unique(geocoded_data$`Dispatch Rider Task`)) {
  rider_data <- geocoded_data %>%
    filter(`Dispatch Rider Task` == rider_task)
  
  # Fetch the rider's name
  rider_name <- unique(rider_data$`Dispatch Rider`)[1]
  
  # Fetch total distance for the rider
  rider_total_distance <- total_distances %>%
    filter(rider == rider_task) %>%
    pull(distance) # Extracts the 'distance' column value
  
  # If no distance found, set it to NA or 0
  if (length(rider_total_distance) == 0) {
    rider_total_distance <- NA
  }
  
  # Add rider code, name, and total distance to each row
  rider_data$Rider_Code <- rider_task
  rider_data$Rider_Name <- rider_name
  rider_data$Total_Distance <- rider_total_distance
  
  # Combine this rider's data with the rest
  all_routes <- rbind(all_routes, rider_data)
}

# Write the combined routes to an Excel file
write_xlsx(list("All_Routes" = all_routes), path = "All_Rider_Routes.xlsx")

# Print out the file path
print("All rider routes have been written to All_Rider_Routes.xlsx")







