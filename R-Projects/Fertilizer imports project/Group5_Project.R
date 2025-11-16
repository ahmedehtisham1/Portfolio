## Group Project
## Group 5
## Members: Ali Danish Riza (24110154) 
##          

## Setting Directory -----

getwd()
setwd("/Users/ahmedehtisham/Downloads")

## Packages -----

library(dplyr)
library(ggplot2)
library(dslabs)
library(ggthemes)
library(ggrepel)
library(tidyr)
library(stringr)
library(NbClust)
library(factoextra)
library(cluster)
library(viridis)
library(maps)
library(forecast)
library(tseries)
library(rpart)
library(rpart.plot)

## Importing CSV files -----

df_fert <- read.csv("Pak_Fert_Import.csv", header = TRUE, stringsAsFactors = FALSE, colClasses = c(PCT = "character"))   # Used colClasses = c(PCT = "character") to avoid trailing 0s from being removed
View(df_fert)
str(df_fert)


## A. Data Cleaning -----

# 1. Data Selection and shortlisting

columns <- c("PCT", "ITEM_DESC", "ORIGIN","IMPORTER_NAME", "CONSIGNERS.NAME", "ASSVALD", "QTY.KG", "CASH_DATE")

imports <- df_fert %>% 
  select(columns) %>%                                                                                                           # Selected shortlisted columns
  mutate(QTY.MT = QTY.KG/1000) %>%                                                                                              # Added quantity by metric tons column
  rename(HSCODE = PCT, ASSDVAL = ASSVALD, IMPORTER.NAME = IMPORTER_NAME, ITEM.DESC = ITEM_DESC, CASH.DATE = CASH_DATE) %>%      # Renamed columns for easier interpretation
  mutate(PRICE.PER.MT = ASSDVAL/QTY.MT) %>%                                                                                     # Added price per metric ton column
  select(-QTY.KG)                                                                                                               # Removed quantity by kgs column

View(imports)
str(imports)


# 2. Convert Date column to Date format

imports$CASH.DATE <- as.Date(imports$CASH.DATE, format = "%d-%b-%y")


# 3. Filtering data based on HS Codes

imports <- imports %>%                
  mutate(HSCODE = str_trim(HSCODE))
HSC.List <- sort(unique(imports$HSCODE), decreasing = FALSE)
HSC.List <- as.data.frame(HSC.List)
View(HSC.List)

imports <- imports %>%
  mutate(HSCODE = case_when(
    HSCODE == "3102.1" ~ "3102.1000", # Corrected values where trailing 0s had been removed
    HSCODE == "3102.21" ~ "3102.2100",
    HSCODE == "3102.3" ~ "3102.3000", 
    HSCODE == "3102.4" ~ "3102.4000",
    HSCODE == "3102.6" ~ "3102.6000",
    HSCODE == "3102.9" ~ "3102.9000",
    HSCODE == "3104.2" ~ "3104.2000",
    HSCODE == "3104.3" ~ "3104.3000",
    HSCODE == "3104.9" ~ "3104.9000",
    HSCODE == "3105.1" ~ "3105.1000",
    HSCODE == "3105.2" ~ "3105.2000",
    HSCODE == "3105.3" ~ "3105.3000",
    HSCODE == "3105.4" ~ "3105.4000",
    HSCODE == "3105.59" ~ "3105.5900",
    HSCODE == "3105.9" ~ "3105.9000",
    TRUE ~ HSCODE
  ))

Hscodes <- c("3102.1000",             # Removed irrelevant products such as 3102.3000 which is Ammonium Nitrate an explosive not a fertilizer
             "3102.2100", 
             "3102.4000", 
             "3102.6000", 
             "3102.9000", 
             "3104.2000", 
             "3104.3000", 
             "3104.9000",
             "3105.1000",
             "3105.2000", 
             "3105.3000", 
             "3105.4000", 
             "3105.5100", 
             "3105.5900",
             "3105.9000")

imports <- imports %>%
  filter(HSCODE %in% Hscodes)


# 4. Standardizing Item descriptions based on HS Codes

imports <- imports %>%
  mutate(ITEM.DESC = case_when(
    HSCODE == "3102.1000" ~ "Urea",
    HSCODE == "3102.2100" ~ "Ammonium Sulphate",
    HSCODE == "3102.4000" ~ "Speciality Fertilizer",
    HSCODE == "3102.6000" ~ "Speciality Fertilizer",
    HSCODE == "3102.9000" ~ "Speciality Fertilizer",
    HSCODE == "3104.2000" ~ "Muriate of Potash",
    HSCODE == "3104.3000" ~ "Sulphate of Potash",
    HSCODE == "3104.9000" ~ "Muriate of Potash",
    HSCODE == "3105.1000" ~ "Speciality Fertilizer",
    HSCODE == "3105.2000" ~ "Nitrate of Potash",
    HSCODE == "3105.3000" ~ "Di-Ammonium Phosphate",
    HSCODE == "3105.4000" ~ "Mono-Ammonium Phosphate",
    HSCODE == "3105.5100" ~ "Speciality Fertilizer",
    HSCODE == "3105.5900" ~ "Urea Phosphate",
    HSCODE == "3105.9000" ~ "Speciality Fertilizer",
    TRUE ~ ITEM.DESC
  ))

View(imports)

Hscodes <- c("3102.1000",             # Removed speciality fertilizers as are not used entirely by farmers
             "3102.2100", 
             "3104.2000", 
             "3104.3000", 
             "3104.9000",
             "3105.2000", 
             "3105.3000", 
             "3105.4000", 
             "3105.5900")

imports <- imports %>%
  filter(HSCODE %in% Hscodes)


# 5. Converting variable classes

str(imports)

imports$HSCODE <- as.factor(imports$HSCODE)
imports$ITEM.DESC <- as.factor(imports$ITEM.DESC)
imports$ORIGIN <- as.factor(imports$ORIGIN)
imports$IMPORTER.NAME <- as.factor(imports$IMPORTER.NAME)
imports$CONSIGNERS.NAME <- as.factor(imports$CONSIGNERS.NAME)
imports$HSCODE <- as.factor(imports$HSCODE)

str(imports)

# 6. Checking for NA Values

table(is.na(imports))


## 7. Shortening country names

imports <- imports %>%
  mutate(ORIGIN = str_trim(ORIGIN))
sort(unique(imports$ORIGIN), decreasing = FALSE)

imports <- imports %>%
  mutate(ORIGIN = case_when(
    ORIGIN == "LAO PEOPLES DEMOCRATIC REPUBL" ~ "LAO",
    ORIGIN == "UNITED ARAB EMIRATES" ~ "UAE",
    ORIGIN == "IRAN (ISLAMIC REPUBLIC OF)" ~ "IRAN",
    ORIGIN == "GERMAN FEDR REPUBLIC" ~ "GERMANY",
    TRUE ~ ORIGIN))

imports$ORIGIN <- as.factor(imports$ORIGIN)

# 8. Removing values below 18 tons 

imports <- imports %>%                # A single container has 18 tons of fertilizer. Entries below that are personal use samples
  filter(QTY.MT > 18)
str(imports)
View(imports)


## B. Exploratory Plots -----


# 1. Which fertilizers are imported mostly from certain countries?

ggplot(imports, aes(x = ORIGIN, fill = ITEM.DESC)) +
  geom_bar(position = "stack", stat = "count", width = 0.7) +
  scale_fill_viridis_d() +
  labs(title = "Most Imported Fertilizers by Country",
       x = "Origin Country",
       y = "Import Transactions Count",
       fill = "Fertilizers") +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))


# 2. Are there any visible trends in total fertilizer import quantities over the years

#         i) Composite

ggplot(imports, aes(x = CASH.DATE, y = QTY.MT, color = ITEM.DESC, group = 1)) +
  geom_line() +
  labs(title = "Total Quantities over the Years",
       x = "LC Dates",
       y = "Quantity",
       color = "Fertilizer Type") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

imports <- imports %>%                # Removing values after 2022 are there is a gap in the recorded data
  filter(CASH.DATE < "2023-01-01")

#         ii) Product-wise

ggplot(imports, aes(x = CASH.DATE, y = QTY.MT, color = ITEM.DESC, group = 1)) +
  geom_line() +
  labs(title = "Individual Quantities over the Years",
       x = "LC Dates",
       y = "Quantity", 
       color = "Fertilizer Type") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5)) +
  facet_grid(~ITEM.DESC)


# 3. Do Prices change over time?

#         i) Composite

ggplot(imports, aes(x = CASH.DATE, y = PRICE.PER.MT, color = ITEM.DESC, group = 1)) +
  geom_line() +
  labs(title = "Price Changes over Years (Composite)",
       x = "LC Dates",
       y = "Price",
       color = "Fertilizer Type") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

#         ii) Product-wise

ggplot(imports, aes(x = CASH.DATE, y = PRICE.PER.MT, color = ITEM.DESC, group = 1)) +
  geom_line() +
  labs(title = "Price Changes over Years (Product-wise)",
       x = "LC Dates",
       y = "Price",
       color = "Fertilizer Type") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(~ITEM.DESC)


# 4. How Prices Impact Import Quantities ?

#         i) Composite

ggplot(imports, aes(x = QTY.MT , y = PRICE.PER.MT, color = ITEM.DESC, group = 1)) +
  geom_line() +
  labs(title = "How Prices impact quantities (Composite)",
       x = "Import Quantities",
       y = "Price",
       color = "Fertilizer Type") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

#         ii) Product-wise

ggplot(imports, aes(x = QTY.MT , y = PRICE.PER.MT, color = ITEM.DESC, group = 1)) +
  geom_line() +
  labs(title = "How Prices impact quantities (Product-wise)",
       x = "Import Quantities",
       y = "Price",
       color = "Fertilizer Type") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(~ITEM.DESC)


# 5. Does product-wise linear regression show any recognizable patterns in the data?

ggplot(imports, aes(x = CASH.DATE, y = QTY.MT, color = ITEM.DESC)) +
  geom_point() +  # Use points to plot the actual data
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
  labs(title = "Linear regression (Product-wise)",
       x = "Dates",
       y = "Imported Quantities",
       color = "Fertilizer Type") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ITEM.DESC, scales = "free_y")



## C. Clustering -----

# 1. Variable selection

str(imports)

dist_matrix_data <- imports %>%       # Excluded date and 
  select(ORIGIN, ITEM.DESC, QTY.MT, IMPORTER.NAME, CONSIGNERS.NAME, ASSDVAL, QTY.MT) 


# 2. Calculating Distance Matrix

dist_matrix <- daisy(dist_matrix_data, metric = "gower")


# 3. Hierarchical Clustering

#         i) Testing Linkages

hc.c <- hclust(dist_matrix, method = "complete") # Creates a better interpretable deprogram 
plot(hc.c, hang = -1, main = "Complete linkage Dendrogram", cex.axis = 0.7)

hc.a <- hclust(dist_matrix, method = "average")
plot(hc.c, hang = -1, main = "Average linkage Dendrogram", cex.axis = 0.7)

summary(hc.c)

#        ii) Testing Number of Clusters

#             a) Creating an Elbow Diagram

fviz_nbclust(dist_matrix_data, FUN = hcut, method = "wss")

#             b) Testing with 2 clusters

Clusters2 <- cutree(hc.c,k=2)
Clusters2
table(Clusters2)

rect.hclust(hc.c, k=2, border = "red")

dataframe <- data.frame(dist_matrix_data, Clusters2)

View(dataframe)

Clustered_data_Origin <- dataframe %>% 
  group_by(Clusters2, ORIGIN) %>% 
  summarise(Count = n())

View(Clustered_data_Origin)

Clustered_data_Products <- dataframe %>% 
  group_by(Clusters2, ITEM.DESC) %>% 
  summarise(Count = n())

View(Clustered_data_Products)

#             c) Testing with 3 clusters

Cluster3 <- cutree(hc.c,k=3)
Cluster3
table(Cluster3)

dataframe2 <- data.frame(dist_matrix_data, Cluster3)

View(dataframe2)

Clustered_data_Origin <- dataframe %>% 
  group_by(Clusters2, ORIGIN) %>% 
  summarise(Count = n())

View(Clustered_data_Origin)

Clustered_data_Products <- dataframe %>% 
  group_by(Clusters2, ITEM.DESC) %>% 
  summarise(Count = n())

View(Clustered_data_Products)

#           d) Plotting silhouettes

plot(silhouette(cutree(hc.c, k = 2), dist_matrix))
plot(silhouette(cutree(hc.c, k = 3), dist_matrix))


# 4. Finalized Deprogram

hc.f <- hclust(dist_matrix, method = "complete")
plot(hc.f, hang = -1, main = "Complete linkage Dendrogram")

Clusters.f <- cutree(hc.f,k=2)
Clusters.f
table(Clusters.f)

rect.hclust(hc.f, k=2, border = "red")

dataframe3 <- data.frame(dist_matrix_data, Clusters.f)

# 5. Analysis

Clustered_data_Origin <- dataframe3 %>% 
  group_by(Clusters.f, ORIGIN) %>% 
  summarise(Count = n())

View(Clustered_data_Origin)

Clustered_data_Products <- dataframe3 %>% 
  group_by(Clusters.f, ITEM.DESC) %>% 
  summarise(Count = n())

View(Clustered_data_Products)

dataframe3 %>% 
  group_by(Clusters.f) %>% 
  summarize(Mean.QTY.MT = mean(QTY.MT), Mean.ASSDVAL = mean(ASSDVAL))

dataframe3 %>% 
  group_by(Clusters.f) %>% 
  summarize(Max.QTY.MT = max(QTY.MT), Max.ASSDVAL = max(ASSDVAL))

dataframe3 %>% 
  group_by(Clusters.f) %>% 
  summarize(Min.QTY.MT = min(QTY.MT), Min.ASSDVAL = min(ASSDVAL))

# Compute Gower distance
dist_matrix <- daisy(dist_matrix_data, metric = "gower")

# Perform hierarchical clustering
hclust_res <- hclust(dist_matrix)
# Cut the dendrogram to get clusters
clusters <- cutree(hclust_res, k = 5)

# Add the clusters to the original data
clustered_data <- cbind(imports, Cluster = clusters)

# Visualization - Box plot of QTY.MT by cluster
ggplot(clustered_data, aes(x = as.factor(Cluster), y = QTY.MT)) +
  geom_boxplot() +
  labs(title = "Box Plot of QTY.MT by Cluster") +
  xlab("Cluster") +
  ylab("QTY.MT")

# Visualization - Bar plot of cluster counts
ggplot(clustered_data, aes(x = as.factor(Cluster))) +
  geom_bar() +
  labs(title = "Bar Plot of Cluster Counts") +
  xlab("Cluster") +
  ylab("Count")

## D. Regression -----

str(imports)

# 1. Model 1

model <- lm(QTY.MT ~ ITEM.DESC + ASSDVAL + ORIGIN + CASH.DATE, data = imports)
summary(model)

# 2. Model 2

model2 <- lm(QTY.MT ~ ITEM.DESC + PRICE.PER.MT + CASH.DATE + ASSDVAL, data = imports)
summary(model2)

# 3. Model 3 (Best Fit)

total.qt <- imports %>% 
  group_by(ITEM.DESC) %>% 
  summarise(SUM = sum(QTY.MT))

View(total.qt)

relevant <- c("Di-Ammonium Phosphate", "Nitrate of Potash", "Sulphate of Potash", "Ammonium Sulphate")

rel.prods <- imports %>% 
  filter(ITEM.DESC %in% relevant)
unique(rel.prods$ITEM.DESC)

model3 <- lm(QTY.MT ~ ITEM.DESC + PRICE.PER.MT + CASH.DATE + ASSDVAL, data = rel.prods)
summary(model3)

# 4. Model 4

model4 <- lm(ASSDVAL ~ ITEM.DESC + CASH.DATE + PRICE.PER.MT, data = imports)
summary(model4)

ggplot(imports, aes(x = CASH.DATE, y = QTY.MT, color = ITEM.DESC, group = 1)) +
  geom_line() +
  labs(title = "Quantities against Dates",
       x = "Dates",
       y = "Quantity") +
  theme_minimal() + 
  abline(model5)

# 5. Model 5

# QTY v Cash-DATE + ABLINE

model5 <- lm(QTY.MT ~ CASH.DATE, data = imports)
summary(model5)

ggplot(imports, aes(x = CASH.DATE, y = QTY.MT, color = ITEM.DESC, group = 1)) +
  geom_line() +
  labs(title = "Quantities against Dates",
       x = "Dates",
       y = "Quantity") +
  theme_minimal() + 
  abline(model5)

# 6. Model 6
# QTY v PRICE + ABLINE

model6 <- lm(QTY.MT ~ PRICE.PER.MT, data = imports)
summary(model6)

ggplot(imports, aes(x = PRICE.PER.MT, y = QTY.MT, color = ITEM.DESC, group = 1)) +
  geom_point() +
  labs(title = "Quantities against Price",
       x = "Price",
       y = "Quantity") +
  theme_minimal() + 
  abline(model6)

# 7. Model 7
# PRICE v DATE + ABLINE

model7 <- lm(PRICE.PER.MT ~ CASH.DATE, data = imports)
summary(model7)

ggplot(imports, aes(x = CASH.DATE, y = PRICE.PER.MT, color = ITEM.DESC, group = 1)) +
  geom_line() +
  labs(title = "Price against Dates",
       x = "Dates",
       y = "Price") +
  theme_minimal() + 
  abline(model7)



# Decision Tree ####
# Select a target variable ('QTY.MT') and predictors
target_variable <- "QTY.MT"
predictor_variables <- c("PRICE.PER.MT", "ASSDVAL", "CASH.DATE")



# Build decision tree for Cluster 1
tree_cluster1 <- rpart(formula(paste(target_variable, "~", paste(predictor_variables, collapse = "+"))),
                       data = filter(clustered_data, Cluster == 1))

# Visualize decision tree for Cluster 1
rpart.plot(tree_cluster1)


# Time Series analysis ####


# Convert the date to a Date object 
imports$CASH.DATE <- as.Date(imports$CASH.DATE, format = "%d-%b-%y")

# Aggregate data by month for time series analysis
monthly_data <- aggregate(QTY.MT ~ format(CASH.DATE, "%Y-%m"), data=imports, sum)
colnames(monthly_data) <- c("Month", "TotalQuantity")

# Convert to time series object
ts_data <- ts(monthly_data$TotalQuantity, start=c(as.numeric(substr(min(monthly_data$Month), 1, 4)), as.numeric(substr(min(monthly_data$Month), 6, 7))), frequency=12)

# Decompose the time series to observe trends and seasonality
decomposed_ts <- decompose(ts_data)
plot(decomposed_ts)


## Mapping ####



# Aggregate data by country
country_data <- imports %>%
  group_by(ORIGIN) %>%
  summarise(TotalQuantity = sum(QTY.MT), .groups = 'drop') %>%
  filter(!is.na(ORIGIN)) # Ensure that there are no NA values in the ORIGIN column

# Load world map data for comparison
world_map <- map_data("world")
map_countries <- unique(world_map$region)

# Compare the country names and print mismatches
data_countries <- unique(country_data$ORIGIN)
mismatched_countries <- setdiff(data_countries, map_countries)

# Print mismatched country names for manual correction
print(mismatched_countries)

## Create a mapping for mismatched country names
country_corrections <- c("AUSTRALIA" = "Australia",
                         "CHINA" = "China",
                         "BELARUS" = "Belarus",
                         "SINGAPORE" = "Singapore",
                         "TAIWAN" = "Taiwan",
                         "SPAIN" = "Spain",
                         "TURKEY" = "Turkey",
                         "BELGIUM" = "Belgium",
                         "KOREA (SOUTH)" = "South Korea",
                         "HONG KONG CHINA" = "China",
                         "NETHERLANDS" = "Netherlands",
                         "EGYPT" = "Egypt",
                         "UAE" = "United Arab Emirates",
                         "GERMANY" = "Germany",
                         "JORDAN" = "Jordan",
                         "RUSSIAN FEDERATION" = "Russia",
                         "SWEDEN" = "Sweden",
                         "EUROPIEN UNION" = NA,  # No direct match available
                         "UNITED STATES" = "USA",
                         "SAUDI ARABIA" = "Saudi Arabia",
                         "UNITED KINGDOM" = "UK",
                         "ITALY" = "Italy",
                         "VIET NAM" = "Vietnam",
                         "TUNISIA" = "Tunisia",
                         "MOROCCO" = "Morocco",
                         "INDONESIA" = "Indonesia",
                         "UZBEKISTAN" = "Uzbekistan",
                         "CANADA" = "Canada",
                         "LITHUANIA" = "Lithuania",
                         "FRANCE" = "France",
                         "LAO" = "Laos",
                         "IRAN" = "Iran",
                         "MADAGASCAR" = "Madagascar",
                         "MALAYSIA" = "Malaysia",
                         "OMAN" = "Oman",
                         "SERBIA" = "Serbia",
                         "CAYMAN ISLANDS" = "Cayman Islands",
                         "VIET NAM" = "Vietnam")


# Apply corrections
country_data$ORIGIN <- ifelse(country_data$ORIGIN %in% names(country_corrections), 
                              country_corrections[country_data$ORIGIN], 
                              country_data$ORIGIN)

# Merge with world map data
merged_data <- merge(world_map, country_data, by.x = "region", by.y = "ORIGIN", all.x = TRUE)

# Plotting
ggplot(merged_data, aes(x = long, y = lat, group = group, fill = TotalQuantity)) + 
  geom_polygon(color = "white") +
  scale_fill_continuous(low = "orange", high = "red", na.value = "black", name="Total Quantity (MT)") +
  labs(title = "World Map of Fertilizer Imports") +
  theme_void() + 
  theme(legend.position = "bottom")



# K-mean Clustering ####

# Prepare data for clustering
clustering_data <- imports %>% 
  select(QTY.MT, PRICE.PER.MT) %>% 
  na.omit()

# Scale the data
clustering_data_scaled <- scale(clustering_data)

# K-means clustering
set.seed(123)  # Setting seed for reproducibility
kmeans_result <- kmeans(clustering_data_scaled, centers = 5, nstart = 25)

# Add cluster information to the original data
imports$Cluster <- kmeans_result$cluster

# Visualizing the clusters
ggplot(imports, aes(x = QTY.MT, y = PRICE.PER.MT, color = as.factor(Cluster))) +
  geom_point(alpha = 0.5) +
  labs(title = "K-means Clustering of Fertilizer Imports", 
       x = "Quantity (MT)", 
       y = "Price per Metric Ton", 
       color = "Cluster") +
  theme_minimal()


# Comparative Analysis ####
# Comparative analysis by country
country_analysis <- imports %>% 
  group_by(ORIGIN) %>% 
  summarise(TotalImports = sum(QTY.MT), 
            AvgPricePerMT = mean(PRICE.PER.MT, na.rm = TRUE)) %>% 
  arrange(desc(TotalImports))

# Visualization
ggplot(country_analysis, aes(x = reorder(ORIGIN, TotalImports), y = TotalImports, fill = AvgPricePerMT)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis_c() +
  labs(title = "Comparative Analysis of Fertilizer Imports by Country", 
       x = "Country", 
       y = "Total Imports (MT)", 
       fill = "Average Price Per MT") +
  theme_minimal()




