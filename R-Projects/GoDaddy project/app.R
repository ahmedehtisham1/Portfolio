## Team 5 Final R File 

# Addhyan Popli
# Alex Wang 
# Muhammad Ahmed Ehtisham
# Uddeshya Raj


#### Before Running the Code - Please Read ####

## For this file to run smoothly without any errors, it is recommended to have this file and the raw data file in the 
#  same folder. Raw Data file is referenced as "VF_US_National_FEB24_RawData.xlsx" and the excel sheet name is referenced
#  as "Data". If your data file or sheet is under a different name, it is recommended to change the names in excel 
#  directly to avoid possible errors or mistakes in the code. In the beginning, please (install if not already installed
#  by removing the "#" in the beginning of the install packages section) and load all packages given below.  
#  You may un-collapse the headers on the left to organise the code better. Each section is Labelled. 
#  Set your working directory below the folder which contains these two files in the beginning. 


# Set your working directory here



### Install Packages  ####

# install these packages if you don't have them in your rstudio 

# install.packages("readr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("shiny")
# install.packages("janitor")
# install.packages("forcats")
# install.packages("shinythemes")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("DT")
# install.packages("reshape2")
# install.packages("usmap")
# install.packages("fmsb")
# install.packages("ggridges")
# install.packages("mice")
# install.packages("ggcorrplot")
# install.packages("VIM")
# install.packages("factoextra")
# install.packages("klaR")
# install.packages("cluster")
# install.packages("broom")
# install.packages("pscl")
# install.packages("knitr")
# install.packages("kableExtra")
# install.packages("rpart.plot")
# install.packages("rpart")
# install.packages("caret")
# install.packages("caret")
# install.packages("caret")
# install.packages("caret")
# install.packages("caret")
# install.packages("shinydashboard")
# install.packages("DT")



### Load necessary libraries ####
library(readr)
library(dplyr)
library(ggplot2)
library(shiny)
library(janitor)
library(forcats)
library(shinythemes)
library(tidyverse)
library(tidyr)
library(DT)
library(reshape2)
library(usmap)
library(fmsb) 
library(ggridges)
library(readxl)
library(mice)
library(tidyverse)
library(ggcorrplot)
library(VIM) 
library(patchwork) 
library(scales)
library(cluster)
library(factoextra)
library(klaR)


### Data Wrangling for R Shiny App ####

# Make sure your excel data file is the same name as this down below and stored in the same working directory you set above
df <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Data")


# Keep only those variables required
df <- subset(df, select = c(
  "id", "cid", "q3a", 
  "q5b_1", "q5b_2", "q5b_3", "q5b_4", "q5b_5", "q5b_6", "q5b_7", "q5b_8", "q5b_9", 
  "q5b_10", "q5b_11", "q5b_12", "q5b_13", "q5b_14", "q5b_15", "q5b_16", "q5b_17", "q5b_18", 
  "q5b_19", "q5b_20", "q5b_21", "q5b_22", "q5b_23", "q5b_24", "q5b_25", "q5b_26", "q5b_27", 
  "q5b_28", "q5b_29", "q5b_31","q5b_98",
  "q7", "q11aa", "d1", "d12", 
  "q80_1", "q80_2","q80_3","q80_4",
  "q81_1", "q81_2", "q81_3", "q81_4", "q81_5", "q81_6", "q81_7", "q81_8", "q81_9", "q81_10", 
  "q81_11", "q81_12", "q81_13", "q81_14", "q81_15", "q81_16", "q81_17",
  "q82_1", "q82_2", "q82_3", "q82_4", "q82_5", "q82_6",
  "q83", "q84", "q89", "q93"
))

### Handling missing values ###

# Define values to be considered as missing
missing_values <- c(-8, -9, 98, 99, -8.00, -9.00)

# Functions to count and calculate % of missing
count_missing <- function(x) {
  sum(is.na(x) | x %in% missing_values)
}
missing_percentage <- function(x) {
  round((sum(is.na(x) | x %in% missing_values) / length(x)) * 100, 2)
}

# Summary table for missing
missing_summary <- data.frame(
  Column = colnames(df),
  Missing_Values = sapply(df, count_missing),
  Missing_Percentage = sapply(df, missing_percentage)
)
print(missing_summary)

### Replace Missing Values with "Unavailable", -7 as "Not Applicable" and Converting to appropriate formats ###

df_cleaned <- df %>%
  mutate(cid = as.character(cid))

# Convert Business Size
df_cleaned$q3a <- factor(df_cleaned$q3a,
                         levels = c(1, 2, 3, 4, 5, 6, 7, 98, 99),
                         labels = c("Just Myself", "2-4", "5-9", "10-19", "20-49", 
                                    "50-99", "100+", "Unavailable", "Unavailable"),
                         ordered = TRUE)

# Convert Industry Variables to Binary Factor
industry_vars <- paste0("q5b_", 1:29)
df_cleaned[industry_vars] <- lapply(df_cleaned[industry_vars], factor, levels = c(0, 1, "Unavailable"))

df_cleaned$q5b_31 <- as.factor(df_cleaned$q5b_31)
df_cleaned$q5b_98 <- as.factor(df_cleaned$q5b_98)

# Convert Revenue Expectations
df_cleaned$q7 <- factor(df_cleaned$q7,
                        levels = c(98, 99, 1, 2, 3),
                        labels = c("Unavailable", "Unavailable", "More Negative", 
                                   "No Change", "More Positive"),
                        ordered = TRUE)

# Convert Business Revenue
df_cleaned$q11aa <- factor(df_cleaned$q11aa,
                           levels = c(98, -7, 1, 2, 3, 4, 5, 6, 7, 8),
                           labels = c("Unavailable", "Not Applicable", "$500 or less", 
                                      "$501 to $2,500", "$2,501 to $5,000",
                                      "$5,001 to $10,000", "$10,001 to $15,000", 
                                      "$15,001 to $25,000", "$25,001 to $50,000", 
                                      "Over $50,000"),
                           ordered = TRUE)

# Convert Gender
df_cleaned$d1 <- factor(df_cleaned$d1,
                        levels = c(1, 2, 97, 99),
                        labels = c("Male", "Female", "Non-Binary", "Unavailable"))

# Convert AI Adoption (q80_1 - q80_4)
ai_adoption_vars <- paste0("q80_", 1:4)
df_cleaned[ai_adoption_vars] <- lapply(df_cleaned[ai_adoption_vars], factor, levels = c(0, 1))

# Convert AI Tool Usage (q81_1 - q81_17)
ai_tool_vars <- paste0("q81_", 1:17)
df_cleaned[ai_tool_vars] <- lapply(df_cleaned[ai_tool_vars], factor, 
                                   levels = c(0, 1, -7), 
                                   labels = c("0", "1", "Not Applicable"))

# Convert AI Application Use (q82_1 - q82_6)
ai_application_vars <- paste0("q82_", 1:6)
df_cleaned[ai_application_vars] <- lapply(df_cleaned[ai_application_vars], factor, 
                                          levels = c(0, 1, -7),
                                          labels = c("0", "1", "Not Applicable"))

# Convert AI Confidence (q83)



df_cleaned$q83_num <- df_cleaned$q83


df_cleaned$q83 <- factor(df_cleaned$q83,
                         levels = c(98, 99, -7, 1, 2, 3, 4, 5, 6, 7),
                         labels = c("Unavailable", "Unavailable", "Not Applicable",
                                    "Not Confident", "2", "3", "Neutral", "5", "6", "Very Confident"),
                         ordered = TRUE)


# Convert AI Impact (q84)
df_cleaned$q84 <- factor(df_cleaned$q84,
                         levels = c(98, 99, -7, 1, 2, 3),
                         labels = c("Unavailable", "Unavailable", "Not Applicable",
                                    "Positive", "Negative", "No Impact"))

# Convert Barriers (q89)
df_cleaned$q89 <- factor(df_cleaned$q89,
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, -7),
                         labels = c("Not Familiar", "No Need", "Too Long to Learn", 
                                    "Only Large Companies Benefit", "No Time", "Too Expensive", 
                                    "Other", "Don't Know", "Prefer not to answer", 
                                    "Not Applicable"))

# Convert AI Competitive Advantage (q93)
df_cleaned$q93 <- factor(df_cleaned$q93,
                         levels = c(98, 99, 1, 2, 3, 4, 5),
                         labels = c("Unavailable", "Unavailable", "Completely Disagree",
                                    "Somewhat Disagree", "Neutral", "Somewhat Agree", 
                                    "Completely Agree"),
                         ordered = TRUE)

### Create Age Group with "Unavailable" ###

# 1) Convert -8/-9 in d12 to NA, then to numeric
df_cleaned$d12 <- ifelse(df_cleaned$d12 %in% c(-8, -9), NA, df_cleaned$d12)
df_cleaned$d12 <- suppressWarnings(as.numeric(df_cleaned$d12))

# 2) Compute age 
df_cleaned$age <- 2024 - df_cleaned$d12

# 3) Use cut() to create 4 age brackets
df_cleaned$age_group <- cut(
  df_cleaned$age,
  breaks = c(-Inf, 20, 40, 60, Inf),
  labels = c("0-20", "20-40", "40-60", "60+"),
  right = FALSE
)

# 4) Turn true NA values in age_group into factor level "Unavailable"
df_cleaned <- df_cleaned %>%
  mutate(age_group = fct_explicit_na(age_group, na_level = "Unavailable"))

# 5) Re-level so "Unavailable" is last
df_cleaned$age_group <- factor(
  df_cleaned$age_group,
  levels = c("0-20", "20-40", "40-60", "60+", "Unavailable")
)

### Print structure and perform data checks ##
str(df)
unique_values <- lapply(df_cleaned, unique)
print(unique_values)

str(df_cleaned)

sum(is.na(df_cleaned$q5b_98))

missing_values <- NA

count_missing <- function(x) {
  sum(is.na(x) | x %in% missing_values)
}
missing_percentage <- function(x) {
  round((sum(is.na(x) | x %in% missing_values) / length(x)) * 100, 2)
}
missing_summary <- data.frame(
  Column = colnames(df_cleaned),
  Missing_Values = sapply(df_cleaned, count_missing),
  Missing_Percentage = sapply(df_cleaned, missing_percentage)
)
print(missing_summary)


#### Question 1  ##


# Industry-Specific AI Confidence (q5b_* vs. AI Adoption)

df_filtered <- df_cleaned %>%
  filter(q83_num <= 7)  # Keep only AI adopters & valid confidence scores

# Define industry names corresponding to q5b_* variables
industry_names <- c(
  "q5b_1" = "Accommodations",
  "q5b_2" = "Administrative and Support Services",
  "q5b_3" = "Agriculture",
  "q5b_4" = "Art and Design",
  "q5b_5" = "Beauty Products/Services",
  "q5b_6" = "Charitable/Political",
  "q5b_7" = "Construction/Engineering/Property Development",
  "q5b_8" = "Consumer Services",
  "q5b_9" = "Education Services and Training",
  "q5b_10" = "Entertainment/Recreation",
  "q5b_11" = "Financial Services",
  "q5b_12" = "Fashion",
  "q5b_13" = "Health Care and Medical",
  "q5b_14" = "Home Services",
  "q5b_15" = "Information Technology & Managed Services",
  "q5b_16" = "Legal Services",
  "q5b_17" = "Manufacturing and Wholesale Trade",
  "q5b_18" = "Marketing, Advertising, and PR",
  "q5b_19" = "Media/Content",
  "q5b_20" = "Packaged Food",
  "q5b_21" = "Professional & Business Services",
  "q5b_22" = "Personal Services",
  "q5b_23" = "Real Estate, Rental, or Leasing",
  "q5b_24" = "Restaurant",
  "q5b_25" = "Retail and Consumer",
  "q5b_26" = "Technology and Telecommunications",
  "q5b_27" = "Transportation, Logistics, and Automotive Services",
  "q5b_28" = "Website Design & Digital Marketing",
  "q5b_29" = "Other"
)

# Reshape data for AI adoption and confidence by industry
industry_ai_stats <- df_filtered %>%
  gather(key = "Industry", value = "Selected", q5b_1:q5b_29) %>%
  filter(Selected == "1") %>%
  group_by(Industry, q80_3) %>%
  summarise(Count = n(), Avg_Confidence = mean(q83_num[q83_num >= 1], na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Industry = industry_names[Industry],  
    AI_Use = ifelse(q80_3 == "1", "AI Adopted", "Not Adopted")
  ) %>%
  group_by(Industry) %>%
  mutate(Total_Industry_Count = sum(Count)) %>%
  ungroup() %>%
  mutate(AI_Adoption_Rate = ifelse(AI_Use == "AI Adopted", (Count / Total_Industry_Count) * 100, 0))

# Ensure industries are ordered correctly by AI adoption rate
industry_order <- industry_ai_stats %>%
  filter(AI_Use == "AI Adopted") %>%
  arrange(AI_Adoption_Rate) %>%
  pull(Industry)  # Extract ordered industry names




# Keep only AI adopters & valid confidence scores
df_filtered <- df_cleaned %>%
  filter(q80_3 == "1",q83_num >= 1, q83_num <= 7)  






























## Combined chart for business revenue and business size 

# Prepare AI adoption data by Business Revenue
df_revenue_summary <- df_cleaned %>%
  group_by(q11aa, q80_3) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(q11aa) %>%
  mutate(Proportion = Count / sum(Count) * 100,  # Convert to percentage
         Total_Count = sum(Count)) %>%  # Calculate total respondents in each category
  mutate(Category = "Business Revenue") %>%
  rename(Group = q11aa) %>%  # Rename for consistency
  mutate(Group = as.character(Group))  # Convert factor to character

# Prepare AI adoption data by Business Size
df_size_summary <- df_cleaned %>%
  group_by(q3a, q80_3) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(q3a) %>%
  mutate(Proportion = Count / sum(Count) * 100,  # Convert to percentage
         Total_Count = sum(Count)) %>%  # Calculate total respondents in each category
  mutate(Category = "Business Size") %>%
  rename(Group = q3a) %>%  # Rename for consistency
  mutate(Group = as.character(Group))  # Convert factor to character

# Combine both datasets (ensuring the Group variable is the same type)
df_combined <- bind_rows(df_revenue_summary, df_size_summary)










## AI Confidence by business size and revenue 



# Compute AI Confidence by Business Revenue
df_revenue_confidence <- df_filtered %>%
  group_by(q11aa) %>%
  summarise(Average_Confidence = mean(q83_num, na.rm = TRUE), Count = n(), .groups = "drop")

# Compute AI Confidence by Business Size
df_size_confidence <- df_filtered %>%
  group_by(q3a) %>%
  summarise(Average_Confidence = mean(q83_num, na.rm = TRUE), Count = n(), .groups = "drop")

#  Bar Plot for AI Confidence by Business Revenue
plot_revenue_confidence <- ggplot(df_revenue_confidence, aes(x = q11aa, y = Average_Confidence, fill = "AI Confidence")) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  
  # Label AI Confidence on top of bars
  geom_text(aes(label = round(Average_Confidence, 2)), 
            vjust = -0.5, size = 4, color = "black") +
  
  # Label Total Respondents inside the bars
  geom_text(aes(label = paste0("Total:", Count)), 
            vjust = 8.5, size = 2.5, color = "black", fontface = "bold") +
  
  scale_fill_manual(values = c("AI Confidence" = "#5bdbef")) +
  labs(title = "AI Confidence by Business Revenue (AI Adopters Only)",
       x = "Business Revenue",
       y = "Average AI Confidence (1-7)") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

#  Bar Plot for AI Confidence by Business Size
plot_size_confidence <- ggplot(df_size_confidence, aes(x = q3a, y = Average_Confidence, fill = "AI Confidence")) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  
  # Label AI Confidence on top of bars
  geom_text(aes(label = round(Average_Confidence, 2)), 
            vjust = -0.5, size = 4, color = "black") +
  
  # Label Total Respondents inside the bars
  geom_text(aes(label = paste0( "Total:", Count)), 
            vjust = 10.5, size = 2.5, color = "black", fontface = "bold") +
  
  scale_fill_manual(values = c("AI Confidence" = "#b69ae2")) +
  labs(title = "AI Confidence by Business Size (AI Adopters Only)",
       x = "Business Size",
       y = "Average AI Confidence (1-7)") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))














#### Question 2 -##


# Stacked Bar Chart - AI Adoption by Age & Gender (Q2)

adoption_age_gender_q2 <- df_cleaned %>%
  mutate(Adoption_Status = ifelse(q80_3 == "1", "Adopter", "Non-Adopter")) %>%
  group_by(age_group, d1, Adoption_Status) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(age_group, d1) %>%
  mutate(Percent = 100 * Count / sum(Count),
         Total_Respondents = sum(Count)) %>%  # Compute total respondents per group
  ungroup()

# Stacked Bar Chart - AI Confidence Distribution by Age & Gender (Q2)

# Updated Color Palette 
colors_conf <- c(
  "Not Confident"   = "#A7E0E5",  # Light Cyan
  "2"               = "#89D4D8",  # Soft Turquoise
  "3"               = "#6FC0C4",  # Pastel Teal
  "Neutral"         = "#55B1B6",  # Muted Aqua
  "5"               = "#3FA0A8",  # Deep Pastel Cyan
  "6"               = "#2D8E96",  # Soft Ocean Blue
  "Very Confident"  = "#1E7B82",  # Dusty Cyan
  "Unavailable"     = "#B0BEC5",  # Light Grey
  "Not Applicable"  = "#CFD8DC"   # Soft Grey
)


confidence_age_gender_q2 <- df_cleaned %>%
  mutate(conf_level = q83) %>%  # Replace NA with Not Applicable
  group_by(age_group, d1, conf_level) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(age_group, d1) %>%
  mutate(Percent = 100 * Count / sum(Count),
         Total_Respondents = sum(Count)) %>%  # Compute total respondents per group
  ungroup()

confidence_age_gender_q2$conf_level <- factor(confidence_age_gender_q2$conf_level)











# Stacked Bar Chart - AI Confidence Distribution by Age & Gender (Q2) - without Not applicable and unavailable values

# Define valid confidence levels (excluding "Unavailable" and "Not Applicable")
valid_confidence_levels <- c("Not Confident", "2", "3", "Neutral", "5", "6", "Very Confident")

# Updated Color Palette (excluding Unavailable & Not Applicable)
colors_conf <- c(
  "Not Confident"   = "#A7E0E5",  # Light Cyan
  "2"               = "#89D4D8",  # Soft Turquoise
  "3"               = "#6FC0C4",  # Pastel Teal
  "Neutral"         = "#55B1B6",  # Muted Aqua
  "5"               = "#3FA0A8",  # Deep Pastel Cyan
  "6"               = "#2D8E96",  # Soft Ocean Blue
  "Very Confident"  = "#1E7B82"  # Dusty Cyan
  
)

confidence_age_gender_q2 <- df_cleaned %>%
  mutate(conf_level = q83) %>%
  filter(conf_level %in% valid_confidence_levels) %>%  # Exclude Unavailable & Not Applicable
  group_by(age_group, d1, conf_level) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(age_group, d1) %>%
  mutate(Percent = 100 * Count / sum(Count),
         Total_Respondents = sum(Count)) %>%  # Compute total respondents per group
  ungroup()

confidence_age_gender_q2$conf_level <- factor(confidence_age_gender_q2$conf_level, levels = valid_confidence_levels)



# Summary Table - AI Adoption & Confidence by Age & Gender (Q2)
summary_table_q2 <- df_cleaned %>%
  group_by(age_group, d1) %>%
  summarise(
    total_responses = n(),
    adoption_count = sum(q80_3 == "1", na.rm = TRUE),
    adoption_rate = round(100 * adoption_count / total_responses, 1),
    avg_confidence = round(mean(q83_num[q83_num >= 1 & q83_num <= 7], na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(age_group, d1)








#### Question 3  ##

# Filter AI adopters for business use (Q80_3 == "1")
df_ai_adopters <- df_cleaned %>% filter(q80_3 == "1")

# Count Total AI Adopters
total_ai_adopters <- nrow(df_ai_adopters)

# AI Tool Usage Analysis (Q81_1 - Q81_17)
ai_tool_columns <- paste0("q81_", 1:17)
ai_tool_columns <- ai_tool_columns[ai_tool_columns %in% colnames(df_ai_adopters)]  

# Define AI tool labels
ai_tool_labels <- c(
  "q81_1" = "ChatGPT", "q81_2" = "DALL-E", "q81_3" = "Stable Diffusion",
  "q81_4" = "Jasper AI", "q81_5" = "Google Bard", "q81_6" = "Gemini",
  "q81_7" = "AlphaCode", "q81_8" = "Descript", "q81_9" = "Synthesia",
  "q81_10" = "Designs.ai", "q81_11" = "Bardeen", "q81_12" = "Claude",
  "q81_13" = "You.com", "q81_14" = "Canva (Magic Design/Magic Writer)",
  "q81_15" = "GitHub Copilot X", "q81_16" = "Airo", "q81_17" = "Other"
)

# Count AI tool usage 
if (length(ai_tool_columns) > 0) {
  ai_tool_counts <- df_ai_adopters %>%
    dplyr::select(all_of(ai_tool_columns)) %>%
    summarise(across(everything(), ~ sum(. == "1", na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "AI_Tool", values_to = "Count") %>%
    mutate(AI_Tool = recode(AI_Tool, !!!ai_tool_labels)) %>%
    arrange(desc(Count))  # Order by descending count
  
  # Reorder factor levels of AI_Tool for ordered legend and pie slices
  ai_tool_counts$AI_Tool <- factor(ai_tool_counts$AI_Tool, levels = ai_tool_counts$AI_Tool)
  
  # **Generate Dynamic Color Palette (Darkest to Lightest Order)**
  custom_colors <- c(
    "ChatGPT" = "#3D348B",  # Soft Pastel Purple
    "Canva (Magic Design/Magic Writer)" = "#A996F5",  # Muted Lavender Periwinkle
    "DALL-E" = "#8075A1",  # Muted Blue-Purple
    "Google Bard" = "#A299C6",  # Soft Lavender
    "Other" = "#C3B1E1",  # Light Lilac
    "Jasper AI" = "#0097A7",  # Deep Cyan
    "Gemini"  = "#00ACC1",  # Ocean Blue
    "GitHub Copilot X"  = "#4DD0E1",  # Sky Cyan
    "Stable Diffusion"  = "#6FC0C4",  # Pastel Turquoise
    "Designs.ai" = "#89D4D8",  # Muted Teal
    "Claude"  = "#A7E0E5",  # Soft Cyan
    "Descript" = "#E0FFFF",  # Light Grey
    "Synthesia" = "#A8B6C3",  # Soft Steel Grey
    "You.com" = "#BBCDE5",  # Pastel Blue
    "Airo"   = "#C3D9E3",  # Gentle Sky Blue
    "Bardeen" = "#E2ECF3",  # Faint Ice Blue
    "AlphaCode" = "#EBF5FA"   # Lightest Cyan (Almost White)
  )
  
  
  
  # **Pie Chart: Most Common AI Tools Used for Business**
  piechart <- ggplot(ai_tool_counts, aes(x = "", y = Count, fill = AI_Tool)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    
    # Add labels with count values positioned to avoid overlap
    geom_text(aes(label = Count), position = position_stack(vjust = 0.5), color = "white", size = 4) +
    
    labs(
      title = paste("Most Commonly Used AI Tools for Business (Total Users:", total_ai_adopters, ")"),
      fill = "AI Tool"
    ) +
    
    # Apply ordered legend and dynamic colors (Darkest to Lightest)
    scale_fill_manual(values = custom_colors, breaks = ai_tool_counts$AI_Tool) +
    
    theme_minimal() +
    theme(axis.text.x = element_blank(), legend.position = "right")
}














### Bar Chart

# AI Service Usage (Q82_1 - Q82_6)
ai_service_columns <- paste0("q82_", 1:6)
ai_service_columns <- ai_service_columns[ai_service_columns %in% colnames(df_ai_adopters)]  # Ensure columns exist

# Define AI service labels
ai_service_labels <- c(
  "q82_1" = "Customer Service", "q82_2" = "Marketing",
  "q82_3" = "Content Creation", "q82_4" = "Boosting Sales",
  "q82_5" = "Business Advice", "q82_6" = "Other"
)

# Count AI service usage (Only if columns exist)
if (length(ai_service_columns) > 0) {
  ai_service_counts <- df_ai_adopters %>%
    select_at(vars(ai_service_columns)) %>%
    summarise(across(everything(), ~ sum(. == "1", na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "AI_Service", values_to = "Count") %>%
    mutate(AI_Service = recode(AI_Service, !!!ai_service_labels)) %>%
    arrange(desc(Count))  # Order bars by count
  
  # Total respondents (sum of all service users)
  ai_service_counts$Total_Respondents <- sum(ai_service_counts$Count)
  
  # Generate a color palette dynamically
  color_palette <- colorRampPalette(c("#6A9FD8", "#92B4F4",  "#B3CDE8",  "#0097A7","#4DD0E1", "#B2EBF2" ))
  custom_colors <- setNames(color_palette(nrow(ai_service_counts)), ai_service_counts$AI_Service)
  
  # **Bar Chart: Services for Which AI is Used**
  bar<-ggplot(ai_service_counts, aes(x = reorder(AI_Service, Count), y = Count, fill = AI_Service)) +
    geom_bar(stat = "identity") +
    
    # Label the total respondents on top of each bar
    geom_text(aes(label = Count), vjust = -0.5, size = 4, fontface = "bold") +
    
    labs(title = "Different Services for Which AI is Used",
         x = "Service",
         y = "Number of Businesses") +
    
    # Apply custom blue-grey color palette
    scale_fill_manual(values = custom_colors, breaks = ai_service_counts$AI_Service) +
    
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")
}


#### Question 4  ##


custom_colors <- c(
  "Not Familiar" = "#006064",  # Dark Cyan Teal (Darkest)
  "No Need" = "#00838F",  # Deep Cyan Blue
  "No Time" = "#00ACC1",  # Bright Pastel Cyan
  "Don't Know" = "#4DD0E1",  # Soft Sky Cyan
  "Other" = "#5A6A9E",  # Deep Muted Blue
  "Prefer not to answer" = "#7986CB",  # Soft Periwinkle Blue
  "Too Long to Learn" = "#9FA8DA",  # Pastel Blue
  "Too Expensive" = "#B39DDB",  # Light Lavender Blue
  "Only Large Companies Benefit" = "#D1C4E9"   # Softest Pastel Purple (Lightest)
)


# Filter respondents who haven't tried AI (Q80_4 = 1)
df_non_adopters <- df_cleaned %>% filter(q80_4 == "1")

# Count Total Non-Adopters
total_non_adopters <- nrow(df_non_adopters)

# AI Adoption Barrier Analysis (Q89)
# Define barrier labels
ai_barrier_labels <- c(
  "1" = "Not Familiar with AI", "2" = "No Business Need",
  "3" = "Too Long to Learn", "4" = "Only Large Companies Benefit",
  "5" = "No Time", "6" = "Too Expensive",
  "7" = "Other", "8" = "Don't Know"
)

# Count responses for each AI adoption barrier
ai_barrier_counts <- df_non_adopters %>%
  count(q89) %>%
  mutate(Barrier = recode(as.character(q89), !!!ai_barrier_labels)) %>%
  arrange(desc(n))
















#### Challenges faced ##

# Make sure your excel data file is the same name as this down below and stored in the same working directory you set above
df <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Data")

df1 <- subset(df, select = c("q89_7_", "q21a_top1_1", "q21a_top1_2", "q21a_top1_3", "q21a_top1_4", "q21a_top1_5", 
                             "q21a_top1_6", "q21a_top1_7", "q21a_top1_8", "q21a_top1_9", "q21a_top1_10", 
                             "q21a_top1_11", "q21a_top1_12", "q21a_top1_13", "q21a_top1_14", "q21a_top1_15", 
                             "q21a_top1_16", "q21a_top1_17", 
                             "q21a_top2_1", "q21a_top2_2", "q21a_top2_3", "q21a_top2_4", 
                             "q21a_top2_5", "q21a_top2_6", "q21a_top2_7", "q21a_top2_8", "q21a_top2_9", 
                             "q21a_top2_10", "q21a_top2_11", "q21a_top2_12", "q21a_top2_13", "q21a_top2_14", 
                             "q21a_top2_15", "q21a_top2_16", "q21a_top2_17",  "q21a_top3_1", "q21a_top3_2", "q21a_top3_3", 
                             "q21a_top3_4", "q21a_top3_5", "q21a_top3_6", "q21a_top3_7", "q21a_top3_8", 
                             "q21a_top3_9", "q21a_top3_10", "q21a_top3_11", "q21a_top3_12", "q21a_top3_13", 
                             "q21a_top3_14", "q21a_top3_15", "q21a_top3_16", "q21a_top3_17", "q21a_top1_r_1", "q21a_top1_r_2", 
                             "q21a_top1_r_3", "q21a_top1_r_4", "q21a_top1_r_5", "q21a_top1_r_6", "q21a_top1_r_7", 
                             "q21a_top1_r_8", "q21a_top1_r_9", "q21a_top1_r_10", "q21a_top1_r_11", "q21a_top1_r_12", 
                             "q21a_top1_r_13", "q21a_top1_r_14", "q21a_top1_r_15", "q21a_top1_r_16", "q21a_top1_r_17", "q21a_top2_r_1", 
                             "q21a_top2_r_2", "q21a_top2_r_3", "q21a_top2_r_4", "q21a_top2_r_5", "q21a_top2_r_6", 
                             "q21a_top2_r_7", "q21a_top2_r_8", "q21a_top2_r_9", "q21a_top2_r_10", "q21a_top2_r_11", 
                             "q21a_top2_r_12", "q21a_top2_r_13", "q21a_top2_r_14", "q21a_top2_r_15", "q21a_top2_r_16",
                             "q21a_top2_r_17", "q21a_top3_r_1","q21a_top3_r_2", "q21a_top3_r_3", "q21a_top3_r_4", "q21a_top3_r_5", "q21a_top3_r_6", "q21a_top3_r_7",
                             "q21a_top3_r_8", "q21a_top3_r_9", "q21a_top3_r_10", "q21a_top3_r_11", "q21a_top3_r_12", "q21a_top3_r_13",
                             "q21a_top3_r_14", "q21a_top3_r_15", "q21a_top3_r_16", "q21a_top3_r_17")
)

library (dplyr)
other_reasons <- df1 %>%
  dplyr::select(q89_7_) %>%
  filter(!is.na(q89_7_))


# Make sure your excel data file is the same name as this down below and stored in the same working directory you set above
df1 <- df <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Data")





# Define challenge variable prefixes (top1, top2, top3)
challenge_prefixes <- c("q21a_top1_", "q21a_top2_", "q21a_top3_")

# Define the challenge numbers (1 to 17 only — excluding 'None', 'Other', 'DK', 'PNA')
challenge_numbers <- sprintf("%d", 1:17)

# Generate full list of variable names
challenge_vars <- unlist(lapply(challenge_prefixes, function(prefix) {
  paste0(prefix, challenge_numbers)
}))

# Keep only relevant columns
df_challenges <- df1 %>% dplyr::select(all_of(challenge_vars))

# Replace -7, -8, -9 with 0, keep 1 as is (selected), treat everything else as 0
df_challenges_clean <- df_challenges %>% 
  mutate(across(everything(), ~ifelse(. %in% c(1), 1, 0)))

# Now group by each of the 17 challenges and take row-wise max to check if it was selected in any top 3
challenge_cols <- paste0("Challenge_", challenge_numbers)

df_final <- data.frame(matrix(nrow = nrow(df_challenges_clean), ncol = 17))
names(df_final) <- challenge_cols

for (i in 1:17) {
  cols_i <- paste0(challenge_prefixes, i)
  df_final[[i]] <- df_challenges_clean %>% dplyr::select(all_of(cols_i)) %>% apply(1, max)
}

challenge_labels <- c(
  "Access to financial capital",
  "Affordable space",
  "Marketing my business online (social media, ads, promotions, etc.)",
  "Marketing my business using traditional media (print, radio, tv, etc.)",
  "Affordable healthcare / Benefits",
  "Finding employees, independent contractors, or consultants with necessary skills",
  "Getting my business website online effectively",
  "Getting my business social media accounts set up",
  "Expertise on business planning and management",
  "Rising costs on wages and materials",
  "Technology management",
  "Networking with other business owners",
  "Licensing / Permits",
  "Supply chain delays",
  "Time management",
  "Taxes",
  "Knowing what to do at each phase"
)

colnames(df_final) <- challenge_labels



# Each column is a challenge, with binary values (1 = selected, 0 = not selected)

# Sum each column to get the total count of each challenge
challenge_counts <- colSums(df_final)

# Convert to a data frame for plotting
challenge_df <- data.frame(
  Challenge = names(challenge_counts),
  Count = as.numeric(challenge_counts)
)









### Challenges by AI Adoption ### 


# Append AI Adoption status to df_final
df_challenges_profile <- df_final %>%
  mutate(adoption_status = df_cleaned$q80_3) %>%
  mutate(adoption_status = ifelse(adoption_status == "1", "AI Adopter", "Non-Adopter"))

# Summarize challenge counts by adoption status
challenge_summary <- df_challenges_profile %>%
  pivot_longer(-adoption_status, names_to = "Challenge", values_to = "Selected") %>%
  filter(Selected == 1) %>%
  group_by(Challenge, adoption_status) %>%
  summarise(Count = n(), .groups = "drop")




##### Challenges against number of tools and confidence table ### 

# Industry variables (Q5b_1 to Q5b_29)
industry_vars <- paste0("q5b_", 1:29)
industry_labels <- c(
  "q5b_1"  = "Accommodations",
  "q5b_2"  = "Admin & Support Services",
  "q5b_3"  = "Agriculture",
  "q5b_4"  = "Art & Design",
  "q5b_5"  = "Beauty Products/Services",
  "q5b_6"  = "Charitable/Political",
  "q5b_7"  = "Construction/Engineering",
  "q5b_8"  = "Consumer Services",
  "q5b_9"  = "Education/Training",
  "q5b_10" = "Entertainment/Recreation",
  "q5b_11" = "Financial Services",
  "q5b_12" = "Fashion",
  "q5b_13" = "Health Care/Medical",
  "q5b_14" = "Home Services",
  "q5b_15" = "IT & Managed Services",
  "q5b_16" = "Legal Services",
  "q5b_17" = "Manufacturing/Wholesale",
  "q5b_18" = "Marketing/Advertising/PR",
  "q5b_19" = "Media/Content",
  "q5b_20" = "Packaged Food",
  "q5b_21" = "Professional/Business Services",
  "q5b_22" = "Personal Services",
  "q5b_23" = "Real Estate/Rental/Leasing",
  "q5b_24" = "Restaurant",
  "q5b_25" = "Retail & Consumer",
  "q5b_26" = "Tech & Telecom",
  "q5b_27" = "Transport/Logistics/Auto",
  "q5b_28" = "Web Design & Digital Mktg",
  "q5b_29" = "Other"
)

# AI tool variables (Q81_1 to Q81_17)
ai_tool_vars <- paste0("q81_", 1:17)

# Challenge ranking variables for Q21a (assumed to be in df1)
top1_vars <- paste0("q21a_top1_", 1:17)
top2_vars <- paste0("q21a_top2_", 1:17)
top3_vars <- paste0("q21a_top3_", 1:17)

# Challenge option labels (for the 17 challenge options)
challenge_labels <- c(
  "Access to financial capital",
  "Affordable space",
  "Marketing my business online (social media, ads, promotions, etc.)",
  "Marketing my business using traditional media (print, radio, tv, etc.)",
  "Affordable healthcare / Benefits",
  "Finding employees, independent contractors, or consultants with necessary skills",
  "Getting my business website online effectively",
  "Getting my business social media accounts set up",
  "Expertise on business planning and management",
  "Rising costs on wages and materials",
  "Technology management",
  "Networking with other business owners",
  "Licensing / Permits",
  "Supply chain delays",
  "Time management",
  "Taxes",
  "Knowing what to do at each phase"
)


# Assume df1 contains the challenge variables along with "id"
df_challenges <- df1 %>% 
  dplyr::select(id, all_of(top1_vars), all_of(top2_vars), all_of(top3_vars))

# Merge df_cleaned (with your other survey variables) with df_challenges on "id"
df_merged <- left_join(df_cleaned, df_challenges, by = "id")

# ---- Filter for Airo Users 
# Airo users: respondents who are AI adopters (q80_3 == "1") and used Airo (q81_16 == "1")
df_airo <- df_merged %>% 
  filter(q80_3 == "1", q81_16 == "1")

# ---- Create Profile Table -
df_airo_profile <- df_airo %>%
  rowwise() %>%
  mutate(
    # Combine selected industries into one string
    Industry = {
      selected <- c_across(all_of(industry_vars))
      inds <- industry_vars[which(selected == "1")]
      if(length(inds) > 0) paste(recode(inds, !!!industry_labels), collapse = ", ") else NA_character_
    },
    # Count the number of AI tools used
    Num_AI_Tools = sum(c_across(all_of(ai_tool_vars)) == "1", na.rm = TRUE),
    # AI Confidence (assumed to be in q83)
    AI_Confidence = q83,
    # Determine Top 1 Challenge from Q21a top1 variables
    Top1 = {
      vals1 <- c_across(all_of(top1_vars))
      idx1 <- which(vals1 == 1)
      if(length(idx1) > 0) challenge_labels[idx1[1]] else NA_character_
    },
    Top2 = {
      vals2 <- c_across(all_of(top2_vars))
      idx2 <- which(vals2 == 1)
      if(length(idx2) > 0) challenge_labels[idx2[1]] else NA_character_
    },
    Top3 = {
      vals3 <- c_across(all_of(top3_vars))
      idx3 <- which(vals3 == 1)
      if(length(idx3) > 0) challenge_labels[idx3[1]] else NA_character_
    },
    Top_Three_Challenges = paste(na.omit(c(Top1, Top2, Top3)), collapse = ", ")
  ) %>%
  ungroup() %>%
  # Select columns and rename them
  dplyr::select(`No of AI Tools Used` = Num_AI_Tools,
                `AI Confidence` = AI_Confidence,
                Industry,
                `Top Three Challenges` = Top_Three_Challenges)

# Convert to a dataframe and display the result
df_airo_profile <- as.data.frame(df_airo_profile)













#### Personas ###


#Define Personas Based on AI Confidence
df_cleaned <- df_cleaned %>%
  mutate(
    persona = case_when(
      q83_num >= 6 ~ "Confident",
      q83_num >= 3 & q83_num <= 5 ~ "Lukewarm",
      q83_num >= 1 & q83_num <= 2 ~ "Not Confident",
      TRUE ~ "Unavailable"
    )
  )




# 1. Bar Chart: Personas by Business Size
persona_by_size <- df_cleaned %>%
  filter(!is.na(q3a), persona != "Unavailable") %>%
  group_by(q3a, persona) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(q3a) %>%
  mutate(Proportion = Count / sum(Count))

plot_size <- ggplot(persona_by_size, aes(x = q3a, y = Proportion, fill = persona)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = Count), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  scale_fill_manual(values = c(
    "Confident" = "#5bdbef",
    "Lukewarm" = "lightgrey",
    "Not Confident" = "#B39DDB"
  )) +
  labs(
    title = "Customer Personas by Business Size",
    x = "Business Size",
    y = "Proportion",
    fill = "Persona"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 2. Bar Chart: Personas by Business Revenue
persona_by_revenue <- df_cleaned %>%
  filter(!is.na(q11aa), persona != "Unavailable") %>%
  group_by(q11aa, persona) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(q11aa) %>%
  mutate(Proportion = Count / sum(Count))

plot_revenue <- ggplot(persona_by_revenue, aes(x = q11aa, y = Proportion, fill = persona)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = Count), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "black") +
  scale_fill_manual(values = c(
    "Confident" = "#5bdbef",
    "Lukewarm" = "lightgrey",
    "Not Confident" = "#B39DDB"
  )) +
  labs(
    title = "Customer Personas by Business Revenue",
    x = "Business Revenue",
    y = "Proportion",
    fill = "Persona"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))










# 1) Define Q82 variables (q82_1 to q82_6) and labels
q82_vars <- paste0("q82_", 1:6)
q82_labels <- c(
  "q82_1" = "Customer Service",
  "q82_2" = "Marketing",
  "q82_3" = "Content Creation",
  "q82_4" = "Boosting Sales",
  "q82_5" = "Business Advice",
  "q82_6" = "Other"
)

# 2) Define valid confidence responses from Q83
valid_confidence <- c("Not Confident", "2", "3", "Neutral", "5", "6", "Very Confident")

# 3) Filter for AI adopters with valid Q83
df_q82 <- df_cleaned %>%
  filter(q80_3 == "1") %>%  # AI Adopters
  filter(q83 %in% valid_confidence) %>%
  # Recode Q83 into 3 groups: Not Confident, Neutral, Confident
  mutate(
    new_confidence = case_when(
      q83 %in% c("Not Confident", "2")       ~ "Not Confident",
      q83 %in% c("3", "Neutral", "5")        ~ "Lukewarm",
      q83 %in% c("6", "Very Confident")      ~ "Confident",
      TRUE                                   ~ NA_character_
    )
  )

# 4) Check which Q82 columns exist in df_cleaned
available_q82 <- intersect(q82_vars, names(df_q82))
if (length(available_q82) == 0) {
  stop("None of the Q82 variables exist in your dataset. Please verify the variable names.")
}

# 5) Pivot Q82 columns into long format and keep only "1" (i.e. selected)
df_q82_long <- df_q82 %>%
  pivot_longer(
    cols = all_of(available_q82),
    names_to = "Q82_item",
    values_to = "Selected"
  ) %>%
  filter(Selected == "1")

# 6) Summarize counts & proportions by Q82 item and recoded confidence
df_q82_summary <- df_q82_long %>%
  group_by(Q82_item, new_confidence) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Q82_item) %>%
  mutate(
    Total = sum(Count),
    Proportion = 100 * Count / Total
  ) %>%
  ungroup()

# Recode Q82_item names for nice labels
df_q82_summary$Q82_item <- recode(df_q82_summary$Q82_item, !!!q82_labels)

# Order new_confidence factor
df_q82_summary$new_confidence <- factor(
  df_q82_summary$new_confidence,
  levels = c("Not Confident", "Lukewarm", "Confident")
)



# 1) Filter for AI adopters (q80_3 == "1") with valid Q83 confidence responses
valid_confidence <- c("Not Confident", "2", "3", "Neutral", "5", "6", "Very Confident")

df_tools <- df_cleaned %>%
  filter(q80_3 == "1") %>%
  filter(q83 %in% valid_confidence) %>%
  # 2) Recode Q83 into three groups
  mutate(
    new_confidence = case_when(
      q83 %in% c("Not Confident", "2")        ~ "Not Confident",
      q83 %in% c("3", "Neutral", "5")         ~ "Lukewarm",
      q83 %in% c("6", "Very Confident")       ~ "Confident",
      TRUE                                    ~ NA_character_
    )
  ) %>%
  # 3) Count how many AI tools each respondent is using (q81_1 to q81_17)
  mutate(
    num_tools = rowSums(dplyr::select(., starts_with("q81_")) == "1", na.rm = TRUE)
  )

# 4) Summarize the distribution of confidence within each num_tools group
df_tools_summary <- df_tools %>%
  group_by(num_tools, new_confidence) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(num_tools) %>%
  mutate(
    Total = sum(Count),
    Proportion = 100 * Count / Total
  ) %>%
  ungroup()

# 5) Order factor levels for new_confidence
df_tools_summary$new_confidence <- factor(
  df_tools_summary$new_confidence,
  levels = c("Not Confident", "Lukewarm", "Confident")
)












#### Clustering ###

#### Preparing personas by adding non adopters to ai confidence

### Create df_persona by modifying only q83 in df_cleaned ###

# Copy the original cleaned dataframe
df_persona <- df_cleaned

# Convert q83 to character first to allow modification
df_persona$q83 <- as.character(df_persona$q83)

# Replace "Not Applicable" with "0" for non-adopters (q80_3 == "0")
df_persona <- df_persona %>%
  mutate(
    q83 = ifelse(q80_3 == "0" & q83 == "Not Applicable", "0", q83)
  )

# Convert q83 back to an ordered factor with new level
df_persona$q83 <- factor(df_persona$q83,
                         levels = c("Unavailable","0", "Not Confident", "2", "3", "Neutral", "5", "6", "Very Confident"
                         ),
                         ordered = TRUE)



# Create a new column 'persona' based on AI confidence level (q83)
df_persona <- df_persona %>%
  mutate(
    persona = case_when(
      q83 %in% c("6", "Very Confident") ~ "Confident",
      q83 %in% c("3", "Neutral", "5") ~ "Lukewarm",
      q83 %in% c("Not Confident", "2") ~ "Not Confident",
      q83 == "0" ~ "Non-Adopter",
      q83 == "Unavailable" ~ "Unavailable",
      TRUE ~ "Unavailable"  # Catch any missing or unmatched levels
    )
  )

# Convert persona to a factor for modeling/analysis
df_persona$persona <- factor(df_persona$persona,
                             levels = c("Confident", "Lukewarm", "Not Confident", "Non-Adopter", "Unavailable"))







df_persona <- dplyr::select(df_persona, -age, -q83_num, -d12, -d1,
                            -q80_1, -q80_2, -q80_3, -q80_4, -q81_17, -q82_6, -id, -cid)





library(dplyr)

# Helper function to get the column number where value is "1"
get_active_column_number <- function(row, prefix) {
  cols <- grep(paste0("^", prefix, "_\\d+$"), names(row), value = TRUE)
  for (col in cols) {
    if (as.character(row[[col]]) == "1") {
      return(gsub(paste0(prefix, "_"), "", col))  # Extract number
    }
  }
  return("Unavailable")
}

# Apply to create q5b
df_persona$q5b <- apply(df_persona, 1, get_active_column_number, prefix = "q5b")
df_persona$q5b <- as.factor(df_persona$q5b)

# Apply to create q81
df_persona$q81 <- apply(df_persona, 1, get_active_column_number, prefix = "q81")
df_persona$q81 <- as.factor(df_persona$q81)

# Apply to create q82
df_persona$q82 <- apply(df_persona, 1, get_active_column_number, prefix = "q82")
df_persona$q82 <- as.factor(df_persona$q82)

# Drop the original binary columns
df_persona <- df_persona %>%
  dplyr::select(-matches("^q5b_\\d+$"), -q5b_31, -q5b_98,
                -matches("^q81_\\d+$"),
                -matches("^q82_\\d+$"))






# Drop q83 NA levels if any remain 
df_kmodes <- df_persona

# Convert to data frame (remove tbl_df class for klaR compatibility)
df_kmodes_clean <- as.data.frame(df_kmodes)

# Ensure all variables are factors (k-modes needs categorical variables)
df_kmodes_clean[] <- lapply(df_kmodes_clean, function(x) {
  if (!is.factor(x)) as.factor(x) else x
})

# Use cost (total dissimilarity) to determine optimal k
set.seed(123)
costs <- c()
k_values <- 2:10

for (k in k_values) {
  km <- kmodes(df_kmodes_clean, modes = k, iter.max = 10, weighted = FALSE)
  costs[k - 1] <- km$withindiff
}



# Fit final model with chosen k (e.g., 4)
set.seed(123)
km_result <- kmodes(df_kmodes_clean, modes = 4, iter.max = 20, weighted = FALSE)

# Add cluster assignment to df_persona
df_persona$cluster <- as.factor(km_result$cluster)



# Summarize characteristics of each cluster
summary_by_cluster <- aggregate(. ~ cluster, data = df_persona, FUN = function(x) names(sort(table(x), decreasing = TRUE)[1]))


#### Classification Tree AI Confidence Descriptive Model ###

#### Import df and columns ###

df <- read_excel("VF_US_National_FEB24_RawData.xlsx", sheet = "Data")

colnames(df)

# Keep only those variables required
df_class_tree <- subset(df, select = c(
  "q3a", "q5a_1", "q5a_2", "q5a_3", "q5a_4", "q5a_98", "q5a_99",
  "q7", "q9", "q11aa", "q11c", "q11d", "q12", "q16", "q17a", "q20",
  "d1", "d3", "d4", "d6", "d12", 
  "q80_3",
  "q81_1", "q81_2", "q81_3", "q81_4", "q81_5", "q81_6", "q81_7", "q81_8", "q81_9", "q81_10", 
  "q81_11", "q81_12", "q81_13", "q81_14", "q81_15", "q81_16", "q81_17",
  "q82_1", "q82_2", "q82_3", "q82_4", "q82_5", "q82_6",
  "q83", "q84", "q85_1", "q85_2", "q85_3", "q85_4", "q85_5", "q85_6", "q85_7", "q85_8", 
  "q87_1", "q87_2", "q87_3", "q87_4", "q87_5", "q87_6", "q87_7", "q87_8", "q93"
))



##### Conversions ###
library(dplyr)
library(forcats)

# Helper: Convert to factor with custom labels
to_factor <- function(x, levels, labels, ordered = FALSE) {
  fct <- factor(x, levels = levels, labels = labels, ordered = ordered)
  fct <- fct %>% fct_explicit_na(na_level = "Missing")
  return(fct)
}

# Convert and label variables
df_class_tree <- df_class_tree %>%
  mutate(
    # Number of Employees
    q3a = to_factor(q3a,
                    levels = c(1:7, 98, 99),
                    labels = c("Just Me", "2-4", "5-9", "10-19", "20-49", "50-99", "100+", "Unavailable", "Unavailable"),
                    ordered = TRUE),
    
    # Business location type (Q5a – multiple select, no recoding needed)
    # Revenue expectation next 6 months
    q7 = to_factor(q7,
                   levels = c(1, 2, 3, 98, 99),
                   labels = c("More Positive", "More Negative", "No Change", "Unavailable", "Unavailable")),
    
    # Income source (Q9)
    q9 = to_factor(q9,
                   levels = c(1, 2, 3, 98, 99),
                   labels = c("Main", "Supplemental", "None", "Unavailable", "Unavailable")),
    
    # Monthly revenue
    q11aa = to_factor(q11aa,
                      levels = c(1:8, 98),
                      labels = c("$0-500", "$501-2500", "$2501-5000", "$5001-10k", "$10k-15k", "$15k-25k", "$25k-50k", "Over $50k", "Unavailable"),
                      ordered = TRUE),
    
    # Revenue trend
    q11c = to_factor(q11c,
                     levels = c(1, 2, 3, 98, 99),
                     labels = c("Increased", "Constant", "Decreased", "Unavailable", "Unavailable")),
    
    # Cash on hand
    q11d = to_factor(q11d,
                     levels = c(6, 1, 2, 3, 98, 99),
                     labels = c("No Cash", "<1 month", "1-3 months", ">3 months", "Unavailable", "Unavailable"),
                     ordered = TRUE),
    
    # Hours/week on business
    q12 = to_factor(q12,
                    levels = c(1:7, 98, 99),
                    labels = c("0", "1-10", "11-20", "21-30", "31-40", "41-50", "51+", "Unavailable", "Unavailable"),
                    ordered = TRUE),
    
    # Employment situation
    q16 = to_factor(q16,
                    levels = c(1:6, 97, 98, 99),
                    labels = c("Employed by business", "FT outside business", "PT outside business", "Student/Unemployed", "Retired", "Unemployed", "Other", "Unavailable", "Unavailable")),
    
    # Business aspiration
    q17a = to_factor(q17a,
                     levels = c(1:4, 6, 98, 99),
                     labels = c("Solo", "Small w/ employees", "Mid-size", "Corporate", "None", "Unavailable", "Unavailable"),
                     ordered = TRUE),
    
    # Business founding year
    q20 = as.numeric(q20),
    
    # Gender
    d1 = to_factor(d1,
                   levels = c(1, 2, 97, 99),
                   labels = c("Male", "Female", "Non-Binary", "Unavailable")),
    
    # Race
    d3 = to_factor(d3,
                   levels = c(1:5, 99),
                   labels = c("White", "Black", "Asian", "Native", "Other/Multi", "Unavailable")),
    
    # Hispanic
    d4 = to_factor(d4,
                   levels = c(1, 2, 99),
                   labels = c("Yes", "No", "Unavailable")),
    
    # Education
    d6 = to_factor(d6,
                   levels = c(1:6, 99),
                   labels = c("Less than HS", "HS Grad", "Trade/Vocational", "Some College", "College Grad", "Grad Degree", "Unavailable"),
                   ordered = TRUE),
    
    # Birth year
    d12 = as.numeric(d12),
    
    # AI usage for business
    q80_3 = to_factor(q80_3,
                      levels = c(0, 1),
                      labels = c("No", "Yes")),
    
    # AI confidence
    q83 = to_factor(q83,
                    levels = c("1", "2", "3", "4", "5", "6", "7", "98", "99"),
                    labels = c("1 - Not Confident", "2", "3", "4 - Neutral", "5", "6", "7 - Very Confident", "Unavailable", "Unavailable"),
                    ordered = TRUE),
    
    # AI impact
    q84 = to_factor(q84,
                    levels = c(1, 2, 3, 98, 99),
                    labels = c("Positive", "Negative", "None", "Unavailable", "Unavailable")),
    
    
    # AI competitiveness belief
    q93 = to_factor(q93,
                    levels = c(1:5, 98, 99),
                    labels = c("Completely Agree", "Somewhat Agree", "Neutral", "Somewhat Disagree", "Completely Disagree", "Unavailable", "Unavailable"),
                    ordered = TRUE)
  )




# Helper to convert binary survey multiselect columns to factor
bin_to_factor <- function(x) {
  factor(x,
         levels = c(1, 0, 98, 99, -7),
         labels = c("Selected", "Not Selected", "Unavailable", "Unavailable", "Not Applicable"))
}

# Apply to multiple columns
multi_select_cols <- c(
  paste0("q5a_", c(1, 2, 3, 4, 98, 99)),
  paste0("q81_", 1:17),
  paste0("q82_", 1:6),
  paste0("q85_", c(1:8)),
  paste0("q87_", c(1:8))
)

df_class_tree[multi_select_cols] <- lapply(df_class_tree[multi_select_cols], bin_to_factor)

# Convert year of birth (d12) to age category
df_class_tree <- df_class_tree %>%
  mutate(
    age = 2024 - as.numeric(d12),
    age_group = cut(age,
                    breaks = c(-Inf, 20, 40, 60, Inf),
                    labels = c("0-20", "21-40", "41-60", "60+"),
                    right = TRUE),
    age_group = factor(age_group, ordered = TRUE)
  )

# Convert business age from q20 (year business was launched)
df_class_tree <- df_class_tree %>%
  mutate(
    business_age = 2024 - as.numeric(q20),
    business_age_cat = case_when(
      business_age <= 1 ~ "1 year",
      business_age <= 5 ~ "2–5 years",
      business_age <= 10 ~ "6–10 years",
      business_age <= 15 ~ "11–15 years",
      business_age > 15 ~ ">15 years",
      TRUE ~ "Unavailable"
    ),
    business_age_cat = factor(business_age_cat,
                              levels = c("1 year", "2–5 years", "6–10 years", "11–15 years", ">15 years", "Unavailable"),
                              ordered = TRUE)
  )




#### Data prep for Class tree ###

# Filter dataset to include only AI adopters (used AI for business)
df_class_tree <- df_class_tree %>%
  filter(q80_3 == "Yes")

df_class_tree <- df_class_tree %>%
  mutate(
    q3a = case_when(
      q3a == "Just Me" ~ "Just Me",
      q3a %in% c("2-4", "5-9", "10-19", "20-49", "50-99", "100+") ~ "More Than 1",
      TRUE ~ "Unavailable"
    ),
    q3a = factor(q3a, levels = c("Just Me", "More Than 1", "Unavailable"))
  )

df_class_tree <- df_class_tree %>%
  mutate(
    q11aa = case_when(
      q11aa %in% c("$0-500", "$501-2500") ~ "< $2500",
      q11aa %in% c("$2501-5000", "$5001-10k", "$10k-15k", "$15k-25k", "$25k-50k", "Over $50k") ~ ">= $2501",
      q11aa %in% c("Don't Know", "Unavailable") ~ "Unavailable",
      q11aa %in% c("Missing") ~ "Not Applicable",
      TRUE ~ "Not Applicable"
    ),
    q11aa = factor(q11aa, levels = c("< $2500", ">= $2501", "Unavailable", "Not Applicable"))
  )


df_class_tree <- df_class_tree %>%
  mutate(
    q11c = fct_collapse(q11c,
                        Unavailable = c("Don't Know", "Unavailable"),
                        `Not Applicable` = "Missing")
  )

df_class_tree <- df_class_tree %>%
  mutate(
    q11d = case_when(
      q11d == "<1 month" ~ "Less than 1 month",
      q11d %in% c("No Cash", "1-3 months", ">3 months") ~ "More than 1 month",
      q11d == "Missing" ~ "Not Applicable",
      q11d == "Unavailable" ~ "Unavailable",
      TRUE ~ "Not Applicable"
    ),
    q11d = factor(q11d, levels = c("Less than 1 month", "More than 1 month", "Unavailable", "Not Applicable"))
  )


df_class_tree <- df_class_tree %>%
  mutate(
    q16 = case_when(
      q16 == "Employed by business" ~ "Employed by business",
      q16 %in% c("PT outside business","FT outside business") ~ "Outside business",
      q16 %in% c("Retired", "Unemployed", "Student/Unemployed", "Other") ~ "Retired/Unemployed",
      q16 %in% c("Don't Know", "Unavailable") ~ q16,
      TRUE ~ "Unavailable"
    ),
    q16 = factor(q16, levels = c("Employed by business", "Outside business", "Retired/Unemployed", "Unavailable"))
  )

df_class_tree <- df_class_tree %>%
  mutate(
    q17a = case_when(
      q17a == "Solo" ~ "Solo",
      q17a == "Small w/ employees" ~ "Small w/ employees",
      q17a %in% c("Mid-size", "Corporate") ~ "Mid-Large Size",
      q17a %in% c("None", "Don't Know", "Unavailable") ~ q17a,
      TRUE ~ "Unavailable"
    ),
    q17a = factor(q17a, levels = c("Solo", "Small w/ employees", "Mid-Large Size", "None", "Don't Know", "Unavailable"))
  )


df_class_tree <- df_class_tree %>%
  dplyr::select(-q20)

df_class_tree <- df_class_tree %>%
  mutate(
    d3 = fct_recode(d3, "Hispanic/Latino" = "Unavailable")
  ) %>%
  dplyr::select(-d4) 

df_class_tree <- df_class_tree %>%
  mutate(
    d6 = case_when(
      d6 %in% c("Less than HS", "HS Grad", "Trade/Vocational") ~ "Less than College",
      d6 %in% c("Some College", "College Grad", "Grad Degree") ~ d6,
      d6 == "Unavailable" ~ "Unavailable",
      TRUE ~ "Unavailable"
    ),
    d6 = factor(d6, levels = c("Less than College", "Some College", "College Grad", "Grad Degree", "Unavailable"))
  )

df_class_tree <- df_class_tree %>%
  dplyr::select(-d12, -q80_3)

df_class_tree <- df_class_tree %>%
  mutate(
    q83 = case_when(
      q83 %in% c("1 - Not Confident", "2", "3") ~ "Not Confident",
      q83 %in% c("4 - Neutral", "5") ~ "Neutral-Lukewarm",
      q83 %in% c("6", "7 - Very Confident") ~ "Confident"
    ),
    q83 = factor(q83, levels = c("Not Confident", "Neutral-Lukewarm", "Confident"))
  )

df_class_tree <- df_class_tree %>%
  mutate(
    q93 = case_when(
      q93 %in% c("Completely Agree", "Somewhat Agree") ~ "Agree",
      q93 %in% c("Somewhat Disagree", "Completely Disagree") ~ "Disagree",
      q93 == "Neutral" ~ "Neutral",
      q93 == "Unavailable" ~ "Unavailable",
      TRUE ~ "Unavailable"
    ),
    q93 = factor(q93, levels = c("Agree", "Neutral", "Disagree", "Unavailable"))
  )


df_class_tree <- df_class_tree %>%
  dplyr::select(-age)

df_class_tree <- df_class_tree %>%
  mutate(
    business_age = ifelse(business_age > 100, NA, business_age),
    business_age_cat = case_when(
      is.na(business_age) ~ "Unavailable",
      business_age <= 1 ~ "1 year",
      business_age <= 5 ~ "2–5 years",
      business_age <= 10 ~ "6–10 years",
      business_age <= 15 ~ "11–15 years",
      business_age > 15 ~ ">15 years"
    ),
    business_age_cat = factor(business_age_cat, levels = c("1 year", "2–5 years", "6–10 years", "11–15 years", ">15 years", "Unavailable"))
  )

df_class_tree <- df_class_tree %>%
  mutate(
    business_age_cat = case_when(
      business_age_cat %in% c("11–15 years", ">15 years") ~ ">10 years",
      business_age_cat %in% c("1 year", "2–5 years", "6–10 years") ~ as.character(business_age_cat),
      business_age_cat == "Unavailable" ~ "Unavailable",
      TRUE ~ "Unavailable"
    ),
    business_age_cat = factor(business_age_cat,
                              levels = c("1 year", "2–5 years", "6–10 years", ">10 years", "Unavailable"))
  )

df_class_tree <- df_class_tree %>%
  dplyr::select(-business_age)




#### Classification Tree Model ###

# Required libraries
library(dplyr)
library(rpart)
library(rpart.plot)

set.seed(123)  # for reproducibility

# Remove rows with missing target (q83)
df_model <- df_class_tree %>%
  filter(!is.na(q83))

# Build the full decision tree on the entire dataset
full_tree <- rpart(q83 ~ .,
                   data = df_model,
                   method = "class",
                   cp = 0)  # Start with no pruning



# Display the complexity parameter (CP) table
cp_table <- printcp(full_tree)

# Find optimal cp based on cross-validation error
optimal_cp <- full_tree$cptable[which.min(full_tree$cptable[,"xerror"]), "CP"]

# Prune the tree for better readability and simplicity
pruned_tree <- prune(full_tree, cp = 0.01)






#### Logistic regression - AI Adoption Predictive Model ###


# Load required libraries
library(dplyr)
library(broom)
library(pscl)       # For pseudo R²
library(knitr)      # For table formatting
library(kableExtra) # For better HTML table formatting

# 1. Prepare dataset with relevant variables as factors
df_logit_input <- df_cleaned %>%
  mutate(
    q3a        = factor(q3a,        ordered = FALSE),
    q11aa      = factor(q11aa,      ordered = FALSE),
    q93        = factor(q93,        ordered = FALSE),
    d1         = factor(d1,         ordered = FALSE),
    age_group  = factor(age_group,  ordered = FALSE)
  ) %>%
  filter(!is.na(q80_3))  # Dependent variable must not be NA

# 2. Split into training and test sets (70/30 split)
set.seed(123)  # for reproducibility
train_idx  <- sample(seq_len(nrow(df_logit_input)), size = 0.7 * nrow(df_logit_input))
trainData  <- df_logit_input[train_idx, ]
testData   <- df_logit_input[-train_idx, ]

# 3. Define industry variables and ensure they exist in the dataset
industry_vars <- paste0("q5b_", 1:29)
industry_vars <- industry_vars[industry_vars %in% names(df_logit_input)]

# 4. Build model formula
model_formula <- as.formula(
  paste("q80_3 ~ q3a + q11aa + q93 + d1 + age_group +",
        paste(industry_vars, collapse = " + "))
)

# 5. Fit logistic regression model on training data
logit_model <- glm(model_formula, data = trainData, family = binomial())

# 6. Tidy up the model output
tidy_model <- tidy(logit_model) %>%
  filter(!grepl("\\.L|\\.Q|\\.C|\\^", term)) %>%
  mutate(sig = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ ".",
    TRUE            ~ ""
  ))

# 7. Map clean labels 
label_map <- c(
  "q3a2-4" = "Business Size: 2–4 employees",
  "q3a5-9" = "Business Size: 5–9 employees",
  "q3a10-19" = "Business Size: 10–19 employees",
  "q3a20-49" = "Business Size: 20–49 employees",
  "q3a50-99" = "Business Size: 50–99 employees",
  "q3a100+" = "Business Size: 100+ employees",
  "q3aUnavailable" = "Business Size: Unavailable",
  "q11aaNot Applicable" = "Revenue: Not Applicable",
  "q11aa$500 or less" = "Revenue: $500 or less",
  "q11aa$501 to $2,500" = "Revenue: $501 to $2,500",
  "q11aa$2,501 to $5,000" = "Revenue: $2,501 to $5,000",
  "q11aa$5,001 to $10,000" = "Revenue: $5,001 to $10,000",
  "q11aa$10,001 to $15,000" = "Revenue: $10,001 to $15,000",
  "q11aa$15,001 to $25,000" = "Revenue: $15,001 to $25,000",
  "q11aa$25,001 to $50,000" = "Revenue: $25,001 to $50,000",
  "q11aaOver $50,000" = "Revenue: Over $50,000",
  "q93Completely Disagree" = "Belief: Completely Disagree",
  "q93Somewhat Disagree" = "Belief: Somewhat Disagree",
  "q93Neutral" = "Belief: Neutral",
  "q93Somewhat Agree" = "Belief: Somewhat Agree",
  "q93Completely Agree" = "Belief: Completely Agree",
  "d1Female" = "Gender: Female",
  "d1Non-Binary" = "Gender: Non-Binary",
  "d1Unavailable" = "Gender: Unavailable",
  "age_group20-40" = "Age Group: 20–40",
  "age_group40-60" = "Age Group: 40–60",
  "age_group60+" = "Age Group: 60+",
  "age_groupUnavailable" = "Age Group: Unavailable",
  "q5b_11" = "Industry: Accommodations",
  "q5b_21" = "Industry: Admin & Support",
  "q5b_31" = "Industry: Agriculture",
  "q5b_41" = "Industry: Art & Design",
  "q5b_51" = "Industry: Beauty Services",
  "q5b_61" = "Industry: Charitable/Political",
  "q5b_71" = "Industry: Construction",
  "q5b_81" = "Industry: Consumer Services",
  "q5b_91" = "Industry: Education",
  "q5b_101" = "Industry: Entertainment",
  "q5b_111" = "Industry: Financial Services",
  "q5b_121" = "Industry: Fashion",
  "q5b_131" = "Industry: Healthcare",
  "q5b_141" = "Industry: Home Services",
  "q5b_151" = "Industry: IT/Managed Services",
  "q5b_161" = "Industry: Legal Services",
  "q5b_171" = "Industry: Manufacturing/Wholesale",
  "q5b_181" = "Industry: Marketing/PR",
  "q5b_191" = "Industry: Media/Content",
  "q5b_201" = "Industry: Packaged Food",
  "q5b_211" = "Industry: Professional Services",
  "q5b_221" = "Industry: Personal Services",
  "q5b_231" = "Industry: Real Estate",
  "q5b_241" = "Industry: Restaurant",
  "q5b_251" = "Industry: Retail",
  "q5b_261" = "Industry: Tech & Telecom",
  "q5b_271" = "Industry: Transport/Auto",
  "q5b_281" = "Industry: Web Design/Marketing",
  "q5b_291" = "Industry: Other"
)

tidy_model$label <- ifelse(
  tidy_model$term %in% names(label_map),
  label_map[tidy_model$term],
  tidy_model$term
)

# 8. Show main regression output
 log_reg_model<- kable(
  tidy_model[, c("label", "estimate", "std.error", "statistic", "p.value", "sig")],
  col.names = c("Variable", "Estimate", "Std. Error", "z value", "Pr(>|z|)", "Signif."),
  caption = "Logistic Regression Coefficients (AI Adoption)"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE
  )

# 9. Add model fit statistics
model_stats <- list(
  Null_Deviance     = logit_model$null.deviance,
  Residual_Deviance = logit_model$deviance,
  AIC               = AIC(logit_model),
  McFadden_R2       = pR2(logit_model)["McFadden"]
)


## Interpretation McFadden R² :
# Values range from 0 to <1

# Closer to 1 = better model fit.

# In practice:

#   0.2 – 0.4 is considered excellent

# 0.4 is rare in social sciences


# 10. Predict on the test set
pred_probs   <- predict(logit_model, newdata = testData, type = "response")
# Determine which factor level is the "positive" outcome
positive_class <- levels(trainData$q80_3)[2]
# Convert probabilities to class labels at 0.5 threshold
pred_class <- factor(
  ifelse(pred_probs > 0.5, positive_class, levels(trainData$q80_3)[1]),
  levels = levels(trainData$q80_3)
)

# 11. Confusion matrix
conf_mat <- table(Predicted = pred_class, Actual = testData$q80_3)


# 12. Compute accuracies
model_accuracy     <- sum(diag(conf_mat)) / sum(conf_mat)
# Benchmark: always predict the most frequent class in the test set
maj_class          <- names(which.max(table(testData$q80_3)))
benchmark_accuracy <- max(table(testData$q80_3)) / nrow(testData)











#### R Shiny Dashboard ####



#### ==== START SHINY DASHBOARD ==== ###

library(shiny)
library(shinydashboard)
library(patchwork) 
library(plotly)
library(shinythemes)
library(DT)
library(usmap)
library(ggplot2)





ui <- dashboardPage(
  skin = "blue",  
  dashboardHeader(
    titleWidth = 250,
    title = "GoDaddy AI Dashboard"
  ),
  
  dashboardSidebar(
    width = 250,
    # collapse by default
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Home", tabName = "cover", icon = icon("home")),
      menuItem("Overview",           tabName = "home",       icon = icon("tachometer-alt")),
      menuItem("Industry",       tabName = "industry",   icon = icon("industry")),
      menuItem("Revenue & Size", tabName = "rev_size",   icon = icon("chart-bar")),
      menuItem("Age & Gender",   tabName = "age_gender", icon = icon("users")),
      menuItem("AI Usage",       tabName = "ai_usage",   icon = icon("robot")),
      menuItem("Barriers",       tabName = "barriers",   icon = icon("ban")),
      menuItem("Predictive Model", tabName = "predictive",icon = icon("magic")),
      menuItem("Descriptive Model", tabName = "descriptive", icon = icon("sitemap"))
    )
  ),
  
  dashboardBody(
    # 2) In the <head> add  brand font + override the default colors
    tags$head(
      # Load GT America
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Work+Sans:wght@400;600;700&display=swap"
      ),
      tags$style(HTML("
        /* Use GoDaddy font everywhere */
        body, .sidebar, .main-header .logo, .box, .nav-tabs {
          font-family: 'Work Sans', sans-serif !important;
        }
        /* Header background */
        .skin-blue .main-header .navbar {
          background-color: #1BD8DB !important;
        }
        .skin-blue .main-header .logo {
          background-color: #191919 !important;
          color: white !important;
          font-weight: 700;
          font-size: 1.2em;
        }
        /* Sidebar background + text */
        .skin-blue .main-sidebar {
          background-color: #191919 !important;
        }
        .skin-blue .main-sidebar .sidebar-menu > li > a {
          color: #EAEAEA !important;
        }
        .skin-blue .main-sidebar .sidebar-menu > li.active > a {
          background-color: #1BD8DB !important;
          color: #191919 !important;
        }
        /* Box headers in brand teal */
        .box.box-solid.box-primary > .box-header {
          background: #1BD8DB !important;
          color: #191919 !important;
        }
        /* ValueBoxes in accent colors */
        .small-box.bg-aqua { background-color: #1BD8DB !important; }
        .small-box.bg-purple { background-color: #191919 !important; }
        .small-box.bg-orange { background-color: #FF8C00 !important; }
        .small-box.bg-green  { background-color: #00A651 !important; }
        /* make all shinydashboard box titles bigger & bold */
        .skin-blue .box.box-solid > .box-header .box-title {
        font-size: 1.4em !important;
        font-weight: 700 !important;
        }

      "))
    ),
    
    tabItems(
      
      
      tabItem(
        tabName = "cover",
        fluidRow(
          box(
            title = "Welcome to the GoDaddy AI Dashboard",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            h4("Overview"),
            p("This dashboard explores small‑business AI adoption and confidence: who’s using it, why (or why not), and what factors drive adoption and confidence."),
            h4("Navigate to:"),
            tags$ul(
             
               tags$li(
                tags$a("Overview"),
                tags$ul(
                  tags$li("Overview with value boxes, interactive gender & ethnicity donut charts, and a US region choropleth."),
                  tags$li("Great for a quick snapshot of overall adoption and demographic breakdowns.")
                )
              ),
              tags$li(
                tags$a("Industry"),
                tags$ul(
                  tags$li("Bar chart: AI adoption vs non‑adoption counts by industry with Overlay adoption‑rate labels"),
                  tags$li("Use to identify top and lagging sectors")
                )
              ),
              tags$li(
                tags$a("Revenue & Size"),
                tags$ul(
                  tags$li("Two stacked‑bar panels showing AI adoption and AI Confidence by revenue and company size. Controls for filtering by revenue or size bins"),
                  tags$li("Ideal for understanding scale and revenue correlations")
                  )
                ),
              tags$li(
                tags$a("Age & Gender"),
                tags$ul(
                  tags$li("Faceted stacked‑bar of adoption rates and confidence by age group and gender.  Filters for selecting age brackets, genders, and confidence levels."),
                  tags$li("Enables deep dive into demographic adoption and comfort with AI.")
                )
              ),
              tags$li(
                tags$a("AI Usage"),
                tags$ul(
                  tags$li("Pie chart of most‑used AI tools and bar chart for AI service use. Slider to limit top N tools, radio buttons to sort tools or services."),
                  tags$li("Shows which AI products and services are driving adoption.")
                )
              ),
              tags$li(
                tags$a("Barriers"),
                tags$ul(
                  tags$li("Bar chart of primary reasons for not trying AI, and adopter vs non‑adopter business challenge comparisons."),
                  tags$li("Dropdowns to sort reasons/challenges alphabetically or by frequency."),
                  tags$li("Helps understand key obstacles to adoption and common business challenges faced.")
                )
              ),
              tags$li(
                tags$a("Predictive Model"),
                tags$ul(
                  tags$li("Interactive logistic regression inputs:"),
                  tags$ul(
                    tags$li("Business size, revenue band"),
                    tags$li("Belief about AI, gender, age, industry")
                  ),
                  tags$li("Interactive logistic regression predictive model. Choose predictors' categories and predict adoption probability."),
                  tags$li("Useful to simulate how changes in predictors affect adoption likelihood.")
                )
              ),
              tags$li(
                tags$a("Descriptive Model"),
                tags$ul(
                  tags$li("Classification‑tree visualization of what drives AI confidence."),
                  tags$li("Use the tree to see key splits and interpret how factors segment confidence levels.")
                )
              )
          )
        )
      )
    ),
      
      
      
      

      tabItem(tabName = "home",
              fluidRow(
                valueBoxOutput("vb_total_respondents", width = 3),
                valueBoxOutput("vb_ai_adopters", width = 3),
                valueBoxOutput("vb_ai_nonadopters", width = 3),      
                valueBoxOutput("vb_avg_confidence", width = 3)
              ),
              fluidRow(
                box(title = "Gender Distribution", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("gender_donut")),
                box(title = "Ethnicity Distribution", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("ethnicity_donut"))
              ),
              fluidRow(
                box(
                  title = "AI Adoption by US Region",
                  width = 12, status = "info", solidHeader = TRUE,
                  plotlyOutput("region_map", height = "400px"))
              )
      ),
      
      tabItem(tabName = "industry",
              fluidRow(
                box(width=4,
                    sliderInput("num_industries","# Industries:",5,29,10,1),
                    radioButtons("sort_by","Sort by:",c("Adoption"="adoption","Confidence"="confidence"))
                ),
                box(width=8, plotlyOutput("industryPlot", height="800px"))
              )
      ),
      
      tabItem(tabName = "rev_size",
              
              ## first row: revenue/size filter + “Show” selector → first chart
              fluidRow(
                box(
                  width = 4,
                  selectInput(
                    inputId = "rev_size_filter_by",
                    label   = "Filter by:",
                    choices = c("Revenue bands" = "revenue", "Size bands" = "size")
                  ),
                  uiOutput("rev_size_values_ui"),
                  radioButtons(
                    inputId = "show_metric",
                    label   = "Show:",
                    choices = c("Pct" = "pct", "Count" = "count")
                  )
                ),
                box(
                  width = 8,
                  plotlyOutput("bizSizeRevenuePlot", height = "600px")
                )
              ),
              
              ## second row: confidence metric + sort selector → second chart
              fluidRow(
                box(
                  width = 4,
                  radioButtons(
                    inputId = "conf_metric",
                    label   = "Confidence Metric:",
                    choices = c(
                      "Average Confidence" = "avg",
                      "% Very Confident"   = "pct_high",
                      "% Least Confident"  = "pct_low"),
                    selected = "avg"
                  ),
                  selectInput(
                    inputId = "sort_conf",
                    label   = "Sort Bins By:",
                    choices = c(
                      "Desc. Confidence" = "desc",
                      "Asc. Confidence"  = "asc",
                      "Sample Size"      = "size",
                      "Alphabetical"     = "alpha"
                    ),
                    selected = "desc"
                  )
                ),
                box(
                  width = 8,
                  plotlyOutput("ai_confidence_by_revenue", height = "600px")
                )
              )
              
      ),
      
      tabItem(tabName = "age_gender",
              
              ## first row: age + gender filter → adoption chart
              fluidRow(
                box(
                  width = 4,
                  checkboxGroupInput(
                    "age_groups", "Ages:",
                    choices = levels(df_cleaned$age_group),
                    selected = levels(df_cleaned$age_group)
                  ),
                  checkboxGroupInput(
                    "genders", "Genders:",
                    choices = levels(df_cleaned$d1),
                    selected = levels(df_cleaned$d1)
                  ),
                  radioButtons(
                    "adopt_metric", "Adopt metric:",
                    c("Pct" = "pct", "Count" = "count")
                  )
                ),
                box(
                  width = 8,
                  plotlyOutput("age_gender_plot", height = "500px")
                )
              ),
              
              ## second row: confidence‐level filter → confidence chart
              fluidRow(
                box(
                  width = 4,
                  checkboxGroupInput(
                    "conf_levels", "Keep Confidence Levels:",
                    choices = c("Not Confident","2","3","Neutral","5","6","Very Confident"),
                    selected = c("Not Confident","2","3","Neutral","5","6","Very Confident")
                  )
                ),
                box(
                  width = 8,
                  plotlyOutput("age_gender_plot_conf", height = "600px")
                )
              )
              
      ),
      
      tabItem(tabName = "ai_usage",
              
              # first row: first filter + first chart
              fluidRow(
                box(
                  width = 4,
                  sliderInput(
                    "num_tools", "# Tools:", 
                    min = 3, max = 17, value = 10, step = 1
                  ),
                  radioButtons(
                    "sort_tools", "Sort Tools:",
                    c("Count" = "count", "Alphabetical" = "alpha")
                  )
                ),
                box(
                  width = 8,
                  plotlyOutput("tools_pie", height = "500px")
                )
              ),
              
              # second row: second filter + second chart
              fluidRow(
                box(
                  width = 4,
                  radioButtons(
                    "sort_services", "Sort Services:",
                    c("Count" = "count", "Alphabetical" = "alpha")
                  )
                ),
                box(
                  width = 8,
                  plotlyOutput("serv_bar", height = "500px")
                )
              )
              
      ),
      
      
      tabItem(tabName = "barriers",
              
              # first row: only the “Sort Reasons” filter + first chart
              fluidRow(
                box(
                  width = 2,
                  selectInput(
                    "sort_reasons", "Sort Reasons:",
                    c("Count" = "count", "Alphabetical" = "alpha")
                  )
                ),
                box(
                  width = 10,
                  plotlyOutput("reasons_ai", height = "400px")
                )
              ),
              
              # second row: only the “Sort Challenges” filter + second chart
              fluidRow(
                box(
                  width = 2,
                  selectInput(
                    "sort_challenges", "Sort Challenges:",
                    c(
                      "Adopters"     = "adopters",
                      "Non‑Adopters" = "nonadopters",
                      "Alphabetical"        = "alpha"
                    )
                  )
                ),
                box(
                  width = 10,
                  plotlyOutput("bus_chall", height = "600px")
                )
              )
              
      ),
      
      tabItem(tabName = "predictive",
              fluidRow(
                box(
                  title = "Choose Predictor Values", status = "primary", solidHeader = TRUE,
                  width = 4,
                  selectInput("pred_size",   "Business Size:",   choices = levels(df_cleaned$q3a),    selected = "Just Myself"),
                  selectInput("pred_rev",    "Revenue Band:",    choices = levels(df_cleaned$q11aa), selected = "$500 or less"),
                  selectInput("pred_belief", "AI will help small businesses compete with large businesses:",
                              choices = levels(df_cleaned$q93), selected = "Completely Disagree"),
                  selectInput("pred_gender", "Gender:",          choices = levels(df_cleaned$d1),    selected = "Female"),
                  selectInput("pred_age",    "Age Group:",       choices = levels(df_cleaned$age_group), selected = "20-40"),
                  selectInput("pred_industry","Industry:",
                              choices = setNames(names(industry_names), industry_names),
                              selected = "q5b_18"),
                  actionButton("btn_predict", "Predict AI Adoption", icon = icon("paper-plane"))
                ),
                column(width = 8,
                       box(
                         title = "Prediction Result", status = "success", solidHeader = TRUE,
                         width = 12,
                         htmlOutput("pred_text"),
                         br(),
                         strong("Probability:"), textOutput("pred_prob"), br(),
                         strong("Predicted Class:"), textOutput("pred_class")
                       ),
                       box(
                         title = "Model Performance", status = "warning", solidHeader = TRUE,
                         width = 12,
                         p("These statistics were computed on the held‑out test set."),
                         tags$ul(
                           tags$li(strong("Accuracy:"), textOutput("model_accuracy", inline = TRUE)),
                           tags$li(strong("Benchmark Accuracy:"), textOutput("benchmark_accuracy", inline = TRUE)),
                           tags$li(strong("McFadden R²:"), textOutput("mcfadden_r2", inline = TRUE))
                         )
                       )
                )
              ),
              fluidRow(
                ## note on the left
                column(
                  width = 2,
                  box(
                    title = "Significance Note",
                    status = "info", solidHeader = TRUE,
                    width = NULL,
                    p(
                      strong("Significant:"), 
                      " p‑values < 0.05 → unlikely due to chance"
                    )
                  )
                ),
                column(
                  width = 10,
                  box(
                    title = "Model Coefficients",
                    status = "info", solidHeader = TRUE,
                    width = NULL,
                    checkboxInput("show_full_coef", "Show full coefficient table", FALSE),
                    DT::dataTableOutput("coef_table")
                  )
                ),
                column(width = 1)
              )
      ),
      
      tabItem(tabName = "descriptive",
              
              ## first row: controls only
              fluidRow(
                box(
                  title = "Tree Controls", status = "warning", width = 4, solidHeader = TRUE,
                  sliderInput("cp",       "Prune Complexity (CP):",
                              min = 0, max = 0.05, step = 0.0025, value = 0.01),
                  numericInput("minsplit","Min observations to split:", value = 20, min = 2, max = 100),
                  numericInput("maxdepth","Max tree depth:",            value = 5,  min = 1, max = 30)
                ),
                column(
                  width = 8,    # same width as the controls box
                  box(
                    title = "What do these settings mean?", status = "info", solidHeader = TRUE, width = NULL,
                    p(strong("CP (Complexity Parameter):"),
                      " Controls how aggressive pruning is. Higher CP → fewer splits (simpler tree), lower CP → more splits (more complex tree)."
                    ),
                    p(strong("Minsplit:"), 
                      " Minimum number of observations in a node before it can be split. Larger values → fewer splits → simpler tree."
                    ),
                    p(strong("Max Depth:"), 
                      " Maximum number of levels in the tree. Smaller depth → shallower tree, larger → more layers of splits."
                    )
                  )
                )
              ),
              
              ## third row: the tree itself full‑width
              fluidRow(
                box(
                  title = "Classification Tree", status = "danger", width = 12, solidHeader = TRUE,
                  plotOutput("class_tree_plot", height = "500px")
                )
              )
      )
      
      
    )
  )
)



server <- function(input, output, session) {
  
  library(plotly)
  library(patchwork) 
  library(usmap)
  library(ggplot2)
  
  
  output$vb_total_respondents <- renderValueBox({
    valueBox(
      formatC(nrow(df_cleaned), big.mark=","), strong("Total Respondents"),
      icon = icon("users"), color = "aqua"
    )
  })
  output$vb_ai_adopters <- renderValueBox({
    n_adopters <- sum(df_cleaned$q80_3=="1", na.rm=TRUE)
    valueBox(
      formatC(n_adopters, big.mark=","), strong("AI Adopters"),
      icon = icon("robot"), color = "purple"
    )
  })
  output$vb_ai_nonadopters <- renderValueBox({
    n_non <- sum(df_cleaned$q80_3 == "0", na.rm = TRUE)
    valueBox(
      formatC(n_non, big.mark = ","), strong("AI Non‑Adopters"),
      icon = icon("user-times"), color = "orange"
    )
  })
  output$vb_avg_confidence <- renderValueBox({
    avg_conf <- mean(df_cleaned$q83_num[df_cleaned$q80_3=="1"], na.rm=TRUE)
    valueBox(
      round(avg_conf,2), strong("Avg AI Confidence"),
      icon = icon("chart-line"), color = "green"
    )
  })
  output$gender_donut <- renderPlotly({
    df_gender <- df_cleaned %>%
      mutate(Gender = recode(d1,
                             "1"  = "Male",
                             "2"  = "Female",
                             "97" = "Non‑Binary",
                             "99" = "Prefer not to answer"
      )) %>%
      count(Gender)
    
    # define your GoDaddy palette
    godaddy_cols <- c(
      "Male"               = "#00A651",  # GoDaddy green
      "Female"             = "#1BD8DB",  # GoDaddy teal
      "Non‑Binary"         = "#B69AE2",  # a nod to brand purple
      "Prefer not to answer" = "#FF8C00"  # accent orange
    )
    
    plot_ly(
      df_gender,
      labels = ~Gender,
      values = ~n,
      type   = "pie",
      hole   = 0.6,
      textinfo  = "label+percent",
      hoverinfo = "label+value+percent",
      marker = list(colors = godaddy_cols[df_gender$Gender])
    ) %>%
      layout(
        title = list(text = "Gender", x = 0.5, xanchor = "center"),
        showlegend = TRUE
      )
  })
  output$ethnicity_donut <- renderPlotly({
    # 1) define your palette
    eth_colors <- c(
      "White"                           = "#5bdbef",
      "Black / African American"        = "#BDC0EE",
      "Asian / Pacific Islander"        = "#73CFEB",
      "American Indian / Alaska Native" = "#8CDFEC",
      "Other / Multi‑racial"            = "#A4CFEE",
      "Prefer not to answer"            = "#C3B1E1",
      "Unavailable"                     = "#EAEAEA"
    )
    
    # 2) recode, count, and force the factor levels to match your palette
    df_eth <- df1 %>%
      mutate(
        Ethnicity = recode(
          as.character(d3),
          "1"  = "White",
          "2"  = "Black / African American",
          "3"  = "Asian / Pacific Islander",
          "4"  = "American Indian / Alaska Native",
          "5"  = "Other / Multi‑racial",
          "99" = "Prefer not to answer",
          .default = "Unavailable"
        ),
        Ethnicity = factor(Ethnicity, levels = names(eth_colors))
      ) %>%
      count(Ethnicity)
    
    # 3) build the donut
    plot_ly(
      df_eth,
      labels    = ~Ethnicity,
      values    = ~n,
      type      = "pie",
      hole      = 0.6,
      sort      = FALSE,            # keep the order of the factor
      marker    = list(colors = eth_colors),
      textinfo  = "label+percent",
      hoverinfo = "label+value+percent"
    ) %>%
      layout(
        title      = list(text = "Ethnicity", x = 0.5, xanchor = "center"),
        showlegend = TRUE
      )
  })
  
  #### Regional Adoption Map ###

  
  # 1) Compute region adoption rates, turning s4 codes into names
  region_rates <- df %>%
    filter(!is.na(s4), s4 != 5) %>%   # drop International
    mutate(region = recode(
      as.character(s4),
      "1" = "Northeast", "2" = "Midwest",
      "3" = "South",     "4" = "West"
    )) %>%
    group_by(region) %>%
    summarise(
      AdoptionRate = round(100 * mean(q80_3 == "1", na.rm = TRUE), 1),
      .groups = "drop"
    )
  
  # 2) Build  state→region lookup (region already character)
  state_region <- data.frame(
    state  = state.abb,
    region = state.region,
    stringsAsFactors = FALSE
  ) %>%
    mutate(region = recode(region, "North Central" = "Midwest"))
  
  # 3) Now join on character `region`
  map_data <- left_join(state_region, region_rates, by = "region")
  
  map_data <- map_data %>%
    rename(`Adoption Rate (%)` = AdoptionRate)
  
  # 4) And render:
  output$region_map <- renderPlotly({
    p <- plot_usmap(
      data    = map_data,
      regions = "states",
      values  = "Adoption Rate (%)"
    ) +
      scale_fill_continuous(
        name   = "Adoption %",
        low    = "#B2DFDB",
        high   = "#004D40",
        labels = scales::percent_format(scale = 1)
      ) +
      labs(title = "AI Adoption by US Region") +
      theme(legend.position = "right")
    
    ggplotly(p, tooltip = c("state", "Adoption Rate (%)"))
  })
  

  
  
  
  
  
  
  
  
  
  output$industryPlot <- renderPlotly({
    # Filter to AI Adopted data
    industry_plot_data <- industry_ai_stats %>%
      filter(AI_Use == "AI Adopted")
    
    # Sorting logic
    sorted_industries <- switch(input$sort_by,
                                adoption = industry_plot_data %>%
                                  arrange(desc(AI_Adoption_Rate)),
                                confidence = industry_plot_data %>%
                                  arrange(desc(Avg_Confidence)))
    
    top_industries <- head(sorted_industries$Industry, input$num_industries)
    
    plot_data <- industry_ai_stats %>%
      filter(Industry %in% top_industries) %>%
      mutate(Industry = factor(Industry, levels = rev(unique(top_industries))))
    
    plt <- ggplot(plot_data, aes(
      x     = Industry,
      y     = Count,
      fill  = AI_Use,
      # inject confidence & total into the hover text
      text  = ifelse(
        AI_Use=="AI Adopted",
        paste0(
          "AI Confidence: ", round(Avg_Confidence,2), "<br>",
          "Total:   ", Total_Industry_Count
        ),
        NA
      )
    )) +
      geom_bar(stat="identity", position=position_dodge(width=0.9)) +
      
      # keep just the adoption‐percent label on‐bar
      geom_text(
        data = subset(plot_data, AI_Use=="AI Adopted"),
        aes(label = paste0(round(AI_Adoption_Rate,1),"%")),
        position = position_dodge(width=0.9),
        vjust = 0.5, hjust = 0.5,
        color = "black", size = 3.5, fontface = "bold"
      ) +
      
      coord_flip() +
      scale_fill_manual(values = c(
        "AI Adopted"   = "#5bdbef",
        "Not Adopted"  = "#EAEAEA"
      )) +
      labs(
        title = "AI Adoption & Confidence by Industry",
        x     = "Industry",
        y     = "Count",
        fill  = "AI Use"
      ) +
      theme_minimal()
    
    # only one ggplotly() call, and include  `text` in the tooltip
    ggplotly(plt, tooltip = c("x","y","text"))
  })
  
  

  output$rev_size_values_ui <- renderUI({
    if (input$rev_size_filter_by == "revenue") {
      checkboxGroupInput(
        inputId  = "rev_cats",
        label    = "Revenue bands:",
        choices  = levels(df_cleaned$q11aa),
        selected = levels(df_cleaned$q11aa)
      )
    } else {
      checkboxGroupInput(
        inputId  = "size_cats",
        label    = "Size bands:",
        choices  = levels(df_cleaned$q3a),
        selected = levels(df_cleaned$q3a)
      )
    }
  })
  
  
  filtered_revenue <- reactive({
    df_combined %>%
      filter(Category == "Business Revenue",
             Group %in% input$rev_cats)
  })
  
  filtered_size <- reactive({
    df_combined %>%
      filter(Category == "Business Size",
             Group %in% input$size_cats)
    })
  
  output$bizSizeRevenuePlot <- renderPlotly({
    req(input$rev_cats, input$size_cats, input$show_metric)
    # grab the two datasets
    d_rev  <- filtered_revenue()
    d_size <- filtered_size()
    
    # helper to build a ggplot for either pct or count
    build_bar <- function(df) {
      p <- if (input$show_metric == "pct") {
        ggplot(df, aes(x = Group, y = Proportion, fill = q80_3)) +
          geom_col(position = "stack") +
          geom_text(aes(label = paste0(round(Proportion,1), "%")),
                    position = position_stack(vjust = 0.5), size = 3) +
          labs(y = "Percent of Respondents")
      } else {
        ggplot(df, aes(x = Group, y = Total_Count, fill = q80_3)) +
          geom_col(position = "stack") +
          geom_text(aes(label = Total_Count),
                    position = position_stack(vjust = 0.5), size = 3) +
          labs(y = "Raw Count of Respondents")
      }
      p +
        scale_fill_manual(
          name   = "AI Adoption",
          values = c("0" = "#EAEAEA", "1" = "#5bdbef"),
          labels = c("0" = "Non‑Adopters", "1" = "Adopters")
        ) +
        labs(x = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    # build each ggplot
    p_size <- build_bar(d_size)
    p_rev  <- build_bar(d_rev)
    
    # convert to plotly and add individual titles
    pl_size <- ggplotly(p_size, tooltip = c("x","y")) %>%
      layout(
        title = list(text = "AI Adoption by Business Size & Revenue", x = 0.02, xanchor = "left")
      )
    
    pl_rev  <- ggplotly(p_rev, tooltip = c("x","y")) %>%
      layout(
        title = list(text = "AI Adoption by Business Size & Revenue", x = 0.02, xanchor = "left")
      )
    
    # stack them vertically
    subplot(
      pl_size,
      pl_rev,
      nrows     = 2,
      shareX    = FALSE,
      shareY    = FALSE,
      margin    = 0.05
    ) %>%
      layout(showlegend = FALSE)
  })
  
  
  
  
  output$ai_confidence_by_revenue <- renderPlotly({
    
    df_rev <- df_filtered %>%
      group_by(q11aa) %>%
      summarise(
        AvgConf = mean(q83_num, na.rm = TRUE),
        PctHigh = 100 * mean(q83_num >= 6, na.rm = TRUE),
        PctLow  = 100 * mean(q83_num <= 2, na.rm = TRUE),
        Total   = n(),
        .groups = "drop"
      ) %>%
      mutate(
        y_val = case_when(
          input$conf_metric == "avg"      ~ AvgConf,
          input$conf_metric == "pct_high" ~ PctHigh,
          input$conf_metric == "pct_low"  ~ PctLow
          )
      )
    
    df_size <- df_filtered %>%
      group_by(q3a) %>%
      summarise(
        AvgConf = mean(q83_num, na.rm = TRUE),
        PctHigh = 100 * mean(q83_num >= 6, na.rm = TRUE),
        PctLow  = 100 * mean(q83_num <= 2, na.rm = TRUE),
        Total   = n(),
        .groups = "drop"
      ) %>%
      mutate(
        y_val = case_when(
          input$conf_metric == "avg"      ~ AvgConf,
          input$conf_metric == "pct_high" ~ PctHigh,
          input$conf_metric == "pct_low"  ~ PctLow
        )
      )
    
    # 2) apply sorting
    sort_df <- function(df, xvar) {
      df <- switch(input$sort_conf,
                   desc  = df %>% arrange(desc(y_val)),
                   asc   = df %>% arrange(y_val),
                   size  = df %>% arrange(desc(Total)),
                   alpha = df %>% arrange(!!sym(xvar))
      )
      df[[xvar]] <- factor(df[[xvar]], levels = df[[xvar]])
      df
    }
    df_rev  <- sort_df(df_rev,  "q11aa")
    df_size <- sort_df(df_size, "q3a")
    
    # 3) build the two ggplots
    y_label <- switch(input$conf_metric,
                      avg      = "Avg Confidence (1–7)",
                      pct_high = "% Very Confident",
                      pct_low  = "% Least Confident"
    )
    
    
    p_rev <- ggplot(df_rev, aes(x = q11aa, y = y_val,text = paste0(
      switch(input$conf_metric,
             avg      = "Avg Conf: ",
             pct_high = "% Very Conf: ",
             pct_low  = "% Least Conf: "
      ),
      round(y_val,2),
      "<br>Total: ", Total)
    )) + 
      
      
      
      geom_col(fill = "#5bdbef", alpha = 0.8) +
      labs(title = "AI Confidence by Business Size & Revenue",
           x     = "Business Revenue",
           y     = y_label) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    p_size <- ggplot(df_size, aes(x = q3a, y = y_val,text = paste0(
      switch(input$conf_metric,
             avg      = "Avg Conf: ",
             pct_high = "% Very Conf: ",
             pct_low  = "% Least Conf: "
      ),
      round(y_val,2),
      "<br>Total: ", Total
    )
    
    ))+
      
      geom_col(fill = "#b69ae2", alpha = 0.8) +
      labs(title = "AI Confidence by Business Size & Revenue",
           x     = "Business Size",
           y     = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # 4) stitch them together in plotly
    subplot(
      ggplotly(p_rev,  tooltip = "text"),
      ggplotly(p_size, tooltip = "text"),
      nrows    = 1,
      shareY   = TRUE
    ) %>%
      layout(showlegend = FALSE)
  })
  
  
  filtered_age_gender <- reactive({
    req(input$age_groups, input$genders)
    df_cleaned %>%
      filter(
        age_group %in% input$age_groups,
        d1        %in% input$genders
      )
  })
  
  output$age_gender_plot <- renderPlotly({
    d <- filtered_age_gender() %>%
      mutate(
        Adoption_Status = ifelse(q80_3 == "1","Adopter","Non-Adopter")
      ) %>%
      group_by(age_group, d1, Adoption_Status) %>%
      summarise(Count = n(), .groups="drop") %>%
      group_by(age_group, d1) %>%
      mutate(
        Percent = 100 * Count / sum(Count),
        Total   = sum(Count)
      ) %>%
      ungroup()
    
    yvar <- if (input$adopt_metric=="pct") "Percent" else "Count"
    ylab <- if (input$adopt_metric=="pct") "Percent of Respondents" else "Raw Count"
    
    plt <- ggplot(d, aes(
      x    = age_group,
      y    = .data[[yvar]],
      fill = Adoption_Status,
      text = paste0(
        "Status: ", Adoption_Status, "<br>",
        if (input$adopt_metric=="pct")
          paste0("Pct: ", round(Percent,1), "%")
        else
          paste0("Count: ", Count),
        "<br>Total: ", Total
      )
    )) +
      geom_col(position = "stack", width = 0.7) +
      facet_wrap(~ d1) +
      labs(x = "Age Group", y = ylab) +
      theme_minimal() +
      scale_fill_manual(
        name   = "Adoption Status",
        values = c(
          "Adopter"     = "#5bdbef",  
          "Non-Adopter" = "#EAEAEA"  
        )
      ) +
      theme(axis.text.x = element_text(angle=45, hjust=1)) +
      labs(title = "AI Adoption by Age & Gender")
    
    ggplotly(plt, tooltip="text")
  })
  
  
  
  output$age_gender_plot_conf <- renderPlotly({
    
    custom_conf_cols <- c(
      "Not Confident"   = "#5bdbef",
      "2"               = "#73cfeb",
      "3"               = "#8cdfec",
      "Neutral"         = "#a4cfee",
      "5"               = "#bdc0ee",
      "6"               = "#d5b1ee",
      "Very Confident"  = "#b69ae2"
    )
    
    valid <- input$conf_levels
    d <- filtered_age_gender() %>%
      mutate(conf_level = q83) %>%
      filter(conf_level %in% valid) %>%
      group_by(age_group, d1, conf_level) %>%
      summarise(Count = n(), .groups="drop") %>%
      group_by(age_group, d1) %>%
      mutate(
        Percent = 100 * Count / sum(Count),
        Total   = sum(Count)
      ) %>%
      ungroup()
    
    # preserve order of selected levels
    d$conf_level <- factor(d$conf_level, levels = valid)
    
    plt2 <- ggplot(d, aes(
      x    = age_group,
      y    = Percent,
      fill = conf_level,
      text = paste0(
        "Conf level: ", conf_level, "<br>",
        "Pct: ", round(Percent,1), "%<br>",
        "Total: ", Total
      )
    )) +
      geom_col(stat="identity", position="fill", width=0.7) +
      facet_wrap(~ d1) +
      labs(x="Age Group", y="Proportion", fill="AI Confidence") +
      theme_minimal() +
      scale_fill_manual(
        name   = "AI Confidence",
        values = custom_conf_cols,
        drop   = FALSE
      )+
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    labs(title = "AI Confidence by Age & Gender")
    
    ggplotly(plt2, tooltip="text")
  })
  
  
  
  output$tools_pie <- renderPlotly({
    # first filter to only AI adopters
    df_ai_adopters <- df_cleaned %>% filter(q80_3 == "1")
    
    # build the counts exactly as before
    ai_tool_columns <- paste0("q81_", 1:17)
    ai_tool_columns <- intersect(ai_tool_columns, names(df_ai_adopters))
    ai_tool_labels <- c(
      "q81_1" = "ChatGPT", "q81_2" = "DALL‑E", "q81_3" = "Stable Diffusion",
      "q81_4" = "Jasper AI", "q81_5" = "Google Bard", "q81_6" = "Gemini",
      "q81_7" = "AlphaCode", "q81_8" = "Descript", "q81_9" = "Synthesia",
      "q81_10"= "Designs.ai", "q81_11"= "Bardeen", "q81_12"= "Claude",
      "q81_13"= "You.com", "q81_14"= "Canva (Magic Design/Writer)",
      "q81_15"= "GitHub Copilot X", "q81_16"= "Airo",   "q81_17"= "Other"
    )
    ai_tool_counts <- df_ai_adopters %>%
      dplyr::select(all_of(ai_tool_columns)) %>%
      summarise(across(everything(), ~ sum(. == "1", na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to="AI_Tool", values_to="Count") %>%
      mutate(AI_Tool = recode(AI_Tool, !!!ai_tool_labels))
    
    # limit & sort
    ai_tool_counts <- switch(input$sort_tools,
                             count = arrange(ai_tool_counts, desc(Count)),
                             alpha = arrange(ai_tool_counts, AI_Tool)
    ) %>% head(input$num_tools)
    
    # define GoDaddy palette
    custom_colors <- c(
      "ChatGPT" = "#3D348B",    "Canva (Magic Design/Writer)" = "#A996F5",
      "DALL‑E"  = "#8075A1",    "Google Bard"                 = "#A299C6",
      "Other"   = "#C3B1E1",    "Jasper AI"                   = "#0097A7",
      "Gemini"  = "#00ACC1",    "GitHub Copilot X"            = "#4DD0E1",
      "Stable Diffusion" = "#6FC0C4",  "Designs.ai"      = "#89D4D8",
      "Claude"  = "#A7E0E5",    "Descript"                    = "#E0FFFF",
      "Synthesia"= "#A8B6C3",    "You.com"                     = "#BBCDE5",
      "Airo"    = "#C3D9E3",    "Bardeen"                     = "#E2ECF3",
      "AlphaCode"= "#EBF5FA"
    )
    
    #  make the interactive pie
    plot_ly(
      data = ai_tool_counts,
      labels = ~AI_Tool,
      values = ~Count,
      type   = "pie",
      textinfo   = "label+percent",
      hoverinfo  = "label+value+percent",
      marker = list(colors = custom_colors[ai_tool_counts$AI_Tool])
    ) %>%
      layout(
        title = paste0("Most Commonly Used AI Tools (top ", input$num_tools, ")"),
        legend = list(orientation="v", x=1.05, y=0.5)
      )
  })
  
  

  output$serv_bar <- renderPlotly({
    # filter adopters once
    df_ai_adopters <- df_cleaned %>% filter(q80_3 == "1")
    
    # count services
    ai_service_columns <- paste0("q82_", 1:6)
    ai_service_columns <- intersect(ai_service_columns, names(df_ai_adopters))
    ai_service_labels <- c(
      "q82_1" = "Customer Service", "q82_2" = "Marketing",
      "q82_3" = "Content Creation",  "q82_4" = "Boosting Sales",
      "q82_5" = "Business Advice",   "q82_6" = "Other"
    )
    ai_service_counts <- df_ai_adopters %>%
      dplyr::select(all_of(ai_service_columns)) %>%
      summarise(across(everything(), ~ sum(. == "1", na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "AI_Service", values_to = "Count") %>%
      mutate(AI_Service = recode(AI_Service, !!!ai_service_labels))
    
    # sort by user choice
    ai_service_counts <- switch(input$sort_services,
                                count = ai_service_counts %>% arrange(desc(Count)),
                                alpha = ai_service_counts %>% arrange(AI_Service)
    )
    
    # build ggplot with a `text` aesthetic
    p <- ggplot(ai_service_counts, aes(
      x     = reorder(AI_Service, if (input$sort_services=="count") Count else AI_Service),
      y     = Count,
      fill  = AI_Service,
      text  = paste0("Service: ", AI_Service, "<br>Count: ", Count)
    )) +
      geom_col() +
      geom_text(aes(label = Count), vjust = -0.5, size = 4) +
      scale_fill_manual(values = setNames(
        colorRampPalette(c("#6A9FD8", "#B2EBF2"))(nrow(ai_service_counts)),
        ai_service_counts$AI_Service
      )) +
      labs(
        title = "Different Services for Which AI Is Used",
        x     = "Service",
        y     = "Number of Businesses"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    
    # convert to plotly, using our `text` for hover
    ggplotly(p, tooltip = "text")
  })
  
  
  
  
  output$reasons_ai <- renderPlotly({
    # build the count table
    df_non_adopters <- df_cleaned %>% filter(q80_4 == "1")
    ai_barrier_counts <- df_non_adopters %>%
      count(q89) %>%
      mutate(
        Barrier = recode(as.character(q89),
                         "1"="Not Familiar", "2"="No Need",
                         "3"="Too Long to Learn", "4"="Only Large Companies Benefit",
                         "5"="No Time", "6"="Too Expensive",
                         "7"="Other", "8"="Don't Know"
        )
      )
    # sort
    if (input$sort_reasons=="alpha") {
      ai_barrier_counts <- ai_barrier_counts %>% arrange(Barrier)
    } else {
      ai_barrier_counts <- ai_barrier_counts %>% arrange(desc(n))
    }
    ai_barrier_counts$Barrier <- factor(ai_barrier_counts$Barrier, levels=ai_barrier_counts$Barrier)
    
    custom_colors <- c(
      "Not Familiar" = "#006064",
      "No Need"      = "#00838F",
      "Too Long to Learn" = "#9FA8DA",
      "Only Large Companies Benefit" = "#D1C4E9",
      "No Time"      = "#00ACC1",
      "Too Expensive"= "#B39DDB",
      "Other"        = "#5A6A9E",
      "Don't Know"   = "#4DD0E1"
    )
    
    p <- ggplot(ai_barrier_counts, aes(
      x    = Barrier, y = n, fill = Barrier,
      text = paste0("Reason: ", Barrier, "<br>Count: ", n)
    )) +
      geom_col(show.legend=FALSE) +
      geom_text(aes(label=n), vjust=-0.5) +
      scale_fill_manual(values = custom_colors) +
      labs(
        title = paste0("Primary Reasons for Not Trying AI"),
        x = NULL, y = "Count"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1))
    
    ggplotly(p, tooltip="text")
  })
  
  
  
  
  output$bus_chall <- renderPlotly({
    df_challenges_profile <- df_final %>%
      mutate(adoption_status = ifelse(df_cleaned$q80_3=="1","Adopter","Non‑Adopter"))
    
    challenge_summary <- df_challenges_profile %>%
      pivot_longer(-adoption_status, names_to="Challenge", values_to="Selected") %>%
      filter(Selected==1) %>%
      count(Challenge, adoption_status, name="Count")
    
    # choose ordering
    ordering <- switch(input$sort_challenges,
                       adopters    = challenge_summary %>% filter(adoption_status=="Adopter") %>%
                         arrange(desc(Count)) %>% pull(Challenge),
                       nonadopters = challenge_summary %>% filter(adoption_status=="Non‑Adopter") %>%
                         arrange(desc(Count)) %>% pull(Challenge),
                       alpha       = sort(unique(challenge_summary$Challenge))
    ) %>% unique()
    challenge_summary$Challenge <- factor(challenge_summary$Challenge, levels=ordering)
    
    adopter_cols <- c("Adopter"    = "#5bdbef",
                      "Non‑Adopter"= "#EAEAEA")
    
    p2 <- ggplot(challenge_summary, aes(
      x    = Challenge, y = Count,
      fill = adoption_status,
      text = paste0(
        "Challenge: ", Challenge,
        "<br>Status:    ", adoption_status,
        "<br>Count:     ", Count
      )
    )) +
      geom_col(position=position_dodge(0.9)) +
      geom_text(aes(label=Count),
                position=position_dodge(0.9),
                vjust=-0.2, size=3) +
      coord_flip() +
      scale_fill_manual(values = adopter_cols) +
      labs(
        title = "Top Challenges: AI Adopters vs Non‑Adopters",
        x = NULL, y = "Count", fill = "Status"
      ) +
      theme_minimal()
    
    ggplotly(p2, tooltip="text")
  })
  
  
  output$demographic_values_ui <- renderUI({
    switch(input$demographics,
           "age_group" = checkboxGroupInput(
             "demographic_values", "Select Age Groups:",
             choices  = levels(df_cleaned$age_group),
             selected = levels(df_cleaned$age_group)
           ),
           "d1"        = checkboxGroupInput(
             "demographic_values", "Select Genders:",
             choices  = levels(df_cleaned$d1),
             selected = levels(df_cleaned$d1)
           ),
           "q11aa"     = checkboxGroupInput(
             "demographic_values", "Select Revenue Bands:",
             choices  = levels(df_cleaned$q11aa),
             selected = levels(df_cleaned$q11aa)
           ),
           "q3a"       = checkboxGroupInput(
             "demographic_values", "Select Business Sizes:",
             choices  = levels(df_cleaned$q3a),
             selected = levels(df_cleaned$q3a)
           ),
           "services"  = checkboxGroupInput(
             "demographic_values", "Select Services:",
             choices  = c("Customer Service","Marketing",
                          "Content Creation","Boosting Sales",
                          "Business Advice","Other"),
             selected = c("Customer Service","Marketing",
                          "Content Creation","Boosting Sales",
                          "Business Advice","Other")
           ),
           "tools"     = sliderInput(
             "demographic_values", "# Tools:",
             min   = min(rowSums(df_cleaned[grep("^q81_",names(df_cleaned))]=="1")),
             max   = max(rowSums(df_cleaned[grep("^q81_",names(df_cleaned))]=="1")),
             value = c(
               min(rowSums(df_cleaned[grep("^q81_",names(df_cleaned))]=="1")),
               max(rowSums(df_cleaned[grep("^q81_",names(df_cleaned))]=="1"))
             ),
             step  = 1
           )
    )
  })
  
  
  
  
  filtered_personas <- reactive({
    df <- df_cleaned %>%
      mutate(persona = case_when(
        q83_num >= 6                ~ "Confident",
        q83_num >= 3 & q83_num <= 5 ~ "Lukewarm",
        q83_num >= 1 & q83_num <= 2 ~ "Not Confident"
      )) %>%
      filter(persona %in% input$show_personas)
    
    # apply whichever filter was chosen
    if (input$demographics %in% c("age_group","d1","q11aa","q3a")) {
      df <- df %>%
        filter(.data[[input$demographics]] %in% input$demographic_values)
      
    } else if (input$demographics == "services") {
      # keep only rows with at least one of the selected services
      svc_cols <- paste0("q82_", match(
        input$demographic_values,
        c("Customer Service","Marketing","Content Creation",
          "Boosting Sales","Business Advice","Other")
      ))
      df <- df %>%
        filter(if_any(all_of(svc_cols), ~ . == "1"))
      
    } else if (input$demographics == "tools") {
      # count how many tools each used, then filter
      df <- df %>%
        mutate(n_tools = rowSums(across(starts_with("q81_")) == "1")) %>%
        filter(n_tools >= input$demographic_values[1],
               n_tools <= input$demographic_values[2])
    }
    
    df
  })
  
  
  #### Logistic Regression ####

  # Reactive new‐data row from user inputs
  new_obs <- eventReactive(input$btn_predict, {
    # 1) start with the five core predictors
    nd <- data.frame(
      q3a       = factor(input$pred_size,   levels = levels(df_cleaned$q3a)),
      q11aa     = factor(input$pred_rev,    levels = levels(df_cleaned$q11aa)),
      q93       = factor(input$pred_belief, levels = levels(df_cleaned$q93)),
      d1        = factor(input$pred_gender, levels = levels(df_cleaned$d1)),
      age_group = factor(input$pred_age,    levels = levels(df_cleaned$age_group))
    )
    
    # 2) add all 29 industry dummies, but turn on exactly the selected one
    industry_vars <- names(industry_names)  # e.g. "q5b_1","q5b_2",...
    for (v in industry_vars) {
      nd[[v]] <- factor(
        ifelse(v == input$pred_industry, "1", "0"),
        levels = levels(df_logit_input[[v]])
      )
    }
    
    nd
  })
  
  
  # Calculate prediction
  observeEvent(input$btn_predict, {
    req(new_obs())
    prob <- predict(logit_model, newdata = new_obs(), type = "response")
    cls  <- ifelse(prob > 0.5,
                   levels(trainData$q80_3)[2],
                   levels(trainData$q80_3)[1])
    output$pred_prob  <- renderText(sprintf("%.1f%%", prob * 100))
    output$pred_class <- renderText(ifelse(cls=="1","Adopter","Non‑Adopter"))
    output$pred_text  <- renderUI({
      HTML("<p>Your inputs produce the following AI‑adoption prediction:</p>")
    })
  })
  
  
  
  # Expose overall model metrics
  output$model_accuracy     <- renderText(sprintf("%.1f%%", model_accuracy * 100))
  output$benchmark_accuracy <- renderText(sprintf("%.1f%%", benchmark_accuracy * 100))
  output$mcfadden_r2        <- renderText(sprintf("%.3f", model_stats$McFadden_R2))
  

  var_labels <- c(
    "q3a2-4" = "Business Size: 2–4 employees",
    "q3a5-9" = "Business Size: 5–9 employees",
    "q3a10-19" = "Business Size: 10–19 employees",
    "q3a20-49" = "Business Size: 20–49 employees",
    "q3a50-99" = "Business Size: 50–99 employees",
    "q3a100+" = "Business Size: 100+ employees",
    "q3aUnavailable" = "Business Size: Unavailable",
    "q11aaNot Applicable" = "Revenue: Not Applicable",
    "q11aa$500 or less" = "Revenue: $500 or less",
    "q11aa$501 to $2,500" = "Revenue: $501 to $2,500",
    "q11aa$2,501 to $5,000" = "Revenue: $2,501 to $5,000",
    "q11aa$5,001 to $10,000" = "Revenue: $5,001 to $10,000",
    "q11aa$10,001 to $15,000" = "Revenue: $10,001 to $15,000",
    "q11aa$15,001 to $25,000" = "Revenue: $15,001 to $25,000",
    "q11aa$25,001 to $50,000" = "Revenue: $25,001 to $50,000",
    "q11aaOver $50,000" = "Revenue: Over $50,000",
    "q93Completely Disagree" = "Belief: Completely Disagree",
    "q93Somewhat Disagree" = "Belief: Somewhat Disagree",
    "q93Neutral" = "Belief: Neutral",
    "q93Somewhat Agree" = "Belief: Somewhat Agree",
    "q93Completely Agree" = "Belief: Completely Agree",
    "d1Female" = "Gender: Female",
    "d1Non-Binary" = "Gender: Non-Binary",
    "d1Unavailable" = "Gender: Unavailable",
    "age_group20-40" = "Age Group: 20–40",
    "age_group40-60" = "Age Group: 40–60",
    "age_group60+" = "Age Group: 60+",
    "age_groupUnavailable" = "Age Group: Unavailable",
    "q5b_11" = "Industry: Accommodations",
    "q5b_21" = "Industry: Admin & Support",
    "q5b_31" = "Industry: Agriculture",
    "q5b_41" = "Industry: Art & Design",
    "q5b_51" = "Industry: Beauty Services",
    "q5b_61" = "Industry: Charitable/Political",
    "q5b_71" = "Industry: Construction",
    "q5b_81" = "Industry: Consumer Services",
    "q5b_91" = "Industry: Education",
    "q5b_101" = "Industry: Entertainment",
    "q5b_111" = "Industry: Financial Services",
    "q5b_121" = "Industry: Fashion",
    "q5b_131" = "Industry: Healthcare",
    "q5b_141" = "Industry: Home Services",
    "q5b_151" = "Industry: IT/Managed Services",
    "q5b_161" = "Industry: Legal Services",
    "q5b_171" = "Industry: Manufacturing/Wholesale",
    "q5b_181" = "Industry: Marketing/PR",
    "q5b_191" = "Industry: Media/Content",
    "q5b_201" = "Industry: Packaged Food",
    "q5b_211" = "Industry: Professional Services",
    "q5b_221" = "Industry: Personal Services",
    "q5b_231" = "Industry: Real Estate",
    "q5b_241" = "Industry: Restaurant",
    "q5b_251" = "Industry: Retail",
    "q5b_261" = "Industry: Tech & Telecom",
    "q5b_271" = "Industry: Transport/Auto",
    "q5b_281" = "Industry: Web Design/Marketing",
    "q5b_291" = "Industry: Other"
  )
  

  coef_df <- as.data.frame(coef(summary(logit_model)))
  coef_df$Predictor <- rownames(coef_df)
  colnames(coef_df) <- c("Estimate","Std.Error","z.value","Pr(>|z|)","Predictor")
  coef_df <- coef_df[, c("Predictor","Estimate","Std.Error","z.value","Pr(>|z|)")]
  

  coef_df <- as.data.frame(coef(summary(logit_model)))
  coef_df$Predictor <- rownames(coef_df)
  # pull out estimate and p‑value only, round to 2 decimals
  coef_df <- coef_df %>%
    transmute(
      Predictor = recode(Predictor, !!!var_labels),
      Estimate  = round(Estimate, 2),
      p_value   = round(`Pr(>|z|)`, 2)
    )
  
  
  # build the “top 5 positive” & “top 5 negative” data.frames

  top_pos <- coef_df %>%
    filter(Estimate > 0) %>%
    arrange(desc(Estimate)) %>%
    head(5)
  top_neg <- coef_df %>%
    filter(Estimate < 0) %>%
    arrange(Estimate) %>%
    head(5)
  short_coef <- bind_rows(top_pos, top_neg)
  
  # 3) render the DT, switching based on the checkbox

  

  output$coef_table <- DT::renderDataTable({
    tbl <- if (isTRUE(input$show_full_coef)) coef_df else short_coef
    
    DT::datatable(
      tbl,
      rownames = FALSE,
      options = list(pageLength = nrow(tbl), dom = 't')
    ) %>%
      DT::formatStyle(
        'p_value',
        fontWeight = DT::styleInterval(0.05, c('bold', 'normal'))
      )
  })
  
  
  
  #### Class Tree ####
  var_labels <- c(
    q80_3        = "Tried AI for Business (Yes/No)",
    q9           = "Income Source",
    q17a         = "Long‑Term Aspiration",
    business_age_cat = "Business Age",
    d6           = "Education",
    q93          = "Agree AI will help compete",
    d1           = "Gender",
    d3           = "Ethnicity",
    q84          = "Experienced AI Impact",
    q11aa        = "Average Monthly Revenue",
    q5a_2        = "Conduct Business on Social Media",
    q85_5        = "Used AI for Summarizing Information",
    q85_6        = "Used AI for Other Tasks",
    q81_2        = "Used ChatGPT",
    q81_6        = "Used Gemini",
    q5b_18       = "Industry: Marketing, PR"
  )
  
  # 2) grab  raw tree data
  df_model_raw <- df_class_tree %>% 
    filter(!is.na(q83))
  
  # 3) rename columns in that data.frame
  df_model <- df_model_raw
  for (orig in names(var_labels)) {
    if (orig %in% names(df_model)) {
      df_model <- df_model %>% 
        rename(!! var_labels[[orig]] := !! sym(orig))
    }
  }
  
  
  reactive_tree <- reactive({
    rpart::rpart(
      q83 ~ ., 
      data   = df_model,                     
      method = "class",
      control = rpart.control(
        cp       = input$cp,
        minsplit = input$minsplit,
        maxdepth = input$maxdepth
      )
    )
  })
  
  
  output$class_tree_plot <- renderPlot({
    tree <- reactive_tree()
    rpart.plot::rpart.plot(
      tree,
      type          = 2,   # split labels below
      extra         = 106, # show class & probs
      fallen.leaves = TRUE,
      tweak         = 1.2
    )
  })
  

  
  
  
  
}

shinyApp(ui, server)






#### Deploy APP ####






