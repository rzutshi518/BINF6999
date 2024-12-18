#******************************************************************************************

### Installing and loading packages

install.packages("readxl")
library(readxl)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("scales")
library(scales)
install.packages("patchwork")
library(patchwork)
install.packages("surveillance")
library(surveillance)
install.packages("plotly")
library(plotly)
install.packages("random")
library(random)
install.packages("devtools")
library(devtools)
install.packages("jsonlite")
library(jsonlite)
install.packages("sf")
library(sf)
install.packages("sp")
library(sp)
install.packages("spdep")
library(spdep)

#******************************************************************************************

### Setting up static values and defining functions

# Assigning Public Health Unit names to an list used later
public_health_units <- c(
  'Ontario', 
  'Northwestern Health Unit', 
  'Thunder Bay District Health Unit', 
  'Porcupine Health Unit', 
  'Algoma Public Health Unit', 
  'Sudbury and District Health Unit', 
  'Timiskaming Health Unit', 
  'North Bay Parry Sound District Health Unit', 
  'Renfrew County and District Health Unit', 
  'Hastings and Prince Edward Counties Health Unit', 
  'Kingston, Frontenac and Lennox and Addington Health Unit', 
  'Leeds, Grenville and Lanark District Health Unit', 
  'Ottawa Public Health', 
  'Eastern Ontario Health Unit', 
  'Haliburton, Kawartha, Pine Ridge District Health Unit', 
  'Peterborough Public Health', 
  'Simcoe Muskoka District Health Unit', 
  'Durham Region Health Department', 
  'York Region Public Health', 
  'Peel Public Health', 
  'Toronto Public Health', 
  'Halton Region Health Department', 
  'Niagara Region Public Health Department', 
  'Wellington-Dufferin-Guelph Health Unit', 
  'Hamilton Public Health Services', 
  'Region of Waterloo, Public Health', 
  'Brant County Health Unit', 
  'Haldimand-Norfolk Health Unit', 
  'Grey Bruce Health Unit', 
  'Huron Perth Health Unit', 
  'Southwestern Public Health', 
  'Middlesex-London Health Unit', 
  'Lambton Public Health', 
  'Chatham-Kent Health Unit', 
  'Windsor-Essex County Health Unit'
  )

# Creating custom function to get the next available date
get_next_available_date <- function() {
  
  # Find the maximum date in the date column
  last_date <- max(as.Date(cases[, "Week start date"]), na.rm = TRUE)
  
  # Return a date one week after the last date
  return(last_date + 7)
  
}

#******************************************************************************************

### Creating the map and adjacency matrix

# Read the shapefile
phu_shapefile <- st_read("MOH_PHU_BOUNDARY/MOH_PHU_BOUNDARY.shp")

# Order shapefile object
phu_shapefile <- phu_shapefile[order(phu_shapefile$NAME_ENG), ]

# Print and plot the data to inspect
print(phu_shapefile)
plot(phu_shapefile)

# Convert to SpatialPolygons object if needed
phu_spatial <- as(phu_shapefile, "Spatial")

# Check row names of map object
row.names(phu_spatial)
# Assign names of PHU regions from map object
row.names(phu_spatial) <- phu_spatial$NAME_ENG

# Extract PHU names
phu_names <- phu_shapefile$NAME_ENG

# Create adjacency matrix
phu_adjmat <- poly2adjmat(phu_spatial)

# Set row and column names to PHU names
rownames(phu_adjmat) <- phu_names
colnames(phu_adjmat) <- phu_names

# Create the neighborhood object
phu_nbOrder <- nbOrder(phu_adjmat, maxlag = Inf)

# Check the result
print(phu_adjmat)
print(phu_nbOrder)

#******************************************************************************************

### Importing and cleaning 'Cases' data

# Read the 'raw_cases.xlsx' file into a data frame with specified column types
raw_data <- read_excel("Raw Data/raw_cases.xlsx", 
                        col_types = c("text", "text", "text", 
                                      "text", "date", "date", "numeric", 
                                      "numeric", "numeric"))

# Replace all NA values in the raw_data data frame with 0
raw_data[is.na(raw_data)] <- 0
# Verify all of NA values in the raw_data data frame are removed
sum(raw_data[is.na(raw_data)])

# Extract relevant columns and reshape the raw_data to wide format
raw_cases <- raw_data %>% 
  select(`Public health unit`, `Week start date`, `Number of cases`) %>%
  pivot_wider(names_from = `Public health unit`, values_from = `Number of cases`)

# Display the structure of the raw_cases data frame
str(raw_cases)
# Generate summary statistics of the raw_cases data
summary(raw_cases)

# Convert the 'Week start date' column to Date format
raw_cases$`Week start date` <- as.Date(raw_cases$`Week start date`, format = "%Y-%m-%d")
# Display the structure of the 'Week start date' column to verify the conversion
str(raw_cases$`Week start date`)

# Convert columns from the second to the last to numeric format
raw_cases[, 2:ncol(raw_cases)] <- apply(raw_cases[, 2:ncol(raw_cases)], 2, as.numeric)
# Display the structure of columns to verify the conversion to numeric format
str(raw_cases[, 2:36])

# Reorder columns to have 'Week start date' and the first column, followed by sorted public health unit columns
raw_cases <- cbind(raw_cases[, 1:2], raw_cases[, sort(colnames(raw_cases)[3:ncol(raw_cases)])])

# Export 'raw cases' as a .csv file for import into Shiny app
write.csv(x = raw_cases, file = "raw_cases.csv", row.names = FALSE)

# Remove rows to subset the data from 2020-2022
cases <- raw_cases[c(1:157), ]

# Convert the new cases data frame to a matrix and verify the conversion
class(cases)
cases <- as.matrix(cases)
class(cases)

# Export cases as a .csv file for import into Shiny app
write.csv(x = cases, file = "cases.csv", row.names = FALSE)

### Plot to view Ontario cases over time

# Create ggplot for Ontario cases
cases_ONT_plot <- ggplot(data = cases, aes(x = `Week start date`, y = `Ontario`)) +
  geom_line() +
  geom_point() +
  theme_gray() +
  labs(y = "Value", color = "PHU")  # Add labels for y-axis and legend
# Convert ggplot to interactive plotly
ggplotly(cases_ONT_plot)

### Plot to view PHU cases over time

# Reshape data to long format 
cases_long <- cases %>% 
  filter
  pivot_longer(cols = -`Week start date`, names_to = "PHU", values_to = "Value") 
# Print the reshaped data frame 
print(cases_long)
# Create the ggplot with all PHU columns as y
cases_PHU_plot <- ggplot(data = cases_long, 
                         aes(x = `Week start date`, y = Value, color = PHU)) + 
  geom_line() +
  geom_point() +
  theme_grey() +
  labs(y = "Value", color = "PHU")  # Add labels for y-axis and legend
# Convert ggplot to interactive plotly
ggplotly(cases_PHU_plot)

#******************************************************************************************

### Generating 'population' data from 'cases' dataset

# Extracting population data from raw_data by selecting relevant columns and reshaping
raw_population <- raw_data %>%
  select(`Public health unit`, `Week start date`, `Population`) %>%
  pivot_wider(names_from = `Public health unit`, values_from = `Population`)

# Display the structure of the raw_population data frame
str(raw_population)
# Generate summary statistics of the raw_population data
summary(raw_population)

# Convert the 'Week start date' column to Date format
raw_population$`Week start date` <- as.Date(raw_population$`Week start date`, format = "%Y-%m-%d")
# Verifying the structure of the 'Week start date' column
str(raw_population$`Week start date`)

# Convert columns from the second to the last to numeric format
raw_population[, 2:ncol(raw_population)] <- apply(raw_population[, 2:ncol(raw_population)], 2, as.numeric)
# Display the structure of columns to verify the conversion
str(raw_population[, 2:36])

# Reorder columns to have 'Week start date' and the first column, followed by sorted public health unit columns
raw_population <- cbind(raw_population[, 1:2], 
                        raw_population[, sort(colnames(raw_population)[3:ncol(raw_population)])])

# Remove rows to subset the data from 2020-2022
population <- raw_population[c(1:157), ]

# Convert the population data frame to a matrix and verify the conversion
class(population)
population <- as.matrix(population)
class(population)

# Export population data as a .csv file for import into Shiny app
write.csv(x = population, file = "population.csv", row.names = FALSE)

#******************************************************************************************

### Importing and cleaning 'hospitalizations' data

# Read the 'hospitalizations.xlsx' file into a data frame
hospitalizations <- read_xlsx("Raw Data/hospitalizations.xlsx")

# Get the dimensions (number of rows and columns) of the data frame
dim(hospitalizations)
# Display the structure of the data frame
str(hospitalizations)
# Generate summary statistics of the hospitalizations data
summary(hospitalizations)
# Display the unique values in the 'Public health unit' column
unique(hospitalizations$`Public health unit`)

# Check for any NA values in the data frame
hospitalizations[is.na(hospitalizations)]
# Replace all NA values in the data frame with 0
hospitalizations[is.na(hospitalizations)] <- 0

# Select relevant columns ('Public health unit', 'Week start date', 'Number') and reshape the data
hospitalizations <- hospitalizations %>%
  select(`Public health unit`, `Week start date`, `Number`) %>%
  pivot_wider(names_from = `Public health unit`, values_from = `Number`)

# Remove rows to subset the data from 2020-2022
hospitalizations <- hospitalizations[c(1:157),]

# Reorder the columns to have the 'Week start date' column first, followed by sorted public health unit columns
hospitalizations <- cbind(hospitalizations[, 1:2], 
                          hospitalizations[, sort(colnames(hospitalizations)[3:ncol(hospitalizations)])])

# View the updated hospitalizations data frame
View(hospitalizations)

# Write the hospitalizations data to a CSV file to be used in the Shiny app
write.csv(x = hospitalizations, file = "hospitalizations.csv")

#******************************************************************************************

### Importing and cleaning 'Test Percent Positivity' data

# Read the 'testing.xlsx' file into a data frame
raw_tests <- read_xlsx("Raw Data/testing.xlsx")

# Display the structure of the raw_tests data frame
str(raw_tests)
# Generate summary statistics of the raw_tests data
summary(raw_tests)

# Check for any NA values in the data frame
raw_tests[is.na(raw_tests)]
# Replace all NA values in the data frame with 0
raw_tests[is.na(raw_tests)] <- 0

# Round the 'Percent positivity (%)' column to 2 decimal places
raw_tests$`Percent positivity (%)` <- round(raw_tests$`Percent positivity (%)`, digits = 2)

# Extract relevant columns and reshape the data to wide format
raw_tests <- raw_tests %>%
  select(`Public health unit`, `Week start date`, `Percent positivity (%)`) %>%
  pivot_wider(names_from = `Public health unit`, values_from = `Percent positivity (%)`)

# Reorder columns to have 'Week start date' and the first column, followed by sorted public health unit columns
raw_tests <- cbind(raw_tests[, 1:2], 
                            raw_tests[, sort(colnames(raw_tests)[3:ncol(raw_tests)])])

# Remove rows to subset the data from 2020-2022
percent_positivity <- raw_tests[c(1:157), ]

# View the updated percent_positivity data frame
View(percent_positivity)

# Convert the data frame to a matrix and verify the conversion
class(percent_positivity)
percent_positivity <- as.matrix(percent_positivity)
class(percent_positivity)

# Export 'percent_positivity' data as a .csv file for import into Shiny app
write.csv(x = percent_positivity, file = "percent_positivity.csv", row.names = FALSE)

### Plot to view percent positivity over time

# Reshape data to long format 
percent_positivity_long <- percent_positivity %>% 
  pivot_longer(cols = -`Week start date`, names_to = "PHU", values_to = "Value") 
# Print the reshaped data frame 
print(percent_positivity_long)
# Create the ggplot with all remaining columns as y
percent_positivity_plot <- ggplot(data = percent_positivity_long, 
                                  aes(x = `Week start date`, y = Value, color = PHU)) + 
  geom_line() +
  geom_point() +
  theme_grey() +
  labs(y = "Value", color = "PHU")  # Add labels for y-axis and legend
# Convert ggplot to interactive plotly
ggplotly(percent_positivity_plot)

#******************************************************************************************

### Importing and cleaning 'Vaccine doses administered' data

# Read the 'ONT_vaccine_data' from the specified Excel file, skipping irrelevant columns and setting column types
ONT_vaccine_data <- read_excel("Raw Data/Number of COVID-19 vaccine doses administered by vaccine product or dose number in Ontario.xlsx", range = "A3:C1243", col_types = c("date", "skip", "numeric"))
# View the imported data
View(ONT_vaccine_data)
# Rename columns, group data by 'Week start date', and summarize total doses administered in Ontario
ONT_vaccine_data <- ONT_vaccine_data %>%
  rename("Week start date" = `'vaxdosevaxdata'[Week start date]`, 
         "Doses administered" = `Number of doses`) %>%
  group_by(`Week start date`) %>% 
  summarize(`Ontario` = sum(`Doses administered`, na.rm = TRUE)) 
# Subset the data to report to the end of 2022
ONT_vaccine_data <- ONT_vaccine_data[1:107, ]
# View the transformed data
View(ONT_vaccine_data)


# Read the 'PHU_vaccine_data' from the specified Excel sheet, skipping irrelevant columns and setting column types
PHU_vaccine_data <- read_excel("Raw Data/orvt-covid-19-vaccine-data.xlsx", sheet = "Doses administered", 
                               col_types = c("text", "skip", "skip", "date", "numeric"))
# View the imported data
View(PHU_vaccine_data)
# Display unique values in the 'Public health unit' column
unique(PHU_vaccine_data$`Public health unit`)
# Filter out rows where 'Public health unit' is 'Unknown', group data, summarize doses, and pivot the data frame wider
PHU_vaccine_data <- PHU_vaccine_data %>%
  filter(!(`Public health unit` == "Unknown")) %>%
  group_by(`Public health unit`, `Week start date`) %>% 
  summarize(`Doses administered` = sum(`Number of doses`, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Public health unit`, values_from = `Doses administered`)
# Subset the data to report to the end of 2022
PHU_vaccine_data <- PHU_vaccine_data[1:107, ]
# View the transformed data
View(PHU_vaccine_data)


# Generate a data frame with a sequence of weekly dates
generated_time_data <- data.frame(
                      # Create a sequence of dates in weekly increments for 50 weeks 
  `Week start date` = date_sequence <- seq.Date(from = as.Date("2019-12-29"), by = "week", length.out = 50)
)
# Create a 50x34 matrix filled with zeros 
matrix_of_zeros <- matrix(0, nrow = 50, ncol = 35) 
# Convert the matrix to a data frame 
df_of_zeros <- as.data.frame(matrix_of_zeros)
# Combine the generated time data with the matrix of zeros
blank_data <- cbind(generated_time_data, df_of_zeros)
# Assign column names to the combined data frame
colnames(blank_data) <- c("Week start date", "Ontario", sort(public_health_units[-1]))


# Combine Ontario vaccine data with public health unit vaccine data
vaccine_data <- cbind(ONT_vaccine_data, PHU_vaccine_data[, 2:ncol(PHU_vaccine_data)])

# Combine the blank data with the vaccine data to create the final dataset
vaccines_administered <- rbind(blank_data, vaccine_data)

# View the final combined dataset to verify the structure and contents
View(vaccines_administered)

# Export 'vaccines_administered' data as a .csv file for import into Shiny app
write.csv(x = vaccines_administered, file = "vaccines_administered.csv", row.names = FALSE)

#******************************************************************************************

### Importing and cleaning 'Wastewater' data

# Read the 'wastewater' data from the specified Excel file and sheet
wastewater <- read_xlsx("Raw Data/wastewater-surveillance-signal-data.xlsx", 
                        sheet = "Wastewater signal data")

# View the imported data to check its structure
View(wastewater)

# Rename the columns for easier referencing
colnames(wastewater) <- c("Sample Date", "Ontario", "Central East", "Central West", "East", 
                          "GTA", "North East", "North West", "South West")

# Check the structure of the data frame to ensure proper data types
str(wastewater)
# Summarize the data to get an overview of each column
summary(wastewater)

# Check for the sum of any NA values in the data frame
sum(wastewater[is.na(wastewater)])
# Replace NA values with 0
wastewater[is.na(wastewater)] <- 0

# Remove rows to subset the data from 2020-2022
wastewater <- wastewater[-c(258:nrow(wastewater)), ]
wastewater <- wastewater[-(sample(1:nrow(wastewater), 100)), ]

# Mutate the 'Sample Date' column and rename it as 'Week start date'
wastewater <- wastewater %>% 
  mutate(`Sample Date` = cases[, 1]) %>% 
  rename(`Week start date` = `Sample Date`) 

# Set column format as date
wastewater$`Week start date` <- as.Date(wastewater$`Week start date`, format = "%Y-%m-%d")

# View the cleaned and transformed wastewater data
View(wastewater)

# Define the PHUs and their respective groups 
phu_groups <- list(`Central East` = c("Simcoe Muskoka District Health Unit",
                                      "Haliburton, Kawartha, Pine Ridge District Health Unit", 
                                      "Peterborough Public Health", 
                                      "Peel Public Health", 
                                      "York Region Public Health",
                                      "Durham Region Health Department"), 
                   `Central West` = c("Wellington-Dufferin-Guelph Health Unit",
                                      "Region of Waterloo, Public Health",
                                      "Halton Region Health Department",
                                      "Hamilton Public Health Services",
                                      "Brant County Health Unit", 
                                      "Niagara Region Public Health Department",
                                      "Haldimand-Norfolk Health Unit"), 
                   `East` = c("Renfrew County and District Health Unit",
                              "Ottawa Public Health", 
                              "Eastern Ontario Health Unit", 
                              "Leeds, Grenville and Lanark District Health Unit",
                              "Kingston, Frontenac and Lennox and Addington Health Unit",
                              "Hastings and Prince Edward Counties Health Unit"),
                   `GTA` = c("Toronto Public Health"), 
                   `North East` = c("Porcupine Health Unit",
                                    "Algoma Public Health Unit",
                                    "Sudbury and District Health Unit", 
                                    "Timiskaming Health Unit",
                                    "North Bay Parry Sound District Health Unit"), 
                   `North West` = c("Northwestern Health Unit", 
                                    "Thunder Bay District Health Unit"), 
                   `South West` = c("Grey Bruce Health Unit", 
                                    "Huron Perth Health Unit", 
                                    "Lambton Public Health", 
                                    "Middlesex-London Health Unit", 
                                    "Southwestern Public Health", 
                                    "Chatham-Kent Health Unit", 
                                    "Windsor-Essex County Health Unit") ) 

# Convert the list to a data frame 
phu_data <- do.call(rbind, lapply(names(phu_groups), function(group) { 
  data.frame(Group = group, PHU = phu_groups[[group]]) 
  })) 

# Print the grouped PHUs data frame 
print(phu_data)

# Initialize the new data frame 
new_wastewater_data <- data.frame("Week start date" = wastewater$`Week start date`, 
                                  "Ontario" = wastewater$`Ontario`)

# Add each PHU column based on their group inclusion 
for (group in names(phu_groups)) { 
  for (phu in phu_groups[[group]]) { 
    new_wastewater_data[[phu]] <- wastewater[[group]][1:length(new_wastewater_data$Week.start.date)] 
  } 
}

# Reorder columns to have 'Week start date' and the first column, followed by sorted public health unit columns
new_wastewater_data <- cbind(new_wastewater_data[, 1:2], 
                             new_wastewater_data[, sort(colnames(new_wastewater_data)
                                                        [3:ncol(new_wastewater_data)])])

# View the updated hospitalizations data frame
View(new_wastewater_data)

# Convert the wastewater data frame to a matrix and verify the conversion
class(new_wastewater_data)
new_wastewater_data <- as.matrix(new_wastewater_data)
class(new_wastewater_data)

# Export 'wastewater' data as a .csv file for import into Shiny app
write.csv(x = new_wastewater_data, file = "wastewater_data.csv", row.names = FALSE)

### Plot to view wastewater signal over time

# Reshape data to long format 
wastewater_long <- wastewater %>% 
  pivot_longer(cols = -`Sample Date`, names_to = "PHU", values_to = "Value") 
# Print the reshaped data frame 
print(wastewater_long)
# Create the ggplot with all remaining columns as y
wastewater_plot <- ggplot(data = wastewater_long, aes(x = `Sample Date`, y = Value, color = PHU)) + 
  geom_line() +
  geom_point() +
  theme_grey() +
  labs(y = "Value", color = "PHU")  # Add labels for y-axis and legend
# Convert ggplot to interactive plotly
ggplotly(wastewater_plot)

#******************************************************************************************

### Importing and cleaning 'Seroprevalence' data

# Read the CSV file containing seroprevalence data
seroprevalence <- read_csv("Raw Data/COVIDdataMasterTableau - Seroprevalence.csv")

# View the seroprevalence data frame
View(seroprevalence)

# Look at the structure of data frame
str(seroprevalence)
# Display a summary of the data 
summary(seroprevalence)

# Select only the first and ninth columns
seroprevalence <- seroprevalence[, c(1,9)]

# Remove the first two rows
seroprevalence <- seroprevalence[-c(1:2), ]

# Remove rows from to subset the data from 2020-2022
seroprevalence <- seroprevalence[-c(158:nrow(seroprevalence)), ]

# Check for the sum of any NA values in the data frame
sum(seroprevalence[is.na(seroprevalence)])
# Replace all NA values in the data frame with 0
seroprevalence[is.na(seroprevalence)] <- 0

# Process the seroprevalence data frame to add a new 'week_start' column, remove the original first column, and reorder columns
seroprevalence <- seroprevalence %>%
  mutate(week_start = floor_date(week_end, "week")) %>%
  select(-1) %>%
  select(week_start, everything())

# Rename the columns of the seroprevalence data frame to 'Week start date' and 'Ontario'
colnames(seroprevalence) <- c("Week start date", "Ontario")

# Duplicate the 'Ontario' column 34 times and set column names based on the PHU list
for (phu in public_health_units) { 
  seroprevalence[[phu]] <- seroprevalence$Ontario 
}

# Reorder columns to have 'Week start date' and the first column, followed by sorted public health unit columns
seroprevalence <- cbind(seroprevalence[, 1:2], 
                        seroprevalence[, sort(colnames(seroprevalence)[3:ncol(seroprevalence)])])

# View the updated seroprevalence data frame
View(seroprevalence)

# Convert the seroprevalence data frame to a matrix and verify the conversion
class(seroprevalence)
seroprevalence <- as.matrix(seroprevalence)
class(seroprevalence)

# Write the seroprevalence data to a CSV file to be used in the Shiny app
write.csv(x = seroprevalence, file = "seroprevalence.csv")

#******************************************************************************************

### Importing and cleaning 'Google Mobility' data

# Read CSV files for mobility data from 2020, 2021, and 2022
mobility_data_2020 <- read_csv("Raw Data/2020_CA_Region_Mobility_Report.csv")
mobility_data_2021 <- read_csv("Raw Data/2021_CA_Region_Mobility_Report.csv")
mobility_data_2022 <- read_csv("Raw Data/2022_CA_Region_Mobility_Report.csv")

# Combine the data from the three years into one data frame
mobility_data <- rbind(mobility_data_2020, mobility_data_2021, mobility_data_2022)

# View the combined data to check its structure
View(mobility_data)

# Get the dimensions of the combined data frame
dim(mobility_data)
# Check the structure of the data frame
str(mobility_data)
# Summarize the combined data
summary(mobility_data)

# Filter the data for Ontario and select specific columns
mobility_data <- mobility_data %>%
  filter(sub_region_1 == "Ontario") %>%
  select(c(2:4, 9:15))

# Display unique values in the 'sub_region_1' column
unique(mobility_data$sub_region_1)
# Display unique values in the 'sub_region_2' column
unique(mobility_data$sub_region_2)

# Replace NA values in the 'sub_region_2' column with 'Ontario'
mobility_data$sub_region_2[is.na(mobility_data$sub_region_2)] <- "Ontario"

# Verify the changes by checking unique values again
unique(mobility_data$sub_region_1)
unique(mobility_data$sub_region_2)

# Select specific columns and pivot the data to a wider format
mobility_data <- mobility_data %>%
  select(c(3, 4, 8)) %>%
  pivot_wider(names_from = sub_region_2, 
              values_from = transit_stations_percent_change_from_baseline)

# View the pivoted data to check the new structure
View(mobility_data)

# Use this code to replace all unwanted PHU names with the correct names
old_name <- "Brant County"
new_name <- "Brant County Health Unit"
mobility_data$sub_region_2 <- replace(mobility_data$sub_region_2, 
                                      mobility_data$sub_region_2 == old_name,
                                      new_name)

# Verify the replacement by checking unique values
unique(mobility_data$sub_region_2)

# Aggregate data by week
weekly_data <- mobility_data %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarize(across(c(4:9), mean, na.rm = T))

# Check for any NA values in 'sub_region_2'
mobility_data[is.na(mobility_data$sub_region_2)]

# Replace NA values in the 'seroprevalence' data with 0
seroprevalence[is.na(seroprevalence)] <- 0

# View the final mobility data
View(mobility_data)

# Convert the mobility_data data frame to a matrix and verify the conversion
class(mobility_data)
mobility_data <- as.matrix(mobility_data)
class(mobility_data)

# Write the mobility_data data to a CSV file to be used in the Shiny app
write.csv(x = mobility_data, file = "mobility_data.csv")

#******************************************************************************************

### Importing and cleaning 'Stay-at-home restrictions' data

# Read the 'stay-at-home-covid.csv' file into a data frame
stay_at_home <- read_csv("Raw Data/stay-at-home-covid.csv")

# View the 'stay_at_home' data frame
View(stay_at_home)

# Filter the data for Canada, select specific columns, create a 'Week' column, and summarize 
stay_at_home <- stay_at_home %>% 
  # Keep only rows where the Entity is "Canada" 
  filter(Entity == "Canada") %>% 
  # Select the 3rd and 4th columns 
  select(c(3, 4)) %>% 
  # Create a 'Week' column by rounding down to the start of the week
  mutate(Week = floor_date(Day, "week")) %>% 
  # Rearrange columns to put 'Week' first 
  select(c(3, 2)) %>% 
  # Group data by 'Week' 
  group_by(Week) %>% 
  # Calculate the mean of 'stay_home_requirements' data for the week, rounded to nearest integer
  summarize(stay_home_requirements = round(mean(stay_home_requirements, na.rm = TRUE), 0), .groups = 'drop') 

# Add columns to match the number of public health units by replicating the 'stay_home_requirements' column
stay_at_home = cbind(stay_at_home, replicate(34, stay_at_home$stay_home_requirements))

# Rename the columns to 'Week start date' and public health unit names
colnames(stay_at_home) <- c("Week start date", public_health_units)

# Reorder columns to have 'Week start date' and the first column, followed by sorted public health unit columns
stay_at_home <- cbind(stay_at_home[, 1:2], 
                      stay_at_home[, sort(colnames(stay_at_home)[3:ncol(stay_at_home)])])

# Convert the stay_at_home data frame to a matrix and verify the conversion
class(stay_at_home)
stay_at_home <- as.matrix(stay_at_home)
class(stay_at_home)

# View the updated 'stay_at_home' matrix
View(stay_at_home)

# Write the stay_at_home data to a CSV file to be used in the Shiny app
write.csv(x = stay_at_home, file = "stay_at_home_restrictions.csv")

#******************************************************************************************

### Importing and cleaning 'Facial covering policies' data

# Read the 'face covering policies' data file into a data frame
face_covering <- read_csv("Raw Data/face-covering-policies-covid.csv")

# View the 'face_covering' data frame
View(face_covering)

# Filter data for Canada, select specific columns, create a 'Week' column, summarize the daily data to weekly means
face_covering <- face_covering %>% 
  # Keep only rows where the Entity is "Canada" 
  filter(Entity == "Canada") %>% 
  # Select the 3rd and 4th columns 
  select(c(3, 4)) %>% 
  # Create a 'Week' column by rounding down to the start of the week 
  mutate(Week = floor_date(Day, "week")) %>% 
  # Rearrange columns to put 'Week' first 
  select(c(3, 2)) %>% 
  # Group data by 'Week'
  group_by(Week) %>%  
  # Calculate the mean of 'facial_coverings' data for the week, rounded to nearest integer
  summarize(facial_coverings = round(mean(facial_coverings, na.rm = TRUE), 0), .groups = 'drop') 

# Add columns to match the number of public health units by replicating the 'facial_coverings' column
face_covering = cbind(face_covering, replicate(34, face_covering$facial_coverings))

# Rename the columns to 'Week start date' and public health unit names
colnames(face_covering) <- c("Week start date", public_health_units)

# Reorder columns to have 'Week start date' and the first column, followed by sorted public health unit columns
face_covering <- cbind(face_covering[, 1:2], 
                       face_covering[, sort(colnames(face_covering)[3:ncol(face_covering)])])

# Convert the face_coverings data frame to a matrix and verify the conversion
class(face_covering)
face_covering <- as.matrix(face_covering)
class(face_covering)

# View the updated 'face_coverings' matrix
View(face_covering)

# Write the face_coverings data to a CSV file to be used in the Shiny app
write.csv(x = face_covering, file = "face_covering_policies.csv")

#******************************************************************************************

### Importing and cleaning 'School & Workplaces Closure' data

# Fetch the data
closures <- read.csv("https://ourworldindata.org/grapher/workplace-closures-covid.csv?v=1&csvType=full&useColumnShortNames=false")

# Set column as data format
closures$Day <- as.Date(closures$Day)

# Set column names for better understanding
colnames(closures) <- c("Country", "Code", "Day", "School_Workplaces_closing")

# Filter data for Canada, select specific columns, create a 'Week' column, summarize the daily data to weekly means
closures <- closures %>% 
  # Keep only rows where the Entity is "Canada" 
  filter(Country == "Canada") %>% 
  # Select the 3rd and 4th columns 
  select(c(3, 4)) %>% 
  # Create a 'Week' column by rounding down to the start of the week 
  mutate(Week = floor_date(Day, "week")) %>% 
  # Rearrange columns to put 'Week' first 
  select(c(3, 2)) %>% 
  # Group data by 'Week'
  group_by(Week) %>%  
  # Calculate the mean of 'School_Workplaces_closing' data for the week, rounded to nearest integer
  summarize(School_Workplaces_closing = round(mean(School_Workplaces_closing, na.rm = TRUE), 0), 
            .groups = 'drop') 
  
# View the 'closures' data frame
View(closures)

# Add columns to match the number of public health units by replicating the 'closures' column
closures = cbind(closures, replicate(34, closures$School_Workplaces_closing))

# Rename the columns to 'Week start date' and public health unit names
colnames(closures) <- c("Week start date", public_health_units)

# Reorder columns to have 'Week start date' and the first column, followed by sorted public health unit columns
closures <- cbind(closures[, 1:2], 
                  closures[, sort(colnames(closures)[3:ncol(closures)])])

# Convert the closures data frame to a matrix and verify the conversion
class(closures)
closures <- as.matrix(closures)
class(closures)

# View the updated 'closures' matrix
View(closures)

# Write the closures data to a CSV file to be used in the Shiny app
write.csv(x = closures, file = "school_workplace_closures.csv")

#******************************************************************************************

### Creating STS object for Ontario

# Check the class of the 'cases' object
class(cases)
# Convert the 'cases' data frame to a matrix
cases <- as.matrix(cases)
# Verify the class of the 'cases' object after conversion
class(cases)

# Subset the row for 2020-2022 and extract the Ontario cases (second column) from the 'cases' matrix
temp_cases_ONT <- cases[1:157, 2]
# Convert the extracted Ontario cases to numeric
temp_cases_ONT <- as.numeric(temp_cases_ONT)

# Check the class of the 'population' object
class(population)
# Convert the 'population' data frame to a matrix
population <- as.matrix(population)
# Verify the class of the 'population' object after conversion
class(population)

# Extract the Ontario population data (second column) from the 'population' matrix
temp_population_ONT <- population[, 2]
# Convert the extracted Ontario population data to numeric
temp_population_ONT <- as.numeric(temp_population_ONT)

# Create an STS (spatio-temporal surveillance) object for Ontario cases only 
# - 'observed' parameter: Ontario cases data 
# - 'start' parameter: Start date of the data (year 2020, week 1) 
# - 'frequency' parameter: Frequency of the data (weekly, so 52 weeks per year) 
# - 'population' parameter: Ontario population data as a matrix
phuSTS_ONT <- sts(observed = temp_cases_ONT, 
                  start = c(2020, 1), 
                  frequency = 52, 
                  population = matrix(temp_population_ONT))

# View STS_ONT object
phuSTS_ONT

# Plot the STS object with observed cases over time
plot(phuSTS_ONT, type = observed ~ time)

#******************************************************************************************

### Creating STS object for the PHU 

# Check the class of the 'cases' object
class(cases)
# Convert the 'cases' data frame to a matrix
cases <- as.matrix(cases)
# Verify the class of the 'cases' object after conversion
class(cases)

# Subset the rows for 2020-2022 and extract all PHU cases (excluding the first two columns) from the 'cases' matrix
temp_cases_PHU <- cases[1:157, c(-1,-2)]
# Convert the extracted PHU cases to numeric
temp_cases_PHU <- apply(temp_cases_PHU, 2, as.numeric)

# Check the class of the 'population' object
class(population)
# Convert the 'population' data frame to a matrix
population <- as.matrix(population)
# Verify the class of the 'population' object after conversion
class(population)

# Extract all PHU population data (excluding the first two columns) from the 'population' matrix
temp_population_PHU <- population[, c(-1,-2)]
# Convert the extracted PHU population data to numeric
temp_population_PHU <- apply(temp_population_PHU, 2, as.numeric)

# Check the class of the 'phu_nbOrder' object
class(phu_nbOrder)

# Create an STS (spatio-temporal surveillance) object for PHU cases 
# - 'observed' parameter: PHU cases data 
# - 'start' parameter: Start date of the data (year 2020, week 1) 
# - 'frequency' parameter: Frequency of the data (weekly, so 52 weeks per year) 
# - 'population' parameter: PHU population data 
# - 'neighbourhood' parameter: Neighbourhood order data 
# - 'map' parameter: Spatial data
phuSTS <- sts(observed = temp_cases_PHU, 
              start = c(2020, 1), 
              frequency = 52, 
              population = temp_population_PHU, 
              neighbourhood = phu_nbOrder, 
              map = phu_spatial)

# View phuSTS object
phuSTS

# Plot the STS object with observed cases over time for each unit shown on the map object
plot(phuSTS, type = observed ~ unit, population = phuSTS@populationFrac, 
     labels = NULL, colorkey = list(space = "right"), 
     sp.layout = layout.scalebar(phuSTS@map, corner = c(0.05, 0.05), scale = 1000, 
                                 labels = c("0", "1000 km"), height = 1))

#******************************************************************************************

### Training model with and without seasonal effect to test for model performance
### (Using data and STS object for Ontario for simplicity)

# Define the model without seasonal effect (SE)
model_without_SE = list(
  
  # Endemic component formula for log(nu_it)
  end = list(f = ~ 1,
             offset = population(phuSTS_ONT)), # multiplicative offset e_it
  
  # Autoregressive component formula for log(lambda_it)
  ar = list(f = ~ 1), 
  
  family = "NegBin1", # distribution model
  
  # Optimizing settings
  optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                   regression = list(method = "nlminb"), # for penalized log-likelihood
                   variance = list(method = "nlminb")), # for marginal log-likelihood
  
  verbose = FALSE, # level of progress reporting
  
  keep.terms = FALSE # whether to keep the model terms
  
)

# Fit the model without seasonal effect using the hhh4 function
hhh4_model_without_SE <- hhh4(stsObj = phuSTS_ONT, control = model_without_SE)

# Generate summary statistics of the model without seasonal effect
summary(hhh4_model_without_SE)


# Define the model with seasonal effect (SE)
model_with_SE = list(
  
  # Endemic component formula for log(nu_it)
  end = list(f = addSeason2formula(~ 1 + t, period = phuSTS_ONT@freq), # Added seasonal effect
             offset = population(phuSTS_ONT)), # multiplicative offset e_it
  
  # Autoregressive component formula for log(lambda_it)
  ar = list(f = ~ 1), 
  
  family = "NegBin1", # distribution model
  
  # Optimizing settings
  optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                   regression = list(method = "nlminb"), # for penalized log-likelihood
                   variance = list(method = "nlminb")), # for marginal log-likelihood
  
  verbose = FALSE, # level of progress reporting

  keep.terms = FALSE # whether to keep the model terms
  
)

# Fit the model with seasonal effect using the hhh4 function
hhh4_model_with_SE <- hhh4(stsObj = phuSTS_ONT, control = model_with_SE)

# Summary statistics of the model with seasonal effect
summary(hhh4_model_with_SE)


# Compare the AIC of the models without and with the seasonal element added
AIC(hhh4_model_without_SE);AIC(hhh4_model_with_SE)

#******************************************************************************************

### Training model with and without neighborhood weight power law to test for model performance
### (Using data and STS object from PHU to observe neighborhood effects)

# Define a model without Power Law (PL)
model_without_PL = list(
  
  # formula for log(nu_it)
  end = list(f = addSeason2formula(~ -1 + fe(1, unitSpecific = TRUE) + t, period = phuSTS@freq), 
             offset = population(phuSTS)), # multiplicative offset e_it
  
  # formula for log(lambda_it)
  ar = list(f = ~ -1 + fe(1, unitSpecific = TRUE)), 
  
  # formula for log(phi_it)
  ne = list(f = ~ -1 + fe(1, unitSpecific = TRUE), 
            weights = neighbourhood(phuSTS) == 1), # (w_ji) matrix
  
  family = "NegBin1", # distribution model
  
  # Optimizing settings
  optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                   regression = list(method = "nlminb"), # for penLogLik
                   variance = list(method = "nlminb")), # for marLogLik
  
  verbose = FALSE, # level of progress reporting
  
  keep.terms = FALSE # whether to keep the model terms
  
)

# Fit the hhh4 model without the power law
hhh4_model_without_PL <- hhh4(stsObj = phuSTS, control = model_without_PL)

# Summarize the fitted model to get an overview of its components
summary(hhh4_model_without_PL)


# Model formula and call for Power Law model
model_with_PL = list(
  
  # formula for log(nu_it)
  end = list(f = addSeason2formula(~ -1 + fe(1, unitSpecific = TRUE) + t, period = phuSTS@freq), 
             offset = population(phuSTS)), # multiplicative offset e_it
  
  # formula for log(lambda_it)
  ar = list(f = ~ -1 + fe(1, unitSpecific = TRUE)), 
  
  # formula for log(phi_it) scaled with population 
  ne = list(f = ~ -1 + fe(log(pop), unitSpecific = TRUE), 
            weights = W_powerlaw(maxlag = 10)),
  
  family = "NegBin1", # distribution model
  
  # Optimizing settings 
  optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                   regression = list(method = "nlminb"), # for penLogLik
                   variance = list(method = "nlminb")), # for marLogLik
  
  verbose = FALSE, # level of progress reporting
  
  data = list(pop = population(phuSTS)), # list for population data
  
  keep.terms = FALSE # whether to keep the model terms
  
)

# Fit the hhh4 model with power law
hhh4_model_with_PL <- hhh4(stsObj = phuSTS, control = model_with_PL)

# Summarize the fitted model
summary(hhh4_model_with_PL)


# Compare the AIC of the two fitted models
AIC(hhh4_model_without_PL);AIC(hhh4_model_with_PL)

#******************************************************************************************

###

# Model formula call for model 1 for Ontario
model1_ONT = list(
  
  # formula for log(nu_it)
  end = list(f = addSeason2formula(~ 1 + t, period = phuSTS_ONT@freq), 
             offset = population(phuSTS_ONT)), # multiplicative offset e_it
  
  # formula for log(lambda_it)
  ar = list(f = ~ 1), 
  
  family = "NegBin1", # distribution model
  
  # Optimziing settings 
  optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                   regression = list(method = "nlminb"), # for penLogLik
                   variance = list(method = "nlminb")), # for marLogLik
  
  verbose = FALSE, # level of progress reporting
  
  keep.terms = FALSE # whether to keep the model terms
  
)

# Generate the model using the Ontario STS object and specified model settings
hhh4_model1_ONT <- hhh4(stsObj = phuSTS_ONT, control = model1_ONT)

# Get summary statistics of ONT model 1 fit
summary(hhh4_model1_ONT)
# Retrieve AIC of model fit
AIC(hhh4_model1_ONT)


# Model formula call for model 1 for PHUs
model1_PHU = list(
  
  # formula for log(nu_it)
  end = list(f = addSeason2formula(~ -1 + fe(1, unitSpecific = TRUE) + t, period = phuSTS@freq), 
             offset = population(phuSTS)), # multiplicative offset e_it
  
  # formula for log(lambda_it)
  ar = list(f = ~ -1 + fe(1, unitSpecific = TRUE)), 
 
  # formula for log(phi_it) scaled with population 
  ne = list(f = ~ -1 + fe(log(pop), unitSpecific = TRUE), 
            weights = W_powerlaw(maxlag = 10)), # (w_ji) matrix
  
  family = "NegBin1", # distribution model
  
  # Optimizing settings
  optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                   regression = list(method = "nlminb"), # for penLogLik
                   variance = list(method = "nlminb")), # for marLogLik
  
  verbose = FALSE, # level of progress reporting
  
  data = list(pop = population(phuSTS)), # named list of covariates
  
  keep.terms = FALSE # whether to keep the model terms
  
)

# Generate the model using the PHU STS object and specified model settings
hhh4_model1_PHU <- hhh4(stsObj = phuSTS, control = model1_PHU)

# Get summary statistics for PHU model 1 fit
summary(hhh4_model1_PHU)
# Retrieve AIC
AIC(hhh4_model1_PHU)

#******************************************************************************************

###

# Model formula call for model 2 for Ontario
model2_ONT = list(
  
  # formula for log(nu_it)
  end = list(f = addSeason2formula(~ 1 + t + percent_positivity, period = phuSTS_ONT@freq), 
             offset = population(phuSTS_ONT)), # multiplicative offset e_it
  
  # formula for log(lambda_it)
  ar = list(f = ~ 1 + wastewater), 
  
  family = "NegBin1", # distribution model
  
  # Optimizing settings
  optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                   regression = list(method = "nlminb"), # for penLogLik
                   variance = list(method = "nlminb")), # for marLogLik
  
  verbose = FALSE, # level of progress reporting
  
  # named list of covariates
  data = list(percent_positivity = as.numeric(percent_positivity[, 2]),
              wastewater = as.numeric(wastewater[, 2])), 
  
  keep.terms = FALSE # whether to keep the model terms
  
)

# Generate the model using the ONT STS object and specified model settings
hhh4_model2_ONT <- hhh4(stsObj = phuSTS_ONT, control = model2_ONT)

# Get summary statistics for ONT model 2 fit
summary(hhh4_model2_ONT)
# Retrieve AIC
AIC(hhh4_model2_ONT)


# Model formula call for model 2 for PHUs
model2_PHU = list(
  
  # formula for log(nu_it)
  end = list(f = addSeason2formula(~ -1 + fe(1, unitSpecific = TRUE) + t + percent_positivity, period = phuSTS@freq), offset = population(phuSTS)), # multiplicative offset e_it
  
  # formula for log(lambda_it)
  ar = list(f = ~ -1 + fe(1, unitSpecific = TRUE) + wastewater), 
  
  # formula for log(phi_it) scaled with population 
  ne = list(f = ~ -1 + fe(log(pop), unitSpecific = TRUE) + stay_at_home, 
            weights = W_powerlaw(maxlag = 10)), # (w_ji) matrix
  
  family = "NegBin1", # distribution model
  
  # Optimizing settings
  optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                   regression = list(method = "nlminb"), # for penLogLik
                   variance = list(method = "nlminb")), # for marLogLik
  
  verbose = FALSE, # level of progress reporting
  
  # named list of covariates
  data = list(pop = population(phuSTS),
              percent_positivity = as.matrix(percent_positivity[, -c(1,2)]),
              wastewater = as.matrix(as.numeric(wastewater[, -c(1,2)])),
              stay_at_home = as.matrix(stay_at_home[, -c(1,2)])), 
  
  keep.terms = FALSE # whether to keep the model terms
  
)

# Generate the model using the PHU STS object and specified model settings
hhh4_model2_PHU <- hhh4(stsObj = phuSTS, control = model2_PHU)

# Get summary statistics for PHU model 2 fit
summary(hhh4_model2_PHU)
# Retrieve AIC
AIC(hhh4_model2_PHU)

#******************************************************************************************

###

# Model formula call for model 3 for Ontario
model3_ONT = list(
  
  # formula for log(nu_it)
  end = list(f = addSeason2formula(~ 1 + t + percent_positivity + seroprevalence, period = phuSTS_ONT@freq), 
             offset = population(phuSTS_ONT)), # multiplicative offset e_it
  
  # formula for log(lambda_it)
  ar = list(f = ~ 1 + wastewater + vaccines_administered), 
  
  family = "NegBin1", # distribution model
  
  # Optimizing settings
  optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                   regression = list(method = "nlminb"), # for penLogLik
                   variance = list(method = "nlminb")), # for marLogLik
  
  verbose = FALSE, # level of progress reporting
  
  # named list of covariates
  data = list(percent_positivity = as.numeric(percent_positivity[, 2]),
              seroprevalence = as.numeric(seroprevalence[, 2]),
              wastewater = as.numeric(wastewater[, 2]),
              vaccines_administered = as.numeric(vaccines_administered[, 2])), 
  
  keep.terms = FALSE # whether to keep the model terms
  
)

# Generate the model using the ONT STS object and specified model settings
hhh4_model3_ONT <- hhh4(stsObj = phuSTS_ONT, control = model3_ONT)

# Get summary statistics for ONT model 3 fit
summary(hhh4_model3_ONT)
# Retrieve AIC
AIC(hhh4_model3_ONT)


# Model formula call for model 3 for PHUs
model3_PHU = list(
  
  # formula for log(nu_it)
  end = list(f = addSeason2formula(~ -1 + fe(1, unitSpecific = TRUE) + t + percent_positivity + seroprevalence, period = phuSTS@freq), offset = population(phuSTS)), # multiplicative offset e_it
  
  # formula for log(lambda_it)
  ar = list(f = ~ -1 + fe(1, unitSpecific = TRUE) + wastewater + vaccines_administered), 
  
  # formula for log(phi_it) scaled with population 
  ne = list(f = ~ -1 + fe(log(pop), unitSpecific = TRUE) + stay_at_home + closures, 
            weights = W_powerlaw(maxlag = 10)), # (w_ji) matrix
  
  family = "NegBin1", # distribution model
  
  # Optimizing settings
  optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                   regression = list(method = "nlminb"), # for penLogLik
                   variance = list(method = "nlminb")), # for marLogLik
  
  verbose = FALSE, # level of progress reporting
  
  # named list of covariates
  data = list(pop = population(phuSTS),
              percent_positivity = as.numeric(percent_positivity[, -c(1,2)]),
              seroprevalence = as.numeric(seroprevalence[, -c(1,2)]),
              wastewater = as.numeric(wastewater[, -c(1,2)]),
              vaccines_administered = as.numeric(vaccines_administered[, -c(1,2)]),
              stay_at_home = as.matrix(stay_at_home[, -c(1,2)]),
              closures = as.matrix(closures[, -c(1,2)])), 
  
  keep.terms = FALSE # whether to keep the model terms
  
)

# Generate the model using the PHU STS object and specified model settings
hhh4_model3_PHU <- hhh4(stsObj = phuSTS, control = model3_PHU)

# Get summary statistics for PHU model 3 fit
summary(hhh4_model3_PHU)
# Retrieve AIC
AIC(hhh4_model3_PHU)

#******************************************************************************************

###

# Model formula call for model 4 for Ontario
model4_ONT = list(
  
  # formula for log(nu_it)
  end = list(f = addSeason2formula(~ 1 + t + percent_positivity + seroprevalence + hospitalizations, period = phuSTS_ONT@freq), offset = population(phuSTS_ONT)), # multiplicative offset e_it
  
  # formula for log(lambda_it)
  ar = list(f = ~ 1 + wastewater + vaccines_administered + face_covering), 
  
  family = "NegBin1", # distribution model
  
  # Optimizing settings
  optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                   regression = list(method = "nlminb"), # for penLogLik
                   variance = list(method = "nlminb")), # for marLogLik
  
  verbose = FALSE, # level of progress reporting
  
  # named list of covariates
  data = list(percent_positivity = as.numeric(percent_positivity[, 2]),
              seroprevalence = as.numeric(seroprevalence[, 2]),
              hospitalizations = as.numeric(hospitalizations[, 2]),
              wastewater = as.numeric(wastewater[, 2]),
              vaccines_administered = as.numeric(vaccines_administered[, 2]),
              face_covering = as.numeric(face_covering[, 2])), 
  
  keep.terms = FALSE # whether to keep the model terms
  
)

# Generate the model using the ONT STS object and specified model settings
hhh4_model4_ONT <- hhh4(stsObj = phuSTS_ONT, control = model4_ONT)

# Get summary statistics for ONT model 4 fit
summary(hhh4_model4_ONT)
# Retrieve AIC
AIC(hhh4_model4_ONT)


# Model formula call for model 4 for PHUs
model4_PHU = list(
  
  # formula for log(nu_it)
  end = list(f = addSeason2formula(~ -1 + fe(1, unitSpecific = TRUE) + t + percent_positivity + seroprevalence + hospitalizations, period = phuSTS@freq), 
             offset = population(phuSTS)), # multiplicative offset e_it
  
  # formula for log(lambda_it)
  ar = list(f = ~ -1 + fe(1, unitSpecific = TRUE) + wastewater + vaccines_administered + face_covering), 
  
  # formula for log(phi_it) scaled with population 
  ne = list(f = ~ -1 + fe(log(pop), unitSpecific = TRUE) + stay_at_home + closures + mobility_data, 
            weights = W_powerlaw(maxlag = 10)), # (w_ji) matrix
  
  family = "NegBin1", # distribution model
  
  # Optimizing settings
  optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                   regression = list(method = "nlminb"), # for penLogLik
                   variance = list(method = "nlminb")), # for marLogLik
  
  verbose = FALSE, # level of progress reporting
  
  # named list of covariates
  data = list(pop = population(phuSTS),
              percent_positivity = as.numeric(percent_positivity[, -c(1,2)]),
              seroprevalence = as.numeric(seroprevalence[, -c(1,2)]),
              hospitalizations = as.numeric(hospitalizations[, -c(1,2)]),
              wastewater = as.numeric(wastewater[, -c(1,2)]),
              vaccines_administered = as.numeric(vaccines_administered[, -c(1,2)]),
              face_covering = as.numeric(face_covering[, -c(1,2)]),
              stay_at_home = as.matrix(stay_at_home[, -c(1,2)]),
              closures = as.matrix(closures[, -c(1,2)]),
              mobility_data = as.numeric(mobility_data[, -c(1,2)])), 
  
  keep.terms = FALSE # whether to keep the model terms
  
)

# Generate the model using the PHU STS object and specified model settings
hhh4_model4_PHU <- hhh4(stsObj = phuSTS, control = model4_PHU)

# Get summary statistics for PHU model 4 fit
summary(hhh4_model4_PHU)
# Retrieve AIC
AIC(hhh4_model4_PHU)

#******************************************************************************************

### Section to create plot of model comparison by AIC

# Create AIC data frame
data <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4"), 
  ONT = c(AIC(hhh4_model1_ONT), AIC(hhh4_model2_ONT), AIC(hhh4_model3_ONT), AIC(hhh4_model4_ONT)),
  PHU = c(AIC(hhh4_model1_PHU), AIC(hhh4_model2_PHU), AIC(hhh4_model3_PHU), AIC(hhh4_model4_PHU))
)

# Lollipop plot to compare model AIC
AIC_comparison_plot <- ggplot(data, aes(x = Model, y = PHU)) + 
  geom_point(color = "orange", size = 4) + 
  geom_segment(aes(x = Model, xend = Model, y = 150000, yend = PHU)) +
  ggtitle("Model AIC Comparison") + 
  xlab(NULL) + 
  ylab("AIC") +
  scale_y_continuous(
    labels = label_number(),  
    breaks = seq(150000, 250000, by = 25000),  
    limits = c(150000, 250000)
    ) + 
  theme(
    panel.grid.major.x = element_blank(), 
    panel.border = element_blank(), 
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
    )

# Display the plot
AIC_comparison_plot

#******************************************************************************************

### Use this section to learn how to call the model formula, make predictions, and conduct quantile analysis for Ontario

# Define the model formula for Ontario
model_ONT = list(
  
  # formula for log(nu_it) using seasonality
  end = list(f = addSeason2formula(~ 1 + t, period = phuSTS_ONT@freq), 
             offset = population(phuSTS_ONT)), 
  
  # formula for log(lambda_it)
  ar = list(f = ~ 1), 
  
  family = "NegBin1", # distribution model
  
  optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                   regression = list(method = "nlminb"), # for penLogLik
                   variance = list(method = "nlminb")), # for marLogLik
  
  verbose = FALSE, # level of progress reporting
  
  keep.terms = FALSE # whether to keep the model terms
  
)

# Generate the model using the ONT STS object and specified model settings
hhh4_model_ONT <-  hhh4(stsObj = phuSTS_ONT, control = model_ONT)

# Get the last observed value of the Ontario cases
y.start_ONT <- tail(observed(phuSTS_ONT), n = 1)

# Get the number of observations in the STS object
n_obs_ONT <- nrow(observed(phuSTS_ONT))

# Simulate future cases using the fitted Ontario model, run 1000 iterations
future_sim_ONT <- simulate(hhh4_model_ONT, nsim = 1000, seed = randomNumbers(1, 1, 1000, 1), 
                           subset = 1:n_obs_ONT, 
                           y.start = y.start_ONT, simplify = T)

# Calculate the median of the predicted counts from the simulation
predicted_counts <- round(apply(future_sim_ONT, c(1, 2), median), 0)

# Get the predicted counts for the specified number of future weeks
future_pred_ONT <- predicted_counts[1:as.numeric(input$weeks), 1]

# Generate future dates 
future_dates_ONT <- seq(
  from = max(as.Date(cases[, 1])) + 7, 
  by = "weeks", 
  length.out = as.numeric(input$weeks)
) 

# Create a data frame for future predictions 
future_data_ONT <- data.frame(`Week start date` = future_dates_ONT, 
                              `Ontario` = future_pred_ONT,
                              `Type` = "Predicted") 

# Set column names for the future data frame
colnames(future_data_ONT) <- c("Week start date", "Ontario", "Type")

# Convert the week start date column to Date format
future_data_ONT$`Week start date` <- as.Date(future_data_ONT$`Week start date`, 
                                             format = "%Y-%m-%d")

# Print future data frame
print(future_data_ONT)

# Create a data frame for observed data
observed_data_ONT <- data.frame(`Week start date` = cases[, 1], 
                                `Ontario` = cases[, 2],
                                `Type` = "Observed")

# Set column names for the observed data frame
colnames(observed_data_ONT) <- c("Week start date", "Ontario", "Type")

# Convert the week start date column to Date format for observed data
observed_data_ONT$`Week start date` <- as.Date(observed_data_ONT$`Week start date`, 
                                               format = "%Y-%m-%d")

# Print future data frame
print(observed_data_ONT)

# Combine the observed and future data into one data frame
combined_counts_ONT <- rbind(observed_data_ONT, future_data_ONT)

# Calculate quantiles and mean across iterations
quantiles <- round(apply(future_sim_ONT, c(1, 2), quantile, probs = c(0.25, 0.75)), 0)

# Prepare data for plotting (replace with your actual data structure)
pred_df <- data.frame(
  `Week start date` = future_dates_ONT,
  `Ontario` = future_pred_ONT,
  `25%` = quantiles[1, 1:input$weeks, ], # 25th percentile
  `75%` = quantiles[2, 1:input$weeks, ]  # 75th percentile
)

# Rename predicted data frame columns
colnames(pred_df) <- c("Week start date", "Ontario", "25%", "75%")

#******************************************************************************************

### Use this section to learn how to generate the updated plot for Ontario

# Generate updated ggplot for historical, observed future, predicted case counts for Ontario
updated_plot_ONT <- ggplot() + 
  # Historical cases 
  geom_line(data = raw_cases[which(raw_cases$`Week start date` == as.Date(input$dateRange[1])):(nrow(cases)),], 
            aes(x = `Week start date`, y = `Ontario`, 
                color = "Historical Cases", linetype = "Historical Cases"), linewidth = 1) + 
  geom_point(data = raw_cases[which(raw_cases$`Week start date` == as.Date(input$dateRange[1])):(nrow(cases)),], 
             aes(x = `Week start date`, y = `Ontario`, 
                 color = "Historical Cases"), size = 1, show.legend = FALSE) + 
  # Observed future cases (dashed) 
  geom_line(data = raw_cases[(nrow(cases)+1):(nrow(cases)+nrow(future_data_ONT)),], 
            aes(x = `Week start date`, y = `Ontario`, 
                color = "Observed Future Cases", linetype = "Observed Future Cases"), linewidth = 1) + 
  geom_point(data = raw_cases[(nrow(cases)+1):(nrow(cases)+nrow(future_data_ONT)),], 
             aes(x = `Week start date`, y = `Ontario`, 
                 color = "Observed Future Cases"), size = 1, show.legend = FALSE) + 
  # Predicted future cases 
  geom_line(data = future_data_ONT, 
            aes(x = `Week start date`, y = `Ontario`, 
                color = "Median Predicted Future Cases", linetype = "Predicted Future Cases"), linewidth = 1) + 
  geom_point(data = future_data_ONT, 
             aes(x = `Week start date`, y = `Ontario`, 
                 color = "Median Predicted Future Cases"), size = 1, show.legend = FALSE) + 
  # 25% quantile prediction 
  geom_line(data = pred_df, 
            aes(x = `Week start date`, y = `25%`, 
                color = "25% Quantile", linetype = "25% Quantile"), linewidth = 1) + 
  geom_point(data = pred_df, 
             aes(x = `Week start date`, y = `25%`, 
                 color = "25% Quantile"), size = 1, show.legend = FALSE) + 
  # 75% quantile prediction 
  geom_line(data = pred_df, 
            aes(x = `Week start date`, y = `75%`, 
                color = "75% Quantile", linetype = "75% Quantile"), linewidth = 1) + 
  geom_point(data = pred_df, 
             aes(x = `Week start date`, y = `75%`, 
                 color = "75% Quantile"), size = 1, show.legend = FALSE) + 
  # Shading area between Predicted Future Cases and 25% quantile 
  geom_ribbon(data = pred_df, 
              aes(x = `Week start date`, ymin = `25%`, ymax = `Ontario`), fill = "lightblue", alpha = 0.5) + 
  # Shading area between Predicted Future Cases and 75% quantile 
  geom_ribbon(data = pred_df, 
              aes(x = `Week start date`, ymin = `Ontario`, ymax = `75%`), fill = "lightcoral", alpha = 0.5) +
  # Customize the theme and legend 
  scale_color_manual(values = c("Historical Cases" = "black", 
                                "Observed Future Cases" = "green", 
                                "Median Predicted Future Cases" = "purple", 
                                "25% Quantile" = "blue", 
                                "75% Quantile" = "red")) + 
  scale_linetype_manual(values = c("Historical Cases" = "solid", 
                                   "Observed Future Cases" = "solid", 
                                   "Median Predicted Future Cases" = "solid", 
                                   "25% Quantile" = "dashed", 
                                   "75% Quantile" = "dashed")) + 
  # Setting plot labels
  labs(title = paste("COVID-19 Cases in", input$i), 
       color = "Legend", 
       linetype = "Legend") + 
  # Setting ggplot theme
  theme_minimal()
  
# Make the ggplot interactive
ggplotly(updated_plot_ONT)


### Use this section to learn how to generate the updated data table for Ontario

# Generating a vector of all the unique dates form the multiple case counts data
all_dates <- sort(unique(c(as.Date(cases[, "Week start date"]), future_dates_ONT, pred_df$`Week start date`)))

# Creating a data frame to hold all the dates
data <- data.frame(`Week start date` = all_dates)

# Merging each data set
data <- merge(data, data.frame(`Week start date` = combined_counts_ONT$`Week start date`[1:length(cases[,1])], 
                               `Historical Cases` = combined_counts_ONT$Ontario[1:length(cases[,1])]), 
              by = "Week.start.date", all = TRUE)
data <- merge(data, data.frame(`Week start date` = future_dates_ONT, 
                               `Observed Future Counts` = raw_cases[(nrow(cases)+1):
                                                                    (nrow(cases)+nrow(future_data_ONT)), "Ontario"]), 
              by = "Week.start.date", all = TRUE)
data <- merge(data, data.frame(`Week start date` = future_dates_ONT, `Median Predicted Future Counts` = pred_df$Ontario), 
              by = "Week.start.date", all = TRUE) 
data <- merge(data, data.frame(`Week start date` = future_dates_ONT, `25% Quantile` = pred_df$`25%`), 
              by = "Week.start.date", all = TRUE)
data <- merge(data, data.frame(`Week start date` = future_dates_ONT, `75% Quantile` = pred_df$`75%`), 
              by = "Week.start.date", all = TRUE) 

# Renaming the merged data frame columns
colnames(data) <- c("Week start date", "Historical Cases", "Observed Future Counts", 
                    "Median Predicted Future Counts", "25% Quantile", "75% Quantile")

# Inspect the combined data frame print(data)
View(data)

#******************************************************************************************

### Use this section to learn how to call the model formula, make predictions, and conduct quantile analysis for the Public Health Units

# Define the model for PHU
model_PHU = list(
  
  # formula for log(nu_it) with seasonality element
  end = list(f = addSeason2formula(~ -1 + fe(1, unitSpecific = TRUE) + t, 
                                   period = phuSTS@freq), 
             offset = population(phuSTS)), # optional multiplicative offset e_it
  
  # formula for log(lambda_it)
  ar = list(f = ~ -1 + fe(1, unitSpecific = TRUE)), 
  
  # formula for log(phi_it)
  ne = list(f = ~ -1 + fe(log(pop), unitSpecific = TRUE), 
            weights = W_powerlaw(maxlag = 10)), # weights power law
  
  family = "NegBin1", # distribution model
  
  # Optimizing settings
  optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                   regression = list(method = "nlminb"), # for penLogLik
                   variance = list(method = "nlminb")), # for marLogLik
  
  verbose = FALSE, # level of progress reporting
  
  data = list(pop = population(phuSTS)), # named list of covariates
  
  keep.terms = FALSE # whether to keep the model terms
  
)

# Generate the model using the PHU STS object and specified model settings
hhh4_model_PHU <-  hhh4(stsObj = phuSTS, control = model_PHU)

# Get the last observed value of the PHU cases
y.start_PHU <- tail(observed(phuSTS), n = 1)

# Simulate future cases using the fitted model
future_sim_PHU <- simulate(hhh4_model_PHU, nsim = 1000, seed = randomNumbers(1, 1, 1000, 1), 
                           subset = 1:nrow(phuSTS@observed), 
                           y.start = y.start_PHU, simplify = TRUE)

# Find the index of the selected PHU
phuIndex <- which(phu_names == input$i)

# Calculate the median of the predicted counts from the simulation
predicted_counts_PHU <- round(apply(future_sim_PHU, c(1, 2), median), 0)

# Get the predicted counts for the specified number of future weeks for the selected PHU
future_pred_PHU <- predicted_counts_PHU[1:input$weeks, phuIndex]

# Generate future dates 
future_dates_PHU <- seq(
  from = max(as.Date(cases[, 1])) + 7, 
  by = "weeks", 
  length.out = as.numeric(input$weeks)
) 

# Create a data frame for future predictions 
future_data_PHU <- data.frame(`Week start date` = future_dates_PHU, 
                              `Predicted` = future_pred_PHU,
                              `Type` = "Predicted") 

# Set column names for the future data frame
colnames(future_data_PHU) <- c("Week start date", input$i, "Type")

# Convert the week start date column to Date format
future_data_PHU$`Week start date` <- as.Date(future_data_PHU$`Week start date`, 
                                             format = "%Y-%m-%d")
# Print future data frame
print(future_data_PHU)

# Create a data frame for observed data
observed_data_PHU <- data.frame(`Week start date` = cases[, 1], 
                                `Predicted` = cases[, input$i],
                                `Type` = "Observed")

# Set column names for the observed data frame
colnames(observed_data_PHU) <- c("Week start date", input$i, "Type")

# Convert the week start date column to Date format for observed data
observed_data_PHU$`Week start date` <- as.Date(observed_data_PHU$`Week start date`, 
                                               format = "%Y-%m-%d")

# Print future data frame
print(observed_data_PHU)

# Combine the observed and future data into one data frame
combined_counts_PHU <- rbind(observed_data_PHU, future_data_PHU)


# Calculate quantiles and mean across iterations
quantiles_PHU <- round(apply(future_sim_PHU, c(1, 2), quantile, probs = c(0.25, 0.75)), 0)

# Prepare data for plotting (replace with your actual data structure)
pred_df_PHU <- data.frame(
  `Week start date` = future_dates_PHU,
  `temp` = future_pred_PHU,
  `25%` = quantiles_PHU[1, 1:input$weeks, input$i], # 25th percentile
  `75%` = quantiles_PHU[2, 1:input$weeks, input$i]  # 75th percentile
)

# Renames the predicted data frame columns
colnames(pred_df_PHU) <- c("Week start date", input$i, "25%", "75%")

#******************************************************************************************

### Use this section to learn how to generate the updated plot for the Public Health Units

# Generate updated ggplot for historical, observed future, predicted case counts for PHUs
updated_plot_PHU <- ggplot() + 
  # Historical cases
  geom_line(data = raw_cases[which(raw_cases$`Week start date` == as.Date(input$dateRange[1])):(nrow(cases)), -2], 
            aes(x = `Week start date`, y = !!sym(input$i), 
                color = "Historical Cases", linetype = "Historical Cases"), linewidth = 1) +
  # Observed future cases (dashed)
  geom_line(data = raw_cases[(nrow(cases) + 1):(nrow(cases) + nrow(future_data_PHU)), -2], 
            aes(x = `Week start date`, y = !!sym(input$i), 
                color = "Observed Future Cases", linetype = "Observed Future Cases"), linewidth = 1) +
  # Predicted future cases
  geom_line(data = pred_df_PHU, 
            aes(x = `Week start date`, y = pred_df_PHU[, 2], 
                color = "Median Predicted Future Cases", linetype = "Median Predicted Future Cases"), linewidth = 1) +
  # 25% quantile prediction
  geom_line(data = pred_df_PHU, 
            aes(x = `Week start date`, y = `25%`, 
                color = "25% Quantile", linetype = "25% Quantile"), linewidth = 1) +
  # 75% quantile prediction
  geom_line(data = pred_df_PHU, 
            aes(x = `Week start date`, y = `75%`, 
                color = "75% Quantile", linetype = "75% Quantile"), linewidth = 1) +
  # Customize the theme and legend
  scale_color_manual(values = c("Historical Cases" = "black",
                                "Observed Future Cases" = "green",
                                "Median Predicted Future Cases" = "purple",
                                "25% Quantile" = "blue",
                                "75% Quantile" = "red")) +
  scale_linetype_manual(values = c("Historical Cases" = "solid",
                                   "Observed Future Cases" = "solid",
                                   "Median Predicted Future Cases" = "solid",
                                   "25% Quantile" = "dashed",
                                   "75% Quantile" = "dashed")) +
  # Setting plot labels
  labs(title = paste("COVID-19 Cases in", input$i), color = "Legend", linetype = "Legend") +
  # Setting ggplot theme
  theme_minimal()
  
# Make the ggplot interactive
ggplotly(updated_plot_PHU)


### Use this section to learn how to generate the updated data table for the Public Health Units

# Create a vector of all the unique dates of multiple case counts data
all_dates <- sort(unique(c(as.Date(cases[, "Week start date"]), future_dates_PHU, pred_df_PHU$`Week start date`)))

# Create a new data frame to hold all the dates
data <- data.frame(`Week start date` = all_dates)

# Merging each data set
data <- merge(data, data.frame(`Week start date` = combined_counts_PHU$`Week start date`[1:length(cases[,1])], 
                               `Historical Cases` = combined_counts_PHU$`Algoma Public Health Unit`[1:length(cases[,1])]), 
              by = "Week.start.date", all = TRUE)
data <- merge(data, data.frame(`Week start date` = future_dates_PHU, 
                               `Observed Future Counts` = raw_cases[(nrow(cases)+1):(nrow(cases)+nrow(future_data_ONT)), 
                                                                    input$i]), 
              by = "Week.start.date", all = TRUE)
data <- merge(data, data.frame(`Week start date` = future_dates_PHU, 
                               `Median Predicted Future Counts` = pred_df_PHU$`Algoma Public Health Unit`), 
              by = "Week.start.date", all = TRUE) 
data <- merge(data, data.frame(`Week start date` = future_dates_PHU, `25% Quantile` = pred_df_PHU$`25%`), 
              by = "Week.start.date", all = TRUE)
data <- merge(data, data.frame(`Week start date` = future_dates_PHU, `75% Quantile` = pred_df_PHU$`75%`), 
              by = "Week.start.date", all = TRUE) 

# Rename the merged data frame columns
colnames(data) <- c("Week start date", "Historical Cases", "Observed Future Counts", 
                    "Median Predicted Future Counts", "25% Quantile", "75% Quantile")

# Inspect the combined data frame print(data)
View(data)

#******************************************************************************************

### Using this section to learn how to import, write to, and export data files with inputs from Shiny ui elements

# Load the existing COVID-19 case data from a CSV file into a data frame
cases <- read.csv(file = "cases.csv", header = TRUE)

# Manually set the inputs to test data input
sample_week_start_date <- "2023-01-05"
input <- 2

# Create a new data frame for the latest case data input by the user
new_case_data <- data.frame(`Week start date` = sample_week_start_date, 
                            `Ontario` = input,
                            `Algoma Public Health Unit`= input,
                            `Brant County Health Unit` = input,
                            `Chatham-Kent Health Unit` = input,
                            `Durham Region Health Department` = input,
                            `Eastern Ontario Health Unit` = input, 
                            `Grey Bruce Health Unit` = input, 
                            `Haldimand-Norfolk Health Unit` = input, 
                            `Haliburton, Kawartha, Pine Ridge District Health Unit` = input, 
                            `Halton Region Health Department` = input, 
                            `Hamilton Public Health Services` = input, 
                            `Hastings and Prince Edward Counties Health Unit` = input, 
                            `Huron Perth Health Unit` = input,
                            `Kingston, Frontenac and Lennox and Addington Health Unit` = input, 
                            `Lambton Public Health` = input, 
                            `Leeds, Grenville and Lanark District Health Unit` = input, 
                            `Middlesex-London Health Unit` = input, 
                            `Niagara Region Public Health Department` = input, 
                            `North Bay Parry Sound District Health Unit` = input, 
                            `Northwestern Health Unit` = input, 
                            `Ottawa Public Health` = input, 
                            `Peel Public Health` = input, 
                            `Peterborough Public Health` = input, 
                            `Porcupine Health Unit` = input, 
                            `Region of Waterloo, Public Health` = input,
                            `Renfrew County and District Health Unit` = input, 
                            `Simcoe Muskoka District Health Unit` = input, 
                            `Southwestern Public Health` = input, 
                            `Sudbury and District Health Unit` = input,
                            `Thunder Bay District Health Unit` = input, 
                            `Timiskaming Health Unit` = input, 
                            `Toronto Public Health` = input, 
                            `Wellington-Dufferin-Guelph Health Unit` = input, 
                            `Windsor-Essex County Health Unit` = input, 
                            `York Region Public Health` = input)

# Print the new case data to the console for verification
print(new_case_data)

# Append the new case data to the existing data frame
cases <- rbind(cases, new_case_data)

# Have a look at the new combined date frame
View(cases)

# Export new 'cases' as a .csv file for import again into Shiny app
write.csv(x = cases, file = "cases.csv", row.names = FALSE)

#******************************************************************************************