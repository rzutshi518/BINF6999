#******************************************************************************************

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find more information, data, and working code related to the Shiny app here:
#
# https://github.com/rzutshi518/BINF6999

#******************************************************************************************

### Installing and loading required packages

if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
library(shiny)

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)

if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
library(plotly)

if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
library(patchwork)

if(!requireNamespace("random", quietly = TRUE)) {
  install.packages("random")
}
library(random)

if (!requireNamespace("bslib", quietly = TRUE)) {
  install.packages("bslib")
}
library(bslib)

if (!requireNamespace("DT", quietly = TRUE)) {
  install.packages("DT")
}
library(DT)

if (!requireNamespace("surveillance", quietly = TRUE)) {
  install.packages("surveillance")
}
library(surveillance)

if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
library(devtools)

if (!requireNamespace("gridlayout", quietly = TRUE)) {
  devtools::install_github("rstudio/gridlayout")
}
library(gridlayout)

if (!requireNamespace("shinyjs", quietly = TRUE)) {
  install.packages("shinyjs")
}
library(shinyjs)

if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}
library(sf)

if (!requireNamespace("sp", quietly = TRUE)) {
  install.packages("sp")
}
library(sp)

if (!requireNamespace("spdep", quietly = TRUE)) {
  install.packages("spdep")
}
library(spdep)

#******************************************************************************************

### Data importing

# Importing raw data from excel file
raw_data <- read_excel("raw_case_data.xlsx",
                       col_types = c("text", "text", "text",
                                     "text", "date", "date", "numeric",
                                     "numeric", "numeric"))
# Replacing all NA values with 0
raw_data[is.na(raw_data)] <- 0


# Importing raw_cases data

# Read in `raw_cases` data from .csv file
raw_cases <- read.csv("raw_cases.csv", check.names = FALSE)
# Convert the 'Week start date' column to Date format
raw_cases$`Week start date` <- as.Date(raw_cases$`Week start date`, format = "%Y-%m-%d")
# Convert columns from the second to the last to numeric format
raw_cases[, 2:ncol(raw_cases)] <- apply(raw_cases[, 2:ncol(raw_cases)], 2, as.numeric)


# Importing in subsetted cases data for 2020-2022

# Read in `cases` .csv file
cases <- read.csv("cases.csv", check.names = FALSE)
# Convert the 'Week start date' column to Date format
cases$`Week start date` <- as.Date(cases$`Week start date`, format = "%Y-%m-%d")
# Convert columns from the second to the last to numeric format
cases[, 2:ncol(cases)] <- apply(cases[, 2:ncol(cases)], 2, as.numeric)
# Converting the 'cases' date frame to a matrix
cases <- as.matrix(cases)


# Importing the population data

# Read in the 'population' csv file
population <- read.csv("population.csv", check.names = FALSE)
# Convert the 'Week start date' column to Date format
population$`Week start date` <- as.Date(population$`Week start date`, format = "%Y-%m-%d")
# Convert columns from the second to the last to numeric format
population[, 2:ncol(population)] <- apply(population[, 2:ncol(population)], 2, as.numeric)
# Converting the 'population' date frame to a matrix
population <- as.matrix(population)


# Importing predictor data

# Read in the 'percent_positivity' CSV file
percent_positivity <- read.csv("percent_positivity.csv", check.names = FALSE)
# Convert the 'percent_positivity' data frame to a matrix
percent_positivity <- as.matrix(percent_positivity)

# Read in the 'wastewater' CSV file
wastewater <- read.csv("wastewater_data.csv", check.names = FALSE)
# Convert the 'wastewater' data frame to a matrix
wastewater <- as.matrix(wastewater)

# Read in the 'stay_at_home' CSV file
stay_at_home <- read.csv("stay_at_home_restrictions.csv", check.names = FALSE)
# Remove the first column from the 'stay_at_home' data frame
stay_at_home <- stay_at_home[, -1]
# Convert the 'stay_at_home' data frame to a matrix
stay_at_home <- as.matrix(stay_at_home)

# Read in the 'seroprevalence' CSV file
seroprevalence <- read.csv("seroprevalence.csv", check.names = FALSE)
# Remove the first column from the 'seroprevalence' data frame
seroprevalence <- seroprevalence[, -1]
# Convert the 'seroprevalence' data frame to a matrix
seroprevalence <- as.matrix(seroprevalence)

# Read in the 'vaccines_administered' CSV file
vaccines_administered <- read.csv("vaccines_administered.csv", check.names = FALSE)
# Convert the 'vaccines_administered' data frame to a matrix
vaccines_administered <- as.matrix(vaccines_administered)

# Read in the 'closures' CSV file
closures <- read.csv("school_workplace_closures.csv", check.names = FALSE)
# Remove the first column from the 'closures' data frame
closures <- closures[, -1]
# Convert the 'closures' data frame to a matrix
closures <- as.matrix(closures)

#******************************************************************************************

### Variable and function mapping

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

# Custom function to get the next available date
get_next_available_date <- function() {
  
  # Find the maximum date in the date column
  last_date <- max(as.Date(cases[, "Week start date"]), na.rm = TRUE)
  
  # Return a date one week after the last date
  return(last_date + 7)
  
}

#******************************************************************************************

### Creating the map and neighborhood adjacency matrix

# Read the shapefile
phu_shapefile <- st_read("MOH_PHU_BOUNDARY/MOH_PHU_BOUNDARY.shp")

# Order shapefile object
phu_shapefile <- phu_shapefile[order(phu_shapefile$NAME_ENG), ]

# Convert to SpatialPolygons object if needed
phu_spatial <- as(phu_shapefile, "Spatial")

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

#******************************************************************************************

### Define UI for application

ui <- page_navbar(
  useShinyjs(),
  title = "COVID-19 endemic-epidemic hhh4 Model",
  position = "static-top",
  collapsible = FALSE,
  selected = "landingPage",
  nav_spacer(),
  # Creating page header with reset action buttons
  header = tags$header(
    actionButton(inputId = "clearInputs", label = "Clear Inputs", width = "100%"), 
    actionButton(inputId = "clearOutputs", label = "Clear Outputs", width = "100%")
  ),
  # Creating landing page tab
  nav_panel(
    title = "Introduction",
    value = "landingPage",
    theme = bs_theme(), 
    fluidRow(
      column(
        width = 8, offset = 2,
        h3("Introduction"),
        hr(),
        p("The COVID-19 pandemic, caused by the SARS-CoV-2 virus, dramatically reshaped our world since its emergence in late 2019. While initial waves have subsided, new variants continue to keep case numbers at endemic levels, posing ongoing challenges. These variants exhibit changing viral properties, making it crucial to adapt our mitigation policies and reconsider population health strategies. Despite extensive efforts, much remains unknown about how these evolving factors impact new case trends."),
        hr(),
        p("In this app, an endemic-epidemic modelling approach based on the work of the 'hhh4' model has been implemented to allows users to analyze and predict COVID-19 trends of Ontario and its Public Health Units. The model incorporates both viral and population health predictors, as well as a network of spatial interactions between the units. Quantile analysis of the predictions is also performed for model evalutation."),
        hr(),
        p("The source data and working code for the model/app can be found at https://github.com/rzutshi518/BINF6999. The Clear Outputs button can be used to reset the plot or table output back to the original view. The Clear Inputs button can be used to reset any selections on the Prediction Tool tab and any inputs on the Data Submission page. ")
      )
    )
  ),
  # Creating Prediction Tool page tab
  nav_panel(
    title = "Prediction Tool",
    value = "predictionTool",
    grid_page(
      layout = c(
        "sidebar display_area"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "400px",
        "1fr"
      ),
      # Creating a sidebar element to contain the settings
      grid_card(
        area = "sidebar",
        card_header("Settings"),
        card_body(
          "The Prediction Tool panel integrates a trained 'hhh4' model and allows the user to generated future predctions of case counts. The output window initially displays a plot (or table) of case counts over time for Ontario or a selected Public Health Unit. To run the model simulation, select a region and specify how many weeks to predict forward, then click 'Run Model'. Please wait for the progress bar to appear and complete for the outputs to update. The date range slider can be used to adjust the plot view. Note: The right hand slider will remain fixed once prediction plots are generated. Please use the Clear Outputs button before re-running model.",
          # Drop-down selection of which region to present data for and analyze
          selectInput(
            inputId = "i",
            label = "Public Health Units",
            choices = c(public_health_units[1], 
                        sort(public_health_units[2:length(public_health_units)])),
            selected = "Ontario",
            width = "100%"
          ),
          # Slider input of a date range of the cases data
          sliderInput(
            inputId = "dateRange", 
            label = "Select Date Range:", 
            min = min(as.Date(cases[, "Week start date"])), 
            max = max(as.Date(cases[, "Week start date"])),
            step = 7,
            value = c(min(as.Date(cases[, "Week start date"])), 
                      max(as.Date(cases[, "Week start date"]))), 
            timeFormat = "%Y-%m-%d", 
            dragRange = TRUE
          ),
          # Buttons to select how many weeks units forward to predict
          radioButtons(
            inputId = "weeks",
            label = "Weeks Predicted Forward",
            choices = list("1" = "1", "2" = "2", "4" = "4", "8" = "8", "12" = "12", "16" = "16"),
            selected = "1",
            width = "100%"
          ),
          actionButton(inputId = "submit", label = "Run Model")
        )
      ),
      # Creating the output area ui
      grid_card(
        area = "display_area",
        card_body(
          tabsetPanel(
            # Output for plot
            nav_panel(
              title = "Plot",
              nav_spacer(),
              nav_spacer(),
              nav_spacer(),
              uiOutput(outputId = "myPlot", width = "100%", height = "auto", fill = TRUE)
            ),
            # Output for table
            nav_panel(
              title = "Table",
              uiOutput(outputId = "myTable", width = "100%", height = "auto", fill = TRUE)
            )
          )
        )
      )
    )
  ),
  # Designing the Data Input submission page tab
  nav_panel(
    title = "Data Inputs",
    value = "dataInputs",
    page_sidebar(
      collapsible = FALSE,
      theme = bslib::bs_theme(),
      # Creating a sidebar to contain the settings 
      sidebar = sidebar(
        title = NULL,
        "Enter reported case counts for all Public Health Units and Ontario. Select the submission date for the reported counts. The first available date will be set as a week ahead of the last date data is reported for. On submission, the app will close to save the data locally, therefore, please rerun the app to incorporate the new data and for further analysis.",
        # Date input showing next available date in cases data 
        dateInput(
          inputId = "submissionDate",
          label = "Select input date",
          value = get_next_available_date(),
          format = "yyyy-mm-dd",
          startview = "month"
        ),
        # Action button to submit input data
        actionButton(
          inputId = "submitData", 
          label = "Submit Data"
        )
      ),
      nav_panel(
        title = NULL,
        grid_container(
          layout = c(
            "area1 area2 area3 area4 area5",
            "area1 area2 area3 area4 area5"
          ),
          gap_size = "10px",
          col_sizes = c(
            "1fr",
            "1fr",
            "1fr",
            "1fr",
            "1fr"
          ),
          row_sizes = c(
            "1fr",
            "1fr"
          ),
          grid_card(
            area = "area1",
            card_body(
              numericInput(
                inputId = "Ontario_input",
                label = "Ontario",
                value = 0
              ),
              numericInput(
                inputId = "Algoma_input",
                label = "Algoma",
                value = 0
              ),
              numericInput(
                inputId = "BrantCounty_input",
                label = "Brant County",
                value = 0
              ),
              numericInput(
                inputId = "Chatham-Kent_input",
                label = "Chatham-Kent",
                value = 0
              ),
              numericInput(
                inputId = "DurhamRegion_input",
                label = "Durham Region",
                value = 0
              ),
              numericInput(
                inputId = "EasternOntario_input",
                label = "Eastern Ontario",
                value = 0
              ),
              numericInput(
                inputId = "GreyBruce_input",
                label = "Grey Bruce",
                value = 0
              )
            )
          ),
          grid_card(
            area = "area2",
            full_screen = TRUE,
            card_body(
              numericInput(
                inputId = "Haldimand-Norfolk_input",
                label = "Haldimand-Norfolk",
                value = 0
              ),
              numericInput(
                inputId = "HaliburtonKawarthaPineRidgeDistrict_input",
                label = "Haliburton, Kawartha, Pine Ridge District",
                value = 0
              ),
              numericInput(
                inputId = "HaltonRegion_input",
                label = "Halton Region",
                value = 0
              ),
              numericInput(
                inputId = "Hamilton_input",
                label = "Hamilton",
                value = 0
              ),
              numericInput(
                inputId = "HastingsAndPrinceEdwardCounties_input",
                label = "Hastings and Prince Edward Counties",
                value = 0
              ),
              numericInput(
                inputId = "HuronPerth_input",
                label = "Huron Perth",
                value = 0
              ),
              numericInput(
                inputId = "KingstonFrontenacAndLennoxAndAddington_input",
                label = "Kingston, Frontenac and Lennox and Addington",
                value = 0
              )
            )
          ),
          grid_card(
            area = "area3",
            full_screen = TRUE,
            card_body(
              numericInput(
                inputId = "Lambton_input",
                label = "Lambton",
                value = 0
              ),
              numericInput(
                inputId = "LeedsGrenvilleAndLanarkDistrict_input",
                label = "Leeds, Grenville and Lanark District",
                value = 0
              ),
              numericInput(
                inputId = "Middlesex-London_input",
                label = "Middlesex-London",
                value = 0
              ),
              numericInput(
                inputId = "NiagaraRegion_input",
                label = "Niagara Region",
                value = 0
              ),
              numericInput(
                inputId = "NorthBayParrySoundDistrict_input",
                label = "North Bay Parry Sound District",
                value = 0
              ),
              numericInput(
                inputId = "Northwestern_input",
                label = "Northwestern",
                value = 0
              ),
              numericInput(
                inputId = "Ottawa_input",
                label = "Ottawa",
                value = 0
              )
            )
          ),
          grid_card(
            area = "area4",
            full_screen = TRUE,
            card_body(
              numericInput(
                inputId = "Peel_input",
                label = "Peel",
                value = 0
              ),
              numericInput(
                inputId = "Peterborough_input",
                label = "Peterborough",
                value = 0
              ),
              numericInput(
                inputId = "Porcupine_input",
                label = "Porcupine",
                value = 0
              ),
              numericInput(
                inputId = "RegionOfWaterloo_input",
                label = "Region of Waterloo",
                value = 0
              ),
              numericInput(
                inputId = "RenfrewCountyAndDistrict_input",
                label = "Renfrew County and District",
                value = 0
              ),
              numericInput(
                inputId = "SimcoeMuskokaDistrict_input",
                label = "Simcoe Muskoka District",
                value = 0
              ),
              numericInput(
                inputId = "Southwestern_input",
                label = "Southwestern",
                value = 0
              )
            )
          ),
          grid_card(
            area = "area5",
            full_screen = TRUE,
            card_body(
              numericInput(
                inputId = "SudburyAndDistrict_input",
                label = "Sudbury and District",
                value = 0
              ),
              numericInput(
                inputId = "ThunderBayDistrict_input",
                label = "Thunder Bay District",
                value = 0
              ),
              numericInput(
                inputId = "Timiskaming_input",
                label = "Timiskaming",
                value = 0
              ),
              numericInput(
                inputId = "Toronto_input",
                label = "Toronto",
                value = 0
              ),
              numericInput(
                inputId = "Wellington-Dufferin-Guelph_input",
                label = "Wellington-Dufferin-Guelph",
                value = 0
              ),
              numericInput(
                inputId = "Windsor-EssexCounty_input",
                label = "Windsor-Essex County",
                value = 0
              ),
              numericInput(
                inputId = "YorkRegion_input",
                label = "York Region",
                value = 0
              )
            )
          )
        )
      )
    )
  )
)

#******************************************************************************************  

### Define server logic
server <- function(input, output, session) {
  
  # Creating reactive values to store whether the data from the model is generated or not
  data_created_ONT <- reactiveVal(FALSE)
  data_created_PHU <- reactiveVal(FALSE)
  
  # Render the appropriate ui based on the condition to plot output
  output$myPlot <- renderUI({
    
    # Conditional statement to select which plot output is displayed
    if (data_created_ONT()) {
      plotlyOutput("updatedPlot_ONT")  # Show updatedPlot_ONT if ONT values are created
    } else if (data_created_PHU()) {
      plotlyOutput("updatedPlot_PHU")  # Show updatedPlot_PHU if PHU values are created
    } else {
      plotlyOutput("initialPlot")  # Show initialPlot by default
    }
    
  })
  
  # Render the appropriate ui based on the condition to table output 
  output$myTable <- renderUI({
    
    # Conditional statement to select which table output is displayed
    if (data_created_ONT()) {
      DTOutput("updatedTable_ONT")    # Show updatedTable_ONT if ONT values are created
    } else if (data_created_PHU()) {
      DTOutput("updatedTable_PHU")    # Show updatedTable_PHU if PHU values are created
    } else {
      DTOutput("initialTable")    # Show the initialTable by default
    }
    
  })

  # Render the Plotly object to initialPlot output
  output$initialPlot <- renderPlotly({
    
    # Generate initial plot of cases over tiem for selected region
    initialPlot <- raw_data %>% 
      filter(`Public health unit` == input$i) %>%
      filter(`Week start date` >= input$dateRange[1] & `Week start date` <= input$dateRange[2]) %>%
      ggplot(aes(x = `Week start date`, y = `Number of cases`, color = "black")) +
      geom_line(color = "black") +
      geom_point(color = "black") +
      labs(title = paste("COVID-19 Cases in", input$i), 
           x = "Week Start Date", 
           y = "Number of Cases") + 
      theme_minimal() +
      theme(legend.position = "none")
    
    # Make the ggplot interactive
    ggplotly(initialPlot)
    
  })
  
  # Render the data table to the initialTable output
  output$initialTable <- renderDT({
    
    # Generate initial data table of cases over time of selected region
    raw_data %>%
      filter(`Public health unit` == input$i) %>%
      filter(`Week start date` >= input$dateRange[1] & `Week start date` <= input$dateRange[2]) %>%
      mutate(`Week start date` = as.Date(`Week start date`),
             `Week end date` = as.Date(`Week end date`),
             `Total Cases` = cumsum(`Number of cases`),
             `Cases per 100,000 population` = round(`Cases per 100,000 population`, digits = 3)) %>%
      select(`Week start date`, `Number of cases`, `Total Cases`, `Population`, `Cases per 100,000 population`)
    
  })
  
  # Function to run on observing a click on the action button
  observeEvent(input$submit, {
    
    # Conditional statement for whether Ontario or an individual PHU is selected
    if (input$i == "Ontario") {
      
      # Subset the cases data to 2020-2022 and exclude the first column (week dates)
      temp_cases_ONT <- cases[1:157, -1]
      
      # Subset the population data excluding the dates column
      temp_population_ONT <- population[, -1]
      
      # Create STS object for Ontario cases only
      phuSTS_ONT <- sts(observed = as.numeric(temp_cases_ONT[, "Ontario"]), 
                        start = c(2020, 1), 
                        frequency = 52, 
                        population = matrix(as.numeric(temp_population_ONT[, "Ontario"])))
      
      # Define the model for Ontario
      model_ONT = list(
        
        # formula for log(nu_it) with seasonality
        end = list(f = addSeason2formula(~ 1 + t, period = phuSTS_ONT@freq), 
                   offset = population(phuSTS_ONT)), # multiplicative offset e_it
        
        # formula for log(lambda_it)
        ar = list(f = ~ 1), 
        
        family = "NegBin1", # distribution model
        
        # Optimizer settings
        optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                         regression = list(method = "nlminb"), # for penLogLik
                         variance = list(method = "nlminb")), # for marLogLik
        
        verbose = FALSE, # level of progress reporting
        
        keep.terms = FALSE # whether to keep the model terms
        
      )
      
      # Fit the hhh4 model using the defined control settings
      hhh4_model_ONT <-  hhh4(stsObj = phuSTS_ONT, control = model_ONT)
      
      # Get the last observed value of the Ontario cases
      y.start_ONT <- tail(observed(phuSTS_ONT), n = 1)
      
      # Get the number of observations in the STS object
      n_obs_ONT <- nrow(observed(phuSTS_ONT))
      
      # Simulate future cases using the fitted model, run 1000 iterations
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
      
      # Create a data frame for observed data
      observed_data_ONT <- data.frame(`Week start date` = cases[, 1], 
                                      `Ontario` = cases[, 2],
                                      `Type` = "Observed")
      
      # Set column names for the observed data frame
      colnames(observed_data_ONT) <- c("Week start date", "Ontario", "Type")
      
      # Convert the week start date column to Date format for observed data
      observed_data_ONT$`Week start date` <- as.Date(observed_data_ONT$`Week start date`, 
                                                     format = "%Y-%m-%d")
      
      # Combine the observed and future data into one data frame
      combined_counts_ONT <- rbind(observed_data_ONT, future_data_ONT)
      
      # Calculate quantiles across iterations
      quantiles <- round(apply(future_sim_ONT, c(1, 2), quantile, probs = c(0.25, 0.75)), 0)

      # Prepare data for plotting (replace with your actual data structure)
      pred_df <- data.frame(
        `Week start date` = future_dates_ONT,
        `Ontario` = future_pred_ONT,
        `25%` = quantiles[1, 1:input$weeks, ], # 25th percentile
        `75%` = quantiles[2, 1:input$weeks, ]  # 75th percentile
      )
      
      # Set column names for the prediction data
      colnames(pred_df) <- c("Week start date", "Ontario", "25%", "75%")

      # Render the updated plot for Ontario using Plotly
      output$updatedPlot_ONT <- renderPlotly({
        
        # Generate the updated plot
        updatedPlot_ONT <- ggplot() + 
        # Historical cases 
        geom_line(data = raw_cases[which(raw_cases$`Week start date` == as.Date(input$dateRange[1])):(nrow(cases)),], aes(x = `Week start date`, y = `Ontario`, color = "Historical Cases", linetype = "Historical Cases"), linewidth = 1) + 
        geom_point(data = raw_cases[which(raw_cases$`Week start date` == as.Date(input$dateRange[1])):(nrow(cases)),], aes(x = `Week start date`, y = `Ontario`, color = "Historical Cases"), size = 1, show.legend = FALSE) + 
        # Observed future cases (dashed) 
        geom_line(data = raw_cases[(nrow(cases)+1):(nrow(cases)+nrow(future_data_ONT)),], aes(x = `Week start date`, y = `Ontario`, color = "Observed Future Cases", linetype = "Observed Future Cases"), linewidth = 1) + 
        geom_point(data = raw_cases[(nrow(cases)+1):(nrow(cases)+nrow(future_data_ONT)),], aes(x = `Week start date`, y = `Ontario`, color = "Observed Future Cases"), size = 1, show.legend = FALSE) + 
        # Predicted future cases 
        geom_line(data = future_data_ONT, aes(x = `Week start date`, y = `Ontario`, color = "Predicted Future Cases", linetype = "Predicted Future Cases"), linewidth = 1) + 
        geom_point(data = future_data_ONT, aes(x = `Week start date`, y = `Ontario`, color = "Predicted Future Cases"), size = 1, show.legend = FALSE) + 
        # 25% quantile prediction 
        geom_line(data = pred_df, aes(x = `Week start date`, y = `25%`, color = "25% Quantile", linetype = "25% Quantile"), linewidth = 1) + 
        geom_point(data = pred_df, aes(x = `Week start date`, y = `25%`, color = "25% Quantile"), size = 1, show.legend = FALSE) + 
        # 75% quantile prediction 
        geom_line(data = pred_df, aes(x = `Week start date`, y = `75%`, color = "75% Quantile", linetype = "75% Quantile"), linewidth = 1) + 
        geom_point(data = pred_df, aes(x = `Week start date`, y = `75%`, color = "75% Quantile"), size = 1, show.legend = FALSE) + 
        # Shading area between Predicted Future Cases and 25% quantile 
        geom_ribbon(data = pred_df, aes(x = `Week start date`, ymin = `25%`, ymax = `Ontario`), fill = "lightblue", alpha = 0.5) + 
        # Shading area between Predicted Future Cases and 75% quantile 
        geom_ribbon(data = pred_df, aes(x = `Week start date`, ymin = `Ontario`, ymax = `75%`), fill = "lightcoral", alpha = 0.5) +
        # Customize the theme and legend 
        scale_color_manual(values = c("Historical Cases" = "black", 
                                      "Observed Future Cases" = "green", 
                                      "Predicted Future Cases" = "purple", 
                                      "25% Quantile" = "blue", 
                                      "75% Quantile" = "red")) + 
        scale_linetype_manual(values = c("Historical Cases" = "solid", 
                                         "Observed Future Cases" = "solid", 
                                         "Predicted Future Cases" = "solid", 
                                         "25% Quantile" = "dashed", 
                                         "75% Quantile" = "dashed")) + 
        # Setting plot labels
        labs(title = paste("COVID-19 Cases in", input$i), 
             color = "Legend", 
             linetype = "Legend") + 
        # Setting plot theme
        theme_minimal()
      
      # Make the ggplot interactive
      ggplotly(updatedPlot_ONT)
        
      })
      
      # Render the updated data table for Ontario using DataTables
      output$updatedTable_ONT <- renderDT({

        # Create a combined list of all dates
        all_dates <- sort(unique(c(as.Date(cases[, "Week start date"]), future_dates_ONT, pred_df$`Week start date`)))
        
        # Creating a new data frame of dates
        data <- data.frame(`Week start date` = all_dates[which(raw_cases$`Week start date` == as.Date(input$dateRange[1])):(nrow(cases))])
        
        # Merging each data set
        data <- merge(data, data.frame(
          `Week start date` = cases[which(raw_cases$`Week start date` == as.Date(input$dateRange[1])):(nrow(cases)),1], 
          `Historical Cases` = cases[which(raw_cases$`Week start date` == as.Date(input$dateRange[1])):(nrow(cases)):
                                       nrow(cases), "Ontario"]), by = "Week.start.date", all = TRUE)
        
        data <- merge(data, data.frame(
          `Week start date` = future_dates_ONT, 
          `Observed Future Counts` = raw_cases[(nrow(cases)+1):(nrow(cases)+nrow(future_data_ONT)), "Ontario"]), 
          by = "Week.start.date", all = TRUE)
        
        data <- merge(data, data.frame(`Week start date` = future_dates_ONT, 
                                       `Median Predicted Future Counts` = pred_df$Ontario), 
                      by = "Week.start.date", all = TRUE) 
        
        data <- merge(data, data.frame(`Week start date` = future_dates_ONT, 
                                       `25% Quantile` = pred_df$`25%`), 
                      by = "Week.start.date", all = TRUE)
        
        data <- merge(data, data.frame(`Week start date` = future_dates_ONT, 
                                       `75% Quantile` = pred_df$`75%`), 
                      by = "Week.start.date", all = TRUE) 
        
        # Assigning column names to data
        colnames(data) <- c("Week start date", "Historical Cases", "Observed Future Counts", "Median Predicted Future Counts", "25% Quantile", "75% Quantile")
        
        # Render the data table 
        datatable(data)
        
      })
      
      # Presenting progress bar for this code chunk
      withProgress(message = 'Processing...', value = 0, { 
        for (i in 1:10) {
          Sys.sleep(0.5) # Simulate a task by sleeping
          incProgress(1/10) # Increment progress 
        } 
      }) 
      # Assigning status element to output ui element
      output$status <- renderText("Task Completed!")
      
      # Setting reactive values to true
      data_created_ONT(TRUE)
      
    # Server logic code to run if a PHU instead of Ontario is selected    
    } else {
      
      # Subset the rows of cases data for 2020-2022, excluding the first two columns (dates, Ontario)
      temp_cases_PHU <- cases[1:157, c(-1,-2)]
      # Convert the cases data to numeric
      temp_cases_PHU <- apply(temp_cases_PHU, 2, as.numeric)
      
      # Subset the population data, excluding the first two columns (dates, Ontario)
      temp_population_PHU <- population[, c(-1,-2)]
      # Convert the population data to numeric
      temp_population_PHU <- apply(temp_population_PHU, 2, as.numeric)
      
      # Create STS object for PHU cases 
      phuSTS <- sts(observed = temp_cases_PHU, 
                    start = c(2020, 1), 
                    frequency = 52, 
                    population = temp_population_PHU, 
                    neighbourhood = phu_nbOrder, 
                    map = phu_spatial)
      
      # Define the model for PHUs
      model_PHU = list(
        
        # formula for log(nu_it), including seasonality and unit specific coefficients
        end = list(f = addSeason2formula(~ -1 + fe(1, unitSpecific = TRUE) + t, 
                                         period = phuSTS@freq), 
                   offset = population(phuSTS)), # multiplicative offset e_it
        
        # formula for log(lambda_it)
        ar = list(f = ~ -1 + fe(1, unitSpecific = TRUE)), 
        
        # formula for log(phi_it)
        ne = list(f = ~ -1 + fe(log(pop), unitSpecific = TRUE), 
                  weights = W_powerlaw(maxlag = 10)), # weights power law
        
        family = "NegBin1", # distribution model
        
        # Optimizer settings
        optimizer = list(stop = list(tol = 1e-5, niter = 100), # stop rules
                         regression = list(method = "nlminb"), # for penLogLik
                         variance = list(method = "nlminb")), # for marLogLik
        
        verbose = FALSE, # level of progress reporting
        
        data = list(pop = population(phuSTS)), # named list of covariates
        
        keep.terms = FALSE # whether to keep the model terms
        
      )
      
      # Fit the hhh4 model using the defined control settings
      hhh4_model_PHU <-  hhh4(stsObj = phuSTS, control = model_PHU)
      
      # Get the last observed value of the PHU cases
      y.start_PHU <- tail(observed(phuSTS), n = 1)
      
      # Simulate future cases using the fitted model, run 1000 iterations
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
      
      # Create a data frame for observed data
      observed_data_PHU <- data.frame(`Week start date` = cases[, 1], 
                                      `Predicted` = cases[, input$i],
                                      `Type` = "Observed")
      
      # Set column names for the observed data frame
      colnames(observed_data_PHU) <- c("Week start date", input$i, "Type")
      
      # Convert the week start date column to Date format for observed data
      observed_data_PHU$`Week start date` <- as.Date(observed_data_PHU$`Week start date`, 
                                                     format = "%Y-%m-%d")
      
      # Combine the observed and future data into one data frame
      combined_counts_PHU <- rbind(observed_data_PHU, future_data_PHU)
      
      # Calculate quantiles across iterations
      quantiles_PHU <- round(apply(future_sim_PHU, c(1, 2), quantile, probs = c(0.25, 0.75)), 0)

      # Prepare data for plotting (replace with your actual data structure)
      pred_df_PHU <- data.frame(
        `Week start date` = future_dates_PHU,
        `temp` = future_pred_PHU,
        `25%` = quantiles_PHU[1, 1:input$weeks, input$i], # 25th percentile
        `75%` = quantiles_PHU[2, 1:input$weeks, input$i]  # 75th percentile
      )
      
      # Set column names for the prediction data frame
      colnames(pred_df_PHU) <- c("Week start date", input$i, "25%", "75%")
      
      # Render the updated plot for PHU using Plotly
      output$updatedPlot_PHU <- renderPlotly({
        
        # Create the ggplot object
        updatedPlot_PHU <- ggplot() +
        # Historical cases
        geom_line(data = raw_cases[which(raw_cases$`Week start date` == as.Date(input$dateRange[1])):(nrow(cases)), -2], 
                  aes(x = `Week start date`, y = !!sym(input$i), 
                      color = "Historical Cases", linetype = "Historical Cases"), linewidth = 1) +
        geom_point(data = raw_cases[which(raw_cases$`Week start date` == as.Date(input$dateRange[1])):(nrow(cases)),], 
                   aes(x = `Week start date`, y = !!sym(input$i), 
                       color = "Historical Cases"), size = 1, show.legend = FALSE) + 
        # Observed future cases (dashed)
        geom_line(data = raw_cases[(nrow(cases) + 1):(nrow(cases) + nrow(future_data_PHU)), -2], 
                  aes(x = `Week start date`, y = !!sym(input$i), 
                      color = "Observed Future Cases", linetype = "Observed Future Cases"), linewidth = 1) +
        geom_point(data = raw_cases[(nrow(cases)+1):(nrow(cases)+nrow(future_data_PHU)),], 
                   aes(x = `Week start date`, y = !!sym(input$i), 
                       color = "Observed Future Cases"), size = 1, show.legend = FALSE) + 
        # Predicted future cases
        geom_line(data = pred_df_PHU, 
                  aes(x = `Week start date`, y = pred_df_PHU[, 2], 
                      color = "Predicted Future Cases", linetype = "Predicted Future Cases"), linewidth = 1) +
        geom_point(data = pred_df_PHU, 
                   aes(x = `Week start date`, y = pred_df_PHU[, 2], 
                       color = "Predicted Future Cases"), size = 1, show.legend = FALSE) + 
        # 25% quantile prediction
        geom_line(data = pred_df_PHU, 
                  aes(x = `Week start date`, y = `25%`, 
                      color = "25% Quantile", linetype = "25% Quantile"), linewidth = 1) +
        geom_point(data = pred_df_PHU, 
                   aes(x = `Week start date`, y = `25%`, 
                       color = "25% Quantile"), size = 1, show.legend = FALSE) + 
        # 75% quantile prediction
        geom_line(data = pred_df_PHU, 
                  aes(x = `Week start date`, y = `75%`, 
                      color = "75% Quantile", linetype = "75% Quantile"), linewidth = 1) +
        geom_point(data = pred_df_PHU, 
                   aes(x = `Week start date`, y = `75%`, 
                       color = "75% Quantile"), size = 1, show.legend = FALSE) + 
        # Shading area between Predicted Future Cases and 25% quantile 
        geom_ribbon(data = pred_df_PHU, aes(x = `Week start date`, ymin = `25%`, ymax = pred_df_PHU[[2]]), 
                    fill = "lightblue", alpha = 0.5) + 
        # Shading area between Predicted Future Cases and 75% quantile 
        geom_ribbon(data = pred_df_PHU, aes(x = `Week start date`, ymin = pred_df_PHU[[2]], ymax = `75%`), 
                    fill = "lightcoral", alpha = 0.5) +  
        # Customize the theme and legend
        scale_color_manual(values = c("Historical Cases" = "black",
                                      "Observed Future Cases" = "green",
                                      "Predicted Future Cases" = "purple",
                                      "25% Quantile" = "blue",
                                      "75% Quantile" = "red")) +
        scale_linetype_manual(values = c("Historical Cases" = "solid",
                                         "Observed Future Cases" = "solid",
                                         "Predicted Future Cases" = "solid",
                                         "25% Quantile" = "dashed",
                                         "75% Quantile" = "dashed")) +
        # Setting plot labels
        labs(title = paste("COVID-19 Cases in", input$i), 
             color = "Legend", 
             linetype = "Legend") +
        # Setting plot theme
        theme_minimal()
      
        
      # Make the ggplot interactive
      ggplotly(updatedPlot_PHU)
        
      })

      # Render the updated data table for PHU using DataTables
      output$updatedTable_PHU <- renderDT({
        
        # Create a combined list of all dates
        all_dates <- sort(unique(c(as.Date(cases[, "Week start date"]), future_dates_PHU, pred_df_PHU$`Week start date`)))
        
        # Create a new data frame of dates
        data <- data.frame(`Week start date` = all_dates[which(raw_cases$`Week start date` == as.Date(input$dateRange[1])):
                                                           (nrow(cases))])
        
        # Merging each data set
        data <- merge(data, data.frame(
          `Week start date` = cases[which(raw_cases$`Week start date` == as.Date(input$dateRange[1])):(nrow(cases)),1], 
          `Historical Cases` = cases[which(raw_cases$`Week start date` == as.Date(input$dateRange[1])):(nrow(cases)):
                                       nrow(cases), input$i]), by = "Week.start.date", all = TRUE)
        
        data <- merge(data, 
                      data.frame(`Week start date` = future_dates_PHU, 
                                 `Observed Future Counts` = raw_cases[(nrow(cases)+1):
                                                                        (nrow(cases)+nrow(future_data_PHU)), input$i]), 
                      by = "Week.start.date", all = TRUE)
        
        data <- merge(data, 
                      data.frame(`Week start date` = future_dates_PHU, 
                                 `Median Predicted Future Counts` = pred_df_PHU[, 2]), 
                      by = "Week.start.date", all = TRUE) 
        
        data <- merge(data, 
                      data.frame(`Week start date` = future_dates_PHU, 
                                 `25% Quantile` = pred_df_PHU$`25%`), 
                      by = "Week.start.date", all = TRUE)
        
        data <- merge(data, 
                      data.frame(`Week start date` = future_dates_PHU, 
                                 `75% Quantile` = pred_df_PHU$`75%`), 
                      by = "Week.start.date", all = TRUE) 
        
        # Set column names for the combined data frame
        colnames(data) <- c("Week start date", "Historical Cases", "Observed Future Counts", "Median Predicted Future Counts", "25% Quantile", "75% Quantile")
        
        # Render the data table 
        datatable(data)
        
      })
      
      # Presenting progress bar for this code chunk
      withProgress(message = 'Processing...', value = 0, { 
        for (i in 1:10) {
          Sys.sleep(0.5) # Simulate a task by sleeping
          incProgress(1/10) # Increment progress 
        } 
      })
      # Assigning status element to output ui
      output$status <- renderText("Task Completed!")
      
      # Setting reactive values to true
      data_created_PHU(TRUE)
      
    }
    
  })
  
  # Server code for event of Submit Data
  observeEvent(input$submitData, {
    
    # Load the existing COVID-19 case data from a CSV file into a data frame
    cases <- read.csv(file = "cases.csv", header = TRUE)
    
    # Create a new data frame for the latest case data input by the user
    new_case_data <- data.frame("Week start date" = as.Date(input$submissionDate, format = "%Y-%m-%d"), 
                                "Ontario" = input$`Ontario_input`,
                                "Algoma Public Health Unit"= input$`Algoma_input`,
                                "Brant County Health Unit" = input$`BrantCounty_input`,
                                "Chatham-Kent Health Unit" = input$`Chatham-Kent_input`,
                                "Durham Region Health Department" = input$`DurhamRegion_input`,
                                "Eastern Ontario Health Unit" = input$`EasternOntario_input`, 
                                "Grey Bruce Health Unit" = input$`GreyBruce_input`, 
                                "Haldimand-Norfolk Health Unit" = input$`Haldimand-Norfolk_input`, 
                                "Haliburton, Kawartha, Pine Ridge District Health Unit" = 
                                  input$`HaliburtonKawarthaPineRidgeDistrict_input`, 
                                "Halton Region Health Department" = input$`HaltonRegion_input`, 
                                "Hamilton Public Health Services" = input$`Hamilton_input`, 
                                "Hastings and Prince Edward Counties Health Unit" =
                                  input$`HastingsAndPrinceEdwardCounties_input`, 
                                "Huron Perth Health Unit" = input$`HuronPerth_input`,
                                "Kingston, Frontenac and Lennox and Addington Health Unit" = 
                                  input$`KingstonFrontenacAndLennoxAndAddington_input`, 
                                "Lambton Public Health" = input$`Lambton_input`, 
                                "Leeds, Grenville and Lanark District Health Unit" = 
                                  input$`LeedsGrenvilleAndLanarkDistrict_input`, 
                                "Middlesex-London Health Unit" = input$`Middlesex-London_input`, 
                                "Niagara Region Public Health Department" = 
                                  input$`NiagaraRegion_input`, 
                                "North Bay Parry Sound District Health Unit" = 
                                  input$`NorthBayParrySoundDistrict_input`, 
                                "Northwestern Health Unit" = input$`Northwestern_input`, 
                                "Ottawa Public Health" = input$`Ottawa_input`, 
                                "Peel Public Health" = input$`Peel_input`, 
                                "Peterborough Public Health" = input$`Peterborough_input`, 
                                "Porcupine Health Unit" = input$`Porcupine_input`, 
                                "Region of Waterloo, Public Health" = input$`RegionOfWaterloo_input`,
                                "Renfrew County and District Health Unit" = 
                                  input$`RenfrewCountyAndDistrict_input`, 
                                "Simcoe Muskoka District Health Unit" = 
                                  input$`SimcoeMuskokaDistrict_input`, 
                                "Southwestern Public Health" = input$`Southwestern_input`, 
                                "Sudbury and District Health Unit" = input$`SudburyAndDistrict_input`,
                                "Thunder Bay District Health Unit" = input$`ThunderBayDistrict_input`, 
                                "Timiskaming Health Unit" = input$`Timiskaming_input`, 
                                "Toronto Public Health" = input$`Toronto_input`, 
                                "Wellington-Dufferin-Guelph Health Unit" = 
                                  input$`Wellington-Dufferin-Guelph_input`, 
                                "Windsor-Essex County Health Unit" = 
                                  input$`Windsor-EssexCounty_input`, 
                                "York Region Public Health" = input$`YorkRegion_input`
                                )
    
    # Append the new case data to the existing data frame
    cases <- rbind(cases, new_case_data)
    
    # Export new 'cases' as a .csv file for import again into Shiny app
    write.csv(x = cases, file = "cases.csv", row.names = FALSE)
    
    # Close the Shiny app 
    stopApp()
    
  })
  
  # Server code for event of Clear inputs
  observeEvent(input$clearInputs, {
    
    # Reset all specified input ids
    reset(id = "i")
    reset(id = "prev_cases")
    reset(id = "weeks")
    reset(id = "submissionDate")
    reset(id = "Ontario_input")
    reset(id = "Algoma_input")
    reset(id = "BrantCounty_input")
    reset(id = "Chatham-Kent_input")
    reset(id = "DurhamRegion_input")
    reset(id = "EasternOntario_input")
    reset(id = "GreyBruce_input")
    reset(id = "Haldimand-Norfolk_input")
    reset(id = "HaliburtonKawarthaPineRidgeDistrict_input")
    reset(id = "HaltonRegion_input")
    reset(id = "Hamilton_input")
    reset(id = "HastingsAndPrinceEdwardCounties_input")
    reset(id = "HuronPerth_input")
    reset(id = "KingstonFrontenacAndLennoxAndAddington_input")
    reset(id = "Lambton_input")
    reset(id = "LeedsGrenvilleAndLanarkDistrict_input")
    reset(id = "Middlesex-London_input")
    reset(id = "NiagaraRegion_input")
    reset(id = "NorthBayParrySoundDistrict_input")
    reset(id = "Northwestern_input")
    reset(id = "Ottawa_input")
    reset(id = "Peel_input")
    reset(id = "Peterborough_input")
    reset(id = "Porcupine_input")
    reset(id = "RegionOfWaterloo_input")
    reset(id = "RenfrewCountyAndDistrict_input")
    reset(id = "SimcoeMuskokaDistrict_input")
    reset(id = "Southwestern_input")
    reset(id = "SudburyAndDistrict_input")
    reset(id = "ThunderBayDistrict_input")
    reset(id = "Timiskaming_input")
    reset(id = "Toronto_input")
    reset(id = "Wellington-Dufferin-Guelph_input")
    reset(id = "Windsor-EssexCounty_input")
    reset(id = "YorkRegion_input")
    
    # Explicitly reset sliderInput to the correct min, max, and value
    updateSliderInput(session, "dateRange", 
                      min = min(as.Date(cases[, "Week start date"])),
                      max = max(as.Date(cases[, "Week start date"])),
                      value = c(min(as.Date(cases[, "Week start date"])), 
                                max(as.Date(cases[, "Week start date"]))))
    
  })
  
  # Server code for event of Clear Outputs
  observeEvent(input$clearOutputs, {
    data_created_ONT(FALSE)
    data_created_PHU(FALSE)
  })
  
}

#******************************************************************************************

### Run the application 
shinyApp(ui = ui, server = server)

#******************************************************************************************