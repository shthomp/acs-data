#===================================================================#
# Author: Shiraya Thompson, Data Analyst                          
# Date Created: 04/15/25
# Date Last Updated: 04/15/25
# Purpose: To access the US Census Bureau API and compile data points
# for Long Beach across time
#===================================================================#

library(tidycensus)
library(tidyverse)
library(httr)
library(readxl)
library(dplyr)
library(readr)
library(purrr)


# The code below allows R to connect with the API through the City server
# Replace the proxy_address value with your actual proxy address  (it is likely the same)
# Fill in username and password with your City credentials -- **remove your information before sharing this script**

proxy_address <- "http://proxy10.ci.long-beach.ca.us"  
proxy_port <- 8080  
set_config(use_proxy(url = proxy_address, port = proxy_port, username = "", password = ""))

# Obtain a personal API key from the U.S. Census website
# RStudio will remember it in the local memory and you do not have to install/run this line again.

census_api_key("", install = "TRUE")

################################################################## SET THE DATA PARAMETERS #################################################################################

# Change the cYear and SetGeo values to update the year and geography for all variables.

# Note: For 5-year estimates, you should use the preceding non-overlapping 5-year data to compare over time (e.g., 2019-2023 data should
# be compared with 2014-2018 data). When referencing "acs5" as the survey, the year to apply to get_acs() will be the ending year of the
# 5-year period of interest (e.g., year=2023 for 2019-2023 acs5 data ).

# You can input any year up to which ACS has data available
# Your options for SetGeo are: "Long Beach", "Los Angeles County", or "California"

cYear <- 2023
SetGeo <- "Long Beach"

# Setting the range of dates for which we want 1-year data for trending. 
# The code sets the range starting at 5 years prior to the most current data year (cYear)
# **2020 is excluded from the pull since data for this acs1 year is not available through the API
trend_dates <- data.frame(setdiff((cYear-5):cYear, 2020))


# These lines update the code below based on which geography you assign to the SetGeo variable. *Do not change this code*
# You must run this code every time you update the SetGeo variable

geoVal <- ifelse(SetGeo == "Long Beach", "place", 
                 ifelse(SetGeo == "Los Angeles County", "county",
                        ifelse(SetGeo == "California", "state", "")))

FipsCode <- ifelse(SetGeo == "Long Beach", "0643000", 
                   ifelse(SetGeo == "Los Angeles County", "06037",
                          ifelse(SetGeo == "California", "06", "")))

#=========================================================================================================================================================================#


################################################################## VIEW/BROWSE THE DATASETS #################################################################################

# We will use the function load_variables() to view all of the possible variables we can use from the dataset
# of interest. We need to save the result as an object so that we can view it, much like you would save a file.

# Note that we are pulling the ACS 5-Year Estimates for the end year set (cYear) in the'SET PARAMETERS' section, 
# Which means we are pulling the [cYear minus 5] to cYear (e.g., 2018-2023) ACS 5-Year Estimates.
# By default, this pulls all the variables for detailed tables. If we want to pull all variables for a 
# product such as data profiles, we would change the dataset to "acs5/profile."

# View each of these results as tables through the R Studio Environment to find the variable codes you need 
# based on the data you are looking to pull.

#-----------------------------------------------------------------------------------------------------------#
## Detailed Table dataset variable names start with 'B' -- this is the most detailed data, which we will need to 
## use for more granular, disaggregated data. All estimates, will need to be converted to percentages.

acs_vars <- load_variables(year = cYear,
                            dataset = "acs1", 
                            cache = TRUE)


## Data Profile dataset variable names start with 'D' -- percentages have the suffix 'PE.'

acsdp_vars <- load_variables(year = cYear,
                              dataset = "acs1/profile", 
                              cache = TRUE)

## Subject Table variable names start with 'S' -- percentage variables do not have a suffix, but you can find them by typing
## 'percent' into the searchbar as you're viewing the variable table in RStudio.

acsst_vars <- load_variables(year = cYear,
                              dataset = "acs1/subject", 
                              cache = TRUE)

## Comparison Profile dataset variable start with 'C' -- the data consists of estimates for both the 5-year estimate period of interest
## and the preceding 5-year period. Can also output statistical significance result (refer to api.census.gov/data/[year]/acs/acs5/cprofile/variables.html
## for list of codes for statistical significance test results).

acscp_vars <- load_variables(year = cYear,
                              dataset = "acs1/cprofile", 
                              cache = TRUE)

## The line below compiles the list of variable names from all of the ACS 5-Year Data Tables above into one table.
## We will use this master table to match descriptive labels to the data frame of selected indicators we produce later in this script.

labels_master <- rbind(acs_vars, acsdp_vars, acsst_vars, acscp_vars)

#=========================================================================================================================================================================#

#### Function to pull multiple years of census data from a geography with TidyCensus #####

#' Pull ACS Census data for multiple years across the same geographies and combine into a single dataframe.

get_acs_years <- function(variables = NULL,
                          years = NULL,
                          geography = NULL,
                          state = NULL,
                          survey = NULL) {
  acs_df <- list()
  
  for(i in 1:nrow(years)) {
    year_pull <- years[i,1]
    
    acs_pull <- tidycensus::get_acs(geography = geography,
                                    state = state,
                                    variables = variables,
                                    year = year_pull,
                                    survey = survey,
                                    geometry = FALSE) %>%
      dplyr::mutate(Year = year_pull)
    
    acs_df[[i+1]] <- acs_pull
  }
  
  acs_combine <- data.table::rbindlist(acs_df, fill = TRUE)
  
}

## =========================================================================================================================================================================#
# using the function

demo_trends <- get_acs_years(variables = c("DP05_0033"),
                             years = trend_dates,
                             state = "CA",
                             survey = "acs1",
                             geography = geoVal)%>%subset(GEOID == FipsCode)


a2c_trends <- get_acs_years(variables = c("DP03_0096PE"),
                      years = trend_dates,
                      state = "CA",
                      survey = "acs1",
                      geography = geoVal)%>%subset(GEOID == FipsCode)



econ_trends <- get_acs_years(variables = c("B19083_001E","DP03_0009PE", "DP03_0119P", "DP04_0046P", "DP04_0142P"),
                            years = trend_dates,
                            state = "CA",
                            survey = "acs1",
                            geography = geoVal)%>%subset(GEOID == FipsCode)

# combining data for all categories

all_trends <- rbind(a2c_trends, econ_trends)

## matches each of the estimates in the output with their assigned labels from the master table we created earlier in this script
labeled_acs_trends <- left_join(all_trends, labels_master, by = c("variable" = "name"))


############################################################################# EXPORT TO EXCEL #############################################################################

# Update with where you want to send the output on your computer
filepath <- ("C:/Users/shthomp/Documents/Community Health Assessment_Improvement Plan/")

# This will include the year you specified for the data in the file name
fileTitle <- paste("ACS_Trends_",toString(cYear-5),"-",toString(cYear))

# This will include the geography you specified for the data in the file name
fileSuffix <- ifelse(SetGeo == "Long Beach", "_Long Beach City", 
                     ifelse(SetGeo == "Los Angeles County", "_Los Angeles County",
                            ifelse(SetGeo == "California", "_California", "")))


# This combines all the file naming information together
filename <- paste0(filepath, fileTitle, fileSuffix,".xlsx")


# This outputs the data into an Excel file
writexl::write_xlsx(labeled_acs_trends, filename)
