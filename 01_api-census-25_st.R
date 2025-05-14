#===================================================================#
# Author: Shiraya Thompson, Data Analyst                          
# Date Created: 01/28/25
# Date Last Updated: 03/06/25
# Purpose: To access the US Census Bureau API and gather data points 
# of interest to the Long Beach Community Health Assessment
#===================================================================#


# This script uses the TidyCensus R package to explore American Community Survey data from the Census Bureau
# Application Programming Interface (API)

# The Census Bureau's Intro to API Course can be found at https://www.census.gov/data/academy/courses.html.

# More information about the Census Bureau's API, including where to request an API key, can be found
# at https://www.census.gov/developers/.

# Objective: Streamline + Automate ACS Data Collection for Community Health Assessment

# First, use the function install.packages() to install TidyCensus and Tidyverse if you have not already. You will only have to do this once.
# Tidyverse is a system of packages that help facilitate statistics and data analysis in R and is useful

install.packages("tidycensus")
install.packages("tidyverse")

# Use library() to load the packages to your session.
# There are also additional packages to load for using other functions
# that help streamline data processing.

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

acs5_vars <- load_variables(year = cYear,
                          dataset = "acs5", 
                          cache = TRUE) %>% subset(select = -geography)


## Data Profile dataset variable names start with 'D' -- percentages have the suffix 'PE.'

acs5dp_vars <- load_variables(year = cYear,
                            dataset = "acs5/profile", 
                            cache = TRUE)

## Subject Table variable names start with 'S' -- percentage variables do not have a suffix, but you can find them by typing
## 'percent' into the searchbar as you're viewing the variable table in RStudio.

acs5st_vars <- load_variables(year = cYear,
                            dataset = "acs5/subject", 
                            cache = TRUE)

## Comparison Profile dataset variable start with 'C' -- the data consists of estimates for both the 5-year estimate period of interest
## and the preceding 5-year period. Can also output statistical significance result (refer to api.census.gov/data/[year]/acs/acs5/cprofile/variables.html
## for list of codes for statistical significance test results).

acs5cp_vars <- load_variables(year = cYear,
                            dataset = "acs5/cprofile", 
                            cache = TRUE)

## The line below compiles the list of variable names from all of the ACS 5-Year Data Tables above into one table.
## We will use this master table to match descriptive labels to the data frame of selected indicators we produce later in this script.

labels_master <- rbind(acs5_vars, acs5dp_vars, acs5st_vars, acs5cp_vars)

#=========================================================================================================================================================================#
## 5-YEAR ESTIMATE DATA ##

# UPDATE THE LIST OF ESTIMATES FOR OUTPUT HERE --------------------------------------

# Demographics --------------------------------------------------------------------------------------------------------------------------------------------------------------


## Type (or add to) the list of demographic variables we want estimates for below as string objects
## Check the previous cycle's output to check which variables are already included, or run the code now to produce the output

demovar_list <- c("DP05_0033", "DP05_0035", "DP05_0037", "DP05_0038", "DP05_0039", "DP05_0047", "DP05_0048","B02015_011",
              "DP05_0049", "DP05_0050", "DP05_0051", "DP05_0052", "DP05_0053", "DP05_0054", "DP05_0055", 
              "DP05_0060", "DP05_0075", "DP05_0076", "DP05_0077", "DP05_0078", "DP05_0079", "DP05_0080", 
              "DP05_0081", "DP05_0082", "DP05_0083", "DP05_0084", "DP05_0085", "DP05_0086", "DP05_0087", 
              "DP05_0088",
              "B01002A_001", "B01002B_001", "B01002C_001", "B01002D_001", "B01002E_001", 
              "B01002F_001", "B01002I_001", "C16002_001", "C16002_002", "C16002_012", "C16002_013",
              "C16002_014", "C16002_003","C16002_004", "C16002_005","C16002_006", "C16002_007",
              "C16002_008", "C16002_009",
              "C16002_010", "C16002_011", "C16002_012","B16001_090","B16001_093", "B16001_099",
              "B16001_102", "B16001_087", "S1810_C01_001", "S1810_C02_019", "S1810_C02_029", 
              "S1810_C02_039","S1810_C02_047", "S1810_C02_055", "S1810_C02_063", "S1810_C02_013", 
              "S1810_C02_014","S1810_C02_015", "S1810_C02_016", "S1810_C02_017", "S1810_C02_018",
              "B06001_002","B06001_003","B06001_004","B06001_005","B06001_006","B06001_007","B06001_008",
              "B06001_009","B06001_010","B06001_011","B06001_012")


# Access to Health Care -----------------------------------------------------------------------------------------------------------------------------------------------------

athcvar_list <- c("DP03_0096PE","DP03_0099PE","S2701_C03_013E","S2701_C03_012E","S2701_C03_011E",
                  "S2701_C03_017", "S2701_C03_018", "S2701_C03_019", "S2701_C03_020", "S2701_C03_021", "S2701_C03_022",
                  "S2701_C03_023","S2701_C03_024", "S2701_C03_031", "S2701_C03_032", "S2701_C03_033", "S2701_C03_034",
                  "S2701_C03_035", "S2701_C03_036", "DP03_0097P", 	"DP03_0098P")
  

# Economic Insecurity ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

econvar_list <- c("B19083_001E","DP03_0062E","S1903_C03_002E","S1903_C03_003E","S1903_C03_004E","S1903_C03_005E",
                  "S1903_C03_006E","S1903_C03_007E","S1903_C03_008E","S1903_C03_009E","S1903_C03_014E", "DP03_0119PE",
                  "DP03_0133PE","DP03_0129PE","DP03_0131PE","DP03_0128PE","DP03_0088E","DP03_0070PE","DP03_0072PE",
                  "DP03_0074PE","DP03_0009PE","S0103_C01_085E","S0103_C01_086E","S0103_C01_087E","S0103_C02_081E",
                  "S2101_C04_037E","S2101_C04_036E","S2101_C04_034E","B18140_002E","B18140_005E","S1811_C02_054E", 
                  "DP04_0079P")


# Exercise/Nutrition ----------------------------------------------------------------------------------------------------------------------------------------------------------

exervar_list <- c("DP03_0022PE","S0801_C01_011E")


# Social Support --------------------------------------------------------------------------------------------------------------------------------------------------------------

socialvar_list <- c("S0103_C02_026E")


# Housing & Homelessness ------------------------------------------------------------------------------------------------------------------------------------------------------

homevar_list <- c("DP04_0004E","DP04_0046PE","DP04_0134E","DP04_0089E","DP04_0101E","DP04_0109E","DP04_0115PE",
               "S0103_C02_097E","DP04_0142PE")


# Combining all variables ====================================================================================================================================================

var_list <- c(demovar_list,athcvar_list,econvar_list,exervar_list,socialvar_list,homevar_list)



# Using TidyCensus to pull indicators for all variables in list
# We use the function get_acs() to access the data.
# The subset() function filters the output to only the geography specified in the parameters section above.
# Then we will save the results with descriptive labels from ACS to easily identify what each estimate is measuring.

acs_final <- get_acs(
  geography = geoVal,
  variables = var_list,
  state = "CA",
  survey = "acs5",
  year = cYear
) %>%
  subset(GEOID == FipsCode)


# Matches each of the estimates in the output with their assigned labels from the master table we created earlier in this script
labeled_acs5_est <- left_join(acs_final, labels_master, by = c("variable" = "name"))



############################################################################# EXPORT TO EXCEL #############################################################################

# Update with where you want to send the output on your computer
filepath <- ("C:/Users/shthomp/Documents/Community Health Assessment_Improvement Plan/")

# This will include the year you specified for the data in the file name
fileTitle <- paste("ACS_5Y_Estimates_", toString(cYear))

# This will include the geography you specified for the data in the file name
fileSuffix <- ifelse(SetGeo == "Long Beach", "_Long Beach City", 
                     ifelse(SetGeo == "Los Angeles County", "_Los Angeles County",
                            ifelse(SetGeo == "California", "_California", "")))


# This combines all the file naming information together
filename <- paste0(filepath, fileTitle, fileSuffix,".xlsx")


# This outputs the data into an Excel file
writexl::write_xlsx(labeled_acs5_est, filename)


##############################################################################################################################################################################-
