
# About project  ----------------------------------------------------------

# Purpose:Automating Routine Review of EBS Records
# Author: Daliso Ngulube 
# Last Updated: 9 September 2026
# Contact email: dariosteckly@gmail.com   


# Lading packages  ----------------------------------------------------

# The pacman package will install each package if necessary,
# and load it for use in the current session


pacman::p_load(
  rio,        # Import data
  here,       # Manage file paths
  janitor,    # Clean names and create frequency tables
  skimr,      # Explore dataset structure
  naniar,     # Explore missing data
  lubridate,  # Work with dates
  stringr,    # Work with text
  tidyr,      # Organize and reshape data
  tidyverse   # Data manipulation and visualisation — load last
)



# Importing data  ---------------------------------------------------------
# import the raw ems line list 

ems_raw <- import(here( "data", "data.raw.xls"))


# Exploratory analysis  -----------------------------------------------------------

names(ems_raw) # View the variable names 

glimpse(ems_raw) # Examine the structure and data types of all variables

skim(ems_raw) # Get an overall summary of the raw EMS dataset

head(ems_raw) # View the first 6 records

# Check for duplicate EMS IDs among records with an ID
ems_raw %>%
  filter(!is.na(`EMS ID`)) %>%
  count(`EMS ID`) %>%
  filter(n > 1)



# Cleaning the data set  --------------------------------------------------

clean <- ems_raw %>% 
  clean_names() # Create a working copy with standardized variable names

