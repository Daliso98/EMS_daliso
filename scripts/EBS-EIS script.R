
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

ems_raw <- import(here("data.raw.xls"))


# Exploratory analysis  -----------------------------------------------------------

names(ems_raw) # View the variable names 

glimpse(ems_raw) # Get a quick overview of the dataset structure and variable types

head(ems_raw) # View the first 6 records

class(ems_raw) # Check the type/class of the dataset

