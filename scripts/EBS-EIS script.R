
# About project  ----------------------------------------------------------

# Purpose:Automating Routine Review of EBS Records
# Author: Daliso Ngulube 
# Last Updated: 9 September 2026
# Contact email: dariosteckly@gmail.com   


# Summary of the project  -------------------------------------------------

#   This script implements a reproducible R-based operational review workflow
#   for EBS records held in the DHIS2 Event Management System (EMS).
#   It screens records against seven agreed Phase 1 review rules, produces
#   flagged record lists, and generates summary outputs for district and
#   provincial surveillance teams.


# Proposed indicators for phase 1 -----------------------------------------

#     Ind 1  —  Agent/Syndrome field: three-way classification
#     Ind 2  —  Analytics-visible disease profile (top 10)
#     Ind 3  —  No source recorded (signals and events separately)
#     Ind 4  —  No verification status recorded (active records)
#     Ind 5  —  Verified true event with no Risk Assessment
#     Ind 6  —  Pending verification backlog stratified by time pending 
#     Ind 7  —  Workflow status breakdown (cross-tabulation)


# Lading packages  ----------------------------------------------------


# The pacman package will install each package if necessary,
# and load it for use in the current session


pacman::p_load(
  rio,        # Import data
  here,       # Manage file paths
  sf, 
  janitor,    # Clean names and create frequency tables
  skimr,      # Explore dataset structure
  naniar,     # Explore missing data
  lubridate,  # Work with dates
  stringr,    # Work with text
  gtsummary,    # creating tables  
  scales,       # percents in tables  
  flextable,    # for making pretty tables
  ggExtra,      # adding marginal plots to ggplot graphs
  tidyr,      # Organize and reshape data
  tidyverse   # Data manipulation and visualisation — load last
)


# Importing data  ---------------------------------------------------------
# import the raw ems line list 

ems_raw <- import(here( "data", "data.raw.xls"))


zambia_districts <- st_read(
  here(
    "shapefiles",
    "BNDA_ZMB_2017-01-01_lastupdate.shp"
  )
)


# Exploratory data analysis  --------------------------------------------------

# this section helps us to understand the structure of 
#the data set before we start the cleaning process

names(ems_raw) # View the variable names 


glimpse(ems_raw) # Examine the structure and data types of all variables


skim(ems_raw) # Get an overall summary of the raw EMS dataset


head(ems_raw) # View the first 6 records


tail(ems_raw, 6)   # Last 6 rows — check no footer rows were imported


# Check for duplicate EMS IDs among records with an ID
ems_raw %>%
  filter(!is.na(`EMS ID`)) %>%
  count(`EMS ID`) %>%
  filter(n > 1)


#Missing data 
# {naniar} gives a ranked table of missingness by column — essential for
# understanding which fields have the completeness problems identified in

miss_var_summary(ems_raw) %>%
  arrange(desc(pct_miss)) %>%
  print(n = 31)   # Show all 31 columns


# Check the variables available in the shapefile
names(zambia_districts)


# Cleaning the data set  --------------------------------------------------

ems <- ems_raw %>% 
  
  clean_names() %>%  
  # Standardize variable names to lowercase with underscores
  
  select(
    # Keep only variables required for the EMS analysis
    ems_id,
    type_of_entry,
    signal_event_verification_status,
    program_status,
    organisation_unit_name_hierarchy,
    signal_event_registration_date,
    verification_date,
    event_start_date,
    event_intervention_by_date,
    public_communication_start_date,
    agent_syndrome,
    agent_syndrome_label,
    signal_source,
    event_source,
    hra_auto_risk_assessment
  ) %>% 
  
  mutate(
    across(
      where(is.character),
      ~ na_if(str_trim(.), "")
    )
  ) %>% 
  # Clean text fields by removing extra spaces and converting blanks to NA
  
  mutate(
    across(
      c(
        signal_event_registration_date,
        verification_date,
        event_start_date,
        event_intervention_by_date,
        public_communication_start_date
      ),
      ~ as_date(
        parse_date_time(
          as.character(.x),
          orders = c(
            "Ymd HMS",             # Handle dates with date and time
            "Ymd"                  # Handle dates without time
          ),
          quiet = TRUE             # Suppress warnings for unrecognized dates
        )
      )
    )
  ) %>% 
  # Parse all selected date columns to Date class
  
  mutate(
    # Split the organisation-unit hierarchy into separate parts
    ou_parts = str_split(
      organisation_unit_name_hierarchy,
      " / "
    ),
    
    # Extract province from the organisation-unit hierarchy
    province = map_chr(
      ou_parts,
      ~ if_else(
        length(.x) >= 2,
        str_trim(.x[2]),
        NA_character_
      )
    ),
    
    # Extract district from the organisation-unit hierarchy
    district = map_chr(
      ou_parts,
      ~ if_else(
        length(.x) >= 3,
        str_trim(.x[3]),
        NA_character_
      )
    )
  ) %>% 
  # Create clean province and district variables for geographic analysis
  
  select(
    -ou_parts,
    -organisation_unit_name_hierarchy
  ) %>% 
  # Remove the temporary variable and original hierarchy column
  
  mutate(
    
    # Recode entry-type system codes into readable categories
    type_of_entry = factor(
      recode(
        type_of_entry,
        "ENTRY_TYPE_EVENT" = "Event",
        "ENTRY_TYPE_PREPAREDNESS" = "Preparedness",
        "ENTRY_TYPE_SIGNAL" = "Signal"
      ),
      levels = c(
        "Signal",
        "Event",
        "Preparedness"
      )
    ),
    
    # Recode verification status into readable categories
    signal_event_verification_status = factor(
      recode(
        signal_event_verification_status,
        "VERIFICATION_STATUS_VERIFIED" = "Verified true event",
        "VERIFICATION_STATUS_PENDING" = "Pending",
        "VERIFICATION_STATUS_NOT_EVENT" = "Not an event"
      ),
      levels = c(
        "Verified true event",
        "Pending",
        "Not an event"
      )
    ),
    
    # Convert program status to readable ordered categories
    program_status = factor(
      program_status,
      levels = c(
        "ACTIVE",
        "COMPLETED",
        "CANCELLED"
      ),
      labels = c(
        "Active",
        "Completed",
        "Cancelled"
      )
    ),
    
    # Convert automated risk scores to readable ordered categories
    hra_auto_risk_assessment = factor(
      hra_auto_risk_assessment,
      levels = c(
        "RISK_SCORE_VERY_LOW",
        "RISK_SCORE_LOW",
        "RISK_SCORE_MODERATE",
        "RISK_SCORE_HIGH",
        "RISK_SCORE_VERY_HIGH"
      ),
      labels = c(
        "Very low",
        "Low",
        "Moderate",
        "High",
        "Very high"
      )
    ),
    
    # Recode agent/syndrome system codes into readable public health terms
    agent_syndrome = recode(
      agent_syndrome,
      
      # Syndromes
      "ACUTE_DIARRHOEAL_SYNDROME"       = "Acute Diarrhoeal Syndrome",
      "ACUTE_ENCEPHALITIS_SYNDROME"     = "Acute Encephalitis Syndrome",
      "ACUTE_FEBRILE_SYNDROME"          = "Acute Febrile Syndrome",
      "ACUTE_FEVER_AND_RASH_SYNDROME"   = "Acute Fever and Rash Syndrome",
      "ACUTE_GASTROINTESTINAL_SYNDROME" = "Acute Gastrointestinal Syndrome",
      "ACUTE_HAEMORRHAGIC_SYNDROME"     = "Acute Haemorrhagic Syndrome",
      "ACUTE_NEUROLOGIC_SYNDROME"       = "Acute Neurologic Syndrome",
      "SARI"                             = "Severe Acute Respiratory Infection",
      
      # Infectious diseases and agents
      "ADENOVIRUS"                      = "Adenovirus",
      "ANTHRACIS"                       = "Anthrax",
      "AVIANFLU"                        = "Avian Influenza",
      "BORDETELLA_PERTUSSIS"            = "Pertussis",
      "BRUCELLA_SPP"                    = "Brucellosis",
      "CLOSTRIDIUM_TETANI"              = "Tetanus",
      "COCCIDIAN_PROTOZOA"              = "Coccidian Protozoal Infection",
      "CRYPTOCOCCUS_SPP"                = "Cryptococcosis",
      "CYTOMEGALOVIRUS"                 = "Cytomegalovirus",
      "DENGUE_VIRUS"                    = "Dengue",
      "EBOLA_VIRUS"                     = "Ebola Virus Disease",
      "ENTAMOEBA"                       = "Amoebiasis",
      "FASCIOLA_HEPATICA"               = "Fascioliasis",
      "FILARIASIS"                      = "Filariasis",
      "GIARDIA_SPP"                     = "Giardiasis",
      "HAEMOPHILUS_INFLUENZAE"          = "Haemophilus influenzae infection",
      "HEPA_VIRUS"                      = "Hepatitis A",
      "HEPB_VIRUS"                      = "Hepatitis B",
      "HEPC_VIRUS"                      = "Hepatitis C",
      "INFLUENZA_H1N1"                  = "Influenza A(H1N1)",
      "INFLUENZA_H3N2"                  = "Influenza A(H3N2)",
      "INFLUENZA_H5N6"                  = "Influenza A(H5N6)",
      "INFLUENZA_VIRUS"                 = "Influenza",
      "LASSA_VIRUS"                     = "Lassa Fever",
      "LEPTOSPIROSIS_SPP"               = "Leptospirosis",
      "MERS_COV"                        = "MERS-CoV",
      "METHICILLIN_RESISTANT_STAPH_AUREUS" = "MRSA",
      "MONKEYPOX_VIRUS"                 = "Mpox",
      "MPOX_VIRUS"                      = "Mpox",
      "MYCOBACTERIUM_LEPRAE"            = "Leprosy",
      "MYCOBACTERIUM_TB"                = "Tuberculosis",
      "NEISSERIA_MENINGITIDIS"           = "Meningococcal Disease",
      "ORF_VIRUS"                       = "Orf",
      "PARVOVIRUS_B19"                  = "Parvovirus B19",
      "PHLEBOVIRUS"                     = "Phlebovirus Infection",
      "PLASMODIUM_SPP"                  = "Malaria",
      "RABIES_VIRUS"                    = "Rabies",
      "RESPIRATORY_SYNCYTIAL_VIRUS"     = "Respiratory Syncytial Virus",
      "ROTAVIRUS"                       = "Rotavirus",
      "RUBELLA_VIRUS"                   = "Rubella",
      "SALMONELLA_SPP"                  = "Salmonellosis",
      "SALMONELLA_TYPHI"                = "Typhoid",
      "SARS_COV"                        = "SARS-CoV",
      "SARS_COV_2"                      = "COVID-19",
      "SCHISTOSOMA_SPP"                 = "Schistosomiasis",
      "SHIGELLA_SPP"                    = "Shigellosis / Dysentery",
      "STAPHYLOCOCCUS_SPP"              = "Staphylococcal Infection",
      "STREPTOCOCCUS_SPP"               = "Streptococcal Infection",
      "TAENIA_SOLIUM"                   = "Taeniasis",
      "TOXOPLASMA_GONDII"               = "Toxoplasmosis",
      "TREPONEMA_PALLIDUM"              = "Syphilis",
      "TRYPANOSOMA"                     = "Trypanosomiasis",
      "TUNGIASIS"                       = "Tungiasis",
      "VIBRIO_CHOLERAE"                 = "Cholera",
      "VARICELLA_ZOSTER_VIRUS"          = "Varicella (Chickenpox)",
      "YELLOW_FEVER_VIRUS"              = "Yellow Fever",
      "YERSINIA_PESTIS"                 = "Plague",
      "ZIKA_VIRUS"                      = "Zika Virus",
      
      # Vaccine-related conditions
      "AEFI"                            = "Adverse Event Following Immunisation",
      "AFP"                             = "Acute Flaccid Paralysis",
      
      # Other diseases and conditions
      "JAUNDICE"                        = "Jaundice",
      "MEASLES_VIRUS"                   = "Measles",
      "MENINGITIS_BACTERIAL"            = "Bacterial Meningitis",
      "MUMPS_VIRUS"                     = "Mumps",
      "NATURAL_DISASTER"                = "Natural Disaster",
      "PNEUMONIA"                       = "Pneumonia",
      "POLIO_VIRUS_CVDPV2"              = "Polio (cVDPV2)",
      "POLIO_VIRUS_WILD"                = "Polio (Wild)",
      "UNKNOWN_FOOD_BORNE_DISEASE"      = "Unknown Foodborne Disease",
      "WATERBORNE_ILLNESS"              = "Waterborne Illness",
      
      # Chemical and toxic exposures
      "ACIDS_CAUSTICS"                  = "Acids/Caustics Poisoning",
      "CARBON_MONOXIDE"                 = "Carbon Monoxide Poisoning",
      "CYANIDE"                          = "Cyanide Poisoning",
      "HERBICIDE"                       = "Herbicide Poisoning",
      "LEAD"                             = "Lead Poisoning",
      "MERCURY"                          = "Mercury Poisoning",
      "PESTICIDE"                       = "Pesticide Poisoning",
      "STRYCHNINE"                       = "Strychnine Poisoning",
      
      # Animal-related exposures
      "ANIMAL_BITES"                    = "Animal Bites"
    ),
    
    # Recode signal-source system codes into readable categories
    signal_source = recode(
      signal_source,
      "REPORT_SMS"                    = "SMS report",
      "REPORT_SOURCE_COMMUNITY"       = "Community",
      "REPORT_SOURCE_CONTACT_TRACING" = "Contact tracing",
      "REPORT_SOURCE_DIRECT_CALL"     = "Direct call",
      "REPORT_SOURCE_HFI"             = "Health facility",
      "REPORT_SOURCE_OTHER"           = "Other",
      "REPORT_SOURCE_SOCIAL_MEDIA"    = "Social media",
      "REFERENCE_LAB"                 = "Reference laboratory"
    ),
    
    # Recode event-source system codes into readable categories
    event_source = recode(
      event_source,
      "REPORT_SMS"                    = "SMS report",
      "REPORT_SOURCE_CALL_CENTER"     = "Call centre",
      "REPORT_SOURCE_COMMUNITY"       = "Community",
      "REPORT_SOURCE_CONTACT_TRACING" = "Contact tracing",
      "REPORT_SOURCE_DIRECT_CALL"     = "Direct call",
      "REPORT_SOURCE_GPEI"            = "GPEI / Polio programme",
      "REPORT_SOURCE_HFI"             = "Health facility",
      "REPORT_SOURCE_MOH"             = "Ministry of Health",
      "REPORT_SOURCE_OTHER"           = "Other",
      "REPORT_SOURCE_OTHER_GOV_AGENCY" = "Other government agency",
      "REPORT_SOURCE_SITREP"          = "Situation report",
      "REPORT_SOURCE_SOCIAL_MEDIA"    = "Social media"
    )
  ) %>% 
  # Arrange key identification and operational variables first in the
  select(ems_id, starts_with("province"),district,type_of_entry, 
               signal_event_registration_date, signal_event_verification_status,
               verification_date, everything())



# Creating derived variables  ---------------------------------------------

# Agent/Syndrome field — three-way classification
ems <- ems %>%
  mutate(
    agent_syndrome_field_status = case_when(
      !is.na(agent_syndrome) ~ "Dropdown filled",
      is.na(agent_syndrome) & !is.na(agent_syndrome_label) ~ "Label only",
      is.na(agent_syndrome) & is.na(agent_syndrome_label) ~ "Both fields blank"
    )
  )



# pending verification backlog 
# Automatically determine the reference date from the current EMS dataset
analysis_end_date <- max(
  ems$signal_event_registration_date,
  na.rm = TRUE
)

# Calculate how many days each active unresolved signal has remained pending
ems <- ems %>%
  mutate(
    days_pending = if_else(
      type_of_entry == "Signal" &
        (is.na(signal_event_verification_status) |
           signal_event_verification_status == "Pending") &
        program_status == "Active",
      as.numeric(
        analysis_end_date - signal_event_registration_date
      ),
      NA_real_
    ),
    
    # Group unresolved signals into analytical age bands
    pending_age_band = case_when(
      !is.na(days_pending) & days_pending <= 7 ~ "0–7 days",
      !is.na(days_pending) & days_pending <= 14 ~ "8–14 days",
      !is.na(days_pending) & days_pending > 14 ~ ">14 days",
      TRUE ~ NA_character_
    )
  )



# Create a map-specific district name to harmonise EMS names with the shapefile
ems <- ems %>%
  mutate(
    district_map = recode(
      district,
      "Chienge" = "Chiengi",
      "Chikankata" = "Chikankanta",
      "Itezhi-tezhi" = "Itezhi-Tezhi",
      "Milenge" = "Milengi",
      "Mushindamo" = "Mushindano",
      "Senga" = "Senga Hill",
      "Shiwang'andu" = "Shiwang'Andu"
    )
  )










# Save the cleaned EMS dataset as an RDS file so it can be reused in
# subsequent analyses without repeating the cleaning process.

export(
  ems,
  here(
    "data",
    "clean",
    "ems_clean_dataset.rds" ))




# Testing area ------------------------------------------------------------

# Run these checks to confirm that the cleaning pipeline produced the
# expected structure, dates, geographic fields, and record classifications.
# These checks do not modify ems.


# Confirm that all EMS date variables have been converted to Date class
cat("\nDate column classes:\n")

date_cols <- c(
  "signal_event_registration_date",
  "verification_date",
  "event_start_date",
  "event_intervention_by_date",
  "public_communication_start_date"
)

for (col in date_cols) {
  cat(
    sprintf(
      "  %-40s %s\n",
      col,
      paste(class(ems[[col]]), collapse = "/")
    )
  )
}

# Check the range of signal registration dates in the current EMS dataset
cat("\nSignal registration date range:\n")
cat(
  "  Earliest:",
  format(min(ems$signal_event_registration_date, na.rm = TRUE)),
  "\n"
)
cat(
  "  Latest:  ",
  format(max(ems$signal_event_registration_date, na.rm = TRUE)),
  "\n"
)


# Check missing data across the cleaned dataset
cat("\nMissing data by column:\n")

miss_var_summary(ems) %>%
  filter(n_miss > 0) %>%
  arrange(desc(pct_miss)) %>%
  print(n = Inf)

# Display the unique provinces identified in the dataset
cat("\nUnique provinces extracted:\n")
print(sort(unique(ems$province)))



# Check the distribution of record types
cat("\nRecord type breakdown:\n")

tabyl(ems, type_of_entry) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting() %>%
  print()

# Check the distribution of verification status
cat("\nVerification status breakdown:\n")

tabyl(ems, signal_event_verification_status) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting() %>%
  print()


# Check the distribution of programme status
cat("\nProgramme status breakdown:\n")

tabyl(ems, program_status) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting() %>%
  print()

# Check the newly created agent/syndrome field-status variable
cat("\nAgent/syndrome field-status breakdown:\n")

tabyl(ems, agent_syndrome_field_status) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting() %>%
  print()

# Check the newly created pending-age variable
cat("\nPending verification age-band breakdown:\n")

tabyl(ems, pending_age_band) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting() %>%
  print()


# List unique district names in the shapefile
zambia_districts %>%
  st_drop_geometry() %>%
  distinct(adm2nm) %>%
  arrange(adm2nm)


# List unique district names in the EMS data
ems %>%
  distinct(district) %>%
  arrange(district)



# Identify EMS district names that do not match the shapefile
ems %>%
  distinct(district_map) %>%
  anti_join(
    st_drop_geometry(zambia_districts) %>%
      distinct(adm2nm),
    by = c("district_map" = "adm2nm")
  )





# Indicator analysis and operational intelligence  ------------------------

# ============================================================
# INDICATOR 1 — AGENT/SYNDROME FIELD COMPLETENESS
# ============================================================

# Define the analysis population as Signals and Events only.
ind1_data <- ems %>%
  filter(
    type_of_entry %in% c("Signal", "Event")
  )


# ------------------------------------------------------------
# Table 1: Use of the agent/syndrome fields
# ------------------------------------------------------------

# Summarise whether the structured dropdown, free-text field,
# or neither field was used.
ind1_table <- ind1_data %>%
  mutate(
    agent_syndrome_field_status = factor(
      agent_syndrome_field_status,
      levels = c(
        "Dropdown filled",
        "Label only",
        "Both fields blank"
      )
    )
  ) %>%
  select(agent_syndrome_field_status) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no",
    label = list(
      agent_syndrome_field_status ~
        "Agent/Syndrome Field Status"
    )
  )

ind1_table


# ------------------------------------------------------------
# Chart 1: Top 10 conditions selected through the dropdown
# ------------------------------------------------------------

# Identify the 10 most frequently selected conditions
# through the structured agent/syndrome dropdown.
ind1_dropdown_top10 <- ind1_data %>%
  filter(
    !is.na(agent_syndrome)
  ) %>%
  count(
    agent_syndrome,
    sort = TRUE
  ) %>%
  mutate(
    percentage = 100 * n / sum(n)
  ) %>%
  slice_head(n = 10)


# Plot the top 10 structured dropdown conditions.
ggplot(
  ind1_dropdown_top10,
  aes(
    x = reorder(agent_syndrome, percentage),
    y = percentage
  )
) +
  geom_col(
    fill = "steelblue"
  ) +
  geom_text(
    aes(
      label = paste0(round(percentage, 1), "%")
    ),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Top 10 Conditions Selected Through Structured Dropdown",
    x = "Condition",
    y = "Percentage of Dropdown Records"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )


# ------------------------------------------------------------
# Chart 2: Top 10 conditions entered only through free text
# ------------------------------------------------------------

# Identify the 10 most frequently entered conditions among
# records where the structured dropdown was not completed.
ind1_free_text_top10 <- ind1_data %>%
  filter(
    agent_syndrome_field_status == "Label only",
    !is.na(agent_syndrome_label)
  ) %>%
  count(
    agent_syndrome_label,
    sort = TRUE
  ) %>%
  mutate(
    percentage = 100 * n / sum(n)
  ) %>%
  slice_head(n = 10)


# Plot the top 10 free-text-only conditions.
ggplot(
  ind1_free_text_top10,
  aes(
    x = reorder(agent_syndrome_label, percentage),
    y = percentage
  )
) +
  geom_col(
    fill = "steelblue"
  ) +
  geom_text(
    aes(
      label = paste0(round(percentage, 1), "%")
    ),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Top 10 Conditions Entered Only Through Free Text",
    x = "Condition",
    y = "Percentage of Free-Text-Only Records"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

  

# Indicator 3 — Signals: top 10 districts with missing source

ems %>%
  filter(type_of_entry == "Signal") %>%
  group_by(district) %>%
  summarise(
    Records = n(),
    Missing = sum(is.na(signal_source)),
    `% Missing` = round(100 * Missing / Records, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Missing)) %>%
  slice_head(n = 10) %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Top 10 Districts with Signals Missing a Source"
  ) %>%
  gt::grand_summary_rows(
    columns = c(Records, Missing),
    fns = list(
      Total = ~sum(.)
    ),
    use_seps = TRUE
  )


# Indicator 3 — Events: top 10 districts with missing source


ems %>%
  filter(type_of_entry == "Event") %>%
  group_by(district) %>%
  summarise(
    Records = n(),
    Missing = sum(is.na(event_source)),
    `% Missing` = round(100 * Missing / Records, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Missing)) %>%
  slice_head(n = 10) %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Top 10 Districts with Events Missing a Source"
  ) %>%
  gt::grand_summary_rows(
    columns = c(Records, Missing),
    fns = list(
      Total = ~sum(.)
    ),
    use_seps = TRUE
  )


# Indicator 4 — Signals with no verification status

ems %>%
  filter(type_of_entry == "Signal") %>%
  group_by(district) %>%
  summarise(
    Records = n(),
    Missing = sum(is.na(signal_event_verification_status)),
    `% Missing` = round(100 * Missing / Records, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Missing)) %>%
  slice_head(n = 10) %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Top 10 Districts with Signals Missing Verification Status"
  ) %>%
  gt::grand_summary_rows(
    columns = c(Records, Missing),
    fns = list(
      Total = ~sum(.)
    ))



# ============================================================
# INDICATOR 5 — RISK ASSESSMENT WORKFLOW
# ============================================================

ems <- ems %>%
  mutate(
    # Verified true event without documented risk assessment
    flag_missing_risk_assessment =
      signal_event_verification_status == "Verified true event" &
      is.na(hra_auto_risk_assessment),
    
    # Pending Signal that already has a risk assessment
    flag_pending_with_risk_assessment =
      type_of_entry == "Signal" &
      signal_event_verification_status == "Pending" &
      !is.na(hra_auto_risk_assessment)
  )



# Indicator 5A — Verified true events missing risk assessment

# Indicator 5A — Verified true events missing risk assessment

ind5_missing_risk <- ems %>%
  filter(
    signal_event_verification_status == "Verified true event"
  ) %>%
  group_by(district) %>%
  summarise(
    Records = n(),
    Missing = sum(is.na(hra_auto_risk_assessment)),
    `% Missing` = round(100 * Missing / Records, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Missing)) %>%
  slice_head(n = 20)

ind5_missing_risk %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Top 20 Districts with Verified True Events Missing Risk Assessment"
  ) %>%
  gt::grand_summary_rows(
    columns = c(Records, Missing),
    fns = list(
      Total = ~sum(.)
    )
  ) %>%
  gt::grand_summary_rows(
    columns = `% Missing`,
    fns = list(
      Total = ~round(
        100 * sum(ind5_missing_risk$Missing) /
          sum(ind5_missing_risk$Records),
        1
      )
    )
  )
# Pending Signals with risk assessment recorded

# Indicator 5B — Pending Signals with risk assessment recorded

ind5_pending_risk <- ems %>%
  filter(
    type_of_entry == "Signal",
    signal_event_verification_status == "Pending"
  ) %>%
  group_by(district) %>%
  summarise(
    Records = n(),
    `Risk Assessment Recorded` =
      sum(!is.na(hra_auto_risk_assessment)),
    `% with Risk Assessment` =
      round(
        100 * `Risk Assessment Recorded` / Records,
        1
      ),
    .groups = "drop"
  ) %>%
  filter(`Risk Assessment Recorded` > 0) %>%
  arrange(desc(`Risk Assessment Recorded`)) %>%
  slice_head(n = 20)

ind5_pending_risk %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Top 20 Districts with Pending Signals and Risk Assessment Recorded"
  ) %>%
  gt::grand_summary_rows(
    columns = c(Records, `Risk Assessment Recorded`),
    fns = list(
      Total = ~sum(.)
    )
  ) %>%
  gt::grand_summary_rows(
    columns = `% with Risk Assessment`,
    fns = list(
      Total = ~round(
        100 * sum(ind5_pending_risk$`Risk Assessment Recorded`) /
          sum(ind5_pending_risk$Records),
        1
      )
    )
  )

# # Indicator 5A — Top 20 districts with missing risk assessment
# 
# ind5_risk <- ems %>%
#   filter(
#     signal_event_verification_status == "Verified true event"
#   ) %>%
#   group_by(district) %>%
#   summarise(
#     Records = n(),
#     Missing = sum(is.na(hra_auto_risk_assessment)),
#     `% Missing` = round(100 * Missing / Records, 1),
#     .groups = "drop"
#   ) %>%
#   arrange(desc(Missing)) %>%
#   slice_head(n = 20)
# 
# 
# 
# ind5_risk %>%
#   gt::gt() %>%
#   gt::tab_header(
#     title = "Top 20 Districts with Verified True Events Missing Risk Assessment"
#   ) %>%
#   gt::grand_summary_rows(
#     columns = c(Records, Missing),
#     fns = list(
#       Total = ~sum(.)
#     )
#   )
# 
# 
# 
# 
# # Indicator 5B — Risk assessment workflow inconsistencies
# 
# ind5_workflow <- tibble(
#   `Workflow Issue` = c(
#     "Verified true event with missing risk assessment",
#     "Pending signal with risk assessment recorded",
#     "Not an event with risk assessment recorded"
#   ),
#   Records = c(
#     sum(
#       ems$signal_event_verification_status == "Verified true event" &
#         is.na(ems$hra_auto_risk_assessment),
#       na.rm = TRUE
#     ),
#     sum(
#       ems$signal_event_verification_status == "Pending" &
#         !is.na(ems$hra_auto_risk_assessment),
#       na.rm = TRUE
#     ),
#     sum(
#       ems$signal_event_verification_status == "Not an event" &
#         !is.na(ems$hra_auto_risk_assessment),
#       na.rm = TRUE
#     )
#   )
# )
# 
# 
# 
# 
# # generate a table for risk assessment inconsistencies 
# ind5_workflow %>%
#   gt::gt() %>%
#   gt::tab_header(
#     title = "Risk Assessment Workflow Inconsistencies"
#   ) %>%
#   gt::grand_summary_rows(
#     columns = Records,
#     fns = list(
#       Total = ~sum(.)
#     )
#   )



# Indicator 6 — Pending verification backlog

# Check the age of active unresolved Signals

ems %>%
  filter(
    type_of_entry == "Signal",
    program_status == "Active",
    is.na(signal_event_verification_status) |
      signal_event_verification_status == "Pending"
  ) %>%
  summarise(
    oldest_signal = min(signal_event_registration_date, na.rm = TRUE),
    newest_signal = max(signal_event_registration_date, na.rm = TRUE),
    unresolved_signals = n()
  )


# Define the analysis end date from the latest registration date

analysis_end_date <- max(
  ems$signal_event_registration_date,
  na.rm = TRUE
)

# Calculate how long active unresolved Signals have remained pending

ems <- ems %>%
  mutate(
    days_pending = if_else(
      type_of_entry == "Signal" &
        (is.na(signal_event_verification_status) |
           signal_event_verification_status == "Pending") &
        program_status == "Active",
      as.numeric(
        analysis_end_date - signal_event_registration_date
      ),
      NA_real_
    ),
    
    pending_age_band = case_when(
      !is.na(days_pending) & days_pending <= 7 ~ "0–7 days",
      !is.na(days_pending) & days_pending <= 14 ~ "8–14 days",
      !is.na(days_pending) & days_pending > 14 ~ ">14 days",
      TRUE ~ NA_character_
    )
  )

# Indicator 6 — Top 20 districts by pending verification backlog

ems %>%
  filter(
    type_of_entry == "Signal",
    program_status == "Active",
    !is.na(pending_age_band)
  ) %>%
  count(
    district,
    pending_age_band
  ) %>%
  tidyr::pivot_wider(
    names_from = pending_age_band,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    `Total unresolved` =
      `0–7 days` +
      `8–14 days` +
      `>14 days`
  ) %>%
  arrange(desc(`Total unresolved`)) %>%
  slice_head(n = 20) %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Top 20 Districts by Pending Verification Backlog"
  ) %>%
  gt::grand_summary_rows(
    columns = c(
      `0–7 days`,
      `8–14 days`,
      `>14 days`,
      `Total unresolved`
    ),
    fns = list(
      Total = ~sum(.)))

# ============================================================
# INDICATOR 7A — TYPE OF ENTRY
# ============================================================

ind7_type <- ems %>%
  count(type_of_entry) %>%
  mutate(
    `%` = round(100 * n / sum(n), 1)
  ) %>%
  rename(
    `Type of Entry` = type_of_entry,
    Records = n
  )

ind7_type %>%
  gt::gt() %>%
  gt::tab_header(
    title = "EMS Records by Type of Entry"
  ) %>%
  gt::grand_summary_rows(
    columns = c(Records, `%`),
    fns = list(
      Total = ~sum(.)
    )
  )



# ============================================================
# INDICATOR 7B — VERIFICATION STATUS
# ============================================================

ind7_verification <- ems %>%
  count(signal_event_verification_status) %>%
  mutate(
    `%` = round(100 * n / sum(n), 1)
  ) %>%
  rename(
    `Verification Status` = signal_event_verification_status,
    Records = n
  )

ind7_verification %>%
  gt::gt() %>%
  gt::tab_header(
    title = "EMS Records by Verification Status"
  ) %>%
  gt::grand_summary_rows(
    columns = c(Records, `%`),
    fns = list(
      Total = ~sum(.)
    )
  )


# ============================================================
# INDICATOR 7C — PROGRAM STATUS
# ============================================================

ind7_program <- ems %>%
  count(program_status) %>%
  mutate(
    `%` = round(100 * n / sum(n), 1)
  ) %>%
  rename(
    `Program Status` = program_status,
    Records = n
  )

ind7_program %>%
  gt::gt() %>%
  gt::tab_header(
    title = "EMS Records by Program Status"
  ) %>%
  gt::grand_summary_rows(
    columns = c(Records, `%`),
    fns = list(
      Total = ~sum(.)
    )
  )

# ============================================================
# SECTION 8 — OVERALL OPERATIONAL FLAGS AND SUMMARY
# ============================================================
#
# Purpose:
# Combine the individual operational checks into a single
# record-level assessment of whether an EMS record requires
# operational attention.
#
# Important:
# - A record can have more than one operational issue.
# - Each record is counted only once in the overall
#   operational_flag.
# - The overall flag is applied only to Signals and Events.
# ============================================================



# ------------------------------------------------------------
# 8.1 Create binary operational flags
# ------------------------------------------------------------

ems <- ems %>%
  mutate(
    
    # Flag Signals/Events with incomplete structured
    # agent/syndrome information.
    flag_agent_syndrome =
      dplyr::coalesce(
        type_of_entry %in% c("Signal", "Event") &
          agent_syndrome_field_status %in% c(
            "Label only",
            "Both fields blank"
          ),
        FALSE
      ),
    
    # Flag Signals or Events where the appropriate
    # source field has not been recorded.
    flag_source =
      dplyr::coalesce(
        (type_of_entry == "Signal" &
           is.na(signal_source)) |
          (type_of_entry == "Event" &
             is.na(event_source)),
        FALSE
      ),
    
    # Flag Signals where no verification status
    # has been documented.
    flag_verification =
      dplyr::coalesce(
        type_of_entry == "Signal" &
          is.na(signal_event_verification_status),
        FALSE
      ),
    
    # Combine the two Indicator 5 risk-assessment
    # workflow conditions into one operational flag.
    flag_risk_assessment =
      dplyr::coalesce(
        flag_missing_risk_assessment |
          flag_pending_with_risk_assessment,
        FALSE
      ),
    
    # Flag active Signals that remain unresolved,
    # either Pending or without a documented
    # verification status.
    flag_backlog =
      dplyr::coalesce(
        type_of_entry == "Signal" &
          program_status == "Active" &
          (
            signal_event_verification_status == "Pending" |
              is.na(signal_event_verification_status)
          ),
        FALSE
      )
  )



# ------------------------------------------------------------
# 8.2 Create the overall record-level operational flag
# ------------------------------------------------------------

ems <- ems %>%
  mutate(
    operational_flag =
      flag_agent_syndrome |
      flag_source |
      flag_verification |
      flag_risk_assessment |
      flag_backlog
  )



# ------------------------------------------------------------
# 8.3 Summarise the number of records flagged by each check
# ------------------------------------------------------------

ind_operational_summary <- tibble(
  `Operational Issue` = c(
    "Agent/Syndrome information problem",
    "Missing source",
    "Missing verification status",
    "Risk assessment workflow issue",
    "Verification backlog"
  ),
  `Records Flagged` = c(
    sum(ems$flag_agent_syndrome),
    sum(ems$flag_source),
    sum(ems$flag_verification),
    sum(ems$flag_risk_assessment),
    sum(ems$flag_backlog)
  )
)

ind_operational_summary %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Operational Issues Identified in EMS Records"
  )




# ------------------------------------------------------------
# 8.4 Calculate the overall operational KPI
# ------------------------------------------------------------

ind_operational_kpi <- ems %>%
  filter(
    type_of_entry %in% c("Signal", "Event")
  ) %>%
  summarise(
    Records_reviewed = n(),
    Records_flagged = sum(operational_flag),
    Percent_flagged =
      round(
        100 * Records_flagged / Records_reviewed,
        1
      )
  )

ind_operational_kpi




# ------------------------------------------------------------
# 8.5 Examine how operational issues overlap within records
# ------------------------------------------------------------

ind_operational_overlap <- ems %>%
  filter(
    type_of_entry %in% c("Signal", "Event")
  ) %>%
  mutate(
    number_of_operational_flags =
      flag_agent_syndrome +
      flag_source +
      flag_verification +
      flag_risk_assessment +
      flag_backlog
  ) %>%
  count(number_of_operational_flags) %>%
  mutate(
    `%` = round(
      100 * n / sum(n),
      1
    )
  ) %>%
  rename(
    `Operational Issues per Record` =
      number_of_operational_flags,
    Records = n
  )

ind_operational_overlap %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Distribution of Operational Issues per EMS Record"
  )




# Calculate operational issue burden by district
district_operational <- ems %>%
  filter(
    type_of_entry %in% c("Signal", "Event")
  ) %>%
  group_by(district_map) %>%
  summarise(
    Records = n(),
    `Records with ≥1 issue` = sum(operational_flag),
    `% flagged` = round(
      100 * `Records with ≥1 issue` / Records,
      1
    ),
    .groups = "drop"
  )


# Join EMS operational results to district boundaries
zambia_operational <- zambia_districts %>%
  left_join(
    district_operational,
    by = c("adm2nm" = "district_map")
  )



# Create categories for the operational burden map
zambia_operational <- zambia_operational %>%
  mutate(
    map_category = case_when(
      Records < 20 ~ "Fewer than 20 records",
      `% flagged` <= 25 ~ "0–25%",
      `% flagged` <= 50 ~ "26–50%",
      `% flagged` <= 75 ~ "51–75%",
      `% flagged` > 75 ~ ">75%"
    )
  )



# map showing districts with 1 or more operational issues 
ggplot(zambia_operational) +
  geom_sf(
    aes(fill = map_category),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_manual(
    values = c(
      "Fewer than 20 records" = "grey80",
      "0–25%" = "#FFF2CC",
      "26–50%" = "#FFD966",
      "51–75%" = "#F4B183",
      ">75%" = "#C00000"
    ),
    name = "Operational issue burden"
  ) +
  labs(
    title = "EMS Operational Issues by District",
    subtitle = "Percentage of Signal/Event records with ≥1 operational issue"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      size = 10
    ),
    legend.title = element_text(
      face = "bold"
    )
  )


# Calculate the monthly percentage of Signal/Event records with
# at least one operational issue.
# This removes the effect of changes in the monthly number of records reviewed.
monthly_baseline <- ems %>%
  filter(
    type_of_entry %in% c("Signal", "Event"),
    lubridate::year(signal_event_registration_date) == 2026,
    lubridate::month(signal_event_registration_date) <= 8
  ) %>%
  mutate(
    month = lubridate::floor_date(
      signal_event_registration_date,
      unit = "month"
    )
  ) %>%
  group_by(month) %>%
  summarise(
    `Records reviewed` = n(),
    `Records flagged` = sum(operational_flag),
    `% flagged` = round(
      100 * `Records flagged` / `Records reviewed`,
      1
    ),
    .groups = "drop"
  )

# Review the monthly percentages before plotting.
monthly_baseline



# Plot the monthly percentage of Signal/Event records with
# at least one operational issue.
# This will serve as the baseline trend for future monthly reviews.
ggplot(
  monthly_baseline,
  aes(
    x = month,
    y = `% flagged`
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2.5
  ) +
  geom_text(
    aes(
      label = paste0(`% flagged`, "%")
    ),
    vjust = -0.8,
    size = 3.5
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Monthly EMS Operational Issue Baseline, January–August 2026",
    subtitle = "Percentage of Signal/Event records with ≥1 operational issue",
    x = "Month",
    y = "Records flagged (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )




# Calculate the headline indicators for the operational review.
overview <- ems %>%
  filter(
    type_of_entry %in% c("Signal", "Event")
  ) %>%
  summarise(
    `Records reviewed` = n(),
    `Records with ≥1 issue` = sum(operational_flag),
    `% with ≥1 issue` = round(
      100 * `Records with ≥1 issue` / `Records reviewed`,
      1
    ),
    `Pending verification backlog` = sum(flag_backlog),
    `Reporting districts` = n_distinct(district)
  )

# Review the overview indicators
overview