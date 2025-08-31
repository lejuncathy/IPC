#### A check for discrepancies of stop sepsis data ####


#### Filepaths ####
kobo_unlabeled_fp <- "Daily_IPC_Checklist_-_latest_version_-_False_-_2025-02-18-18-58-56.xlsx"

#### Loading libraries ####
library(tidyverse) # Has a lot of functions that make data manipulation.
library(readxl)
library(openxlsx)

#### Clinician, Just checking for multiple UUIDs ####
#### Clinician, loading and initial data processing 
clinician_data <- read_excel(kobo_unlabeled_fp, sheet = "Daily IPC Checklist") %>%
  mutate(Hospital_Name_Full = case_when( #Make a column with the hospital name (for graph labeling purposes)
    Hospital_Name == 1 ~ "Felegehiwot", #If it's hospital 1, give it the name "Felegehiwot"
    Hospital_Name == 2 ~ "Debretabor" #If it's hospital 2, give it the name "Debretabor"
  ), .after = Hospital_Name) %>% #Put this column after the "Hospital Name" column
  mutate(Unit_Name_Full = case_when( #Make another column with the name of the unit (which I don't really use).
    Unit_Name == 1 ~ "L & D",
    Unit_Name == 2 ~ "NICU",
    Unit_Name == 3 ~ "KMC"
  ), .after = Unit_Name) %>%
  mutate(week_visit_mid = case_when( # To more cleanly plot data, it can be nice to have a consistent week date, so data data from that week line up (even if checks were done on different dates)
    year(Date_and_time_of_visit) == 2023 ~ ymd("2023-01-01") + weeks(week(Date_and_time_of_visit) - 1) + 3,
    year(Date_and_time_of_visit) == 2024 ~ ymd("2024-01-01") + weeks(week(Date_and_time_of_visit) - 1) + 3
  )) %>%
  mutate(date_visit = as.Date(Date_and_time_of_visit), .after = Date_and_time_of_visit)

### Checking which have multiple submissions/uuids
clinician_submission_check <- clinician_data %>%
  group_by(Hospital_Name_Full, Unit_Name_Full, date_visit) %>%
  summarise(
    submissions = n(),
    uuid_count = n_distinct(`_uuid`),
    uuids = paste(unique(`_uuid`), collapse = ", "),
    index_count = n_distinct(`_index`),
    indexes = paste(unique(`_index`), collapse = ", ")
  ) %>%
  arrange(desc(submissions), date_visit)

#### Clinician, checking which also have discrepancies w/in groups ####
### Starting by making a version of the data that is only duplicates
clinician_data_dup <- clinician_data %>%
  group_by(Hospital_Name_Full, Unit_Name_Full, date_visit) %>%
  filter(n()>1) %>% # Filtering out all cases where there is more than one response per unit/week
  ungroup() %>%
  arrange(week_visit_mid, Hospital_Name_Full, Unit_Name_Full)

# Added on 7/3/2025 to flag which have different responses
### Next assigning which columns to check
ignore_cols <- c("_uuid","_index", "week_visit_mid", "Date_and_time_of_visit", "date_visit", "Hospital_Name_Full", "Unit_Name_Full", "week_visit_mid") # Columns where I don't want it to flag if there is a difference in the colum
check_cols <- setdiff(names(clinician_data_dup), ignore_cols) # Keeps all column names, EXCEPT those ones that I don't want to compare.

clinican_data_dup_discrepcheck <- clinician_data_dup %>%
  group_by(Hospital_Name_Full, Unit_Name_Full, date_visit) %>%
  summarise(
    across(all_of(check_cols), ~n_distinct(.) > 1) # For these groups, anytime there is more than 1 answer (i.e. a discrepancy), it will return "TRUE"
  )



### Going to pivot longer so it will be easier to summarise all discrepancies per group
clinican_data_cols_discrep <- clinican_data_dup_discrepcheck %>%
  pivot_longer(
    cols = Hospital_Name:`group_zu5jb43_standing_water_1/group_zu5jb43_standing_water_1_shower_patient`,
    values_to = "discrep",
    names_to = "column_discrep"
  ) %>%
  filter(discrep == TRUE) %>%
  group_by(Hospital_Name_Full, Unit_Name_Full, date_visit) %>%
  summarise(
    col_discrep_count = n_distinct(column_discrep),
    col_discrep = paste(unique(column_discrep), collapse = ", ")
  )


clinician_submission_check_wdiscrep <- left_join(clinician_submission_check,
                                                 clinican_data_cols_discrep,
                                                 by = c("Hospital_Name_Full", "Unit_Name_Full", "date_visit")) %>%
  arrange(desc(col_discrep_count))

#### Checking out duplicates for room cleanliness ####
## This is mostly just to highlight any duplicate issues
kobo_submission_metadata <- read_excel(kobo_unlabeled_fp, sheet = "Daily IPC Checklist") %>%
  select(Hospital_Name, Unit_Name, Date_and_time_of_visit, `_uuid`, `_index`) %>%
  mutate(Hospital_Name_Full = case_when( #Make a column with the hospital name (for graph labeling purposes)
    Hospital_Name == 1 ~ "Felegehiwot", #If it's hospital 1, give it the name "Felegehiwot"
    Hospital_Name == 2 ~ "Debretabor" #If it's hospital 2, give it the name "Debretabor"
  ), .after = Hospital_Name) %>% #Put this column after the "Hospital Name" column
  mutate(Unit_Name_Full = case_when( #Make another column with the name of the unit (which I don't really use).
    Unit_Name == 1 ~ "L & D",
    Unit_Name == 2 ~ "NICU",
    Unit_Name == 3 ~ "KMC"
  ), .after = Unit_Name) %>%
  mutate(date_visit = as.Date(Date_and_time_of_visit), .after = Date_and_time_of_visit) %>%
  rename(`_parent_index` = `_index`) %>%
  rename(`_submission__uuid` = `_uuid`) %>%
  distinct(`_parent_index`, `_submission__uuid`, .keep_all = TRUE)

## Loading the main sheet with data (which has the NICU & L&D units)
patient_caregiver_room_data <- read_excel(kobo_unlabeled_fp, sheet = "patient_caregiver_hand_hygiene") %>%
  mutate(Unit_Name_Full = case_when( # More cleanly assigning the unit name
    !is.na(`patient_caregiver_hand_hygiene/Room_number_PHH_FH_NICU`) ~ "NICU",
    !is.na(`patient_caregiver_hand_hygiene/Room_number_PHH_DT_NICU`) ~ "NICU",
    !is.na(`patient_caregiver_hand_hygiene/Room_number_PHH_FH_L_D`) ~ "L & D",
    !is.na(`patient_caregiver_hand_hygiene/Room_number_PHH_DT_L_D`) ~ "L & D"
  ), .before = "patient_caregiver_hand_hygiene/Room_number_PHH_FH_NICU") %>%
  mutate(Hospital_Name_Full = case_when( # More cleanly assigning the hospital name
    !is.na(`patient_caregiver_hand_hygiene/Room_number_PHH_FH_NICU`) ~ "Felegehiwot",
    !is.na(`patient_caregiver_hand_hygiene/Room_number_PHH_DT_NICU`) ~ "Debretabor",
    !is.na(`patient_caregiver_hand_hygiene/Room_number_PHH_FH_L_D`) ~ "Felegehiwot",
    !is.na(`patient_caregiver_hand_hygiene/Room_number_PHH_DT_L_D`) ~ "Debretabor"
  ), .after = "Unit_Name_Full") %>%
  mutate(Room_number = case_when( # You only columns filled out for the unit you're in, so I'm trying to consolidate them here
    !is.na(`patient_caregiver_hand_hygiene/Room_number_PHH_FH_NICU`) ~ `patient_caregiver_hand_hygiene/Room_number_PHH_FH_NICU`,
    !is.na(`patient_caregiver_hand_hygiene/Room_number_PHH_DT_NICU`) ~ `patient_caregiver_hand_hygiene/Room_number_PHH_DT_NICU`,
    !is.na(`patient_caregiver_hand_hygiene/Room_number_PHH_FH_L_D`) ~ `patient_caregiver_hand_hygiene/Room_number_PHH_FH_L_D`,
    !is.na(`patient_caregiver_hand_hygiene/Room_number_PHH_DT_L_D`) ~ `patient_caregiver_hand_hygiene/Room_number_PHH_DT_L_D`
  ), .after = Hospital_Name_Full) %>%
  select(-c(`patient_caregiver_hand_hygiene/Room_number_PHH_FH_NICU`:`patient_caregiver_hand_hygiene/Room_number_PHH_DT_L_D`)) %>% # Removing some extra columns
  select(-c(`_submission__submission_time`:`_submission__tags`)) 

## Loading all the data for the KMC unit
patient_caregiver_room_data_kmc <- read_excel(kobo_unlabeled_fp, sheet = "KMC") %>%
  mutate(Unit_Name_Full = "KMC", .before = "KMC/Room_number_KMC_FH") %>% # Assigning the unit
  mutate(Hospital_Name_Full = case_when( # Assigning the hospital name
    !is.na(`KMC/Room_number_KMC_FH`) ~ "Felegehiwot",
    !is.na(`KMC/Room_number_KMC_Debretabor`) ~ "Debretabor"
  ), .after = "Unit_Name_Full") %>%
  mutate(Room_number = case_when( # There are two column with room numbers, one of which is actually filled
    !is.na(`KMC/Room_number_KMC_FH`) ~ `KMC/Room_number_KMC_FH`,
    !is.na(`KMC/Room_number_KMC_Debretabor`) ~ `KMC/Room_number_KMC_Debretabor`
  ), .after = Hospital_Name_Full) %>%
  select(-c(`KMC/Room_number_KMC_FH`:`KMC/Room_number_KMC_Debretabor`)) %>% # Removing a bunch of columns
  select(-c(`_submission__submission_time`:`_submission__tags`)) %>%
  select(-c(`KMC/Floors_visibly_clean_kmc`:`KMC/Remark_KMC`))  

all_room_data <- bind_rows(patient_caregiver_room_data,
                           patient_caregiver_room_data_kmc) %>%
  left_join(kobo_submission_metadata %>% select(Hospital_Name_Full, Unit_Name_Full, date_visit, `_submission__uuid`, `_parent_index`),
            by = c("Hospital_Name_Full", "Unit_Name_Full", "_submission__uuid", "_parent_index"))


patient_room_dup_check <- all_room_data %>%
  drop_na(Room_number) %>%
  group_by(Unit_Name_Full, Hospital_Name_Full, date_visit, Room_number) %>%
  summarise(
    submissions = n(),
    uuid_count = n_distinct(`_submission__uuid`),
    uuids = paste(unique(`_submission__uuid`), collapse = ", "),
    index_count = n_distinct(`_parent_index`),
    indexes = paste(unique(`_parent_index`), collapse = ", ")
  ) %>%
  arrange(desc(uuid_count))



#### Saving the data ####
discrep_output_list <- list("clinician dup check" = clinician_submission_check,
                            "clinician dup check w discrep" = clinician_submission_check_wdiscrep
                            ) 

write.xlsx(discrep_output_list, file = "discrepancy_check_Daily_IPC.xlsx")
