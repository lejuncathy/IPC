#### Checking out duplicates for room cleanliness ####
kobo_unlabeled_fp <- "Daily_with_cleaned_PHHandKMC.xlsx"
#### Loading libraries ####
library(tidyverse) # Has a lot of functions that make data manipulation.
library(readxl)
library(openxlsx)

## This is mostly just to highlight any duplicate issues
kobo_submission_metadata <- read_excel(kobo_unlabeled_fp, sheet = "Daily IPC Checklist cleaned") %>%
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
patient_caregiver_room_data <- read_excel(kobo_unlabeled_fp, sheet = "patient_hand_hygiene_cleaned") %>%
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
patient_caregiver_room_data_kmc <- read_excel(kobo_unlabeled_fp, sheet = "KMC_cleaned") %>%
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
  select(-c(`_submission__submission_time`:`_submission__tags`)) 
  #select(-c(`KMC/Floors_visibly_clean_kmc`:`KMC/Remark_KMC`))  



all_room_data <- bind_rows(patient_caregiver_room_data,
                           patient_caregiver_room_data_kmc) %>%
  left_join(kobo_submission_metadata %>% select(Hospital_Name_Full, Unit_Name_Full, date_visit, `_submission__uuid`, `_parent_index`),
            by = c("Hospital_Name_Full", "Unit_Name_Full", "_submission__uuid", "_parent_index"))


patient_room_dup_check <- all_room_data %>%
  #drop_na(Room_number) %>%
  group_by(Unit_Name_Full, Hospital_Name_Full, date_visit, Room_number) %>%
  summarise(
    submissions = n(),
    uuid_count = n_distinct(`_submission__uuid`),
    uuids = paste(unique(`_submission__uuid`), collapse = ", "),
    index_count = n_distinct(`_parent_index`),
    indexes = paste(unique(`_parent_index`), collapse = ", ")
  ) %>%
  arrange(desc(uuid_count))

#### Checking out duplicates for room cleanliness, and for discrepancies ####
all_room_data_dup <- all_room_data %>%
  group_by(Unit_Name_Full, Hospital_Name_Full, date_visit, Room_number) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(date_visit, Hospital_Name_Full, Unit_Name_Full)

ignore_cols <- c("Unit_Name_Full", "Hospital_Name_Full", "Room_number", "_index", "_parent_table_name", "_parent_index", "_submission__id", "_submission__uuid", "date_visit")
check_cols <- setdiff(names(all_room_data_dup), ignore_cols) # Keeps all column names, EXCEPT those ones that I don't want to compare.


all_room_data_dup_discrepcheck <- all_room_data_dup %>%
  group_by(Hospital_Name_Full, Unit_Name_Full, Room_number, date_visit) %>%
  summarise(
    across(all_of(check_cols), ~n_distinct(.) > 1) # For these groups, anytime there is more than 1 answer (i.e. a discrepancy), it will return "TRUE"
  )



all_room_data_discrep <- all_room_data_dup_discrepcheck %>%
  pivot_longer(
    cols = `patient_caregiver_hand_hygiene/Running_Water_PHH`:`KMC/Soap_available_kmc`,
    values_to = "discrep",
    names_to = "column_discrep"
  ) %>%
  filter(discrep == TRUE) %>%
  drop_na(Room_number) %>%
  group_by(Hospital_Name_Full, Unit_Name_Full, Room_number, date_visit) %>%
  summarise(
    col_discrep_count = n_distinct(column_discrep),
    col_discrep = paste(unique(column_discrep), collapse = ", ")
  )

patient_room_dup_check_wdiscrep <- left_join(patient_room_dup_check,
                                             all_room_data_discrep,
                                             by = c("Hospital_Name_Full", "Unit_Name_Full", "Room_number", "date_visit")) %>%
  arrange(desc(col_discrep_count), desc(uuid_count))


#### Saving the data ####
discrep_output_list <- list(
                            "ptntroom dup check" = patient_room_dup_check,
                            "ptntroom dup check w discrep" = patient_room_dup_check_wdiscrep) 

write.xlsx(discrep_output_list, file = "discrepancy_check_sh_room_20250827.xlsx")
