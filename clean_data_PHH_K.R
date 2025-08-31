
library(tidyverse)
library(readxl)
library(lubridate)
library(readr)
library(openxlsx)

## File paths
disc_fp  <- "discrepancy_check_sh_room_20250827.xlsx"
daily_fp <- "Daily_with_cleaned_PHHandKMC.xlsx"


# 1) Read discrepancy file: find rows where submission >=2
disc_multi_room <- read_excel(disc_fp, sheet = "ptntroom dup check w discrep") %>%
  mutate(group_id = row_number()) %>%
  mutate(index_num = as.integer(indexes)) %>%
  filter(!is.na(index_num)) %>%
  filter(submissions >= 2)


## 2) Read cleaned Daily IPC Checklist (two sheets: PHH + KMC) 
##    and convert `_parent_index` into integer for later matching
daily_PD_room <- read_excel(daily_fp, sheet = "patient_hand_hygiene_cleaned") %>%
  mutate(
    index_int = suppressWarnings(as.integer(`_parent_index`))
)

daily_K_room <- read_excel(daily_fp, sheet = "KMC_cleaned") %>%
  mutate(
    index_int = suppressWarnings(as.integer(`_parent_index`))
  )


## Consolidate multiple room number columns into a single variable
daily_PD_room2 <- daily_PD_room %>%
  mutate(Room_numberPD = coalesce(`patient_caregiver_hand_hygiene/Room_number_PHH_FH_NICU`,
                                  `patient_caregiver_hand_hygiene/Room_number_PHH_DT_NICU`,
                                  `patient_caregiver_hand_hygiene/Room_number_PHH_FH_L_D`,
                                  `patient_caregiver_hand_hygiene/Room_number_PHH_DT_L_D`))

daily_K_room2 <- daily_K_room %>%
  mutate(Room_numberK = coalesce(`KMC/Room_number_KMC_FH`,
                                 `KMC/Room_number_KMC_Debretabor`))


## 3) Match discrepancy rows with Daily data using index and room number
cand_mul_PD <- disc_multi_room %>%
  inner_join(daily_PD_room2, by = c("index_num" = "index_int", "Room_number" = "Room_numberPD"))

cand_mul_K <- disc_multi_room %>%
  inner_join(daily_K_room2, by = c("index_num" = "index_int", "Room_number" = "Room_numberK"))


## 4) For each duplicate group (group_id), keep only the row with the largest `_index` (latest submission)
latest_per_cell_PD <- cand_mul_PD %>%
  group_by(group_id) %>%
  slice_max(order_by = `_index`, n = 1, with_ties = FALSE) %>%
  ungroup()
latest_per_cell_PD <- latest_per_cell_PD %>%
  select(-(1:13)) %>%    
  arrange(`_index`)

latest_per_cell_K <- cand_mul_K %>%
  group_by(group_id) %>%
  slice_max(order_by = `_index`, n = 1, with_ties = FALSE) %>%
  ungroup()
latest_per_cell_K <- latest_per_cell_K %>%
  select(-(1:13)) %>%    
  arrange(`_index`)


## 5) For each duplicate group, identify the row with the smallest `_index` (to be deleted)
delete_per_cell_PD <- cand_mul_PD %>%
  group_by(group_id) %>%
  slice_min(order_by = `_index`, n = 1, with_ties = FALSE) %>%
  ungroup()
delete_per_cell_PD <- delete_per_cell_PD %>%
  select(-(1:13)) %>%    
  arrange(`_index`)


delete_per_cell_K <- cand_mul_K %>%
  group_by(group_id) %>%
  slice_min(order_by = `_index`, n = 1, with_ties = FALSE) %>%
  ungroup()
delete_per_cell_K <- delete_per_cell_K %>%
  select(-(1:13)) %>%    
  arrange(`_index`)




## 6) Remove the rows with smaller `_index` from the original Daily sheets 
daily_PD_room_clean <- daily_PD_room %>%
  anti_join(delete_per_cell_PD, by = "_index")


daily_K_room_clean <- daily_K_room %>%
  anti_join(delete_per_cell_K, by = "_index")




## 7) Save all cleaned results into a new Excel file

fp_out <- "final_cleaned.xlsx"

daily_ipc_cleaned <- read_excel(daily_fp, sheet = "Daily IPC Checklist cleaned")
visible_cleanliness <- read_excel(daily_fp, sheet = "visible_cleanliness")

write.xlsx(
  list(
    `Daily IPC Checklist cleaned` = daily_ipc_cleaned,
    daily_PHH_room_clean          = daily_PD_room_clean,
    daily_K_room_clean           = daily_K_room_clean,
    visible_cleanliness          = visible_cleanliness
  ),
  file = fp_out
)



