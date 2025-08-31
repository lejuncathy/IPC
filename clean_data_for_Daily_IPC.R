library(tidyverse)
library(readxl)
library(lubridate)
library(readr)
library(openxlsx)

#### Filepaths ####
disc_fp  <- "discrepancy_check_Daily_IPC.xlsx"
daily_fp <- "Daily_IPC_Checklist_-_latest_version_-_False_-_2025-02-18-18-58-56.xlsx"


## 1) Read discrepancy sheet: find rows where "indexes" contains multiple index values
disc_multi <- read_excel(disc_fp, sheet = "clinician dup check w discrep") %>%
  mutate(group_id = row_number()) %>%
  filter(!is.na(indexes), str_detect(indexes, "[,;]")) %>%
  separate_rows(indexes, sep = "[,;]\\s*") %>%
  mutate(index_num = parse_number(indexes) %>% as.integer()) %>%
  filter(!is.na(index_num))

## 2) Read Daily IPC Checklist: standardize index and submission time
daily <- read_excel(daily_fp, sheet = "Daily IPC Checklist") %>%
  mutate(
    index_int = suppressWarnings(as.integer(`_index`)),
    submission_time = parse_date_time(
      `_submission_time`,
      orders = c("Y/m/d H:M:S", "Y-m-d H:M:S", "Y/m/d", "Y-m-d"),
      tz = "UTC"
    )
  )

## 3) Match multi-index rows with corresponding Daily IPC Checklist rows
cand <- disc_multi %>%
  inner_join(daily, by = c("index_num" = "index_int"))

## 4) Keep only the latest row (based on submission_time) within each group_id
latest_per_cell <- cand %>%
  group_by(group_id) %>%
  slice_max(order_by = submission_time, n = 1, with_ties = FALSE) %>%
  ungroup()

## 5) Extract rows from Daily IPC Checklist that are not part of multi-index (single index rows)

unchanged <- daily %>%
  filter(!(index_int %in% disc_multi$index_num))


## 6) Extract rows that were deleted (older rows within duplicates)
deleted <- cand %>%
  anti_join(latest_per_cell, by = c("group_id", "index_num"))
## Extract their index numbers
deleted_indexes <- deleted$index_num

## 7) Merge: final result contains the latest row from multi-index groups + unchanged rows
final_result <- bind_rows(latest_per_cell, unchanged) %>%
  arrange(index_int)

## 8) Remove first 12 columns (A-L), then sort by index_int
final_result_cleaned <- final_result %>%
  select(-(1:12)) %>%     # 删除前 12 列
  arrange(`_index`)

## Export cleaned result
write.xlsx(final_result_cleaned, "Daily_IPC_Checklist_final_latest_unique_cleaned.xlsx")











####  Remove rows from PHH and KMC sheets where _parent_index is in deleted_indexes ####

## 1) Read the two sheets
phh <- read_excel(daily_fp, sheet = "patient_caregiver_hand_hygiene")
kmc <- read_excel(daily_fp, sheet = "KMC")

## 2)Remove rows where _parent_index belongs to deleted_indexes
phh_clean <- phh %>%
  filter(!(`_parent_index` %in% deleted_indexes))

kmc_clean <- kmc %>%
  filter(!(`_parent_index`%in% deleted_indexes))

## 3) Save back to Excel
write.xlsx(
  list(
    patient_caregiver_hand_hygiene = phh_clean,
    KMC = kmc_clean
  ),
  file = "PHH_KMC_cleaned.xlsx"
)








#### 2) Merge the three cleaned sheets into one Excel file ####


#### Filepaths ####
fp_original <- "Daily_IPC_Checklist_-_latest_version_-_False_-_2025-02-18-18-58-56.xlsx"
fp_cleaned  <- "PHH_KMC_cleaned.xlsx"
fp_parent_cleaned <- "Daily_IPC_Checklist_final_latest_unique_cleaned.xlsx"


fp_out      <- "Daily_with_cleaned_PHHandKMC.xlsx"


## 1) Load all sheets from the original workbook
wb <- loadWorkbook(fp_original)


## 2) Load the cleaned PHH, KMC, and parent sheets
phh_clean <- read.xlsx(fp_cleaned, sheet = "patient_caregiver_hand_hygiene")
kmc_clean <- read.xlsx(fp_cleaned, sheet = "KMC")
parent_clean <- read.xlsx(fp_parent_cleaned, sheet= "Sheet 1")

## 3) Add new sheets to the original workbook
addWorksheet(wb, "patient_hand_hygiene_cleaned")
writeData(wb, "patient_hand_hygiene_cleaned", phh_clean)

addWorksheet(wb, "KMC_cleaned")
writeData(wb, "KMC_cleaned", kmc_clean)

addWorksheet(wb, "Daily IPC Checklist cleaned")
writeData(wb, "Daily IPC Checklist cleaned", parent_clean)


## 4) Apply date-time formatting
dateStyle <- createStyle(numFmt = "yyyy-mm-dd hh:mm:ss")


addStyle(wb, sheet = "patient_hand_hygiene_cleaned", style = dateStyle,
         cols = which(names(phh_clean) == "_submission__submission_time"), rows = 2:(nrow(phh_clean)+1), gridExpand = TRUE)

addStyle(wb, sheet = "KMC_cleaned", style = dateStyle,
         cols = which(names(kmc_clean) == "_submission__submission_time"), rows = 2:(nrow(kmc_clean)+1), gridExpand = TRUE)


addStyle(wb, sheet = "Daily IPC Checklist cleaned", style = dateStyle,
         cols = which(names(parent_clean) %in% c("_submission_time", "start", "end","Date_and_time_of_visit")), rows = 2:(nrow(parent_clean)+1), gridExpand = TRUE)

## save
saveWorkbook(wb, fp_out, overwrite = TRUE)
























