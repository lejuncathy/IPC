# Abstract


The workflow focuses on identifying duplicate submissions, resolving discrepancies。

---
  
  ## 📂 Project Structure
  
  | File/Folder                                                          | Description                     |
  |----------------------------------------------------------------------|---------------------------------|
  | `Daily_IPC_Checklist_-latest_version-_False-2025-02-18-18-58-56.xlsx`| Original raw data               |
  | `stop sepsis discrepancy check sh new.R `                            | discrepancy check (daily IPC)   |
  | `clean_data_for_Daily_IPC.R `                                        | clean Daily IPC Checklist       |
  | `discrepancy for room.R `                                            | room-level duplicate detection. |    
  | `clean_data_PHH_K.R  `                                               | Script to clean PHH & KMC data. |
  
  
  
  ---
  
  ## 🚀 Workflow
  input:Daily_IPC_Checklist_-latest_version-_False-2025-02-18-18-58-56.xlsx


1. **Discrepancy detection for Daily_IPC_Checklist **  
  `stop sepsis discrepancy check sh new.R`

2. **Daily IPC Checklist cleaning**  
  - Remove duplicated submissions.  
- Keep the latest record when duplicates exist.  

`clean_data_for_Daily_IPC.R`

3. **Discrepancy detection for PHH & KMC after the cleaning for Daily IPC Checklist**          
  `discrepancy for room.R`


4. **PHH & KMC cleaning**  
  - Remove duplicated submissions.  
- Keep the latest record "index" when duplicates exist. 

`clean_data_PHH_K.R `  

output:final_cleaned.xlsx

---