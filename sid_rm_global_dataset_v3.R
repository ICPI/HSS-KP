## TITLE: Creating RM Global Dataset
## AUTHOR: Femi Akinmade (qlx6@cdc.gov)
## DESCRIPTION: 
##      RM GLobal Dataset
## CREATION DATE: 11/23/2021

rm(list = ls())
date <- Sys.Date()

setwd("C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/clusters/hss-kp/dm_sid/forMerge/pre_final_output/dataset_rm")

rm.list <- list.files(pattern='*.xlsx') # Read in multiple files
df.list <- lapply(rm.list, read_excel)  # Read in multiple files

global_df <- bind_rows(df.list)

setwd("C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/clusters/hss-kp/dm_sid/forMerge/pre_final_output/dataset_rm") # Set where you want the files should be saved
openxlsx::write.xlsx(global_df, file = paste("RM_Global_Extract_",date, "_Time_09-30.xlsx", sep = ""), keepNA = T, asTable = TRUE)
write.csv(global_df, file = "C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/clusters/hss-kp/dm_sid/forMerge/pre_final_output/dataset_rm/RM_Global_Dataset_09-30.csv")
