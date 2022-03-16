
## TITLE: Extraction & Manipulation of Responsibility Matrix Dataset
## AUTHOR: Femi Akinmade (qlx6@cdc.gov)
## DESCRIPTION: 
##      Extraction and tidying SID df for SIDs dashboard 2.0
## CREATION DATE: 11/23/2021

library(readxl)
library(tidyverse)
library(openxlsx)
library(dplyr)


rm(list = ls())
#sessionInfo()

date <- Sys.Date()
# ------------------------- Extracting the SID tool -------------------------- #
# ---------------------------------------------------------------------------- #
################################################################################

ouid <- read_csv(file = "C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/clusters/hss-kp/dm_sid/forMerge/pre_final_output/ouid.csv")  # 

# Read in the OU from file
#ou <- readxl::read_excel("Guyana_SID_ 2021.xlsx", sheet = "Dashboard", range = "E1:E1",col_names = FALSE)[[1]]
ou <- "Nepal"

# ---------------------------------------------------------------------------- #
################################################################################

setwd("C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/clusters/hss-kp/work_group-SID/SID_2021")

sid.list_21 <- list.files(pattern='*.xlsx') # Read in multiple files
#df.list_21 <- lapply(sid.list_21, read_excel)  # Read in multiple files

sid_2021_tab_a <- read.xlsx("Nepal_SID_2021_clone.xlsx", sheet = "A. Governance, Leadership & Acc", rows = 3:292, cols = 1:12) # Change the SID assessment tool
id <- rownames(sid_2021_tab_a)
sid_2021_tab_a <- cbind(id=id, sid_2021_tab_a)
colnames(sid_2021_tab_a) <- paste0("col_", 1:ncol(sid_2021_tab_a))# Add temp col names
sid_2021_tab_a$col_1 <- as.numeric(sid_2021_tab_a$col_1)

sid_2021_tab_b <- read.xlsx("Nepal_SID_2021_clone.xlsx", sheet = "B. Natl Health System & Serv", rows = 2:224, cols = 1:18) # Change the SID assessment tool
id <- rownames(sid_2021_tab_b)
sid_2021_tab_b <- cbind(id=id, sid_2021_tab_b)
colnames(sid_2021_tab_b) <- paste0("col_", 1:ncol(sid_2021_tab_b))# Add temp col names
sid_2021_tab_b$col_1 <- as.numeric(sid_2021_tab_b$col_1)


sid_2021_tab_c <- read.xlsx("Nepal_SID_2021_clone.xlsx", sheet = "C. Strat Finance & Mkt Openness", rows = 4:211, cols = 1:12) # Change the SID assessment tool
id <- rownames(sid_2021_tab_c)
sid_2021_tab_c <- cbind(id=id, sid_2021_tab_c)
colnames(sid_2021_tab_c) <- paste0("col_", 1:ncol(sid_2021_tab_c))# Add temp col names
sid_2021_tab_c$col_1 <- as.numeric(sid_2021_tab_c$col_1)

sid_2021_tab_d <- read.xlsx("Nepal_SID_2021_clone.xlsx", sheet = "D. Strategic Info", rows = 3:196, cols = 1:18) # Change the SID assessment tool
id <- rownames(sid_2021_tab_d)
sid_2021_tab_d <- cbind(id=id, sid_2021_tab_d)
colnames(sid_2021_tab_d) <- paste0("col_", 1:ncol(sid_2021_tab_d))# Add temp col names
sid_2021_tab_d$col_1 <- as.numeric(sid_2021_tab_d$col_1)

# ---------------------------------------------------------------------------- #

sid.list_21
# ========== Write all sids raw extracts ========== #
#setwd("C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/clusters/hss-kp/dm_sid/forMerge/raw_sid_extract/")

setwd("C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/clusters/hss-kp/dm_sid/forMerge/raw_sid_extract")

write_csv(sid_2021_tab_a, file = "sid_a_21.csv")
write_csv(sid_2021_tab_b, file = "sid_b_21.csv")
write_csv(sid_2021_tab_c, file = "sid_c_21.csv")
write_csv(sid_2021_tab_d, file = "sid_d_21.csv")

# ============================================= #
# ============================================= #

# ------------- Read Match files in ------------ #
#################################################

# Read in extracted tabs from SID

sid21_a <- read_csv(file = "sid_a_21.csv", trim_ws = TRUE, col_types = cols(Temp_ID = col_character()))
sid21_b <- read_csv(file = "sid_b_21.csv", trim_ws = TRUE, col_types = cols(Temp_ID = col_character()))
sid21_c <- read_csv(file = "sid_c_21.csv", trim_ws = TRUE, col_types = cols(Temp_ID = col_character()))
sid21_d <- read_csv(file = "sid_d_21.csv", trim_ws = TRUE, col_types = cols(Temp_ID = col_character()))

# Read in R crosswalk
setwd("C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/clusters/hss-kp/dm_sid/forMerge/crosswalk_master")

cw_a <- readxl::read_excel("cw_a_KW.xlsx", col_type = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
                                                        "text", "text", "numeric", "numeric", "numeric", "numeric"))

cw_b <- readxl::read_excel("cw_b_KW.xlsx", col_type = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
                                                        "text", "text", "numeric", "numeric", "numeric", "numeric"))

cw_c <- readxl::read_excel("cw_c_KW.xlsx", col_type = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
                                                        "text", "text", "numeric", "numeric", "numeric", "numeric"))

cw_d <- readxl::read_excel("cw_d_KW.xlsx", col_type = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
                                                        "text", "text", "numeric", "numeric", "numeric", "numeric"))


# ---------------------------------------------------------------------------- #
# ========== Merge CW & SID Extract. Add columns and select columns ========== #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

tab_a_2021 <- merge(cw_a, sid21_a, by.x = c("Temp_ID_2021"), by.y = c("col_1")) %>% 
  mutate(SIDdomain = "A") %>% 
  mutate(operatingunit = ou) %>%
  mutate(fiscal_year = 2021) %>%
  select(operatingunit, SIDdomain, `2017_SIDUID`, `2019_SIDUID`, `2021_SIDUID`,  disagg, SIDQuestion, SIDshortquestion, 
         SIDfullquestion, Short_Question, program_area1:program_area5, Question_Type, col_2, col_4:col_13, fiscal_year, 
         JID) %>% 
  unite(JID_1, c(SIDdomain, JID), sep = "_", remove = FALSE)

all_a <- tab_a_2021 %>% 
  rename(SIDresponse = col_4, SIDresponsescore = col_5, SIDshortquestion_score = col_6, 
         SIDraw_answer = col_7, SIDmultiplier = col_8, SIDweight = col_9, SIDquestions_applied = col_10, 
         SIDweighted_answer = col_11, Data_Source = col_12, quali_response = col_13)

a <- nrow(all_a)
an <- sprintf("AUID_%03d", 1:a)
all_a <- bind_cols(an, all_a) %>% rename(UUID = "...1")
# ---------------------------------------------------------------------------- #

tab_b_2021 <- merge(cw_b, sid21_b, by.x = c("Temp_ID_2021"), by.y = c("col_1")) %>% 
  mutate(SIDdomain = "B") %>% 
  mutate(operatingunit = ou) %>% 
  mutate(fiscal_year = 2021) %>%
  select(operatingunit, SIDdomain, `2017_SIDUID`, `2019_SIDUID`, `2021_SIDUID`,  disagg, SIDQuestion, SIDshortquestion, 
         SIDfullquestion, Short_Question, program_area1:program_area5, Question_Type, col_2, col_4:col_14, fiscal_year, 
         JID) %>% 
  unite(JID_1, c(SIDdomain, JID), sep = "_", remove = FALSE)



all_b <- tab_b_2021 %>%
  rename(SIDresponse = col_5, SIDresponsescore = col_6, SIDshortquestion_score = col_7, 
         SIDraw_answer = col_8, SIDmultiplier = col_9, SIDweight = col_10, SIDquestions_applied = col_11, 
         SIDweighted_answer = col_12, Data_Source = col_13, quali_response = col_14) %>% 
  select(-c(col_4))

rm(a, an)
b <- nrow(all_b)
bn <- sprintf("BUID_%03d", 1:b)
all_b <- bind_cols(bn, all_b) %>% rename(UUID = "...1")
# ---------------------------------------------------------------------------- #

tab_c_2021 <- merge(cw_c, sid21_c, by.x = c("Temp_ID_2021"), by.y = c("col_1")) %>% 
  mutate(SIDdomain = "C") %>% 
  mutate(operatingunit = ou) %>%
  mutate(fiscal_year = 2021) %>%
  select(operatingunit, SIDdomain, `2017_SIDUID`, `2019_SIDUID`, `2021_SIDUID`, disagg, SIDQuestion, SIDshortquestion, 
         SIDfullquestion, Short_Question, program_area1:program_area5, Question_Type, col_2, col_4:col_13, fiscal_year, 
         JID) %>% 
  unite(JID_1, c(SIDdomain, JID), sep = "_", remove = FALSE)

all_c <- tab_c_2021 %>% 
  rename(SIDresponse = col_4, SIDresponsescore = col_5, SIDshortquestion_score = col_6, 
         SIDraw_answer = col_7, SIDmultiplier = col_8, SIDweight = col_9, SIDquestions_applied = col_10, 
         SIDweighted_answer = col_11, Data_Source = col_12, quali_response = col_13)
rm(b, bn)
c <- nrow(all_c)
cn <- sprintf("CUID_%03d", 1:c)
all_c <- bind_cols(cn, all_c) %>% rename(UUID = "...1")
# ---------------------------------------------------------------------------- #

tab_d_2021 <- merge(cw_d, sid21_d, by.x = c("Temp_ID_2021"), by.y = c("col_1")) %>% 
  mutate(SIDdomain = "D") %>% 
  mutate(operatingunit = ou) %>% 
  mutate(fiscal_year = 2021) %>%
  select(operatingunit, SIDdomain, `2017_SIDUID`, `2019_SIDUID`, `2021_SIDUID`,  disagg, SIDQuestion, SIDshortquestion, 
         SIDfullquestion, Short_Question, program_area1:program_area5, Question_Type, col_2, col_4:col_14, fiscal_year, 
         JID) %>% 
  unite(JID_1, c(SIDdomain, JID), sep = "_", remove = FALSE)

all_d <- tab_d_2021 %>%
  rename(SIDresponse = col_5, SIDresponsescore = col_6, SIDshortquestion_score = col_7, 
         SIDraw_answer = col_8, SIDmultiplier = col_9, SIDweight = col_10, SIDquestions_applied = col_11, 
         SIDweighted_answer = col_12, Data_Source = col_13, quali_response = col_14) %>% 
  select(-c(col_4))

rm(c, cn)
d <- nrow(all_d)
dn <- sprintf("DU_%03d", 1:d)
all_d <- bind_cols(dn, all_d) %>% rename(UUID = "...1")
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #

all_a$SIDresponse <- as.character(all_a$SIDresponse)
all_a$SIDresponsescore <- as.character(all_a$SIDresponsescore)
all_a$SIDshortquestion_score <- as.character(all_a$SIDshortquestion_score)
all_a$SIDraw_answer <- as.character(all_a$SIDraw_answer)
all_a$SIDmultiplier <- as.character(all_a$SIDmultiplier)
all_a$SIDweight <- as.character(all_a$SIDweight)
all_a$SIDquestions_applied <- as.character(all_a$SIDquestions_applied)
all_a$SIDquestions_applied <- as.character(all_a$SIDquestions_applied)
all_a$SIDweighted_answer <- as.character(all_a$SIDweighted_answer)
all_a$Data_Source <- as.character(all_a$Data_Source)
all_a$quali_response <- as.character(all_a$quali_response)

all_b$SIDresponse <- as.character(all_b$SIDresponse)
all_b$SIDresponsescore <- as.character(all_b$SIDresponsescore)
all_b$SIDshortquestion_score <- as.character(all_b$SIDshortquestion_score)
all_b$SIDraw_answer <- as.character(all_b$SIDraw_answer)
all_b$SIDmultiplier <- as.character(all_b$SIDmultiplier)
all_b$SIDweight <- as.character(all_b$SIDweight)
all_b$SIDquestions_applied <- as.character(all_b$SIDquestions_applied)
all_b$SIDquestions_applied <- as.character(all_b$SIDquestions_applied)
all_b$SIDweighted_answer <- as.character(all_b$SIDweighted_answer)
all_b$Data_Source <- as.character(all_b$Data_Source)
all_b$quali_response <- as.character(all_b$quali_response)

all_c$SIDresponse <- as.character(all_c$SIDresponse)
all_c$SIDresponsescore <- as.character(all_c$SIDresponsescore)
all_c$SIDshortquestion_score <- as.character(all_c$SIDshortquestion_score)
all_c$SIDraw_answer <- as.character(all_c$SIDraw_answer)
all_c$SIDmultiplier <- as.character(all_c$SIDmultiplier)
all_c$SIDweight <- as.character(all_c$SIDweight)
all_c$SIDquestions_applied <- as.character(all_c$SIDquestions_applied)
all_c$SIDquestions_applied <- as.character(all_c$SIDquestions_applied)
all_c$SIDweighted_answer <- as.character(all_c$SIDweighted_answer)
all_c$Data_Source <- as.character(all_c$Data_Source)
all_c$quali_response <- as.character(all_c$quali_response)

all_d$SIDresponse <- as.character(all_d$SIDresponse)
all_d$SIDresponsescore <- as.character(all_d$SIDresponsescore)
all_d$SIDshortquestion_score <- as.character(all_d$SIDshortquestion_score)
all_d$SIDraw_answer <- as.character(all_d$SIDraw_answer)
all_d$SIDmultiplier <- as.character(all_d$SIDmultiplier)
all_d$SIDweight <- as.character(all_d$SIDweight)
all_d$SIDquestions_applied <- as.character(all_d$SIDquestions_applied)
all_d$SIDquestions_applied <- as.character(all_d$SIDquestions_applied)
all_d$SIDweighted_answer <- as.character(all_d$SIDweighted_answer)
all_d$Data_Source <- as.character(all_d$Data_Source)
all_d$quali_response <- as.character(all_d$quali_response)



sid_all_1 <- bind_rows(all_a, all_b, all_c, all_d)

#sid_all_1 <- sid_all_1 %>%  # Swaziland versus Eswatini
#  mutate(operatingunit = case_when(
#    operatingunit == "Swaziland" ~ "Eswatini"
#  ))




sid_all <- merge(sid_all_1, ouid, by.x = c("operatingunit"), by.y = c("operatingunit")) %>% 
  rename(SIDsubquestion = Short_Question, questiontype = Question_Type) %>% 
  select(UUID, JID_1, `2017_SIDUID`, `2019_SIDUID`, `2021_SIDUID`, operatingunit, operatingunituid, disagg, SIDdomain, 
         SIDQuestion, SIDshortquestion, SIDfullquestion, SIDsubquestion, questiontype, program_area1:program_area5, 
         SIDresponse:fiscal_year, JID)

#sid_all[is.na(sid_all)] <- ""

sid_all$SIDmultiplier <- as.numeric(sid_all$SIDmultiplier)
sid_all$SIDweight <- as.numeric(sid_all$SIDweight)
sid_all$SIDquestions_applied <- as.numeric(sid_all$SIDquestions_applied)
sid_all$SIDraw <- as.numeric(sid_all$SIDraw)
sid_all$SIDraw_answer <- as.numeric(sid_all$SIDraw_answer)
sid_all$SIDweighted_answer <- as.numeric(sid_all$SIDweighted_answer)

sid_all[is.na(sid_all)] <- ""

sid_all_rounded <- sid_all %>% 
  mutate_if(is.numeric, round, digits=2)





# - Trend Dataset -------#
t_21 <- sid_all %>% filter(fiscal_year %in% c("2021")) %>% 
  select(UUID, JID, JID_1, SIDdomain, SIDshortquestion,
         SIDfullquestion, SIDsubquestion, questiontype, 
         SIDresponse, SIDresponsescore)




trend <- sid_all %>% 
  select(UUID, JID, JID_1, SIDdomain,  SIDQuestion, SIDshortquestion, SIDfullquestion, 
         SIDsubquestion, questiontype, SIDresponse, SIDresponsescore) %>% 
  rename(Universal_UID = UUID,
         Domain = SIDdomain,
         SID_Sub_Question = SIDshortquestion, 
         SID_Question = SIDfullquestion, 
         Response = SIDsubquestion, 
         Response_Type = questiontype, 
         FY_21_SIDresponse = SIDresponse, 
         FY_21_SIDresponsescore = SIDresponsescore) %>% 
  select(Universal_UID, JID, Domain, SIDQuestion, SID_Sub_Question, SID_Question, Response, Response_Type,
         FY_21_SIDresponse, FY_21_SIDresponsescore) %>% 
  mutate(Response_2021 = case_when(
    is.na(FY_21_SIDresponsescore) ~ "N",
    FY_21_SIDresponsescore == 0 ~ "N",
    FY_21_SIDresponsescore < 0 ~ "Y",
    FY_21_SIDresponsescore > 0 ~ "Y"))



setwd("C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/clusters/hss-kp/dm_sid/forMerge/pre_final_output/dataset_sid") # Set where you want the files should be saved
openxlsx::write.xlsx(sid_all, file = paste("SID_Data_Extract_",ou,"_",date, "_SID_2.0_Time_9-00.xlsx", sep = ""), keepNA = T, asTable = TRUE)

openxlsx::write.xlsx(trend, file = paste("SID_FY_Trend_",ou,"_",date, "_SID_Time_9-00.xlsx", sep = ""), keepNA = T, asTable = TRUE)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

