
## TITLE: Extraction & Manipulation of Responsibility Matrix Dataset
## AUTHOR: Femi Akinmade (qlx6@cdc.gov)
## DESCRIPTION: 
##      Extraction and tidying RM df for SIDs dashboard 2.0
## CREATION DATE: 11/23/2021

rm(list = ls())


setwd("C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/clusters/hss-kp/work_group-SID/RM_2021")
path <- "C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/clusters/hss-kp/work_group-SID/RM_2021"

# Read sheet into R
# of Qs to which weight applies
rm <- read.xlsx("Vietnam_RM_2021.xlsx", sheet = "RM Functional Resp", rows = 9:114, cols = 2:16)

# -- Next 3 lines is used if the OU name is written in the tool -- #
# ---------------------------------------------------------------- #
#ou <- readxl::read_excel("CDI_RM_2021.xlsx", sheet = "RM Functional Resp", range = "B3:B3",col_names = FALSE)[[1]]
#stopwords <- c("Country: ")
#ou <- removeWords(ou,stopwords)

# Use below for cases where OU was not entered on tool
ou <- "Vietnam"



# Title file import
rm_aux <- read_csv(file = "C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/clusters/hss-kp/dm_sid/forMerge/pre_final_output/rm_aux_titles.csv",
                   trim_ws = TRUE)

# Add temp row ids
id <- rownames(rm_aux)
rm_aux <- cbind(id=id, rm_aux)
# Add temp col names
colnames(rm_aux) <- paste0("col_", 1:ncol(rm_aux))



#stopwords <- c("Country: ")
#ou <- removeWords(ou,stopwords)


# -- Read in ou and ouid dataset
# ------------------------------ #
ouid <- read_csv(file = "C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/clusters/hss-kp/dm_sid/forMerge/pre_final_output/ouid.csv")
# of Qs to which weight applies

# -- Set period & ou
period <- "2021"
date <- Sys.Date()


# Add temp row ids
id <- rownames(rm)
rm <- cbind(id=id, rm)
# Add temp col names
colnames(rm) <- paste0("col_", 1:ncol(rm))


rm_2 <- left_join(rm_aux, rm, 
                  by = "col_1", all.x = TRUE)

# Clean dataset
rm_3 <- rm_2 %>% 
  select(col_1:col_14, -c(col_3.x))

rm_4 <- rm_3 %>%
  select(col_1:col_14) %>% 
  rename(Sn = col_1, 
         Classification = col_2.x,
         Questions = col_2.y,
         SD_Host_Govt = col_3.y, 
         SD_Private_Sector = col_4, 
         SD_PEPFAR_Implementers = col_5, 
         SD_GF_Implementers = col_6, 
         NON_SD_Host_Govt = col_7, 
         NON_SD_Private_Sector = col_8, 
         NON_SD_PEPFAR_Implementers = col_9, 
         NON_SD_GF_Implementers = col_10, 
         SFP_Host_Govt = col_11, 
         SFP_Private_Sector = col_12, 
         SFP_PEPFAR = col_13, 
         SFP_GF = col_14)

rm_5 <- rm_4[-1,]
rm_6 <- rm_5[,-1]

a <- nrow(rm_6)
an <- sprintf("RMUID_%03d", 1:a)
rm_7 <- bind_cols(an, rm_6) %>% rename(UUID = "...1")


#write.csv(rm_7, file = "RM_7.csv")


rm_8 <- rm_7 %>% mutate(
  Func_Element = case_when(
    UUID == "RMUID_001" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_002" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_003" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_004" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_005" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_006" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_007" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_008" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_009" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_010" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_011" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_012" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_013" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_014" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_015" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_016" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_017" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_018" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_019" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_020" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_021" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_022" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_023" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_024" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_025" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_026" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_027" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_028" ~ "Site-Level Programs (excl. Commodities and Health Workforce)",
    UUID == "RMUID_029" ~ "Commodities",
    UUID == "RMUID_030" ~ "Commodities",
    UUID == "RMUID_031" ~ "Commodities",
    UUID == "RMUID_032" ~ "Commodities",
    UUID == "RMUID_033" ~ "Commodities",
    UUID == "RMUID_034" ~ "Commodities",
    UUID == "RMUID_035" ~ "Commodities",
    UUID == "RMUID_036" ~ "Commodities",
    UUID == "RMUID_037" ~ "Commodities",
    UUID == "RMUID_038" ~ "Commodities",
    UUID == "RMUID_039" ~ "Commodities",
    UUID == "RMUID_040" ~ "Commodities",
    UUID == "RMUID_041" ~ "Commodities",
    UUID == "RMUID_042" ~ "Commodities",
    UUID == "RMUID_043" ~ "Commodities",
    UUID == "RMUID_044" ~ "Commodities",
    UUID == "RMUID_045" ~ "Commodities",
    UUID == "RMUID_046" ~ "Commodities",
    UUID == "RMUID_047" ~ "Commodities",
    UUID == "RMUID_048" ~ "Health Workforce",
    UUID == "RMUID_049" ~ "Health Workforce",
    UUID == "RMUID_050" ~ "Health Workforce",
    UUID == "RMUID_051" ~ "Health Workforce",
    UUID == "RMUID_052" ~ "Health Workforce",
    UUID == "RMUID_053" ~ "Health Workforce",
    UUID == "RMUID_054" ~ "Health Workforce",
    UUID == "RMUID_055" ~ "Health Workforce",
    UUID == "RMUID_056" ~ "Health Workforce",
    UUID == "RMUID_057" ~ "Health Workforce",
    UUID == "RMUID_058" ~ "Health Workforce",
    UUID == "RMUID_059" ~ "Health Workforce",
    UUID == "RMUID_060" ~ "Health Workforce",
    UUID == "RMUID_061" ~ "Health Workforce",
    UUID == "RMUID_062" ~ "Health Workforce",
    UUID == "RMUID_063" ~ "Health Workforce",
    UUID == "RMUID_064" ~ "Health Workforce",
    UUID == "RMUID_065" ~ "Health Workforce",
    UUID == "RMUID_066" ~ "Health Workforce",
    UUID == "RMUID_067" ~ "Health Workforce",
    UUID == "RMUID_068" ~ "Health Workforce",
    UUID == "RMUID_069" ~ "Health Workforce",
    UUID == "RMUID_070" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_071" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_072" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_073" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_074" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_075" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_076" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_077" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_078" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_079" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_080" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_081" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_082" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_083" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_084" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_085" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_086" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_087" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_088" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_089" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_090" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_091" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_092" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_093" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_094" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_095" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_096" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_097" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_098" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_099" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_100" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_101" ~ "Above Site (Systems) Programs",
    UUID == "RMUID_102" ~ "Program Management",
    UUID == "RMUID_103" ~ "Program Management",
    UUID == "RMUID_104" ~ "Program Management"
))


rm_9 <- rm_8 %>% mutate(
  Func_Elem_Disagg_Sub = case_when(
    UUID == "RMUID_001" ~ "Func_Elem",
    UUID == "RMUID_002" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_003" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_004" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_005" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_006" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_007" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_008" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_009" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_010" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_011" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_012" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_013" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_014" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_015" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_016" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_017" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_018" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_019" ~ "Care and Treatment (excl. ARV drugs)",
    UUID == "RMUID_020" ~ "Key and Priority Populations",
    UUID == "RMUID_021" ~ "Key and Priority Populations",
    UUID == "RMUID_022" ~ "Key and Priority Populations",
    UUID == "RMUID_023" ~ "Key and Priority Populations",
    UUID == "RMUID_024" ~ "Key and Priority Populations",
    UUID == "RMUID_025" ~ "Key and Priority Populations",
    UUID == "RMUID_026" ~ "Key and Priority Populations",
    UUID == "RMUID_027" ~ "Gender-Based Violence (GBV) Programming",
    UUID == "RMUID_028" ~ "Orphans and Vulnerable Children (OVC)",
    UUID == "RMUID_029" ~ "Func_Elem",
    UUID == "RMUID_030" ~ "Antiretroviral Drugs",
    UUID == "RMUID_031" ~ "Antiretroviral Drugs",
    UUID == "RMUID_032" ~ "Antiretroviral Drugs",
    UUID == "RMUID_033" ~ "Antiretroviral Drugs",
    UUID == "RMUID_034" ~ "Antiretroviral Drugs",
    UUID == "RMUID_035" ~ "Antiretroviral Drugs",
    UUID == "RMUID_036" ~ "Antiretroviral Drugs",
    UUID == "RMUID_037" ~ "Antiretroviral Drugs",
    UUID == "RMUID_038" ~ "Medicines",
    UUID == "RMUID_039" ~ "Medicines",
    UUID == "RMUID_040" ~ "Medicines",
    UUID == "RMUID_041" ~ "Medicines",
    UUID == "RMUID_042" ~ "Laboratory",
    UUID == "RMUID_043" ~ "Laboratory",
    UUID == "RMUID_044" ~ "Laboratory",
    UUID == "RMUID_045" ~ "Laboratory",
    UUID == "RMUID_046" ~ "Health Equipment",
    UUID == "RMUID_047" ~ "PSM Costs",
    UUID == "RMUID_048" ~ "Func_Elem",
    UUID == "RMUID_049" ~ "Health Care Worker: Clinical ",
    UUID == "RMUID_050" ~ "Health Care Worker: Clinical ",
    UUID == "RMUID_051" ~ "Health Care Worker: Clinical ",
    UUID == "RMUID_052" ~ "Health Care Worker: Clinical ",
    UUID == "RMUID_053" ~ "Health Care Worker: Ancillary",
    UUID == "RMUID_054" ~ "Health Care Worker: Ancillary",
    UUID == "RMUID_055" ~ "Health Care Worker: Ancillary",
    UUID == "RMUID_056" ~ "Health Care Worker: Ancillary",
    UUID == "RMUID_057" ~ "Ancillary Staff (Site-Level)",
    UUID == "RMUID_058" ~ "Ancillary Staff (Site-Level)",
    UUID == "RMUID_059" ~ "Ancillary Staff (Site-Level)",
    UUID == "RMUID_060" ~ "Ancillary Staff (Site-Level)",
    UUID == "RMUID_061" ~ "Other Staff",
    UUID == "RMUID_062" ~ "Other Staff",
    UUID == "RMUID_063" ~ "Other Staff",
    UUID == "RMUID_064" ~ "Other Staff",
    UUID == "RMUID_065" ~ "Number of staff (Total)",
    UUID == "RMUID_066" ~ "Number of staff (Total)",
    UUID == "RMUID_067" ~ "Number of staff (Total)",
    UUID == "RMUID_068" ~ "Number of staff (Total)",
    UUID == "RMUID_069" ~ "Number of staff (Total)",
    UUID == "RMUID_070" ~ "Func_Elem",
    UUID == "RMUID_071" ~ "Human Resources for Health (HRH) Systems",
    UUID == "RMUID_072" ~ "Human Resources for Health (HRH) Systems",
    UUID == "RMUID_073" ~ "Human Resources for Health (HRH) Systems",
    UUID == "RMUID_074" ~ "Procurement and Supply Chain Management (PSCM) Systems",
    UUID == "RMUID_075" ~ "Procurement and Supply Chain Management (PSCM) Systems",
    UUID == "RMUID_076" ~ "Procurement and Supply Chain Management (PSCM) Systems",
    UUID == "RMUID_077" ~ "Procurement and Supply Chain Management (PSCM) Systems",
    UUID == "RMUID_078" ~ "Procurement and Supply Chain Management (PSCM) Systems",
    UUID == "RMUID_079" ~ "Procurement and Supply Chain Management (PSCM) Systems",
    UUID == "RMUID_080" ~ "Procurement and Supply Chain Management (PSCM) Systems",
    UUID == "RMUID_081" ~ "Procurement and Supply Chain Management (PSCM) Systems",
    UUID == "RMUID_082" ~ "Procurement and Supply Chain Management (PSCM) Systems",
    UUID == "RMUID_083" ~ "Health Management Information Systems (HMIS), Surveillance, and Research",
    UUID == "RMUID_084" ~ "Health Management Information Systems (HMIS), Surveillance, and Research",
    UUID == "RMUID_085" ~ "Health Management Information Systems (HMIS), Surveillance, and Research",
    UUID == "RMUID_086" ~ "Health Management Information Systems (HMIS), Surveillance, and Research",
    UUID == "RMUID_087" ~ "Health Management Information Systems (HMIS), Surveillance, and Research",
    UUID == "RMUID_088" ~ "Health Management Information Systems (HMIS), Surveillance, and Research",
    UUID == "RMUID_089" ~ "Laboratory Systems",
    UUID == "RMUID_090" ~ "Laboratory Systems",
    UUID == "RMUID_091" ~ "Laboratory Systems",
    UUID == "RMUID_092" ~ "Laboratory Systems",
    UUID == "RMUID_093" ~ "Laboratory Systems",
    UUID == "RMUID_094" ~ "Laboratory Systems",
    UUID == "RMUID_095" ~ "Laboratory Systems",
    UUID == "RMUID_096" ~ "Laboratory Systems",
    UUID == "RMUID_097" ~ "Health Financing",
    UUID == "RMUID_098" ~ "Governance and Policy",
    UUID == "RMUID_099" ~ "Institutional and Organizational Development",
    UUID == "RMUID_100" ~ "Site Level Quality Management",
    UUID == "RMUID_101" ~ "Other Systems Support",
    UUID == "RMUID_102" ~ "Func_Elem",
    UUID == "RMUID_103" ~ "At the Implementation Level (Implementing Partners)",
    UUID == "RMUID_104" ~ "At the Donor Level"
))



rm_10 <- rm_9 %>% mutate(
  Func_Elem_Disagg = case_when(
    UUID == "RMUID_001" ~ "Func_Elem",
    UUID == "RMUID_002" ~ "Func_Elem_Disagg",
    UUID == "RMUID_003" ~ "Clinical",
    UUID == "RMUID_004" ~ "Laboratory (e.g., Lab monitoring; OI, EID, TB, CD4, VL testing)",
    UUID == "RMUID_005" ~ "Community (e.g., Linkage, Retention, Adherence)",
    UUID == "RMUID_006" ~ "TB-HIV",
    UUID == "RMUID_007" ~ "HIV Testing Services",
    UUID == "RMUID_008" ~ "Facility-based Testing",
    UUID == "RMUID_009" ~ "Community-based Testing",
    UUID == "RMUID_010" ~ "Self-testing",
    UUID == "RMUID_011" ~ "Testing at Pregnancy-related Visits",
    UUID == "RMUID_012" ~ "Prevention",
    UUID == "RMUID_013" ~ "Community Mobilization, Behavior and Norms Change",
    UUID == "RMUID_014" ~ "Condom and Lubricant Programming",
    UUID == "RMUID_015" ~ "Prevention of Mother-To-Child Transmission (PMTCT)",
    UUID == "RMUID_016" ~ "Voluntary Medical Male Circumcision (VMMC)",
    UUID == "RMUID_017" ~ "Pre-Exposure Prophylaxis (PrEP)",
    UUID == "RMUID_018" ~ "Other Biomedical Prevention",
    UUID == "RMUID_019" ~ "Adolescent Girls and Young Women (AGYW) HIV Prevention",
    UUID == "RMUID_020" ~ "Func_Elem_Disagg",
    UUID == "RMUID_021" ~ "Female Sex Workers (FSW)",
    UUID == "RMUID_022" ~ "People Who Inject Drugs (PWID)",
    UUID == "RMUID_023" ~ "Men Who have Sex with Men (MSM) ",
    UUID == "RMUID_024" ~ "Transgender Population (TG)",
    UUID == "RMUID_025" ~ "Prison Population",
    UUID == "RMUID_026" ~ "Priority Population",
    UUID == "RMUID_027" ~ "Func_Elem_Disagg",
    UUID == "RMUID_028" ~ "Func_Elem_Disagg",
    UUID == "RMUID_029" ~ "Func_Elem",
    UUID == "RMUID_030" ~ "Func_Elem_Disagg",
    UUID == "RMUID_031" ~ "ARVs for Treatment",
    UUID == "RMUID_032" ~ "ARVs for PrEP",
    UUID == "RMUID_033" ~ "Consumables",
    UUID == "RMUID_034" ~ "Condoms and Lubricants",
    UUID == "RMUID_035" ~ "Rapid Test Kits",
    UUID == "RMUID_036" ~ "Self-testing Kits",
    UUID == "RMUID_037" ~ "Male Circumcision Kits and Supplies",
    UUID == "RMUID_038" ~ "Func_Elem_Disagg",
    UUID == "RMUID_039" ~ "Medication-Assisted Treatment (e.g., Naloxone)",
    UUID == "RMUID_040" ~ "Tuberculosis Medicines",
    UUID == "RMUID_041" ~ "Other Essential Medicines",
    UUID == "RMUID_042" ~ "Func_Elem_Disagg",
    UUID == "RMUID_043" ~ "CD4",
    UUID == "RMUID_044" ~ "Viral Load",
    UUID == "RMUID_045" ~ "Reagents and Supplies (exclusive of VL & CD4)",
    UUID == "RMUID_046" ~ "Func_Elem_Disagg",
    UUID == "RMUID_047" ~ "Func_Elem_Disagg",
    UUID == "RMUID_048" ~ "Func_Elem",
    UUID == "RMUID_049" ~ "Func_Elem_Disagg",
    UUID == "RMUID_050" ~ "Func_Elem_Disagg_Sub",
    UUID == "RMUID_051" ~ "Func_Elem_Disagg_Sub",
    UUID == "RMUID_052" ~ "Func_Elem_Disagg_Sub",
    UUID == "RMUID_053" ~ "Func_Elem_Disagg",
    UUID == "RMUID_054" ~ "AW: Salary and Benefits",
    UUID == "RMUID_055" ~ "AW: Salary Top-Ups",
    UUID == "RMUID_056" ~ "AW: Training and Supervision",
    UUID == "RMUID_057" ~ "Func_Elem_Disagg",
    UUID == "RMUID_058" ~ "AS: Salary and Benefits",
    UUID == "RMUID_059" ~ "AS: Salary Top-Ups",
    UUID == "RMUID_060" ~ "AS: Training and Supervision",
    UUID == "RMUID_061" ~ "Func_Elem_Disagg",
    UUID == "RMUID_062" ~ "OS: Salary and Benefits",
    UUID == "RMUID_063" ~ "OS: Salary Top-Ups",
    UUID == "RMUID_064" ~ "OS: Training and Supervision",
    UUID == "RMUID_065" ~ "Func_Elem_Disagg",
    UUID == "RMUID_066" ~ "nHealth Care Worker: Clinical ",
    UUID == "RMUID_067" ~ "nHealth Care Worker: Ancillary",
    UUID == "RMUID_068" ~ "nAncillary Staff (Site-Level)",
    UUID == "RMUID_069" ~ "nOther Staff",
    UUID == "RMUID_070" ~ "Func_Elem",
    UUID == "RMUID_071" ~ "Func_Elem_Disagg",
    UUID == "RMUID_072" ~ "Pre-Service Training",
    UUID == "RMUID_073" ~ "In-Service Training/Continuing Medical Education Systems",
    UUID == "RMUID_074" ~ "Func_Elem_Disagg",
    UUID == "RMUID_075" ~ "Forecasting and Planning",
    UUID == "RMUID_076" ~ "Sourcing and Procurement",
    UUID == "RMUID_077" ~ "Quality Assurance and Control",
    UUID == "RMUID_078" ~ "Risk Management",
    UUID == "RMUID_079" ~ "Logistics Management",
    UUID == "RMUID_080" ~ "Warehousing and Inventory Management",
    UUID == "RMUID_081" ~ "Transport and Distribution",
    UUID == "RMUID_082" ~ "Waste Management and Return",
    UUID == "RMUID_083" ~ "Func_Elem_Disagg",
    UUID == "RMUID_084" ~ "Data Systems",
    UUID == "RMUID_085" ~ "Monitoring and Evaluation",
    UUID == "RMUID_086" ~ "Surveys and Surveillance",
    UUID == "RMUID_087" ~ "HIV Population-based survey (e.g., PHIA)",
    UUID == "RMUID_088" ~ "KP Demographic Surveys (e.g., IBBS)",
    UUID == "RMUID_089" ~ "Func_Elem_Disagg",
    UUID == "RMUID_090" ~ "Conventional and Point of Care Instruments",
    UUID == "RMUID_091" ~ "Laboratory Infrastructure and Equipment",
    UUID == "RMUID_092" ~ "Laboratory Information System",
    UUID == "RMUID_093" ~ "Procurement",
    UUID == "RMUID_094" ~ "Quality Management Systems and Accreditation",
    UUID == "RMUID_095" ~ "Logistics Management",
    UUID == "RMUID_096" ~ "Sample Transport System",
    UUID == "RMUID_097" ~ "Func_Elem_Disagg",
    UUID == "RMUID_098" ~ "Func_Elem_Disagg",
    UUID == "RMUID_099" ~ "Func_Elem_Disagg",
    UUID == "RMUID_100" ~ "Func_Elem_Disagg",
    UUID == "RMUID_101" ~ "Func_Elem_Disagg",
    UUID == "RMUID_102" ~ "Func_Elem",
    UUID == "RMUID_103" ~ "Func_Elem_Disagg",
    UUID == "RMUID_104" ~ "Func_Elem_Disagg"
  )) %>% 
  dplyr::mutate(operatingunit = ou)


rm_11 <- dplyr::left_join(rm_10, ouid, by = "operatingunit")

rm_12 <- rm_11 %>% select(
  operatingunit, operatingunituid, Func_Element, Func_Elem_Disagg, Func_Elem_Disagg_Sub,	
  SD_Host_Govt, SD_Private_Sector, SD_PEPFAR_Implementers, SD_GF_Implementers, 
  NON_SD_Host_Govt, NON_SD_Private_Sector, NON_SD_PEPFAR_Implementers, 
  NON_SD_GF_Implementers, SFP_Host_Govt, SFP_Private_Sector, SFP_PEPFAR, SFP_GF)

rm_12[is.na(rm_12)] <- ""

setwd("C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/clusters/hss-kp/dm_sid/forMerge/pre_final_output/dataset_rm")
openxlsx::write.xlsx(rm_12, file = paste("RM_",ou, "_", period,"_",date, "_Time_1-03PM.xlsx", sep = ""), keepNA = T, asTable = TRUE)




