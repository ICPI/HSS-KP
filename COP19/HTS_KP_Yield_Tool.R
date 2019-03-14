
library(tidyverse)
library(ICPIutilities)

#df_OU <- ICPIutilities::read_msd("MER_Structured_Dataset_OU_IM_FY17-19_20190215_v1_2", to_lower = FALSE)

folderpath <- "~/MER Data Pulls/FY19Q1"
df_OU <- read_rds(file.path(folderpath,"MER_Structured_Dataset_OU_IM_FY17-19_20190215_v1_2.rds"))


kp.df.long <- df_OU %>%
  # filter for only testing indicators and KP & modality disaggregates
  select(OperatingUnit:modality, FY2018APR, FY2019Q1) %>%
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS") &  numeratorDenom == "N") %>%
  filter(standardizedDisaggregate %in% c("Modality/Age/Sex/Result", "KeyPop/Result")) %>%
  # transpose to long format, remove NA values
  gather("period", "value", starts_with("FY20")) %>%
  filter(!is.na(value) & value != 0) %>%
  
  # renaming KP groups, modalities and new regions
  mutate(kpgroup = case_when(
    otherDisaggregate == "Newly Identified" ~ NA_character_,
    grepl("MSM",otherDisaggregate) ~ "MSM",
    grepl("FSW",otherDisaggregate) ~ "FSW",
    grepl("TG",otherDisaggregate) ~ "TG",
    grepl("PWID",otherDisaggregate) ~ "PWID",
    grepl("prisons",otherDisaggregate) ~ "Prisoners")) %>%
  mutate(TestingModality = case_when(
    modality == "Emergency Ward" ~ "Emergency_Facility", 
    modality == "Index" ~ "Index_Facility", 
    modality == "IndexMod" ~ "Index_Community", 
    modality == "Inpat" ~ "Inpat_Facility", 
    modality == "Malnutrition" ~ "Malnutrition_Facility", 
    modality == "MobileMod" ~ "Mobile_Community", 
    modality == "OtherMod" ~ "Other_Community", 
    modality == "OtherPITC" ~ "OtherPITC_Facility",
    modality == "Pediatric" ~ "Peds_Facility", 
    modality == "PMTCT ANC" ~ "PMTCT_Facility", 
    modality == "Post ANC1" ~ "PostANC1_Facility", 
    modality == "STI Clinic" ~ "STI_Facility", 
    modality == "TBClinic" ~ "TB_Facility",
    modality == "VCT" ~ "VCT_Facility", 
    modality == "VCTMod" ~ "VCT_Community", 
    modality == "VMMC" ~ "VMMC_Facility")) %>%
  mutate(Region = case_when(
    OperatingUnit == "Asia Regional Program" ~ "Asia Region",
    OperatingUnit == "Burma" ~ "Asia Region",
    OperatingUnit == "Cambodia" ~ "Asia Region",
    OperatingUnit == "Central Asia Region" ~ "Asia Region",
    OperatingUnit == "India" ~ "Asia Region",
    OperatingUnit == "Indonesia" ~ "Asia Region",
    OperatingUnit == "China" ~ "Asia Region",
    OperatingUnit == "Papua New Guinea" ~ "Asia Region",
    OperatingUnit == "Thailand" ~ "Asia Region",
    OperatingUnit == "Caribbean Region" ~ "Western Hemisphere",
    OperatingUnit == "Central America Region" ~ "Western Hemisphere",
    OperatingUnit == "Ghana" ~ "West Africa Region"
  ))


# adding column indicating if mechanism reports KP data
kp_mechanisms <- kp.df.long %>% 
  count(MechanismID, kpgroup) %>%
  filter(!is.na(kpgroup) & !is.na(n)) %>%
  distinct(MechanismID)
kp.df.long <- kp.df.long %>%
  mutate(KP_mech = case_when(
    MechanismID %in% kp_mechanisms$MechanismID ~ "KP_mechanism",
    TRUE~NA_character_
  ))

# roll up TA+DSD to OU-level
kp.df.ou <- kp.df.long %>%
  group_by(Region, CountryName, PrimePartner, FundingAgency, MechanismID, ImplementingMechanismName,
         indicator, kpgroup, TestingModality, KP_mech, period) %>%
  summarise(value=sum(value))

kp.df.wide <- spread(kp.df.ou, indicator, value)

write.csv(kp.df.wide,file="KP_Casefinding.csv",row.names=F)
