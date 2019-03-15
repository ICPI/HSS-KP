# write a function to standardize KP groups across indicators/disaggs, and to calculate the general population
library(ICPIutilities)
library(tidyverse)

# sitexim <- readRDS("C:/Users/wvp3/Desktop/Large data files/MER_Structured_Dataset_SITE_IM_FY17-19_20190215_v1_1_Tanzania.RDS")
# sitexim <- ICPIutilities::read_msd("C:/Users/wvp3/Desktop/Large data files/MER_Structured_Dataset_Site_IM_FY17-19_20190215_v1_2_India.txt",to_lower=F)

genpop <- function(df) {
  ## Function only takes a data.frame
  if(is.data.frame(df)) {
    
    ## code is written to take MSD columns as named in native dataset.  read_msd(df,to_lower=F) only
    if(any(grepl("[[:upper:]]",names(df)))) {
      
    require(tidyverse)
    kp.group.df <- df %>%
      ## subset indicators and disaggs related to KP ##
      filter(indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW","KP_PREV","KP_MAT","PrEP_NEW","PrEP_CURR")) %>%
      filter(grepl("KeyPop",disaggregate,ignore.case=T) | standardizedDisaggregate=="Total Numerator") %>%
      filter(numeratorDenom=="N") %>%
      
      ## Standardize KP groups ##
      mutate(kpgroup = case_when(
        grepl("MSM",categoryOptionComboName) ~ "MSM",
        grepl("FSW",categoryOptionComboName) ~ "FSW",
        grepl("TG",categoryOptionComboName) ~ "TG",
        grepl("PWID",categoryOptionComboName) ~ "PWID",
        grepl("prison",categoryOptionComboName,ignore.case=T) ~ "Prisoners",
        indicator=="KP_MAT" ~ "PWID",
        TRUE ~ NA_character_
      )) %>%
      
      gather("fiscalperiod","value",starts_with("FY20")) %>%
      # create a temporary variable keypopgenpop to be used to calculate estimated general population
      mutate(keypopgenpop = case_when(
        standardizedDisaggregate=="Total Numerator" ~ "Total Numerator",
        grepl("KeyPop",disaggregate,ignore.case=T) ~ "Key Pop"
      )) 
      
      ## calculate the values for general population / unattributable risk
    genpop.df <- kp.group.df %>%
      # KP_PREV and KP_MAT are not eligible for general population calculations, as all results are KP by definition.
      filter(indicator!="KP_PREV" & indicator!="KP_MAT") %>%
      ungroup() %>%
      
      # need to aggregate the kp sub-groups within disagg options before transposing to wide
      # this group_by / group_by_at is critical - selecting the wrong grouping variables can cause issues with re-aggregating later
      # using group_by_at and deselecting variables allows for multiple types of MSD (ouxIM, sitexIM)
      group_by_at(vars(-value,-standardizedDisaggregate,-disaggregate,-categoryOptionComboName, -ImplementingMechanismName,-otherDisaggregate, -kpgroup,-resultStatus)) %>%
      summarise(value=sum(value,na.rm=T)) %>%
      
      tidyr::spread(keypopgenpop, value) %>%
      rowwise() %>%
      mutate(genpop = sum(`Total Numerator`, -`Key Pop`,na.rm=TRUE)) %>%
      
      # in unusual cases where sum of KP disaggs > Total Numerator, set General population -> 0
      # Be careful with handling dedup here, which will also be negative
      mutate(genpop = ifelse(
                             (genpop<0 & !grepl("Dedup",PrimePartner,ignore.case=T)), 0,
                              genpop)) %>%
      
      # fix column names
      select(-`Total Numerator`,-`Key Pop`) %>%
      dplyr::mutate(kpgroup="General Population/Unidentified Risk",
                    otherDisaggregate="General Population/Unidentified Risk") %>%
      dplyr::rename(value=genpop) %>%
      filter(!is.na(value)) %>%
      ungroup()
      
      ## bind the genpop rows back onto original data frame ##
      kp.group.df <- select(kp.group.df,-keypopgenpop)
      gp <- dplyr::bind_rows(kp.group.df,genpop.df) 
      return(gp)
    }
  
  else(warning("You may have converted variable names to lower case.  Try ICPIUtilities::read_msd(df,to_lower=FALSE)"))
  }
  else(warning("Not a data frame"))
}
