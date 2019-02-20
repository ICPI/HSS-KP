library(readxl)
library(tidyverse)

### first prep the MSD ouxim file, most current version.  Read from your corresponding folder

ouxim <- ICPIutilities::read_msd("C:/Users/wvp3/Desktop/Large data files/MER_Structured_Dataset_OU_IM_FY17-18_20181221_v2_1.txt",to_lower=FALSE)

kp.df.long <- ouxim %>%
  # filter for KP-specific indicators and disaggs
  select(Region:isMCAD,ends_with("TARGETS"),ends_with("APR")) %>%
  filter(indicator %in% c("KP_PREV","HTS_TST_POS","TX_NEW","HTS_TST","HTS_TST_NEG","PrEP_NEW","KP_MAT")) %>%
  filter(indicator=="KP_PREV" | otherDisaggregate %in%
           c("MSM","TG","FSW","PWID","People in prisons and other enclosed settings", 
             "Other Key Populations") | 
           standardizedDisaggregate=="Total Numerator" |
           standardizedDisaggregate=="Total Denominator") %>%
  
  # transpose to long format, removing NA values
  gather("fiscalperiod","value",starts_with("FY20")) %>%
  filter(!is.na(value)) %>%
  # remove the double counting of KP_PREV results which has two disaggregates.
  # (KP_PREV targets only have one disagg.)
  filter(!(indicator=="KP_PREV" & grepl("APR",fiscalperiod) & disaggregate=="KeyPop")) %>%

  # assign broad KP group - kp group irrespective of indicator/disagg
  mutate(kpgroup = case_when(
                             grepl("MSM",otherDisaggregate) ~ "MSM",
                             grepl("FSW",otherDisaggregate) ~ "FSW",
                             grepl("TG",otherDisaggregate) ~ "TG",
                             grepl("PWID",otherDisaggregate) ~ "PWID",
                             grepl("Total Numerator",standardizedDisaggregate) ~ "Total Numerator",
                             grepl("Prisoners",otherDisaggregate) ~ "Prisoners",
                             indicator=="KP_MAT" ~ "PWID",
                             TRUE ~ NA_character_
))

kp.ou.long <- kp.df.long %>%
  # rolls up TA+DSD, IM, MSM SW+non SW, TG SW + non SW, male vs female PWID 
  group_by(CountryName,indicator,numeratorDenom,kpgroup,fiscalperiod) %>%
  summarise(value=sum(value, na.rm=T))





#########################
# Next, loop for all DataPack files
# IMPORTANT: you must save all DataPacks as .xlsx format first to use the readxl package (read_excel function)
# the .xlsb format and the excel.link package can encounter difficulty with datapack structure

# you must setwd() to the folder containing all datapacks saved as .xlsx
# you must remove all other .xlsx files from that directory
# close any open DataPacks

library(readxl)

filenames = dir(pattern="*.xlsx")

files <- (filenames)


# this is a basic loop, you may also use lapply() , purrr::map(), or similar
for( i in 1:length(filenames) )
  
{
  # temp is a df from the KP tab of the datapack
  temp <- read_excel(files[i], sheet = "KP", skip = 4)
  
  # these are the HTS_TST and HTS_TST_POS targets from the HTS tab
  hts <- read_excel(files[i], sheet = "HTS", skip=4) %>% select(HTS_TST.N.pos,HTS_TST.N.total) %>%
    summarise_all(funs(sum),na.rm=T) 
  
  hts_pos <- as.numeric(hts[1])
  
  hts <- as.numeric(hts[2])
  
  # this is the TX_NEW target from the TX tab
  tx <- read_excel(files[i], sheet = "TX", skip=4) %>% select(`TX_NEW.N.Age/Sex/HIVStatus.20T`) %>%
    summarise_all(funs(sum),na.rm=T) %>% as.numeric()
  
  # this is the Country name from the home tab
  country <- as.character(read_excel(files[i], sheet = "Home", range ="B20",col_types="text",col_names=FALSE))
  
  temp$`CountryName` <- country
  

  long <- temp %>%
    mutate_if(is.logical,is.numeric) %>%
    dplyr::select(CountryName,PSNU:`PrEP_NEW.N.KeyPop.20T`) %>%

    # Need to calculate HTS_TST = HTS_TST_POS column + HTS_TST_NEG column
    mutate(`HTS_TST.N.KeyPop/Result.20T.Total_Numerator`=`HTS_TST.N.KeyPop/Result.20T.Positive` + `HTS_TST.N.KeyPop/Result.20T.Negative`) %>%

    # transpose df to long by indicator
    gather("header","value",`KP_ESTIMATES.N.KeyPop/TotalSizeEstimate.20T`:`HTS_TST.N.KeyPop/Result.20T.Total_Numerator`) %>%
    filter(!is.na(value)) %>%
    
    # standardize indicator names from prefix
    mutate(indicator=stringr::word(header,sep=fixed("."))) %>%
    mutate(indicator=ifelse(indicator=="HTS_TST" & grepl("Positive",header),
                            "HTS_TST_POS",indicator)) %>%
    mutate(indicator=ifelse(indicator=="HTS_TST" & grepl("Negative",header),
                            "HTS_TST_NEG",indicator)) %>%
    
    # drop FY19 targets
    filter(!grepl(".19T",header)) %>%
    mutate(fiscalperiod="FY2020_TARGETS") %>%
    mutate(kpgroup = case_when(
      grepl("MSM",KeyPop) ~ "MSM",
      grepl("FSW",KeyPop) ~ "FSW",
      grepl("TG",KeyPop) ~ "TG",
      grepl("PWID",KeyPop) ~ "PWID",
      grepl("prison",KeyPop) ~ "Prisoners",
      TRUE ~ NA_character_
    )) %>%
    group_by(CountryName,indicator,kpgroup,fiscalperiod) %>%
    summarise(value=sum(value, na.rm=T))
  
  # add all the Total Numerator targets from the other tabs
  long <- bind_rows(long,data.frame(CountryName=country,indicator="HTS_TST",kpgroup="Total Numerator",
                                    fiscalperiod="FY2020_TARGETS",value=hts)) 
  
  long <- bind_rows(long,data.frame(CountryName=country,indicator="HTS_TST_POS",kpgroup="Total Numerator",
                                    fiscalperiod="FY2020_TARGETS",value=hts_pos)) 
  
  long <- bind_rows(long,data.frame(CountryName=country,indicator="TX_NEW",kpgroup="Total Numerator",
                                    fiscalperiod="FY2020_TARGETS",value=tx)) 
  
  ## append 
  ifelse(i<=1,
         # if first country file in directory
         agg <- long,
         # else additional country files, append
         agg <- dplyr::bind_rows(agg,long))
  
  # end loop
  
  remove(temp)
  remove(tx)
  remove(hts)
  remove(hts_pos)
  remove(country)
  
}

# Append the datapack targets to the processed MSD

trend <- dplyr::bind_rows(kp.ou.long,agg)

write.csv(trend,file="checktrend.csv",row.names=F)
