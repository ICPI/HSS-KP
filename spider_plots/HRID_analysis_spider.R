library(tidyverse)
library(reshape2)
library(ICPIutilities)
library(readxl)
library(plotly)

hrid <- read_excel("~/ICPI/HRH TWG/South Africa 2018apr/HRID_FactViewStructure_FY117Q2-FY18Q4_20181115.xlsx", 
                                           col_types = c("text", "text", "text", 
                                                         "text", "text", "text", "text", "text", 
                                                         "numeric", "text", "text", "numeric", 
                                                         "text", "text", "text", "text", "text", 
                                                         "text", "text", "text", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric"))


## we are dropping cost - to - company (CTC) under "indicator" and only keep FTE 
## also summarising (rolling up) to the site level, with cadre FTEs as data columns
hrid_fte <- hrid %>%
  gather("fiscalperiod","value",starts_with("FY20")) %>%
  dcast(... ~ indicator,value.var="value",fun.aggregate=sum) %>%
  
  # filter out HRH hired only for partner internal ops
  filter(!grepl("Partner management and operations",otherDisaggregate,ignore.case=TRUE)) %>%
  
  # aggregate up across "other disagg" (to ignore "based at"; for now, ignore mechanism/partner)
  # also for now roll up above Community because HRID uses subdistrict this causes duplication 
  # in the merge
  group_by(PSNU, Facility, categoryOptionComboName, fiscalperiod) %>%
  summarise(HRH_HRID_FTE=sum(HRH_HRID_FTE, na.rm=TRUE)) 

# Transpose structure to pivot cadre categories into the columns
cadre_fte <- hrid_fte %>%
  ungroup() %>%
  dcast(... ~ categoryOptionComboName,value.var="HRH_HRID_FTE",fun.aggregate=sum) %>%
  rowwise() %>%
  # calculate what percent of FTE are clincial (out of total FTE supported)
  mutate(pctclinical = Clinical / (Clinical + `Clinical Support` + 
                                     Management + Lay + Other + `Social Service`)) %>%
  #optional: change fiscal period to lower case (default: upper case, same as Genie extract)
  #mutate(fiscalperiod=stringr::str_to_lower(fiscalperiod)) %>%
  mutate(pctclinical = ifelse(is.nan(pctclinical) & Clinical==0,0,pctclinical))
                              
## see the code in MSD-for-HRID_analysis.R file to create the sitexim_collapse dataset


# Next in order to estimate the annual FTE, we take the average of FTE across four quarters, by site
# to do that we define fiscal years and average four quarters in the year
annual_cadre_fte <- cadre_fte %>%
  mutate(fiscalyear=str_sub(fiscalperiod,3,6)) %>%
  group_by(PSNU,Facility,fiscalyear) %>%
  summarise_at(vars(Clinical:`Social Service`),funs(mean=mean(.)),na.rm=TRUE) %>%
  ungroup() %>%
  mutate(fiscalperiod = paste0("FY",fiscalyear,"APR"))
 
# Merge in the MER data by facility name, see code MSD-for-HRID_analysis.R script
hrid_mer <- left_join(sitexim_tomerge,annual_cadre_fte,by=c("PSNU","Facility","fiscalperiod"))

# Use text and string matching to identify site type based on site name
hrid_mer_site <- hrid_mer %>%
  mutate(sitetype = case_when(grepl("Clinic",Facility,ignore.case=TRUE) ~ "Clinic",
                            grepl("Hospital",Facility,ignore.case=TRUE) ~ "Hospital",
                            grepl("Health Centre",Facility,ignore.case=TRUE) ~ "Health Centre",
                            grepl("Health Center",Facility,ignore.case=TRUE) ~ "Health Centre",
                            grepl("CHC",Facility,ignore.case=TRUE) ~ "Health Centre",
                            grepl("CDC",Facility,ignore.case=TRUE) ~ "Health Centre",
                            grepl("dispensary",Facility,ignore.case=TRUE) ~ "Dispensary/pharmacy",
                            grepl("pharmacy",Facility,ignore.case=TRUE) ~ "Dispensary/pharmacy",
                            grepl("Maternity",Facility,ignore.case=TRUE) ~ "Maternity clinic/hospital",
                            grepl("Mobile",Facility,ignore.case=TRUE) ~ "Mobile",
                            grepl("CC",Facility,ignore.case=FALSE) ~ "Correctional centre",
                            grepl("Med [ABC]",Facility,ignore.case=FALSE) ~ "Correctional centre",
                            grepl("Max",Facility,ignore.case=FALSE) ~ "Correctional centre",
                            grepl("Correctional",Facility,ignore.case=TRUE) ~ "Correctional centre",
                            grepl("Private",Facility,ignore.case=TRUE) ~ "Private practice"
)) %>%
  mutate(sitetype=ifelse(is.na(sitetype),"Other",sitetype)) %>%
  # mutate(totalfte=Clinical + `Clinical Support` + 
  #        Management + Lay + Other + `Social Service`) %>%
  arrange(Facility,fiscalperiod,performance) 
# to do: need to remove N/A or NA or blank or above-site or community from site type

# create a temporary output file that can be explored and checked in Excel
write.csv(hrid_mer_site,file="hrid_mer_merged.csv",row.names=FALSE)

# Roll up again by summing the FTEs within each site performance group (poor, middle, high)
# this step prepares the dataframe to be used by the plotly package, which has a 
# radar plot.  Other packages may require different data structures than this.
hrhspidertop10 <- hrid_mer_site %>%
  select(fiscalperiod,Clinical_mean:`Social Service_mean`,performance,sitetype) %>%
  group_by(fiscalperiod,performance) %>%
  summarise_at(vars(Clinical_mean,`Clinical Support_mean`,Lay_mean,
                    Management_mean,Other_mean,`Social Service_mean`),funs(sum),na.rm=TRUE) %>%
  gather("cadre","fte",Clinical_mean:`Social Service_mean`) %>%
  arrange(fiscalperiod,performance,cadre)

#also write this to a csv to be able to check with pivots
hrid_mer_site %>% gather("cadre","fte",Clinical_mean:`Social Service_mean`) %>%
  write.csv(file="checkhridmer.csv",row.names=F)
  
# create subset datasets to be able to plot high, middle and poor groups of sites separately
hrhspidertop10 %>% filter(performance=="high") -> high
hrhspidertop10 %>% filter(performance=="middle") -> middle
hrhspidertop10 %>% filter(performance=="poor") -> poor

# create the spider radar plots in plotly following examples at: https://plot.ly/r/radar-chart/
p_high <- plot_ly(
  high,
  type = 'scatterpolar',
  r = high$fte,
  theta = high$cadre,
  fill = 'toself',
  frame = high$fiscalperiod,
  name = 'top 10% NET NEW',
  mode='markers'
) %>%
  
  animation_opts(2700) %>%
  layout(bgcolor='green',
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,max(high$fte))
      )
    )
  )

p_low <- plot_ly(
  poor,
  type = 'scatterpolar',
  r = poor$fte,
  theta = poor$cadre,
  fill = 'toself',
  frame = poor$fiscalperiod,
  name='bottom 10% NET NEW',
  mode='markers'
) %>%
  
  animation_opts(2700) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,max(poor$fte))
      )
    )
  )

p_med <- plot_ly(
  middle,
  type = 'scatterpolar',
  r = middle$fte,
  theta = middle$cadre,
  fill = 'toself',
  frame = middle$fiscalperiod,
  name= 'middle (10th to 90th pctile)',
  mode='markers'
) %>%
  
  animation_opts(2700) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,max(middle$fte))
      )
    )
  )

# display plots separately
print(p_low)
print(p_med)
print(p_high)

# overlay plots
subplot(p_low,p_high,p_med)

# in order to show plots spearately on the same html output file, please see the R Markdown (.rmd) code file.


