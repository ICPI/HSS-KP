library(readr)
library(tidyverse)
library(reshape2)


sitexim <- read_delim("C:/Users/wvp3/Desktop/Large data files/PEPFAR-Data_Genie-SiteByIMs-RSA-2018-11-20.txt", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)

# clean and restructure data - keep only 27 PEPFAR scale-up districts, and transpose long by period
sitexim_collapse <- sitexim %>%
  filter(grepl("Scale-Up",SNUPrioritization,ignore.case=TRUE)) %>%
  gather("fiscalperiod","value",starts_with("FY20")) %>%
  filter(!is.na(value)) %>%
  group_by(PSNU, Community, Facility, indicator,fiscalperiod) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>%
  ungroup() 
  # This is for multiple indicators

# calculate TX_NET_NEW and use it to classify sites as poor vs. high performers
sitexim_netnew <- sitexim_collapse %>%
  filter(indicator=="TX_CURR" & grepl("APR",fiscalperiod)) %>%
  dcast(... ~ fiscalperiod, fun.aggregate=sum, value.var="value") %>%
  # Consider using only sites that reported treatment during both periods
  # otherwise analysis may be influenced by new PEPFAR sites that report TX_CURR for the first time
  # or sites where PEPFAR no longer works
  mutate(TX_NET_NEW = ifelse(FY2017APR > 0 & FY2018APR > 0, FY2018APR - FY2017APR, NA)) %>%
  select(-indicator) 

# Check the cutoff levels for top 10% and bottom 10% of sites by 
# in this case we drop extreme outliers from the calculation first.
sitexim_netnew %>%
  filter(TX_NET_NEW>-1000 & TX_NET_NEW <1000) -> netnew

ggplot(netnew,aes(x=TX_NET_NEW)) +
  geom_density()

quantile(netnew$TX_NET_NEW, probs = c(0,.1,.9,1))
# 0%      10%    90%   100% 
# Results redacted

# define the cutoffs
sitexim_netnew <- sitexim_netnew %>% 
  mutate(performance = case_when(
    TX_NET_NEW <  ######## insert actual numbers here from quantile analysis above #####
       ~ "poor",
    TX_NET_NEW >= ######## insert actual numbers here from quantile analysis #####
       ~ "high",
    is.na(TX_NET_NEW) ~ NA_character_,
    TRUE ~ "middle"
  ))

sitexim_tomerge <- sitexim_netnew %>%
  mutate(FY2017APR=TX_NET_NEW,FY2018APR=TX_NET_NEW) %>%
  select(-TX_NET_NEW) %>%
  gather("fiscalperiod","TX_NET_NEW",starts_with("FY20"))
  
