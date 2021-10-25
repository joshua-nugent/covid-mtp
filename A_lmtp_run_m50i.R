library(tidyverse)
library(lubridate)
library(data.table)
library(sl3)
`%nin%` <- negate(`%in%`)

load("2021-01-25_covid_wk.RData")
load("11_29_screened_covars.RData")

expo <- "m50_index"
the_date <- "2021_02_19"
outcome_lead <- "2wks"
WY_lag_or_current <- "lag1"
outC <- paste0("new_cases_",outcome_lead,"_ahead_percap")
weeksC <- 21:46

unad <- F
if(WY_lag_or_current == "lag1"){
  case_covars <- c(case_covars, "new_cases_1wks_behind_percap")  
} else if(WY_lag_or_current == "current"){
  case_covars <- c(case_covars, "new_cases_this_week_percap")
} else{
  print("covariate adjustment set does not include lagged Y variable.")
}


if(unad == F){
  source("A_lmtp.R")
  case_file_name <- paste0(the_date, "_unadj=",unad,"_leadY=",outcome_lead,"_","WY_lag=",WY_lag_or_current,"_", expo, ".rds")
} else if (unad == T){
  source("A_lmtp_unadj.R")
  case_file_name <- paste0(the_date, "_unadj=",unad,"_leadY=",outcome_lead,"_","WY_lag=",WY_lag_or_current,"_", expo, ".rds")
}

dfwk <- dfwk %>% filter(population > 40000)

CASES <- analysis_wrapper(covars = case_covars,
                          data = dfwk, 
                          expo = expo, 
                          outcome = outC,
                          weeks = weeksC,
                          pop_cutoff = 0, 
                          shifts = c(.9, .85), shift_type = "mult",
                          shift_lower_bound = 0, shift_upper_bound = 500,
                          outcome_lower_bound = 0, outcome_upper_bound = 99999)

# saveRDS(object = CASES,
#         file = case_file_name)

















