library(tidyverse)
library(lubridate)
library(data.table)
library(SuperLearner)
`%nin%` <- negate(`%in%`)

load("2021-01-25_covid_wk.RData")
dfwk <- dfwk %>% filter(population > 40000)
source("A_screen_covariates.R")
source("A_lmtp_screener.R")
expo <- "dex_a"
the_date <- "as_2021_10_23"
outcome_lead <- "2wks"
WY_lag_or_current <- "lag1"
outC <- paste0("new_cases_",outcome_lead,"_ahead_percap")
weeksC <- 22:46
case_file_name <- paste0(the_date, "_unadj=",F,"_leadY=",outcome_lead,"_","WY_lag=",WY_lag_or_current,"_", expo, ".rds")
superset <- static_covars[static_covars %nin% c("meatpack_cat5","meatpack_cat4",
                                      "ppl_below_poverty", "prison", "log_pop", "pop_dens", "population")]
max_covars <- 3
shifts <- c(.6, .5, .4)

for(i in 1){
  Y_cor <- I(screen_covars_cor(data = dfwk, wk = weeksC[i], outcome = outC, static_covars = superset)$Row.names)
  A_cor <- screen_covars_cor(data = dfwk, wk = weeksC[i], outcome = expo, static_covars = superset)$Row.names
  A_cor <- A_cor[A_cor %in% Y_cor]
  case_covars <- c(A_cor, Y_cor[Y_cor != A_cor])
  if(length(case_covars) > 0){
    if(length(case_covars >= max_covars)){
      finalset <- case_covars[1:max_covars]
    } else {finalset <- case_covars[1:length(case_covars)]}} else {finalset <- NULL}
  #print(finalset)
  CASES <- analysis_wrapper(covars = c(finalset,"new_cases_1wks_behind_percap"),
                          data = dfwk,
                          expo = expo,
                          outcome = outC,
                          weeks = weeksC[i],
                          pop_cutoff = 0,
                          shifts = shifts, shift_type = "mult",
                          shift_lower_bound = 0, shift_upper_bound = 9999,
                          outcome_lower_bound = 0, outcome_upper_bound = 99999)
}

for(i in 2:length(weeksC)){
  Y_cor <- I(screen_covars_cor(data = dfwk, wk = weeksC[i], outcome = outC, static_covars = superset)$Row.names)
  A_cor <- screen_covars_cor(data = dfwk, wk = weeksC[i], outcome = expo, static_covars = superset)$Row.names
  A_cor <- A_cor[A_cor %in% Y_cor]
  case_covars <- c(A_cor, Y_cor[Y_cor != A_cor])
  if(length(case_covars) > 0){
    if(length(case_covars >= max_covars)){
      finalset <- case_covars[1:max_covars]
    } else {finalset <- case_covars[1:length(case_covars)]}} else {finalset <- NULL}
  #print(finalset)
  CASES <- rbind.data.frame(CASES,
                            analysis_wrapper(covars = c(finalset,"new_cases_1wks_behind_percap"),
                                             data = dfwk,
                                             expo = expo,
                                             outcome = outC,
                                             weeks = weeksC[i],
                                             pop_cutoff = 0,
                                             shifts = shifts, shift_type = "mult",
                                             shift_lower_bound = 0, shift_upper_bound = 9999,
                                             outcome_lower_bound = 0, outcome_upper_bound = 99999))
}

saveRDS(object = CASES,
        file = case_file_name)









