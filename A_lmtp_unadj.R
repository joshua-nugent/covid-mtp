library(tidyverse)
library(lubridate)
library(data.table)
library(sl3)
library(delayed)
library(future)
library(lmtp)
library(progressr)
`%nin%` <- negate(`%in%`)

#source("A_screen_covariates.R")

lmtp_analysis <- function(
  the_data = NA,
  wk = 30,
  shift_amount = -40,
  shift_lower_bound = 0,
  shift_upper_bound = 99999,
  outcome_lower_bound = 0,
  outcome_upper_bound = 99999,
  exposure = "dex_a",
  outcome = "new_deaths_4wks_ahead_percap",
  screen_method = c("lasso", "cor", "rf", "none"),
  predetermined_covars = NULL,
  num_covars = NA, # lasso and rf only
  screen_cor_p_cutoff = .05, # cor only
  screen_cor_r_cutoff = .05, # cor only
  screen_cor_trim_hi = 1, # cor only
  screen_cor_trim_lo = 0, # cor only
  cores = 1,
  sts_abb = c("CA", "OR", "WA",
              "NV", "CO", "AZ", "NM", "UT",
              "ID", "MT", "WY", "ND", "SD",
              "RI", "MA", "CT", "ME", "NH", "VT",
              "IL", "IN",
              "NY", "NJ",
              "WV", "AR", "KY", "TN",
              "LA", "TX", "MS", "AL", "FL",
              "MI", "WI", "OH", "MN",
              "MO", "IA", "NE", "OK", "KS",
              "PA", "DE", "MD",
              "VA", "NC", "SC", "GA", "AK", "HI", "DC"),
  ntrees_gq = 1000,
  shift_type = c("mult", "add")){
  
  start_time <- Sys.time()
  
  ################################ Not needed for unadjusted analysis
  # # For outcome
  # #sl3_list_learners(properties = "continuous")
  # l_q <- 
  #   make_learner_stack(
  #     Lrnr_mean,
  #     list(Lrnr_xgboost, max_depth = 7),
  #     Lrnr_glm,
  #     list(Lrnr_ranger, num.trees = ntrees_gq, mtry = 5),
  #     Lrnr_gam#,list(Lrnr_dbarts, ntree = ntrees_gq)
  #   )
  # # For classification density
  # #sl3_list_learners(properties = "binomial")
  # # perhaps add more nuance like:
  # l_g <-
  #   make_learner_stack(
  #     Lrnr_mean,
  #     list(Lrnr_xgboost, max_depth = 4, eta = .25),
  #     Lrnr_glm,
  #     list(Lrnr_ranger, num.trees = 500),
  #     Lrnr_gam
  #   )
  
  if(shift_type == "add"){
    shift <- function(data, trt){
      dd <- data[[trt]] + shift_amount
      dd[dd < shift_lower_bound] <- shift_lower_bound
      dd[dd > shift_upper_bound] <- shift_upper_bound
      return(dd)
    }
  }
  if(shift_type == "mult"){
    shift <- function(data, trt){
      dd <- data[[trt]] * shift_amount
      dd[dd < shift_lower_bound] <- shift_lower_bound
      dd[dd > shift_upper_bound] <- shift_upper_bound
      return(dd)
    }
  }

  if(screen_method == "none"){
    Wvars <- predetermined_covars
    screen_cor_p_cutoff <- NA
    screen_cor_r_cutoff <- NA
    screen_cor_trim_hi <- NA
    screen_cor_trim_lo <- NA
  }
  if(screen_method == "lasso"){
    Wvars <- screen_covars_lasso(data = the_data,
                                 wk = wk,
                                 outcome = outcome,
                                 desired_covars = num_covars)$Wvars
    screen_cor_p_cutoff <- NA
    screen_cor_r_cutoff <- NA
    screen_cor_trim_hi <- NA
    screen_cor_trim_lo <- NA
  }
  if(screen_method == "cor"){
    Wvars <- screen_covars_cor(data = the_data,
                               wk = wk,
                               outcome = outcome,
                               p_cutoff = screen_cor_p_cutoff,
                               cor_cutoff = screen_cor_r_cutoff,
                               trimhi = screen_cor_trim_hi,
                               trimlo = screen_cor_trim_lo)$Wvars
    num_covars = NA
  }
  if(screen_method == "rf"){
    Wvars <- screen_covars_rf(data = the_data,
                              wk = wk,
                              outcome = outcome,
                              desired_covars = num_covars)$Wvars
    screen_cor_p_cutoff <- NA
    screen_cor_r_cutoff <- NA
    screen_cor_trim_hi <- NA
    screen_cor_trim_lo <- NA
  }
    
  #plan(multisession, workers = cores) ############ unadjusted
  dat <- the_data %>%
    select(all_of(c(#Wvars, ####################### unadjusted
                    outcome,
                    exposure,
                    "week", "state_abb", "fips",
                    "cum_cases_this_date"))) %>%
    ungroup() %>% 
    filter(week == wk,
           cum_cases_this_date > 0,
           state_abb %in% sts_abb) %>%
    drop_na %>% 
    mutate(W = 1) ################################## unadjusted needs an intercept to run
  
  n <- nrow(dat)
  
  print("================================================================")
  print("================================================================")
  print(paste0("Week: ", wk))
  print(paste0("Sample size: ", n))
  print(paste0("Exposure variable: ", exposure))
  print(paste0("Shift type: ", shift_type))
  print(paste0("Shift amount: ", shift_amount))
  print(paste0("Shift lower bound: ", shift_lower_bound))
  print(paste0("Shift upper bound: ", shift_upper_bound))
  print(paste0("Outcome: ", outcome))
  print(paste0("Outcome lower bound: ", outcome_lower_bound))
  print(paste0("Outcome upper bound: ", outcome_upper_bound))
  print(paste0("Covariate screening method: ", screen_method))
  print(paste0("# of covariates requested: ", num_covars))
  print("Data being used (unadjusted, so no covariates):")
  print(colnames(dat))
  print("States included:")
  print(sts_abb)
  print("Starting shifted fit...")
  with_progress({
    fit_shift <- lmtp_tmle(
      data = dat,
      trt = exposure,
      outcome = outcome,
      baseline = "W",
      shift = shift,
      intervention_type = "mtp",
      outcome_type = "continuous",
      bounds = c(outcome_lower_bound, outcome_upper_bound)#,
      #learners_outcome = l_q, ############################## unadj
      #learners_trt = l_g  ################################## unadj
    )
  })
  
  obs_mean <- mean(dat[[outcome]])
  est <- fit_shift$theta
  se <- fit_shift$standard_error
  ic <- fit_shift$eif
  density_ratios <- fit_shift$density_ratios
  mean_diff <- est - obs_mean
  IC_0 <- dat[[outcome]] - obs_mean
  mean_diff_se <- sd(ic - IC_0) / sqrt(n)
  md_upper <- mean_diff + 1.96*mean_diff_se
  md_lower <- mean_diff - 1.96*mean_diff_se
  
  print(fit_shift)
  print(paste0("shifted mean outcome : ", round(est, digits = 3)))
  print(paste0("observed mean outcome: ", round(obs_mean, digits = 3)))
  print("         ...    ")
  print(paste0("Mean difference      : ", round(mean_diff, digits = 3)))
  print(paste0("SE for md            : ", round(mean_diff_se, digits = 3)))
  print(paste0("Upper                : ", round(md_upper, digits = 3)))
  print(paste0("Lower                : ", round(md_lower, digits = 3)))
  
  print("Summary of EIF:")
  print(summary(ic))
  print("Summary of density ratios:")
  print(summary(density_ratios))
  
  #plan(sequential)
  elapsed_time <- Sys.time() - start_time
  print(paste("Time for this analysis:"))
  print(elapsed_time)
  return(list(week = wk,
              outcome = outcome,
              exposure = exposure,
              shift = shift_amount,
              shift_lower_bound = shift_lower_bound,
              shift_upper_bound = shift_upper_bound,
              summary = fit_shift,
              observed_mean_y = obs_mean,
              data = dat,
              ic = ic,
              tmle_estimate = est,
              mean_diff = mean_diff,
              se = se,
              mean_diff_se = mean_diff_se,
              covariates = Wvars,
              screen_method = screen_method,
              screen_cor_p_cutoff = screen_cor_p_cutoff,
              screen_cor_r_cutoff = screen_cor_r_cutoff,
              screen_cor_trim_hi = screen_cor_trim_hi,
              screen_cor_trim_lo = screen_cor_trim_lo,
              covars_requested = num_covars,
              covars_used = length(Wvars),
              complete_case_sample_size = n,
              states_included = sts_abb,
              runtime = elapsed_time,
              cores = cores,
              shift_type = shift_type,
              outcome_lower_bound = outcome_lower_bound,
              outcome_upper_bound = outcome_upper_bound))
}


analysis_wrapper <- function(covars,
                             data,
                             exposure,
                             outcome,
                             weeks,
                             lagA = F,
                             shifts,
                             shift_type,
                             shift_lower_bound,
                             shift_upper_bound,
                             outcome_lower_bound = 0,
                             outcome_upper_bound = 99999,
                             pop_cutoff = 0){
  
  if(pop_cutoff > 0){
    pop <- c("high", "low")
  } else {
    pop <- "all"
  }
  
  grid_matrix <- expand.grid(week = weeks,
                             shift = shifts,
                             type = shift_type,
                             pop = pop,
                             stringsAsFactors = F)
  doneC <- data.frame(matrix(NA, nrow = nrow(grid_matrix), ncol = 31))
  
  for(i in 1:nrow(grid_matrix)){
    
    if(grid_matrix[i,"pop"] == "high"){
      print("================================================================")
      print("analyzing high-population counties now")
      data_pd <- data %>% filter(population >= pop_cutoff)
    } else if(grid_matrix[i,"pop"] == "low"){
      print("================================================================")
      print("analyzing low-density counties now")
      data_pd <- data %>% filter(population < pop_cutoff)
    } else {
      data_pd <- data
      print("not stratifying by population density")
    }
    
    tt <- lmtp_analysis(wk = grid_matrix[i,"week"],
                        screen_method = "none",
                        predetermined_covars = covars,
                        the_data = data_pd,
                        exposure = exposure,
                        outcome = outcome,
                        shift_amount = grid_matrix[i,"shift"],
                        shift_lower_bound = shift_lower_bound,
                        shift_upper_bound = shift_upper_bound,
                        outcome_lower_bound = outcome_lower_bound,
                        outcome_upper_bound = outcome_upper_bound,
                        ntrees_gq = 1000,
                        shift_type = grid_matrix[i,"type"]
    )
    colnames(doneC) <- c(names(tt), "pop_level", "pop_cutoff")
    doneC[i,"pop_level"] <- grid_matrix[i,"pop"]
    doneC[i,"pop_cutoff"] <- pop_cutoff
    doneC[i,"week"] <- tt$week
    doneC[i,"outcome"] <- tt$outcome
    doneC[i,"exposure"] <- tt$exposure
    doneC[i,"shift"] <- tt$shift
    doneC[i,"shift_lower_bound"] <- tt$shift_lower_bound
    doneC[i,"shift_upper_bound"] <- tt$shift_upper_bound
    doneC[i,"outcome_lower_bound"] <- tt$outcome_lower_bound
    doneC[i,"outcome_upper_bound"] <- tt$outcome_upper_bound
    doneC[["summary"]][i] <- tt["summary"]
    doneC[i,"observed_mean_y"] <- tt$observed_mean_y
    doneC[["data"]][i] <- tt["data"]
    doneC[["ic"]][i] <- tt["ic"]
    doneC[i,"tmle_estimate"] <- tt$tmle_estimate
    doneC[i,"mean_diff"] <- tt$mean_diff
    doneC[i,"se"] <- tt$se
    doneC[i,"mean_diff_se"] <- tt$mean_diff_se
    doneC[["covariates"]][i] <- tt["covariates"]
    doneC[i,"covars_requested"] <- tt$covars_requested
    doneC[i,"covars_used"] <- tt$covars_used
    doneC[i,"complete_case_sample_size"] <- tt$complete_case_sample_size
    doneC[["states_included"]][i] <- tt["states_included"]
    doneC[["runtime"]][i] <- tt["runtime"]
    doneC[i,"cores"] <- tt$cores
    doneC[i,"screen_method"] <- tt$screen_method
    doneC[i,"screen_cor_p_cutoff"] <- tt$screen_cor_p_cutoff
    doneC[i,"screen_cor_r_cutoff"] <- tt$screen_cor_r_cutoff
    doneC[i,"screen_cor_trim_hi"] <- tt$screen_cor_trim_hi
    doneC[i,"screen_cor_trim_lo"] <- tt$screen_cor_trim_lo
    doneC[i,"shift_type"] <- tt$shift_type
  }
  return(doneC)
}


get_complete_counties <- function(weeks, outcome, data, covars){
  DAT <- data %>% filter(week >= min(weeks), week <= max(weeks)) %>% 
    select(all_of(c(covars, ###########################################
                    "week", "state_abb", "cum_cases_this_date",
                    time_covars[time_covars != "temp_deviation_from_70"],
                    outcome)))
  nC <- unique(DAT$fips[!complete.cases(DAT)])
  DAT2 <- DAT %>% filter(fips %nin% nC)
  return(DAT2)
}









