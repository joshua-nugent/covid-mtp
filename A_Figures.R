require(tidyverse)
require(lubridate)

day <- "results/2021_02_19"
dd <- "2021_02_19"
leadY <- "2wks" # Change these for sensitivity analysis plots, "2wks" or "1wks"
WY_lag <- "lag1" # Change these for sensitivity analysis plots "Lag1" or "current"
IMGNAME <- paste0("_",dd,"_",leadY,"_",WY_lag,".png")

Cda <- readRDS(paste0(day, "_unadj=FALSE_leadY=",leadY,"_WY_lag=",WY_lag,"_dex_a.rds"))
Cd <- readRDS(paste0(day, "_unadj=FALSE_leadY=",leadY,"_WY_lag=",WY_lag,"_dex.rds"))
CdaU <- readRDS(paste0(day, "_unadj=TRUE_leadY=",leadY,"_WY_lag=","lag1","_dex_a.rds"))
CdU <- readRDS(paste0(day, "_unadj=TRUE_leadY=",leadY,"_WY_lag=","lag1","_dex.rds"))

CFBm <- readRDS(paste0(day, "_unadj=FALSE_leadY=",leadY,"_WY_lag=",WY_lag,"_all_day_bing_tiles_visited_relative_change.rds"))
CFBs <- readRDS(paste0(day, "_unadj=FALSE_leadY=",leadY,"_WY_lag=",WY_lag,"_all_day_ratio_single_tile_users.rds"))
CFBmU <- readRDS(paste0(day, "_unadj=TRUE_leadY=",leadY,"_WY_lag=","lag1","_all_day_bing_tiles_visited_relative_change.rds"))
CFBsU <- readRDS(paste0(day, "_unadj=TRUE_leadY=",leadY,"_WY_lag=","lag1","_all_day_ratio_single_tile_users.rds"))

Cm50i <- readRDS(paste0(day, "_unadj=FALSE_leadY=",leadY,"_WY_lag=",WY_lag,"_m50_index.rds"))
Cm50 <- readRDS(paste0(day, "_unadj=FALSE_leadY=",leadY,"_WY_lag=",WY_lag,"_m50.rds"))
Cm50iU <- readRDS(paste0(day, "_unadj=TRUE_leadY=",leadY,"_WY_lag=","lag1","_m50_index.rds"))
Cm50U <- readRDS(paste0(day, "_unadj=TRUE_leadY=",leadY,"_WY_lag=","lag1","_m50.rds"))

CGR <- readRDS(paste0(day, "_unadj=FALSE_leadY=",leadY,"_WY_lag=",WY_lag,"_retail_and_recreation_percent_change_from_baseline.rds"))
CGW <- readRDS(paste0(day, "_unadj=FALSE_leadY=",leadY,"_WY_lag=",WY_lag,"_workplaces_percent_change_from_baseline.rds"))
CGH <- readRDS(paste0(day, "_unadj=FALSE_leadY=",leadY,"_WY_lag=",WY_lag,"_residential_percent_change_from_baseline.rds"))
CGT <- readRDS(paste0(day, "_unadj=FALSE_leadY=",leadY,"_WY_lag=",WY_lag,"_transit_stations_percent_change_from_baseline.rds"))
CGRU <- readRDS(paste0(day, "_unadj=TRUE_leadY=",leadY,"_WY_lag=","lag1","_retail_and_recreation_percent_change_from_baseline.rds"))
CGWU <- readRDS(paste0(day, "_unadj=TRUE_leadY=",leadY,"_WY_lag=","lag1","_workplaces_percent_change_from_baseline.rds"))
CGHU <- readRDS(paste0(day, "_unadj=TRUE_leadY=",leadY,"_WY_lag=","lag1","_residential_percent_change_from_baseline.rds"))
CGTU <- readRDS(paste0(day, "_unadj=TRUE_leadY=",leadY,"_WY_lag=","lag1","_transit_stations_percent_change_from_baseline.rds"))




abc2 <- rbind.data.frame(
  Cda %>% filter(shift == .75) %>% mutate(adj_type = "Adjusted"),
  Cd %>% filter(shift == .75) %>% mutate(adj_type = "Adjusted"),
  CFBs %>% filter(shift == 1.05) %>% mutate(adj_type = "Adjusted"),
  CFBm %>% filter(shift == -.04) %>% mutate(adj_type = "Adjusted"),
  Cm50 %>% filter(shift == .8) %>% mutate(adj_type = "Adjusted"),
  Cm50i %>% filter(shift == .9) %>% mutate(adj_type = "Adjusted"),
  CGR %>% filter(shift == -5) %>% mutate(adj_type = "Adjusted"),
  CGH %>% filter(shift == 2) %>% mutate(adj_type = "Adjusted"),
  CGT %>% filter(shift == -5) %>% mutate(adj_type = "Adjusted"),
  CGW %>% filter(shift == -3) %>% mutate(adj_type = "Adjusted"),
  CdaU %>% filter(shift == .75) %>% mutate(adj_type = "Unadjusted"),
  CdU %>% filter(shift == .75) %>% mutate(adj_type = "Unadjusted"),
  CFBsU %>% filter(shift == 1.05) %>% mutate(adj_type = "Unadjusted"),
  CFBmU %>% filter(shift == -.04) %>% mutate(adj_type = "Unadjusted"),
  Cm50U %>% filter(shift == .8) %>% mutate(adj_type = "Unadjusted"),
  Cm50iU %>% filter(shift == .9) %>% mutate(adj_type = "Unadjusted"),
  CGRU %>% filter(shift == -5) %>% mutate(adj_type = "Unadjusted"),
  CGHU %>% filter(shift == 2) %>% mutate(adj_type = "Unadjusted"),
  CGTU %>% filter(shift == -5) %>% mutate(adj_type = "Unadjusted"),
  CGWU %>% filter(shift == -3) %>% mutate(adj_type = "Unadjusted"))

casey <- c(-40, 20)





absplot <- function(data,
                    y_range = c(-25, 15),
                    barwidth = 2,
                    dodgewidth = 3,
                    as_prop = F,
                    week_cutoff = 22){
  
  a1 <- data %>% select(outcome, week, observed_mean_y, shift, mean_diff,
                        mean_diff_se, covars_requested, exposure,
                        tmle_estimate, adj_type) %>% 
    mutate(low = mean_diff - 1.96*mean_diff_se,
           hi  = mean_diff + 1.96*mean_diff_se,
           date = as.Date(lubridate::ymd( "2020-01-01" ) + lubridate::weeks(week - 1))) %>% 
    rename(estimate = mean_diff) %>% 
    select(-mean_diff_se, -tmle_estimate) %>% 
    filter(week > week_cutoff) %>% 
    mutate(exposure = factor(exposure,
                             levels = c("all_day_ratio_single_tile_users",
                                        "all_day_bing_tiles_visited_relative_change",
                                        "dex",
                                        "dex_a",
                                        "m50",
                                        "m50_index",
                                        "retail_and_recreation_percent_change_from_baseline",
                                        "workplaces_percent_change_from_baseline",
                                        "residential_percent_change_from_baseline",
                                        "transit_stations_percent_change_from_baseline"),
                             labels = c("Facebook single tile users\n5% increase",
                                        "Facebook tiles visited\n4% decrease",
                                        "PlaceIQ DEX\n25% decrease",
                                        "PlaceIQ DEX-A\n25% decrease",
                                        "Descartes Labs m50\n20% decrease",
                                        "Descartes Labs m50 index\n10% decrease",
                                        "Google retail & recreation index\n5% decrease",
                                        "Google workplace index\n3% decrease",
                                        "Google residential index\n2% increase",
                                        "Google transit station index\n5% decrease")))
  ggplot(data = a1) +
    geom_hline(aes(yintercept = 0)) +
    geom_errorbar(data = a1 %>% filter(adj_type == "Unadjusted"),
                  mapping = aes(x = date, ymin = low, ymax = hi),      
                  width = barwidth,
                  color = "grey60",
                  alpha = .6,
                  size = .5            
    ) +
    geom_point(data = a1 %>% filter(adj_type == "Unadjusted"),
               mapping = aes(x = date, y = estimate),
               color = "grey60",
               alpha = .6,
               size = 1,
    ) +
    geom_point(data = a1 %>% filter(adj_type == "Adjusted"),
               mapping = aes(x = date, y = estimate, color = exposure),
               size = 1,
    ) +
    
  geom_errorbar(data = a1 %>% filter(adj_type == "Adjusted"),
                mapping = aes(x = date, ymin = low, ymax = hi,  
                              color = exposure),
                width = barwidth,
                alpha = .7,
                size = .5            
  ) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.caption.position = "panel",
          plot.caption = element_text(hjust = 0, size = 9),
          plot.title = element_text(size=10),
          strip.text = element_text(size = 6),
          axis.title.y = element_text(size = 8),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 6),
          legend.text = element_text(size = 9)) +
    facet_wrap(~exposure) +
    coord_cartesian(ylim = y_range) +
    labs(y = "Expected difference in new COVID-19 cases per 100,000 residents\nwith shifted mobility as compared to observed mobiilty",
         x = "Date")
}

theplot <- absplot(data = abc2, y_range = casey)
theplot

# ggsave(filename = IMGNAME,
#        device = "png",
#        dpi = 320,
#        width = 6,
#        height = 5,
#        units = "in",
#        plot = theplot,
#        bg = "white")
