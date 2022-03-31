require(tidyverse)
require(lubridate)
library(gridExtra)

day <- "results/as_2021_10_23"
leadY <- "2wks"
WY_lag <- "lag1"
#IMGNAME <- paste0("_",day,"_",leadY,"_",WY_lag,".png")

Cda <- readRDS(paste0(day, "_unadj=FALSE_leadY=",leadY,"_WY_lag=",WY_lag,"_dex_a.rds"))
CFBm <- readRDS(paste0(day, "_unadj=FALSE_leadY=",leadY,"_WY_lag=",WY_lag,"_all_day_bing_tiles_visited_relative_change.rds"))
CGH <- readRDS(paste0(day, "_unadj=FALSE_leadY=",leadY,"_WY_lag=",WY_lag,"_residential_percent_change_from_baseline.rds"))




abc2 <- rbind.data.frame(
  Cda %>% mutate(adj_type = "Adjusted"),
  CFBm %>% mutate(adj_type = "Adjusted"),
  CGH %>% mutate(adj_type = "Adjusted"))
casey <- c(-80, 50)



absplot <- function(data,
                    y_range = c(-60, 35),
                    barwidth = 2,
                    dodgewidth = 4,
                    as_prop = F,
                    week_cutoff = 22,
                    factor_reverse = T){
  if(factor_reverse){
    data <- data %>% mutate(shift = fct_rev(factor(shift)))
  } else {
    data <- data %>% mutate(shift = factor(shift))
  }
  
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
                             levels = c("all_day_bing_tiles_visited_relative_change",
                                        "dex_a",
                                        "residential_percent_change_from_baseline"),
                             labels = c("Facebook tiles visited",
                                        "PlaceIQ DEX-A",
                                        "Google residential index")))
  ggplot(data = a1) +
    geom_hline(aes(yintercept = 0)) +
    geom_point(data = a1 %>% filter(adj_type == "Adjusted"),
               mapping = aes(x = date, y = estimate,
                             color = shift,
                             group = shift),
               size = 1#,         # position = position_dodge(width = dodgewidth)
    ) +
  geom_errorbar(data = a1 %>% filter(adj_type == "Adjusted"),
                mapping = aes(x = date, ymin = low, ymax = hi,      #group = adj_type,
                              color = shift,
                              group = shift),#, linetype = shift),
                width = barwidth,
                alpha = .7,
                size = .5 # #,          position = position_dodge(width = dodgewidth)
  ) +

    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size=8),
          strip.text = element_text(size = 5),
          axis.title.y = element_text(size = 5),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 5),
          legend.text = element_text(size = 7)) +
    facet_grid(.~shift, labeller = label_both) +
    coord_cartesian(ylim = y_range) +
    labs(title = paste0(a1$exposure[1], ", with smaller confounder set and larger shifts"),
         y = "Expected difference in new COVID-19 cases per 100,000 residents\nwith shifted mobility as compared to observed mobiilty",
         x = "Date")
}

(theplot <- absplot(data = abc2 %>% filter(exposure == "all_day_bing_tiles_visited_relative_change"), y_range = casey))
ggsave(filename = "_fb_as.png",
       device = "png",
       dpi = 320,
       width = 6,
       height = 3,
       units = "in",
       plot = theplot,
       bg = "white")
(theplot <- absplot(data = abc2 %>% filter(exposure == "dex_a"), y_range = casey))
ggsave(filename = "_da_as.png",
       device = "png",
       dpi = 320,
       width = 6,
       height = 3,
       units = "in",
       plot = theplot,
       bg = "white")
(theplot <- absplot(data = abc2 %>%
                      filter(exposure == "residential_percent_change_from_baseline"),
                    y_range = casey, factor_reverse = F))
ggsave(filename = "_gh_as.png",
       device = "png",
       dpi = 320,
       width = 6,
       height = 3,
       units = "in",
       plot = theplot,
       bg = "white")

