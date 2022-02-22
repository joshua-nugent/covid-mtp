require(tidyverse)
require(lubridate)
library(gridExtra)

day <- "results/2021_02_19"
dd <- "2021_02_19"
leadY <- "2wks"
WY_lag <- "lag1"
IMGNAME <- paste0("_",dd,"_",leadY,"_",WY_lag,"_FB_ONLY.png")

CFBs <- readRDS(paste0(day, "_unadj=FALSE_leadY=",leadY,"_WY_lag=",WY_lag,"_all_day_ratio_single_tile_users.rds"))
CFBsU <- readRDS(paste0(day, "_unadj=TRUE_leadY=",leadY,"_WY_lag=","lag1","_all_day_ratio_single_tile_users.rds"))

abc2 <- rbind.data.frame(
  CFBs %>% filter(shift == 1.05) %>% mutate(adj_type = "Adjusted"),
  CFBsU %>% filter(shift == 1.05) %>% mutate(adj_type = "Unadjusted"))

casey <- c(-40, 20)

# Pull out raw numbers for the text
abc_CI <- abc2 %>% select(week, shift, mean_diff, mean_diff_se,
                          covars_requested, exposure,
                          tmle_estimate, adj_type) %>% 
  mutate(low = mean_diff - 1.96*mean_diff_se,
         hi  = mean_diff + 1.96*mean_diff_se,
         date = as.Date(lubridate::ymd( "2020-01-01" ) + lubridate::weeks(week - 1))) %>% 
  rename(estimate = mean_diff) %>% 
  filter(week > 22)
View(abc_CI)

absplot <- function(data,
                    y_range = c(-25, 15),
                    barwidth = 4,
                    dw = 3,
                    sz = 2,
                    as_prop = F,
                    week_cutoff = 22){
  print(levels(factor(data$adj_type)))
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
                             levels = c("all_day_ratio_single_tile_users"),
                             labels = c("Facebook single tile users\n5% increase")))
  ggplot(data = a1 %>% arrange(desc(adj_type))) +
    geom_hline(aes(yintercept = 0)) +
    geom_point(mapping = aes(x = date, y = estimate,
                             color = fct_relevel(adj_type, "Unadjusted", "Adjusted")),
               size = 2) +
  geom_errorbar(mapping = aes(x = date, ymin = low, ymax = hi,
                              color = fct_relevel(adj_type, "Unadjusted", "Adjusted")),
                width = barwidth,
                alpha = 1,
                size = sz) +
    theme_minimal() +
    theme(#legend.position = "none",
          plot.caption.position = "panel",
          plot.caption = element_text(hjust = 0, size = 9),
          plot.title = element_text(size=10),
          strip.text = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 9),
          legend.text = element_text(size = 9)) +
    facet_wrap(~exposure) +
    scale_color_manual(name = 'Adjustment',guide=guide_legend(reverse=TRUE),
                        values =c('Adjusted'='#F8766D','Unadjusted'='grey60'))+#, labels = c('c2','c1'))+
    coord_cartesian(ylim = y_range) +
    labs(y = "Expected difference in new COVID-19 cases per 100,000 residents\nwith shifted mobility as compared to observed mobiilty",
         x = "Date")
}

theplot <- absplot(data = abc2, y_range = casey, sz = 1, barwidth = 3)
theplot

# ggsave(filename = IMGNAME,
#        device = "png",
#        dpi = 320,
#        width = 6,
#        height = 5,
#        units = "in",
#        plot = theplot,
#        bg = "white")








