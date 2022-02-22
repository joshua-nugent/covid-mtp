library(tidyverse)
`%nin%` <- negate(`%in%`)

load("2021-01-25_covid_wk.RData")

time_covars

d <- dfwk %>% filter(week > 9, week < 47,
                     population > 40000) %>% ungroup() %>% select(fips, week,
                     workplaces_percent_change_from_baseline,
                     retail_and_recreation_percent_change_from_baseline,
                     #grocery_and_pharmacy_percent_change_from_baseline,
                     residential_percent_change_from_baseline,
                     transit_stations_percent_change_from_baseline,
                     #parks_percent_change_from_baseline,
                     all_day_ratio_single_tile_users,
                     all_day_bing_tiles_visited_relative_change,   
                     m50,                                    
                     m50_index,
                     dex, dex_a) %>% pivot_longer(cols = 3:12)

d2 <- d %>% filter(!(name == "m50" & value > 200),
                   !(name == "m50_index" & value > 1000))

(G <- ggplot(data = d2 %>% filter(week > 23) %>% 
               mutate(date = lubridate::ymd( "2020-01-01" ) + lubridate::weeks(week - 1)) %>% 
               mutate(name = factor(name,
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
                                        labels = c("Facebook single tile users",
                                                   "Facebook tiles visited",
                                                   "PlaceIQ DEX",
                                                   "PlaceIQ DEX-A",
                                                   "Descartes Labs m50",
                                                   "Descartes Labs m50 index",
                                                   "Google retail & recreation index",
                                                   "Google workplace index",
                                                   "Google residential index",
                                                   "Google transit station index")))) +
  geom_violin(aes(x = date,#factor(week),
                  group = date,
                  y = value),
              #color = "red",
              fill = "grey80", scale = "width") +
    theme_minimal() +
  theme(#axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        #axis.ticks.x = element_blank(),
        strip.text = element_text(size = 8))+
    labs(#x = "week",
         y = "index value",
         title = "Observed exposure distributions over time") +
  facet_wrap(~name, scales = "free_y"))

ggsave(filename = "exposure_violins.png",
       device = "png",
       dpi = 300,
       width = 10,
       height = 6,
       units = "in",
       plot = G,
       bg = "white")



d3 <- dfwk %>% filter(week > 9, week < 47,
                     population > 40000) %>%
  ungroup() %>% select(fips, week, pct_dem_2016, dex_a) %>% 
  mutate(pd = ifelse(pct_dem_2016>.5, "clinton16", "trump16"))

(G <- ggplot(data = d3) +
    geom_violin(aes(x = factor(week), y = dex_a),
                #color = "red",
                fill = "grey80", scale = "width") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          strip.text = element_text(size = 7),
          axis.ticks.x = element_blank()) +
    labs(x = "week", y = "index value",
         title = "Exposure distributions over time") +
    facet_wrap(~pd, scales = "free_y"))

