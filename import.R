library(tidyverse)
library(googlesheets)

gs_auth()
gs_data <- "1Sotvl3o7J_ckKUg5sRiZTqNQn3hPqhepBSeOpMTK15Q" %>%  gs_key

rpdr_episodes <- gs_data %>% gs_read("all_episodes") %>% write_csv()
rpdr_contestants <- gs_data %>% gs_read("all_contestants")
rpdr_rankings <- gs_data %>% gs_read("all_rankings")%>% rbind(rpdr_rankings[1186:1200,])

# Fixes actual results
rpdr_contestants[137:140,"season_outcome"] <- c(3, 1, 3, 2)
rpdr_rankings[1201:1215,]$episode_number <- 14
rpdr_rankings[c(1201:1204, 1206),"episode_placement"] <- c("Eliminated", "Runner-up", "Eliminated", "Winner", "Miss C")

# Joins all datasets
rpdr_all <- rpdr_contestants %>% left_join(rpdr_rankings) %>% left_join(rpdr_episodes) %>% 
  arrange(season_number, episode_number) %>% 
  select(contestant_id, contestant_name, season_number, episode_number, season_outcome,
         episode_placement, episode_title, episode_airdate:episode_maxi_challenge_type, everything())

# Saves all datasets
rpdr_all %>% write_csv("data/rpdr_all.csv")
rpdr_contestants %>% write_csv("data/rpdr_contestants.csv")
rpdr_rankings %>% left_join(rpdr_contestants %>% select(contestant_id, contestant_name)) %>% 
  select(season_number:contestant_id, contestant_name, episode_placement) %>% write_csv("data/rpdr_rankings.csv")
