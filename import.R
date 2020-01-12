library(tidyverse)
library(htmltab)
library(janitor)
library(googlesheets)


# imports data from RuPaul-Predict-a-Looza --------------------------------
gs_auth()
gs_data <- "1Sotvl3o7J_ckKUg5sRiZTqNQn3hPqhepBSeOpMTK15Q" %>%  gs_key

rpdr_episodes <- gs_data %>% gs_read("all_episodes") %>% write_csv('data/rpdr_episodes.csv', na="")
rpdr_contestants <- gs_data %>% gs_read("all_contestants")
rpdr_rankings <- gs_data %>% gs_read("all_rankings")

## fixes old data with current results
rpdr_contestants[137:140,"season_outcome"] <- c(3, 1, 3, 2)
rpdr_rankings <- rpdr_rankings %>% rbind(rpdr_rankings[1186:1200,])
rpdr_rankings[1201:1215,]$episode_number <- 14
rpdr_rankings[c(1201:1204, 1206),"episode_placement"] <- c("Eliminated", "Runner-up", "Eliminated", "Winner", "Miss C")

## joins all datasets
rpdr_all <- rpdr_contestants %>% left_join(rpdr_rankings) %>% left_join(rpdr_episodes) %>% 
  arrange(season_number, episode_number) %>% 
  select(contestant_id, contestant_name, season_number, episode_number, season_outcome,
         episode_title, episode_placement, episode_airdate:episode_maxi_challenge_type, everything())

## saves all datasets
rpdr_all %>% write_csv("data/rpdr_all.csv", na="")
rpdr_contestants %>% write_csv("data/rpdr_contestants.csv", na="")
rpdr_rankings %>% left_join(rpdr_contestants %>% select(contestant_id, contestant_name)) %>% 
  select(season_number:contestant_id, contestant_name, episode_placement) %>% write_csv("data/rpdr_rankings.csv", na="")


# imports lipsync data from Wikipedia -------------------------------------
rpdr_lipsyncs <- list()

for (i in 1:11) {
  rpdr_lipsyncs[[i]] <- htmltab(paste0('https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_', i, ')'), which=4)
}

tidy_lipsyncs <- function(df) {
  df %>% as_tibble(.name_repair = "unique") %>% janitor::clean_names("snake") %>% type_convert() %>% 
    select(episode_number=episode, lipsync_song=song)
}

rpdr_lipsyncs <- bind_rows(lapply(rpdr_lipsyncs, tidy_lipsyncs), .id = "season_number") %>% type_convert() %>% 
  mutate(lipsync_song=str_replace_all(lipsync_song, c('"'='', '“'='', '”'=''))) %>%
  extract(lipsync_song, c("lipsync_song", "song_author"), regex="(.*)\\(([^)]*)\\)[^(]*$") %>% 
  write_csv('data/rpdr_lipsyncs.csv', na="")
rm(i)

## joins lipsync data
rpdr_episodes_lipsyncs <- rpdr_episodes %>% left_join(rpdr_lipsyncs) %>% write_csv('data/rpdr_episodes_lipsyncs.csv', na="")
rapdr_all <- rpdr_all %>% left_join(rpdr_lipsyncs) %>%
  select(contestant_id:episode_maxi_challenge_type, lipsync_song:song_author, everything()) %>% 
  write_csv('data/rpdr_all.csv')


# import ratings data from Wikipedia (in progress...) ---------------------
ratings_s11 <- htmltab('https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_11)', which=6,
                       colNames=c("episode_number", "title", "air_date", "rating", "viewers")) %>%
  as_tibble() %>% type_convert()