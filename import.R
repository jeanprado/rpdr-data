library(tidyverse)
library(htmltab)
library(janitor)
library(googlesheets)


# imports data from RuPaul-Predict-a-Looza --------------------------------
gs_auth()
gs_data <- "1Sotvl3o7J_ckKUg5sRiZTqNQn3hPqhepBSeOpMTK15Q" %>%  gs_key

rpdr_episodes <- gs_data %>% gs_read("all_episodes") %>% write_csv('data/rpdr_episodes.csv', na="")
rpdr_contestants <- gs_data %>% gs_read("all_contestants")

## fixes old data with current results
rpdr_contestants[137:140,"season_outcome"] <- c(3, 1, 3, 2)

# imports rankings from Wikipedia --------------------------------
rpdr_rankings <- list()

for (i in 1:11) {
  rpdr_rankings[[i]] <- htmltab(paste0('https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_', i, ')'), which=3,
                                rm_nodata_cols = F)
}

tidy_rankings <- function(df) {
  df %>% as_tibble(.name_repair = "unique") %>% gather("episode_number", "episode_placement", -Contestant) %>%
    janitor::clean_names("snake") %>% drop_na(episode_placement) %>% select(contestant_name=contestant, everything())
}

## fix difference in names to join with contestants data
rpdr_rankings <- bind_rows(lapply(rpdr_rankings, tidy_rankings), .id = "season_number") %>%
  mutate(contestant_name=case_when(season_number == 2 & contestant_name == "Shangela" ~ "Shangela Laquifa Wadley",
                                   contestant_name == 'Victoria "Porkchop" Parker' ~ "Victoria Porkchop Parker",
                                   contestant_name == "A'Keria C. Davenport" ~ "A'keria Chanel Davenport",
                                   contestant_name == "Ra'Jah O'Hara" ~ "Ra'jah D. O'Hara",
                                   TRUE ~ contestant_name),
         episode_number=recode(episode_number, "14...15"="14"),
         episode_placement=recode(episode_placement, "Runner-Up"="Runner-up")) %>% type_convert()

## fixes contestant ids for returning seasons
rpdr_rankings <- rpdr_rankings %>% left_join(rpdr_contestants %>% select(contestant_id, contestant_name)) %>% 
  select(season_number, episode_number, contestant_id, contestant_name, episode_placement) %>% 
  mutate(contestant_id=case_when(season_number == 8 & contestant_name == "Cynthia Lee Fontaine" ~ 91,
                                 season_number == 9 & contestant_name == "Cynthia Lee Fontaine" ~ 115,
                                 season_number == 9 & contestant_name == "Eureka" ~ 104,
                                 season_number == 10 & contestant_name == "Eureka" ~ 116,
                                 season_number == 10 & contestant_name == "Vanessa Vanjie Mateo" ~ 128,
                                 season_number == 11 & contestant_name == "Vanessa Vanjie Mateo" ~ 130,
                                 TRUE ~ contestant_id)) %>% 
  distinct() %>% write_csv('data/rpdr_rankings.csv', na="")

## RuPaul-Predict-a-Looza had wrong episode_placement scores, this line shows which ones were wrong
# left_join(rpdr_rankings, gs_data %>% gs_read("all_rankings"), by=c("season_number", 'episode_number', 'contestant_id')) %>%
#   select(season_number, episode_number, contestant_name, new=episode_placement.x, old=episode_placement.y) %>%
#   mutate(compare=new==old) %>% filter(compare==F) %>% select(-compare)

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

# joins all datasets ------------------------------------------------------
rpdr_all <- rpdr_contestants %>% left_join(rpdr_rankings) %>% left_join(rpdr_episodes) %>% 
  arrange(season_number, episode_number) %>% left_join(rpdr_lipsyncs) %>% 
  select(contestant_id, contestant_name, season_number, episode_number, season_outcome, episode_title,
         episode_placement, episode_airdate:episode_maxi_challenge_type, lipsync_song:song_author, everything()) %>% 
  write_csv('data/rpdr_all.csv')

# saves unsaved datasets ------------------------------------------------------
rpdr_contestants %>% select(season_number:contestant_entrance, contestant_id, everything()) %>%
  write_csv("data/rpdr_contestants.csv", na="")

rpdr_episodes %>% left_join(rpdr_lipsyncs) %>% write_csv('data/rpdr_episodes_lipsyncs.csv', na="")

# import ratings data from Wikipedia (in progress...) ---------------------
ratings_s11 <- htmltab('https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_11)', which=6,
                       colNames=c("episode_number", "title", "air_date", "rating", "viewers")) %>%
  as_tibble() %>% type_convert()