library(tidyverse)

# loads data --------------------------------------------------------------
rpdr_all <- read_csv('data/rpdr_all.csv')
rpdr_contestants <- read_csv('data/rpdr_contestants.csv')
rpdr_rankings <- read_csv('data/rpdr_rankings.csv')
rpdr_episodes <- read_csv('data/rpdr_episodes.csv')
rpdr_lipsyncs <- read_csv('data/rpdr_lipsyncs.csv')

# gets season's scores based on Dusted or Busted Scoring System -----------
# [view metodology](https://rupaulsdragrace.fandom.com/wiki/%22Dusted_or_Busted%22_Scoring_System)

base_scores <- rpdr_rankings %>% distinct(episode_placement) %>% 
  add_column(points=c(0, 2, -1, 1, -2, -2, rep(NA, 7), -2),
             performance=c(5, 10, 2.5, 7.5, 0, 0, rep(NA, 7), -2))

no_episodes <- rpdr_episodes %>% group_by(season_number) %>% summarize(episodes=n())

## calculates scores and weights on number of episodes
rpdr_rankings <- rpdr_rankings %>% left_join(base_scores) %>% left_join(no_episodes) %>% 
  mutate(points_episodes=points/episodes, performance_episodes=performance/episodes)

rpdr_scores <- rpdr_rankings %>% group_by(season_number, contestant_name) %>%
  summarize(points=sum(points, na.rm=T),
            performance=sum(performance, na.rm=T),
            points_episodes=sum(points_episodes, na.rm=T),
            performance_episodes=sum(performance_episodes, na.rm=T)) %>% arrange(desc(performance_episodes))

rpdr_scores <- left_join(rpdr_scores, rpdr_contestants %>%
                           select(season_number, contestant_name, season_outcome)) %>%
                           arrange(desc(performance_episodes), desc(points_episodes))

## comparison of balancing or not scores by number of season episodes
comparison_scores <- tibble(general=(rpdr_scores %>% arrange(desc(performance), desc(points)))[1:10,"contestant_name"],
                     weighted=rpdr_scores[1:10,"contestant_name"])

## graph of performance scores (weighted)
ggplot(rpdr_scores[1:10,], aes(fct_reorder(contestant_name, performance), performance,
                               fill=as.factor(season_number))) + coord_flip() + theme_minimal() +
  geom_col() + labs(x="contestant") + guides(fill=F) + theme(panel.grid.major = element_blank(),
                                                             panel.grid.minor = element_blank())

## episodes with double shantay or double elimination
double_sh_elim <- rpdr_rankings %>% filter(str_detect(episode_placement, "BTM|ELIM")) %>%
  group_by(season_number, episode_number) %>% count(episode_placement) %>% filter(n>=2)

## episodes with double or more wins
double_win <- rpdr_rankings %>% filter(str_detect(episode_placement, "WIN")) %>%
  group_by(season_number, episode_number) %>% count(episode_placement) %>% filter(n>=2)


# number of lipsyncs by song author ---------------------------------------
rpdr_lipsyncs %>% count(song_author, sort=T) %>% filter(n>=2) %>% 
  ggplot(aes(fct_reorder(song_author, n), n, fill=song_author)) +
  geom_col() + guides(fill=F) + coord_flip() + labs(x="author") + theme_minimal() +
  scale_y_continuous(breaks=c(0, 2, 3, 4, 7, 11)) + theme(panel.grid.major = element_blank(),
                                                          panel.grid.minor = element_blank())
