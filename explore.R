library(tidyverse)

# Loads data
rpdr_all <- read_csv('data/rpdr_all.csv')
rpdr_contestants <- read_csv('data/rpdr_contestants.csv')
rpdr_rankings <- read_csv('data/rpdr_rankings.csv')
rpdr_episodes <- read_csv('data/rpdr_episodes.csv')

# Explores data

## Gets season's scores
base_scores <- rpdr_rankings %>% distinct(episode_placement) %>% 
  add_column(points=c(0, 2, -1, 1, -2, -2, rep(NA, 7), -2),
             performance=c(5, 10, 2.5, 7.5, 0, 0, rep(NA, 7), -2))

no_episodes <- rpdr_episodes %>% group_by(season_number) %>% summarize(episodes=n())

rpdr_rankings <- rpdr_rankings %>% left_join(base_scores) %>% left_join(no_episodes)

rpdr_scores <- rpdr_rankings %>% group_by(season_number, contestant_name) %>%
  summarize(points=sum(points, na.rm=T),
            performance=sum(performance, na.rm=T),
            points_episodes=sum(points_episodes, na.rm=T),
            performance_episodes=sum(performance_episodes, na.rm=T)) %>% arrange(desc(performance_episodes))

rpdr_scores <- left_join(rpdr_scores, rpdr_contestants %>%
                           select(season_number, contestant_name, season_outcome)) %>%
                           arrange(desc(performance_episodes), desc(points_episodes))

# Comparison of balancing or not by episode_number
comparison_scores <- tibble(general=(rpdr_scores %>% arrange(desc(performance), desc(points)))[1:10,"contestant_name"],
                     weighted=rpdr_scores[1:10,"contestant_name"])

# Graph of performance
ggplot(rpdr_scores[1:10,], aes(fct_reorder(contestant_name, -performance), performance,
                               fill=as.factor(season_number))) +
  geom_col() + labs(x="contestant") + guides(fill=F)

## Episodes with double shantay or double elimination
double_sh_elim <- rpdr_rankings %>% filter(str_detect(episode_placement, "BTM|ELIM")) %>%
  group_by(season_number, episode_number) %>% count(episode_placement) %>% filter(n>=2)

## Episodes with double or more wins
double_win <- rpdr_rankings %>% filter(str_detect(episode_placement, "WIN")) %>%
  group_by(season_number, episode_number) %>% count(episode_placement) %>% filter(n>=2)
