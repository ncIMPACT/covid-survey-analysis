library(tidyverse)
library(ggtext)
library(extrafont)
library(here)
library(janitor)
library(glue)
library(gt)
library(RColorBrewer)
library(tidytext)
library(quanteda)
library(stm)

dat <- read_csv(here("data/final-clean-data.csv")) %>% 
  rename(q4 = q4_1) %>%
  mutate(juri_type = case_when(
    str_length(coded_geoid) > 5 ~ "Municipality",
    str_length(coded_geoid) <= 5 ~ "County"
  )) %>%
  mutate(county_tier = as.character(county_tier))

tidy_dat <- dat %>%
  filter(!(is.na(q14))) %>%
  select(response_id, q14) %>%
  mutate(index = row_number()) %>%
  unnest_tokens(word, q14) %>%
  anti_join(stop_words) %>%
  filter(word != "covid", word != "19") %>%
  group_by(index, word) %>%
  count(sort = TRUE) %>%
  ungroup()

dat_dfm <- tidy_dat %>%
  count(index, word, sort = TRUE) %>%
  cast_dfm(index, word, n)

sparse_dat <- tidy_dat %>%
  cast_sparse(index, word, n)

topic_model <- stm(sparse_dat, K = 5, 
                   verbose = FALSE, init.type = "Spectral")

summary(topic_model)

td_beta <- tidy(topic_model)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")
