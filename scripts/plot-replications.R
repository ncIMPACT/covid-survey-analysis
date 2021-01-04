library(tidyverse)
library(ggtext)
library(showtext)
library(here)
library(janitor)
library(glue)

dat <- read_csv(here("data/final-clean-data.csv")) %>% 
  mutate(survey = "Two") %>%
  rename(q4 = q4_1)

survey_one <- read_csv(here("data/survey-one-data.csv")) %>%
  clean_names() %>%
  mutate(survey = "One") %>%
  mutate(q24 = "") %>%
  select(colnames(dat))

dat <- rbind(dat, survey_one)

plot_labels <- c("Spring 2020", "Fall 2020")

font_add(family = "Myriad Pro", regular = "C:/Windows/Installer/$PatchCache$/Managed/68AB67CA3301FFFF7706C0F070E41400/15.7.20033/MyriadPro_Regular.otf",
         bold = "C:/Windows/Installer/$PatchCache$/Managed/68AB67CA3301FFFF7706C0F070E41400/15.7.20033/MyriadPro_Bold.otf1")

showtext_auto()
showtext_opts(dpi = 320)

plot_theme <- theme(text = element_text(family = "Myriad Pro"),
                    plot.background = element_rect(fill = "transparent"),
                   panel.background = element_rect(fill = "transparent"),
                   panel.grid.major.y = element_line(color = "#808080", linetype = "solid"),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.ticks = element_blank(),
                   axis.line = element_line(color = "#808080"),
                   plot.title = element_text(size = 12, face = "bold", color = "#cf2d00", hjust = 0),
                   plot.subtitle = element_text(size = 14),
                   axis.title = element_blank(),
                   axis.text = element_text(size = 10, face = "bold", color = "#151515"),
                   legend.text = element_text(size = 10),
                   legend.position = "top",
                   legend.justification = "left",
                   plot.caption = element_markdown(size = 11, hjust = 0))

plot_theme_flip <- theme(text = element_text(family = "Myriad Pro"),
                    plot.background = element_rect(fill = "transparent"),
                    panel.background = element_rect(fill = "transparent"),
                    panel.grid.major.x = element_line(color = "#808080", linetype = "solid"),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.ticks = element_blank(),
                    axis.line = element_line(color = "#808080"),
                    plot.title = element_text(size = 12, face = "bold", color = "#cf2d00", hjust = 0),
                    plot.subtitle = element_text(size = 14),
                    axis.title = element_blank(),
                    axis.text = element_text(size = 10, face = "bold", color = "#151515"),
                    legend.text = element_text(size = 10),
                    legend.position = "top",
                    legend.justification = "left",
                    plot.caption = element_markdown(size = 11, hjust = 0),
                    plot.margin = unit(x = c(1, 1, 1, 1), units = "lines"))

## Impact on community
dat %>%
  tabyl(q20, survey, show_na = FALSE) %>%
  adorn_percentages(denominator = "col") %>%
  pivot_longer(cols = c("One", "Two"), names_to = "Survey", values_to = "valid_percent") %>%
  ggplot(aes(q20, valid_percent, fill = Survey)) +
  geom_col(position = position_dodge(1)) +
  geom_text(aes(y = valid_percent + 0.03, color = Survey, label = scales::percent(valid_percent, accuracy = 1)), position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
  expand_limits(y = 1) +
  labs(title = "Impact on Local Community") +
  scale_fill_manual(values = c("#003e85", "#cf2d00"), labels = plot_labels) +
  scale_color_manual(values = c("#003e85", "#cf2d00"), guide = FALSE) +
  plot_theme

ggsave(filename = "community-impact.png", device = "png", dpi = "retina",
       width = 7, height = 5, path = here("plots/"))

ggsave(filename = "community-impact.svg", device = "svg", dpi = "retina",
       width = 7, height = 5, path = here("plots/"))

## Top three negative community
denom_one <- dat %>%
  filter(survey == "One") %>%
  filter(!(is.na(q10))) %>%
  nrow()

denom_two <- dat %>%
  filter(survey == "Two") %>%
  filter(!(is.na(q10))) %>%
  nrow()

dat %>%
  mutate(q10 = str_split(q10, ",")) %>%
  unnest(cols = q10) %>%
  group_by(survey, q10) %>%
  count() %>%
  ungroup() %>%
  mutate(valid_percent = case_when(
    survey == "One" ~ n / denom_one,
    survey == "Two" ~ n / denom_two
  )) %>%
  filter(!(is.na(q10))) %>%
  ggplot(aes(reorder(q10, valid_percent), valid_percent, fill = survey)) +
  geom_col(position = position_dodge(-1)) +
  geom_text(aes(y = valid_percent + 0.02, label = scales::percent(valid_percent, accuracy = 1), color = survey), position = position_dodge(-1)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
  expand_limits(y = .8) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_fill_manual("Survey", values = c("#003e85", "#cf2d00"), labels = plot_labels) +
  scale_color_manual(values = c("#003e85", "#cf2d00"), guide = FALSE) +
  coord_flip() +
  labs(title = "Percent Citing Top Three Negative Community Effects") +
  plot_theme_flip

ggsave(filename = "negative-community.png", device = "png", dpi = "retina",
       width = 9, height = 9, path = here("plots/"))

ggsave(filename = "negative-community.svg", device = "svg", dpi = "retina",
       width = 9, height = 9, path = here("plots/"))

## When are impacts expected for communities
dat %>%
  tabyl(q12, survey, show_na = FALSE) %>%
  adorn_percentages(denominator = "col") %>%
  pivot_longer(cols = c("One", "Two"), names_to = "Survey", values_to = "valid_percent") %>%
  ggplot(aes(reorder(q12, desc(valid_percent)), valid_percent, fill = Survey)) +
  geom_col(position = position_dodge(1)) +
  geom_text(aes(y = valid_percent + 0.03, color = Survey, label = scales::percent(valid_percent, accuracy = 1)), position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 15)) +
  expand_limits(y = 1) +
  labs(title = "When Negative Impacts Are Expected for Local Communities") +
  scale_fill_manual(values = c("#003e85", "#cf2d00"), labels = plot_labels) +
  scale_color_manual(values = c("#003e85", "#cf2d00"), guide = FALSE) +
  plot_theme

ggsave(filename = "community-when-impact.png", device = "png", dpi = "retina",
       width = 8, height = 4, path = here("plots/"))

ggsave(filename = "community-when-impact.svg", device = "svg", dpi = "retina",
       width = 8, height = 4, path = here("plots/"))

## Estimated Expected Impact on Economy by NC Region
dat %>%
  group_by(prosperity_zone, survey) %>%
  summarise(avg = mean(q14_1, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!(is.na(prosperity_zone))) %>%
  mutate(prosperity_zone = str_remove(prosperity_zone, " Region")) %>%
  mutate(avg = avg * -1) %>%
  ggplot(aes(reorder(prosperity_zone, prosperity_zone), avg, fill = survey)) +
  geom_col(position = position_dodge(-1)) +
  geom_text(aes(label = glue("-{round(avg, 0)}%"), y = avg + 1.5, color = survey), position = position_dodge(-1)) +
  scale_y_continuous(labels = function(x) glue("-{x}%"), expand = c(0,0)) +
  expand_limits(y = 50) +
  scale_fill_manual("Survey", values = c("#003e85", "#cf2d00"), labels = plot_labels) +
  scale_color_manual(values = c("#003e85", "#cf2d00"), guide = FALSE) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip() +
  labs(title = "Estimated Expected Impact on Economy by NC Region") +
  plot_theme_flip

ggsave(filename = "community-expected-economic.png", device = "png", dpi = "retina",
       width = 9, height = 9, path = here("plots/"))

ggsave(filename = "community-expected-economic.svg", device = "svg", dpi = "retina",
       width = 9, height = 9, path = here("plots/"))

## Estimated Expected Impact on Local Employment by NC Region
dat %>%
  group_by(prosperity_zone, survey) %>%
  summarise(avg = mean(q14_2, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!(is.na(prosperity_zone))) %>%
  mutate(prosperity_zone = str_remove(prosperity_zone, " Region")) %>%
  mutate(avg = avg * -1) %>%
  ggplot(aes(reorder(prosperity_zone, prosperity_zone), avg, fill = survey)) +
  geom_col(position = position_dodge(-1)) +
  geom_text(aes(label = glue("-{round(avg, 0)}%"), y = avg + 1, color = survey), position = position_dodge(-1)) +
  scale_y_continuous(labels = function(x) glue("-{x}%"), expand = c(0,0)) +
  expand_limits(y = 40) +
  scale_fill_manual("Survey", values = c("#003e85", "#cf2d00"), labels = plot_labels) +
  scale_color_manual(values = c("#003e85", "#cf2d00"), guide = FALSE) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip() +
  labs(title = "Estimated Expected Impact on Local Employment by NC Region") +
  plot_theme_flip

ggsave(filename = "community-expected-employment.png", device = "png", dpi = "retina",
       width = 9, height = 9, path = here("plots/"))

ggsave(filename = "community-expected-employment.svg", device = "svg", dpi = "retina",
       width = 9, height = 9, path = here("plots/"))

## Impact on local government
dat %>%
  tabyl(q5, survey, show_na = FALSE) %>%
  adorn_percentages(denominator = "col") %>%
  pivot_longer(cols = c("One", "Two"), names_to = "Survey", values_to = "valid_percent") %>%
  ggplot(aes(q5, valid_percent, fill = Survey)) +
  geom_col(position = position_dodge(1)) +
  geom_text(aes(y = valid_percent + 0.03, color = Survey, label = scales::percent(valid_percent, accuracy = 1)), position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
  expand_limits(y = 1) +
  labs(title = "Impact on Local Government") +
  scale_fill_manual(values = c("#003e85", "#cf2d00"), labels = plot_labels) +
  scale_color_manual(values = c("#003e85", "#cf2d00"), guide = FALSE) +
  plot_theme

ggsave(filename = "local-gov-impact.png", device = "png", dpi = "retina",
       width = 6, height = 4, path = here("plots/"))

ggsave(filename = "local-gov-impact.svg", device = "svg", dpi = "retina",
       width = 6, height = 4, path = here("plots/"))

## Top three negative local gov
denom_one <- dat %>%
  filter(survey == "One") %>%
  filter(!(is.na(q6))) %>%
  nrow()

denom_two <- dat %>%
  filter(survey == "Two") %>%
  filter(!(is.na(q6))) %>%
  nrow()

dat %>%
  mutate(q6 = str_split(q6, ",")) %>%
  unnest(cols = q6) %>%
  group_by(survey, q6) %>%
  count() %>%
  ungroup() %>%
  mutate(valid_percent = case_when(
    survey == "One" ~ n / denom_one,
    survey == "Two" ~ n / denom_two
  )) %>%
  filter(!(is.na(q6))) %>%
  ggplot(aes(reorder(q6, valid_percent), valid_percent, fill = survey)) +
  geom_col(position = position_dodge(-1)) +
  geom_text(aes(y = valid_percent + 0.03, label = scales::percent(valid_percent, accuracy = 1), color = survey), position = position_dodge(-1)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
  expand_limits(y = 1) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35)) +
  scale_fill_manual("Survey", values = c("#003e85", "#cf2d00"), labels = plot_labels) +
  scale_color_manual(values = c("#003e85", "#cf2d00"), guide = FALSE) +
  coord_flip() +
  labs(title = "Percent Citing Top Three Negative Effects") +
  plot_theme_flip

ggsave(filename = "negative-local-gov.png", device = "png", dpi = "retina",
       width = 9, height = 9, path = here("plots/"))

ggsave(filename = "negative-local-gov.svg", device = "svg", dpi = "retina",
       width = 9, height = 9, path = here("plots/"))

## When are impacts expected for local gov
dat %>%
  tabyl(q9, survey, show_na = FALSE) %>%
  adorn_percentages(denominator = "col") %>%
  pivot_longer(cols = c("One", "Two"), names_to = "Survey", values_to = "valid_percent") %>%
  ggplot(aes(reorder(q9, desc(valid_percent)), valid_percent, fill = Survey)) +
  geom_col(position = position_dodge(1)) +
  geom_text(aes(y = valid_percent + 0.03, color = Survey, label = scales::percent(valid_percent, accuracy = 1)), position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 15)) +
  expand_limits(y = 1) +
  labs(title = "When Negative Impacts Are Expected for Local Government") +
  scale_fill_manual(values = c("#003e85", "#cf2d00"), labels = plot_labels) +
  scale_color_manual(values = c("#003e85", "#cf2d00"), guide = FALSE) +
  plot_theme

ggsave(filename = "local-gov-when-impact.png", device = "png", dpi = "retina",
       width = 8, height = 4, path = here("plots/"))

ggsave(filename = "local-gov-when-impact.svg", device = "svg", dpi = "retina",
       width = 8, height = 4, path = here("plots/"))

## Estimated Average Revenue Impact on Local Gov
dat %>%
  group_by(prosperity_zone, survey) %>%
  summarise(avg = mean(q7_1, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!(is.na(prosperity_zone))) %>%
  mutate(prosperity_zone = str_remove(prosperity_zone, " Region")) %>%
  mutate(avg = avg * -1) %>%
  ggplot(aes(reorder(prosperity_zone, prosperity_zone), avg, fill = survey)) +
  geom_col(position = position_dodge(-1)) +
  geom_text(aes(label = glue("-{round(avg, 0)}%"), y = avg + 1, color = survey), position = position_dodge(-1)) +
  scale_y_continuous(labels = function(x) glue("-{x}%"), expand = c(0,0)) +
  expand_limits(y = 40) +
  scale_fill_manual("Survey", values = c("#003e85", "#cf2d00"), labels = plot_labels) +
  scale_color_manual(values = c("#003e85", "#cf2d00"), guide = FALSE) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip() +
  labs(title = "Estimated Average Revenue Impact on Local Government") +
  plot_theme_flip

ggsave(filename = "local-gov-expected-revenue.png", device = "png", dpi = "retina",
       width = 9, height = 9, path = here("plots/"))

ggsave(filename = "local-gov-expected-revenue.svg", device = "svg", dpi = "retina",
       width = 9, height = 9, path = here("plots/"))

## Positive Impacts on Organization
dat %>%
  mutate(q19 = str_remove(q19, " \\(Explain below\\)")) %>%
  tabyl(q19, survey, show_na = FALSE) %>%
  adorn_percentages(denominator = "col") %>%
  pivot_longer(cols = c("One", "Two"), names_to = "Survey", values_to = "valid_percent") %>%
  ggplot(aes(reorder(q19, desc(valid_percent)), valid_percent, fill = Survey)) +
  geom_col(position = position_dodge(1)) +
  geom_text(aes(y = valid_percent + 0.03, color = Survey, label = scales::percent(valid_percent, accuracy = 1)), position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 15)) +
  expand_limits(y = 1) +
  labs(title = "Positive Impacts of Crisis On Local Government") +
  scale_fill_manual(values = c("#003e85", "#cf2d00"), labels = plot_labels) +
  scale_color_manual(values = c("#003e85", "#cf2d00"), guide = FALSE) +
  plot_theme

ggsave(filename = "local-gov-positive-impact.png", device = "png", dpi = "retina",
       width = 8, height = 4, path = here("plots/"))

ggsave(filename = "local-gov-positive-impact.svg", device = "svg", dpi = "retina",
       width = 8, height = 4, path = here("plots/"))

## Positive Impacts on Community
dat %>%
  mutate(q21 = str_remove(q21, " \\(Explain below\\)")) %>%
  tabyl(q21, survey, show_na = FALSE) %>%
  adorn_percentages(denominator = "col") %>%
  pivot_longer(cols = c("One", "Two"), names_to = "Survey", values_to = "valid_percent") %>%
  ggplot(aes(reorder(q21, desc(valid_percent)), valid_percent, fill = Survey)) +
  geom_col(position = position_dodge(1)) +
  geom_text(aes(y = valid_percent + 0.03, color = Survey, label = scales::percent(valid_percent, accuracy = 1)), position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 15)) +
  expand_limits(y = 1) +
  labs(title = "Positive Impacts of Crisis On Community") +
  scale_fill_manual(values = c("#003e85", "#cf2d00"), labels = plot_labels) +
  scale_color_manual(values = c("#003e85", "#cf2d00"), guide = FALSE) +
  plot_theme

ggsave(filename = "community-positive-impact.png", device = "png", dpi = "retina",
       width = 8, height = 4, path = here("plots/"))

ggsave(filename = "community-positive-impact.svg", device = "svg", dpi = "retina",
       width = 8, height = 4, path = here("plots/"))

## Estimated Average Employment Impact on Local Gov
dat %>%
  group_by(prosperity_zone, survey) %>%
  summarise(avg = mean(q7_2, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!(is.na(prosperity_zone))) %>%
  mutate(prosperity_zone = str_remove(prosperity_zone, " Region")) %>%
  mutate(avg = avg * -1) %>%
  ggplot(aes(reorder(prosperity_zone, prosperity_zone), avg, fill = survey)) +
  geom_col(position = position_dodge(-1)) +
  geom_text(aes(label = glue("-{round(avg, 0)}%"), y = avg + 1, color = survey), position = position_dodge(-1)) +
  scale_y_continuous(labels = function(x) glue("-{x}%"), expand = c(0,0)) +
  expand_limits(y = 25) +
  scale_fill_manual("Survey", values = c("#003e85", "#cf2d00"), labels = plot_labels) +
  scale_color_manual(values = c("#003e85", "#cf2d00"), guide = FALSE) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip() +
  labs(title = "Estimated Average Employment Impact on Local Government") +
  plot_theme_flip

ggsave(filename = "local-gov-expected-employment.png", device = "png", dpi = "retina",
       width = 9, height = 9, path = here("plots/"))

ggsave(filename = "local-gov-expected-employment.svg", device = "svg", dpi = "retina",
       width = 9, height = 9, path = here("plots/"))

## Difference in survey respondents
subgroups <- c("juri_type", "q18", "county_tier", "prosperity_zone", "cog")

grouped_dat <- dat %>%
  mutate(juri_type = case_when(
    str_length(coded_geoid) > 5 ~ "Municipality",
    str_length(coded_geoid) <= 5 ~ "County"
  )) %>%
  mutate(county_tier = as.character(county_tier)) %>%
  pivot_longer(cols = all_of(subgroups), names_to = "subgroup", values_to = "subgroup_value") %>%
  mutate(subgroup_value = ifelse(is.na(subgroup_value), "Unknown", subgroup_value)) %>%
  filter(subgroup_value != "Unknown") %>%
  group_by(survey, subgroup, subgroup_value) %>%
  count() %>%
  ungroup() %>%
  group_by(survey, subgroup) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = "survey", values_from = c("n", "pct")) %>%
  mutate(diff = pct_Two - pct_One) %>%
  clean_names()
  
grouped_dat %>%
  pivot_longer(cols = c("pct_one", "pct_two")) %>%
  filter(subgroup == "prosperity_zone") %>%
  mutate(subgroup_value = str_remove(subgroup_value, " Region")) %>%
  ggplot(aes(x = reorder(subgroup_value, -value), y = value, fill = name, color = name)) +
  geom_col(position = position_dodge(-1)) +
  geom_text(aes(y = value + 0.01, label = scales::percent(value, accuracy = 1)), position = position_dodge(-1)) +
  scale_fill_manual("Survey", values = c("#003e85", "#cf2d00"), labels = plot_labels) +
  scale_color_manual(values = c("#003e85", "#cf2d00"), guide = FALSE) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand =  c(0,0)) +
  expand_limits(y = .25) +
  coord_flip() +
  labs(title = "Response Percentages by Prosperity Zone") +
  plot_theme_flip

ggsave(filename = "prosp-response-diff.png", device = "png", dpi = "retina",
       width = 9, height = 9, path = here("plots/"))

ggsave(filename = "prosp-response-diff.svg", device = "svg", dpi = "retina",
       width = 9, height = 9, path = here("plots/"))

grouped_dat %>%
  pivot_longer(cols = c("pct_one", "pct_two")) %>%
  filter(subgroup == "juri_type") %>%
  mutate(subgroup_value = str_remove(subgroup_value, " Region")) %>%
  ggplot(aes(x = reorder(subgroup_value, -value), y = value, fill = name, color = name)) +
  geom_col(position = position_dodge(1)) +
  geom_text(aes(y = value + 0.02, label = scales::percent(value, accuracy = 1)), position = position_dodge(1)) +
  scale_fill_manual("Survey", values = c("#003e85", "#cf2d00"), labels = plot_labels) +
  scale_color_manual(values = c("#003e85", "#cf2d00"), guide = FALSE) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand =  c(0,0)) +
  expand_limits(y = .80) +
  labs(title = "Response Percentages by Jurisdiction Type") +
  plot_theme

ggsave(filename = "juri-response-diff.png", device = "png", dpi = "retina",
       width = 9, height = 6, path = here("plots/"))

ggsave(filename = "juri-response-diff.svg", device = "svg", dpi = "retina",
       width = 9, height = 6, path = here("plots/"))


showtext_auto(enable = FALSE)
