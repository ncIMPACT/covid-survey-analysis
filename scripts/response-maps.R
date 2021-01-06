library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(showtext)
library(ggtext)
library(janitor)
library(patchwork)
library(classInt)
library(glue)
library(lubridate)
library(here)

font_add(family = "Myriad Pro", regular = "C:/Windows/Installer/$PatchCache$/Managed/68AB67CA3301FFFF7706C0F070E41400/15.7.20033/MyriadPro_Regular.otf",
         bold = "C:/Windows/Installer/$PatchCache$/Managed/68AB67CA3301FFFF7706C0F070E41400/15.7.20033/MyriadPro_Bold.otf1")

showtext_auto()
showtext_opts(dpi = 320)

yel_pal <- c("#FCCB41","#FDBF11","#E88E2D","#CA5800","#843215")

map_theme <- theme(text = element_text(family = "Myriad Pro"),
                   plot.background = element_rect(fill = "transparent"),
                   panel.background = element_rect(fill = "transparent"),
                   panel.grid = element_blank(),
                   axis.ticks = element_blank(),
                   plot.title = element_markdown(size = 18, face = "bold", color = "#cf2d00"),
                   plot.subtitle = element_markdown(size = 14),
                   axis.title = element_text(size = 12, face = "italic"),
                   axis.text = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "top",
                   legend.justification = "left",
                   plot.caption = element_markdown(size = 11, hjust = 0))

logo <- png::readPNG("C:/Users/jsnjns/OneDrive - University of North Carolina at Chapel Hill/r-projects/covid-keys-impact/logo/black-logo-long.png", native = TRUE)

nc_counties <- counties(state = 37)

nc_places <- places(state = 37)

dat <- read_csv(here("data/final-clean-data.csv")) %>% 
  mutate(survey = "Survey Two") %>%
  rename(q4 = q4_1)

survey_one <- read_csv(here("data/survey-one-data.csv")) %>%
  clean_names() %>%
  mutate(survey = "Survey One") %>%
  mutate(q24 = "") %>%
  select(colnames(dat))

dat <- rbind(dat, survey_one)

dat %>%
  mutate(type = case_when(
    str_length(coded_geoid) > 5 ~ "Municipality",
    str_length(coded_geoid) <= 5 ~ "County"
  )) %>%
  filter(coded_geoid != "Unknown") -> resp_dat

x <- resp_dat %>%
  filter(type == "Municipality") %>%
  left_join(nc_places, by = c("coded_geoid" = "GEOID")) %>%
  select(coded_short_name, coded_geoid, geometry, survey) %>%
  st_as_sf() %>%
  st_join(nc_counties) %>%
  as_tibble() %>%
  group_by(GEOID, survey) %>%
  count(name = "responses") 

final <- resp_dat %>%
  filter(type == "County") %>%
  select(coded_geoid, survey) %>%
  group_by(coded_geoid, survey) %>%
  count(name = "responses") %>%
  rename(GEOID = coded_geoid) %>%
  rbind(x) %>%
  group_by(GEOID, survey) %>%
  summarise(responses = sum(responses)) %>%
  left_join(nc_counties)

natural_breaks <- classIntervals(final$responses, 5, style = "jenks")

final_map <- final %>%
  mutate(natural_breaks = cut(responses, breaks = c(0, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
  mutate(upper = str_extract(natural_breaks, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(natural_breaks, "(?<=,)[^\\]]*")) %>%
  mutate(upper = ifelse(upper == "0", "0", upper)) %>%
  mutate(label = glue("{as.integer(upper) + 1} to {lower}"))

final_map %>%
  filter(!(is.na(natural_breaks))) %>%
  as_tibble() %>%
  distinct(natural_breaks, .keep_all = T) %>%
  select(natural_breaks, label) %>%
  arrange(natural_breaks) %>%
  pull(label) -> legend_labels

p1 <- final_map %>%
  mutate(survey = case_when(
    survey == "Survey One" ~ "Spring 2020",
    survey == "Survey Two" ~ "Fall 2020"
  )) %>%
  mutate(survey = factor(survey, levels = c("Spring 2020", "Fall 2020"))) %>%
  st_as_sf() %>%
  ggplot() + 
  geom_sf(data = nc_counties, fill = "white", color = "black") +
  geom_sf(aes(fill = natural_breaks), color = "black") +
  scale_fill_manual(values = yel_pal, labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "<b style='color:#CA5800;'>COVID-19 Survey of Local Governments: Responses by County</b>") +
  facet_wrap(~survey, nrow = 2, ncol = 1) +
  map_theme +
  theme(strip.background = element_rect(fill = "#cf2d00"),
        strip.text = element_text(family = "Myriad Pro", face = "bold", color = "white", size = 14))

p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

p1

ggsave(filename = "response-map.png", device = "png",
       path = "C:/Users/jsnjns/Desktop/", dpi = "retina", width = 16, height = 16)

ggsave(filename = "response-map.svg", device = "svg",
       path = "C:/Users/jsnjns/Desktop/", dpi = "retina", width = 16, height = 16)

###### New Legislators Presentation

map_theme <- theme(text = element_text(family = "Arial"),
                   plot.background = element_rect(fill = "transparent"),
                   panel.background = element_rect(fill = "transparent"),
                   panel.grid = element_blank(),
                   axis.ticks = element_blank(),
                   plot.title = element_markdown(size = 18, face = "bold"),
                   plot.subtitle = element_markdown(size = 14),
                   axis.title = element_text(size = 12, face = "italic"),
                   axis.text = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "top",
                   legend.justification = "left",
                   plot.caption = element_markdown(size = 11, hjust = 0))

p1 <- final_map %>%
  filter(survey == "Survey One") %>%
  st_as_sf() %>%
  ggplot() + 
  geom_sf(data = nc_counties, fill = "white", color = "black") +
  geom_sf(aes(fill = natural_breaks), color = "black") +
  scale_fill_manual(values = yel_pal, labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  ggtitle("Survey One") +
  map_theme

p2 <- final_map %>%
  filter(survey == "Survey Two") %>%
  st_as_sf() %>%
  ggplot() + 
  geom_sf(data = nc_counties, fill = "white", color = "black") +
  geom_sf(aes(fill = natural_breaks), color = "black") +
  scale_fill_manual(values = yel_pal, labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  ggtitle("Survey Two") +
  map_theme

(p1/p2)
  

ggsave(filename = "response-map.png", device = "png",
       path = "C:/Users/jsnjns/Desktop/", dpi = "retina", width = 15, height = 15)

p1

ggsave(filename = "response-one-map.png", device = "png",
       path = "C:/Users/jsnjns/Desktop/", dpi = "retina", width = 16, height = 9)

p2

ggsave(filename = "response-two-map.png", device = "png",
       path = "C:/Users/jsnjns/Desktop/", dpi = "retina", width = 16, height = 9)

showtext_auto(enable = FALSE)
