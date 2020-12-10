library(tidyverse)
library(ggtext)
library(extrafont)
library(here)
library(janitor)
library(glue)
library(gt)

dat <- read_csv(here("data/final-clean-data.csv")) %>% 
  mutate(survey = "Survey Two") %>%
  rename(q4 = q4_1)

survey_one <- read_csv(here("data/survey-one-data.csv")) %>%
  clean_names() %>%
  mutate(survey = "Survey One") %>%
  mutate(q24 = "") %>%
  select(colnames(dat))

dat <- rbind(dat, survey_one) %>%
  mutate(juri_type = case_when(
    str_length(coded_geoid) > 5 ~ "Municipality",
    str_length(coded_geoid) <= 5 ~ "County"
  ))

## Unique responses
dat %>%
  filter(survey == "Survey One") %>%
  distinct(coded_geoid, .keep_all = TRUE) %>%
  group_by(juri_type) %>% count()

dat %>%
  filter(survey == "Survey Two") %>%
  distinct(coded_geoid, .keep_all = TRUE) %>%
  group_by(juri_type) %>% count()


## Jurisdiction breakdown
dat %>%
  group_by(survey, juri_type) %>%
  count() %>%
  ungroup() %>%
  group_by(survey) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  arrange(desc(pct)) %>%
  group_by(survey) %>%
  gt(rowname_col = "juri_type") %>%
  fmt_percent(columns = vars(pct), decimals = 1) %>%
  cols_label(.list = list(juri_type = "Jurisdiction", n = "Responses", pct = "Percent")) %>%
  tab_style(style = list(cell_text(font = "Lato", weight = "bold"),
                         cell_borders(sides = "bottom", weight = px(3))),
            locations = list(cells_row_groups())) %>%
  tab_style(style = list(cell_text(font = "Lato", weight = "bold")),
            locations = list(cells_column_labels(columns = vars(n, pct)))) %>%
  tab_header(title = html("<b><i>Table 1:</b></i> Survey Responses By Jurisdiction Type")) %>%
  tab_options(table.width = pct(60))

## Prosperity Zone Breakdown
dat %>%
  group_by(survey, prosperity_zone) %>%
  count() %>%
  ungroup() %>%
  group_by(survey) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(prosperity_zone = ifelse(is.na(prosperity_zone), "Unknown", prosperity_zone)) %>%
  arrange(desc(pct)) %>%
  group_by(survey) %>%
  gt(rowname_col = "prosperity_zone") %>%
  fmt_percent(columns = vars(pct), decimals = 1) %>%
  cols_label(.list = list(prosperity_zone = "Jurisdiction", n = "Responses", pct = "Percent")) %>%
  tab_style(style = list(cell_text(font = "Lato", weight = "bold"),
                         cell_borders(sides = "bottom", weight = px(3))),
            locations = list(cells_row_groups())) %>%
  tab_style(style = list(cell_text(font = "Lato", weight = "bold")),
            locations = list(cells_column_labels(columns = vars(n, pct)))) %>%
  tab_header(title = html("<b><i>Table 2:</b></i> Survey Responses By Prosperity Zone")) %>%
  tab_options(table.width = pct(60))

## Councils of Government Breakdown
dat %>%
  group_by(survey, cog) %>%
  count() %>%
  ungroup() %>%
  group_by(survey) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(cog = ifelse(is.na(cog), "Unknown", cog)) %>%
  arrange(desc(pct)) %>%
  group_by(survey) %>%
  gt(rowname_col = "cog") %>%
  fmt_percent(columns = vars(pct), decimals = 1) %>%
  cols_label(.list = list(cog = "Jurisdiction", n = "Responses", pct = "Percent")) %>%
  tab_style(style = list(cell_text(font = "Lato", weight = "bold"),
                         cell_borders(sides = "bottom", weight = px(3))),
            locations = list(cells_row_groups())) %>%
  tab_style(style = list(cell_text(font = "Lato", weight = "bold")),
            locations = list(cells_column_labels(columns = vars(n, pct)))) %>%
  tab_header(title = html("<b><i>Table 3:</b></i> Survey Responses By Councils of Government")) %>%
  tab_options(table.width = pct(60))

## 
dat %>%
  group_by(survey, q18, q20) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from = survey, values_from = n) %>%
  filter(!(is.na(q20))) %>%
  mutate(across(starts_with("Survey"), .fns = ~replace_na(., 0))) %>%
  group_by(q18) %>%
  mutate(`Survey One` = `Survey One` / sum(`Survey One`)) %>%
  mutate(`Survey Two` = `Survey Two` / sum(`Survey Two`)) %>%
  arrange(desc(`Survey Two`)) %>%
  gt(rowname_col = "q20") %>%
  fmt_percent(columns = vars(`Survey One`, `Survey Two`), decimals = 1) %>%
  tab_style(style = list(cell_text(size = "large", weight = "bold"),
                         cell_borders(sides = "bottom", weight = px(3))),
            locations = list(cells_row_groups())) %>%
  tab_style(style = list(cell_text(align = "right"),
                         cell_borders(sides = "right", weight = px(3))),
            locations = list(cells_stub())) %>%
  tab_style(style = list(cell_text(weight = "bold", align = "center")),
            locations = list(cells_column_labels(columns = vars(`Survey One`, `Survey Two`)))) %>%
  data_color(columns = vars(`Survey One`), colors = RColorBrewer::brewer.pal(5, "Blues"), apply_to = "fill") %>%
  data_color(columns = vars(`Survey Two`), colors = RColorBrewer::brewer.pal(5, "Oranges"), apply_to = "fill") %>%
  tab_options(table.font.names = "Arial", table.width = pct(60)) %>%
  tab_header(title = html("Impact on Local Community by Respondent Classification")) %>%
  tab_source_note(source_note = html("<b>Source:</b> ncIMPACT COVID-19 surveys of NC local governments. Survey one conducted April - May and survey two conducted October - December 2020"))

dat %>%
  group_by(survey, juri_type, q20) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from = survey, values_from = n) %>%
  filter(!(is.na(q20))) %>%
  mutate(across(starts_with("Survey"), .fns = ~replace_na(., 0))) %>%
  group_by(juri_type) %>%
  mutate(`Survey One` = `Survey One` / sum(`Survey One`)) %>%
  mutate(`Survey Two` = `Survey Two` / sum(`Survey Two`)) %>%
  arrange(desc(`Survey Two`)) %>%
  gt(rowname_col = "q20") %>%
  fmt_percent(columns = vars(`Survey One`, `Survey Two`), decimals = 1) %>%
  tab_style(style = list(cell_text(size = "large", weight = "bold"),
                         cell_borders(sides = "bottom", weight = px(3))),
            locations = list(cells_row_groups())) %>%
  tab_style(style = list(cell_text(align = "right"),
                         cell_borders(sides = "right", weight = px(3))),
            locations = list(cells_stub())) %>%
  tab_style(style = list(cell_text(weight = "bold", align = "center")),
            locations = list(cells_column_labels(columns = vars(`Survey One`, `Survey Two`)))) %>%
  data_color(columns = vars(`Survey One`), colors = RColorBrewer::brewer.pal(5, "Blues"), apply_to = "fill") %>%
  data_color(columns = vars(`Survey Two`), colors = RColorBrewer::brewer.pal(5, "Oranges"), apply_to = "fill") %>%
  tab_options(table.font.names = "Arial", table.width = pct(60)) %>%
  tab_header(title = html("Impact on Local Community by Respondent Classification")) %>%
  tab_source_note(source_note = html("<b>Source:</b> ncIMPACT COVID-19 surveys of NC local governments. Survey one conducted April - May and survey two conducted October - December 2020"))

dat %>%
  group_by(survey, prosperity_zone, q20) %>%
  count() %>%
  ungroup() %>%
  filter(!(is.na(prosperity_zone))) %>%
  pivot_wider(names_from = survey, values_from = n) %>%
  filter(!(is.na(q20))) %>%
  mutate(across(starts_with("Survey"), .fns = ~replace_na(., 0))) %>%
  group_by(prosperity_zone) %>%
  mutate(`Survey One` = `Survey One` / sum(`Survey One`)) %>%
  mutate(`Survey Two` = `Survey Two` / sum(`Survey Two`)) %>%
  arrange(desc(`Survey Two`)) %>%
  gt(rowname_col = "q20") %>%
  fmt_percent(columns = vars(`Survey One`, `Survey Two`), decimals = 1) %>%
  tab_style(style = list(cell_text(size = "large", weight = "bold"),
                         cell_borders(sides = "bottom", weight = px(3))),
            locations = list(cells_row_groups())) %>%
  tab_style(style = list(cell_text(align = "right"),
                         cell_borders(sides = "right", weight = px(3))),
            locations = list(cells_stub())) %>%
  tab_style(style = list(cell_text(weight = "bold", align = "center")),
            locations = list(cells_column_labels(columns = vars(`Survey One`, `Survey Two`)))) %>%
  data_color(columns = vars(`Survey One`), colors = RColorBrewer::brewer.pal(5, "Blues"), apply_to = "fill") %>%
  data_color(columns = vars(`Survey Two`), colors = RColorBrewer::brewer.pal(5, "Oranges"), apply_to = "fill") %>%
  tab_options(table.font.names = "Arial", table.width = pct(60)) %>%
  tab_header(title = html("Impact on Local Community by Respondent Classification")) %>%
  tab_source_note(source_note = html("<b>Source:</b> ncIMPACT COVID-19 surveys of NC local governments. Survey one conducted April - May and survey two conducted October - December 2020"))



################## Impact on Employment Breakdown
dat %>%
  group_by(survey, q18) %>%
  summarise(avg = mean(q7_2, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = survey, values_from = avg) %>%
  mutate(across(starts_with("Survey"), .fns = ~replace_na(., 0))) %>%
  arrange(`Survey Two`) %>%
  gt() %>%
  fmt_percent(columns = vars(`Survey One`, `Survey Two`), decimals = 1, scale_values = FALSE) %>%
  tab_style(style = list(cell_text(size = "large", weight = "bold"),
                         cell_borders(sides = "bottom", weight = px(3))),
            locations = list(cells_row_groups())) %>%
  tab_style(style = list(cell_text(align = "right"),
                         cell_borders(sides = "right", weight = px(3))),
            locations = list(cells_stub())) %>%
  tab_style(style = list(cell_text(weight = "bold", align = "center")),
            locations = list(cells_column_labels(columns = vars(`Survey One`, `Survey Two`)))) %>%
  data_color(columns = vars(`Survey One`), colors = RColorBrewer::brewer.pal(5, "Blues"), apply_to = "fill") %>%
  data_color(columns = vars(`Survey Two`), colors = RColorBrewer::brewer.pal(5, "Oranges"), apply_to = "fill") %>%
  tab_options(table.font.names = "Arial", table.width = pct(60)) %>%
  tab_header(title = html("Impact on Local Community by Respondent Classification")) %>%
  tab_source_note(source_note = html("<b>Source:</b> ncIMPACT COVID-19 surveys of NC local governments. Survey one conducted April - May and survey two conducted October - December 2020"))

dat %>%
  group_by(survey, juri_type, q7_2) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from = survey, values_from = n) %>%
  filter(!(is.na(q7_2))) %>%
  mutate(across(starts_with("Survey"), .fns = ~replace_na(., 0))) %>%
  group_by(juri_type) %>%
  mutate(`Survey One` = `Survey One` / sum(`Survey One`)) %>%
  mutate(`Survey Two` = `Survey Two` / sum(`Survey Two`)) %>%
  arrange(desc(`Survey Two`)) %>%
  gt(rowname_col = "q7_2") %>%
  fmt_percent(columns = vars(`Survey One`, `Survey Two`), decimals = 1) %>%
  tab_style(style = list(cell_text(size = "large", weight = "bold"),
                         cell_borders(sides = "bottom", weight = px(3))),
            locations = list(cells_row_groups())) %>%
  tab_style(style = list(cell_text(align = "right"),
                         cell_borders(sides = "right", weight = px(3))),
            locations = list(cells_stub())) %>%
  tab_style(style = list(cell_text(weight = "bold", align = "center")),
            locations = list(cells_column_labels(columns = vars(`Survey One`, `Survey Two`)))) %>%
  data_color(columns = vars(`Survey One`), colors = RColorBrewer::brewer.pal(5, "Blues"), apply_to = "fill") %>%
  data_color(columns = vars(`Survey Two`), colors = RColorBrewer::brewer.pal(5, "Oranges"), apply_to = "fill") %>%
  tab_options(table.font.names = "Arial", table.width = pct(60)) %>%
  tab_header(title = html("Impact on Local Community by Respondent Classification")) %>%
  tab_source_note(source_note = html("<b>Source:</b> ncIMPACT COVID-19 surveys of NC local governments. Survey one conducted April - May and survey two conducted October - December 2020"))

dat %>%
  group_by(survey, prosperity_zone, q7_2) %>%
  count() %>%
  ungroup() %>%
  filter(!(is.na(prosperity_zone))) %>%
  pivot_wider(names_from = survey, values_from = n) %>%
  filter(!(is.na(q7_2))) %>%
  mutate(across(starts_with("Survey"), .fns = ~replace_na(., 0))) %>%
  group_by(prosperity_zone) %>%
  mutate(`Survey One` = `Survey One` / sum(`Survey One`)) %>%
  mutate(`Survey Two` = `Survey Two` / sum(`Survey Two`)) %>%
  arrange(desc(`Survey Two`)) %>%
  gt(rowname_col = "q7_2") %>%
  fmt_percent(columns = vars(`Survey One`, `Survey Two`), decimals = 1) %>%
  tab_style(style = list(cell_text(size = "large", weight = "bold"),
                         cell_borders(sides = "bottom", weight = px(3))),
            locations = list(cells_row_groups())) %>%
  tab_style(style = list(cell_text(align = "right"),
                         cell_borders(sides = "right", weight = px(3))),
            locations = list(cells_stub())) %>%
  tab_style(style = list(cell_text(weight = "bold", align = "center")),
            locations = list(cells_column_labels(columns = vars(`Survey One`, `Survey Two`)))) %>%
  data_color(columns = vars(`Survey One`), colors = RColorBrewer::brewer.pal(5, "Blues"), apply_to = "fill") %>%
  data_color(columns = vars(`Survey Two`), colors = RColorBrewer::brewer.pal(5, "Oranges"), apply_to = "fill") %>%
  tab_options(table.font.names = "Arial", table.width = pct(60)) %>%
  tab_header(title = html("Impact on Local Community by Respondent Classification")) %>%
  tab_source_note(source_note = html("<b>Source:</b> ncIMPACT COVID-19 surveys of NC local governments. Survey one conducted April - May and survey two conducted October - December 2020"))












################ SUPPLEMENTAL

dat %>%
  filter(survey == "Two") %>%
  tabyl(q20, q18) %>%
  adorn_totals(where = "col") %>%
  adorn_percentages(denominator = "col") %>%
  rename("Response" = q20) %>%
  mutate(Response = ifelse(is.na(Response), "No Response", Response)) %>%
  gt() %>%
  data_color(columns = vars(Total),
             colors = RColorBrewer::brewer.pal(5, "YlGnBu")) %>%
  fmt_percent(columns = 2:5) %>%
  tab_style(
    style = list(
      cell_text(
        font = "Lato",
        align = "center"
      )
    ),
    locations = list(
      cells_body(columns = vars(`Elected Official`, Manager, Other, Total))
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = vars(Total)
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(font = "Lato", align = "center", weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = list(cell_text(font = "Lato", weight = "bold")),
    locations = cells_body(columns = vars(Response))
  ) %>%
  tab_options(table.width = pct(70))


dat %>%
  group_by(q20, q18, survey) %>%
  count() %>%
  ungroup() %>%
  filter(!(is.na(q20))) %>%
  pivot_wider(names_from = survey, values_from = n) %>%
  arrange(q18) %>%
  replace_na(list(One = 0, Two = 0)) %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")

dat %>%
  tabyl(q20, survey, q18, show_na = FALSE) %>%
  adorn_totals() %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns()
