library(tidyverse)

rm(list = ls())

raw_data_df <- list.files("raw", full.names = TRUE) %>% 
  map(~ {load(.); raw_data}) %>% 
  reduce(c) %>% 
  bind_rows() %>% 
  mutate(data = map(data, 2))

trade_df <- raw_data_df %>%
  unnest(data) %>% 
  select(time = yr, product = id, direction = rgDesc, geo_from = rt3ISO, geo_to = pt3ISO, value = TradeValue) %>% 
  mutate(
    direction = str_to_lower(direction),
    value = as.numeric(value)
  ) %>% 
  distinct() %>% 
  filter(!is.na(geo_to)) %>% # TODO ez mekkora arányt jelent
  pivot_wider(names_from = direction, values_from = value, values_fill = 0) %>% 
  mutate(
    netto_export = export - `re-export`,
    netto_import = import - `re-import`
  ) %>% 
  janitor::clean_names() %>% 
  pivot_longer(import:last_col(), names_to = "direction")

trade_avg_df <- trade_df %>% # compare export and import
  filter(direction == "netto_export") %>% 
  select(- direction) %>% 
  rename(export = value) %>% 
  left_join(
    trade_df %>% 
      filter(direction == "netto_import") %>% 
      select(- direction) %>% 
      rename(geo_to = 3, geo_from = 4, import = value)
  ) %>% 
  mutate_at(vars(export:import), ~ ifelse(. == 0, NA, .)) %>% # zero treated as NA
  mutate(
    import = ifelse(is.na(import), export, import),
    export = ifelse(is.na(export), import, export),
    value = (import + export) / 2
  ) %>% 
  select(- export, - import)

netto_export_df <- trade_avg_df %>% 
  rename(export = value) %>% 
  left_join(
    trade_avg_df %>% 
      rename(geo_to = 3, geo_from = 4, import = value)
  ) %>% 
  replace_na(list(export = 0, import = 0)) %>% 
  mutate(netto_export = export - import) %>% 
  filter(geo_to != "WLD", geo_from != "WLD")


save.image(file = "data/trade_data.RData")