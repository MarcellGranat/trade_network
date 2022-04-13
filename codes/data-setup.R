rm(list = ls())

raw_data_df <- list.files("raw", full.names = TRUE) %>% 
  keep(str_detect, "total|5_10", negate = TRUE) %>%
  map(~ {load(.); raw_data}) %>% 
  reduce(c) %>% 
  bind_rows() %>% 
  mutate(data = map(data, 2))

load("raw/data_total.RData")
load("raw/data_5_10.rdata")

trade_df <- raw_data_df %>%
  unnest(data) %>% 
  bind_rows(
    map_df(raw_data_total$data, 2) %>% 
  mutate(id = "total"), 
  map_df(raw_data_5_10$data, 2)
    ) %>% 
  select(time = yr, product = id, direction = rgDesc, geo_from = rt3ISO, geo_to = pt3ISO, value = TradeValue) %>% 
  mutate(
    direction = str_to_lower(direction),
    value = as.numeric(value)
  ) %>% 
  distinct() %>% 
  filter(!is.na(geo_to)) %>% # TODO ez mekkora arányt jelent
  replace_na(list(value = 0)) %>% 
  drop_na() %>%
  pivot_wider(names_from = direction, values_from = value, values_fill = 0) %>% 
  mutate(
    netto_export = export - `re-export`,
    netto_import = import - `re-import`,
    netto_trade =netto_export - netto_import,
  ) %>% 
  janitor::clean_names() %>% 
  pivot_longer(import:last_col(), names_to = "direction") %>% 
  filter(product %in% c("85", "84", "27", "87", "71", "30", "90", "39", "29", "72", "88", "73", "94", "26", "61", "62", "38", "40", "76", "total"))

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

gdp_df <- wbstats::wb(indicator = "NY.GDP.MKTP.CD") %>% 
  filter(date >= 2000 & date <= 2019) %>% 
  select(time = date, geo = iso2c, gdp = value) %>%
  mutate(
    geo = countrycode::countrycode(geo, "iso2c", "iso3c")
  )

library(rvest)

centroids_df <- read_html('https://developers.google.com/public-data/docs/canonical/countries_csv') %>% 
  html_nodes('table') %>% 
  html_table() %>% 
  first()

library(sf)

centroid_distance_df <- centroids_df %>% 
  transmute(country, geometry = map2(longitude, latitude, ~ st_point(c(.x, .y)))) %>% 
  st_as_sf() %>% 
  st_set_crs(4326) %>% 
  st_distance() %>% 
  data.frame() %>% 
  set_names(centroids_df$country) %>% 
  data.frame() %>% 
  mutate(geo_to = centroids_df$country) %>% 
  pivot_longer(- geo_to, names_to = "geo_from", values_to = "distance") %>% 
  mutate(
    distance = as.numeric(distance),
    distance = as.numeric(distance) / 1e3 # km
  ) %>% 
  filter(geo_to != geo_from) %>% 
  mutate(
    geo_from = countrycode::countrycode(geo_from, "iso2c", "iso3c"),
    geo_to = countrycode::countrycode(geo_to, "iso2c", "iso3c")
  )

rm(centroids_df)

save.image(file = "data/trade_data.RData")



