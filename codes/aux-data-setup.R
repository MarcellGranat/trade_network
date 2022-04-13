rm(list = ls())
library(tidyverse)

trade_aggrement <- readxl::read_excel("data/dummy_list.xlsx") %>% 
  {set_names(., str_c(.[1, ], names(.)))} %>% 
  slice(-1) %>% 
  pivot_longer(3:last_col()) %>% 
  janitor::clean_names() %>% 
  transmute(
    geo = geo_to_1,
    time = gsub("\\D.*", "", name),
    trade_aggrement = str_remove(name, time),
    trade_aggrement = gsub("[.]*\\d*$", "", trade_aggrement),
    trade_aggrement = str_remove(trade_aggrement, "is_"),
    value = !is.na(value)
  )

landlocked_df <- trade_aggrement %>% 
  filter(trade_aggrement == "landlocked") %>% 
  select(- trade_aggrement) %>% 
  rename(landlocked = value)

trade_aggrement <- trade_aggrement %>%
  filter(value) %>% 
  select(- value) %>% 
  filter(trade_aggrement != "landlocked") %>% 
  group_by(time, trade_aggrement) %>% 
  nest() %>% 
  mutate(
    data = map(data, pull),
    data = map(data, ~ crossing(geo_from = ., geo_to = .))
  ) %>% 
  unnest() %>% 
  filter(geo_from != geo_to) %>% 
  mutate(value = TRUE) %>% 
  pivot_wider(names_from = trade_aggrement, values_fill = FALSE) %>% 
  ungroup()

epu_df <- readxl::read_excel("data/economic_policy_uncertainty.xlsx") %>%
  pivot_longer(3:last_col()) %>% 
  transmute(
    geo = countrycode::countrycode(name, "country.name", "iso3c"), 
    time = lubridate::ym(str_c(Year, Month)),
    epu = value / 100
  ) %>% 
  drop_na()


annual_epu_df <- epu_df %>% 
  mutate(m = lubridate::month(time)) %>% 
  filter(m == 1) %>% 
  group_by(geo) %>% 
  arrange(geo, time) %>%  # TODO remo
  mutate(epu_avg = (epu + lead(epu)) / 2) %>% 
  ungroup() %>% 
  transmute(geo, time = lubridate::year(time), epu = epu_avg)

rm(epu_df) # montlhy and not used

gprc_df <- readxl::read_excel("data/geopolitical_risk_index.xls") %>% 
  select(time = month, everything(), - GPR) %>% 
  pivot_longer(- time) %>% 
  transmute(
    time = lubridate::year(time),
    geo = str_remove(name, "GPRC_"),
    gprc = value
  )

library(sf)

capital_distance_df <- maps::world.cities %>% 
  tibble() %>% 
  filter(capital == 1) %>% 
  distinct(country.etc, .keep_all = TRUE) %>% 
  select(geo = country.etc, lat, long) %>% 
  mutate(
    geo = countrycode::countrycode(geo, "country.name", "iso3c"),
    geometry = map2(long, lat, ~ st_point(c(.x, .y)))
  ) %>% 
  drop_na() %>% # Micronesia, Netherlands Antilles
  st_as_sf() %>% 
  st_set_crs(4326) %>% 
  {
    out <- st_distance(.)
    out <- data.frame(out)
    out <- set_names(out, .$geo)
    out$geo_from <- .$geo
    out
  } %>% 
  tibble() %>% 
  pivot_longer(
    cols = - geo_from, 
    names_to = "geo_to", 
    values_to = "distance_km", 
    values_transform = ~ as.numeric(.) / 1e3
  ) %>% 
  filter(geo_from != geo_to)

cds_premium_df <- readxl::read_excel("data/CDS_premium.xlsx") %>% 
  transmute(
    time = as.character(Year),
    geo = countrycode::countrycode(Country, "country.name", "iso3c"),
    spread = Spread
    ) %>% 
  drop_na()

save.image(file = "data/aux-data.RData")
