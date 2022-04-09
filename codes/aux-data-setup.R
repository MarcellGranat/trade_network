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

gprc_df <- readxl::read_excel("data/geopolitical_risk_index.xls") %>% 
  select(time = month, everything(), - GPR) %>% 
  pivot_longer(- time) %>% 
  transmute(
    time = lubridate::year(time),
    geo = str_remove(name, "GPRC_"),
    gprc = value
  )

library(sf)

capital_sf <- readxl::read_excel("data/world_cities.xlsx") %>% 
  filter(capital == "primary") %>% 
  select(geo = iso3, lat, lng) %>% 
  group_by(geo) %>% 
  summarise_all(mean) %>% 
  mutate(geometry = map2(lng, lat, ~ st_point(c(.x, .y)))) %>% 
  st_as_sf() %>% 
  st_set_crs(4326)

capital_distance_df <- st_distance(capital_sf) %>% 
  data.frame() %>% 
  set_names(capital_sf$geo) %>% 
  mutate(geo_from = capital_sf$geo) %>% 
  pivot_longer(- geo_from, names_to = "geo_to", values_to = "capital_distance_km") %>% 
  filter(geo_from != geo_to) %>% 
  mutate(capital_distance_km = as.numeric(capital_distance_km) / 1e3)
