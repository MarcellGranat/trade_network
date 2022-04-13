library(tidyverse)

load("data/trade_data.RData")

geo_list <- c(trade_avg_df$geo_from, trade_avg_df$geo_to) %>% 
  unique() %>% 
  setdiff("WLD")

possible_directions_df <- crossing(geo_from = geo_list, geo_to = geo_list) %>% 
  crossing(product = trade_avg_df$product)

eigen_df <- trade_avg_df %>% 
  left_join(x = possible_directions_df) %>% 
  replace_na(list(value = 0)) %>%
  group_by(product, geo_to) %>% # proportion of import by product
  mutate(value =  value / sum(value)) %>% 
  replace_na(list(value = 0)) %>% # TODO INF
  drop_na(time) %>% 
  group_by(time, product) %>% 
  nest() %>% 
  mutate(
    data = map(data, pivot_wider, names_from = "geo_to", values_from = "value"),
    data = map(data, column_to_rownames, "geo_from"),
    data = map(data, as.matrix),
    ei = map(data, eigen),
    ei_vector = map(ei, ~ .$vectors[, 1]),
    full_ei = map2(data, ei_vector, ~ tibble(geo_full = colnames(.x), ei_full = as.numeric(.y)))
  ) %>% 
  select(product, data, full_ei) %>% 
  crossing(geo_omit = geo_list) %>% 
  mutate(
    data = map2(data, geo_omit, ~ .x[!rownames(.x) == .y, !colnames(.x) == .y]),
    ei = map(data, eigen),
    ei_vector = map(ei, ~ .$vectors[, 1]),
    omitted_ei = map2(data, ei_vector, ~ tibble(geo = colnames(.x), ei_omitted = as.numeric(.y)))
  ) %>% 
  mutate(full_ei = map2(full_ei, geo_omit, ~ filter(.x, geo_full != .y))) %>% 
  unnest(c(full_ei, omitted_ei)) %>% 
  select(product, geo_omit, geo, ei_full, ei_omitted)

eigen_df %>% 
  mutate(
    abs_ei_diff = ei_omitted - ei_full,
    rel_ei_diff = ei_omitted / ei_full,
  ) %>% 
  filter(product == "01") %>% 
  arrange(desc(rel_ei_diff))
