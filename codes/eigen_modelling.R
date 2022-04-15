library(tidyverse)

load("data/trade_data.RData")

geo_list <- c(trade_avg_df$geo_from, trade_avg_df$geo_to) %>% 
  unique() %>% 
  setdiff("WLD")

possible_directions_df <- crossing(geo_from = geo_list, geo_to = geo_list) %>% 
  crossing(product = trade_avg_df$product) %>% 
  crossing(time = as.character(2000:2019))

eigen_raw_df <- trade_avg_df %>% 
  left_join(x = possible_directions_df) %>% 
  replace_na(list(value = 0)) %>%
  group_by(product, time, geo_to) %>% # proportion of import by product
  mutate(value =  value / sum(value)) %>% 
  replace_na(list(value = 0)) %>% # TODO INF
  group_by(time, product) %>% 
  nest() %>% 
  mutate(
    data = map(data, pivot_wider, names_from = "geo_to", values_from = "value"),
    data = map(data, column_to_rownames, "geo_from"),
    # .row export equals to proportion of .col's all import > connect to geo_from
    data = map(data, as.matrix),
  ) %>% 
  crossing(geo_omit = geo_list)

eigen_df <- eigen_raw_df %>% 
  mutate(m = (row_number() - 1) %/% 500) %>% 
  group_by(m) %>% 
  nest() %>% 
  ungroup() %>% 
  granatlib::create_pb() %>% 
  unnest() %>%
  group_by(m) %>% 
  group_split(.keep = FALSE) %>% 
  map_df(function(xx) {
    pb$tick()
    mutate(xx, 
           data = map2(data, geo_omit, ~ .x[!rownames(.x) == .y, !colnames(.x) == .y]),
           ei = map(data, eigen),
           ei = map_dbl(ei, ~ max(as.numeric(.$value))),
    ) %>%
    select(time, product, geo_omit, ei)
  }
  )

save(eigen_df, file = "data/eigen.RData")