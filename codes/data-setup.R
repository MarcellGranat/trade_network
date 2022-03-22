library(tidyverse)

raw_data_df <- list.files("raw", full.names = TRUE) %>% 
  map(~ {load(.); raw_data}) %>% 
  reduce(c) %>% 
  bind_rows() %>% 
  mutate(data = map(data, 2))

trade_df <- raw_data_df %>% 
  unnest(data) %>% 
  select(product = id, direction = rgDesc, geo_from = rt3ISO, geo_to = pt3ISO, value = TradeValue) %>% 
  mutate(
    direction = str_to_lower(direction),
    value = as.numeric(value)
    )

save(trade_df, trade_df, file = "data/trade_data.RData")