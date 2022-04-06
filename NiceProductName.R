library(tidyverse)

code_label_df <- raw_data_df %>% 
  select(text) %>% 
  distinct() %>% 
  separate(text, c("code", "label"), sep = " - ")

NiceProductName <- function(x, wrap = NULL) {
  out <- map_chr(x, function(current_x) {
    pull(code_label_df, label, code) %>% 
      .[[current_x]]
  })
  
  if (!is.null(wrap)) {
    out <- str_wrap(out, wrap)
  }
  out
}
