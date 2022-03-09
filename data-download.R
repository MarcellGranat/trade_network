library(rjson)
library(tidyverse)

country_list <- fromJSON(file= "http://comtrade.un.org/data/cache/partnerAreas.json") %>% 
  {as.data.frame(t(sapply(.$results,rbind)))} %>% 
  tibble() %>% 
  janitor::clean_names() %>% 
  unnest() %>% 
  mutate(
    geo = countrycode::countrycode(sourcevar = v2, origin = "country.name", destination = "iso3c"),
    geo = case_when(
      v2 == "World" ~ "world",
      TRUE ~ geo
    )
  ) %>% 
  group_by(geo) %>% 
  na.omit() %>% 
  filter(str_detect(v2, "Fmr ", negate = TRUE)) %>% 
  mutate(n = n()) %>% 
  filter(n == 1 | str_detect(v2, "[ ]", negate = TRUE)) %>% 
  ungroup() %>% 
  select(-n) %>% 
  arrange(v1)

country_list %>% 
  filter(v1 == "660")


commodity_list <- 
  fromJSON(file= "https://comtrade.un.org/Data/cache/classificationHS.json") %>% 
  {bind_rows(.$results)} %>% 
  filter(str_length(id) == 2 | id == "ALL" | id == "TOTAL")

req_df <- crossing(country_list, commodity_list) %>% 
  arrange(id, geo)

get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )
  
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}

get_data <- function(r, time, cc) {
    tryCatch({
      data_country <- get.Comtrade(r = r, p = "all", freq = "A", ps = str_c(time, collapse = ","), cc = cc) %>% 
        {.$data}
    }, error = function(e) Sys.sleep(3700))
  data_country
}

downloaded_files <- list.files("raw") %>% 
  str_remove_all("\\D") %>% 
  as.numeric() 

req_df <- req_df %>% 
  mutate(
    row_num = row_number(),
    g = row_num %/% 100
  ) %>% 
  filter(!(g %in% downloaded_files))

# for (i in rev(unique(req_df$g))) { # Bálint
# for (i in unique(req_df$g)) {
for (i in 1) {
  tictoc::tic()
  answer_df <- apply(filter(req_df, g == i)[, c(1, 4)], 1, function(x) get_data(r = x[1], time = 2016:2020, cc = x[2]))
  out <- bind_cols(
    filter(req_df, g == i), 
    tibble(data = answer_df)
  ) %>% 
    unnest(cols = c(data))
  
  save(out, file = str_c("raw/data", i, ".RData"))
  tictoc::toc()
  # Sys.sleep(3700)
}


