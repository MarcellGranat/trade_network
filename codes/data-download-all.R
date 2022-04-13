library(tidyverse)
library(comtradr)
library(rjson)
library(progress)

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
  arrange(v1) %>% 
  filter(geo != "world")

df <- country_list %>% 
  mutate(m = (row_number() - 1) %/% 5) %>% 
  group_by(m) %>% 
  summarise(
    geo = str_c(geo, collapse = ","),
    v1 = str_c(v1, collapse = ","),
    ) %>% 
  ungroup() %>% 
  select(geo, v1)
  

get.Comtrade <- function(url="http://comtrade.un.org/api/get?", maxrec=50000, type="C", freq="A",
                         px="HS", ps="now", r, p, rg="all", cc="TOTAL", fmt="json"){
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

done_l <- list.files("raw", full.names = TRUE) %>% 
  keep(str_detect, "all_data")

if (length(done_l) >= 1) {
  df <- map(done_l, ~ {load(.); get("raw_data")}) %>% 
    reduce(c) %>% 
    bind_rows() %>% 
    anti_join(x = df)
}

i <- 1
raw_data <- list()
n_error <- 0
pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = 470,
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100) 

while(TRUE) {
  iwalk(df, ~ assign(.y, .x[i], envir = rlang::env_parent()))
  
  if (i != 1) rm("api_answer") # error if not created
  
  tryCatch(
    expr = {
      api_answer <- get.Comtrade(r = v1, p = "0", freq = "A", ps = str_c(2019, collapse = ","), cc = "AG2")
    },
    error = function(e){
      n_error <<- n_error + 1
    }
  )   
  
  if (exists("api_answer")) {
    raw_data[[length(raw_data) + 1]] <- df %>% 
      slice(i) %>% 
      mutate(data = list(api_answer))
    
    
    n_error <- 0
    i <- i + 1 
    pb$tick()
    
    
  } else {
    if (n_error >= 3 & n_error %% 3 == 0) {
      beepr::beep(9)
    }
    Sys.sleep(10)
  }
  
  if ((i %% 470 == 0 & i != 0) | i >= nrow(df)) {
    n_raw <- list.files("raw") %>% 
      keep(str_detect, "all_data") %>% 
      length()
    
    save(raw_data, file = str_c("raw/all_data", n_raw + 1, ".RData"))
    
    pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                           total = 470,
                           complete = "=",   # Completion bar character
                           incomplete = "-", # Incomplete bar character
                           current = ">",    # Current bar character
                           clear = FALSE,    # If TRUE, clears the bar when finish
                           width = 100)
    
    raw_data <- list()
    closeAllConnections()
    
  }
  
  Sys.sleep(.1)
  
  if (i == nrow(df)) {
    break
  }
}

