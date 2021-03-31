library(tidyverse)

setwd("C:/school/szem_8/TDK-yieldcurve/yieldcurve/yields raw csv3")
# this folder contains the csv files from 

dat_yields %>% 
  lapply(function(x) {
    names(x) %>% 
      length()
  }) %>% 
  reduce(c) %>% 
  {which(. != 6)}

dat_yields <- list.files(pattern = ".$") %>% 
  lapply(function(x) {
    read.csv(x) %>%
       set_names('Date', 'Price', 'Open', 'High', 'Low', 'change') %>%
      mutate(
        source = x
      )
  }) %>% 
  reduce(rbind) %>% 
  tibble() %>% 
  janitor::clean_names() %>% 
  mutate(
    date = lubridate::mdy(date),
    maturity = gsub('Bond.*', '', source), 
    maturity = paste(str_remove_all(maturity, '[^\\d]'), str_remove_all(gsub('.*[1-9]', '', maturity), '[^A-Z]')),
    country = countrycode::countrycode(gsub(' \\d.*', '', source), origin = 'country.name', destination = 'iso2c')
  ) %>% 
  select(-source, -change) %>% 
  select(date, country, everything()) %>% 
  group_by(country, maturity) %>% 
  group_modify(~ filter(.x, !duplicated(date))) %>% 
  ungroup()

dat_gdp <- read_csv("C:/school/szem_8/TDK-yieldcurve/yieldcurve/namq_10_gdp_1_Data.csv") %>% 
  mutate(country = countrycode::countrycode(GEO, origin = 'country.name',
                                            destination = 'iso2c'),
         values = as.numeric(str_remove_all(Value, ' ')),
         date = lubridate::yq(TIME)
  ) %>% 
  filter(
    country %in% dat_yields$country &
      UNIT == 'Chain linked volumes (2015), million euro' &
      NA_ITEM == 'Gross domestic product at market prices' &
      S_ADJ == 'Seasonally and calendar adjusted data'
  ) %>% 
  select(country, date, values) %>% 
  na.omit() %>% 
  rbind(
    read_csv("GDPC1.csv") %>% 
      set_names('date', 'values') %>% 
      transmute(
        country = 'US',
        date = lubridate::ymd(date),
        values
      )
  )

dat_US_yield <- read_csv("C:/school/szem_8/TDK-yieldcurve/yieldcurve/GS1.csv") %>% 
merge(read_csv("C:/school/szem_8/TDK-yieldcurve/yieldcurve/GS10.csv")) %>% 
merge(read_csv("C:/school/szem_8/TDK-yieldcurve/yieldcurve/DGS1.csv")) %>% 
merge(read_csv("C:/school/szem_8/TDK-yieldcurve/yieldcurve/DGS10.csv")) %>% 
  mutate_at(-1, function(x) as.numeric(x)) %>% 
  mutate(date = lubridate::ymd(DATE)) %>% 
  select(date, everything(), -DATE)

save(list = c('dat_yields', 'dat_gdp', 'dat_US_yield'), file = 'C:/school/szem_8/TDK-yieldcurve/yieldcurve/dat.RData')
