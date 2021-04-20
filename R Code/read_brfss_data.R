library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(foreign)


brfss_files <- list.files(here('Data','brfss'))

all_data <- tibble()

for(i in brfss_files){
  temp_file <- read.xport(here('Data','brfss',i))
  
  year <- gsub('[[:alpha:] | [:punct:]]', '', i)
  
  if(nchar(year)!=4){
    
    if(as.numeric(year)>=50){
      year <- as.numeric(paste('19',year,sep=''))
    } else{
      year <- as.numeric(paste('20',year,sep=''))
    }
   
  } else {
    year <- as.numeric(year)
  }
  
  print(year)
  
  if(year<2013){
    temp_file_2 <- temp_file %>%
      select(IMONTH, MARITAL, MENTHLTH, GENHLTH, AGE) %>%
      group_by(IMONTH, MARITAL, MENTHLTH, GENHLTH, AGE) %>%
      summarize(count = n()) %>%
      mutate(YEAR = year,
             month_end = rollback(ceiling_date(ymd(paste(as.character(YEAR),IMONTH, '01')),unit='month'))) %>%
      clean_names()
  } else {
    
    
    
    temp_file_2 <- temp_file %>%
      select(IMONTH, MARITAL, MENTHLTH, GENHLTH, X_AGE_G) %>%
      group_by(IMONTH, MARITAL, MENTHLTH, GENHLTH, X_AGE_G) %>%
      summarize(count = n()) %>%
      mutate(YEAR = year,
             month_end = rollback(ceiling_date(ymd(paste(as.character(YEAR),IMONTH, '01')),unit='month'))) %>%
      clean_names()
  }
  

  
  all_data <- bind_rows(all_data, temp_file_2)
  
  
}

write_csv(all_data, here('Data','all_brfss_data.csv'))








