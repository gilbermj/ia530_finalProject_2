library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(foreign)

# get the list of brfss files
brfss_files <- list.files(here('Data','brfss'))

all_data <- tibble()



for(i in brfss_files){
  
  # read in the brfss file
  temp_file <- read.xport(here('Data','brfss',i))
  
  # get the year from the file name
  year <- gsub('[[:alpha:] | [:punct:]]', '', i)
  
  # for the files where the year is two digits
  # pad with 20 or 19 based on the year
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
  
  # before 2013 the names of the variables was slightly different
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








