library(tidyverse)
library(lubridate)
library(here)
library(janitor)

beginning <- ymd('1999-01-01')
end <- ymd('2019-12-31')

brfss_data <- read_csv(here('Data','all_brfss_data.csv'), col_types=cols(x_age_g=col_double()))

brfss_codes <- read_csv(here('Data','brfss_codes.csv')) %>%
  clean_names() %>%
  filter(variable_name %in% c('MENTHLTH', 'GENHLTH', 'MARITAL')) %>%
  mutate(value=as.numeric(value))

brfss_data <- brfss_data %>%
  mutate(age_pre_2013=case_when(between(age, 18,24) ~ 1,
                                between(age,25,34) ~ 2,
                                between(age,35,44) ~ 3,
                                between(age,45,54) ~ 4,
                                between(age,55,64) ~ 5,
                                between(age,65,99) ~ 6,
                                TRUE ~ as.numeric(NA)),
         age_final = if_else(is.na(x_age_g), age_pre_2013, x_age_g),
         age_group = case_when(is.na(age_final)~as.character(NA),
                               age_final==1~'18-24',
                               TRUE~'25+'))

brfss_data_cons <- brfss_data %>%
  group_by(month_end, age_group, marital, genhlth, menthlth) %>%
  summarize(count=sum(count))

brfss_data_cons <- brfss_data_cons %>%
  left_join(brfss_codes %>% filter(variable_name=='MARITAL') %>% select(value, value_label), by=c('marital'='value')) %>%
  left_join(brfss_codes %>% filter(variable_name=='GENHLTH') %>% select(value, value_label), by=c('genhlth'='value'), suffix=c('_marital','_genhlth')) %>%
  left_join(brfss_codes %>% filter(variable_name=='MENTHLTH') %>% select(value, value_label), by=c('menthlth'='value')) %>%
  rename(value_label_menthlth = value_label)

brfss_data_cons <- brfss_data_cons %>%
  filter(!is.na(menthlth)) %>%
  mutate(value_label_menthlth_cond = if_else(grepl('[[:digit:]]',value_label_menthlth),'Mntl Health Not Good','Mental Health Good/No Answer'),
         value_label_marital_cond = if_else(value_label_marital %in% c('Divorced', 'Widowed', 'Separated'),'Divorced/Widowed/Separated','Married/NeverMarried/MemberOfUnmarriedCouple'),
         value_label_genhlth_cond = if_else(value_label_genhlth %in% c('Poor'),'Poor Gen Health','Good Gen Health/No Answer'))

brfss_adol_data <- brfss_data_cons %>%
  filter(age_group=='18-24')

brfss_adult_data <- brfss_data_cons %>%
  filter(age_group=='25+')

get_brfss_var <- function(b_data){
  
  ment_health_no_good <- b_data %>%
    filter(!is.na(menthlth)) %>%
    select(month_end, value_label_menthlth_cond, count) %>%
    group_by(month_end, value_label_menthlth_cond) %>%
    summarize(count=sum(count)) %>%
    group_by(month_end) %>%
    mutate(percent=count/sum(count)) %>%
    filter(value_label_menthlth_cond!='Mental Health Good/No Answer') %>%
    select(month_end, percent) %>%
    rename(ment_health_no_good = percent)
  
  genhlth_no_good <- b_data %>%
    filter(!is.na(genhlth)) %>%
    select(month_end, value_label_genhlth_cond, count) %>%
    group_by(month_end, value_label_genhlth_cond) %>%
    summarize(count=sum(count)) %>%
    group_by(month_end) %>%
    mutate(percent=count/sum(count)) %>%
    filter(value_label_genhlth_cond!='Good Gen Health/No Answer') %>%
    select(month_end, percent) %>%
    rename(genhlth_no_good = percent)
  
  divorced <- b_data %>%
    filter(!is.na(marital)) %>%
    select(month_end, value_label_marital_cond, count) %>%
    group_by(month_end, value_label_marital_cond) %>%
    summarize(count=sum(count)) %>%
    group_by(month_end) %>%
    mutate(percent=count/sum(count)) %>%
    filter(value_label_marital_cond!='Married/NeverMarried/MemberOfUnmarriedCouple') %>%
    select(month_end, percent) %>%
    rename(divorced_widowed_separated = percent)
  
  return(list(ment_health_no_good, genhlth_no_good, divorced))
}

brfss_adol_summ <- get_brfss_var(brfss_adol_data)
brfss_adult_summ <- get_brfss_var(brfss_adult_data)
brfss_summ <- get_brfss_var(brfss_data_cons)

master_data_1 <- brfss_adol_summ[[1]] %>%
  full_join(brfss_adol_summ[[2]], by='month_end') %>%
  full_join(brfss_adol_summ[[3]], by='month_end') %>%
  full_join(brfss_adult_summ[[1]], by='month_end', suffix=c('_18_24', '_25on')) %>%
  full_join(brfss_adult_summ[[2]], by='month_end', suffix=c('_18_24', '_25on')) %>%
  full_join(brfss_adult_summ[[3]], by='month_end', suffix=c('_18_24', '_25on')) %>%
  full_join(brfss_summ[[1]], by='month_end') %>%
  full_join(brfss_summ[[2]], by='month_end') %>%
  full_join(brfss_summ[[3]], by='month_end')

stock_files <- c('apple_stock.csv', 'atandt_stock.csv', 'facebook_stock.csv', 'tmobile_stock.csv', 'twitter_stock.csv', 'verizon_stock.csv')

i <- 0 

for(file in stock_files){
  temp_stock <- read_csv(here('Data', file)) %>%
    clean_names() %>%
    mutate(month_end=rollback(ceiling_date(date,unit='month')))
  
  var_name <- paste('avg_close_',gsub('.csv', '', file), sep='')
  
  agg <- temp_stock %>%
    group_by(month_end) %>%
    summarize(!!var_name:=mean(close)) %>%
    filter(between(month_end, beginning, end))
  
  if(i==0){
    stock_data <- agg
  } else {
    stock_data <- stock_data %>%
      full_join(agg, by='month_end')
  }
  
  i <- i+1
}

stock_data <- stock_data %>%
  mutate_if(is.numeric,replace_na, replace=0)

master_data_2 <- master_data_1 %>%
  full_join(stock_data, by='month_end') 


gdp_quart <- read_csv(here('Data','GDPC1.csv')) %>%
  clean_names() %>%
  mutate(month_end=rollback(ceiling_date(date, unit='quarter'))) %>%
  select(-date) %>%
  rename(gdp_quart=gdpc1) %>%
  filter(between(month_end, beginning, end))

master_data_3 <- master_data_2 %>%
  full_join(gdp_quart,by='month_end') %>%
  arrange(month_end) %>%
  ungroup() %>%
  mutate(gdp_fill = gdp_quart) %>%
  fill(gdp_fill,.direction='up')


cdc_suicide_data_all <- read_delim(here('Data','suicide_data_1999_2019.txt'), delim='\tab') %>%
  clean_names() %>%
  select(-c(population, crude_rate, notes))

cdc_suicide_data <- cdc_suicide_data_all %>%
  filter(!is.na(ten_year_age_groups) & ten_year_age_groups!='Not Stated' & deaths!=0) %>%
  mutate(age_group = factor(ten_year_age_groups_code, levels=c("1","1-4","5-14","15-24","25-34","35-44","45-54","55-64","65-74","75-84","85+"))) %>%
  separate(month_code, sep='/', into=c(NA,'month')) %>%
  mutate(month_end = rollback(ceiling_date(ymd(paste(as.character(year),month, '01')),unit='month')),
         year_cat = as.character(year),
         grouped_age=case_when(age_group %in% c('1', '1-4', '15-24') ~ '0-24',
                               TRUE ~ '25+')) %>%
  group_by(grouped_age, month_end) %>%
  summarize(deaths=sum(deaths))

suicide_wide <- cdc_suicide_data %>%
  pivot_wider(names_from=grouped_age, values_from=deaths) %>%
  clean_names() %>%
  rename(x0_24_suicides=x0_24,
         x25on_suicides=x25)


social_media_usage <- read_delim(here('Data', 'social_media_usage_05_19.txt'),delim='\t')

social_media_usage <- social_media_usage %>%
  mutate(all_social_media_usage = as.numeric(gsub('[[:punct:]]','',percentage))/100,
         date=mdy(date),
         month_end=rollback(ceiling_date(date, unit='month')),
         year=year(month_end),
         month=month(month_end)) %>%
  group_by(year, month) %>%
  mutate(rank_date=rank(desc(date))) %>%
  filter(rank_date==1) %>%
  ungroup() %>%
  select(month_end, all_social_media_usage)

social_media_usage_age <- read_delim(here('Data', 'social_media_usage_by_age.txt'), delim='\t') %>%
  clean_names()

social_media_usage_age <- social_media_usage_age %>%
  mutate(x18_29_social_media_usage = as.numeric(gsub('[[:punct:]]', '', x18_29))/100,
         x30_49_social_media_usage = as.numeric(gsub('[[:punct:]]', '', x30_49))/100,
         x50_64_social_media_usage = as.numeric(gsub('[[:punct:]]', '', x50_64))/100,
         x65_social_media_usage = as.numeric(gsub('[[:punct:]]', '', x65))/100,
         date=mdy(date),
         month_end=rollback(ceiling_date(date, unit='month')),
         year=year(month_end),
         month=month(month_end)) %>%
  group_by(year, month) %>%
  mutate(rank_date=rank(desc(date))) %>%
  filter(rank_date==1) %>%
  ungroup() %>%
  select(month_end,x18_29_social_media_usage,x30_49_social_media_usage,x50_64_social_media_usage,x65_social_media_usage)

population_data <- read_delim(here('Data', 'us_population_data.txt'), delim='\t', ) %>%
  clean_names()

population_data <- population_data %>%
  mutate(month_end=rollback(mdy(date)),
         population_num_million = as.numeric(gsub('[[:alpha:]]', '',population))) %>%
  filter(between(month_end,beginning,end)) %>%
  select(month_end, population_num_million)

broadband_usage <- read_delim(here('Data', 'broadband_usage.txt'), delim='\t')

broadband_usage <- broadband_usage %>%
  mutate(broadband_usage_per = as.numeric(gsub('[[:punct:]]','',broadband_usage))/100,date=mdy(date),
         month_end=rollback(ceiling_date(date, unit='month')),
         year=year(month_end),
         month=month(month_end)) %>%
  group_by(year, month) %>%
  mutate(rank_date=rank(desc(date))) %>%
  filter(rank_date==1) %>%
  ungroup() %>%
  select(month_end,broadband_usage_per)


unemp <- read_csv(here('Data', 'unemp.csv')) %>%
  clean_names()

unemp_longer <- unemp %>%
  pivot_longer(cols=jan:dec, names_to='month', values_to='unemp') %>%
  mutate(unemp_per=unemp/100,
         month_end=rollback(ceiling_date(ymd(paste(year,month,'01')),unit='month'))) %>%
  filter(between(month_end, beginning, end)) %>%
  select(month_end, unemp_per)

pers_sav <- read_csv(here('Data', 'PersSavingsRate_1999to2019.csv')) %>%
  clean_names() %>%
  mutate(month_end=rollback(ceiling_date(observation_date, unit='month'))) %>%
  select(-observation_date) %>%
  mutate(personal_save_rate = psavert/100) %>%
  filter(between(month_end, beginning, end))

master_data_4 <- master_data_3 %>%
  full_join(suicide_wide, by='month_end') %>%
  full_join(social_media_usage, by='month_end') %>%
  full_join(social_media_usage_age, by='month_end') %>%
  full_join(population_data, by='month_end') %>%
  full_join(broadband_usage, by='month_end') %>%
  full_join(unemp_longer, by='month_end') %>%
  full_join(pers_sav, by='month_end')

master_data_5 <- master_data_4 %>%
  ungroup() %>%
  fill(all_social_media_usage, .direction='down') %>%
  fill(x18_29_social_media_usage, .direction='down') %>%
  fill(x30_49_social_media_usage, .direction='down') %>%
  fill(x50_64_social_media_usage, .direction='down') %>%
  fill(x65_social_media_usage, .direction='down') %>%
  fill(broadband_usage_per, .direction='down') %>%
  mutate_if(is.numeric,replace_na, replace=0)

write_csv(master_data_5, here('Data','master_data.csv'))

csv_names <- tibble(names(master_data_5))

write_csv(csv_names, here('Data','master_data_names.csv'))


master_data_6 <- master_data_5 %>%
  select(-c(all_social_media_usage, 
            avg_close_facebook_stock, 
            avg_close_tmobile_stock, 
            avg_close_twitter_stock, 
            broadband_usage_per, 
            gdp_fill, 
            gdp_quart,
            x18_29_social_media_usage,
            x30_49_social_media_usage,
            x50_64_social_media_usage, 
            x65_social_media_usage,
            divorced_widowed_separated_18_24,
            divorced_widowed_separated_25on,
            genhlth_no_good,
            ment_health_no_good,
            psavert)) %>%
  ungroup() %>%
  mutate(x0_24_suicides_per_thous = x0_24_suicides/(population_num_million*1000000)*1000,
         x25on_suicides_per_thous = x25on_suicides/(population_num_million*1000000)*1000)


write_csv(master_data_6, here('Data','final_data.csv'))

