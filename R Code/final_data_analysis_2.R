### Description ################################################ 
# Amy Moyer & Michael Gilbert
# IA 530 Final Project
# 
# This file is the final analysis for the project
# this includes functions and then code which are described below
# 
# SET WORKING DIRECTORY TO SOURCE FILE LOCATION!!!!!!!!!!!!!!!!
#
### Packages ################################################

library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(gridExtra)
library(scales)
library(dynlm)
library(fastDummies)
library(urca)
library(vars)
library(gt)
library(webshot)
library(knitr)

### Functions #########################################################

graph_vars <- function(data, vars, clean, dollars, percents, theme){
  
  # this function is to graph the variables
  # Arguments:
  #  data: the data frame containing the data to be graphed
  #  vars: the y variables to be graphed
  #  clean: the data frame containing the clean names to be graphed
  #  dollars: the list of variables that should be formatted as dollars
  #  percents: the list of variables that should be formatted as percents
  #  theme: the theme to be used for the plots
  
  
  temp_list <- list()
  
  
  for(i in vars){
    
    # get the clean name for the y variable
    clean_name <- as.character(clean_names %>% filter(var_name==i) %>% dplyr::select(clean_name))
    
    # plot the variable and store in list
    temp_plot <- ggplot(data, aes_string(x='month_end', y=i)) + 
      geom_line() +
      theme
    
    # add the label.  If the variable is adjusted for stationarity then add that label
    # if the variable is adjusted for seasonality then add that label
    # otherwise, label as is
    if(grepl('_sa',i) & !grepl('_station',i)){
      temp_plot <- temp_plot + labs(title=paste(clean_name,'\n - Seasonally Adjusted', sep=''), x='', y='')
    } else if(grepl('_station',i)) {
      temp_plot <- temp_plot + labs(title=paste(clean_name,' - Stationary', sep=''), x='', y='')
    } else {
      temp_plot <- temp_plot + labs(title=clean_name, x='', y='')
    }
      
    # format the y labels as dollars or percents
    if(i %in% dollars){
      temp_plot <- temp_plot + scale_y_continuous(labels = dollar_format())
    } else if(i %in% percents) {
      temp_plot <- temp_plot + scale_y_continuous(labels = percent)
    }
    
    temp_list <- append(temp_list, list(temp_plot))
  }
  
  return(temp_list)
}

test_station <- function(data, vars) {
  
  # function to test the variables in data for stationarity
  # Arguments
  #  data: data frame holding the variables to be tested for stationarity
  #  vars: the variables to be tested for stationarity
  
  # vectors to hold the test statistics
  df_testStat <- c()
  df_critValue <- c()
  pp_testStat <- c()
  pp_critValue <- c()
  adfgls_testStat <- c()
  adfgls_critValue <- c()
  kpss_testStat <- c()
  kpss_critValue <- c()
  
  
  for(v in vars){
    # dickey-fuller test
    temp_df <-ur.df(data[,v], type=c('trend'), selectlags='BIC')
    df_testStat <- c(df_testStat, temp_df@teststat[1])
    df_critValue <- c(df_critValue, temp_df@cval[1,2])
    
    # phillips-perron test
    temp_pp <-ur.pp(data[,v], type=('Z-tau'), model=c('trend'))
    pp_testStat <- c(pp_testStat, temp_pp@teststat[1])
    pp_critValue <- c(pp_critValue, temp_pp@cval[1,2])
    
    # augmented dickey-Fuller
    temp_adfgls <- ur.ers(data[,v], type='DF-GLS', model='trend')
    adfgls_testStat <- c(adfgls_testStat, temp_adfgls@teststat[1])
    adfgls_critValue <- c(adfgls_critValue, temp_adfgls@cval[1,2])
    
    # kpss test
    temp_kpss <- ur.kpss(data[,v], type=c('tau'))
    kpss_testStat <- c(kpss_testStat, temp_kpss@teststat[1])
    kpss_critValue <- c(kpss_critValue, temp_kpss@cval[1,2])
  }
  
  # create new tibble with stats
  stationarity_stats <- tibble(stationarity_vars,
                               df_testStat,
                               df_critValue,
                               pp_testStat,
                               pp_critValue,
                               adfgls_testStat,
                               adfgls_critValue,
                               kpss_testStat,
                               kpss_critValue)
  
  # add columns to compare the test stats to the critical values.  Include 1 
  # if the variables is stationary and 0 if not.  For kpss the null and 
  # alternative are different
  
  stationarity_stats <- stationarity_stats %>%
    mutate(df_result = if_else(abs(df_testStat)>abs(df_critValue),1,0),
           pp_result = if_else(abs(pp_testStat)>abs(pp_critValue),1,0),
           adfgls_result = if_else(abs(adfgls_testStat)>abs(adfgls_critValue),1,0),
           kpss_result = if_else(abs(kpss_testStat)>abs(kpss_critValue),0,1),
           total_stationary=df_result+pp_result+adfgls_result+kpss_result)
  
  return(stationarity_stats)
}

var_models <- function(data, 
                       vars, 
                       out_months, 
                       response, 
                       runs=100, 
                       against_self=T, 
                       clean, 
                       theme,
                       n_row = 1,
                       title = '',
                       file_name = '',
                       dpi = 300,
                       width = 10,
                       height = 10,
                       layout=NULL){
  
  # Function to fit a VAR model for the variables indicated and return the 
  # irf functions graphed in a list 
  # 
  # Arguments
  #   data: the data frame that holds the variables to be in the VAR model
  #   vars: the variables to be included in the VAR model IN THE ORDER YOU WANT THEM
  #     TO BE INCLUDED IN THE MODEL
  #   out_months: the number of months to project out for the irf
  #   response: the response in the irf
  #   runs: number of runs for CI in the irf
  #   against_self: indicates whether to graph the irf of the response variable against
  #     itself
  #   clean: the data frame holding the clean names for display purposes
  #   theme: the theme for the graphs
  #   nrow: the number of rows in the grid
  #   title: the title of the grid graph
  #   file_name: the file_name to save the graph
  #   dpi: the resolution to save the graph
  #   width: the width to save the graph
  #   height: the height to save the graph
  #   layout: the layout (can be NULL if you want grid.arrange to auto arrange the graphs)
  

  # grab only the variables we need
  temp_data <- data %>%
    dplyr::select(month_end, vars, all_of(response))
  
  # grab the min month and year to create the time series
  start_year <- year(min(temp_data$month_end))
  start_month <- month(min(temp_data$month_end))
  
  # get rid of month-end
  temp_data <- temp_data %>%
    dplyr::select(-c(month_end))
  
  temp_ts <- temp_data %>% ts(start=c(start_year,start_month), frequency=12)
  
  # convert to time-series and create the VAR models
  var_model <- VAR(temp_ts, lag.max = 12, ic = 'AIC')
  
  var_lag <- as.numeric(var_model$p)
  
  ts_names <- names(temp_data)
  
  # tibbles to hold the impulse response functions as well as the lower and
  # upper bounds
  lower <- tibble(month=1:(out_months+1))
  irf <- tibble(month=1:(out_months+1))
  upper <- tibble(month=1:(out_months+1))

  
  for(name in ts_names){
    
    if(name!='month_end' & (name!=response | against_self)){
      # calculate the irf and CI
      temp_irf <- irf(var_model,impulse=c(name), response=c(response), n.ahead=out_months, cumulative = TRUE,runs=runs, ci=0.95)
      
      lower <- bind_cols(lower, !!name:=temp_irf$Lower[[1]])
      upper <- bind_cols(upper, !!name:=temp_irf$Upper[[1]])
      irf <- bind_cols(irf, !!name:=temp_irf$irf[[1]])
      
      print(name)
    }
 
  }
  
  # get minimum and maximum for lower and upper bounds for the y axis limits
  min_lower <- min(lower[,2:ncol(lower)])
  max_upper <- max(upper[,2:ncol(upper)])
  
  temp_list <- list() 
  
  for(name in ts_names){
    
    # get the nice names for graphing
    clean_name <- as.character(clean_names %>% filter(var_name==name) %>% dplyr::select(clean_name))
    response_name <- as.character(clean_names %>% filter(var_name==response) %>% dplyr::select(clean_name))
  
    if(name!='month_end' & (name!=response | against_self)){
      
      # graph the irf
      temp_plot <- ggplot(data=lower, aes_string(x='month', y=name)) +
        geom_line(color='red', alpha=0.75, linetype = "dashed") + #lower bound
        geom_line(data=irf, aes_string(x='month', y=name)) + # irf
        geom_line(data=upper, aes_string(x='month', y=name), color='red', alpha=0.75, linetype = "dashed") + #upper bound
        geom_hline(yintercept=0) +
        scale_x_continuous(breaks=seq(from=2, to=out_months, by=4)) +
        scale_y_continuous(labels = percent, limits=c(min_lower,max_upper)) +
        theme +
        labs(x='Month', 
             y='', 
             title=paste(clean_name,sep=''))
      
      temp_list <- append(temp_list, list(temp_plot))
      
    }
    
  }
  
  # if no layout just auto arrange the graphs
  if(is.null(layout)){
  
    grid <- grid.arrange(grobs=temp_list,
                         nrow=n_row,
                         top=paste(title,as.character(var_lag),sep=' - Optimal Lag:'))
    
  } else {
    
    grid <- grid.arrange(grobs=temp_list,
                         layout_matrix=layout,
                         nrow=n_row,
                         top=paste(title,as.character(var_lag),sep=' - Optimal Lag:'))
    
  }
  
  # if there is not a file name, then don't save
  if(file_name!=''){
  
    ggsave(here('Plots', file_name),
           plot=grid,
           dpi=dpi, 
           width = width, 
           height = height, 
           units='in')
    
  }
  
  return(temp_list)
  
  
}

### Read Data########################################################

final_data <- read_csv(here('Data','final_data.csv'))

# filter out unneeded variables
# AND rename variables to something simpler
final_data <- final_data %>%
  dplyr::select(-c(population_num_million, x0_24_suicides, x25on_suicides)) %>%
  rename(ment24 = ment_health_no_good_18_24,
         gen24 = genhlth_no_good_18_24,
         ment25 =ment_health_no_good_25on,
         gen25 = genhlth_no_good_25on,
         divorced = divorced_widowed_separated,
         apple = avg_close_apple_stock,
         atandt = avg_close_atandt_stock,
         verizon = avg_close_verizon_stock,
         unemp = unemp_per,
         savings = personal_save_rate,
         suicide24 = x0_24_suicides_per_thous,
         suicide25 = x25on_suicides_per_thous)

clean_names <- read_csv(here('Data', 'all_names_cleaned.csv'))

main_theme <- theme(panel.grid = element_blank(),
                    panel.background = element_blank(),
                    plot.title = element_text(size=14),
                    plot.subtitle = element_text(size=8),
                    axis.title.x = element_text(size=8),
                    axis.title.y = element_text(size=8),
                    axis.text.x = element_text(size=8),
                    axis.text.y = element_text(size=8))

### Graph Variables ####################################################################


# the "common variables" or the variables that are not age dependent
common_vars <- c('unemp', 'savings', 'apple', 'atandt', 'verizon', 'divorced')
age_vars <- c('gen24', 'ment24', 'suicide24', 'ment25', 'gen25', 'suicide25')

dollars <- c('apple', 'atandt', 'verizon')
percents <- c('ment24', 'gen24', 'ment25', 'gen25', 'unemp', 'divorced')

# graph the suicide variables
temp_list <- graph_vars(final_data, c('suicide24', 'suicide25'), clean_names, dollars, percents, main_theme)

grid <- grid.arrange(grobs=temp_list, nrow=1,top='')
ggsave(here('Plots', 'suicide_variables.png'),plot=grid,dpi=300, width = 15, height = 7, units='in')

# graph the common variables
temp_list <- graph_vars(final_data, common_vars, clean_names, dollars, percents, main_theme)

grid <- grid.arrange(grobs=temp_list, nrow=2,top='')
ggsave(here('Plots', 'common_variables.png'),plot=grid,dpi=300, width = 15, height = 9, units='in')

# graph the age variables
temp_list <- graph_vars(final_data, age_vars, clean_names, dollars, percents, main_theme)

grid <- grid.arrange(grobs=temp_list, nrow=2,top='')
ggsave(here('Plots', 'age_variables.png'),plot=grid,dpi=300, width = 15, height = 9, units='in')

### Test for Seasonality ####################################################################


# Create month dummy variables
final_data_2 <- final_data %>%
  mutate(month_num=month(month_end),
         month_name=month.abb[month_num])

final_data_2 <- dummy_cols(final_data_2, select_columns = c('month_name'))

# get rid of the month_name_ prefix
names(final_data_2)[16:27] <- gsub('month_name_','',names(final_data_2)[16:27])

# Convert data to time-series and fit dummy regressions
ts_final_data <- ts(final_data_2, start=c(1999,1), frequency=12)

suicide24_lm <- lm(suicide24 ~ 
                            Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, 
                          data=ts_final_data)

suicide25_lm <- lm(suicide25 ~ 
                           Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, 
                         data=ts_final_data)

ment24_lm <- lm(ment24 ~ 
                             Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, 
                           data=ts_final_data)

ment25_lm <- lm(ment25 ~ 
                            Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, 
                          data=ts_final_data)

# grab the coefficients for the over 25 suicide regression
suicide25_coef <- coef(summary(suicide25_lm))

suicide25_coef_coef_tibble <- bind_cols(var_name=rownames(suicide25_coef), 
                                         as_tibble(suicide25_coef)) %>%
  clean_names() 

# create a pretty table for the coefficients
suicide25_coef_coef_pretty_table <- suicide25_coef_coef_tibble %>%
  filter(pr_t<=0.05) %>%
  rename('P-value' = pr_t,
         'Estimate'=estimate,
         'Standard Error'=std_error,
         'Test Statistic'=t_value,
         'Variable Name'=var_name) %>%
  dplyr::select('Variable Name',
                'Estimate',
                'Test Statistic',
                'Standard Error',
                'P-value') %>%
  mutate_if(is.numeric, ~round(.,4))%>%
  gt() %>%
  tab_options(table.font.names = c('Times New Roman', NULL))


gt::gtsave(suicide25_coef_coef_pretty_table ,here('Plots', 'suicide25_coef.png'))

### Adjust for Seasonality ####################################################################


# Remove seasonal component from suicide variables

suicide24_decomp <- decompose(ts_final_data[,'suicide24'])
suicide24_sa <- suicide24_decomp$x - suicide24_decomp$seasonal  

suicide25_decomp <- decompose(ts_final_data[,'suicide25'])
suicide25_sa <- suicide25_decomp$x - suicide25_decomp$seasonal  

# Remove seasonal component from mental health variables

ment24_decomp <- decompose(ts_final_data[,'ment24'])
ment24_sa <- ment24_decomp$x - ment24_decomp$seasonal  

ment25_decomp <- decompose(ts_final_data[,'ment25'])
ment25_sa <- ment25_decomp$x - ment25_decomp$seasonal

# add the seasonally adjusted variables
final_data_3 <- bind_cols(final_data_2, 
                          suicide24_sa=suicide24_sa, 
                          suicide25_sa=suicide25_sa, 
                          ment24_sa=ment24_sa, 
                          ment25_sa=ment25_sa)

# get rid of the dummy variables
final_data_3 <- final_data_3 %>%
  dplyr::select(!month_num:Sep)

### Graph Before and After Seasonality ####################################################################


percents <- c(percents, 'ment24_sa', 'ment25_sa')

# graph before
temp_list <- graph_vars(final_data_3, c('ment24', 'suicide24', 'ment25', 'suicide25'), clean_names, dollars, percents, main_theme)
grid <- grid.arrange(grobs=temp_list, nrow=1,top='')
ggsave(here('Plots', 'before_season_adjust.png'),plot=grid,dpi=300, width = 15, height = 4, units='in')

# graph after
temp_list <- graph_vars(final_data_3, c('ment24_sa', 'suicide24_sa', 'ment25_sa', 'suicide25_sa'), clean_names, dollars, percents, main_theme)
grid <- grid.arrange(grobs=temp_list, nrow=1,top='')
ggsave(here('Plots', 'after_season_adjust.png'),plot=grid,dpi=300, width = 15, height = 4, units='in')

ts_final_data <- ts(final_data_3, start=c(1999,1), frequency=12)

### Test for Stationarity ####################################################################

# variables we will test for stationarity
stationarity_vars <- c('gen24', 
                       'gen25', 
                       'divorced', 
                       'apple', 
                       'atandt', 
                       'verizon',
                       'unemp',
                       'savings',
                       'suicide24_sa',
                       'suicide25_sa',
                       'ment24_sa',
                       'ment25_sa')

# test for stationarity
stationarity_stats_1 <- test_station(ts_final_data, stationarity_vars)

nvar <- nrow(stationarity_stats_1)
nobs <- nrow(ts_final_data)

# remove the first variable because we adjusted for seasonality
final_data_3 <- final_data_3 %>%
  filter(month_end!=ymd('1999-01-31'))

### Adjust for stationarity####################################################################

for(i in 1:nvar){
  
  # If all four tests say that the data is stationary, don't adjust
  if(stationarity_stats_1[i,'total_stationary']<4) {
    new_name <- paste(stationarity_vars[i],'_station',sep='')
    
    # grab the indicator of if the log adjustment is needed
    log <- as.numeric(clean_names %>% filter(var_name==new_name) %>% dplyr::select(log))
    
    if(log==1){
      final_data_3 <- final_data_3 %>% 
        mutate(!!new_name:=diff(log(ts_final_data[,stationarity_vars[i]])))
    } else{
      final_data_3 <- final_data_3 %>% 
        mutate(!!new_name:=diff(ts_final_data[,stationarity_vars[i]]))
    }


  }
}

### Test after stationarity adjustment ####################################################################

stationarity_vars <- c('gen24_station', 
                       'gen25_station', 
                       'divorced_station', 
                       'apple_station', 
                       'atandt_station', 
                       'verizon_station',
                       'unemp_station',
                       'savings_station',
                       'suicide24_sa_station',
                       'suicide25_sa_station',
                       'ment24_sa_station',
                       'ment25_sa_station')

ts_final_data <- ts(final_data_3, start=c(1999,2), frequency=12)

# test variables again
stationarity_stats_2 <- test_station(ts_final_data, stationarity_vars)

# join the before and after stationarity adjustments into one table
final_stationarity <- stationarity_stats_1 %>%
  mutate(join_name = gsub('_sa','',stationarity_vars)) %>%
  left_join(stationarity_stats_2 %>%
              mutate(join_name = gsub('_sa_station','',stationarity_vars),
                     join_name = gsub('_station','', join_name)), by='join_name', suffix=c('', '_station'))

# create a pretty table of the stationary test results

# the alpha for the shading of the cells in the table
alpha_1 <- 0.4

pretty_stationary <- final_stationarity %>%
  left_join(clean_names, by=c('stationarity_vars'='var_name')) %>%
  dplyr::arrange(order) %>%
  dplyr::select(clean_name,
                df_testStat,
                pp_testStat,
                adfgls_testStat,
                kpss_testStat,
                df_result:kpss_result,
                df_testStat_station,
                pp_testStat_station,
                adfgls_testStat_station,
                kpss_testStat_station,
                df_result_station:kpss_result_station) %>%
  mutate_if(is.numeric,~round(.,4)) %>%
  gt(rowname_col = 'clean_name') %>%
  tab_row_group(
    group = html('Economic Variables'),
    rows = 1:2
  ) %>%
  tab_row_group(
    group = html('Societal Variables'),
    rows = 3:12
  ) %>%
  row_group_order(
    groups = c('Economic Variables', 'Societal Variables')
  )  %>%
  tab_spanner(
    label = 'Non-adjusted',
    columns = c('df_testStat', 'pp_testStat', 'adfgls_testStat', 'kpss_testStat')
  ) %>%
  tab_spanner(
    label = 'Adjusted',
    columns = c('df_testStat_station', 'pp_testStat_station', 'adfgls_testStat_station', 'kpss_testStat_station')
  )  %>%
  tab_style(
    style = cell_fill(color = "lightgray", alpha=alpha_1),
    locations = cells_body(
      columns = 'df_testStat',
      rows = df_result==0)
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray", alpha=alpha_1),
    locations = cells_body(
      columns = 'pp_testStat',
      rows = pp_result==0)
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray", alpha=alpha_1),
    locations = cells_body(
      columns = 'adfgls_testStat',
      rows = adfgls_result==0)
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray", alpha=alpha_1),
    locations = cells_body(
      columns = 'kpss_testStat',
      rows = kpss_result==0)
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray", alpha=alpha_1),
    locations = cells_body(
      columns = 'df_testStat_station',
      rows = df_result_station==0)
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray", alpha=alpha_1),
    locations = cells_body(
      columns = 'pp_testStat_station',
      rows = pp_result_station==0)
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray", alpha=alpha_1),
    locations = cells_body(
      columns = 'adfgls_testStat_station',
      rows = adfgls_result_station==0)
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray", alpha=alpha_1),
    locations = cells_body(
      columns = 'kpss_testStat_station',
      rows = kpss_result_station==0)
  ) %>%
  cols_label(
    df_testStat = 'ADF',
    pp_testStat = 'PP',
    adfgls_testStat = 'ERS',
    kpss_testStat = 'KPSS',
    df_testStat_station = 'ADF',
    pp_testStat_station = 'PP',
    adfgls_testStat_station = 'ERS',
    kpss_testStat_station = 'KPSS') %>%
  cols_hide(columns=c('df_result',
                      'pp_result',
                      'adfgls_result',
                      'kpss_result',
                      'df_result_station',
                      'pp_result_station',
                      'adfgls_result_station',
                      'kpss_result_station')) %>%
  tab_options(table.font.names = c('Times New Roman', NULL))

gt::gtsave(pretty_stationary ,here('Plots', 'stationarity_test_results.png'))

### Graph variables after stationarity adjustments####################################################################

common_vars <- c('unemp_station', 'savings_station', 'divorced_station','apple_station', 'atandt_station', 'verizon_station')
age_vars <- c('gen24_station', 'ment24_sa_station', 'suicide24_sa_station', 'gen25_station', 'ment25_sa_station', 'suicide25_sa_station')

temp_list <- graph_vars(final_data_3, common_vars, clean_names, dollars, percents, main_theme)
grid <- grid.arrange(grobs=temp_list, nrow=2,top='')
ggsave(here('Plots', 'common_variables_stationary.png'),plot=grid,dpi=300, width = 17, height = 8, units='in')

temp_list <- graph_vars(final_data_3, age_vars, clean_names, dollars, percents, main_theme)
grid <- grid.arrange(grobs=temp_list, nrow=2,top='')
ggsave(here('Plots', 'age_variables_stationary.png'),plot=grid,dpi=300, width = 17, height = 8, units='in')

### Create and graph VAR models####################################################################

main_theme <- theme(panel.grid = element_blank(),
                    panel.background = element_blank(),
                    plot.title = element_text(size=12),
                    plot.subtitle = element_text(size=8),
                    axis.title.x = element_text(size=8),
                    axis.title.y = element_text(size=8),
                    axis.text.x = element_text(size=8),
                    axis.text.y = element_text(size=8))

runs <- 1000
out_months <- 18
dpi <- 300
height <- 5
width <- 14

### Age 24 and Below (Adolescents)##############################################################

# Following creates the irf for combination of variables and responses listed below

# General Health, Mental Health and Suicides for 24 and Below

temp_var_plots_1 <- var_models(final_data_3, 
            c('gen24_station', 'ment24_sa_station'), 
            out_months = out_months, 
            response = 'suicide24_sa_station', 
            runs=runs, 
            against_self=T, 
            clean=clean_names,
            theme=main_theme,
            n_row = 1,
            title = 'Response of Suicides Per Thousand Adolescents to Various Shocks',
            file_name = 'age24only_VAR.png',
            dpi = dpi,
            width = width,
            height = height,
            layout=NULL)

# Mental Health, General Health and Suicides for 24 and Below

temp_var_plots_1 <- var_models(final_data_3, 
           c('ment24_sa_station', 'gen24_station'), 
           out_months = out_months, 
           response = 'suicide24_sa_station', 
           runs=runs, 
           against_self=T, 
           clean=clean_names,
           theme=main_theme,
           n_row = 1,
           title = 'Response of Suicides Per Thousand Adolescents to Various Shocks',
           file_name = 'age24only_reverse_VAR.png',
           dpi = dpi,
           width = width,
           height = height,
           layout = NULL)

# Common  societal variables against 24 and below suicides.  Stock variables first and then divorced

height <- 7
width <- 11.5

layout <- rbind(c(1,1,2,2,3,3), c(NA,4,4,5,5,NA))

temp_var_plots_1 <- var_models(final_data_3, 
           c('apple_station', 'atandt_station', 'verizon_station', 'divorced_station'), 
           out_months = out_months, 
           response = 'suicide24_sa_station', 
           runs=runs, 
           against_self=T, 
           clean=clean_names,
           theme=main_theme,
           n_row = 1,
           title = 'Response of Suicides Per Thousand Adolescents to Various Shocks',
           file_name = 'age24commonSocietal_VAR.png',
           dpi = dpi,
           width = width,
           height = height,
           layout = layout)

# Common economic variables against 24 and below suicides

height <- 5
width <- 14

temp_var_plots_1 <- var_models(final_data_3, 
           c('unemp_station', 'savings_station'), 
           out_months = out_months, 
           response = 'suicide24_sa_station', 
           runs=runs, 
           against_self=T, 
           clean=clean_names,
           theme=main_theme,
           n_row=1,
           title = 'Response of Suicides Per Thousand Adolescents to Various Shocks',
           file_name = 'age24commonEconomic_VAR.png',
           dpi = dpi,
           width = width,
           height = height,
           layout = NULL)

# All variables for 25 and Above.  Listed least endogenous to most endogenous
height <- 7
width <- 11.5

layout <- rbind(c(1,2,3), c(4,5,6), c(NA,7,NA))

temp_var_plots_1 <- var_models(final_data_3, 
           c('unemp_station','savings_station','apple_station','divorced_station','gen24_station','ment24_sa_station'), 
           out_months = out_months, 
           response = 'suicide24_sa_station', 
           runs=runs, 
           against_self=T, 
           clean=clean_names,
           theme=main_theme,
           n_row=3,
           title = 'Response of Suicides Per Thousand Adolescents to Various Shocks',
           file_name = 'age24all_VAR.png',
           dpi = dpi,
           width = width,
           height = height,
           layout=layout)


### Age 25 and Above (Adults)#######################################################################

# Following creates the irf for combination of variables and responses listed below

# General Health, Mental Health and Suicides for 25 and Above

height <- 5
width <- 14

temp_var_plots_1 <- var_models(final_data_3, 
           c('gen25_station', 'ment25_sa_station'), 
           out_months = out_months, 
           response = 'suicide25_sa_station', 
           runs=runs, 
           against_self=T, 
           clean=clean_names,
           theme=main_theme,
           n_row = 1,
           title = 'Response of Suicides Per Thousand Adults to Various Shocks',
           file_name = 'age25only_VAR.png',
           dpi = dpi,
           width = width,
           height = height,
           layout=NULL)

# Mental Health, General Health and Suicides for 25 and Above

temp_var_plots_1 <- var_models(final_data_3, 
           c('ment25_sa_station', 'gen25_station'), 
           out_months = out_months, 
           response = 'suicide25_sa_station', 
           runs=runs, 
           against_self=T, 
           clean=clean_names,
           theme=main_theme,
           n_row = 1,
           title = 'Response of Suicides Per Thousand Adults to Various Shocks',
           file_name = 'age25only_reverse_VAR.png',
           dpi = dpi,
           width = width,
           height = height,
           layout = NULL)

# Common  societal variables against 25 and Above suicides.  Stock variables first and then divorced


height <- 7
width <- 11.5

layout <- rbind(c(1,1,2,2,3,3), c(NA,4,4,5,5,NA))

temp_var_plots_1 <- var_models(final_data_3, 
           c('apple_station', 'atandt_station', 'verizon_station', 'divorced_station'), 
           out_months = out_months, 
           response = 'suicide25_sa_station', 
           runs=runs, 
           against_self=T, 
           clean=clean_names,
           theme=main_theme,
           n_row = 1,
           title = 'Response of Suicides Per Thousand Adults to Various Shocks',
           file_name = 'age25commonSocietal_VAR.png',
           dpi = dpi,
           width = width,
           height = height,
           layout = layout)

# Common economic variables against 25 and Above suicides


height <- 5
width <- 14

temp_var_plots_1 <- var_models(final_data_3, 
           c('unemp_station', 'savings_station'), 
           out_months = out_months, 
           response = 'suicide25_sa_station', 
           runs=runs, 
           against_self=T, 
           clean=clean_names,
           theme=main_theme,
           n_row=1,
           title = 'Response of Suicides Per Thousand Adults to Various Shocks',
           file_name = 'age25commonEconomic_VAR.png',
           dpi = dpi,
           width = width,
           height = height,
           layout = NULL)

# All variables for 25 and Above.  Listed least endogenous to most endogenous

height <- 7
width <- 11.5

layout <- rbind(c(1,2,3), c(4,5,6), c(NA,7,NA))

temp_var_plots_1 <- var_models(final_data_3, 
           c('unemp_station','savings_station','apple_station','divorced_station','gen25_station','ment25_sa_station'), 
           out_months = out_months, 
           response = 'suicide25_sa_station', 
           runs=runs, 
           against_self=T, 
           clean=clean_names,
           theme=main_theme,
           n_row=3,
           title = 'Response of Suicides Per Thousand Adults to Various Shocks',
           file_name = 'age25all_VAR.png',
           dpi = dpi,
           width = width,
           height = height,
           layout=layout)

# Fit two models.  First 24 and below suicides vs 25 and above suicides
# Don't save these variables

temp_var_plots_1 <- var_models(final_data_3, 
                               c('suicide24_sa_station'), 
                               out_months = out_months, 
                               response = 'suicide25_sa_station', 
                               runs=runs, 
                               against_self=F, 
                               clean=clean_names,
                               theme=main_theme,
                               n_row=1,
                               title = '',
                               file_name = '',
                               dpi = dpi,
                               width = width,
                               height = height,
                               layout=layout)

temp_var_plots_2 <- var_models(final_data_3, 
                               c('suicide25_sa_station'), 
                               out_months = out_months, 
                               response = 'suicide24_sa_station', 
                               runs=runs, 
                               against_self=F, 
                               clean=clean_names,
                               theme=main_theme,
                               n_row=1,
                               title = '',
                               file_name = '',
                               dpi = dpi,
                               width = width,
                               height = height,
                               layout=layout)
# Instead of graphing them as normal, graph the responses of these two models in one graph

temp_list <- c(list(temp_var_plots_1[[1]] +  
                      scale_y_continuous(labels = percent, limits=c(-0.00005,0.0002)) + 
                      labs(x='Month',y='',title=paste('Response: ','Suicides Per Thousand Adolescents',' \nShock: ','Suicides Per Thousand Adults',sep=''))), 
               list(temp_var_plots_2[[1]] +  
                      scale_y_continuous(labels = percent, limits=c(-0.00005,0.0002)) + 
                      labs(x='Month',y='',title=paste('Response: ','Suicides Per Thousand Adults',' \nShock: ','Suicides Per Thousand Adolescents',sep=''))))

grid <- grid.arrange(grobs=temp_list, nrow=1,top='')
ggsave(here('Plots', 'suicideOnly_VAR.png'),plot=grid,dpi=300, width = width, height = height, units='in')

###Fit some ARMA models############################################################################

main_theme <- theme(panel.grid = element_blank(),
                    panel.background = element_blank(),
                    plot.title = element_text(size=16),
                    plot.subtitle = element_text(size=8),
                    axis.title.x = element_text(size=8),
                    axis.title.y = element_text(size=8),
                    axis.text.x = element_text(size=8),
                    axis.text.y = element_text(size=8))
# max lag we will check
max_lag <- 36

ardl_aic <- c()
ardl_models <- c()

# fit an ARMA model with all values of lag for 1 to 24
for(i in 1:max_lag){
  temp_ARDL <- dynlm(suicide24_sa_station ~ 
                       L(suicide24_sa_station, 1:i) +
                       L(gen24_station, 0:i) + 
                       L(divorced_station, 0:i) + 
                       L(apple_station, 0:i) +
                       L(unemp_station, 0:i) +
                       L(savings_station, 0:i) +
                       L(ment24_sa_station, 0:i)
                     , data = ts_final_data)
  
  ardl_models <- append(ardl_models, list(temp_ARDL))
  ardl_aic <- append(ardl_aic, abs(AIC(temp_ARDL)))
}

ardl_aic_tibble <- tibble(lag=1:max_lag, aic=ardl_aic)

best_model_num <- which.min(ardl_aic)

# plot the AIC for each lag length
aic_plot <- ggplot(ardl_aic_tibble, aes(x=lag, y=aic)) +
  geom_line() +
  geom_point() +
  geom_point(data=NULL, aes(x=best_model_num, y=ardl_aic[best_model_num]), color='red', size=4) +
  main_theme +
  scale_x_continuous(breaks=seq(from=2, to=max_lag, by =2)) +
  labs(title='AIC For Various Lags for Adolescent ADRL Models (Lowest AIC in Red)', x='Lag', y='AIC')

ggsave(here('Plots', 'age24_arma_aic.png'),plot=aic_plot,dpi=300, width = 11.5, height = 8, units='in')

# grab the best model coefficients
best_model_coef <- coef(summary(ardl_models[[best_model_num]]))

best_model_tibble <- bind_cols(var_name=rownames(best_model_coef), as_tibble(best_model_coef)) %>%
  clean_names() %>%
  mutate(Lag =  rep(seq(0,which.min(ardl_aic)),7))

# create a pretty table
best_model_pretty_table <- best_model_tibble %>% 
  mutate(pretty_var_name =  gsub('L\\(','',gsub(',.*','',var_name))) %>%
  left_join(clean_names, by=c('pretty_var_name'='var_name')) %>%
  mutate(clean_name = if_else(is.na(clean_name),'Intercept', clean_name)) %>%
  filter(pr_t<=0.05) %>%
  rename('P-value' = pr_t,
         'Estimate'=estimate,
         'Standard Error'=std_error,
         'Test Statistic'=t_value,
         'Variable Name'=clean_name) %>%
  dplyr::select('Variable Name',
                'Lag',
                'Estimate',
                'Test Statistic',
                'Standard Error',
                'P-value') %>%
  mutate_if(is.numeric, ~round(.,4)) %>%
  gt() %>%
  tab_options(table.font.names = c('Times New Roman', NULL))

gt::gtsave(best_model_pretty_table,here('Plots', 'age24_arma_best_coef.png'))

# Do the same for over 25

max_lag <- 36

ardl_aic <- c()
ardl_models <- c()

for(i in 1:max_lag){
  temp_ARDL <- dynlm(suicide25_sa_station ~ 
                       L(suicide25_sa_station, 1:i) +
                       L(gen25_station, 0:i) + 
                       L(divorced_station, 0:i) + 
                       L(apple_station, 0:i) +
                       L(unemp_station, 0:i) +
                       L(savings_station, 0:i) +
                       L(ment25_sa_station, 0:i)
                     , data = ts_final_data)
  
  ardl_models <- append(ardl_models, list(temp_ARDL))
  ardl_aic <- append(ardl_aic, abs(AIC(temp_ARDL)))
}

ardl_aic_tibble <- tibble(lag=1:max_lag, aic=ardl_aic)

best_model_num <- which.min(ardl_aic)

aic_plot <- ggplot(ardl_aic_tibble, aes(x=lag, y=aic)) +
  geom_line() +
  geom_point() +
  geom_point(data=NULL, aes(x=best_model_num, y=ardl_aic[best_model_num]), color='red', size=4) +
  main_theme +
  scale_x_continuous(breaks=seq(from=2, to=max_lag, by =2)) +
  labs(title='AIC For Various Lags for Adult ADRL Models (Lowest AIC in Red)', x='Lag', y='AIC')

ggsave(here('Plots', 'age25_arma_aic.png'),plot=aic_plot,dpi=300, width = 11.5, height = 8, units='in')

best_model_coef <- coef(summary(ardl_models[[best_model_num]]))

best_model_tibble <- bind_cols(var_name=rownames(best_model_coef), as_tibble(best_model_coef)) %>%
  clean_names() %>%
  mutate(Lag =  rep(seq(0,which.min(ardl_aic)),7))

best_model_pretty_table <- best_model_tibble %>% 
  mutate(pretty_var_name =  gsub('L\\(','',gsub(',.*','',var_name))) %>%
  left_join(clean_names, by=c('pretty_var_name'='var_name')) %>%
  mutate(clean_name = if_else(is.na(clean_name),'Intercept', clean_name)) %>%
  filter(pr_t<=0.05) %>%
  rename('P-value' = pr_t,
         'Estimate'=estimate,
         'Standard Error'=std_error,
         'Test Statistic'=t_value,
         'Variable Name'=clean_name) %>%
  dplyr::select('Variable Name',
                'Lag',
                'Estimate',
                'Test Statistic',
                'Standard Error',
                'P-value') %>%
  mutate_if(is.numeric, ~round(.,4)) %>%
  gt() %>%
  tab_options(table.font.names = c('Times New Roman', NULL))

gt::gtsave(best_model_pretty_table,here('Plots', 'age25_arma_best_coef.png'))