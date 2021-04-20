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

graph_vars <- function(data, vars, clean, dollars, percents, theme){
  
  temp_list <- list()
  
  for(i in vars){
    
    clean_name <- as.character(clean_names %>% filter(var_name==i) %>% dplyr::select(clean_name))
    
    temp_plot <- ggplot(data, aes_string(x='month_end', y=i)) + 
      geom_line() +
      theme
    
    if(grepl('_sa',i) & !grepl('_station',i)){
      temp_plot <- temp_plot + labs(title=paste(clean_name,'\n - Seasonally Adjusted', sep=''), x='', y='')
    } else if(grepl('_station',i)) {
      temp_plot <- temp_plot + labs(title=paste(clean_name,' - Stationary', sep=''), x='', y='')
    } else {
      temp_plot <- temp_plot + labs(title=clean_name, x='', y='')
    }
      
    
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

pretty_station <- function(st_stats, clean_names){
  alpha_1 <- 0.4
  
  station_pretty <- st_stats %>%
    left_join(clean_names, by=c('stationarity_vars'='var_name')) %>%
    dplyr::arrange(clean_name) %>%
    dplyr::select('Variable Name'=clean_name,
                  'ADF'=df_testStat,
                  'PP'=pp_testStat,
                  'ERS'=adfgls_testStat,
                  'KPSS'=kpss_testStat,
                  df_result,
                  pp_result,
                  adfgls_result,
                  kpss_result) %>%
    mutate_if(is.numeric, ~round(.,4)) %>%
    gt()  %>%
    tab_style(
      style = cell_fill(color = "lightgray", alpha=alpha_1),
      locations = cells_body(
        columns = 'ADF',
        rows = df_result==0)
    ) %>%
    tab_style(
      style = cell_fill(color = "lightgray", alpha=alpha_1),
      locations = cells_body(
        columns = 'PP',
        rows = pp_result==0)
    ) %>%
    tab_style(
      style = cell_fill(color = "lightgray", alpha=alpha_1),
      locations = cells_body(
        columns = 'ERS',
        rows = adfgls_result==0)
    ) %>%
    tab_style(
      style = cell_fill(color = "lightgray", alpha=alpha_1),
      locations = cells_body(
        columns = 'KPSS',
        rows = kpss_result==0)
    ) %>%
    cols_hide(columns=c('df_result',
                        'pp_result',
                        'adfgls_result',
                        'kpss_result')) %>%
    cols_width(
      'Variable Name' ~ px(400)
    )
}

var_models <- function(data, vars, out_months, response, runs=100, against_self=T, clean){
  
  # grab only the variables we need
  temp_data <- data %>%
    dplyr::select(month_end, vars, all_of(response))
  
  start_year <- year(min(temp_data$month_end))
  start_month <- month(min(temp_data$month_end))
  
  temp_data <- temp_data %>%
    dplyr::select(-c(month_end))
  
  ts <- temp_data %>% ts(start=c(start_year,start_month), frequency=12)
  
  # convert to time-series and the VAR models
  var_model <- VAR(ts, lag.max = 12, ic = 'AIC')
  
  ts_names <- names(temp_data)
  
  
  ######################
  
  # tibbles to hold the impulse response functions as well as the lower and
  # upper bounds
  lower <- tibble(month=1:(out_months+1))
  irf <- tibble(month=1:(out_months+1))
  upper <- tibble(month=1:(out_months+1))

  
  for(name in ts_names){
    
    if(name!='month_end' & (name!=response | against_self)){
      
      # when we fit the final models maybe do a 1000 runs
      temp_irf <- irf(var_model,impulse=c(name), response=c(response), n.ahead=out_months, cumulative = TRUE,runs=runs, ci=0.95)
      
      lower <- bind_cols(lower, !!name:=temp_irf$Lower[[1]])
      upper <- bind_cols(upper, !!name:=temp_irf$Upper[[1]])
      irf <- bind_cols(irf, !!name:=temp_irf$irf[[1]])
      
      print(name)
    }
 
  }
  
  # get minimum and maximum for lower and upper bounds
  min_lower <- min(lower[,2:ncol(lower)])
  max_upper <- max(upper[,2:ncol(upper)])
  
  temp_list <- list() 
  
  for(name in ts_names){
    
    # get the nice names for graphing
    clean_name <- as.character(clean_names %>% filter(var_name==name) %>% dplyr::select(clean_name))
    
    #clean_name <- gsub('Adolescents','', clean_name)
    #clean_name <- gsub('Adults','', clean_name)
    
    response_name <- as.character(clean_names %>% filter(var_name==response) %>% dplyr::select(clean_name))
    
    #response_name <- gsub('Adolescents','', response_name)
    #response_name <- gsub('Adults','', response_name)
    
    if(name!='month_end' & (name!=response | against_self)){
      
      
      temp_plot <- ggplot(data=lower, aes_string(x='month', y=name)) +
        geom_line(color='red', alpha=0.75, linetype = "dashed") + #lower bound
        geom_line(data=irf, aes_string(x='month', y=name)) + # irf
        geom_line(data=upper, aes_string(x='month', y=name), color='red', alpha=0.75, linetype = "dashed") + #upper bound
        geom_hline(yintercept=0) +
        scale_x_continuous(breaks=seq(from=2, to=out_months, by=4)) +
        scale_y_continuous(labels = percent, limits=c(min_lower,max_upper)) +
        main_theme +
        labs(x='Month', 
             y='', 
             title=paste(clean_name,sep=''))
      
      temp_list <- append(temp_list, list(temp_plot))
      
    }
    
  }
  
  return(temp_list)
  
  
}



final_data <- read_csv(here('Data','final_data.csv'))

# filter out unneeded variables
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
                    plot.title = element_text(size=12),
                    plot.subtitle = element_text(size=8),
                    axis.title.x = element_text(size=8),
                    axis.title.y = element_text(size=8),
                    axis.text.x = element_text(size=8),
                    axis.text.y = element_text(size=8))

common_vars <- c('apple', 'atandt', 'verizon', 'divorced', 'savings', 'unemp')
age_vars <- c('gen24', 'ment24', 'suicide24', 'gen25', 'ment25', 'suicide25')

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

# Create month dummy variables

final_data_2 <- final_data %>%
  mutate(month_num=month(month_end),
         month_name=month.abb[month_num])

final_data_2 <- dummy_cols(final_data_2, select_columns = c('month_name'))

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

suicide25_coef <- coef(summary(suicide25_lm))

suicide25_coef_coef_tibble <- bind_cols(var_name=rownames(suicide25_coef), 
                                         as_tibble(suicide25_coef)) %>%
  clean_names() 

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
  gt() 


gt::gtsave(suicide25_coef_coef_pretty_table ,here('Plots', 'suicide25_coef.png'))

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

final_data_3 <- bind_cols(final_data_2, 
                          suicide24_sa=suicide24_sa, 
                          suicide25_sa=suicide25_sa, 
                          ment24_sa=ment24_sa, 
                          ment25_sa=ment25_sa)


final_data_3 <- final_data_3 %>%
  dplyr::select(!month_num:Sep)


percents <- c(percents, 'ment24_sa', 'ment25_sa')

temp_list <- graph_vars(final_data_3, c('ment24', 'suicide24', 'ment25', 'suicide25'), clean_names, dollars, percents, main_theme)

grid <- grid.arrange(grobs=temp_list, nrow=1,top='')
ggsave(here('Plots', 'before_season_adjust.png'),plot=grid,dpi=300, width = 15, height = 4, units='in')

temp_list <- graph_vars(final_data_3, c('ment24_sa', 'suicide24_sa', 'ment25_sa', 'suicide25_sa'), clean_names, dollars, percents, main_theme)

grid <- grid.arrange(grobs=temp_list, nrow=1,top='')
ggsave(here('Plots', 'after_season_adjust.png'),plot=grid,dpi=300, width = 15, height = 4, units='in')

ts_final_data <- ts(final_data_3, start=c(1999,1), frequency=12)


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

stationarity_stats_1 <- test_station(ts_final_data, stationarity_vars)

nvar <- nrow(stationarity_stats_1)
nobs <- nrow(ts_final_data)

# remove the first variable because we adjusted for seasonality
final_data_3 <- final_data_3 %>%
  filter(month_end!=ymd('1999-01-31'))

for(i in 1:nvar){
  
  # If all four tests say that the data is stationary, don't adjust
  if(stationarity_stats_1[i,'total_stationary']<4) {
    var_name <- paste(stationarity_vars[i],'_station',sep='')
    final_data_3 <- final_data_3 %>% 
      mutate(!!var_name:=diff(log(ts_final_data[,stationarity_vars[i]])))
  }
}

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

stationarity_stats_2 <- test_station(ts_final_data, stationarity_vars)

pretty_station_before <- pretty_station(stationarity_stats_1, clean_names)
pretty_station_after <- pretty_station(stationarity_stats_2, clean_names)

gt::gtsave(pretty_station_before ,here('Plots', 'stationarity_test_results_before.png'))
gt::gtsave(pretty_station_after ,here('Plots', 'stationarity_test_results_after.png'))

common_vars <- c('apple_station', 'atandt_station', 'verizon_station', 'divorced_station', 'savings_station', 'unemp_station')
age_vars <- c('gen24_station', 'ment24_sa_station', 'suicide24_sa_station', 'gen25_station', 'ment25_sa_station', 'suicide25_sa_station')

temp_list <- graph_vars(final_data_3, common_vars, clean_names, dollars, percents, main_theme)

grid <- grid.arrange(grobs=temp_list, nrow=2,top='')
ggsave(here('Plots', 'common_variables_stationary.png'),plot=grid,dpi=300, width = 17, height = 8, units='in')

temp_list <- graph_vars(final_data_3, age_vars, clean_names, dollars, percents, main_theme)

grid <- grid.arrange(grobs=temp_list, nrow=2,top='')
ggsave(here('Plots', 'age_variables_stationary.png'),plot=grid,dpi=300, width = 17, height = 8, units='in')

main_theme <- theme(panel.grid = element_blank(),
                    panel.background = element_blank(),
                    plot.title = element_text(size=11),
                    plot.subtitle = element_text(size=8),
                    axis.title.x = element_text(size=8),
                    axis.title.y = element_text(size=8),
                    axis.text.x = element_text(size=8),
                    axis.text.y = element_text(size=8))

height <- 5
width <- 17
runs <- 1000
out_months <- 30

temp_var_plots <- var_models(final_data_3, 
                    c('gen24_station', 'ment24_sa_station'), 
                    out_months = out_months, 
                    response = 'suicide24_sa_station', 
                    runs=runs, 
                    against_self=T, 
                    clean=clean_names)

grid <- grid.arrange(grobs=temp_var_plots, nrow=1,top='Response of Suicides Per Thousand Adolescents to Various Shocks')
ggsave(here('Plots', 'age24only_VAR.png'),plot=grid,dpi=300, width = width, height = height, units='in')

temp_var_plots <- var_models(final_data_3, 
                    c('ment24_sa_station', 'gen24_station'), 
                    out_months = out_months, 
                    response = 'suicide24_sa_station', 
                    runs=runs, 
                    against_self=T, 
                    clean=clean_names)

grid <- grid.arrange(grobs=temp_var_plots, nrow=1,top='Response of Suicides Per Thousand Adolescents to Various Shocks')
ggsave(here('Plots', 'age24only_reverse_VAR.png'),plot=grid,dpi=300, width = width, height = height, units='in')

temp_var_plots <- var_models(final_data_3, 
                             c('apple_station', 'atandt_station', 'verizon_station', 'divorced_station'), 
                             out_months = out_months, 
                             response = 'suicide24_sa_station', 
                             runs=runs, 
                             against_self=T, 
                             clean=clean_names)

grid <- grid.arrange(grobs=temp_var_plots, nrow=1,top='Response of Suicides Per Thousand Adolescents to Various Shocks')
ggsave(here('Plots', 'age24commonSocietal_VAR.png'),plot=grid,dpi=300, width = width, height = height, units='in')

temp_var_plots <- var_models(final_data_3, 
                             c('savings_station', 'unemp_station'), 
                             out_months = out_months, 
                             response = 'suicide24_sa_station', 
                             runs=runs, 
                             against_self=T, 
                             clean=clean_names)

grid <- grid.arrange(grobs=temp_var_plots, nrow=1,top='Response of Suicides Per Thousand Adolescents to Various Shocks')
ggsave(here('Plots', 'age24commonEconomic_VAR.png'),plot=grid,dpi=300, width = 12, height = height, units='in')

temp_var_plots <- var_models(final_data_3, 
                             c('ment24_sa_station', 'gen24_station', 'apple_station', 'divorced_station', 'savings_station', 'unemp_station'), 
                             out_months = out_months, 
                             response = 'suicide24_sa_station', 
                             runs=runs, 
                             against_self=T, 
                             clean=clean_names)
height <- 7
width <- 11.5

layout <- rbind(c(1,2,3), c(4,5,6), c(NA,7,NA))

grid <- grid.arrange(grobs=temp_var_plots,layout_matrix=layout, nrow=3,top='Response of Suicides Per Thousand Adolescents to Various Shocks')
ggsave(here('Plots', 'age24all_VAR.png'),plot=grid,dpi=300, width = width, height = height, units='in')

# 25 and older

height <- 5
width <- 17

temp_var_plots <- var_models(final_data_3, 
                             c('gen25_station', 'ment25_sa_station'), 
                             out_months = out_months, 
                             response = 'suicide25_sa_station', 
                             runs=runs, 
                             against_self=T, 
                             clean=clean_names)

grid <- grid.arrange(grobs=temp_var_plots, nrow=1,top='Response of Suicides Per Thousand Adults to Various Shocks')
ggsave(here('Plots', 'age25only_VAR.png'),plot=grid,dpi=300, width = width, height = height, units='in')

temp_var_plots <- var_models(final_data_3, 
                             c('ment25_sa_station', 'gen25_station'), 
                             out_months = out_months, 
                             response = 'suicide25_sa_station', 
                             runs=runs, 
                             against_self=T, 
                             clean=clean_names)

grid <- grid.arrange(grobs=temp_var_plots, nrow=1,top='Response of Suicides Per Thousand Adults to Various Shocks')
ggsave(here('Plots', 'age25only_reverse_VAR.png'),plot=grid,dpi=300, width = width, height = height, units='in')

temp_var_plots <- var_models(final_data_3, 
                             c('apple_station', 'atandt_station', 'verizon_station', 'divorced_station'), 
                             out_months = out_months, 
                             response = 'suicide25_sa_station', 
                             runs=runs, 
                             against_self=T, 
                             clean=clean_names)

grid <- grid.arrange(grobs=temp_var_plots, nrow=1,top='Response of Suicides Per Thousand Adults to Various Shocks')
ggsave(here('Plots', 'age25commonSocietal_VAR.png'),plot=grid,dpi=300, width = width, height = height, units='in')

temp_var_plots <- var_models(final_data_3, 
                             c('savings_station', 'unemp_station'), 
                             out_months = out_months, 
                             response = 'suicide25_sa_station', 
                             runs=runs, 
                             against_self=T, 
                             clean=clean_names)

grid <- grid.arrange(grobs=temp_var_plots, nrow=1,top='Response of Suicides Per Thousand Adults to Various Shocks')
ggsave(here('Plots', 'age25commonEconomic_VAR.png'),plot=grid,dpi=300, width = 12, height = height, units='in')

temp_var_plots <- var_models(final_data_3, 
                             c('ment25_sa_station', 'gen25_station', 'apple_station', 'divorced_station', 'savings_station', 'unemp_station'), 
                             out_months = out_months, 
                             response = 'suicide25_sa_station', 
                             runs=runs, 
                             against_self=T, 
                             clean=clean_names)
height <- 6
width <- 11.5

layout <- rbind(c(1,2,3), c(4,5,6), c(NA,7,NA))

grid <- grid.arrange(grobs=temp_var_plots,layout_matrix=layout, nrow=3,top='Response of Suicides Per Thousand Adults to Various Shocks')
ggsave(here('Plots', 'age25all_VAR.png'),plot=grid,dpi=300, width = width, height = height, units='in')

temp_var_plots_1 <- var_models(final_data_3, 
                             c('suicide24_sa_station'), 
                             out_months = out_months, 
                             response = 'suicide25_sa_station', 
                             runs=runs, 
                             against_self=F, 
                             clean=clean_names)

temp_var_plots_2 <- var_models(final_data_3, 
                             c('suicide25_sa_station'), 
                             out_months = out_months, 
                             response = 'suicide24_sa_station', 
                             runs=runs, 
                             against_self=F, 
                             clean=clean_names)

temp_list <- c(list(temp_var_plots_1[[1]] +  
                      scale_y_continuous(labels = percent, limits=c(-0.015,0.02)) + 
                      labs(x='Month',y='',title=paste('Response: ','Suicides Per Thousand Adolescents',' \nShock: ','Suicides Per Thousand Adults',sep=''))), 
               list(temp_var_plots_2[[1]] +  
                      scale_y_continuous(labels = percent, limits=c(-0.015,0.02)) + 
                      labs(x='Month',y='',title=paste('Response: ','Suicides Per Thousand Adults',' \nShock: ','Suicides Per Thousand Adolescents',sep=''))))

grid <- grid.arrange(grobs=temp_list, nrow=1,top='')
ggsave(here('Plots', 'suicideOnly_VAR.png'),plot=grid,dpi=300, width = width, height = height, units='in')

# try some arma models

main_theme <- theme(panel.grid = element_blank(),
                    panel.background = element_blank(),
                    plot.title = element_text(size=16),
                    plot.subtitle = element_text(size=8),
                    axis.title.x = element_text(size=8),
                    axis.title.y = element_text(size=8),
                    axis.text.x = element_text(size=8),
                    axis.text.y = element_text(size=8))

max_lag <- 24

ardl_aic <- c()
ardl_models <- c()

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

aic_plot <- ggplot(ardl_aic_tibble, aes(x=lag, y=aic)) +
  geom_line() +
  geom_point() +
  geom_point(data=NULL, aes(x=best_model_num, y=ardl_aic[best_model_num]), color='red', size=4) +
  main_theme +
  scale_x_continuous(breaks=seq(from=2, to=max_lag, by =2)) +
  labs(title='AIC For Various Lags for Adolescent ADRL Models (Lowest AIC in Red)', x='Lag', y='AIC')

aic_plot

ggsave(here('Plots', 'age24_arma_aic.png'),plot=aic_plot,dpi=300, width = 11.5, height = 8, units='in')

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
  gt() 

gt::gtsave(best_model_pretty_table,here('Plots', 'age24_arma_best_coef.png'))

# over 25

max_lag <- 24

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

aic_plot

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
  gt() 

gt::gtsave(best_model_pretty_table,here('Plots', 'age25_arma_best_coef.png'))
