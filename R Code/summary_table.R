library(tidyverse)
library(here)
library(gt)

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



summary <- final_data %>%
  pivot_longer(cols=-month_end, names_to = 'Variable',values_to='value') %>%
  group_by(Variable) %>%
  summarize(Mean=mean(value),
            SD=sd(value),
            Min=min(value),
            Max=max(value)) %>%
  left_join(clean_names, by=c('Variable'='var_name')) %>%
  mutate(Source = c('Yahoo! Finance',
                    'Yahoo! Finance',
                    'BRFSS',
                    'BRFSS',
                    'BRFSS',
                    'BRFSS',
                    'BRFSS',
                    'FRED',
                    'CDC',
                    'CDC',
                    'FRED',
                    'Yahoo! Finance')) %>%
  arrange(order) %>%
  mutate_if(is.numeric, ~round(.,4)) %>%
  dplyr::select(Variable=clean_name, Mean, SD, Min, Max,Source) %>%
  gt(rowname_col = 'Variable') %>%
  tab_options(table.font.names = c('Times New Roman', NULL)) %>%
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
  )

gt::gtsave(summary ,here('Plots', 'summary_stats.png'))
