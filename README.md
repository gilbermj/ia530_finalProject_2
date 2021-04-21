Amy Moyer & Michael Gilbert 
IA 530 
Final Project ReadMe
May 2021

# Introduction

The following is a ReadMe file for the organization of our code for the final project for IA 530

# Folders

1. Data: contains all the data for this project.
    a. brfss\:  this is a folder and holds the raw brfss data.  There will be no data in here unless you run get_brfss_data.py
    b. all_brfss_data.csv:  this is the compiled BRFSS data from the folder in (a).  This is compiled in the script read_brfss_data.ReadMe
    c. all_names_cleaned.csv:  this is the list of variable names and the "pretty" names.  A pretty name is just a nicer looking variable name for graphing.  This also contains and ordering variable and a variable indicating if the variable should be log transformed
    d. apple_stock.csv:  the daily Apple stock prices from Yahoo! finance
    e. atandt_stock.csv: the daily AT&T stock prices from Yahoo! finance
    f. brfss_codes.csv:  this is a file of BRFSS codes to link up to the codes in the all_brfss_data.csv file.  Often survey responses are given codes like 88 for None or 44 for Not/Sure.  This file is used to convert those numeric responses to strings
    g. brfss_links.txt:  this is a list of the BRFSS file urls
    h. final_data.csv:  this is the final data for analysis.  This is produced by compile_all_data_2.R and used in final_data_analysis_2.R
    i. PerSavingsRate_1999to2019.csv:  the personal savings rate data from St Louis FRED
    j. suicide_data_1999_2019.txt:  the suicide data from the CDC Wonder
    k. unemp.csv:  the monthly unemployment rate from FRED
    l. us_population_data.txt:  the monthly US population from US Census Bureau
    m. verizon_stock.csv: the daily Verizon stock prices from Yahoo! finance
2. Plots: contains the plots produced as a result of final_data_analysis_2.R
3. R Code:
    a. combine_all_data_2.R: this code combines all the data in the Data folder into the final_data.csv for analysis
    b. final_data_analysis_2.R: code completes all analysis for the paper
    c. get_brfss_data.py: code downloads the BRFSS data from the web, unzips it and puts it in the Data/brfss folder
    d. read_brfss_data.R: reads the data downloaded in the Data/brfss folder and puts it into the all_brfss_data.csv
4. .here: necessary file to indicate the parent directory for the snippits of code in the R Code folder
