# -*- coding: utf-8 -*-
"""
Created on Mon Mar  8 13:11:02 2021

@author: gilberm
https://api.usa.gov/crime/fbi/sapi/api/data/arrest/national/offense/monthly/1999/2019?API_KEY=iiHnOKfno2Mgkt5AynpvPpUQTEyxE77jo1RU8PIv
"""

import os
import requests
from zipfile import ZipFile

current_path = os.getcwd()

parent_dir = os.path.dirname(current_path)

data_directory = os.path.join(parent_dir, 'Data')

brfss_dir = os.path.join(data_directory, 'brfss')

zipped_file_paths = []

with open(os.path.join(data_directory, 'brfss_links.txt')) as f:
    brfss_file_urls = f.read().split('\n')

exist_files = os.listdir(brfss_dir)  
exclude_files = []

for e in exist_files:
    exclude_files.append(e.split('.')[0].lower())


for url in brfss_file_urls:
    
    zipped_file_name = os.path.join(brfss_dir, url.split('/')[-1])
    
    short_name = url.split('/')[-1].split('XPT.')[0].lower()
    
    if short_name not in exclude_files:
        r = requests.get(url)
        
        with open(zipped_file_name, 'wb') as f:
            f.write(r.content)
    
        # un-zip the file
        with ZipFile(zipped_file_name) as zip_file:
            # Extract to new folder with same name as zip
            zip_file.extractall(path=brfss_dir)
        
        os.remove(zipped_file_name)
    
