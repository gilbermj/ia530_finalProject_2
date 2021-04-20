# -*- coding: utf-8 -*-
"""
Michael Gilbert & Amy Moyer

The purpose of this code is just to grab the BRFSS data from online

"""

import os
import requests
from zipfile import ZipFile

# get the current working directory
current_path = os.getcwd()

parent_dir = os.path.dirname(current_path)

data_directory = os.path.join(parent_dir, 'Data')

brfss_dir = os.path.join(data_directory, 'brfss')

zipped_file_paths = []

# read in the links to the BRFSS files that we want
with open(os.path.join(data_directory, 'brfss_links.txt')) as f:
    brfss_file_urls = f.read().split('\n')

# get the list of files that have already been read in
# this is to prevent duplicate reading of files in the 
# event you run this script twice
# probably should just use a try catch.  This is to avoid requesting the files
# more than once
exist_files = os.listdir(brfss_dir)  
exclude_files = []

for e in exist_files:
    exclude_files.append(e.split('.')[0].lower())


for url in brfss_file_urls:
    
    # get the zipped file name
    zipped_file_name = os.path.join(brfss_dir, url.split('/')[-1])
    
    short_name = url.split('/')[-1].split('XPT.')[0].lower()
    
    # if the file name in the brfss_links is not in the list 
    # of files already in the Data/brfss folder
    # request the file, unzip and remove the zipped file
    if short_name not in exclude_files:
        r = requests.get(url)
        
        with open(zipped_file_name, 'wb') as f:
            f.write(r.content)
    
        # un-zip the file
        with ZipFile(zipped_file_name) as zip_file:
            # Extract to new folder with same name as zip
            zip_file.extractall(path=brfss_dir)
        
        os.remove(zipped_file_name)
    
