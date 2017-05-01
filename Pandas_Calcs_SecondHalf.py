# Michael Mangiante
# 9/20/2016

# Calculate Total Annual Flow and Mean R&B Flashiness

import csv
import glob
import pandas as pd
#import numpy as np

# Target File Location for Annual Summary Statistics
AnnualCSV = r'H:\Streamflow\US_Watersheds_Automation\Annual_Summary_Flash_Flow.csv'

# Target File location for 5 year average Summary Stats

CSV_5yrStats = r'H:\Streamflow\US_Watersheds_Automation\Summary_5yr_Stats_Flash_Flow.csv'

# txt file that contains sitenames
siteNames = r'H:\Streamflow\US_Watersheds_Automation\Watershed_list.txt'

# Open target CSV file to load in information

# loop through folder of CSVs

files = sorted(glob.glob(r'H:\Streamflow\US_Watersheds_Automation\Streamflow9222016_CSVs\*.csv'))
#########################################################################################

# Read in annual Stats CSV
v = open(AnnualCSV, 'rb')
info = pd.read_csv(v)

sites = open(siteNames, 'r')

with open(CSV_5yrStats, 'w') as out_v:
    write = csv.writer(out_v, lineterminator = '\n')
    write.writerow(["Full_SiteID", "Site_ID", "Drainage_Area_sqKm", "Flow92", "Flash92", "Flow01", "Flash01", "Flow06", "Flash06", "Flow11", "Flash11"])

    # loop through each ID
    for line in sites.readlines():
        test = line.split()
        siteIDs = test[0]
        Full_siteIDs = 'WS' + siteIDs
        #sqkm = info.Drainage_Area_sqKm[1]
        #if Full_siteIDs in info.Full_SiteID:
        try:
        #if info['Full_SiteID'].str.contains(Full_siteIDs)[0]: # THIS ONLY LOOKS IN FIRST COLUMN! 
            # Subset the data to the current WS
            infosub = info.loc[(info['Full_SiteID']== Full_siteIDs)]
            Years = (1992, 2001, 2006, 2011)
            x = []
            print('gothere')
            for i in Years:
                try:
                    subset = infosub.loc[(infosub['Year']==i) | (infosub['Year']==(i-1)) | (infosub['Year']==(i-2)) | (infosub['Year']==(i+1)) | (infosub['Year']==(i+2))]
                    sqkm = subset.Drainage_Area_sqKm[1]                    
                    if subset.shape[0] == 5:
                        numericsubset = subset.apply(pd.to_numeric, errors = 'coerce')                    
                        meanFlow = numericsubset['Annual_Total_Flow_CMS'].mean()
                        meanFlash = numericsubset['Annual_Flashiness'].mean()
                        # add it to a string that has values 
                        x.extend([meanFlow, meanFlash])
                        print(x)
                        continue
                    else:
                        x.extend(["",""])
                        print(x)
                        continue
                except:
                    x.extend(["",""])
                    print(x)
                    continue
            write.writerow([Full_siteIDs]+ [siteIDs]+ [sqkm] + x)
        except:
        #else:
            print('Site has NO DATA')
            # pass
            # continue
out_v.close()
print('Done')