# Michael Mangiante
# 9/20/2016

# Calculate Total Annual Flow and Mean R&B Flashiness

import csv
import glob
import pandas as pd
import numpy as np

# Target File Location for Annual Summary Statistics
AnnualCSV = r'O:\PRIV\NERL_LEB\Other Projects\A-M\Historic Streamflow\QA Files for Historical Streamflow\Steamflow_Automation\Annual_Summary_Flash_Flow_72added.csv'

# Target File location for 5 year average Summary Stats

CSV_5yrStats = r'O:\PRIV\NERL_LEB\Other Projects\A-M\Historic Streamflow\QA Files for Historical Streamflow\Steamflow_Automation\Summary_5yr_Stats_Flash_Flow_72added.csv'

# txt file that contains sitenames
siteNames = r'O:\PRIV\NERL_LEB\Other Projects\A-M\Historic Streamflow\QA Files for Historical Streamflow\Steamflow_Automation\WatershedList.txt'

# Open target CSV file to load in information

# loop through folder of CSVs

files = sorted(glob.glob(r'O:\PRIV\NERL_LEB\Other Projects\A-M\Historic Streamflow\QA Files for Historical Streamflow\Steamflow_Automation\WatershedsCSVs_R3\*.csv'))
#########################################################################################
# set up a loop through the files

# Open Target CSV file to load in information
with open(AnnualCSV, 'w') as out_f:
    writer = csv.writer(out_f, lineterminator = '\n')
    writer.writerow(["Full_SiteID", "Site_ID", "Year", "Drainage_Area_sqKm", "Annual_Total_Flow_CMS", "Annual_Flashiness"])
    for element in files:
        try:
            # Get File Name
            parts = element.split("'")
            Site_ID = parts[1]
            Full_SiteID = 'WS' + parts[1]
            print(Full_SiteID)

            # Read data in CSV
            f = open(element,'rb')
            #data = csv.reader(f, delimiter = ',')

            # Read CSV using Pandas
            data = pd.read_csv(f)

            # Make Sure there is data in the file
            #if sum(1 for row in data) <= 1:
            #    print('NO DATA')
            #    pass
            #else:
            #    print('there is data')
            #    continue
            # Check if data is in file using Pandas
            if data.shape[0] <= 1:
                print('No Data')
                continue
            else:
                print('there is data')
                #pass

                # Get Drainage Area in Km
            area_sqmi = data.Drainage_Area_sqmi[1]
            if type(area_sqmi) is np.float64 or type(area_sqmi) is np.int64:
                area_sqkm = float(area_sqmi) * 2.58999
            else:
                area_sqkm = float(area_sqmi.replace(',','')) * 2.58999

            # Loop to go through each year and print information
            years = (1970, 1971, 1972, 1973, 1974, 1982, 1983, 1984, 1985, 1986, 1990, 1991, 1992, 1993, 1994, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)
            days = (365, 365, 366, 365, 365, 365, 365, 366, 365, 365, 365, 365, 366, 365, 365, 365, 366, 365, 365, 365, 366, 365, 365, 365, 366, 365, 365, 365, 366, 365)

            for yrs, dys in zip(years, days):
                try:
                    if (data.Water_Year==yrs).sum() >= (dys * 0.75):
                        # Subset the data you want
                        sub = data.loc[data['Water_Year']==yrs]
                        # calculate Annual Total Flow
                        AnnualFlow = (sub.Flow_CMS.sum()) * dys
                        # Annual Total Flow
                        AnnualTotalFlow = AnnualFlow / area_sqkm
                        # Calculate Flashiness
                        AnnualFlash = (sub.Daily_Flashiness.sum())/ (sub.Flow_CMS.sum())
                        #Write information
                        writer.writerow([Full_SiteID]+[Site_ID]+[yrs]+[area_sqkm]+[AnnualTotalFlow]+[AnnualFlash])

                except:
                    AnnualTotalFlow = "NotEnoughData"
                    AnnualFlash = "NotEnoughData"
                    writer.writerow([Full_SiteID]+[Site_ID]+[yrs]+[area_sqkm]+[AnnualTotalFlow]+[AnnualFlash])
        except:
            continue
out_f.close()
print('Finished Writing Annual Stats')

# Read in annual Stats CSV
v = open(AnnualCSV, 'rb')
info = pd.read_csv(v)

sites = open(siteNames, 'r')

with open(CSV_5yrStats, 'w') as out_v:
    write = csv.writer(out_v, lineterminator = '\n')
    write.writerow(["Full_SiteID", "Site_ID", "Drainage_Area_sqKm","Flow72", "Flash72", "Flow84", "Flash84", "Flow92", "Flash92", "Flow01", "Flash01", "Flow06", "Flash06", "Flow11", "Flash11"])

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
            Years = (1972, 1984, 1992, 2001, 2006, 2011)
            x = []
            print('gothere')
            for i in Years:
                try:
                    subset = infosub.loc[(infosub['Year']==i) | (infosub['Year']==(i-1)) | (infosub['Year']==(i-2)) | (infosub['Year']==(i+1)) | (infosub['Year']==(i+2))]
                    sqkm = subset['Drainage_Area_sqKm'].iloc[0]
                    print(sqkm)
                    if subset.shape[0] == 5:
                        print('entered loop')
                        #numericsubset = subset.apply(pd.to_numeric, errors = 'coerce')
                        #print("numericsub is: " + str(numericsubset))
                        #meanFlow = numericsubset['Annual_Total_Flow_CMS'].mean()
                        meanFlow = subset['Annual_Total_Flow_CMS'].mean()
                        print(meanFlow)
                        #meanFlash = numericsubset['Annual_Flashiness'].mean()
                        meanFlash = subset['Annual_Flashiness'].mean()
                        print(meanFlash)
                        # add it to a string that has values
                        x.extend([meanFlow, meanFlash])
                        print(x)
                        print('first')
                        #continue
                    else:
                        x.extend(["",""])
                        print(x)
                        #continue
                        print('second')
                except:
                    x.extend(["",""])
                    print(x)
                    print('third')
                    #continue
            write.writerow([Full_siteIDs]+ [siteIDs]+ [sqkm] + x)
        except:
        #else:
            print('Site has NO DATA')
            # pass
            # continue
out_v.close()
v.close()
print('Done')