# Michael Mangiante
# 9/19/2016

# Designed to download streamflow data from USGS NWIS website
#import requests
import urllib2
import csv
#import os
import re

# list of Gage IDs in new line of a text file
siteNames = open('O:\PRIV\NERL_LEB\Other Projects\A-M\Historic Streamflow\QA Files for Historical Streamflow\Steamflow_Automation\WatershedList.txt', 'r')

for line in siteNames.readlines():
    values = line.split()
    for i in xrange(len(values)):
        # set up a counter
        key_field = 0
        url = 'http://nwis.waterdata.usgs.gov/nwis/dv?cb_00060=on&format=rdb&site_no={}&period=&begin_date=1969-10-01&end_date=2015-09-30'.format(values[i])
        url_size = 'http://waterdata.usgs.gov/nwis/inventory/?site_no={}&agency_cd=USGS'.format(values[i])
        sizeinfo = urllib2.urlopen(url_size)
        info = sizeinfo.read()
        sizelist = re.findall('<dd>Drainage area: .*</dd>', info)
        # select drainage area from HTML
        for element in sizelist:
            parts = element.split(' ')
            print(parts)[2:5]
            sqmiles = parts[2]
        print(url)
        data = urllib2.urlopen(url) ######### change back to req from url
         # dump results into a text file
        with open('O:\PRIV\NERL_LEB\Other Projects\A-M\Historic Streamflow\QA Files for Historical Streamflow\Steamflow_Automation\Streamflow362017_R3_CSVs\Watershed_{}.csv'.format(values), 'w') as out_f: # CHANGE THE LOCATION OF THIS BASED ON WHERE YOU WANT CSVs
            #dialect = csv.Sniffer().sniff(data.read(), delimiters = '  ')
            #in_txt = csv.reader(data, dialect)
            in_txt = csv.reader(data, dialect = 'excel-tab')
            for row in in_txt:
                #Remove any rows that contain a # Or a 5s
                if not any("#" in s for s in row):
                    if not any("5s" in s for s in row):
                        if any("agency_cd" in s for s in row):
                            # add column header for Water year
                            writer = csv.writer(out_f, lineterminator = '\n')
                            writer.writerow(["Agency", "Site_ID", "DateTime", "Flow_USGS", "Flow_Code","Water_Year","Drainage_Area_sqmi", "Flow_CMS", "Daily_Flashiness"])
                        else:
                            # write water year into each additional line
                            # if there is no data in the first row, pass
                            try:
                                date = row[2]
                                year = date[0:4]
                                month = date[5:7]
                                day = date[8:10]
                                if int(month) >= 10:
                                    wateryear = int(year) + 1
                                else:
                                    wateryear = int(year)
                                #Calculate flow in CMS
                                flow_CMS = float(row[3]) * 0.02832
                                # Calculate Flashiness
                                try:
                                    prevFlow_CMS = float(prevLine[3]) * 0.02832
                                    if key_field== 0:
                                        dailyFlash = ""
                                    else:
                                        dailyFlash = abs(flow_CMS - prevFlow_CMS)
                                except:
                                    dailyFlash = ""
                                # Print information to CSV file under the header.
                                writer = csv.writer(out_f, lineterminator = '\n')
                                writer.writerow(row + [wateryear]+[sqmiles]+[flow_CMS]+[dailyFlash])
                                key_field = key_field + 1
                            except:
                                #os.remove(str(out_f))
                                pass
                            # Save this line to be references for Flashiness in next loop.
                            prevLine = row
            # Close the script.
            out_f.close()

print('DONE')
# extracting file names: http://stackoverflow.com/questions/30390896/adding-extra-column-to-output-csv-file-in-python
# doing calculation on groups: http://stackoverflow.com/questions/5328971/python-csv-need-to-group-and-calculate-values-based-on-one-key