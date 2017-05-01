# Michael Mangiante
 # 9/19/2016
# The purpose of this script is to grab suplimental information from USGS NWIS
# and put it into a seperate CSV file.

# suplimental information is gage ID, watershed size (sq mi), lat, long

import urllib2
import csv
import re

# list of Gage IDs in new line of a text file
siteNames = open(r'K:\Streamflow\Final_Streamflow_Download\WatershedList.txt', 'r')

with open(r'K:\Streamflow\Final_Streamflow_Download\Watershed_Suplimental.csv', 'w') as out_f: # CHANGE THE LOCATION OF THIS BASED ON WHERE YOU WANT CSV
    # add in header
    writer = csv.writer(out_f, lineterminator = '\n')
    writer.writerow(["Agency", "Site_ID", "Site_Name", "Site_Type", "Lat", "Long", "Lat_Long_Acc", "Datum", "Altitude", "Alt_Acc", "Alt_Datum", "HUC", "DrainageArea_sqmi", "Full_SiteID"])
    for line in siteNames.readlines():
        values = line.split()
        for i in xrange(len(values)):
            try:
                url = 'http://waterservices.usgs.gov/nwis/site/?format=rdb&sites={}'.format(values[i])
                url_size = 'http://waterdata.usgs.gov/nwis/inventory/?site_no={}&agency_cd=USGS'.format(values[i])
                print(url)
                print(url_size)
                # open both URLs
                sizeinfo = urllib2.urlopen(url_size)
                info = sizeinfo.read()
                sizelist = re.findall('<dd>Drainage area: .*</dd>', info)
                # select drainage area from HTML
                for element in sizelist:
                    parts = element.split(' ')
                    print(parts)[2:5]
                    sqmiles = parts[2]
                # open other url
                data = urllib2.urlopen(url) ######### change back to req from url
                # all other data from tab seperated
                in_txt = csv.reader(data, dialect = 'excel-tab')
                for row in in_txt:
                    #Remove any rows that contain a # Or a 5s
                    if not any("#" in s for s in row):
                        if not any("5s" in s for s in row):
                            if not any("agency_cd" in s for s in row):
                                ID_string = "WS" + str(row[1])
                                writer.writerow(row+ [sqmiles]+ [ID_string])
            except:
                continue

out_f.close()

print('DONE')