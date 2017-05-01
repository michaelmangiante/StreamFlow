# Michael Mangiante
# 8/25/2015
# This script Runs through street data and defines width
# Function Class 1 = 3.6576 m lanes, R shoulder = 3.6576 m, L shoulder = 3.51621 m
# Function Class 2 = 3.6576 m lanes, R shoulder = 3.048 m, L shoulder = 0.93342 m
# Function Class 3 = 3.6576 m lanes, R shoulder = 2.40679, L shoulder = 0,
# Function Class 4 = 3.6576 m lanes, R shoulder = 0.85367 m, L shoulder = 0,
# Function Class 5 = 3.58593 m lanes, R shoulder = 0, L shoulder = 0.

# 12 ft = 3.6576 m
# 10 ft = 3.048 m
# 4 ft = 1.2192 m

import arcpy, os

arcpy.CheckOutExtension("Spatial")

# Input Streets Layer
in_streets = r"H:\RoadCalculations\Roads.gdb\Streets_Albers2"

# Input Watersheds Layer
WS = r"H:\RoadCalculations\Roads.gdb\Watersheds_Combined"

FuncClass = "FUNC_CLASS"
toLanes = "TO_LANES"
fromLanes = "FROM_LANES"

arcpy.AddField_management(in_streets, "Rd_Width", "DOUBLE", 10,5, "","Rd_Width")

cursor = arcpy.UpdateCursor(in_streets)

for row in cursor:
    if row.getValue(FuncClass) == "1":
        countlanes = row.getValue(toLanes) + row.getValue(fromLanes)
        if countlanes == 0:
            goto = (3.6576 + 3.6576 + 3.51621)
            goFrom = (3.6576 + 3.6576 + 3.51621)
        else:
            tojustlanes = (row.getValue(toLanes) * 3.6576)
            if tojustlanes > 0:
                goto = tojustlanes + (3.6576 + 3.51621)
            else:
                goto = (0)
            fromjustlanes = (row.getValue(fromLanes) * 3.6576)
            if fromjustlanes > 0:
                goFrom = fromjustlanes + (3.6576 + 3.51621)
            else:
                goFrom = (0)

    if row.getValue(FuncClass) == "2":
        countlanes = row.getValue(toLanes) + row.getValue(fromLanes)
        if countlanes == 0:
            goto = (3.6576 + 3.048 + 0.93342)
            goFrom = (3.6576 + 3.048 + 0.93342)
        else:
            tojustlanes = (row.getValue(toLanes) * 3.6576)
            if tojustlanes > 0:
                goto = tojustlanes + (3.048 + 0.93342)
            else:
                goto = 0
            fromjustlanes = (row.getValue(fromLanes) * 3.6576)
            if fromjustlanes > 0:
                goFrom = fromjustlanes + (3.048 + 0.93342)
            else:
                goFrom = 0

    if row.getValue(FuncClass) == "3":
        countlanes = row.getValue(toLanes) + row.getValue(fromLanes)
        if countlanes == 0:
            goto = (3.6576 + 2.40679)
            goFrom = (3.6576 + 2.40679)
        else:
            tojustlanes = (row.getValue(toLanes) * 3.6576)
            if tojustlanes > 0:
                goto = tojustlanes + (2.40679)
            else:
                goto = 0
            fromjustlanes = (row.getValue(fromLanes) * 3.6576)
            if fromjustlanes > 0:
                goFrom = fromjustlanes + (2.40679)
            else:
                goFrom = 0

    if row.getValue(FuncClass) == "4":
        countlanes = row.getValue(toLanes) + row.getValue(fromLanes)
        if countlanes == 0:
            goto = (3.6576 + 0.85367)
            goFrom = (3.6576 + 0.85367)
        else:
            tojustlanes = (row.getValue(toLanes) * 3.6576)
            if tojustlanes > 0:
                goto = tojustlanes + (0.85367)
            else:
                goto = 0
            fromjustlanes = (row.getValue(fromLanes) * 3.6576)
            if fromjustlanes > 0:
                goFrom = fromjustlanes + (0.85367)
            else:
                goFrom = 0

    if row.getValue(FuncClass) == "5":
        countlanes = row.getValue(toLanes) + row.getValue(fromLanes)
        if countlanes == 0:
            goto = (3.58593)
            goFrom = (3.58593)
        else:
            tojustlanes = (row.getValue(toLanes) * 3.58593)
            if tojustlanes > 0:
                goto = tojustlanes
            else:
                goto = 0
            fromjustlanes = (row.getValue(fromLanes) * 3.58593)
            if fromjustlanes > 0:
                goFrom = fromjustlanes
            else:
                goFrom = 0

    lanewidth = goFrom + goto
    row.setValue("Rd_Width", lanewidth)
    cursor.updateRow(row)

## might need to insert thing about no functional class

# delete cursor and row objects to remove locks on data
del row
del cursor

print "Finished calculating width"

# clip roads to ws boundaries

clipped_roads = r"H:\RoadCalculations\Roads.gdb\Clipped_Roads2"
arcpy.Clip_analysis(in_streets, WS, clipped_roads)
print "END OF CLIP"

# Create new field for clipped_roads for road area

arcpy.AddField_management(clipped_roads, "Road_Area","DOUBLE", 10,5, "","Road_Area")

# Calculate field

expression = "!SHAPE.LENGTH! * !Rd_Width!"

arcpy.CalculateField_management(clipped_roads, "Road_Area", expression, "PYTHON")

print "END OF CALCULATE FIELD"
# Tabulate Intersection of WS and the clipped roads feature.
# Interested in sum of road area

outputtable = r"H:\RoadCalculations\Roads.gdb\RoadArea_byWS2"
arcpy.TabulateIntersection_analysis(WS, "GRIDCODE", clipped_roads, outputtable,"","Road_Area")

print "END OF SCRIpT"