__author__ = 'Mathis Messager'
import arcpy
import os
import re
from collections import defaultdict
from datetime import date
from dateutil.relativedelta import relativedelta
import numpy
arcpy.env.overwriteOutput = True

############ TO UPDATE ################
datadir = 'F:\Chapter2_HexSim_Crayfish\data'
arcpy.env.workspace = 'F:\Chapter2_HexSim_Crayfish\src\Crayfish_model'

########################################################################################################################
####################     MAP AND COMPUTE LENGTH OF INVADED RIVER FOR EACH MODEL ITERATION      ################
########################################################################################################################

def field_maps(mxd, table_path, outpath, name_property, vernum, ini, step_interval, length, area, name, updist, reporting, mapping):
    #Reference to existing layer (layers to specify map document and dataframe, here only map document)
    #print arcpy.mapping.ListLayers(mxd)
    #Import table
    if vernum > 1:
        table_name = "\\" + name_property + "-[" + str(vernum) + "]"
        table = table_path + table_name + ".csv"
    else :
        table_name = "\\" + name_property
        table = table_path + table_name + ".csv"
    print(table)

    #Grab the first dataframe
    df = arcpy.mapping.ListDataFrames(mxd, "Layers")[0]

    #Grab the layer to which a join will be made
    layerName = arcpy.mapping.ListLayers(mxd)[0]
    # Join the feature layer to the table
    arcpy.AddJoin_management(in_layer_or_view=layerName, in_field="rid", join_table=table, join_field="Reach", join_type="KEEP_COMMON")

    # Copy the layer to a new permanent feature class

    join_out = outpath + name_property + str(vernum)
    #
    if arcpy.Exists(join_out + ".shp"):
        arcpy.Delete_management(join_out + ".shp")
        arcpy.Delete_management(join_out + ".lyr")

        arcpy.CopyFeatures_management(layerName, join_out + ".shp")
        arcpy.MakeFeatureLayer_management(join_out + ".shp", join_out)
        arcpy.SaveToLayerFile_management(join_out, join_out + ".lyr", "ABSOLUTE")
        #Remove non-join layer
        arcpy.mapping.RemoveLayer(df, layerName)
        #Add layer to df
        join_lyr_df = arcpy.mapping.Layer(join_out + ".lyr")
        arcpy.mapping.AddLayer(df, join_lyr_df)

    else:
        arcpy.CopyFeatures_management(layerName, join_out + ".shp")
        arcpy.MakeFeatureLayer_management(join_out + ".shp", join_out)

        arcpy.SaveToLayerFile_management(join_out, join_out + ".lyr", "ABSOLUTE")
        #Remove non-join layer
        arcpy.mapping.RemoveLayer(df, layerName)
        #Add layer to df
        join_lyr_df = arcpy.mapping.Layer(join_out + ".lyr")
        arcpy.mapping.AddLayer(df, join_lyr_df)

    #If you are using default line styles, then your layer.symbologyType should be equal to u'OTHER'.
    #There is no way through arcpy or arcpy.mapping to access the symbology properties for a layer with u'OTHER' symbology.
    ## You should get a result of "NameError: The attribute 'symbology' is not supported on this instance of Layer" when
    ## trying to access the symbology attribute for that layer.
    #Even if the symbology was one of the types supported by arcpy.mapping, none of the supported types currently supports
    #  reading the color values of individual symbols.

    symbo_lyr = arcpy.mapping.ListLayers(mxd)[1]
    #print(symbo_lyr.name)
    symbology0 = symbo_lyr.symbology

    #List all layout elements
    arcpy.mapping.ListLayoutElements(mxd)
    arcpy.mapping.UpdateLayer(df, join_lyr_df, symbo_lyr, True)

    layer = arcpy.mapping.ListLayers(mxd)[0]
    #print(layer)
    #Change layer symbology type to graduate colours
    arcpy.mapping.UpdateLayer(df, layer, symbo_lyr, True)
    layer.visible = True
    #print(arcpy.mapping.ListLayers(mxd)[2])
    arcpy.mapping.MoveLayer(df, layer, arcpy.mapping.ListLayers(mxd)[2], "BEFORE")
    #print(arcpy.mapping.ListLayers(mxd)[3])
    arcpy.mapping.MoveLayer(df, layer, arcpy.mapping.ListLayers(mxd)[3], "BEFORE")

    #Format legend
    legend = arcpy.mapping.ListLayoutElements(mxd, "LEGEND_ELEMENT")[0]
    #print("remove " + str(legend.listLegendItemLayers()[0]))
    legend.removeItem(legend.listLegendItemLayers()[0])
    #print("remove " + str(legend.listLegendItemLayers()[0]))
    legend.removeItem(legend.listLegendItemLayers()[0])
    legend.removeItem(legend.listLegendItemLayers()[0])

    date_txt = arcpy.mapping.ListLayoutElements(mxd,"TEXT_ELEMENT")[0]

    #Get layer fields that starts with Tmn
    desc = arcpy.Describe(layer)
    layersource = desc.catalogPath
    fields = arcpy.ListFields(layersource, name_property + "*")
    #print(len(fields))

    #Loop through each selected field
    for j in range(0,len(fields)):
        #Update field
        f = fields[j]
        #print(f.name)

        #Because we start by 0 to grab the first field and because density in a reach is sampled at the beginning of a step before movement, use -1 to get the right month
        ini_after = ini + relativedelta(months = (j-1)*step_interval)
        date_format = ini_after.strftime('%Y-%m-%d')
        #rint(date_format)
        date_txt.text = str(date_format)
        #print(date_format)

        if reporting == True:
            if date_format == "2005-07-01" and len(fields) > 20:
                totalarea_2005[vernum] = 0
                totallength_2005[vernum] = 0
                print(str(date_format))
                with arcpy.da.SearchCursor(layer, [f.name, length, area]) as searchcur:
                    #Iterate over every row and assign reachid to key and pres_abs to value
                    for row in searchcur:
                        if row[0] > 0:
                            totallength_2005[vernum] += row[1]
                            totalarea_2005[vernum] += row[2]
                del searchcur
                del row
            elif date_format == "2010-07-01" and len(fields) > 20:
                totalarea_2010[vernum] = 0
                totallength_2010[vernum] = 0
                print(str(date_format))
                with arcpy.da.SearchCursor(layer, [f.name, length, area, tribname, updist]) as searchcur:
                    #Iterate over every row and assign reachid to key and pres_abs to value
                    for row in searchcur:
                        if row[0] > 0:
                            totallength_2010[vernum] += row[1]
                            totalarea_2010[vernum] += row[2]
                            if row[3] == "John Day River" and downstreamedge_2010[vernum] > row[4]:
                                downstreamedge_2010[vernum] = row[4]

            elif date_format == "2016-08-01" and len(fields) > 20:
                totalarea_2016[vernum] = 0
                totallength_2016[vernum] = 0
                print(str(date_format))
                with arcpy.da.SearchCursor(layer, [f.name, length, area]) as searchcur:
                    #Iterate over every row and assign reachid to key and pres_abs to value
                    for row in searchcur:
                        if row[0] > 0:
                            totallength_2016[vernum] += row[1]
                            totalarea_2016[vernum] += row[2]
                del searchcur
                del row
            elif date_format == "2025-05-01" and len(fields) > 20:
                totalarea_2025[vernum] = 0
                totallength_2025[vernum] = 0
                print(str(date_format))
                with arcpy.da.SearchCursor(layer, [f.name, length, area, tribname, updist]) as searchcur:
                    #Iterate over every row and assign reachid to key and pres_abs to value
                    for row in searchcur:
                        if row[0] > 0:
                            totallength_2025[vernum] += row[1]
                            totalarea_2025[vernum] += row[2]
                            if row[3] == "John Day River" and downstreamedge_2025[vernum] > row[4]:
                                downstreamedge_2010[vernum] = row[4]
                del searchcur
                del row

        if mapping == True:
            layerSymbolClass = layer.symbology
            layerSymbolClass.valueField = str(f.name)
            #print layerSymbolClass.valueField

            #Apply symbology
            arcpy.ApplySymbologyFromLayer_management(layer, symbo_lyr)
            #layerSymbolClass.classBreakValues = [0,0,10,50,100,250,500,2000]

            #print(arcpy.mapping.ListLayers(mxd))

            #Update figure
            #pict = arcpy.mapping.ListLayoutElements(mxd, "PICTURE_ELEMENT")[0]
            #print("F:\\Chapter2_HexSim_Crayfish\\Temp_flow_animation\\Plots_2\\" + str(f.name) + ".emf")
            #pict.sourceImage = "F:\\Chapter2_HexSim_Crayfish\\Temp_flow_animation\\Plots_2\\" + str(f.name) + ".emf"

            #Export to jpeg
            #print(outpath + "test_" + str(testnum) + "rep_" + str(vernum) + "step_" + str(j*step_interval) + ".jpg")
            arcpy.mapping.ExportToJPEG(mxd, outpath + "test_" + str(testnum) + "rep_" + str(vernum) + "step_" + str(j*step_interval) + ".jpg", resolution = 150)

            #fields2 = arcpy.ListFields(layersource, "*")
            #for f in fields2:
            #    print(f.name)

    #arcpy.mapping.RemoveLayer(df, layer)
    #print(layerName)
    ##Add layer to df
    #edge_lyr = arcpy.mapping.Layer("F:\Hexsim\Crayfish_model\Hexsim_ready_data\Network_module\edges_2.shp")
    #arcpy.mapping.AddLayer(df, edge_lyr)
    #arcpy.mapping.ListLayers(mxd)[0]

#Test #
netnum = 23
testnum = "16_2025"
#Date of model start
date_ini = date(1999, 6, 1)
#Time step intervals
inter = 1

#Property name to map
name_property = "Dens"
#Map path
map_path  = "Parameters_and_outputs\\"
#Import mxd
mxd = arcpy.mapping.MapDocument(map_path + "Mapping_basemap_18_4.mxd")
#Output path
outpath = "Parameters_and_outputs\Network_" + str(netnum) + "\Network" + str(netnum) + "_test" + str(testnum) + "\\"

#Network length and area
#If error: "RuntimeError: A column was specified that does not exist." comes up. Go into function and un-hash the list field
#function at the end of the function to check out the field names of the joined layer
length = "Shape_Leng"
area = "reach_area"
tribname = "GNIS_NAME"
updist = "updist"

totalarea_2005= {}
totallength_2005 = {}
totalarea_2010 = {}
totallength_2010 = {}
totalarea_2016 = {}
totallength_2016 = {}
totalarea_2025 = {}
totallength_2025 = {}
downstreamedge_2010 = {}
downstreamedge_2025 = {}

arcpy.env.qualifiedFieldNames = False
countver = len([name for name in os.listdir("Network_" + str(netnum) + "\Results\Network" + str(netnum) + "_test" + str(testnum))])
for ver in range(1,countver+1) :
    mxd = arcpy.mapping.MapDocument(map_path + "Mapping_basemap_18_4.mxd")
    vernum = ver
    downstreamedge_2010[vernum] = 1000000
    downstreamedge_2025[vernum] = 1000000
    #Table workspace
    table_path = "Network_" + str(netnum) + "\Results\Network" + str(netnum) + "_test" + str(testnum)\
                 + "\Network" + str(str(netnum))+ "_test" + str(testnum) + "-[" + str(vernum) + "]\Generated Properties"
    field_maps(mxd, table_path, outpath, name_property, vernum, date_ini, inter, length, area, name, updist, reporting = False, mapping = True)

print("Mean invaded river length 2005: " + str(numpy.mean(totallength_2005.values())))
print("SD invaded river length 2005: " + str(numpy.std(totallength_2005.values())))
print("Mean invaded river length 2010: " + str(numpy.mean(totallength_2010.values())))
print("SD invaded river length 2010: " + str(numpy.std(totallength_2010.values())))
print("Mean invaded river length 2016: " + str(numpy.mean(totallength_2016.values())))
print("SD invaded river length 2016: " + str(numpy.std(totallength_2016.values())))
print("Mean invaded river length 2025: " + str(numpy.mean(totallength_2025.values())))
print("SD invaded river length 2025: " + str(numpy.std(totallength_2025.values())))

print("Mean downstream leading edge location in 2010: " + str(numpy.mean(downstreamedge_2010.values())))
print("Mean downstream leading edge location in 2025: " + str(numpy.mean(downstreamedge_2025.values())))

#########################################################################################################################

def field_maps_single(mxd, table_path, outpath, name_property, vernum, ini, step_interval, ts):
    #Reference to existing layer (layers to specify map document and dataframe, here only map document)
    print arcpy.mapping.ListLayers(mxd)
    #Import table
    if vernum > 1:
        table_name = "\\" + name_property + "-[" + str(vernum) + "]"
        table = table_path + table_name + ".csv"
    else :
        table_name = "\\" + name_property
        table = table_path + table_name + ".csv"
    print(table)

    #Grab the first dataframe
    df = arcpy.mapping.ListDataFrames(mxd, "Layers")[0]

    #Grab the layer to which a join will be made
    layerName = arcpy.mapping.ListLayers(mxd)[0]
    # Join the feature layer to the table
    arcpy.AddJoin_management(in_layer_or_view=layerName, in_field="rid", join_table=table, join_field="Reach", join_type="KEEP_COMMON")

    # Copy the layer to a new permanent feature class

    join_out = outpath + name_property + str(vernum)
    #
    if arcpy.Exists(join_out + ".shp"):
        arcpy.Delete_management(join_out + ".shp")
        arcpy.Delete_management(join_out + ".lyr")

        arcpy.CopyFeatures_management(layerName, join_out + ".shp")
        arcpy.MakeFeatureLayer_management(join_out + ".shp", join_out)
        arcpy.SaveToLayerFile_management(join_out, join_out + ".lyr", "ABSOLUTE")
        #Remove non-join layer
        arcpy.mapping.RemoveLayer(df, layerName)
        #Add layer to df
        join_lyr_df = arcpy.mapping.Layer(join_out + ".lyr")
        arcpy.mapping.AddLayer(df, join_lyr_df)

    else:
        arcpy.CopyFeatures_management(layerName, join_out + ".shp")
        arcpy.MakeFeatureLayer_management(join_out + ".shp", join_out)

        arcpy.SaveToLayerFile_management(join_out, join_out + ".lyr", "ABSOLUTE")
        #Remove non-join layer
        arcpy.mapping.RemoveLayer(df, layerName)
        #Add layer to df
        join_lyr_df = arcpy.mapping.Layer(join_out + ".lyr")
        arcpy.mapping.AddLayer(df, join_lyr_df)

    #If you are using default line styles, then your layer.symbologyType should be equal to u'OTHER'.
    #There is no way through arcpy or arcpy.mapping to access the symbology properties for a layer with u'OTHER' symbology.
    ## You should get a result of "NameError: The attribute 'symbology' is not supported on this instance of Layer" when
    ## trying to access the symbology attribute for that layer.
    #Even if the symbology was one of the types supported by arcpy.mapping, none of the supported types currently supports
    #  reading the color values of individual symbols.

    symbo_lyr = arcpy.mapping.ListLayers(mxd)[1]
    symbo_lyr.name
    symbology0 = symbo_lyr.symbology

    #List all layout elements
    arcpy.mapping.ListLayoutElements(mxd)
    arcpy.mapping.UpdateLayer(df, join_lyr_df, symbo_lyr, True)

    layer = arcpy.mapping.ListLayers(mxd)[0]
    layer.visible = True
    #Change layer symbology type to graduate colours
    arcpy.mapping.UpdateLayer(df, layer, symbo_lyr, True)
    print(arcpy.mapping.ListLayers(mxd)[2])
    arcpy.mapping.MoveLayer(df, layer, arcpy.mapping.ListLayers(mxd)[2], "BEFORE")

    #Format legend
    legend = arcpy.mapping.ListLayoutElements(mxd, "LEGEND_ELEMENT")[0]
    #print("remove " + str(legend.listLegendItemLayers()[0]))
    legend.removeItem(legend.listLegendItemLayers()[0])
    #print("remove " + str(legend.listLegendItemLayers()[0]))
    legend.removeItem(legend.listLegendItemLayers()[0])

    date_txt = arcpy.mapping.ListLayoutElements(mxd,"TEXT_ELEMENT")[0]

    #Get layer fields that starts with Tmn
    desc = arcpy.Describe(layer)
    layersource = desc.catalogPath

    #Import table
    fields = arcpy.ListFields(layersource, name_property + "*")
    print(len(fields))

    #Loop through each selected field
    #Due to joining, the rid field is called "range_rid" so does not start range() by 0 but 1 to start by the second
    #field that starts by range
    #Update field
    f = fields[ts]
    #print(f.name)

    ini_after = ini + relativedelta(months = (ts-1)*step_interval)
    date_format = ini_after.strftime('%Y-%m-%d')
    #rint(date_format)
    date_txt.text = str(date_format)

    layerSymbolClass = layer.symbology
    layerSymbolClass.valueField = str(f.name)
    #print layerSymbolClass.valueField

    #Apply symbology
    arcpy.ApplySymbologyFromLayer_management(layer, symbo_lyr)

    #Update figure
    #pict = arcpy.mapping.ListLayoutElements(mxd, "PICTURE_ELEMENT")[0]
    #print("F:\\Chapter2_HexSim_Crayfish\\Temp_flow_animation\\Plots_2\\" + str(f.name) + ".emf")
    #pict.sourceImage = "F:\\Chapter2_HexSim_Crayfish\\Temp_flow_animation\\Plots_2\\" + str(f.name) + ".emf"

    #Export to jpeg
    #print(outpath + "test_" + str(testnum) + "rep_" + str(vernum) + "step_" + str(j*step_interval) + ".jpg")
    arcpy.mapping.ExportToJPEG(mxd, outpath + "test_" + str(testnum) + "rep_" + str(vernum) + "step_" + str(ts*step_interval) + ".jpg", resolution = 150)

#Table workspace
#vernum = 10
#ts = 116
#table_path = "F:\Hexsim\Crayfish_model\Network_" + str(netnum) + "\Results\Network" + str(netnum) + "_test" + str(testnum)\
#             + "\Network" + str(str(netnum))+ "_test" + str(testnum) + "-[" + str(vernum) + "]\Generated Properties"
#field_maps(mxd, table_path, outpath, name_property, vernum, date_ini, inter, ts)


########################################################################################################################
####################              PRESENCE/ABSENCE MATCHING                      #######################################
########################################################################################################################

#This script intends to quantify the degree to which the model output matches observed patterns of crayfish spread
#at each time step

#Points with presence/absence records
points_2005 = os.path.join(datadir, r"Crayfish_occurence\Adams\2005_sites.shp")
points_2010 = os.path.join(datadir, r"Crayfish_occurence\Sorenson_Olden\Sorenson_Olden_sampling_presabs_test.shp")
#Needed to edit the location of the downstream-most site manually in Arcmap because it was located on a tributary
points_2016 = os.path.join(datadir, r"Field_work_Data\Sampled_sites_notes.shp")

#Stream network
#network = "F:\Hexsim\Crayfish_model\Hexsim_ready_data\Network_module/Network_18/edges_18.shp"
network_updist = "Hexsim_ready_data\Network_module/Network_18/edges_18_updist.shp"
#Output joined feature class
netnum = 20
outfc2005 = "Parameters_and_outputs\Network_" + str(netnum) + "\\2005points_network_join.shp"
outfc2010 = "Parameters_and_outputs\Network_" + str(netnum) + "\\2010points_network_join.shp"
outfc2016 = "Parameters_and_outputs\Network_" + str(netnum) + "\\2016points_network_join.shp"

#Set time step to match
ts_2025 = 312
ts_2016 = 207
ts_2010 = 135
ts_2005 = 75
ts_1999 = 1

arcpy.env.qualifiedFieldNames = False

def pres_abs_match(year, ts,network, points, outfc, id_field, name_property, netnum, testnum):
    points_snap =  "Parameters_and_outputs\Network_" + str(netnum) + "\\presabs" + str(year) + "_snap.shp"
    arcpy.CopyFeatures_management(points, points_snap)
    snap_env = [network, "EDGE", "100 meters"]
    arcpy.Snap_edit(points_snap, [snap_env])

    #Join rusty crayfish occurrence points to network
    operation = "JOIN_ONE_TO_ONE"
    type = "KEEP_COMMON"
    match = "CLOSEST"
    radius = "1 meters"
    distfield = "dist"
    arcpy.SpatialJoin_analysis(target_features = network, join_features = points_snap, out_feature_class = outfc,
                               join_operation = operation, join_type = type, match_option = match,
                               search_radius = radius, distance_field_name = distfield)

    arcpy.DeleteIdentical_management (outfc, id_field) #Make sure to format id-field well for function

    ### Create a dictionary of reach ids and whether crayfish were present or absent: ref
    #Get the known downstream-most invasion distance + upstream extent in tributaries in the year of interest: presdist

    #Field uniquely identifying reaches with crayfish presence/absence data
    reachid = "rid"
    #Create dictionary placeholder
    ref = {}
    presdist = {}
    absdist = {}
    with arcpy.da.SearchCursor(outfc, ["rid","Pres_Abs", "GNIS_NAME", "updist"]) as cursor:
        for row in cursor:
            ref[row[0]] = row[1]
            if row[1] == 1:
                presdist[row[0]] = [row[2],row[3]]
            if row[1] == 0:
                absdist[row[0]] = [row[2],row[3]]
    del cursor

    up_main = {}
    down_main = {}
    up_sf = {}
    up_nf = {}
    up_rc = {}

    maxpres = {}
    maxabs = {}
    keyspres = {}
    keysabs = {}
    maxmatch = {}
    keys = {}
    presmatch = {}
    absmatch = {}
    totalmatch = {}

    countver = len([name for name in os.listdir("Network_" + str(netnum) + "\Results\Network" + str(netnum) + "_test" + str(testnum))])
    for ver in range(1,countver+1):
        vernum = ver
        #Table workspace
        table_path = "Network_" + str(netnum) + "\Results\Network" + str(netnum) + "_test" + str(testnum)\
                     + "\Network" + str(str(netnum))+ "_test" + str(testnum) + "-[" + str(vernum) + "]\Generated Properties"

        #Density spatial distribution model output
        outtable = "Parameters_and_outputs\Network_" + str(netnum) + "\Network" + str(netnum) + "_test" + \
                   str(testnum) + "\\rep" + str(vernum) + "_" + str(year) + "presabs_match.dbf"

        #Import table
        if vernum > 1:
            table_name = "\\" + name_property + "-[" + str(vernum) + "]"
            table = table_path + table_name + ".csv"
        else :
            table_name = "\\" + name_property
            table = table_path + table_name + ".csv"
        #print(table)

        if arcpy.Exists(outtable):
            #print("delete table")
            arcpy.Delete_management(outtable)
        arcpy.CopyRows_management(table, outtable)

        ###############################################################################################################
        # Compute distance from upstream and downstream presences and absences

        #Run through every reach, match with network18_updist rids and put it in a dictionary
        arcpy.MakeFeatureLayer_management(network, 'network_lyr')
        arcpy.AddJoin_management('network_lyr', 'rid', outtable, 'Reach')
        #Ideally would just have the distance in original network used in HexSim
        join_fields = [j.name for j in arcpy.ListFields('network_lyr')]
        f_dist = [j.name for j in arcpy.ListFields('network_lyr', '*NAME')] + [j.name for j in arcpy.ListFields('network_lyr', '*_'+str(ts))] + [j.name for j in arcpy.ListFields('network_lyr', '*updist.updist')]

        presdist_mod = defaultdict(list)


        with arcpy.da.SearchCursor('network_lyr', f_dist) as cursor:
            for row in cursor:
                if row[1] > 0 and row[0] != " ":
                   presdist_mod[row[0]].append(row[2])
        del cursor

        up_main[vernum] = max(presdist_mod['John Day River'])
        down_main[vernum] = min(presdist_mod['John Day River'])
        up_sf[vernum] = max(presdist_mod['South Fork John Day River'] or [0])
        up_nf[vernum] = max(presdist_mod['North Fork John Day River']or [0])
        up_rc[vernum] = max(presdist_mod['Rock Creek']or [0])

        ###############################################################################################################
        # Compute matched absences and presences

        #Insert cursor in table
        with arcpy.da.UpdateCursor(outtable, '*') as cursor:
            #Iterate cursor over each row
            for row in cursor:
                if row[1] in dict.keys(ref):
                    for f in range(2,len(row)):
                        if (row[f] > 0 and ref[row[1]] == 1):
                            row[f] = 1
                        elif (row[f] > 0 and ref[row[1]] == 0):
                            row[f] = 0
                        elif (row[f] == 0 and ref[row[1]] == 0):
                            row[f] = -1
                        else :
                            row[f] = 0
                    cursor.updateRow(row)
                else:
                    cursor.deleteRow()
            del cursor
            del row

        #Sum the score (0 and 1) for each time step
        table_fields = [j.name for j in arcpy.ListFields(outtable)]
        sum_pres = {}
        sum_abs = {}
        sumdic = {}
        for f in table_fields[2:len(table_fields)]:
            #int(f[5:]) is to grab from the name of the field which time step is being selected. Adjust based on field names
            #If property is called Dens, use 5:, if property is called e.g. Dens2, use 6: as the field name will be Dens2_2, Dens2_3, etc.
            sum_pres[int(f[5:])] = 0
            sum_abs[int(f[5:])] = 0
            sumdic[int(f[5:])] = 0
            for r in arcpy.da.SearchCursor(outtable, f):
                if r[0] == 1:
                    sum_pres[int(f[5:])] += 1
                    sumdic[int(f[5:])] += 1
                elif r[0] == -1:
                    sum_abs[int(f[5:])] += 1
                    sumdic[int(f[5:])] += 1

        maxpres[vernum] = max(sum_pres.values())
        maxabs[vernum] = max(sum_abs.values())
        maxmatch[vernum] = max(sumdic.values())
        keyspres[vernum] = sorted([k for k,v in dict.items(sum_pres) if v == maxpres[vernum]])
        keysabs[vernum] = sorted([k for k,v in dict.items(sum_abs) if v == maxabs[vernum]])
        keys[vernum] = sorted([k for k,v in dict.items(sumdic) if v == maxmatch[vernum]])

        try:
            presmatch[vernum] = sum_pres[ts]
            absmatch[vernum] = sum_abs[ts]
            totalmatch[vernum] = sumdic[ts]
        except:
            print("Population in replicate #" + str(vernum) + "crashed at time step #" + str(max(sumdic.keys())))
    #################################################################################################################
    #Print messages for matched presences and absences

    print(str(year) + " matched presence # mean: " + str(numpy.mean(presmatch.values())))
    print(str(year) + " matched presence # std: " + str(numpy.std(presmatch.values())))
    print(str(year) + " matched presence # max: " + str(max(presmatch.values())))
    print(str(year) + " matched absence # mean: " + str(numpy.mean(absmatch.values())))
    print(str(year) + " matched absence # std: " + str(numpy.std(absmatch.values())))
    print(str(year) + " matched absence # max: " + str(max(absmatch.values())))
    print(str(year) + " total match mean: " + str(numpy.mean(totalmatch.values())))

    #################################################################################################################
    #Print messages for distance from upstream and downstream edges
    #Positive values mean that the model has spread beyond (upstream or downstream) observed position

    print(str(year) + " difference between predicted and observed edge in upstream main stem JDR: " +
          str(numpy.mean(up_main.values()) - max([v[1] for k, v in presdist.iteritems() if 'John Day River' in v])))
    print(str(year) + " upstream location of leading edge in upstream main stem JDR: " + str(numpy.mean(up_main.values())))
    #Grab the first absence past the last upstream presence

    up_main_obs = min([x for x in [v[1] for k, v in absdist.iteritems() if 'John Day River' in v] if x > max([v[1] for k, v in presdist.iteritems() if 'John Day River' in v])])
    try:
        print(str(year) + " difference between predicted edge and observed absence in upstream main stem JDR: " + str(numpy.mean(up_main.values()) - up_main_obs))
    except:
        print("No absence recorded in the downstream mainstem")

    print(str(year) + " difference between predicted and observed edge in downstream main stem JDR: " + str(min([v[1] for k, v in presdist.iteritems() if 'John Day River' in v]) - numpy.mean(down_main.values())))
    print(str(year) + " downstream location of leading edge in main stem JDR: " + str(numpy.mean(down_main.values())))
    #Make sure that the absence is actually past the downstream edge
    if min([v[1] for k, v in absdist.iteritems() if 'John Day River' in v]) < min([v[1] for k, v in presdist.iteritems() if 'John Day River' in v]):
        try:
            print(str(year) + " difference between predicted edge and observed absence in downstream main stem JDR: " + str(min([v[1] for k, v in absdist.iteritems() if 'John Day River' in v]) - numpy.mean(down_main.values())))
        except:
            print("No absence recorded in the downstream mainstem")

    print(str(year) + " difference between predicted and observed edge in upstream south fork JDR: " + str(numpy.mean(up_sf.values()) - max([v[1] for k, v in presdist.iteritems() if 'South Fork John Day River' in v])))
    print(str(year) + " downstream location of leading edge in upstream south fork JDR: " + str(numpy.mean(up_sf.values())))
    print(str(year) + " difference between predicted edge and observed absence in upstream south fork JDR: " + str(numpy.mean(up_sf.values()) - min([v[1] for k, v in absdist.iteritems() if 'South Fork John Day River' in v])))

    try :
        print(str(year) + " difference between predicted and observed edge in upstream north fork JDR: " + str(numpy.mean(up_nf.values()) - max([v[1] for k, v in presdist.iteritems() if 'North Fork John Day River' in v])))
        print(str(year) + " downstream location of leading edge in upstream north fork JDR: " + str(numpy.mean(up_nf.values())))
    except :
        print("No crayfish observed in the North Fork in " + str(year) + " but crayfish reached" + str(numpy.mean(up_nf.values())))
    try :
        print(str(year) + " difference between predicted edge and observed absence in upstream north fork JDR: " + str(numpy.mean(up_nf.values()) - min([v[1] for k, v in absdist.iteritems() if 'North Fork John Day River' in v])))
    except:
        print("No absence recorded in the upstream north fork")

    try :
        print(str(year) + " difference between predicted and observed edge in Rock Creek: " + str(numpy.mean(up_rc.values()) -max([v[1] for k, v in presdist.iteritems() if 'Rock Creek' in v])))
    except:
        print("No crayfish observed in Rock Creek in " + str(year) + " but crayfish reached" + str(numpy.mean(up_rc.values())))
    try :
        print(str(year) + " difference between predicted edge and observed absence in Rock Creek: " + str(numpy.mean(up_rc.values()) - min([v[1] for k, v in absdist.iteritems() if 'Rock Creek' in v])))
    except:
        print("No absence recorded in Rock Creek")


for y in range(1999, 2026):
    print(y)
    ts = 2 + 12*(y-1999)
    pres_abs_match(y, ts, network_updist, points_2005, outfc2005,"Name", "Dens", 23, "16_2025")




pres_abs_match(2005, ts_2005, network_updist, points_2005, outfc2005,"Name", "Dens", 23, "16_2025_control2009_50perc_2")
pres_abs_match(2010, ts_2010, network_updist, points_2010, outfc2010, "Site_no","Dens", 23, "16_2025_control2009_50perc_2")
pres_abs_match(2016, ts_2016, network_updist, points_2016, outfc2016, "Site_ID","Dens", 23, "16_2025_control2009_50perc_2")
pres_abs_match(2025, ts_2025, network_updist, points_2016, outfc2016, "Site_ID","Dens", 23, "16_2025")
pres_abs_match(1999, ts_1999, network_updist, points_2016, outfc2016, "Site_ID","Dens", 23, "16_2025")


#year = 2016
#ts = ts_2016
#points = points_2016
#outfc = outfc2016
#id_field = "Site_ID"
#name_property = "Dens"
#netnum = 20
#testnum = 14


########################################################################################################################
############################ MAP OF NUMBER OF REPLICATES SHOWING CRAYFISH PRESENCE IN AN EDGE ##########################

network_updist = "Hexsim_ready_data\Network_module/Network_18/edges_18_updist.shp"
#network_updist = "F:\Hexsim\Crayfish_model\Hexsim_ready_data\Network_module/Network_16/edges_2.shp"
name_property = "Dens"
#Map path
map_path  = "Parameters_and_outputs\\"

#basemap = "Mapping_basemap_17_future_3.mxd"

ts_2025 = 312 #(May 2025) Why not June?
ts_2016 = 207 #07/18/2017 changed from 204 (April 2016)
ts_2010 = 135
ts_2005 = 75

netnum = 23
testnum = "16_2025"
format = "png"

ini_table = "Network_" + str(netnum) + "\Results\Network" + str(netnum) + "_test" + str(testnum)\
             + "\Network" + str(str(netnum))+ "_test" + str(testnum) + "-[" + str(1) + "]\Generated Properties"\
    + "\\" + name_property +".csv"

basemap = "Mapping_basemap_20_replicateoutput_2.mxd"
def replicated_mod_distrib(netnum, testnum, name_property, ts_2005, ts_2010, ts_2016, map_path, basemap, format):
    pres_2005 = {}
    pres_2010 = {}
    pres_2016 = {}
    countver = len([name for name in os.listdir("Network_" + str(netnum) + "\Results\Network" + str(netnum) + "_test" + str(testnum))])
    #Output path
    outpath = "Parameters_and_outputs\Network_" + str(netnum) + "\Network" + str(netnum) + "_test" + str(testnum) + "\\"

    with arcpy.da.SearchCursor(ini_table, ['*']) as cursor:
        for row in cursor:
            pres_2005[row[0]] = 0
            pres_2010[row[0]] = 0
            pres_2016[row[0]] = 0

    for ver in range(1,countver+1) :
        vernum = ver
        #Table workspace
        table_path = "Network_" + str(netnum) + "\Results\Network" + str(netnum) + "_test" + str(testnum)\
                     + "\Network" + str(str(netnum))+ "_test" + str(testnum) + "-[" + str(vernum) + "]\Generated Properties"

        #Import table
        if vernum > 1:
            table_name = "\\" + name_property + "-[" + str(vernum) + "]"
            table = table_path + table_name + ".csv"
        else :
            table_name = "\\" + name_property
            table = table_path + table_name + ".csv"
        print(table)

        #Add 1 to a reach in dictionaries for each replicate for which crayfish are in a reach
        with arcpy.da.SearchCursor(table, ['*']) as cursor:
            for row in cursor:
                if row[ts_2005] > 0:
                    pres_2005[row[0]] += 1
                if row[ts_2010] > 0:
                    pres_2010[row[0]] += 1
                if row[ts_2016] > 0:
                    pres_2016[row[0]] += 1

    #Create copy of network and add model outputs in 2005,2010, and 2016 to three new fields
    repli_out = outpath + "replicated_output" + "test" + str(testnum)
    arcpy.CopyFeatures_management(network_updist, repli_out + ".shp")

    arcpy.AddField_management(repli_out+".shp", "modpres05", "INTEGER", field_is_nullable="NULLABLE")
    arcpy.AddField_management(repli_out+".shp", "modpres10", "INTEGER", field_is_nullable="NULLABLE")
    arcpy.AddField_management(repli_out+".shp", "modpres16", "INTEGER", field_is_nullable="NULLABLE")

    [f.name for f in arcpy.ListFields(repli_out + ".shp")]

    with arcpy.da.UpdateCursor(repli_out + ".shp", ["rid","modpres05", "modpres10","modpres16"]) as cursor:
        for row in cursor:
            row[1] = pres_2005[row[0]]
            row[2] = pres_2010[row[0]]
            row[3] = pres_2016[row[0]]
            cursor.updateRow(row)

    #Import mxd
    mxd = arcpy.mapping.MapDocument(map_path + basemap)

    #Grab the first dataframe
    df = arcpy.mapping.ListDataFrames(mxd, "Layers")[0]
    if arcpy.Exists(repli_out + ".lyr"):
        arcpy.Delete_management(repli_out + ".lyr")
        arcpy.MakeFeatureLayer_management(repli_out + ".shp", "modpresabs")
        arcpy.SaveToLayerFile_management("modpresabs", repli_out + ".lyr", "ABSOLUTE")
        #Add layer to df
        join_lyr_df = arcpy.mapping.Layer(repli_out + ".lyr")
        arcpy.mapping.AddLayer(df, join_lyr_df)
    else:
        arcpy.MakeFeatureLayer_management(repli_out + ".shp", "modpresabs")
        arcpy.SaveToLayerFile_management("modpresabs", repli_out + ".lyr", "ABSOLUTE")
        #Add layer to df
        join_lyr_df = arcpy.mapping.Layer(repli_out + ".lyr")
        arcpy.mapping.AddLayer(df, join_lyr_df)


    symbo_lyr = arcpy.mapping.ListLayers(mxd)[1]
    print(symbo_lyr.name)
    symbology0 = symbo_lyr.symbology
    layer = arcpy.mapping.ListLayers(mxd)[0]

    #List all layout elements
    arcpy.mapping.ListLayoutElements(mxd)
    arcpy.mapping.UpdateLayer(df, layer, symbo_lyr, True)

    #Change layer symbology type to graduate colours
    layer.visible = True
    introp = [l for l in arcpy.mapping.ListLayers(mxd) if re.search('Intro', l._fullName)][0]
    arcpy.mapping.MoveLayer(df, layer, introp, "BEFORE")

    date_txt = arcpy.mapping.ListLayoutElements(mxd,"TEXT_ELEMENT")[0]

    for y in [2005, 2010, 2016]:
        presabs_ref = [l for l in arcpy.mapping.ListLayers(mxd) if re.search(str(y)[2:], l._fullName)][0]
        arcpy.mapping.MoveLayer(df, layer, presabs_ref, "BEFORE")

        #Get layer fields that starts with Tmn
        desc = arcpy.Describe(layer)
        layersource = desc.catalogPath
        fields = arcpy.ListFields(layersource, "*" + str(y)[2:] + "*")[0]

        #Set date text
        date_txt.text = str(y)

        layerSymbolClass = layer.symbology

        #print layerSymbolClass.valueField

        #Apply symbology
        arcpy.ApplySymbologyFromLayer_management(layer, symbo_lyr)
        layerSymbolClass.valueField = str(fields.name)
        layer.visible = True
        arcpy.mapping.ListLayers(mxd)[1].visible = True
        arcpy.RefreshTOC()
        arcpy.RefreshActiveView()
        symbo_lyr.visible = False
        #Export to jpeg
        #print(outpath + "test_" + str(testnum) + "_mod_distrib" + str(year) + ".jpg")
        if format == "png":
            arcpy.mapping.ExportToJPEG(mxd, outpath + "test_" + str(testnum) + "_mod_distrib" + str(y) + ".jpg", resolution = 300)
        if format == "pdf":
            arcpy.mapping.ExportToPDF(mxd, outpath + "test_" + str(testnum) + "_mod_distrib" + str(y) + ".pdf","PAGE_LAYOUT", image_quality = "BEST")
        symbo_lyr.visible = True
        arcpy.mapping.RemoveLayer(df, arcpy.mapping.ListLayers(mxd)[1])
replicated_mod_distrib(23, "16_2025", name_property, ts_2005, ts_2010, ts_2016, map_path, basemap, "png")

basemap = "Mapping_basemap_20_replicateoutput_5.mxd"
def replicated_mod_distrib_future(netnum, testnum, name_property, ts_2005, ts_2010, ts_2016, ts_2025, map_path, basemap, format):
    pres_2005 = {}
    pres_2010 = {}
    pres_2016 = {}
    pres_2025 = {}
    countver = len([name for name in os.listdir("Network_" + str(netnum) + "\Results\Network" + str(netnum) + "_test" + str(testnum))])
    #Output path
    outpath = "Parameters_and_outputs\Network_" + str(netnum) + "\Network" + str(netnum) + "_test" + str(testnum) + "\\"

    with arcpy.da.SearchCursor(ini_table, ['*']) as cursor:
        for row in cursor:
            pres_2005[row[0]] = 0
            pres_2010[row[0]] = 0
            pres_2016[row[0]] = 0
            pres_2025[row[0]] = 0


    for ver in range(1,countver+1) :
        vernum = ver
        #Table workspace
        table_path = "Network_" + str(netnum) + "\Results\Network" + str(netnum) + "_test" + str(testnum)\
                     + "\Network" + str(str(netnum))+ "_test" + str(testnum) + "-[" + str(vernum) + "]\Generated Properties"

        #Import table
        if vernum > 1:
            table_name = "\\" + name_property + "-[" + str(vernum) + "]"
            table = table_path + table_name + ".csv"
        else :
            table_name = "\\" + name_property
            table = table_path + table_name + ".csv"
        print(table)

        #Add 1 to a reach in dictionaries for each replicate for which crayfish are in a reach
        with arcpy.da.SearchCursor(table, ['*']) as cursor:
            for row in cursor:
                try:
                    if row[ts_2005] > 0:
                        pres_2005[row[0]] += 1
                    if row[ts_2010] > 0:
                        pres_2010[row[0]] += 1
                    if row[ts_2016] > 0:
                        pres_2016[row[0]] += 1
                    if row[ts_2025] > 0:
                        pres_2025[row[0]] += 1
                except:
                    pass

    #Create copy of network and add model outputs in 2005,2010, and 2016 to three new fields
    repli_out = outpath + "replicated_output" + "test" + str(testnum)
    arcpy.CopyFeatures_management(network_updist, repli_out + ".shp")

    arcpy.AddField_management(repli_out+".shp", "modpres05", "INTEGER", field_is_nullable="NULLABLE")
    arcpy.AddField_management(repli_out+".shp", "modpres10", "INTEGER", field_is_nullable="NULLABLE")
    arcpy.AddField_management(repli_out+".shp", "modpres16", "INTEGER", field_is_nullable="NULLABLE")
    arcpy.AddField_management(repli_out+".shp", "modpres25", "INTEGER", field_is_nullable="NULLABLE")

    [f.name for f in arcpy.ListFields(repli_out + ".shp")]

    with arcpy.da.UpdateCursor(repli_out + ".shp", ["rid","modpres05", "modpres10","modpres16","modpres25"]) as cursor:
        for row in cursor:
            row[1] = pres_2005[row[0]]
            row[2] = pres_2010[row[0]]
            row[3] = pres_2016[row[0]]
            row[4] = pres_2025[row[0]]
            cursor.updateRow(row)

    #Import mxd
    mxd = arcpy.mapping.MapDocument(map_path + basemap)

    #Grab the first dataframe
    df = arcpy.mapping.ListDataFrames(mxd, "Layers")[0]
    if arcpy.Exists(repli_out + ".lyr"):
        arcpy.Delete_management(repli_out + ".lyr")
        arcpy.MakeFeatureLayer_management(repli_out + ".shp", "modpresabs")
        arcpy.SaveToLayerFile_management("modpresabs", repli_out + ".lyr", "ABSOLUTE")
        #Add layer to df
        join_lyr_df = arcpy.mapping.Layer(repli_out + ".lyr")
        arcpy.mapping.AddLayer(df, join_lyr_df)
    else:
        arcpy.MakeFeatureLayer_management(repli_out + ".shp", "modpresabs")
        arcpy.SaveToLayerFile_management("modpresabs", repli_out + ".lyr", "ABSOLUTE")
        #Add layer to df
        join_lyr_df = arcpy.mapping.Layer(repli_out + ".lyr")
        arcpy.mapping.AddLayer(df, join_lyr_df)


    symbo_lyr = arcpy.mapping.ListLayers(mxd)[1]
    print(symbo_lyr.name)
    symbology0 = symbo_lyr.symbology
    layer = arcpy.mapping.ListLayers(mxd)[0]

    #List all layout elements
    arcpy.mapping.ListLayoutElements(mxd)
    arcpy.mapping.UpdateLayer(df, layer, symbo_lyr, True)

    #Change layer symbology type to graduate colours
    layer.visible = True
    introp = [l for l in arcpy.mapping.ListLayers(mxd) if re.search('Intro', l._fullName)][0]
    arcpy.mapping.MoveLayer(df, layer, introp, "BEFORE")

    date_txt = arcpy.mapping.ListLayoutElements(mxd,"TEXT_ELEMENT")[1]

    for y in [2005, 2010, 2016, 2025]:
        if y <= 2016:
            presabs_ref = [l for l in arcpy.mapping.ListLayers(mxd) if re.search(str(y)[2:], l._fullName)][0]
            arcpy.mapping.MoveLayer(df, layer, presabs_ref, "BEFORE")
        try:
            #Get layer fields that starts with Tmn
            desc = arcpy.Describe(layer)
            layersource = desc.catalogPath
            fields = arcpy.ListFields(layersource, "*" + str(y)[2:] + "*")[0]

            #Set date text
            date_txt.text = str(y)

            layerSymbolClass = layer.symbology

            #print layerSymbolClass.valueField

            #Apply symbology
            arcpy.ApplySymbologyFromLayer_management(layer, symbo_lyr)
            layerSymbolClass.valueField = str(fields.name)
            layer.visible = True
            arcpy.mapping.ListLayers(mxd)[1].visible = True
            arcpy.RefreshTOC()
            arcpy.RefreshActiveView()
            symbo_lyr.visible = False
            #Export to jpeg
            #print(outpath + "test_" + str(testnum) + "_mod_distrib" + str(year) + ".jpg")
            if format == "png":
                arcpy.mapping.ExportToJPEG(mxd, outpath + "test_" + str(testnum) + "_mod_distrib" + str(y) + ".jpg", resolution = 300)
            if format == "pdf":
                arcpy.mapping.ExportToPDF(mxd, outpath + "test_" + str(testnum) + "_mod_distrib" + str(y) + ".pdf","PAGE_LAYOUT", image_quality = "BEST")
            symbo_lyr.visible = True
        except:
            "Error in making map for year" + str(y)
        if y <= 2016:
            arcpy.mapping.RemoveLayer(df, arcpy.mapping.ListLayers(mxd)[1])
replicated_mod_distrib_future(23, "16_2025", name_property, ts_2005, ts_2010, ts_2016, ts_2025, map_path, basemap, "pdf")

def replicated_mod_distrib_paper(netnum, testnum, name_property, ts_2005, ts_2010, ts_2016, map_path, basemap, format):
    pres_2005 = {}
    pres_2010 = {}
    pres_2016 = {}
    countver = len([name for name in os.listdir("Network_" + str(netnum) + "\Results\Network" + str(netnum) + "_test" + str(testnum))])
    #Output path
    outpath = "Parameters_and_outputs\Network_" + str(netnum) + "\Network" + str(netnum) + "_test" + str(testnum) + "\\"

    with arcpy.da.SearchCursor(ini_table, ['*']) as cursor:
        for row in cursor:
            pres_2005[row[0]] = 0
            pres_2010[row[0]] = 0
            pres_2016[row[0]] = 0

    for ver in range(1,countver+1) :
        vernum = ver
        #Table workspace
        table_path = "Network_" + str(netnum) + "\Results\Network" + str(netnum) + "_test" + str(testnum)\
                     + "\Network" + str(str(netnum))+ "_test" + str(testnum) + "-[" + str(vernum) + "]\Generated Properties"

        #Import table
        if vernum > 1:
            table_name = "\\" + name_property + "-[" + str(vernum) + "]"
            table = table_path + table_name + ".csv"
        else :
            table_name = "\\" + name_property
            table = table_path + table_name + ".csv"
        print(table)
        #Add 1 to a reach in dictionaries for each replicate for which crayfish are in a reach
        with arcpy.da.SearchCursor(table, ['*']) as cursor:
            for row in cursor:
                try:
                    if row[ts_2005] > 0:
                        pres_2005[row[0]] += 1
                    if row[ts_2010] > 0:
                        pres_2010[row[0]] += 1
                    if row[ts_2016] > 0:
                        pres_2016[row[0]] += 1
                except:
                    print("Replicate" + str(ver) + "failed. Surely missing time steps")
                    pass


    #Create copy of network and add model outputs in 2005,2010, and 2016 to three new fields
    repli_out = outpath + "replicated_output" + "test" + str(testnum)
    arcpy.CopyFeatures_management(network_updist, repli_out + ".shp")

    arcpy.AddField_management(repli_out+".shp", "modpres05", "INTEGER", field_is_nullable="NULLABLE")
    arcpy.AddField_management(repli_out+".shp", "modpres10", "INTEGER", field_is_nullable="NULLABLE")
    arcpy.AddField_management(repli_out+".shp", "modpres16", "INTEGER", field_is_nullable="NULLABLE")

    [f.name for f in arcpy.ListFields(repli_out + ".shp")]

    with arcpy.da.UpdateCursor(repli_out + ".shp", ["rid","modpres05", "modpres10","modpres16"]) as cursor:
        for row in cursor:
            row[1] = pres_2005[row[0]]
            row[2] = pres_2010[row[0]]
            row[3] = pres_2016[row[0]]
            cursor.updateRow(row)

    #Import mxd
    mxd = arcpy.mapping.MapDocument(map_path + basemap)

    #Grab the first dataframe
    df = arcpy.mapping.ListDataFrames(mxd, "Layers")[0]
    if arcpy.Exists(repli_out + ".lyr"):
        arcpy.Delete_management(repli_out + ".lyr")
        arcpy.MakeFeatureLayer_management(repli_out + ".shp", "modpresabs")
        arcpy.SaveToLayerFile_management("modpresabs", repli_out + ".lyr", "ABSOLUTE")
        #Add layer to df
        join_lyr_df = arcpy.mapping.Layer(repli_out + ".lyr")
        arcpy.mapping.AddLayer(df, join_lyr_df)
    else:
        arcpy.MakeFeatureLayer_management(repli_out + ".shp", "modpresabs")
        arcpy.SaveToLayerFile_management("modpresabs", repli_out + ".lyr", "ABSOLUTE")
        #Add layer to df
        join_lyr_df = arcpy.mapping.Layer(repli_out + ".lyr")
        arcpy.mapping.AddLayer(df, join_lyr_df)


    symbo_lyr = arcpy.mapping.ListLayers(mxd)[1]
    print(symbo_lyr.name)
    symbology0 = symbo_lyr.symbology
    layer = arcpy.mapping.ListLayers(mxd)[0]

    #List all layout elements
    arcpy.mapping.ListLayoutElements(mxd)
    arcpy.mapping.UpdateLayer(df, layer, symbo_lyr, True)

    #Change layer symbology type to graduate colours
    layer.visible = True
    introp = [l for l in arcpy.mapping.ListLayers(mxd) if re.search('Intro', l._fullName)][0]
    arcpy.mapping.MoveLayer(df, layer, introp, "BEFORE")

    date_txt = arcpy.mapping.ListLayoutElements(mxd,"TEXT_ELEMENT")[1]

    for y in [2005, 2010, 2016]:
        presabs_ref = [l for l in arcpy.mapping.ListLayers(mxd) if re.search(str(y)[2:], l._fullName)][0]
        arcpy.mapping.MoveLayer(df, layer, presabs_ref, "BEFORE")

        #Get layer fields that starts with Tmn
        desc = arcpy.Describe(layer)
        layersource = desc.catalogPath
        fields = arcpy.ListFields(layersource, "*" + str(y)[2:] + "*")[0]

        #Set date text
        date_txt.text = str(y)

        layerSymbolClass = layer.symbology

        #print layerSymbolClass.valueField

        #Apply symbology
        arcpy.ApplySymbologyFromLayer_management(layer, symbo_lyr)
        layerSymbolClass.valueField = str(fields.name)
        layer.visible = True
        arcpy.mapping.ListLayers(mxd)[1].visible = True
        arcpy.RefreshTOC()
        arcpy.RefreshActiveView()
        symbo_lyr.visible = False
        #Export to jpeg
        #print(outpath + "test_" + str(testnum) + "_mod_distrib" + str(year) + ".jpg")
        if format == "png":
            arcpy.mapping.ExportToJPEG(mxd, outpath + "test_" + str(testnum) + "_mod_distrib" + str(y) + ".jpg", resolution = 300)
        if format == "pdf":
            arcpy.mapping.ExportToPDF(mxd, outpath + "test_" + str(testnum) + "_mod_distrib" + str(y) + ".pdf","PAGE_LAYOUT", image_quality = "BEST")
        symbo_lyr.visible = True
        arcpy.mapping.RemoveLayer(df, arcpy.mapping.ListLayers(mxd)[1])
#replicated_mod_distrib_paper(23, "16_2025", name_property, ts_2005, ts_2010, ts_2016, map_path, basemap, "pdf")



########################################################################################################################
####################              GENERATE NETWORK TO COMPUTE DISTANCE FROM INVASION EDGE                 ##############
########################################################################################################################
#Stream network
network = "Hexsim_ready_data\Network_module/Network_18/edges_18.shp"
[f.name for f in arcpy.ListFields(network)]
rout_out = "Parameters_and_outputs\edges_18_rout.shp"
net_point = "Parameters_and_outputs\edges_18_point.shp"
#Edit GNIS name for mainstem and main tributaries in "F:\Hexsim\Crayfish_model\Hexsim_ready_data\Network_module/Network_18/edges_18.shp"


arcpy.CreateRoutes_lr(in_line_features=network, route_id_field = 'GNIS_NAME', out_feature_class = rout_out,
                      measure_source = 'LENGTH')

#Flip direction of Beech Creek route in Arcmap: Edit, double-click on route, right click, flip, right click again, then in Route measure editing, Set distance as.. replace value by 0

#Compute distance upstream from final outlet for each edge in the network
arcpy.FeatureVerticesToPoints_management(network, net_point, point_location = "MID")
#The output table has FID as the inputID
arcpy.LocateFeaturesAlongRoutes_lr(in_features= net_point, in_routes= rout_out, route_id_field = 'GNIS_NAME', radius_or_tolerance = '2 meters',
                                   out_table = "Parameters_and_outputs\edge_updist.dbf",
                                   out_event_properties = "Rout_field POINT updist", in_fields = "NO_FIELDS")

#Then join table to network and output network as
network_updist = "Hexsim_ready_data\Network_module/Network_18/edges_18_updist.shp"
arcpy.MakeFeatureLayer_management(network, "network_lyr")
arcpy.env.qualifiedFieldNames = False
arcpy.AddJoin_management(in_layer_or_view="network_lyr", in_field="FID", join_table="Parameters_and_outputs\edge_updist.dbf", join_field="OID", join_type="KEEP_ALL")
arcpy.CopyFeatures_management("network_lyr", network_updist)


########################################################################################################################
###################################Delete all heavy files from HexSim output############################################
netnum = 20

for name in os.listdir("Network_" + str(netnum) + "\Results"):
    for ver in range(1,11):
        restab_path = "Network_" + str(netnum) + "\Results\\" + name + "\\" + name + "-[" + str(ver) + "]\\" + name + ".log"
        print restab_path
        try:
            os.remove(restab_path)
            print("Deleted")
        except OSError:
            print("Pass")
            pass

########################################################################################################################
###################################Compare HexSim model output densities to 2005, 2010, and 2016 data for supplementary information figure ##################
# Repetitive from previous tasks but want to leave functions as is
#Compute an average density and standard deviation for all reaches of network
netnum = 23
network = 'Hexsim_ready_data\Network_module\edges_25.shp'
testnum = "16_2025"
#Property name to map
name_property = "Dens"

ts_2025 = 312
ts_2016 = 208
ts_2010 = 135
ts_2005 = 75

countver = len([name for name in os.listdir("Network_" + str(netnum) + "\Results\Network" + str(netnum) + "_test" + str(testnum))])
#Output path
outpath = "Parameters_and_outputs\Network_" + str(netnum) + "\Network" + str(netnum) + "_test" + str(testnum) + "\\"

net_dens = defaultdict(list)
for ver in range(1,countver+1) :
    vernum = ver
    #Table workspace
    table_path = "\Network_" + str(netnum) + "\Results\Network" + str(netnum) + "_test" + str(testnum)\
                 + "\Network" + str(str(netnum))+ "_test" + str(testnum) + "-[" + str(vernum) + "]\Generated Properties"

    #Import table
    if vernum > 1:
        table_name = "\\" + name_property + "-[" + str(vernum) + "]"
        table = table_path + table_name + ".csv"
    else :
        table_name = "\\" + name_property
        table = table_path + table_name + ".csv"
    print(table)

    #Append the density for each reach to a dictionary
    with arcpy.da.SearchCursor(table, ['*']) as cursor:
        for row in cursor:
            net_dens[row[0]].append(row[ts_2016])

#Compute average
avgstd_dens = defaultdict(list)
for key, values in net_dens.items():
    avgstd_dens[key]=[numpy.mean(values)]
    avgstd_dens[key].append(numpy.std(values))

#Join sites to network
points= os.path.join(datadir, "Field_work_Data\Sampled_sites_notes_CPUE_kick_join.shp")
points_2005 = os.path.join(datadir, r"Crayfish_occurence\Adams\2005_sites.shp")
points_2010 = os.path.join(datadir, r"Crayfish_occurence\Sorenson_Olden\Sorenson_Olden_sampling_presabs_test.shp")
network = "Hexsim_ready_data\Network_module/Network_18/edges_18_updist.shp"
outpoints_2016= "Parameters_and_outputs\Network_" + str(netnum) + "\\Network" + str(netnum) + "_test" + str(testnum) + "\\Sampled_sites_notes_CPUE_kick_edges_18_join.shp"
outpoints_2005= "Parameters_and_outputs\Network_" + str(netnum) + "\\Network" + str(netnum) + "_test" + str(testnum) + "\\2005_sites_edges_18_join.shp"
outpoints_2010= "Parameters_and_outputs\Network_" + str(netnum) + "\\Network" + str(netnum) + "_test" + str(testnum) + "\\Sorenson_Olden_sampling_edges_18_join.shp"

year=2016

#Define projection to WGS1984 for 2016 data points
#sr = arcpy.SpatialReference(4326)
#arcpy.DefineProjection_management(in_dataset=points_2010,coor_system=sr)

points_snap =  "Parameters_and_outputs\Network_" + str(netnum) + "\\CPUE" + str(year) + "_snap.shp"
arcpy.CopyFeatures_management(points_2016, points_snap)
snap_env = [network, "EDGE", "100 meters"]
arcpy.Snap_edit(points_snap, [snap_env])

#Edit point on Canyon creek in CPUE2016_snap.shp to lie on Canyon Creek

#Join rusty crayfish occurrence points to network
operation = "JOIN_ONE_TO_ONE"
type = "KEEP_COMMON"
match = "CLOSEST"
radius = "1 meters"
distfield = "dist"
arcpy.SpatialJoin_analysis(target_features = points_snap, join_features = network, out_feature_class = outpoints_2016,
                           join_operation = operation, join_type = type, match_option = match,
                           search_radius = radius, distance_field_name = distfield)

arcpy.AddField_management(in_table=outpoints_2016,field_name='avgmoddens',field_type='FLOAT')
arcpy.AddField_management(in_table=outpoints_2016,field_name='stdmoddens',field_type='FLOAT')

with arcpy.da.UpdateCursor(outpoints_2016, ['rid','avgmoddens','stdmoddens']) as cursor:
        for row in cursor:
            row[1]=avgstd_dens[row[0]][0]/10
            row[2]=avgstd_dens[row[0]][1]/10
            cursor.updateRow(row)
        del cursor

arcpy.TableToDBASE_conversion(Input_Table=outpoints_2016, Output_Folder="Parameters_and_outputs\Network_" + str(netnum) + "\\Network" + str(netnum) + "_test" + str(testnum))
