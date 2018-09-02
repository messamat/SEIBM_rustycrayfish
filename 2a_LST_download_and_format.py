__author__ = 'Mathis Messager'
#Related publication: Messager ML, Olden JD. Individual-based models forecast the spread and inform the management
#                                             of an emerging riverine invader. Divers Distrib.2018;00:1-14. https://doi.org/10.1111/ddi.12829
#Purpose: Download MODIS Land Surface Temperature + extract LST values at the location of each water temperature logger in the John Day River

import urllib2
import requests
###############################################################################

#Enter this line in the terminal to test whether it works (the _cut file is a reduced text file with only 35 URLs to test out lines)
#Password is a fake, obviously.
wget -L --user=messamat --password=password  -i F:\Hexsim\Data\MODIS_LST_download\data_url_script_2016-11-03_172354_cut.txt --no-check-certificate
#Full download
#wget -L --user=messamat --password=password  -i F:\Hexsim\Data\MODIS_LST_download\data_url_script_2016-11-03_172354.txt --no-check-certificate
#wget -L --user=messamat --password=password  -i F:\Hexsim\Data\MODIS_LST_download\data_url_script_2016-11-04_205337.txt --no-check-certificate
#wget -L --user=messamat --password=password  -i F:\Hexsim\Data\MODIS_LST_download\data_url_script_2016-11-04_222912.txt --no-check-certificate
#wget -L --user=messamat --password=password  -i F:\Hexsim\Data\MODIS_LST_download\data_url_script_2016-11-04_222912.txt --no-check-certificate
wget -L --user=messamat --password=password  -i F:\Hexsim\Data\MODIS_LST_download\URL_lists\data_url_script_2016-11-16_173704.txt --no-check-certificate

###################################################################################
####################### GET LST VALUES FOR LOGGERS ################################

from pyhdf.SD import SD, SDC
import os
import pprint
import arcpy
import re
import glob
import sys
import time

##############################################
#### Check out structure of HDf files
os.chdir('F:\Hexsim\Data\MODIS_LST_download')
file_name = 'MOD11A1.A2000065.h09v04.005.2007176143322.hdf'
file = SD(file_name, SDC.READ)
print file.info()

datasets_dic = file.datasets()
#Check datasets in HDF
for idx, sds in enumerate(datasets_dic.keys()):
    print idx, sds


LST_obj = file.select('LST_Day_1km')  #Select dataset of interest
data = LST_obj.get() #Get sds data
print(data) #Check content

#Check data attributes
pprint.pprint(LST_obj.attributes())
#Correct for scale factor

########## Extract subdataset and output to GRID file ##################
#Defined coordinate system for a MODIS layer in Arcgis following https://code.env.duke.edu/projects/mget/wiki/SinusoidalMODIS
#Then save it in MODIS_LST_download directory as a .prj file
#Create Custom Geographic Transformation tool in ArcGIS following the previous link

#Because had 100 missing days, redo it with missing data in subfolder LST_missing

arcpy.env.workspace = r"F:\Hexsim\Data\MODIS_LST_download\LST_missing\LST_raw"
arcpy.env.overwriteOutput = True

dir = "F:\Hexsim\Data\MODIS_LST_download\LST_missing\LST_raw"
ref_dataset = 'F:\\Hexsim\\Data\MODIS_LST_download\\LST_records\\ref_prof_layer\\MOD2000065.tif'
sr_out = arcpy.SpatialReference("NAD 1983")

for name in glob.glob(dir + '\\MOD11A1*'):
    print(name)
    base = os.path.basename(name)
    file_name, file_ext = os.path.splitext(base)
    if file_ext == '.hdf':
        outfile = 'F:\\Hexsim\\Data\MODIS_LST_download\\LST_missing\\LST_records\\' + file_name[0:3] + file_name[9:16]
        arcpy.ExtractSubDataset_management(name, outfile + ".tif", "0")

        #Define projection
        sr_def = arcpy.Describe(ref_dataset).spatialReference
        arcpy.DefineProjection_management(name, sr_def)

        #Project raster
        arcpy.ProjectRaster_management(in_raster=outfile + ".tif", out_raster=outfile + "pr.tif",
                               out_coor_system= sr_out,
                               resampling_type="CUBIC",
                               geographic_transform = "MODIS V5 Sinusoidal to GCS NAD 1983")


####################################################################################################
################# CREATE LAYER OF POINTS WITH LOGGER LOCATIONS ##############################


sites_table = r'F:\Hexsim\Data\ISEMP\Collated\SitesInfo_JD_nodupli_clean.csv'
#Create XY layer
print[f.name for f in arcpy.ListFields(sites_table)]
out_Layer = 'loggers_sites_points'
out_feat_gdb =  r'F:\Hexsim\Data\ISEMP\Collated\ISEMP_MODIS.gdb\log_points3'
arcpy.MakeXYEventLayer_management(table = sites_table, in_x_field = 'SiteLongitudeDD', in_y_field = 'SiteLatitudeDD', out_layer = out_Layer, spatial_reference= sr_out)
arcpy.CreateFileGDB_management('F:\\Hexsim\\Data\\ISEMP\\Collated\\', 'ISEMP_MODIS')
arcpy.CopyFeatures_management(out_Layer,out_feat_gdb)


############################################################################################################
##### EXTRACT RASTER VALUES FOR EACH LOGGER FOR EACH DAY FROM 1999 TO 2016

############
# TRY ARCPY

#This worked until 2010175 and then crashed... so created a second point layer to restart from 2010176 until the end
#Extract value by point across all
ras_list = [ras for ras in glob.glob('F:\\Hexsim\\Data\MODIS_LST_download\\LST_missing\\LST_records\\*pr.tif')]
arcpy.CheckOutExtension("Spatial")

from arcpy.sa import *
time.clock()
for lay in ras_list:
    try:
        ExtractMultiValuesToPoints(out_feat_gdb, lay, 'NONE')
        print("Succeeded processing: " + lay)
    # Return geoprocessing specific errors
    except arcpy.ExecuteError:
        arcpy.AddError(arcpy.GetMessages(2))
    # Return any other type of error
    except Exception:
        e = sys.exc_info()[1]
        print(e.args[0])
time.clock()

#Given that tool crashed at 2010175, find position of that layer in ras_list and restart process using new point layer starting at the date.
for i, lay in enumerate(ras_list):
    if '2010175' in lay:
        print i

#Export both attribute tables in Arcmap to LST:
# for log_points1: LSTrecords_MOD2000066_MOD2010175.csv
# for log_points2: LSTrecords_MOD2010176_MOD2016290.csv
# for log_points3: LSTrecords_MOD2011107_MOD2011206.csv

########################################################################################################################
########################################### EXTRACT COVARIATES FOR EACH LOGGER #########################################
#Created a logger_points feature class with no fields to speed up/ease analysis: log_points_noLST
targ =  r'F:\Hexsim\Data\ISEMP\Collated\ISEMP_MODIS.gdb\log_points_noLST'
#Project logger points in UTM
arcpy.Project_management(in_dataset= targ , out_dataset= targ + '_UTM',
                               out_coor_system= arcpy.SpatialReference('NAD 1983 UTM Zone 11N'))

#Spatially join logger points to river network
#Only keep those on the river network (more robust position, hard to get variables for them, and more representative of streams in the NHD)
operation = "JOIN_ONE_TO_ONE"
type = "KEEP_COMMON"
match = "CLOSEST"
radius = "200 meters"
distfield = "dist"

targ =  r'F:\Hexsim\Data\ISEMP\Collated\ISEMP_MODIS.gdb\log_points_noLST'
net = 'F:\Hexsim\Data\River_network\NHD_formatting.gdb\NHDplus_streams_HU6clip_format_UTM'
outfea = r'F:\Hexsim\Data\ISEMP\Collated\ISEMP_MODIS.gdb\log_points_NHD_join'
#field_mapping = fieldmappings)

arcpy.SpatialJoin_analysis(target_features = targ + '_UTM', join_features = net, out_feature_class = outfea,
                           join_operation = operation, join_type = type, match_option = match,
                           search_radius = radius, distance_field_name = distfield)

#Take out all the useless fields in Arcmap:log_points_NHD_join_nofields
#In Arcmap join with StreamCat data
    #Get elevation(watershed: ElevWs, catchment: ElevCat)
    #Stream watershed area (WsAreaSqKm),
    #Stream gradient and reach maximum elevation (from F:\Hexsim\Data\River_network\NHDplusV2\NHDPlusPN\NHDPlus17\NHDPlusAttributes)
    #Mean housing density 	HUDen2010Cat and HUDen2010Ws (table USCensus2010)
    #Baseflow index: BFICat and BFIWs
    #Riparian vegetation: SUM of PctDecid2011CatRp100, PctConif2011CatRp100, PctMxFst2011CatRp100
        # SUM of PctDecid2011Ws, PctConif2011Ws, PctMxFst2011Ws
    #Ice in watershed PctIce2011Ws
    #After looking at the LCD, seems like Woody Wetlands is an important land cover type in the John Day
        #So will calculate PctWdWet2011CatRp100 (% of catchment area classified within 100m buffer)
        #Woody wetlands:  areas where forest or shrubland vegetation accounts for greater than 20% of vegetative cover and the soil or substrate is periodically saturated with or covered with water
#Only keep useful fields and export to log_points_NHD_join_selecfields
#Then join points with HUC6 and export to log_points_NHD_join_selecfields2


#To do it programatically
# Create a feature layer from the vegtype featureclass
#arcpy.MakeFeatureLayer_management (inFeatures,  layerName)
# Join the feature layer to a table
#arcpy.AddJoin_management(layerName, joinField, joinTable, joinField)
#Copy layer to a new permanent feature class
#arcpy.CopyFeatures_management(layerName, outFeature)

########################################################################################################################
########################################### EXTRACT TEMPERATURE AND COVARIATES FOR ALL STREAM REACHES ##################
#ADDED THE RECORDS FOR MISSING FOLDER TO MAIN MODIS_LST_DOWNLOAD FOLDER FOR THIS PART
#Stopped at 2008133
#Stopped again at 2011108

net = 'F:\Hexsim\Data\River_network\NHD_formatting.gdb\NHDplus_streams_HU6clip_format_UTM'
#net_point = 'F:\Hexsim\Data\ISEMP\Collated\ISEMP_MODIS.gdb\NHDplus_streams_HU6clip_format_UTM_point'
#net_point2 = "F:\Hexsim\Data\ISEMP\Collated\ISEMP_MODIS.gdb\NHDplus_streams_HU6clip_format_UTM_point_2008130"
net_point3 = "F:\Hexsim\Data\ISEMP\Collated\ISEMP_MODIS.gdb\NHDplus_streams_HU6clip_format_UTM_point_2011108"

#Convert network to points
#arcpy.FeatureToPoint_management(net, net_point, "INSIDE")


#Extract value by point across all
ras_list = [ras for ras in glob.glob('F:\\Hexsim\\Data\MODIS_LST_download\\LST_records\\*pr.tif')]
arcpy.CheckOutExtension("Spatial")

#Given that tool crashed at 2008130, find position of that layer in ras_list and restart process using new point layer starting at the date.
for i, lay in enumerate(ras_list):
    if '2015343' in lay:
        print i


from arcpy.sa import *
time.clock()
for lay in ras_list[5697:]:
    try:
        ExtractMultiValuesToPoints(net_point3, lay, 'NONE')
        print("Succeeded processing: " + lay)
    # Return geoprocessing specific errors
    except arcpy.ExecuteError:
        arcpy.AddError(arcpy.GetMessages(2))
    # Return any other type of error
    except Exception:
        e = sys.exc_info()[1]
        print(e.args[0])
time.clock()

#Join and export data
elev_tab = "F:\Hexsim\Data\River_network\NHDplusV2\NHDPlusPN\NHDPlus17\NHDPlusAttributes\elevslope.dbf"
arcpy.MakeFeatureLayer_management ("F:\Hexsim\Data\ISEMP\Collated\ISEMP_MODIS.gdb\NHDplus_streams_HU6clip_format_UTM_point",  "points1")
arcpy.JoinField_management("points1", "COMID", elev_tab, "COMID", ["MAXELEVSMO"])

arcpy.Delete_management("points1")

#Export both attribute tables in Arcmap to LST:
# for NHDplus_streams_HU6clip_format_UTM_point:
# for NHDplus_streams_HU6clip_format_UTM_point_2008130:
# for NHDplus_streams_HU6clip_format_UTM_point_2008130: LSTrecords_MOD2011107_MOD2011206.csv