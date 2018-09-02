# SEIBM_rustycrayfish
Computer codes and data for Messager and Olden 2018 - Individual-based models forecast the spread and inform the management of an emerging riverine invader (https://onlinelibrary.wiley.com/doi/abs/10.1111/ddi.12829).

See https://figshare.com/account/home#/projects/36875 for data and HexSim build used in this study.

A fair bit of data cleaning, processing, and mapping was performed by hand in Excel, shiny apps, and ArcGIS for this study so the files cannot automatically reproduce all the outputs presented in the study. The original folder structure was conserved to correspond to paths in computer codes and may thus seem redundant. 

For additional help for modelling with HexSim, refer to:
-	Schumaker, N.H. & Brookes, A. (2018)  HexSim: a modeling environment for ecology and conservation Landscape Ecology, 33, 197-211. https://doi.org/10.1007/s10980-017-0605-9
-	http://www.hexsim.net/
-	https://groups.google.com/forum/#!forum/hexsim

Workflow

1.	Model river width for each reach in the HexSim network: width_calc_20180901.R
2.	Model water temperature for each reach in the HexSim network using ISEMP water temperature sensor dataset (http://isemp.org/watersheds/john-day/):
a.	Download MODIS daily land surface temperature data for 2000-2016 and extract landscape predictors of water temperature for each water temperature sensor: 2a_LST_download_and_format.py
b.	Clean, format, and join water temperature data and land surface temperature data (2000-2016), then develop model of water temperature data based on land surface temperature: 2b_ISEMP_format_9.R
c.	Format land surface temperature data and predict water temperature in each reach of the John Day River based on models developed in 2b_ISEMP_format_9.R: 2c_ISEMP_MODIS_TempOutput_3.R
d.	Clean, format, and join water temperature data and air temperature data (1999). Then develop model of water temperature data based on air temperature. Join water temperature predictions for 1999 and 2000-2016 and output dataset ready for HexSim model: 2d_ISEMP_WeatherStations_TempOutput.R
3.	Run HexSim model: Crayfish_model
4.	Process HexSim outputs: 4_HexSim_popoutput_visual_11.R
5.	Visualize HexSim outputs spatial (requires ArcGIS Desktop license): 5_Crayfish_model_visualization8.py
6.	Analyze spread and population size of rusty crayfish for a final model (produce Figure 3): 6_Fig3_model_output_graph.R
7.	Analyze and plot cost and effects of strategies to control the population and spread of rusty crayfish (produce Figure 5 and 6): 7_Fig5_and_6_control_graphs_6.R 
8.	Compare observed and modeled crayfish density throughout the HexSim network (produce Figure S7.4): 8_FigS7_4_density_comparison.R
