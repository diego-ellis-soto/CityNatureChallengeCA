# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# CalEnviroScreen:
# Get Calenviroscreen variables for buffered puzzle locaitons 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

library(sf)       
library(dplyr)    
library(ggplot2)  
require(mapview)

# puzzles  = read.csv('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/StantonPuzzleStudyLocations_11042024.csv') |>
#   mutate(states_abbrev ='CA')

cenv = st_read('/Users/diegoellis/Downloads/calenviroscreen40gdb_F_2021.gdb/') |>
  dplyr::select(
    Tract, ZIP, Population, CIscore, CIscoreP, PM2_5, PM2_5_Pctl,
    Pesticides, Pesticides_Pctl, Tox_Releases, Tox_Releases_Pctl,
    Traffic, Traffic_Pctl,
    Solid_Waste, Solid_Waste_Pctl, Pollution, PollutionScore,
    Pollution_Pctl, Poverty, Poverty_Pctl,
    HousBurd, HousBurd_Pctl,
    ApproxLoc, Shape_Area, County
  )


# df = puzzles
# cenv = cenv
# buffer_size= 1000

get_cenv = function(df, cenv,  buffer_size=NA){
# cenv: Cal Enviro Screen GDB
  # df is puzzle locaitons
  # buffer_size in meters: 
# cenv_col_name: Cal Enviroscreen column name
  
  df_sf = st_as_sf(SpatialPointsDataFrame(puzzles,
                                          coords = df[,c('Long', 'Lat')],
                                          proj4string =CRS("+proj=longlat +datum=WGS84")
  ))

   p_sf = df %>% st_as_sf(coords = c('Long', 'Lat'), crs = st_crs(4326))  %>% st_transform(st_crs(cenv))
  
   # If no buffer, return
   if(
     is.na(buffer_size)
   ){
     # Spatial join point locations and income
     p_sf_cenv = st_join(p_sf, cenv)
     return(p_sf_cenv)
   }
   
   # If buffer:
   if(
     !is.na(buffer_size)
   ){
     df_sf_buffer <- p_sf %>%
       st_buffer(dist = buffer_size) # For example 1000m buffer
     
     buffered_point_joined_cenv <- st_join(
       df_sf_buffer,
       cenv, 
       join = st_intersects,
       left = FALSE)
     
     # Calculate the Mean Cenv Variables scores Within Each Buffer
     # Using total scores, not percentiles
     mean_cenv_buff_p <- buffered_point_joined_cenv %>%
       group_by(Name) %>%
       summarize(mean_CIscore = mean(CIscore, na.rm = TRUE),
                 mean_PM2_5 = mean(PM2_5, na.rm = TRUE),
                 mean_Pesticides = mean(Pesticides, na.rm = TRUE),
                 mean_Tox_Releases = mean(Tox_Releases, na.rm = TRUE),
                 mean_Traffic = mean(Traffic, na.rm = TRUE),
                 mean_Solid_Waste = mean(Solid_Waste, na.rm = TRUE),
                 mean_Pollution = mean(Pollution, na.rm = TRUE),
                 mean_Poverty = mean(Poverty, na.rm = TRUE),
                 mean_HousBurd = mean(HousBurd, na.rm = TRUE)
                 
                 )
     
     df_sf_w_mean_cenv_buf <- df %>%
       left_join(mean_cenv_buff_p, by = "Name") |>
       # Now to get it back to our original data framer annotate the mean income age and provide the size of the buffer
       # dplyr::select(Name, mean_income, mean_age) |> 
       mutate(buffer_size = paste0(buffer_size))
     
     return(df_sf_w_mean_cenv_buf)
   }
   
  }

df_cenv = get_cenv(puzzles, cenv, 1000)

