# --- --- --- --- --- --- --- --- --- --- --- ---
# Load vairables for the city Nature Challenge
# --- --- --- --- --- --- --- --- --- --- --- ---

library(tidycensus)
library(sf)
library(dplyr)
library(ggplot2)


# Counties of interest
counties_bay_area <- c("San Francisco", "Marin", "Sonoma", "Napa",
                       "Solano", "Contra Costa", "Alameda",
                       "Santa Clara", "San Mateo")
counties_socal <- c("Los Angeles", "San Diego")

all_counties <- c(counties_bay_area, counties_socal)
all_counties <- c("San Francisco", "Marin", "Sonoma", "Napa",
                  "Solano", "Contra Costa", "Alameda",
                  "Santa Clara", "San Mateo")


# State of interest
state_fips <- "06"  # FIPS code for California

# --- --- --- --- --- --- --- --- --- --- --- ---
# ACS variables for demonstration
# --- --- --- --- --- --- --- --- --- --- --- ---

acs_vars <- c(
  median_household_income = "B19013_001",  # Median Household Income
  median_age              = "B01002_001"   # Median Age
  # You can add more variables as needed, e.g.:
  # total_pop              = "B01003_001"
  # or detailed race / poverty variables from tables B03002 or B17001
)

acs_data <- get_acs(
  geography = "tract",
  variables = acs_vars,
  state = state_fips,
  county = all_counties,
  year = 2023,           # Adjust as needed
  survey = "acs5",       # 5-year ACS
  geometry = TRUE,
  output = "wide"        # wide format (variables get separate columns)
)

# --- --- --- --- --- --- --- --- --- --- --- ---
# Cal Enviro Screen
# --- --- --- --- --- --- --- --- --- --- --- ---

# Load CalEnviro Screen
cenv = st_read('/Users/diegoellis/Desktop/Projects/Postdoc/CityNatureChallengeCA/Data/calenviroscreen40gdb_F_2021.gdb/') |>
  dplyr::select(
    Tract, ZIP, Population, CIscore, CIscoreP, PM2_5, PM2_5_Pctl,
    Pesticides, Pesticides_Pctl, Tox_Releases, Tox_Releases_Pctl,
    Traffic, Traffic_Pctl,
    Solid_Waste, Solid_Waste_Pctl, Pollution, PollutionScore,
    Pollution_Pctl, Poverty, Poverty_Pctl,
    HousBurd, HousBurd_Pctl,
    ApproxLoc, Shape_Area, County
  ) |> st_transform(st_crs(acs_data))


# --- --- --- --- --- --- --- --- --- --- --- ---
# National Walkability Index
# --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# National Walkability Index ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# NatWalkInd: National Walkability Index. In many analyses, this is the primary variable of interest.
# D2B_E8MIXA: Measure of land-use mix (sometimes called “Entropy”). The more mixed the land uses, the higher the potential walkability.
# D2A_EPHHM (housing/employment density): Often corresponds to population or housing-unit density.
# D3B (street connectivity): Often related to street-network connectivity or intersection density. High intersection density is typically associated with better walkability.
# D4A (transit accessibility): # Often related to transit accessibility.
# Ac_Land, TotPop, HH (households), Workers # Could be useful as demographic or land-area controls

walkabiltiy_sf = st_read('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/WalkabilityIndex/Natl_WI.gdb')  |> 
  dplyr::filter(STATEFP =='06') |> dplyr::mutate(
    Land_use_mix=D2B_E8MIXA,
    housing_employment_density=D2A_EPHHM,
    street_connectivity=D3B,
    transit_accessibility=D4A
    
  ) |> select(CBSA_Name, GEOID10, NatWalkInd, Land_use_mix, housing_employment_density,
              street_connectivity, transit_accessibility, Shape_Area) |>
  st_transform(st_crs(acs_data))




# Careful with -99999.00 values ! 
# state_income_centroid_buffer_transf_walk_scores = st_join(state_income_centroid_buffer_transf, walk_sf_transf)



# --- --- --- --- --- --- --- --- --- --- --- ---
# Social Vulnerability Score
# --- --- --- --- --- --- --- --- --- --- --- ---

us_svi <- st_read('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/SVI2022_US_tract.gdb')

# nationwide
us_svi_ca <- us_svi %>%
  dplyr::select(1:7, contains("RPL_THEME")) %>%
  dplyr::rename(GEOID = FIPS,
                #format switched to ESRI gdb, col name change -7/28/23
                geometry = Shape) %>%
  #CDC use -999 as NAs
  dplyr::filter(
    ST_ABBR == "CA") |>
  st_transform(st_crs(acs_data))   |> 
  mutate(GEOID_tract = GEOID) |>
  dplyr::select(GEOID_tract, GEOID,
                RPL_THEME1, RPL_THEME2,
                RPL_THEME3, RPL_THEME4,
                RPL_THEMES)# |> dplyr::filter(RPL_THEMES >= 0)


state_wide_svi_cali = st_read('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/SVI2022_CALIFORNIA_tract.gdb') %>%
  dplyr::select(1:7, contains("RPL_THEME")) %>%
  dplyr::rename(GEOID = FIPS,
                geometry = Shape) |>
  st_transform(st_crs(acs_data)) |> mutate(GEOID_tract = GEOID) |>
  dplyr::select(GEOID_tract, GEOID,
                RPL_THEME1, RPL_THEME2,
                RPL_THEME3, RPL_THEME4,
                RPL_THEMES)
# %>%
#   dplyr::filter(RPL_THEMES>= 0)


# Plot:

nation <- us_svi_ca %>%
  dplyr::select(GEOID, geometry, RPL_THEMES) %>%
  tidyr::drop_na() %>%
  #using NAD83 in tmap
  tm_shape(projection = sf::st_crs(3310))+
  tm_polygons("RPL_THEMES",
              palette = c("orange","navy"),
              style = "cont",
              breaks = c(0, 0.25, 0.5, 0.75, 1),
              title = "SVI (by tract)")+
  tm_layout(title = "Nationwide SVI: \nCalifornia subset",
            title.size = 1,
            title.position = c("left", "TOP"),
            legend.position = c("RIGHT", "bottom"),
            legend.title.size = 0.9,
            legend.width = 2)


state <- state_wide_svi_cali %>%
  dplyr::select(GEOID, geometry, RPL_THEMES) %>%
  tidyr::drop_na() %>%
  tm_shape(projection = sf::st_crs(3310))+
  tm_polygons("RPL_THEMES",
              palette = c("orange","navy"),
              style = "cont",
              breaks = c(0, 0.25, 0.5, 0.75, 1),
              title = "SVI (by tract)")+
  tm_layout(title = "Statewide SVI: \nCalifornia subset",
            title.size = 1,
            title.position = c("left", "TOP"),
            legend.position = c("RIGHT", "bottom"),
            legend.title.size = 0.9,
            legend.width = 2)


plots <- list(nation, state)

current.mode <- tmap_mode("plot")

tmap_arrange(
  plots,
  nrow = 1,
  width = c(0.34, 0.33, 0.33)
)


# 
# state_income_centroid_buffer_transf_walk_scores_vect_sf = st_as_sf(state_income_centroid_buffer_transf_walk_scores_vect)
# 
# state_wide_svi_cali_trans = state_wide_svi_cali |> st_transform(
#   st_crs(state_income_centroid_buffer_transf_walk_scores_vect_sf)
# ) |> mutate(GEOID_tract = GEOID) |>
#   dplyr::select(GEOID_tract, 
#                 RPL_THEME1, RPL_THEME2,
#                 RPL_THEME3, RPL_THEME4,
#                 RPL_THEMES)
# 
# 
# # state_wide_svi_cali_trans_tbl = state_wide_svi_cali_trans |> as_tibble() 
# 
# # Or left join?
# # Somewhrer more rows appeared 
# 
# 
# us_svi_ca_trans = us_svi_ca  |> st_transform(
#   st_crs(state_income_centroid_buffer_transf_walk_scores_vect_sf)
# ) |> mutate(GEOID_tract = GEOID) |> dplyr::select(GEOID_tract, 
#                                                   RPL_THEME1, RPL_THEME2,
#                                                   RPL_THEME3, RPL_THEME4,
#                                                   RPL_THEMES)
# 
# 
# state_wide_svi_cali_trans_v2 = st_make_valid(state_wide_svi_cali_trans)
# us_svi_ca_trans_v2 = st_make_valid(us_svi_ca_trans)





# --- --- --- --- --- --- --- --- --- --- --- ---
# Justice40
# --- --- --- --- --- --- --- --- --- --- --- ---



# --- --- --- --- --- --- --- --- --- --- --- ---
# HFI - Bio
# --- --- --- --- --- --- --- --- --- --- --- ---


continents <- ne_countries(scale = "medium", returnclass = "sf")
america_continents <- continents[continents$continent %in% c("North America"), ]
# america_continents <- st_transform(america_continents, crs(ndvi))
human_mod_americas_masked = raster('/Users/diegoellis/Downloads/PressPulsePause/hmod_americas_masked.tif')
continents <- ne_countries(scale = "medium", returnclass = "sf")
bio1_masked = raster('/Users/diegoellis/Downloads/PressPulsePause/bio1_americas_masked.tif')
bio_precip = raster('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/CHELSA_pr_12_1981-2010_V.2.1.tif')
bio1_terra = rast(bio1_masked)
bio_precip_terra = rast(bio_precip)
