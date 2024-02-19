#### Setup #### 
library(tidycensus)
library(tidyverse)
library(sf)
library(mapview)
library(tigris)
library(RSocrata)

calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

conflicts_prefer(dplyr::filter)

setwd("/Users/annaduan/Desktop/GitHub/Phila-Housing-Voucher-Database/data")

#### Boundaries ####
phl_bound <- places(year = 2020, state = "42") %>%
  filter(NAME == "Philadelphia") %>%
  st_transform('ESRI:102728') %>%
  select(geometry)

zip_codes <- tigris::zctas(year = 2010, state = "42", starts_with = "191") %>%
  st_transform('ESRI:102728') %>%
  rename(zip_code = ZCTA5CE10) %>%
  select(zip_code)

tracts <- tracts(year = 2020, state = "42", county = "Philadelphia") %>%
  st_transform('ESRI:102728')


#### Neighborhood social and economic ####
hospitals<- st_read("https://opendata.arcgis.com/datasets/df8dc18412494e5abbb021e2f33057b2_0.geojson") %>%
  dplyr::select(geometry) %>%
  mutate(class = "hospital") %>% 
  sf::st_transform('ESRI:102728')

health_centers <- st_read("https://opendata.arcgis.com/datasets/f87c257e1039470a8a472694c2cd2e4f_0.geojson") %>%
  dplyr::select(geometry) %>%
  mutate(class = "health center")%>% 
  sf::st_transform('ESRI:102728')

census_api_key("7aaf667fa42073b560f4759176037374b38cb7fd", install = TRUE, overwrite = TRUE)

vars <- load_variables(year = 2022,
                       dataset = "acs5",
                       cache = TRUE)

##### ACS #####
acs_call = function(year){
dat <- get_acs(geography = "tract", variables = c(
                                             # DEMOGRAPHICS              
                                             "B01003_001", #TOTAL POPULATION
                                             "B01002_001", #MEDIAN AGE
                                             "B01001_026", #TOTAL FEMALE POP
                                             "B02001_002", #TOTAL WHITE ALONE
                                             "B02001_003", #TOTAL BLACK ALONE
                                             "B03003_003", #TOTAL HISPANIC/LATINO
                                             "C18120_006", #TOTAL UNEMPLOYED
                                             "B01001_003", #MALE UNDER 5
                                             "B01001_004", #MALE 5-9
                                             "B01001_005", #MALE 10-14
                                             "B01001_006", #MALE 15-17
                                             "B01001_027", #FEMALE UNDER 5
                                             "B01001_028", #FEMALE 5-9
                                             "B01001_029", #FEMALE 10-14
                                             "B01001_030", #FEMALE 15-17
                                             
                                             # ECONOMIC SITUATION
                                             "B11012_008", #TOTAL FEMALE-HEADED HOUSEHOLD
                                             "B17001_002", #TOTAL BELOW POVERTY
                                             "B19057_002", #TOTAL RECEIVING PUBLIC ASSISTANCE
                                             "B19013_001", #MEDIAN HOUSEHOLD INCOME
                                             
                                             # TENURE/MOBILITY/PROPERTY
                                             "B25003_003", #RENTER OCCUPIED
                                             "B11012_001", #TOTAL HOUSEHOLDS
                                             "B07013_001", #MOBILITY BY TENURE DENOM
                                             "B07013_004", #SAME HOUSE 1 YEAR AGO
                                             "B07013_007", #SAME COUNTY MOVE
                                             "B07013_010", #DIF COUNTY SAME STATE MOVE
                                             "B07013_013", #DIF STATE MOVE
                                             "B07013_016", #ABROAD MOVE
                                             "B25002_003", #VACANT PROPERTY
                                             "B25002_001", #HOUSING UNIT COUNT
                                             "B25002_002", #OWNER PROPERTY BY CONDITION DENOM
                                             "B25123_003", #OWNER OCC 1+ CONDITION
                                             "B25123_008", #RENTER PROPERTY BY CONDITION DENOM
                                             "B25123_009", #RENTER OCC 1+ CONDITION
                                             
                                             # HH INCOME
                                             "B19001_001", #HOUSEHOLDS W/ KNOWN INCOME
                                             "B19001H_014",#WHITE ALONE 100-124.99
                                             "B19001H_015",#WHITE ALONE 125-149.99
                                             "B19001H_016",#WHITE ALONE 150-199.99
                                             "B19001H_017",#WHITE ALONE 200+
                                             "B19001B_002",#BLACK ALONE UNDER 10	
                                             "B19001B_003",#BLACK ALONE 10-14.99	
                                             "B19001B_004",#BLACK ALONE 15-19.99	
                                             "B19001B_005" #BLACK ALONE 20-24.99
  ),
  year=year, state=42, county=101, geometry = FALSE, survey ="acs5")
  
  
dat_processed <- 
    dat %>%
    dplyr::select( -NAME, -moe) %>%
    spread(variable, estimate) %>%
    rename(population = B01003_001, 
           median_age = B01002_001,
           female = B01001_026, 
           white = B02001_002,
           black = B02001_003, 
           hispanic = B03003_003,
           unemployed = C18120_006,
           male_under_5 = B01001_003,
           male_5_9 = B01001_004,
           male_10_14 = B01001_005,
           male_15_17 = B01001_006,
           female_under_5 = B01001_027,
           female_5_9 = B01001_028,
           female_10_14 = B01001_029,
           female_15_17 = B01001_030,
           female_hh = B11012_008,
           public_asst = B19057_002, 
           poverty = B17001_002,
           
           medhhinc = B19013_001,
           renter_hh = B25003_003,
           total_hh = B11012_001,
           mob_tenure_denom = B07013_001,
           same_house_1_year = B07013_004,
           same_county_move = B07013_007,
           dif_county_same_state_move = B07013_010,
           dif_state_move = B07013_013,
           abroad_move = B07013_016,
           vacant = B25002_003,
           housing_units = B25002_001,
           owner_condition_denom = B25002_002,
           owner_1condition = B25123_003,
           renter_condition_denom = B25123_008,
           renter_1condition = B25123_009,
           known_hh_incomes = B19001_001,
           white100_124k = B19001H_014,
           white125_149k = B19001H_015,
           white150_199k = B19001H_016,
           white200k_plus = B19001H_017,
           black_10k_below = B19001B_002,
           black_10_14k = B19001B_003,
           black_15_19k = B19001B_004,
           black_20_24k = B19001B_005) %>%
  
  mutate(
    child_male = male_under_5 + male_5_9 + male_10_14 + male_15_17,
    child_female = female_under_5 + female_5_9 + female_10_14 + female_15_17,
    child_pct = ifelse(population > 0, (child_male + child_female) / population, 0),
    female_pct = ifelse(population > 0, female / population, 0),
    white_pct = ifelse(population > 0, white / population, 0),
    black_pct = ifelse(population > 0, black / population, 0),
    hispanic_pct = ifelse(population > 0, hispanic / population, 0),
    female_hh_pct = ifelse(total_hh > 0, female_hh / total_hh, 0),
    public_asst_pct = ifelse(total_hh > 0, public_asst / total_hh, 0),
    poverty_pct = ifelse(population > 0, poverty / population, 0),
    renter_pct = ifelse(housing_units > 0, renter_hh / housing_units, 0),
    vacancy_pct = ifelse(housing_units > 0, vacant / housing_units, 0),
    same_house_pct = ifelse(mob_tenure_denom > 0, same_house_1_year / mob_tenure_denom, 0),
    same_county_move_pct = ifelse(mob_tenure_denom > 0, same_county_move / mob_tenure_denom, 0),
    dif_county_same_state_move_pct = ifelse(mob_tenure_denom > 0, dif_county_same_state_move / mob_tenure_denom, 0),
    dif_state_move_pct = ifelse(mob_tenure_denom > 0, dif_state_move / mob_tenure_denom, 0),
    abroad_move_pct = ifelse(mob_tenure_denom > 0, abroad_move / mob_tenure_denom, 0),
    owner_housing_condition_pct = ifelse(owner_condition_denom > 0, owner_1condition / owner_condition_denom, 0),
    renter_housing_condition_pct = ifelse(renter_condition_denom > 0, renter_1condition / renter_condition_denom, 0),
    unemployed_pct = ifelse(population > 0, unemployed / population, 0),
    race_ice = ifelse(known_hh_incomes > 0, ((white100_124k + white125_149k + white150_199k + white200k_plus) - (black_10k_below + black_10_14k + black_15_19k + black_20_24k)) / known_hh_incomes, 0)) %>%
  dplyr::select(population, median_age, child_male, child_female, child_pct, female_pct, white_pct, black_pct, hispanic_pct,
                female_hh_pct, public_asst_pct, poverty_pct, medhhinc, renter_pct, vacancy_pct, same_house_pct,
                same_county_move_pct, dif_county_same_state_move_pct, dif_state_move_pct, abroad_move_pct,
                owner_housing_condition_pct, renter_housing_condition_pct, unemployed_pct, race_ice,
                GEOID) %>%
  rename_at(vars(-GEOID), ~ paste0(., as.character(year)))

return(dat_processed)
}

acs_2022 <- acs_call(2022)
acs_2021 <- acs_call(2021)
acs_2020 <- acs_call(2020)
acs_2019 <- acs_call(2019)

dat_acs <- acs_2022 %>%
  left_join(acs_2021, by = "GEOID") %>%
  left_join(acs_2020, by = "GEOID") %>%
  left_join(acs_2019, by = "GEOID") 

#### AFFH ####
affh <- st_read("AFFH.csv") %>%
  dplyr::select(zip_code, continue_living, q4, q7e, q7f, q9, q7b, q7g, q7h, q12, q14, q7a, q7c, q7d) %>% 
  #continue_living, q4(recommend), q7e (schools), q7f (transit), q9 (safety during day), 
  #q7b (building conditions), q7g (quality housing), q7h (housing afford), 
  #q12(satisfied w current housing), q14 (cost increase), q7a (cleanliness), 
  #q7c (streets and sidewalks), q7d (public space)
  dplyr::rename(recommend_neigh = q4,
                school_qual = q7e,
                transit_access = q7f,
                neigh_safe_day = q9,
                building_conditions = q7b,
                quality_housing = q7g,
                housing_afford = q7h,
                housing_satisfy = q12,
                rent_increase = q14,
                neigh_clean = q7a,
                street_qual = q7c,
                publ_space = q7d) %>%
  mutate(zip_code = as.numeric(zip_code)) 

dat_affh <- affh %>%
  group_by(zip_code) %>%
  summarise(continue_living = calculate_mode(continue_living),
            recommend_neigh = calculate_mode(recommend_neigh),
            school_qual = calculate_mode(school_qual),
            transit_access = calculate_mode(transit_access),
            neigh_safe_day = calculate_mode(neigh_safe_day),
            building_conditions = calculate_mode(building_conditions),
            quality_housing = calculate_mode(quality_housing),
            housing_afford = calculate_mode(housing_afford),
            housing_satisfy = calculate_mode(housing_satisfy),
            rent_increase = calculate_mode(rent_increase),
            neigh_clean = calculate_mode(neigh_clean),
            street_qual = calculate_mode(street_qual),
            publ_space = calculate_mode(publ_space))


#### Crime ####
crime_clean = function(dat, year){
crime_dat <- dat %>% dplyr::select(text_general_code, lat, lng) %>%
    mutate(lat = as.numeric(lat),
           lng = as.numeric(lng),
           year = year) %>%
    na.omit(lat) %>%
    na.omit(lng) %>%
    filter(lat > 38) %>%
    st_as_sf(., crs = 4326, coords = c("lng", "lat")) %>%
    sf::st_transform("ESRI:102728")
}


# call from opendataphilly SQL Carto API
crime_2023 <- read.csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272023-01-01%27%20AND%20dispatch_date_time%20%3C%20%272024-01-01%27") %>%
  crime_clean(., 2023)
crime_2022 <- read.csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272022-01-01%27%20AND%20dispatch_date_time%20%3C%20%272023-01-01%27") %>%
  crime_clean(., 2022)
crime_2021 <- read.csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272021-01-01%27%20AND%20dispatch_date_time%20%3C%20%272022-01-01%27") %>%
  crime_clean(., 2021)
crime_2020 <- read.csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272020-01-01%27%20AND%20dispatch_date_time%20%3C%20%272021-01-01%27") %>%
  crime_clean(., 2020)
crime_2019 <- read.csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272019-01-01%27%20AND%20dispatch_date_time%20%3C%20%272020-01-01%27") %>%
  crime_clean(., 2019)

dat_crime <- rbind(crime_2019, crime_2020, crime_2021, crime_2022, crime_2023) 

# function for parsing crime types
crime_parse <- function(dat, code_list){
  pattern <- paste(code_list, collapse = "|")
  crime_cat <- dat %>%
    filter(str_detect(text_general_code, pattern)) %>%
    select(year, geometry, text_general_code)
}

dat_firearm <- crime_parse(dat_crime, c("Assault Firearm", "Robbery Firearm"))

dat_persons <- crime_parse(dat_crime, c("Assault", "Robbery", "Rape", 
                                        "Offenses Against Family and Children", 
                                        "Homicide", "Sex Offenses"))

dat_drug <- crime_parse(dat_crime, c("Drug"))

dat_property <- crime_parse(dat_crime, c("Theft", "Vandalism", "Fraud", "Burglary", 
                                          "Arson", "Embezzlement", "Forgery", "Stolen"))

#### Environment ####
landuse <- st_read("https://opendata.arcgis.com/datasets/e433504739bd41049de5d8f4a22d34ba_0.geojson") %>%
  dplyr::select(C_DIG1DESC, C_DIG2DESC) %>%  #residential = 1, commercial = 2, industrial = 3, Vacant = 91
  sf::st_transform("ESRI:102728") 

residential_lu <- landuse %>%
  filter(C_DIG1DESC == 1) %>%
  mutate(area = gsub(" [US_survey_foot^2]","",as.character(st_area(.))))

commercial_lu <- landuse %>%
  filter(C_DIG1DESC == 2) %>%
  mutate(area = gsub(" [US_survey_foot^2]","",as.character(st_area(.))))

industrial_lu <- landuse %>%
  filter(C_DIG1DESC == 3) %>%
  mutate(area = gsub(" [US_survey_foot^2]","",as.character(st_area(.))))

vacant_lu <- landuse %>%
  filter(C_DIG2DESC == 91) %>%
  mutate(area = gsub(" [US_survey_foot^2]","",as.character(st_area(.))))

#### Public services ####
police <- st_read("https://opendata.arcgis.com/datasets/7e522339d7c24e8ea5f2c7780291c315_0.geojson") %>%
  sf::st_transform('ESRI:102728') %>%
  dplyr::select(geometry) %>%
  mutate(class = "police")

fire <- st_read("https://opendata.arcgis.com/datasets/341526186e014aa0aa3ef7e08a394a78_0.geojson") %>%
  sf::st_transform('ESRI:102728') %>%
  dplyr::select(geometry) %>%
  mutate(class = "fire")

schools <- st_read("https://opendata.arcgis.com/datasets/d46a7e59e2c246c891fbee778759717e_0.geojson") %>%
  filter(TYPE_SPECIFIC == "DISTRICT") %>%
  sf::st_transform('ESRI:102728') %>%
  dplyr::select(geometry, GRADE_LEVEL) %>%
  rename(grades = GRADE_LEVEL) %>%
  mutate(class = "schools")

septa_train <- st_read("https://opendata.arcgis.com/datasets/af52d74b872045d0abb4a6bbbb249453_0.geojson") %>%
  sf::st_transform('ESRI:102728') %>%
  st_intersection(phl_bound) %>%
  dplyr::select(geometry) %>%
  mutate(class = "septa trains")

septa_bus <- st_read("https://services2.arcgis.com/9U43PSoL47wawX5S/arcgis/rest/services/Bus_Stops_Fall_2023/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>%
  sf::st_transform('ESRI:102728') %>%
  st_intersection(phl_bound) %>%
  dplyr::select(geometry) %>%
  mutate(class = "septa buses")

#### Health ####
dat_brfss <- read_csv("https://data.cdc.gov/resource/yjkw-uj5s.csv?stateabbr=PA&countyname=Philadelphia") %>%
  dplyr::select(-stateabbr, -countyfips, -ends_with("crude95ci"), -geolocation) %>%
  rename(tract = tractfips,
         uninsured = access2_crudeprev,
         arthritis = arthritis_crudeprev,
         bingeDrink = binge_crudeprev,
         highBloodPressure = bphigh_crudeprev,
         bloodPressureMeds = bpmed_crudeprev,
         cancer = cancer_crudeprev,
         asthma = casthma_crudeprev,
         heartDisease = chd_crudeprev,
         doctorCheckups = checkup_crudeprev,
         cholesterolScreen = cholscreen_crudeprev,
         colonScreen65 = colon_screen_crudeprev,
         pulmonaryDisease = copd_crudeprev,
         clinicalServicesMen65 = corem_crudeprev,
         clinicalServicesWomen65 = corew_crudeprev,
         smoking = csmoking_crudeprev,
         dentalVisits = dental_crudeprev,
         diabetes = diabetes_crudeprev,
         highCholesterol = highchol_crudeprev,
         kidneyDisease = kidney_crudeprev,
         noPhysicalActivity = lpa_crudeprev,
         mammograms50 = mammouse_crudeprev,
         poorMentalHealth = mhlth_crudeprev,
         obese = obesity_crudeprev,
         poorPhysicalHealth = phlth_crudeprev,
         stroke = stroke_crudeprev)

shooting <- st_read("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+shootings&filename=shootings&format=geojson&skipfields=cartodb_id") %>%
  filter(year %in% c(2023, 2022, 2021, 2020, 2019),
         point_y > 38) %>%
  dplyr::select(year) %>%
  mutate(class = "Shooting",
         year = year - 2000) %>%
  sf::st_transform('ESRI:102728')
shooting$class <- paste(shooting$class,shooting$year, sep = "_")
shooting <- shooting %>% dplyr::select(class)
