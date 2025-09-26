rm(list = ls())

#0a.Declare root directory, create folders, and load packages 
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R')) 

#0b.Load data and set file paths
pop_data <- 'ghsl_pop/'
unit_of_analysis <- 'adm2'
filenames <- list.files(path = paste0(data.folder, pop_data), pattern = 'tif$', full.names = T)
load_files <- map(filenames, terra::rast) 
all_admin_borders <- sf::st_read(paste0(data.folder, 'geo_boundaries/geoBoundariesCGAZ_ADM2.geojson'))  |> 
  left_join(read_csv(paste0(data.folder, 'geo_boundaries/geographic_region_codes.csv'))) |> na.omit()

#1.Set values for iteration
countries <- c(unique(all_admin_borders$name)) 
filename_nums <- c(1:length(load_files))

#2.Run function to process global population by ADM2 
pop_by_adm <- create_adm_pop_rasters(filename_nums, countries)

create_adm_pop_rasters <- function(filename_nums, countries){
  for(fn_nums in filename_nums){
    print(fn_nums)
    
    #0.Prep and load data 
    year <- filenames[[fn_nums]]
    year <- str_extract(year, '\\d{4}')
    pop_raster <- load_files[[fn_nums]] 
    
    for (raster_country in countries){
      print(raster_country)
      
      #Create raster of administrative unit boundaries
      admin_borders <- all_admin_borders |> filter(name == raster_country) 
      .GlobalEnv$admin_borders <- admin_borders
      
      vector_admin_borders <- terra::vect(admin_borders) 
      .GlobalEnv$vector_admin_borders <- vector_admin_borders
      
      adm_template <- terra::rast(vector_admin_borders, res = 0.008333333) 
      rast_adm_data <- terra::rasterize(vector_admin_borders, adm_template, field = 'shapeID') #1-km resolution raster of ADM2 ID
      
      #Clip population raster
      clipped_pop <- clip_country_admin_function(pop_raster) #1-km raster of population 

      #Calculate population per administrative unit and convert to df 
      resample_adm_data <- resample(rast_adm_data, clipped_pop) #Match population and ADM2 ID rasters
      
      adm_data_df <- c(clipped_pop, resample_adm_data) |> 
        as.data.frame() |>                          
        rename(pop = 1) |> 
        zoo::na.locf() |> 
        group_by(shapeID) |> 
        summarize(adm_pop = sum(pop))

      if(exists("results_df")){
        results_df <- bind_rows(results_df, adm_data_df)
      }else{
        results_df <- adm_data_df
      }
    }
    results_df |> write_feather(paste0(adm2.pop.folder, unit_of_analysis,'_pop_',year,'.feather')) #Save adm2 population as dataframe (shapeID; no geometry)
    rm(results_df)
  }
}

test <- read_feather(paste0(adm2.pop.folder, 'adm2_pop_2020.feather'))
test
sum(test$adm_pop)
test_raster <- terra::rast(paste0(data.folder, ghsl_pop, 'GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.tif'))
terra::global(test_raster, fun = 'sum', na.rm = TRUE)
