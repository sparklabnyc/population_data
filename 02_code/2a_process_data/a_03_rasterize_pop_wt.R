rm(list = ls())

#0a.Declare root directory, create folders, and load packages 
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(code.folder,'packages_to_load.R'))

#0b. Select data and load relevant functions
filenames <- list.files(path = paste0(pop.wt.grdi.folder), pattern = 'feather$', full.names = T)
load_files <- map(filenames, feather::read_feather) 
all_admin_borders <- sf::st_read(paste0(data.folder, 'geo_boundaries/geoBoundariesCGAZ_ADM2.geojson'))  |> 
  left_join(read_csv(paste0(data.folder, 'geo_boundaries/geographic_region_codes.csv'))) 

filename_nums <- c(1:length(load_files))

rasterize_pop_wt_function(filename_nums)

rasterize_pop_wt_function <- function(filename_nums){
  for(fn_num in filename_nums){
    print(fn_num)
    
    #0.Prep and load data 
    year <- filenames[[fn_num]]
    year <- str_extract(year, '\\d{4}')
    adm_pop_df <- load_files[[fn_num]] 

    #1.Add geographic feature to pop-wt data
    add_geo <- left_join(all_admin_borders, adm_pop_df, by = 'shapeID')
    
    #2.Rasterize pop-wt data
    vector_admin_borders <- terra::vect(add_geo) 
    .GlobalEnv$vector_admin_borders <- vector_admin_borders
    
    adm_template <- terra::rast(vector_admin_borders, res = 0.008333333) 
    rast_adm_data <- terra::rasterize(vector_admin_borders, adm_template, field = 'pop_wt_grdi')
    
    rast_adm_data |> terra::writeRaster(paste0(pop.grdi.raster.folder,'pop_wt_grdi_raster_',year,'.tif'))
  }
}


test <- raster(paste0(output.folder, 
                       'specific_countries/Philippines_pop_wt_grdi_2005_2020/Philippines_2005_pop_wt_grdi.tif'))

tes2 <- st_read(paste0(output.folder, 
                      'specific_countries/Philippines_pop_wt_grdi_2005_2020/Philippines_pop_wt_grdi.shp'))
