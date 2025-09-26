rm(list = ls())

#0a.Declare root directory, create folders, and load packages 
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R')) #source(paste0(functions.folder, "01_data_processing_functions.R")) 


#0b.Load data and set file paths
pop_data_type <- 'ghsl_pop/'
grdi_data <- 'grdi/'
filenames <- list.files(path = paste0(adm2.pop.folder), pattern = 'feather$', full.names = T)
load_files <- map(filenames, feather::read_feather) 
all_admin_borders <- sf::st_read(paste0(data.folder, 'geo_boundaries/geoBoundariesCGAZ_ADM2.geojson'))  |> 
  left_join(read_csv(paste0(data.folder, 'geo_boundaries/geographic_region_codes.csv'))) |> 
  na.omit(name)
grdi_data <- terra::rast(paste0(data.folder,grdi_data,'povmap-grdi-v1_all.tif'))

#1.Set values for iteration
countries <- c(unique(all_admin_borders$name)) 
filename_nums <- c(1:length(load_files))

#2.Run function to calculate population-weighted GRDI by ADM2
pop_wt_by_adm2 <- pop_wt_function(filename_nums, countries)

pop_wt_function <- function(filename_nums, countries){
  for(fn_num in filename_nums){
    print(fn_num)
    
    #0.Prep and load data 
    year <- filenames[[fn_num]]
    year <- str_extract(year, '\\d{4}')
    adm2_pop_df <- load_files[[fn_num]] 
    pop_data <- terra::rast(paste0(data.folder, pop_data_type, 'GHS_POP_E',year,'_GLOBE_R2023A_4326_30SS_V1_0.tif'))
    
    
    for (raster_country in countries){
      print(raster_country)
      
    #Create raster of administrative boundaries
    admin_borders <- all_admin_borders |> filter(name == raster_country) 
    .GlobalEnv$admin_borders <- admin_borders
    vector_admin_borders <- terra::vect(admin_borders) 
    .GlobalEnv$vector_admin_borders <- vector_admin_borders
    adm_template <- terra::rast(vector_admin_borders, res = 0.008333333) 
  
    #Add borders to ADM2 population data and rasterize 
    join_adm_borders <- left_join(admin_borders, adm2_pop_df)
    
    rast_adm_pop <- terra::rasterize(vect(join_adm_borders), adm_template, field = 'adm_pop')
    rast_adm_ids <- terra::rasterize(vector_admin_borders, adm_template, field = 'shapeID') 
  
    #Clip ADM2 population 
    clipped_adm_pop <- clip_country_admin_function(rast_adm_pop) 
    
    #Clip population raster
    clipped_pop <- clip_country_admin_function(pop_data) #1-km raster of population 
    
    #Clip GRDI 
    clipped_grdi <- clip_country_admin_function(grdi_data) #1-km raster of population 
    
    #Match extents of all rasters 
    resample_grdi <- resample(clipped_grdi, clipped_pop)
    resample_adm_pop <- resample(clipped_adm_pop, clipped_pop)
    resample_adm_ids <- resample(rast_adm_ids, clipped_pop)
    
    #Calculate population-weighted GRDI by ADM2
    calc_pop_wt <- terra::lapp(sds(list(clipped_pop, resample_grdi, resample_adm_pop)),
                               fun = function(r1,r2,r3){return(r1*r2/r3)})
    
    #Join pop-wt raster to adm raster and convert to df
    output_df <- c(calc_pop_wt,resample_adm_ids) |> as.data.frame() |> 
      rename(pop = 1) |> 
      group_by(shapeID) |> 
      summarize(pop_wt_grdi = sum(pop, na.rm = TRUE)) |> 
      mutate(country = raster_country) |> 
      mutate(pop_wt_grdi = case_when(
      pop_wt_grdi > 100 ~ 100, 
      TRUE ~ pop_wt_grdi
    ))
    
    if(exists("results_df")){
      results_df <- bind_rows(results_df, output_df)
    }else{
      results_df <- output_df
    }
    }
    results_df |> write_feather(paste0(pop.wt.grdi.folder, 'pop_wt_grdi_',year,'.feather'))
    rm(results_df)
  }
}