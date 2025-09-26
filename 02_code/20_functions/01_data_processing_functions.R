
#Extract country border using giscoR package
get_country_borders <- function(country) {
  country <- giscoR::gisco_get_countries(country = country,
                                         resolution = "01")
  return(country)
}

get_region_borders <- function(region) {
  country <- giscoR::gisco_get_countries(region = region,
                                         resolution = "01")
  return(region)
}

#Clip population raster to a given country
clip_country_function <- function(x) {
  terra::crop(
    x,
    terra::vect(country_borders),
    snap = "in",
    mask = T
  )
}

clip_country_admin_function <- function(x) {
  terra::crop(
    x,
    terra::vect(admin_borders),
    snap = "in",
    mask = T, touches = T
  )
}

clip_region_function <- function(x) {
  terra::crop(
    x,
    terra::vect(region_borders),
    snap = "in",
    mask = T
  )
}

#Process population-weighted GRDI values for specific countries
process_raster_smaller_region <- function(countries,years){
  for(country in countries){
    print(country)
    
    #1.Get adm borders for area of interest
    admin_borders <- geoboundaries(country, 'adm2') |> rowid_to_column()
    .GlobalEnv$admin_borders <- admin_borders
    
    for(year in years){
      print(year)
      
      #2.Load population data 
      pop_wt_raster <- terra::rast(paste0(pop.grdi.raster.folder, 'pop_wt_grdi_raster_',year,'.tif'))
      
      #3.Clip pop-weight raster to area of interest and save 
      clip_to_admin <- clip_country_admin_function(pop_wt_raster)
      clip_to_admin |> terra::writeRaster(paste0(output.folder,'specific countries/',country, '_',year,'_pop_wt_grdi.tif'))
    }
  }
}


process_df_smaller_region <- function(countries, years){
  for(ctry in countries){
    print(ctry)
    
    for(yr in years){
      print(yr)
      data <- read_feather(paste0(pop.wt.grdi.folder, 'pop_wt_grdi_',yr,'.feather')) |> 
        filter(country == ctry) |> 
        mutate(year = yr)
      
      if(exists("results_df")){
        results_df <- bind_rows(results_df, data)
      }else{
        results_df <- data
      }
    }
  }
  return(results_df)
}
