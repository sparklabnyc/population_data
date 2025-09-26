rm(list = ls())

#0a.Declare root directory, create folders, and load packages 
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R')) 

#0b. Select data and load relevant functions
gpw_pop <- 'gwpv4_pop/'
ghsl_pop <- 'ghsl_pop/'
grdi_data <- 'grdi/'
country <- 'Madagascar'
year <- '2010'

#1a.Select country or region of interest
admin_borders <- geoboundaries(country, 'adm2') |> rowid_to_column()

#1b.Load population data
ghsl_data <- terra::rast(paste0(data.folder, ghsl_pop, 'GHS_POP_E',year,'_GLOBE_R2023A_4326_30SS_V1_0.tif'))
gpw_data <- terra::rast(paste0(data.folder,gpw_pop,'gpw_v4_basic_demographic_characteristics_rev11_atotpopbt_2010_cntm_30_sec.tif'))

#2a.Population of China according to GHSL data
ghsl_pop_raster <- clip_country_admin_function(ghsl_data)
ghsl_country_total <- terra::global(ghsl_pop_raster, fun = 'sum', na.rm = TRUE)

#2b.Population of China according to GPW data
gpw_pop_raster <- clip_country_admin_function(gpw_data)
gpw_country_total <- terra::global(gpw_pop_raster, fun = 'sum', na.rm = TRUE)

  #Absolute difference between datasets
   ghsl_country_total - gpw_country_total
   
  #Percent difference between datasets
   (ghsl_country_total - gpw_country_total)/ghsl_country_total

#3.Calculate pop per adm with GPW data
   
   #Create raster of administrative unit boundaries
   vector_admin_borders <- terra::vect(admin_borders) 
   adm_template <- terra::rast(vector_admin_borders, res = 0.008333333) 
   rast_adm_data <- terra::rasterize(vector_admin_borders, adm_template, field = 'shapeID') #1-km resolution raster of ADM2 ID
   
   #Clip population raster
   clipped_pop <- clip_country_admin_function(gpw_data) #1-km raster of population 
   
   #Match population and ADM2 ID rasters
   resample_adm_data <- resample(rast_adm_data, clipped_pop) 
   
   #Calculate population per administrative unit and convert to df 
   adm_gpw_data_df <- c(clipped_pop, resample_adm_data) |> 
     as.data.frame() |>                          
     rename(pop = 1) |> 
     zoo::na.locf() |> 
     group_by(shapeID) |> 
     summarize(adm_gpw_pop = sum(pop))
   
#4.Load pop per adm with GHSL data
  
  filenames <- list.files(path = paste0(adm2.pop.folder), pattern = 'feather$', full.names = T)
  load_files <- map(filenames, feather::read_feather) 
  adm_ghsl_data_df <- load_files[[7]] |> right_join(admin_borders)
  
#5.Map differences
  map_data <- left_join(adm_ghsl_data_df, adm_gpw_data_df) |> 
    mutate(pop_diff = adm_pop - adm_gpw_pop) |> 
    sf::st_as_sf()


world <- ne_countries(scale = 'medium', returnclass = 'sf') |> 
  filter(name == country)

pop_diff_map <- ggplot(data = world) +
  geom_sf() + 
  geom_sf(data = map_data, aes(fill = pop_diff)) + 
  scale_fill_viridis_c() +
  
  guides(fill = guide_colorbar(title = "Pop diff between GHSL and GPW")) +
  theme_bw() + theme(text = element_text(size = 12),
                     legend.text=element_text(size=12), 
                     legend.title = element_text(size = 12),
                     panel.grid.major = element_blank(),
                     axis.text.x = element_blank(), 
                     axis.text.y = element_blank(), 
                     axis.ticks = element_blank(),
                     plot.margin=grid::unit(c(0.1,0.1,0.1,0.1), "mm"), 
                     #plot.title = element_text(hjust = 0.5), 
                     panel.background = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_blank(),
                     legend.position = 'bottom',
                     legend.justification='center',
                     legend.key.width = unit(2.3, 'cm'),
                     legend.background = element_rect(fill="white"),
                     panel.border = element_blank(), 
                     strip.background = element_blank())


jpeg(file = paste0(figures.folder,'.jpg'), width = 4000, height = 3000, res = 400)
print(pop_diff_map)
dev.off()
