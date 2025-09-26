rm(list = ls())

#0a.Declare root directory, create folders, and load packages 
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(code.folder,'packages_to_load.R'))

#0b. Select data and load relevant functions
ghsl_pop <- 'ghsl_pop/'
grdi_data <- 'grdi/'
country <- 'Madagascar'
year <- '2010'
source(paste0(functions.folder, "01_data_processing_functions.R")) 

#1a.Select country or region of interest
country_borders <- get_country_borders(country) 
country_admin_borders <- geoboundaries(country, 'adm2') |> rowid_to_column()

#1b.Load population and administrative unit data 
pop_data <- terra::rast(paste0(data.folder, ghsl_pop, 'GHS_POP_E',year,'_GLOBE_R2023A_4326_30SS_V1_0.tif'))
grdi_data <- terra::rast(paste0(data.folder,grdi_data,'povmap-grdi-v1_all.tif'))

#2.Create correct (per GHSL) population raster of given country (example here is Madagascar)
clipped_pop <- clip_country_admin_function(pop_data) #function relies on terra pkg 
terra::global(clipped_pop, fun = 'sum', na.rm = TRUE) #Population = 21,727,843 in 2010 given GHSL data 

#3.Process pop raster to ADM2

#3a.Turn population raster into sf points 
pop_df <- as.data.frame(clipped_pop, xy = TRUE) |> 
  rename(long = x, lat = y)
colMeans(is.na(pop_df))
pop_sf <- sf::st_as_sf(x = pop_df, coords = c("long", "lat"), crs = "EPSG:4326")
sum(pop_sf$GHS_POP_E2010_GLOBE_R2023A_4326_30ss_V1_0) #Population is consistent 

#3b.Join population points with administrative boundaries (here, ADM2)
join_pop_admin <- st_join(pop_sf,country_admin_borders, join = st_nearest_feature) |> rowid_to_column('ID')
colMeans(is.na(join_pop_admin))
sum(join_pop_admin$GHS_POP_E2010_GLOBE_R2023A_4326_30ss_V1_0)

#4.Extract GRDI value for every population point and join with pop, administrative data
extract_grdi <- terra::extract(grdi_data, pop_sf) #raster pkg also works; exact_extract does not support sf point file
mean(extract_grdi$`povmap-grdi-v1_all`, na.rm = TRUE) #mean GRDI value for Madagascar 

join_pop_grdi_admin <- left_join(extract_grdi, join_pop_admin)
colMeans(is.na(join_pop_grdi_admin)) #59% of GRDI values are NA; this is consistent with raw GRDI raster file 

#5.Assess consistency of extracted data by comparing GRDI mean from raw raster to extracted raster 
clipped_grdi <- clip_country_function(grdi_data)
terra::global(clipped_grdi, fun = 'mean', na.rm = TRUE) #mean GRDI value from raw GRDI raster matches mean GRDI from 'new' raster

#6.Explore population loss issue when using just an extract function to aggregate by administrative unit 

#6a.Using terra package; population numbers are closer but slower with larger datasets
extract_pop <- terra::extract(pop_data, country_admin_borders, fun = 'sum', bind = TRUE) |> as.data.frame() |> #TOUCHES = TRUE does not work (overcount)
  rename(extr_pop = GHS_POP_E2010_GLOBE_R2023A_4326_30ss_V1_0)
sum(extract_pop$extr_pop) #Population = 21,634,663; loss of 93,180

#6b.Join with correct population data to explore losses
examine_pop_loss <- left_join(extract_pop, join_pop_grdi_admin) |> 
  group_by(shapeName, shapeID, extr_pop) |> 
  summarize(correct_pop = sum(GHS_POP_E2010_GLOBE_R2023A_4326_30ss_V1_0)) |> 
  mutate(diff = correct_pop - extr_pop) |> 
  left_join(country_admin_borders) |> st_as_sf()

highlight_areas_with_top_losses <- examine_pop_loss |> filter(diff > 10000)

#7.Map differences

world <- ne_countries(scale = 'medium', returnclass = 'sf') |> filter(name != 'Antarctica') 
country_map <- world |> filter(admin == country)

map_pop_loss <- ggplot(data = country_map) + 
  geom_sf() +
  geom_sf(data = examine_pop_loss, aes(fill = diff)) + 
  colorspace::scale_fill_continuous_sequential(palette = "Mako", labels = comma, n_interp = 11, n.breaks = 6) + 
  guides(fill = guide_colorbar(title = "Population inaccuracy")) +
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
                     legend.key.width = unit(3, 'cm'),
                     legend.background = element_rect(fill="white"),
                     panel.border = element_blank(), 
                     strip.background = element_blank()) 

jpeg(paste0(figures.folder,'map_pop_loss_',country,'.jpeg'), res = 400, height = 3000, width = 3500)
print(map_pop_loss)
dev.off()


#Scrap
#6b.Using exact_extract package; may be easier to fix pop. loss issues w/ this pkg if we had population density data 
extract_pop <- exact_extract(pop_data, country_admin_borders, fun = 'sum', append_cols = c('shapeName', 'shapeID')) |> 
  rename(extr_pop = sum) #this is faster, but much more population mismatch from this extracted file to sf file 
                         #compared to terra extracted file to sf file
