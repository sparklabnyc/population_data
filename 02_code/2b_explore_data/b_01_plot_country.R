rm(list = ls())

#0a.Declare root directory, create folders, and load packages 
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'create_folder_structure.R'))
source(paste0(functions.folder,'script_initiate.R')) 

#1.Set values for iteration 
countries <- 'Madagascar'
years <- seq('1980', '2020', by = 5)
  
#2.Function for rasters; raster output in 03_output folder
process_raster_smaller_region(countries, years)

#3.Function for dataframe
country_spf_pop_wt_grdi <- process_df_smaller_region(countries, years)
  
  #Add ADM2 names
  add_admin_names <- left_join(admin_borders, country_spf_pop_wt_grdi)
  add_admin_names |>  write_sf(paste0(output.folder, 'specific countries/',countries,'_pop_wt_grdi.shp'))
  add_admin_names |> st_drop_geometry() |> dplyr::select(-c(rowid,shapeISO,shapeCanonical)) |> 
    write_csv(paste0(output.folder, 'specific countries/',countries,'_pop_wt_grdi.csv'))
  
#4.Map
#4a.Restrict pop-weight df to area of interest
add_borders <- left_join(admin_borders, pop_wt_data)
world <- ne_countries(scale = 'medium', returnclass = 'sf') |> filter(name != 'Antarctica') 
country_map <- world |> filter(admin == country)

#4b.Map
map_grdi <- ggplot(data = country_map) + 
  geom_sf() +
  geom_sf(data = add_borders, aes(fill = pop_wt_grdi)) + 
  colorspace::scale_fill_continuous_sequential(palette = "Mako", labels = comma, n_interp = 11, n.breaks = 6) + 
  guides(fill = guide_colorbar(title = "Population-weighted GRDI by ADM2")) +
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

jpeg(paste0(figures.folder,'map_pop_wt_grdi_adm2_',country,'.jpeg'), res = 400, height = 3000, width = 3500)
print(map_grdi)
dev.off()

#3c.Values
mean(add_borders$pop_wt_grdi)
max(add_borders$pop_wt_grdi)
min(add_borders$pop_wt_grdi)
hist(add_borders$pop_wt_grdi)
