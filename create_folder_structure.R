rm(list=ls())

#0.Load Packages
library(here)

#1a.Declare directories (can add to over time)
project.folder <-  paste0(print(here::here()),'/')

data.folder <-  paste0(project.folder, "01_data/")
adm2.pop.folder <- paste0(data.folder, 'pop_by_adm2/')

code.folder <-  paste0(project.folder, "02_code/")
process.code.folder <-  paste0(code.folder, "2a_process_data/")
explore.code.folder <-  paste0(code.folder, "2b_explore_data/")
comp.code.folder <-  paste0(code.folder, "2c_compare_pop_methods/")
functions.folder <- paste0(code.folder, "20_functions/")

output.folder <-  paste0(project.folder, "03_output/")
pop.wt.grdi.folder <- paste0(output.folder, 'pop_wt_grdi_data/')
pop.grdi.raster.folder <- paste0(output.folder, 'pop_wt_grdi_rasters/')
tables.folder <- paste0(project.folder, "04_tables/")
figures.folder <- paste0(project.folder, "05_figures/")


#1b.Identify list of folder locations which have just been created above
folders.names <-  grep(".folder",names(.GlobalEnv),value=TRUE)

#1c.Create function to create list of folders
# note that the function will not create a folder if it already exists 
create_folders <-  function(name){
  ifelse(!dir.exists(get(name)), dir.create(get(name), recursive=TRUE), FALSE)
}

#1d.Create the folders named above
lapply(folders.names, create_folders)