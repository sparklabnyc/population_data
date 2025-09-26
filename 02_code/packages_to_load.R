############ 1. Packages on CRAN ############

# list of packages to use (in alphabetical order and 5 per row)
# If new packages add here in alphabetical order
list.of.packages = c('acs','dlnm','dplyr','haven','ggplot2', 'here', 'tidycensus', 'totalcensus', 'zipcodeR',
                     'lubridate', 'fst','pipeR',
                     'RColorBrewer','readxl','rgdal','scales', 
                     'sqldf','tidyr','tidyverse', 'table1', 'survival', 'Epi')

# check if list of packages is installed. If not, it will install ones not yet installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) invisible(install.packages(new.packages,repos = "https://cloud.r-project.org"))

# load packages
invisible(lapply(list.of.packages, require, character.only = TRUE, quietly=TRUE))

############ 2. Packages on not on CRAN and to download from source ############

# list of packages not on CRAN (INLA only in this case)
list.of.packages.not.on.cran <- c('INLA')
new.packages.not.on.cran <- list.of.packages[!(list.of.packages.not.on.cran %in% installed.packages()[,"Package"])]
if(length(new.packages.not.on.cran))
  install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
invisible(lapply(list.of.packages.not.on.cran, require, character.only = TRUE, quietly=TRUE))
