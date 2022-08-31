rm(list=ls())
#Download R Shiny from github and extract it, you will see a folder called "RShiny-main".
#Set the working directory to be the path to the folder "RShiny-main" (excluding "/RShiny-main" from the directory).
path_to_DiffCircaPipelineRShiny = "/THE_DIRECTORY_TO_THE_FOLDER_CONTAINING_RShiny-main"
setwd(path_to_DiffCircaPipelineRShiny)
shiny::runApp('RShiny-main', port=9987, launch.browser=T)

# setwd("/Users/xiangningxue/Library/CloudStorage/OneDrive-UniversityofPittsburgh/Research/circadian/ThePipeline/R_package_github/DiffCircaPipeline/") #XXN
# rm(list=ls())
# shiny::runApp('RShiny', port=9987, launch.browser=T)
