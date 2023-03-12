# DiffCircaPipelie Shiny App

## Notice

Currently the github repository is being editted for submission for Bioconductor. Some functions may be removed. Please download at https://zenodo.org/record/7559084 for the version that produces the same result for the Bioinformatics paper. 

## Installation 

Check list: 
  - Have R installed in your system. 
  - Install the DiffCircaPipeline R package following instructions here: https://github.com/DiffCircaPipeline/Rpackage. 
  - Install other packages required for running Rshiny (this is not a complete list):
  ```{R}
  
  if (!require("shinyBS")) install.packages('shinyBS')
  if (!require("shinyjs")) install.packages('shinyjs')
  if (!require("DT")) install.packages('DT')
  if (!require("colourpicker")) install.packages('colourpicker')
  if (!require("rjson")) install.packages('rjson')
  # if you use R version lower than 4.0.0, you will need to install rjson from archive: 
  # install.packages("https://cran.r-project.org/src/contrib/Archive/rjson/rjson_0.2.20.tar.gz", repos = NULL)
  
  ```
  - Download the Shiny app to your local directory from this page by clicking Code -> Download ZIP. 
  - Unzip what you downloaded. 
  - Open the Rshiny-main folder, and then open app.R file. 
  - Change the "/THE_DIRECTORY_TO_THE_FOLDER_CONTAINING_RShiny" to the directory where you unzip (do not include "/Rshiny-main" in the directory)
  - Click "Run App" on the top right. 
  
## Instruction 

The detailed user manual could be found here: https://github.com/DiffCircaPipeline/RShiny/blob/main/Others/DiffCircaPipeline_RShiny_Manual.pdf .
  



