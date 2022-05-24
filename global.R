# This file will be executed prior to app startup to setup the necessary environment
installed <- installed.packages()[,"Package"]
if(!("DiffCircaPipeline" %in% installed)){
  stop("Please install DiffCircaPipeline R package first")
}

# #CRAN packages:
# for (package in c("utils", "devtools", "shinyBS", "cluster", "parallel", "igraph",
#                   "Rcpp", "RcppArmadillo", "RcppGSL", "ggplot2",
#                   "gplots", "shinyjs","dplyr","gridExtra", "reticulate")) {
#   if (!(package %in% installed)) {
#     install.packages(package, repos='http://cran.us.r-project.org')
#   }
# }
#
#
# #Bioconductor packages:
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# for (package in c("utils", "devtools", "shinyBS", "cluster", "parallel", "igraph",
#                   "Rcpp", "RcppArmadillo", "RcppGSL", "ggplot2",
#                   "gplots", "shinyjs","dplyr","gridExtra", "reticulate")) {
#   if (!(package %in% installed)) {
#     BiocManager::install(package)
#   }
# }

#load libraries
library(shiny)

###############
#library(CircadianPipeline)  # main pkg

#Include all global functions
dir <- "global"
for (f in list.files(path=dir, pattern="*.R")) {
  source(paste(dir, f, sep="/"))
}

# # Create the directory for database prior to application startup
# db <- new("Database", name="studies")

# Include all server modules
dir <- "server"
for (f in list.files(path=dir, pattern="*.R")) {
  source(paste(dir, f, sep="/"))
}

# Include all UI modules
dir <- "ui"
for (f in list.files(path=dir, pattern="*.R")) {
  source(paste(dir, f, sep="/"))
}

# # Create the directory for
# wd_default <- new("Database", name="studies")
#
# # Setting default working directory
# tryCatch({
#   DB.load.working.dir(wd_default)
# }, error=function(error){
#   DB.set.working.dir(wd_default, paste(getwd(), "data", sep="/"))
# })

