db = list(working.dir = paste(getwd(), DB.dir, sep = "/"),
          studyname = "studies", studytype = "Two",
          gIinfo = "Group I", gIIinfo = "Group II",
          printCP.obj = FALSE,
          TOJR = list(),
          TOJRsummary = data.frame(NULL),
          printDRP = FALSE, printDRF = FALSE,
          dA = FALSE, dPhase = FALSE, dM = FALSE, upadte = 0)

# Study class
# setClass("Study1", representation(
#   studyName="character",
#   species="character",
#   MCMC="matrix"))

# Database class

# setClass("Database",
#          slots = c(studyname = "character",
#                    data1.file = "character",
#                    meta1.file = "character",
#                    info1 = "character",
#                    data2.file = "character",
#                    meta2.file = "character",
#                    info2 = "character",
#                    working.path = "character"))
#
# setMethod("initialize", "Database",
#           function(.Object, studyname) {
#             .Object <- callNextMethod()
#             .Object@data1.file <- paste(DB.dir, studyname, sep="/")
#             .Object@meta1.file <- paste(DB.dir, studyname, sep="/")
#             .Object@info1 <- "groupI"
#             .Object@data2.file <- NULL
#             .Object@meta2.file <- NULL
#             .Object@info2 <- "groupII"
#             .Object@working.path <- paste(".Database", studyname, sep="/") #the working directory for outputs. In the directory there will be a save folder for intermidiate outputs
#             if (!dir.exists(.Object@working.path))
#               dir.create(.Object@working.path, recursive = TRUE)
#             dir.create(paste(.Object@working.path, "save", sep="/"), recursive = TRUE)
#             .Object
#           }
# )
#
# db = new("Database", studyname = "ExampleData")
#
# # read Database into an R object.
# setMethod("toX", signature("Database"), function(object) {
#   x1.data = read.csv()
# })
# setMethod("toX", "Database",
#           function(.Object){
#
#           })
#
# # # Return Database meta information as data.frame
# # setMethod("meta", signature("Database"), function(object) {
# #   readRDS(db@meta.file)
# # })
# # setMethod("meta", signature("Study1"), function(object) {
# #   data.frame(species=object@species, studyName=object@studyName)
# # })
# # write database meta data to file, should be called everytime when
# # database is modified
#
# # save x to db as file
# DB.save <- function(db, study_use) {
#   #  if(class(study) != "Study") stop("study must be Study")
#   saveRDS(study_use, file=paste(db@dir, study_use@studyName, sep="/"))
#   studies <- DB.load(db, list.files(path=db@dir))
#   db.meta <- lapply(studies, function(study_use) meta(study_use))
#   db.meta <- do.call(rbind, db.meta)
#   DB.sync(db, db.meta)
# }
#
# # load file from db
# DB.load <- function(db, studies) {
#   res <- c()
#   for(study in studies) {
#     res <- c(res, readRDS(paste(db@dir, study, sep='/')))
#   }
#   res
# }
#
# MergedDB.load <- function(db){
#   if(file.exists(paste(DB.load.working.dir(db), "MergedDB.rds", sep="/"))){
#     readRDS(paste(DB.load.working.dir(db), "MergedDB.rds", sep="/"))
#   }else{
#     return(NA)
#   }
# }
#
# MergedSpecies.load <- function(db){
#   if(file.exists(paste(DB.load.working.dir(db),
#                        "MergedSpecies.rds", sep="/"))){
#     readRDS(paste(DB.load.working.dir(db),
#                   "MergedSpecies.rds", sep="/"))
#   }else{
#     return(NA)
#   }
# }
#
# MergedStudyNames.load <- function(db){
#   if(file.exists(paste(DB.load.working.dir(db),
#                        "MergedStudyNames.rds", sep="/"))){
#     readRDS(paste(DB.load.working.dir(db),
#                   "MergedStudyNames.rds", sep="/"))
#   }else{
#     return(NA)
#   }
# }
#
# MergedPM.load <- function(db){
#   if(file.exists(paste(DB.load.working.dir(db),
#                        "MergedPM.rds", sep="/"))){
#     readRDS(paste(DB.load.working.dir(db),
#                   "MergedPM.rds", sep="/"))
#   }else{
#     return(NA)
#   }
# }
#
# # delete file from db
# DB.delete <- function(db, studies) {
#   file.remove(paste(db@dir, list.files(path=db@dir)[as.numeric(studies)], sep="/"))
#   db.meta <- meta(db)
#   db.meta <- db.meta[!(rownames(db.meta) %in% studies),]
#   if(nrow(db.meta)>0){
#     rownames(db.meta) = 1:nrow(db.meta)
#   }
#   DB.sync(db, db.meta)
# }
#
# # list all files in db
# DB.ls <- function(db) {
#   meta(db)$studyName
# }
#
# DB.set.working.dir <- function(db, path){
#   file.con <- file(db@working.path)
#   writeLines(path, file.con)
#   close(file.con)
# }
#
# DB.load.working.dir <- function(db){
#   file.con <- file(db@working.path)
#   path <- readLines(file.con)
#   close(file.con)
#   if (length(path) == 0)
#     stop(MSG.no.working.dir)
#   else
#     path
# }
#
