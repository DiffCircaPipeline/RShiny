#data_input_server
X_checkinput = function(yy, tt){
  stopifnot("Please make sure the time file contains column 'SampleID' and 'time'. " = all(c("SampleID", "time") %in% colnames(tt)) )
  stopifnot("The number of samples does not match the expression and time files. " =
              ncol(yy)==nrow(tt))
  stopifnot("SampleID in the time file does not match column names in the expression file. " =
              all(colnames(yy)%in%tt$SampleID)&all(tt$SampleID%in%colnames(yy)))
}

validate.study <- function(rv, db) {
  if(db$studytype=="Two"){
    if(is.null(rv$expr1)|is.null(rv$time1)|is.null(rv$expr2)|is.null(rv$time2))  {
      stop(MSG.datasetInput.noinput)
    }
  }else if(db$studytype=="One"){
    if(is.null(rv$expr1)|is.null(rv$time1))  {
      stop(MSG.datasetInput.noinput)
    }
  }
  studyname <- db$studyname
  if(is.null(studyname) || studyname == "") {
    stop(MSG.study.noname)
  }
  if(studyname %in% list.files(db$working.dir0)) stop(MSG.study.duplicate(studyname))
  #print("Validate study done. ")
}

toX = function(CP.obj0){
  expr1 = CP.obj0$expr1
  time1 = CP.obj0$time1
  groupIinfo = CP.obj0$groupIinfo
  groupIIinfo = CP.obj0$groupIIinfo
  stopifnot("Group I: Samples IDs in SampleID column in time input does not match column names of the data input." = all(colnames(expr1)%in%time1$SampleID)&all(time1$SampleID%in%colnames(expr1)))
  #print("load 1")

  x1 = list(data = expr1,
            time = time1$time[match(colnames(expr1), time1$SampleID)],
            gname = rownames(expr1))

  if(!(is.null(CP.obj0$expr2)|is.null(CP.obj0$time2))){
    expr2 = CP.obj0$expr2
    time2 = CP.obj0$time2
    stopifnot("Group II: Samples IDs in SampleID column in time input does not match column names of the data input." = all(colnames(expr1)%in%time1$SampleID)&all(time1$SampleID%in%colnames(expr1)))

    x2 = list(data = expr2,
              time = time2$time[match(colnames(expr2), time2$SampleID)],
              gname = rownames(expr2))
  }else{
    x2 = NULL
  }
  x = list(x1, x2)
  #print("load 2")
  names(x) = c(groupIinfo, groupIIinfo)
  return(x)
}

saveX = function(X, study, info = "FormattedData.rds"){
  save.dir = paste(study$working.dir, "save", sep = "/")
  print(save.dir)
  dir.create(save.dir, recursive = TRUE)
  saveRDS(X, file = paste(save.dir, info, sep = "/"))
}

loadX = function(workding.dir){
  out <- list()
  out$db <- readRDS(paste(workding.dir, "save", "db.rds", sep = "/"))
  if(out$db$printCP.obj){
    out$CP.obj = readRDS(paste(workding.dir, "save", "FormattedData.rds", sep = "/"))
  }
  if(out$db$printDRP){
    DRPfile = list.files(paste0(workding.dir, "/save"))[grepl("DR_parameter", list.files(paste0(workding.dir, "/save")))] #later: will list all the available results and let choose
    if(length(DRPfile)>0){
      out$DRP = readRDS( paste(workding.dir, "save", DRPfile[1], sep = "/"))
    }
  }
  if(out$db$printDRF){
    DRFfile = list.files(paste0(workding.dir, "/save"))[grepl("DR_fitness", list.files(paste0(workding.dir, "/save")))]
    if(length(DRFfile)>0){
      out$DRF = readRDS(paste(workding.dir, "save", DRFfile[1], sep = "/"))
    }
  }
  return(out)
}

update_rvDB <- function(rv, out, type = "match"){
  names.rv = names(rv)
  names.out = names(out)
  names.intersect = intersect(names.rv, names.out)
  if(type == "match"){
    for(i in 1:length(names.intersect)){
      a.name = names.intersect[i]
      rv[[a.name]] <- out[[a.name]]
    }
  }else if(type == "take2"){
    for(i in 1:length(names.out)){
      a.name = names.out[i]
      rv[[a.name]] <- out[[a.name]]
    }
  }
  return(rv)
}

# summary.loadStudy = function(){} #later.

#make time distribution plots

data_input_plot_time = function(df, type = "Two"){
  if(type == "Two"){
    ggplot2::ggplot(data = df, ggplot2::aes(x = time))+
      ggplot2::geom_histogram(bins = max(round(nrow(df)/10), 5))+
      ggplot2::theme_bw()+
      ggplot2::facet_grid(~group)
  }else{
    ggplot2::ggplot(data = df, ggplot2::aes(x = time))+
      ggplot2::geom_histogram(bins = max(round(nrow(df)/10), 5))+
      ggplot2::theme_bw()
  }
}


# Functions for joint_rhythm ----------------------------------------------
summary.TOJR = function(a.TOJR, type = "two_FirstTime", method = "Sidak_FS", alpha = 0.05, alpha.FDR = 0.05, alpha.type = "p-value"){ #if type = "two", we summarize TOJR, otherwise it is table(p-value<0.05)
  if(type == "two_FirstTime"){
    tab.p = data.frame(method = method,
                       cutoff = paste0("p-value < ", alpha),
                       RhyI = sum(a.TOJR$TOJR=="rhyI"),
                       RhyII = sum(a.TOJR$TOJR=="rhyII"),
                       RhyBoth = sum(a.TOJR$TOJR=="both"),
                       Arrhy = sum(a.TOJR$TOJR=="arrhy"))
    tab.q = data.frame(method = method,
                       cutoff = paste0("q-value < ", alpha.FDR),
                       RhyI = sum(a.TOJR$TOJR.FDR=="rhyI"),
                       RhyII = sum(a.TOJR$TOJR.FDR=="rhyII"),
                       RhyBoth = sum(a.TOJR$TOJR.FDR=="both"),
                       Arrhy = sum(a.TOJR$TOJR.FDR=="arrhy"))
    tab = rbind.data.frame(tab.p, tab.q)
  }else if(type == "two_more"){
    tab = data.frame(method = method,
                     cutoff = paste0(alpha.type, " < ", alpha),
                     RhyI = sum(a.TOJR$TOJR=="rhyI"),
                     RhyII = sum(a.TOJR$TOJR=="rhyII"),
                     RhyBoth = sum(a.TOJR$TOJR=="both"),
                     Arrhy = sum(a.TOJR$TOJR=="arrhy"))
  }else if(type == "one"){
    tab.p = data.frame(cutoff = paste0("p-value < ", alpha),
                       nRhythmic = sum(a.TOJR$pvalue<alpha))
    tab.q = data.frame(cutoff = paste0("q-value < ", alpha.FDR),
                       nRhythmic = sum(a.TOJR$qvalue<alpha))
  }
}

writeCPoutput <- function(x, db, type = "est2", TOJR.sel){
  if(type == "est1"){
    write.csv(x$rhythm, paste(db$working.dir, "ParamEst.csv", sep = "/"))
  }else if(type == "est2"){
    write.csv(x[[1]]$rhythm, paste0(db$working.dir, "/ParamEst_", db$gIinfo, ".csv"))
    write.csv(x[[2]]$rhythm, paste0(db$working.dir, "/ParamEst_", db$gIIinfo, ".csv"))
  }else if(type == "DRP"){
    write.csv(x, paste0(db$working.dir, "/DRparameter_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue,  ".csv"))
  }else if(type == "DRF"){
    write.csv(x, paste0(db$working.dir, "/DRfitness_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue,  ".csv"))
  }else if(type == "TOJR"){
    print("not yet")
  }
}

# dr_analysis
updateTOJR <- function(TOJR, selected, type = "DRP"){
  for(i in 1:length(TOJR)){
    if(i == selected){
      TOJR[[i]][[type]] <- "selected"
    }else{
      if(TOJR[[i]][[type]] == "selected"){
        TOJR[[i]][[type]] <- "Y"
      }
    }
  }
  return(TOJR)
}
updateTOJRsummary <- function(db, type = "DRP"){
  db$TOJRsummary[[type]] <- sapply(db$TOJR, function(a.list){
    a.list[[type]]
  })
  return(db)
}

# plots_scatter -----------------------------------------------------------

check.plot = function(db, obj = "CP.obj"){
  if((!db$printCP.obj)|(!exists(obj))){
    stop("No CP.obj present in the environment. Please run joint rhythm panel first. ")
  }
}

g.sel.reset = function(Rhythm1 = NULL, Rhythm2 = NULL, DRPres = NULL, DRFres = NULL, sel = row.select){
  if(!is.null(Rhythm1)){
    sel$Rhythm2_rows_selected = NULL;
    sel$DRPres_rows_selected = NULL;
    sel$DRFres_rows_selected = NULL;
  }else if(!is.null(Rhythm2)){
    sel$Rhythm1_rows_selected = NULL;
    sel$DRPres_rows_selected = NULL;
    sel$DRFres_rows_selected = NULL;
  }else if(!is.null(DRPres)){
    sel$Rhythm1_rows_selected = NULL;
    sel$Rhythm2_rows_selected = NULL;
    sel$DRFres_rows_selected = NULL;
  }else if(!is.null(DRFres)){
    sel$Rhythm1_rows_selected = NULL;
    sel$Rhythm2_rows_selected = NULL;
    sel$DRPres_rows_selected = NULL;
  }
  return(sel)
}


##later
# a function to check if the objects in the db are present in the save folder.

ifelse2 <- function(condition, condition.TRUE, condition.FALSE){
  if(condition){
    a.return <- condition.TRUE
  }else{
    a.return <- condition.FALSE
  }
  return(a.return)
}

round2 <- function(df, digits = 3){
  col.class = sapply(df, class)
  df[, col.class=="numeric"] <- round(df[, col.class=="numeric"], digits)
  return(df) 
}
