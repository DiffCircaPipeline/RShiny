dr_analysis_server <- function(input, output, session, globalDB) {

  ns <- NS("dr_analysis")
  #show the table from joint_rhythm
  output$nTOJR <- DT::renderDataTable({
    # if(!is.null(db$TOJRsummary)){
    # DT::datatable(db$TOJRsummary, selection = 'single')
    # }
    if(nrow(globalDB$TOJRsummary)>0){
      DT::datatable({globalDB$TOJRsummary}, selection = 'single')
    }
    })

  ##########################
  # Reactive Values        #
  ##########################
  intDB <- reactiveValues(TOJR = list(),
                          TOJRsummary = data.frame(NULL),
                          printDRP = FALSE,
                          printDRF = FALSE,
                          dA = FALSE, dPhase = FALSE, dM = FALSE,
                          trigger = 0)
  SUMMARY <- reactiveValues(DRP=data.frame(NULL),
                            DRF=data.frame(NULL))

  ##########################
  # Observers              #
  ##########################
  # select the criteria for TOJR

  observeEvent(input$startDRP, {
    if(!is.null(input$nTOJR_rows_selected)){
      if(globalDB$studytype == "One"){
        sendErrorMessage(session, "Differential rhythmicity parameter test is not available for one group analysis. ")
      }else{
        # print("DEBUG entered startDRP")
        # TOJR.sel = db$TOJR[[input$nTOJR_rows_selected]]
        # res.DRP <<- DiffCircaPipeline::DCPDiffPar(CP.obj, Par = input$DRP_par, TOJR = readRDS(paste0(db$working.dir, "/", db$studyname,  "/save/TOJR_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue, ".rds"))$TOJR,
        #                                           alpha = input$DRP_criticalValue, parallel.ncores = input$DR_parallel.ncores)
        # suppressWarnings(saveX(res.DRP, db, info = paste0("DR_parameter", "_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue, ".rds")))
        # print("selected row is: ")
        # print(input$nTOJR_rows_selected)
        intDB <- update_rvDB(intDB, globalDB, type = "match")
        # # print(intDB$TOJR)
        # print(paste0("Length of intDB$TOJR is ", length(intDB$TOJR)))
        # print(paste0("Length of globalDB$TOJR is ", length(globalDB$TOJR)))
        # print(paste0("Length of db$TOJR is ", length(db$TOJR)))
        # print(intDB$TOJR[[input$nTOJR_rows_selected]])
        # print(globalDB$TOJR[[input$nTOJR_rows_selected]])
        # if(is.null(input$nTOJR_rows_selected)){
        #   #sendErrorMessage()
        # }#later
        TOJR.sel <- intDB$TOJR[[input$nTOJR_rows_selected]]
        TOJR.sel.load <- readRDS(paste0(globalDB$working.dir,  "/save/TOJR_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue, ".rds"))
        a.Try <-  Try({
          res.DRP <<- DiffCircaPipeline::DCP_DiffPar(CP.obj, Par = input$DRP_par, TOJR = TOJR.sel.load$TOJR[match(CP.obj$gname_overlap, TOJR.sel.load$TOJR$gname), "TOJR"],
                                                    alpha = input$DRP_criticalValue, parallel.ncores = input$DR_parallel.ncores)
        }, session)
        intDB$TOJR <- updateTOJR(intDB$TOJR, input$nTOJR_rows_selected, "DRP")
        intDB <- updateTOJRsummary(intDB, "DPP")
        suppressWarnings(saveX(res.DRP, globalDB, info = paste0("DR_parameter", "_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue, ".rds")))
        writeCPoutput(res.DRP, globalDB, "DRP", TOJR.sel)
        # print("1. run CP_DiffPar")
        SUMMARY$DRP = res.DRP #might change it later.
        # db$printDRP <<- TRUE
        # if(grepl("A", input$DRP_par)){
        #   db$dA <<- TRUE
        # }
        # if(grepl("phase", input$DRP_par)){
        #   db$dPhase <<- TRUE
        # }
        # if(grepl("M", input$DRP_par)){
        #   db$dM <<- TRUE
        # }
        intDB$printDRP <- TRUE
        if(grepl("A", input$DRP_par)){
          intDB$dA <- TRUE
        }
        if(grepl("phase", input$DRP_par)){
          intDB$dPhase <- TRUE
        }
        if(grepl("M", input$DRP_par)){
          intDB$dM <- TRUE
        }
        intDB$trigger <- intDB$trigger + 1
      }
    }else{
      showNotification(ui = "Please select a cutoff from TOJR list",
                       type = "error",
                       duration = 3)
    }

  })

  observeEvent(input$startDRF, {
    if(!is.null(input$nTOJR_rows_selected)){
      if(db$studytype == "One"){
        sendErrorMessage(session, "Differential rhythmicity parameter test is not available for one group analysis. ")
      }else{
        # print("selected row is: ")
        # print(input$nTOJR_rows_selected)
        intDB <- update_rvDB(intDB, globalDB, type = "match")
        # if(is.null(input$nTOJR_rows_selected)){
        #   #sendErrorMessage()
        # }#later
        TOJR.sel = db$TOJR[[input$nTOJR_rows_selected]]
        TOJR.sel.load <- readRDS(paste0(globalDB$working.dir,  "/save/TOJR_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue, ".rds"))
        res.DRF <<- DiffCircaPipeline::DCP_DiffR2(CP.obj, method = input$DRR_method, TOJR = TOJR.sel.load$TOJR[match(CP.obj$gname_overlap, TOJR.sel.load$TOJR$gname), "TOJR"],
                                                 nSampling = input$DRF_nsamp, parallel.ncores = input$DR_parallel.ncores)
        intDB$TOJR <- updateTOJR(intDB$TOJR, input$nTOJR_rows_selected, "DRF")
        intDB <- updateTOJRsummary(intDB, "DPF")
        suppressWarnings(saveX(res.DRF, globalDB, info = paste0("DR_fitness", "_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue, ".rds")))
        writeCPoutput(res.DRF, globalDB, "DRF", TOJR.sel)
        # print("2. run CP_DiffFit")
        SUMMARY$DRF = res.DRF #might change it later.
        # db$printDRF <<- TRUE
        intDB$printDRF <- TRUE
        intDB$trigger <- intDB$trigger + 1
      }
    }else{
      showNotification(ui = "Please select a cutoff from TOJR list",
                       type = "error",
                       duration = 3)
    }

  })

  ##########################
  # Render output/UI       #
  ##########################
  output$DRP <- DT::renderDataTable({
    if(nrow(SUMMARY$DRP)>0){
      DT::datatable({round2(SUMMARY$DRP, 3)}, rownames = FALSE)
    }
  })
  output$DRF <- DT::renderDataTable({
    if(nrow(SUMMARY$DRF)>0){
      DT::datatable({round2(SUMMARY$DRF, 3)}, rownames = FALSE)
    }
  })

  return(intDB)
}
