dr_fit_server <- function(input, output, session, globalDB) {

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
                          # printDRP = FALSE,
                          printDRF = FALSE,
                          # dA = FALSE, dPhase = FALSE, dM = FALSE,
                          trigger = 0)
  SUMMARY <- reactiveValues(DRF=data.frame(NULL))

  ##########################
  # Observers              #
  ##########################
  # select the criteria for TOJR

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
  output$DRF <- DT::renderDataTable({
    if(nrow(SUMMARY$DRF)>0){
      DT::datatable({round2(SUMMARY$DRF, 3)}, rownames = FALSE)
    }
  })

  return(intDB)
}
