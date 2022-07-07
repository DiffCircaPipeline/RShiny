dr_fit_server <- function(input, output, session, globalDB) {

  ns <- NS("dr_fit")
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
        # print("DEBUG: in startDRF")
        # print("selected row is: ")
        # print(input$nTOJR_rows_selected)
        intDB <- update_rvDB(intDB, globalDB, type = "match")
        # print("DEBUG: DRF 1")
        # if(is.null(input$nTOJR_rows_selected)){
        #   #sendErrorMessage()
        # }#later
        TOJR.sel = db$TOJR[[input$nTOJR_rows_selected]]
        TOJR.sel.load <- readRDS(paste0(globalDB$working.dir,  "/save/TOJR_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue, "_Amp", TOJR.sel$ampCut, ".rds"))
        # print(input$DRF_nsamp)
        # print(input$DR_parallel.ncores)
        res.DRF <<- DiffCircaPipeline::DCP_DiffR2(CP.obj, method = input$DRR_method, TOJR = TOJR.sel.load$TOJR[match(CP.obj$gname_overlap, TOJR.sel.load$TOJR$gname), "TOJR"],
                                                 nSampling = input$DRF_nsamp, parallel.ncores = input$DR_parallel.ncores)
        # print("DEBUG: DRF 4")
        print("Differential rhythm fitness test finished. ")
        intDB$TOJR <- updateTOJR(intDB$TOJR, input$nTOJR_rows_selected, "DRFitness")
        intDB <- updateTOJRsummary(intDB, "DRFitness")
        # print("DEBUG: DRF 5")
        suppressWarnings(saveX(res.DRF, globalDB, info = paste0("DR_fitness", "_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue,  "_Amp", TOJR.sel$ampCut, ".rds")))
        writeCPoutput(res.DRF, globalDB, "DRF", TOJR.sel, input$DRR_method)
        print(paste0("Differential rhythm fitness test saved to ", db$working.dir))
        # print("2. run CP_DiffFit")
        SUMMARY$DRF = res.DRF #might change it later.
        # db$printDRF <<- TRUE
        intDB$printDRF <- TRUE
        intDB$trigger <- intDB$trigger + 1
        # print("DEBUG: DRF 6")
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

  output$DR_fit_ui <- renderUI({
    if(globalDB$studytype == "One"){
      output <- tagList()
      output[[1]] <- h2("Differential fitness test")
      output[[2]] <- h3("Not applicable. ")
    }else if(globalDB$studytype == "Two"){
      output <- tagList()
      output[[1]] <- h2("Differential fitness test")
      output[[2]] <- numericInput(ns("DR_parallel.ncores"), "Number of cores for computation:", value = 1, min = 1,
                                  max = ifelse(.Platform$OS.type != "windows", parallel::detectCores(), 1))
      output[[3]] <- selectInput(ns("DRR_method"), HTML("Select method for differential R<sup>2</sup> test"),
                                 choices = c("likelihood ratio test (Default)" = "LR",
                                             "permutation" = "permutation",
                                             "bootstrap" = "bootstrap"),
                                 selected = "likelihood ratio test (Default)")
      output[[4]] <- conditionalPanel(
        condition = paste0("input['", ns("DRR_method"), "'] != 'LR' "),
        numericInput(ns("DRF_nsamp"), label = "number of samplings", value = 1000, min = 10)
        #leave the rest empty first ...
      )
      output[[5]] <- actionButton(ns('startDRF'), 'DR fitness analysis', icon=icon("play"), class="btn-success")
    }
    output
  })
  return(intDB)
}
