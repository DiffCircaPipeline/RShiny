dr_param_server <- function(input, output, session, globalDB) {

  ns <- NS("dr_param")
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
                          dA = FALSE, dPhase = FALSE, dM = FALSE,
                          trigger = 0)
  SUMMARY <- reactiveValues(DRP=data.frame(NULL))

  ##########################
  # Observers              #
  ##########################
  # select the criteria for TOJR

  observeEvent(input$startDRP, {
    # print("DEBUG entered startDRP0")
    if(!is.null(input$nTOJR_rows_selected)){
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
      TOJR.sel.load <- readRDS(paste0(globalDB$working.dir,  "/save/TOJR_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue, "_Amp", TOJR.sel$ampCut, ".rds"))
      a.Try <-  Try({
        res.DRP <<- DiffCircaPipeline::DCP_DiffPar(CP.obj, Par = input$DRP_par, TOJR = TOJR.sel.load$TOJR[match(CP.obj$gname_overlap, TOJR.sel.load$TOJR$gname), "TOJR"],
                                                   alpha = input$DRP_criticalValue, parallel.ncores = input$DR_parallel.ncores)
      }, session)
      print("Differential rhythm parameter test finished. ")
      intDB$TOJR <- updateTOJR(intDB$TOJR, input$nTOJR_rows_selected, "DRParameter")
      intDB <- updateTOJRsummary(intDB, "DRParameter")
      suppressWarnings(saveX(res.DRP, globalDB, info = paste0("DR_parameter", "_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue,  "_Amp", TOJR.sel$ampCut, ".rds")))
      writeCPoutput(res.DRP, globalDB, "DRP", TOJR.sel, input$DRP_par)
      print(paste0("Differential rhythm parameter test saved to ", db$working.dir))
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
        intDB$TOJR <- updateTOJR(intDB$TOJR, input$nTOJR_rows_selected, "dA")
      }
      if(grepl("phase", input$DRP_par)){
        intDB$dPhase <- TRUE
        intDB$TOJR <- updateTOJR(intDB$TOJR, input$nTOJR_rows_selected, "dPhase")
      }
      if(grepl("M", input$DRP_par)){
        intDB$dM <- TRUE
        intDB$TOJR <- updateTOJR(intDB$TOJR, input$nTOJR_rows_selected, "dM")
      }
      intDB$trigger <- intDB$trigger + 1
    }else{
      showNotification(ui = "Please select a cutoff from summary of TOJR table",
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

  output$DR_param_ui <- renderUI({
    if(globalDB$studytype == "One"){
      output <- tagList()
      output[[1]] <- h2("Differential parameter tests")
      output[[2]] <- h3("Not applicable. ")
    }else if(globalDB$studytype == "Two"){
      output <- tagList()
      output[[1]] <- h2("Differential Parameter tests")
      output[[2]] <- numericInput(ns("DR_parallel.ncores"), "Number of cores for computation:", value = 1, min = 1,
                                  max = ifelse(.Platform$OS.type != "windows", parallel::detectCores(), 1))
      output[[3]] <- selectInput(ns("DRP_par"), "Parameters to test",
                                 choices = c("amplitude" = "A",
                                             "phase" = "phase",
                                             "MESOR" = "M",
                                             "amplitude and phase" = "A&phase",
                                             "amplitude and phase and MESOR" = "A&phase&M"),
                                 selected = "phase")
      output[[4]] <- numericInput(ns("DRP_criticalValue"), label = "Critical value", value = 0.05, min = 0, max = 1)
      output[[5]] <- actionButton(ns("startDRP"), 'DR parameter analysis', icon=icon("play"), class="btn-success")
    }
    output
  })
  return(intDB)

}
