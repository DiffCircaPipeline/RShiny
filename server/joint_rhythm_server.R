joint_rhythm_server <- function(input, output, session, globalDB) {
  ns <- NS("joint_rhythm")

  # First analysis
  TOJRsummary <- reactiveValues(TOJR = list(),
                                TOJRsummary = data.frame(NULL),
                                printCP.obj = FALSE,
                                Rhythm1 = data.frame(NULL),
                                Rhythm2 = data.frame(NULL),
                                runType = NULL,
                                trigger = 0)

  observeEvent(input$startTOJR, {
    # print("1. in")
    # TOJRsummary$printCP.obj <- globalDB$printCP.obj
    if(globalDB$printCP.obj){
      showNotification(
        ui = paste0("The analysis has been run before. Current run will overwrite previous results. "),
        type = "warning",
        duration = 5
      )
      warning("The analysis has been run before. Current run will overwrite previous results. ")
    }
    TOJRsummary$runType = ifelse(is.null(CP.obj[[2]]), "one", "two")
    # print("CP.obj[[1]]")
    # print(CP.obj[[1]])
    # print("CP.obj[[2]]")
    # print(CP.obj[[2]])
    CP.obj <<- DiffCircaPipeline::DCP_Rhythmicity(x1 = CP.obj[[1]], x2 = CP.obj[[2]],
                                               method = input$TOJRmethod, period = input$rhythmPeriod,
                                               alpha = input$criticalValue, alpha.FDR = input$criticalValueFDR,
                                               CI = input$outputCI, parallel.ncores = input$parallel.ncores)
    suppressWarnings(saveX(CP.obj, db))
    writeCPoutput(CP.obj, db, ifelse(is.null(CP.obj[[2]]), "est1", "est2"))
    rhythm.joint.run1 = CP.obj$rhythm.joint
    rhythm.joint.run1.p = rhythm.joint.run1[, -c(which(colnames(rhythm.joint.run1)=="TOJR.FDR"))]
    rhythm.joint.run1.q = rhythm.joint.run1
    rhythm.joint.run1.q$TOJR = rhythm.joint.run1.q$TOJR.FDR
    rhythm.joint.run1.q = rhythm.joint.run1.q[, -c(which(colnames(rhythm.joint.run1)=="TOJR.FDR"))]
    suppressWarnings(saveX(list(TOJR = rhythm.joint.run1.p,
                                method = input$TOJRmethod,
                                alpha = input$criticalValue,
                                adjustP = FALSE), db, info = paste0("TOJR", "_", input$TOJRmethod, "_", "p-value", "_", input$criticalValue, ".rds")))
    suppressWarnings(saveX(list(TOJR = rhythm.joint.run1.q,
                                method = input$TOJRmethod,
                                alpha = input$criticalValue,
                                adjustP = TRUE), db, info = paste0("TOJR", "_", input$TOJRmethod, "_", "q-value", "_", input$criticalValueFDR, ".rds")))
    TOJRsummary$TOJR <- list(list(method = input$TOJRmethod,
                                  cutoffType = "p-value",
                                  cutoffValue = input$criticalValue,
                                  DRP = "N",
                                  DRF = "N"),
                             list(method = input$TOJRmethod,
                                  cutoffType = "q-value",
                                  cutoffValue = input$criticalValueFDR,
                                  DRP = "N",
                                  DRF = "N"))
    TOJRsummary$printCP.obj <- TRUE
    # db$TOJR <<- list(list(method = input$TOJRmethod,
    #                       cutoffType = "p-value",
    #                       cutoffValue = input$criticalValue),
    #                  list(method = input$TOJRmethod,
    #                       cutoffType = "q-value",
    #                       cutoffValue = input$criticalValueFDR))
    # db$printCP.obj <<- TRUE

    # print("1. run CP_Rhythmicity")
    if(TOJRsummary$runType == "one"){
      TOJRsummary$Rhythm1 <- CP.obj[[1]]$rhythm
      TOJRsummary$TOJRsummary <- summary.TOJR(CP.obj[[1]]$rhythm, type = "one", method = input$TOJRmethod,
                                        alpha = input$criticalValue, alpha.FDR = input$criticalValueFDR, alpha.type = "p-value")
      # db <<-
    }else{
      TOJRsummary$Rhythm1 <- CP.obj[[1]]$rhythm
      TOJRsummary$Rhythm2 <- CP.obj[[2]]$rhythm
      TOJRsummary$TOJRsummary <- summary.TOJR(CP.obj$rhythm.joint, type = "two_FirstTime", alpha = input$criticalValue, alpha.FDR = input$criticalValueFDR, alpha.type = "p-value")
      # db <<-
    }
    TOJRsummary$trigger <- TOJRsummary$trigger + 1
  })

  observeEvent(input$startTOJR2, {
    if((!TOJRsummary$printCP.obj)&(!globalDB$printCP.obj)){
      sendErrorMessage(session, "Please run the Joint Rhythmicity test first before trying new cutoffs. ")
    }else{
      newTOJR = DiffCircaPipeline::toTOJR(CP.obj, method = input$newTOJRmethod, alpha = input$newCriticalValue, adjustP = ifelse(input$newTOJRcutType == "q-value", TRUE, FALSE),
                       p.adjust.method = "BH", parallel.ncores = input$parallel.ncores)
      # print("2. run toTOJR")
      # print(newTOJR)
      newTOJR = list(TOJR = data.frame(gname = CP.obj$gname_overlap, TOJR = newTOJR),
                     method = input$newTOJRmethod,
                     alpha = input$newCriticalValue,
                     adjustP = ifelse(input$newTOJRcutType == "q-value", TRUE, FALSE))
      suppressWarnings(saveX(newTOJR, db, info = paste0("TOJR", "_", input$newTOJRmethod, "_", input$newTOJRcutType, "_", input$newCriticalValue, ".rds")))
      # db$TOJR <<- append(db$TOJR,
      #                    list(method = input$newTOJRmethod,
      #                         cutoffType = input$newTOJRcutType,
      #                         cutoffValue = input$newCriticalValue))
      TOJRsummary$TOJR <- append(TOJRsummary$TOJR, list(list(method = input$newTOJRmethod,
                                                             cutoffType = input$newTOJRcutType,
                                                             cutoffValue = input$newCriticalValue,
                                                             DRP = "N",
                                                             DRF = "N"))
                         )
      TOJRsummary$TOJRsummary <- rbind(TOJRsummary$TOJRsummary,
                                 summary.TOJR(newTOJR$TOJR, method = input$newTOJRmethod, type = "two_more",
                                              alpha = input$newCriticalValue, alpha.type = input$newTOJRcutType))
      TOJRsummary$trigger <- TOJRsummary$trigger + 1
      # db$TOJRsummary <<- TOJRsummary$TOJRsummaryc

    }
  })

  output$Rhythm1 <- DT::renderDataTable({
    if(nrow(TOJRsummary$Rhythm1)>0){
      DT::datatable({TOJRsummary$Rhythm1})
    }
  })
  output$Rhythm2 <- DT::renderDataTable({
    if(nrow(TOJRsummary$Rhythm2)>0){
      DT::datatable({TOJRsummary$Rhythm2})
    }
  })
  output$RhythmEst <- renderUI({
    if(rvDB$printCP.obj&rbDB$studytype == "Two"){
      tabPanel("Parameter estimates",
               fluidRow(width = 12,
                        column(6, h5(rvDB$gIinfo), DT::dataTableOutput(ns("Rhythm1"), width = "auto")),
                        column(6, h5(rvDB$gIinfo), DT::dataTableOutput(ns("Rhythm2"), width = "auto"))
               )
      )
    }else if(rvDB$printCP.obj&rbDB$studytype == "One"){
      tabPanel("Parameter estimates",
               h4(rvDB$studyname),
               DT::dataTableOutput(ns("Rhythm1"))
      )
    }else{
      p("No estimation has been performed. Please do so in the Joint rhythm module. ")
    }
  })

  # if(nrow(TOJRsummary$Rhythm1)>0){
  #   DT::datatable({TOJRsummary$Rhythm1})
  # }

  output$TOJRsummary <- DT::renderDataTable({
    # if(nrow(TOJRsummary$TOJRsummary)>0){
    #   DT::datatable({TOJRsummary$TOJRsummary})
    # }
    if(nrow(globalDB$TOJRsummary)>0){
      DT::datatable({globalDB$TOJRsummary})
    }
  })
  return(TOJRsummary)
}

