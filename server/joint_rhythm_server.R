joint_rhythm_server <- function(input, output, session, globalDB) {
  ns <- NS("joint_rhythm")

  # First analysis
  TOJRsummary <- reactiveValues(TOJR = list(),
                                TOJRsummary = data.frame(NULL),
                                printCP.obj = FALSE,
                                Rhythm1 = data.frame(NULL),
                                Rhythm2 = data.frame(NULL),
                                ampCut = 0,
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

    # print("1. run CP_Rhythmicity")
    if(globalDB$studytype == "One"){
      if(c("rhythm")%in%names(CP.obj)){
        CP.obj <<- DiffCircaPipeline::DCP_Rhythmicity(x1 = CP.obj, x2 = NULL,
                                                      method = input$TOJRmethod, period = input$rhythmPeriod,
                                                      amp.cutoff = input$ampCut,
                                                      alpha = input$criticalValue, alpha.FDR = input$criticalValueFDR,
                                                      CI = input$outputCI, parallel.ncores = input$parallel.ncores)
      }else{
        CP.obj <<- DiffCircaPipeline::DCP_Rhythmicity(x1 = CP.obj[[1]], x2 = CP.obj[[2]],
                                                      method = input$TOJRmethod, period = input$rhythmPeriod,
                                                      amp.cutoff = input$ampCut,
                                                      alpha = input$criticalValue, alpha.FDR = input$criticalValueFDR,
                                                      CI = input$outputCI, parallel.ncores = input$parallel.ncores)
      }
      print("One group rhythmicity analysis finished. ")
      suppressWarnings(saveX(CP.obj, db))
      writeCPoutput(CP.obj, db, "est1")
      print(paste0("One group rhythmicity analysis result saved to ", db$working.dir))
      TOJRsummary$printCP.obj <- TRUE
      TOJRsummary$Rhythm1 <- CP.obj$rhythm
      TOJRsummary$TOJRsummary <- summary.TOJR(CP.obj, type = "one", amp.cutoff = input$ampCut,
                                        alpha = input$criticalValue,  alpha.type = "p-value")
    }else{
      CP.obj <<- DiffCircaPipeline::DCP_Rhythmicity(x1 = CP.obj[[1]], x2 = CP.obj[[2]],
                                                    method = input$TOJRmethod, period = input$rhythmPeriod,
                                                    amp.cutoff = input$ampCut,
                                                    alpha = input$criticalValue, alpha.FDR = input$criticalValueFDR,
                                                    CI = input$outputCI, parallel.ncores = input$parallel.ncores)
      print("Two-group rhythmicity and joint rhythmicity analysis finished. ")
      suppressWarnings(saveX(CP.obj, db))

      writeCPoutput(CP.obj, db, "est2")
      print(paste0("Two-group rhythmicity and joint rhythmicity result saved to ", db$working.dir))

      TOJRsummary$printCP.obj <- TRUE

      TOJRsummary$Rhythm1 <- CP.obj[[1]]$rhythm
      TOJRsummary$Rhythm2 <- CP.obj[[2]]$rhythm
      TOJRsummary$TOJRsummary <- summary.TOJR(CP.obj, method = input$TOJRmethod, type = "two_FirstTime",amp.cutoff = input$ampCut,
                                              alpha = input$criticalValue, alpha.FDR = input$criticalValueFDR, alpha.type = "p-value")

      rhythm.joint.run1 = CP.obj$rhythm.joint
      rhythm.joint.run1.p = rhythm.joint.run1[, -c(which(colnames(rhythm.joint.run1)=="TOJR.FDR"))]
      rhythm.joint.run1.q = rhythm.joint.run1
      rhythm.joint.run1.q$TOJR = rhythm.joint.run1.q$TOJR.FDR
      rhythm.joint.run1.q = rhythm.joint.run1.q[, -c(which(colnames(rhythm.joint.run1)=="TOJR.FDR"))]
      suppressWarnings(saveX(list(TOJR = rhythm.joint.run1.p,
                                  method = input$TOJRmethod,
                                  alpha = input$criticalValue,
                                  adjustP = FALSE), db, info = paste0("TOJR", "_", input$TOJRmethod, "_", "p-value", "_", input$criticalValue, "_Amp", input$ampCut, ".rds")))
      writeTOJR(rhythm.joint.run1.p, db, info = paste0(input$TOJRmethod, "_", "p-value", "_", input$criticalValue, "_Amp", input$newAmpCut))
      suppressWarnings(saveX(list(TOJR = rhythm.joint.run1.q,
                                  method = input$TOJRmethod,
                                  alpha = input$criticalValue,
                                  adjustP = TRUE), db, info = paste0("TOJR", "_", input$TOJRmethod, "_", "q-value", "_", input$criticalValueFDR, "_Amp", input$newAmpCut, ".rds")))
      writeTOJR(rhythm.joint.run1.q, db, info = paste0(input$TOJRmethod, "_", "q-value", "_", input$criticalValue, "_Amp", input$newAmpCut))
      TOJRsummary$TOJR <- list(list(method = input$TOJRmethod,
                                    ampCut = input$ampCut,
                                    cutoffType = "p-value",
                                    cutoffValue = input$criticalValue,
                                    DRParameter = "N",
                                    dA = "N",
                                    dPhase = "N",
                                    dM = "N",
                                    DRFitness = "N"),
                               list(method = input$TOJRmethod,
                                    ampCut = input$ampCut,
                                    cutoffType = "q-value",
                                    cutoffValue = input$criticalValueFDR,
                                    DRParameter = "N",
                                    dA = "N",
                                    dPhase = "N",
                                    dM = "N",
                                    DRFitness = "N"))
    }
    TOJRsummary$ampCut <- input$ampCut #?
    TOJRsummary$trigger <- TOJRsummary$trigger + 1
  })

  observeEvent(input$startTOJR2, {
    if((!TOJRsummary$printCP.obj)&(!globalDB$printCP.obj)){
      sendErrorMessage(session, "Please run the Joint Rhythmicity test first before trying new cutoffs. ")
    }else{
      if(globalDB$studytype == "One"){
        TOJRsummary$TOJRsummary <- rbind(TOJRsummary$TOJRsummary,
                                         summary.TOJR(CP.obj, type = "one_more", amp.cutoff = input$newAmpCut, alpha = input$newCriticalValue,  alpha.type = input$newTOJRcutType))
        # print("DEBUG: one study TOJRsummary")
        # print(table(CP.obj$rhythm$pvalue<0.05))
      }else if(globalDB$studytype == "Two"){
        newTOJR = DiffCircaPipeline::toTOJR(CP.obj, method = input$newTOJRmethod, amp.cutoff = input$newAmpCut, alpha = input$newCriticalValue, adjustP = ifelse(input$newTOJRcutType == "q-value", TRUE, FALSE),
                                            p.adjust.method = "BH", parallel.ncores = input$parallel.ncores)
        # print("2. run toTOJR")
        # print(newTOJR)
        newTOJR = list(TOJR = data.frame(gname = CP.obj$gname_overlap, TOJR = newTOJR),
                       method = input$newTOJRmethod,
                       alpha = input$newCriticalValue,
                       adjustP = ifelse(input$newTOJRcutType == "q-value", TRUE, FALSE))
        suppressWarnings(saveX(newTOJR, db, info = paste0("TOJR", "_", input$newTOJRmethod, "_", input$newTOJRcutType, "_", input$newCriticalValue, "_Amp", input$newAmpCut, ".rds")))
        writeTOJR(data.frame(CP.obj$rhythm.joint[, 1:5], TOJR = newTOJR$TOJR$TOJR), db, info = paste0(input$newTOJRmethod, "_", input$newTOJRcutType, "_", input$newCriticalValue, "_Amp", input$newAmpCut))
        print(paste0("Additional joint rhythmicity result saved to ", db$working.dir))
        # db$TOJR <<- append(db$TOJR,
        #                    list(method = input$newTOJRmethod,
        #                         cutoffType = input$newTOJRcutType,
        #                         cutoffValue = input$newCriticalValue))
        TOJRsummary$TOJR <- append(TOJRsummary$TOJR, list(list(method = input$newTOJRmethod,
                                                               cutoffType = input$newTOJRcutType,
                                                               cutoffValue = input$newCriticalValue,
                                                               ampCut = input$ampCut,
                                                               DRParameter = "N",
                                                               dA = "N",
                                                               dPhase = "N",
                                                               dM = "N",
                                                               DRFitness = "N"))
        )
        TOJRsummary$TOJRsummary <- rbind(TOJRsummary$TOJRsummary,
                                         summary.TOJR(newTOJR, method = input$newTOJRmethod, type = "two_more", amp.cutoff = input$newAmpCut,
                                                      alpha = input$newCriticalValue, alpha.type = input$newTOJRcutType))
      }
      TOJRsummary$trigger <- TOJRsummary$trigger + 1
      # db$TOJRsummary <<- TOJRsummary$TOJRsummaryc

    }
  })

  output$Rhythm1 <- DT::renderDataTable({if(globalDB$printCP.obj&globalDB$studytype == "Two"){
    DT::datatable(round2(CP.obj[[1]]$rhythm, 3), options = list(scrollX = TRUE), rownames = FALSE)
  }else if(globalDB$printCP.obj&globalDB$studytype == "One"){
    DT::datatable(round2(CP.obj$rhythm, 3), options = list(scrollX = TRUE), rownames = FALSE)
  }})
  output$Rhythm2 <- DT::renderDataTable({if(globalDB$printCP.obj&globalDB$studytype == "Two"){
    DT::datatable(round2(CP.obj[[2]]$rhythm, 3), options = list(scrollX = TRUE), rownames = FALSE)
  }})

  output$TOJRandRhythmEst <- renderUI({
    if(globalDB$printCP.obj&globalDB$studytype == "Two"){
      output = tagList()
      output[[1]] <-
        tabsetPanel(
          tabPanel("Summary of TOJR", DT::dataTableOutput(ns("TOJRsummary"))),
          tabPanel("Parameter estimates",
                   fluidRow(width = 12,
                            column(6, h5(globalDB$gIinfo), DT::dataTableOutput(ns("Rhythm1"), width = "auto")),
                            column(6, h5(globalDB$gIIinfo), DT::dataTableOutput(ns("Rhythm2"), width = "auto"))
                   )
          )
        )

    }else if(globalDB$printCP.obj&globalDB$studytype == "One"){
      output = tagList()
      output[[1]] <-
        tabsetPanel(
          tabPanel("Summary of TOJR", DT::dataTableOutput(ns("TOJRsummary"))),
          tabPanel("Parameter estimates",
                   h4(globalDB$studyname),
                   DT::dataTableOutput(ns("Rhythm1"))
          )
        )

    }else{
      output = tagList()
      output[[1]] <-
      p("No estimation has been performed. ")
    }
    output
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

  #UI output
  #UI
  output$UIStudyOneOrTwo <- renderUI({
    if(globalDB$studytype == "One"){
      output <- tagList()
      output[[1]] <- h3("Set parameters: ");
      #### choose parameters for analysis
      output[[2]] <- checkboxInput(ns("outputCI"), label = "Output confidence interval (CI)?", value = FALSE);
      output[[3]] <- numericInput(ns("rhythmPeriod"), label = "Period", value = 24, min = 1, step = 1);
      #remember to add that if it is single study analysis, the critical value is only used for CI coverage
      output[[4]] <- numericInput(ns("ampCut"), label = "Amplitude cutoff", value = 0, min = 0);
      output[[5]] <- numericInput(ns("criticalValue"), label = "Critical value", value = 0.05, min = 0, max = 1, step = 0.05);
      output[[6]] <- actionButton(ns('startTOJR'), 'Start analysis', icon=icon("play"), class="btn-success");

      output[[7]] <- h3("Try other cutoffs");
      output[[8]] <- selectInput(ns("newTOJRcutType"), "type of cutoff:",
                                  choices = c("p-value" = "p-value",
                                              "q-value" = "q-value"));
      output[[9]] <- numericInput(ns("newCriticalValue"), label = "Critical value", value = 0.05, min = 0, max = 1, step = 0.05);
      output[[10]] <- actionButton(ns('startTOJR2'), 'Try the new cutoff', icon=icon("play"), class="btn-success")

    }else if(globalDB$studytype == "Two"){
      output <- tagList()
      output[[1]] <- h3("Set parameters: ");
      #### choose parameters for analysis
      output[[2]] <- checkboxInput(ns("outputCI"), label = "Output confidence interval (CI)?", value = FALSE);
      output[[3]] <- numericInput(ns("rhythmPeriod"), label = "Period", value = 24, min = 1, step = 1);
      #remember to add that if it is single study analysis, the critical value is only used for CI coverage
      output[[4]] <- numericInput(ns("ampCut"), label = "Amplitude cutoff", value = 0, min = 0);
      output[[5]] <- numericInput(ns("criticalValue"), label = "Critical value", value = 0.05, min = 0, max = 1, step = 0.05);
      output[[6]] <- numericInput(ns("criticalValueFDR"), label = "FDR", value = 0.05, min = 0, max = 1, step = 0.05);
      output[[7]] <- selectInput(ns("TOJRmethod"), "Method for joint rhythmicity categorization:",
                  choices = c("Sidak_FS" = "Sidak_FS", #left is shown in UI and right is the value for function.
                              "Sidak_BS" = "Sidak_BS",
                              "VDA" = "VDA",
                              "AWFisher" = "AWFisher"),
                  selected = "Sidak_FS");
      output[[8]] <- numericInput(ns("parallel.ncores"), "Number of cores for computation:", value = 1, min = 1,
                   max = ifelse(.Platform$OS.type != "windows", parallel::detectCores(), 1));
      output[[9]] <- actionButton(ns('startTOJR'), 'Start analysis', icon=icon("play"), class="btn-success");

      output[[10]] <- h3("Try other cutoffs");
      output[[11]] <- numericInput(ns("newAmpCut"), label = "Amplitude cutoff", value = 0, min = 0);
      output[[12]] <- selectInput(ns("newTOJRcutType"), "type of cutoff:",
                  choices = c("p-value" = "p-value",
                              "q-value" = "q-value"));
      output[[13]] <- selectInput(ns("newTOJRmethod"), "Method for joint rhythmicity categorization:",
                  choices = c("Sidak_FS" = "Sidak_FS",
                              "Sidak_BS" = "Sidak_BS",
                              "VDA" = "VDA",
                              "AWFisher" = "AWFisher"),
                  selected = "Sidak_FS");
      output[[14]] <- numericInput(ns("newCriticalValue"), label = "Critical value", value = 0.05, min = 0, max = 1, step = 0.05);
      output[[15]] <- actionButton(ns('startTOJR2'), 'Try the new cutoff', icon=icon("play"), class="btn-success")
    }
    output
  })

  return(TOJRsummary)
}

