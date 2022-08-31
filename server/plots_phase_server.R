plots_phase_server <- function(input, output, session, globalDB) {

  ns <- NS("plots_phase")

  # ##########################
  # # Reactive Values        #
  # ##########################
  UI_server_com <- reactiveValues(renderPlotType2 = NULL,
                                  output = NULL)

  a.plot <- reactiveValues(p = NULL)

  # observeEvent(db$studytype, {
  #   if(db$studytype=="One"){
  #     print(paste0("db$studytype=='one'"))
  #   }else{
  #     print(paste0("db$studytype=='two'"))
  #   }
  # })

  output$UIStudyColorByDPhase <-renderUI({
    if(globalDB$studytype == "One"){
      output <- tagList()
      output[[1]] <- h3("Circos phase difference plot is not applicable to one-group study. ")
    }else if(globalDB$studytype == "Two"){
      output <- tagList()
      if(globalDB$dPhase == TRUE){
        output[[1]] <- h3("Color specification")
        output[[2]] <- checkboxInput(ns("diffColorCut"), "Different color for genes by differential phase result?", FALSE)

        }else{
        output[[1]] <- h5("Choose a TOJR cutoff from main panel. ")

      }
    }
    output
    })

  output$UIStudyOthers <- renderUI({
    if(globalDB$studytype == "One"){
      output <- tagList()
      output[[1]] <- br()
    }else if(globalDB$studytype == "Two"){
      output <- tagList()
      if(globalDB$dPhase == TRUE&input$diffColorCut){
          output[[1]] <- fluidRow(width = 12,
                          column(4,uiOutput(ns("diffsigCutParamColumns"))),
                          column(4,selectInput(ns("diffsigCutFun"), "direction ",
                      c("<" = "<",
                        ">" = ">", 
                        "=" = "="))),
                          column(4, numericInput(ns("diffsigCutValue"), "Cutoff value: ", 0.05))
                          )
          output[[2]] <- fluidRow(width = 12,
                          column(6, colourpicker::colourInput(ns("diffColorPointSig"), "Select color for genes that pass the criterion: ", "#03a1fc")),
                          column(6, colourpicker::colourInput(ns("diffColorPointNone"), "Select color for genes that do not pass the criterion: ", "#545454"))
                          )
      }
      output[[3]] <- h3("More settings: ")
      output[[4]] <- fluidRow(width = 12,
                          column(6, numericInput(ns("diffTimeStart"), "Starting Time: ", -6)),
                          column(6, numericInput(ns("diffTimeBandWidth"), "Band width around 0: ", 4))
                          )
      output[[5]] <- fluidRow(width = 12,
                          column(6, numericInput(ns("Xstart"), "Radius grid starts at: ", -12)),                     
                          column(6, numericInput(ns("diffX"), "Radius grid jumps at: ", 4))
                          )
      output[[6]] <- fluidRow(width = 12,
                          column(6, numericInput(ns("diffY"), "Angular grid jumps at: ", 4)),
                          column(6, numericInput(ns("diffTextSize"), "Set text size: ", 12))
                          )

      output[[7]] <- h5("Select color for ")
      output[[8]] <- fluidRow(width = 12,
                          column(4, colourpicker::colourInput(ns("diffColor0"), "baseline (difference=0)", "blue")),
                          column(4, colourpicker::colourInput(ns("diffColorBand"), "reference band (near baseline)", "darkgreen")),
                          column(4, colourpicker::colourInput(ns("diffColorCon"), "peak difference limits", "grey80"))
                          )
      output[[9]] <- selectInput(ns("diffLegendPosition"), "Set legend position",
                                  c("right" = "right",
                                    "left" = "left",
                                    "top" = "top",
                                    "bottom" = "bottom",
                                    "no legend" = "none"))

                          
      output[[9]] <- actionButton(ns('plotDiff'), 'Plot', icon=icon("play"), class="btn-success")
      output[[10]] <- h2("Export to PDF file")
      output[[12]] <- textInput(ns("p_phase_name"), "file name: ", placeholder = "PhaseDiff_gII_vs_gI")
      output[[13]] <- numericInput(ns("p_phase_wid"), "plot width: ", value = 8)
      output[[14]] <- numericInput(ns("p_phase_hei"), "plot height: ", value = 8)
      output[[15]] <- actionButton(ns('savePlots'), 'Export', icon=icon("play"), class="btn-success")
    }  
    output 
    })

  # output$UIStudyOneOrTwo <- renderUI({
  #   if(globalDB$studytype == "One"){
  #     output <- tagList()
  #     output[[1]] <- h3("Circos phase difference plot is not applicable to one-group study. ")
  #   }else if(globalDB$studytype == "Two"){
  #     output <- tagList()
  #     if(globalDB$dPhase == TRUE){
  #       output[[1]] <- h3("Color specification")
  #       output[[2]] <- checkboxInput(ns("diffColorCut"), "Different color for genes by differential phase result?", FALSE)

  #       if(input$diffColorCut){
  #         output[[3]] <- fluidRow(width = 12,
  #                         column(4,uiOutput(ns("diffsigCutParamColumns"))),
  #                         column(4,selectInput(ns("diffsigCutFun"), "direction ",
  #                     c("<" = "<",
  #                       ">" = ">", 
  #                       "=" = "="))),
  #                         column(4, numericInput(ns("diffsigCutValue"), "Cutoff value: ", 0.05))
  #                         )
  #         output[[4]] <- fluidRow(width = 12,
  #                         column(6, colourpicker::colourInput(ns("diffColorPointSig"), "Select color for genes that pass the criterion: ", "#03a1fc")),
  #                         column(6, colourpicker::colourInput(ns("diffColorPointNone"), "Select color for genes that do not pass the criterion: ", "#545454"))
  #                         )
  #       }else{
  #         output[[3]] <- br()
  #         output[[4]] <- br()
  #       }
  #       # output[[3]] <- conditionalPanel(
  #       #   condition = paste0("input['", ns("diffColorCut"), "'] == TRUE "),
  #       #   fluidRow(width = 12,
  #       #                   column(4,uiOutput(ns("diffsigCutParamColumns"))),
  #       #                   column(4,selectInput(ns("diffsigCutFun"), "direction ",
  #       #               c("<" = "<",
  #       #                 ">" = ">", 
  #       #                 "=" = "="))),
  #       #                   column(4, numericInput(ns("diffsigCutValue"), "Cutoff value: ", 0.05))
  #       #                   ),
  #       #   fluidRow(width = 12,
  #       #                   column(6, colourpicker::colourInput(ns("diffColorPointSig"), "Select color for genes that pass the criterion: ", "#03a1fc")),
  #       #                   column(6, colourpicker::colourInput(ns("diffColorPointNone"), "Select color for genes that do not pass the criterion: ", "#545454"))
  #       #                   )
  #       # )


  #       # output[[3]] <- conditionalPanel(
  #       #   condition = paste0("input['", ns("diffColorCut"), "'] == TRUE "),
  #       #   uiOutput(ns("diffsigCutParamColumns")
  #       #   ),
  #       #   selectInput(ns("diffsigCutFun"), "direction ",
  #       #               c("<" = "<",
  #       #                 ">" = ">")),
  #       #   numericInput(ns("diffsigCutValue"), "Cutoff value: ", 0.05),
  #       #   colourpicker::colourInput(ns("diffColorPointSig"), "Select color for genes that pass the criterion: ", "#03a1fc"),
  #       #   colourpicker::colourInput(ns("diffColorPointNone"), "Select color for genes that do not pass the criterion: ", "#545454"),
  #       # )
  #       # output[[4]] <- h5("More settings: ")
  #     }else{
  #       output[[1]] <- h5("Choose a TOJR cutoff from main panel")
  #       output[[2]] <- br()
  #       output[[3]] <- h5("More settings: ")
  #       output[[4]] <- br()
  #     }
  #     output[[5]] <- numericInput(ns("diffTimeStart"), "Starting Time: ", -6)
  #     output[[6]] <- numericInput(ns("diffTimeBandWidth"), "Band width around 0: ", 4)
  #     output[[7]] <- numericInput(ns("diffY"), "Set clock grid jump: ", 4)
  #     output[[8]] <- numericInput(ns("diffX"), "Set peak difference grid jump: ", 4)
  #     output[[9]] <- numericInput(ns("diffTextSize"), "Set text size: ", 12)
  #     output[[10]] <- colourpicker::colourInput(ns("diffColorBand"), "Select color for the band around 0", "darkgreen")
  #     output[[11]] <- colourpicker::colourInput(ns("diffColorCon"), "Select color for the peak difference terminals", "grey80")
  #     output[[12]] <- colourpicker::colourInput(ns("diffColor0"), "Select color for 0 peak difference reference", "blue")
  #     output[[13]] <- selectInput(ns("diffLegendPosition"), "Set legend position",
  #                                 c("right" = "right",
  #                                   "left" = "left",
  #                                   "top" = "top",
  #                                   "bottom" = "bottom",
  #                                   "no legend" = "none"))
  #     output[[14]] <- actionButton(ns('plotDiff'), 'Plot', icon=icon("play"), class="btn-success")
  #     output[[15]] <- h2("Export to PDF file")
  #     output[[16]] <- textInput(ns("p_phase_name"), "file name: ", value = "")
  #     output[[17]] <- numericInput(ns("p_phase_wid"), "plot width: ", value = 8)
  #     output[[18]] <- numericInput(ns("p_phase_hei"), "plot height: ", value = 8)
  #     output[[19]] <- actionButton(ns('savePlots'), 'Export', icon=icon("play"), class="btn-success")
  #   }
  #   output
  # })




  # output$nTOJR <- DT::renderDataTable({if(!is.null(db$TOJRsummary)){
  #   DT::datatable(db$TOJRsummary, selection = 'single')
  # }})
  #
  output$nTOJR <- DT::renderDataTable({
    # if(!is.null(db$TOJRsummary)){
    # DT::datatable(db$TOJRsummary, selection = 'single')
    # }
    if(nrow(globalDB$TOJRsummary)>0){
      DT::datatable({globalDB$TOJRsummary}, selection = 'single')
    }
  })
  # output$studytype <- renderText({
  #   if(!is.null(db$studytype)){
  #     db$studytype
  #   }
  # })


  # ##########################
  # # Observers              #
  # ##########################

  observeEvent(input$plotDiff, {
    if(is.null(input$nTOJR_rows_selected)){
      showNotification(ui = "Please select a cutoff from summary of TOJR table",
                       type = "error",
                       duration = 3)
    }else{
      print("DEBUG: enter plotDiff")
      TOJR.sel <- globalDB$TOJR[[input$nTOJR_rows_selected]]
      a.TOJR <- readRDS(paste0(globalDB$working.dir,  "/save/TOJR_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue,  "_Amp", TOJR.sel$ampCut, ".rds"))$TOJR$TOJR
      # a.plot$p = DiffCircaPipeline::DCP_PlotPeakDiff(x = CP.obj,
      # print("DEBUG in 2")
      # print(input$diffColorCut)
      # print(input$diffsigCutParam)
      # print(input$diffsigCutFun)
      # print(input$diffsigCutValue)
      # print(input$diffColorPointSig)
      # print(input$diffColorPointNone)

      # print("DEBUG: dPhase")
      # print(paste0("DEBUG: diffColorCut ", input$diffColorCut))
      # print(paste0("DEBUG: globalDB$dPhase ", globalDB$dPhase))
      a.dPhase <- ifelse2(input$diffColorCut,
                          ifelse2(globalDB$dPhase, res.DRP, "diffColorCut_error"),
                          NULL)
      # print(a.dPhase)

      if(!is.null(a.dPhase)){
        if(a.dPhase=="diffColorCut_error"){
          showNotification(
            ui = paste0("No differential phase analysis has been performed. Will plot peak difference without color specification. "),
            type = "warning",
            duration = NULL
          )
        }
      }


      # print("DEBUG a.dPhase")
      # print(a.dPhase)

      a.color.cut = ifelse2(input$diffColorCut,ifelse2(globalDB$dPhase, list(param = input$diffsigCutParam,
                                                                           fun = input$diffsigCutFun,
                                                                           val = input$diffsigCutValue,
                                                                           color.sig = input$diffColorPointSig,
                                                                           color.none = input$diffColorPointNone),
                                                     NULL),
                           NULL)
      # if(a.color.cut == "toNULL"){
      #   a.color.cut <- NULL
      # }

      # calculate cir.x.breaks
      # The number should be betweeen -1/2*P to 1/2*P
      if(input$Xstart>1/2*CP.obj[[1]]$P|input$Xstart<(-1/2*CP.obj[[1]]$P)){
      showNotification(ui = "Radius grid start should be within (-1/2, 1/2)*period",
                       type = "error",
                       duration = 3)
      }else{
      cir.x.breaks0 = seq(input$Xstart, by = input$diffX, length.out = ceiling(CP.obj[[1]]$P/input$diffX))
      cir.x.breaks0 = append(cir.x.breaks0, input$Xstart)
      cir.x.breaks0[cir.x.breaks0>1/2*CP.obj[[1]]$P] = cir.x.breaks0[cir.x.breaks0>1/2*CP.obj[[1]]$P]-CP.obj[[1]]$P
      cir.x.breaks0[cir.x.breaks0<(-1/2*CP.obj[[1]]$P)] = cir.x.breaks0[cir.x.breaks0<(-1/2*CP.obj[[1]]$P)]+CP.obj[[1]]$P
      }


      a.plot$p <- DiffCircaPipeline::DCP_PlotPeakDiff(x = CP.obj,
                                                    TOJR = a.TOJR,
                                                    dPhase = a.dPhase,
                                                    color.cut = a.color.cut,
                                                    time.start = input$diffTimeStart,
                                                    Info1 = globalDB$gIinfo,
                                                    Info2 = globalDB$gIIinfo,
                                                    concordance.ref = input$diffTimeBandWidth,
                                                    cir.x.breaks = cir.x.breaks0,
                                                    cir.y.breaks = seq(input$diffTimeStart, input$diffTimeStart+CP.obj[[1]]$P, input$diffY),
                                                    axis.text.size = input$diffTextSize,
                                                    legend.position = input$diffLegendPosition,
                                                    color.diff.refband = input$diffColorBand,
                                                    color.diff.xlim = input$diffColorCon,
                                                    color.diff.baseline = input$diffColor0
      )
      # print("DEBUG print diff plot")
      # print(a.plot$p)
      print("Peaktime difference circos plot plotted. ")

    }
  })

  observeEvent(input$savePlots, {
    pdf(paste0(db$working.dir, "/", input$p_phase_name, ".pdf"))
    print(a.plot$p)
    dev.off()
    print(paste0("Peaktime circos plot saved to ", db$working.dir))
  })


  # ##########################
  # # Render output/UI       #
  # ##########################

  output$diffsigCutParamColumns = renderUI({
    if(globalDB$dPhase){
      dPhase.cols = colnames(res.DRP)
      dPhase.cols = dPhase.cols[!dPhase.cols%in%c("gname", "P")]
      selectInput(ns('diffsigCutParam'), 'Cutoff criterion: ', dPhase.cols, selected = "pvalue")
    }
  })

  output$DRPres <- DT::renderDataTable({
    if(globalDB$printDRP){
    DT::datatable(round2(res.DRP, 3), options = list(scrollX = TRUE), rownames = FALSE)
  }})

  output$PeakPlot <- renderPlot({
    if(globalDB$studytype == "Two"){
      # print("DEBUG: renderPlot")
      print(a.plot$p)
    }
  }, height = 600, width = 600)

}
