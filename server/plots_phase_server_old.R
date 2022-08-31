plots_phase_server <- function(input, output, session, globalDB) {

  ns <- NS("plots_phase")

  # ##########################
  # # Reactive Values        #
  # ##########################
  UI_server_com <- reactiveValues(RVplotType2 = "circos_diff",
                                  renderPlotType2 = NULL,
                                  choose_StudyTwo = FALSE,
                                  output = NULL)

  a.plot <- reactiveValues(p = NULL)

  # observeEvent(db$studytype, {
  #   if(db$studytype=="One"){
  #     print(paste0("db$studytype=='one'"))
  #   }else{
  #     print(paste0("db$studytype=='two'"))
  #   }
  # })

  output$UIStudyOneOrTwo <- renderUI({
    if(globalDB$studytype == "One"){
      output <- tagList()
      output[[1]] <- h3("Circos phase difference plot is not applicable to one-grouo study. ")
    }else if(globalDB$studytype == "Two"){
      output <- tagList()
      output[[1]] <- selectInput(ns("plotType2"), "Plot type: ",
                                 c("Peak time difference" = "circos_diff",
                                   "Peak time linked" = "circos_linked"#,
                                   # "Peak time histogram" = "circos_hist2"
                                   ))
      UI_server_com$choose_StudyTwo <- TRUE

    }
    output
  })

  observeEvent(c(UI_server_com$choose_StudyTwo, input$plotType2), {
    if(UI_server_com$choose_StudyTwo){
      UI_server_com$RVplotType2 <- input$plotType2
    }
  })

  output$UIStudyTwo <- renderUI({
    if((!is.null(UI_server_com$RVplotType2))&(UI_server_com$choose_StudyTwo)){
      # print("check warning renderUI")
      # print(globalDB$studytype)
      # print(UI_server_com$RVplotType2)
      # print(UI_server_com$choose_StudyTwo)
      if(globalDB$studytype=="Two"&UI_server_com$RVplotType2 == "circos_diff"){
        output <- tagList()
        if(globalDB$dPhase == TRUE){
          output[[1]] <- h3("Color specification")
          output[[2]] <- checkboxInput(ns("diffColorCut"), "Different color for genes by differential phase result?", FALSE)
          output[[3]] <- conditionalPanel(
            condition = paste0("input['", ns("diffColorCut"), "'] == TRUE "),
            uiOutput(ns("diffsigCutParamColumns")
            ),
            selectInput(ns("diffsigCutFun"), "direction ",
                        c("<" = "<",
                          ">" = ">")),
            numericInput(ns("diffsigCutValue"), "Cutoff value: ", 0.05),
            colourpicker::colourInput(ns("diffColorPointSig"), "Select color for genes that pass the criterion: ", "#03a1fc"),
            colourpicker::colourInput(ns("diffColorPointNone"), "Select color for genes that do not pass the criterion: ", "#545454"),
          )
          output[[4]] <- h5("More settings: ")
        }else{
          output[[1]] <- h5("Choose TOJR from main panel")
          output[[2]] <- br()
          output[[3]] <- h5("More settings: ")
          output[[4]] <- br()
        }
        output[[5]] <- numericInput(ns("diffTimeStart"), "Starting Time: ", -6)
        output[[6]] <- numericInput(ns("diffTimeBandWidth"), "Band width around 0: ", 4)
        output[[7]] <- numericInput(ns("diffY"), "Set clock grid jump: ", 4)
        output[[8]] <- numericInput(ns("diffX"), "Set peak difference grid jump: ", 4)
        output[[9]] <- numericInput(ns("diffTextSize"), "Set tick size: ", 12)
        output[[10]] <- colourpicker::colourInput(ns("diffColorBand"), "Select color for the band around 0", "darkgreen")
        output[[11]] <- colourpicker::colourInput(ns("diffColorCon"), "Select color for the peak difference terminals", "grey80")
        output[[12]] <- colourpicker::colourInput(ns("diffColor0"), "Select color for 0 peak difference reference", "blue")
        output[[13]] <- selectInput(ns("diffLegendPosition"), "Set legend position",
                                    c("right" = "right",
                                      "left" = "left",
                                      "top" = "top",
                                      "bottom" = "bottom",
                                      "no legend" = "none")
        )
        output[[14]] <- actionButton(ns('plotDiff'), 'Plot', icon=icon("play"), class="btn-success")
        output[[15]] <- h2("Export to PDF file")
        output[[16]] <- textInput(ns("p_phase_name"), "file name: ", value = "")
        output[[17]] <- numericInput(ns("p_phase_wid"), "plot width: ", value = 8)
        output[[18]] <- numericInput(ns("p_phase_hei"), "plot height: ", value = 8)
        output[[19]] <- actionButton(ns('savePlots'), 'Export', icon=icon("play"), class="btn-success")
        output

      }else if(globalDB$studytype=="Two"&UI_server_com$RVplotType2 == "circos_linked"){
        output <- tagList()
        if(globalDB$dPhase == TRUE){
          output[[1]] <- h3("Color specification")
          output[[2]] <- checkboxInput(ns("linkColorCut"), "linkerent color for genes by linkerential phase result?", FALSE)
          output[[3]] <- conditionalPanel(
            condition = paste0("input['", ns("linkColorCut"), "'] == TRUE "),
            uiOutput(ns("linksigCutParamColumns")
            ),
            selectInput(ns("linksigCutFun"), "direction ",
                        c("<" = "<",
                          ">" = ">")),
            numericInput(ns("linksigCutValue"), "Cutoff value: ", 0.05),
            colourpicker::colourInput(ns("linkColorPointSig"), "Select color for genes that pass the criterion: ", "#03a1fc"),
            colourpicker::colourInput(ns("linkColorPointNone"), "Select color for genes that do not pass the criterion: ", "#545454"),
          )
        }else{
          output[[1]] <- h5("Choose TOJR from main panel")
          output[[2]] <- br()
          output[[3]] <- h5("More subtle settings: ")
        }
        output[[5]] <- colourpicker::colourInput(ns("linkColor.line.low"), "Select line color for small |peak difference|: ", "#e8f7ff")
        output[[6]] <- colourpicker::colourInput(ns("linkColor.line.high"), "Select line color for large |peak difference|: ", "#03a1fc")
        output[[7]] <- h5("More subtle settings: ")
        output[[8]] <- numericInput(ns("linkTimeStart"), "Starting Time: ", -6)
        output[[9]] <- numericInput(ns("linkY"), "Set clock grid jump: ", 4)
        output[[10]] <- numericInput(ns("linkTextSize"), "Set tick size: ", 12)
        output[[11]] <- selectInput(ns("linkLegendPosition"), "Set legend position",
                                    c("right" = "right",
                                      "left" = "left",
                                      "top" = "top",
                                      "bottom" = "bottom",
                                      "no legend" = "none"),
        )
        output[[12]] <- actionButton(ns('plotLinked'), 'Plot', icon=icon("play"), class="btn-success")
        output[[13]] <- h2("Export to PDF file")
        output[[14]] <- textInput(ns("p_phase_name"), "file name: ", value = "")
        output[[15]] <- numericInput(ns("p_phase_wid"), "plot width: ", value = 8)
        output[[16]] <- numericInput(ns("p_phase_hei"), "plot height: ", value = 8)
        output[[17]] <- actionButton(ns('savePlots'), 'Export', icon=icon("play"), class="btn-success")
        output

      }else if(globalDB$studytype=="Two"&UI_server_com$RVplotType2 == "circos_hist2"){
        output <- tagList()
        output[[1]] <- h3("Make a peak time histogram ")
        output[[2]] <- h5("Select a TOJR cutoff from the right panel: ")
        output[[3]] <- checkboxInput(ns("hist2BothOnly"), "Only plot RhyBoth genes? ", FALSE)
        output[[4]] <- checkboxInput(ns("hist2GroupSplit"), "Separate plots for two groups? ", FALSE)
        output[[5]] <- colourpicker::colourInput(ns("hist2color1"), "Select group I color: ", "#F8766D")
        output[[6]] <- colourpicker::colourInput(ns("hist2color2"), "Select group II color", "#00BFC4")
        output[[7]] <- h5("More subtle settings: ")
        output[[8]] <- numericInput(ns("hist2TimeStart"), "Starting Time: ", -6)
        output[[9]] <- numericInput(ns("hist2BinWidth"), "Set binwidth: ", 1)
        output[[10]] <- numericInput(ns("hist2Y"), "Set clock grid jump: ", 4)
        output[[11]] <- numericInput(ns("hist2TextSize"), "Set tick size: ", 12)
        output[[12]] <- selectInput(ns("hist2LegendPosition"), "Set legend position",
                                    c("right" = "right",
                                      "left" = "left",
                                      "top" = "top",
                                      "bottom" = "bottom",
                                      "no legend" = "none"),
        )
        output[[13]] <- actionButton(ns('plotHist2'), 'Plot', icon=icon("play"), class="btn-success")
        output[[14]] <- h2("Export to PDF file")
        output[[15]] <- textInput(ns("p_phase_name"), "file name: ", value = "")
        output[[16]] <- numericInput(ns("p_phase_wid"), "plot width: ", value = 8)
        output[[17]] <- numericInput(ns("p_phase_hei"), "plot height: ", value = 8)
        output[[18]] <- actionButton(ns('savePlots'), 'Export', icon=icon("play"), class="btn-success")
        output
      }else{
        br()
      }
    }
  })

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
  # observeEvent(input$plotHist1, {
  #   a.plot$p = DiffCircaPipeline::DCP_PlotPeakHist(x = CP.obj, sig.cut = list(param = input$hist1sigCutParam,
  #                                                                            fun = input$hist1sigCutFun,
  #                                                                            val = input$hist1sigCutValue),
  #                                                 time.start = input$hist1TimeStart,
  #                                                 Info1 = db$studyname,
  #                                                 color.hist = input$colorHist,
  #                                                 cir.y.breaks = seq(input$hist1TimeStart, input$hist1TimeStart+CP.obj$P, input$hist1Y),
  #                                                 single.binwidth = input$hist1BinWidth,
  #                                                 axis.text.size = input$hist1TextSize,
  #                                                 legend.position = input$hist1LegendPosition
  #   )
  # })

  # observeEvent(input$plotHist2, {
  #   if(is.null(input$nTOJR_rows_selected)){
  #     stop("Please select a TOJR cutoff") #later a pop up message
  #   }else{
  #     print("selected color")
  #     print(input$hist2color1)
  #     print(input$hist2color2)
  #     TOJR.sel <- globalDB$TOJR[[input$nTOJR_rows_selected]]
  #     a.plot$p = DiffCircaPipeline::DCP_PlotPeakHist(x = CP.obj, TOJR = readRDS(paste0(globalDB$working.dir,  "/save/TOJR_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue, ".rds"))$TOJR,
  #                                                   RhyBothOnly = input$hist2BothOnly,
  #                                                   time.start = input$hist2TimeStart,
  #                                                   Info1 = globalDB$gIinfo,
  #                                                   Info2 = globalDB$gIIinfo,
  #                                                   GroupSplit = input$hist2GroupSplit,
  #                                                   color.hist = c(input$hist2color1, input$hist2color2),
  #                                                   cir.y.breaks = seq(input$hist2TimeStart, input$hist2TimeStart+CP.obj[[1]]$P, input$hist2Y),
  #                                                   single.binwidth = input$hist2BinWidth,
  #                                                   axis.text.size = input$hist2TextSize,
  #                                                   legend.position = input$hist2LegendPosition
  #     )
  #   }
  #
  # })

  observeEvent(input$plotLinked, {
    if(is.null(input$nTOJR_rows_selected)){
      stop("Please select a TOJR cutoff") #later a pop up message
    }else{
      TOJR.sel <- intDB$TOJR[[input$nTOJR_rows_selected]]
      a.dPhase <- ifelse2(input$diffColorCut, 
                          ifelse2( #if selected to plot diffColorCut, check if the the selected TOJR has performed diff phase 
                            TOJR.sel[["dPhase"]] == "N", 
                            "diffColorCut_error", readRDS(paste0(globalDB$working.dir, "/save/DR_parameter", "_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue, ".rds"))),
                          NULL)
      # print(a.dPhase)

      if(!is.null(a.dPhase)){
        if(a.dPhase=="diffColorCut_error"){
          showNotification(
            ui = paste0("No differential phase analysis has been performed with this TOJR cutoff. Will plot peak difference without color specification. "),
            type = "warning",
            duration = NULL
          )
        }
      }

      TOJR.sel <- globalDB$TOJR[[input$nTOJR_rows_selected]]
      a.plot$p <- DiffCircaPipeline::DCP_PlotPeakLink(x = CP.obj, TOJR = readRDS(paste0(globalDB$working.dir,  "/save/TOJR_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue, ".rds"))$TOJR$TOJR,
                                                    dPhase = a.dPhase,
                                                    color.cut = ifelse2(input$linkColorCut, ifelse2(db$dPhase, list(param = input$linksigCutParam,
                                                                                                       fun = input$linksigCutFun,
                                                                                                       val = input$linksigCutValue,
                                                                                                       color.sig = input$linkColorPointSig,
                                                                                                       color.none = input$linkColorPointNone),
                                                                                       NULL),
                                                                        NULL),
                                                    time.start = input$linkTimeStart,
                                                    Info1 = db$gIinfo,
                                                    Info2 = db$gIIinfo,
                                                    cir.y.breaks = seq(input$linkTimeStart, input$linkTimeStart+CP.obj[[1]]$P, input$linkY),
                                                    axis.text.size = input$linkTextSize,
                                                    legend.position = input$linkLegendPosition,
                                                    color.link.low = input$linkColor.line.low,
                                                    color.link.high = input$linkColor.line.high
      )
      # print("DEBUG print linked plot")
      print(a.plot$p)
      print("Linked circos plot plotted. ")
      }
  })

  observeEvent(input$plotDiff, {
    if(is.null(input$nTOJR_rows_selected)){
      stop("Please select a TOJR cutoff") #later a pop up message
    }else{
      print("DEBUG: enter plotDiff")
      TOJR.sel <- globalDB$TOJR[[input$nTOJR_rows_selected]]
      a.TOJR <- readRDS(paste0(globalDB$working.dir,  "/save/TOJR_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue, ".rds"))$TOJR$TOJR
      print(paste0("DEBUG TOJR selection ", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue))
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

      # print(a.color.cut)
      a.plot$p <- DiffCircaPipeline::DCP_PlotPeakDiff(x = CP.obj,
                                                    TOJR = a.TOJR,
                                                    dPhase = a.dPhase,
                                                    color.cut = a.color.cut,
                                                    time.start = input$diffTimeStart,
                                                    Info1 = globalDB$gIinfo,
                                                    Info2 = globalDB$gIIinfo,
                                                    concordance.ref = input$diffTimeBandWidth,
                                                    cir.x.breaks = seq(-1/2*CP.obj[[1]]$P, 1/2*CP.obj[[1]]$P, input$diffX),
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
  output$linksigCutParamColumns = renderUI({
    if(globalDB$dPhase){
      dPhase.cols = colnames(res.DRP)
      dPhase.cols = dPhase.cols[!dPhase.cols%in%c("gname", "P")]
      selectInput(ns('linksigCutParam'), 'Cutoff criterion: ', dPhase.cols)
    }
  })
  output$DRPres <- DT::renderDataTable({
    if(globalDB$printDRP){
    DT::datatable(round2(res.DRP, 3), options = list(scrollX = TRUE), rownames = FALSE)
  }})

  output$PeakPlot <- renderPlot({
    if(UI_server_com$choose_StudyTwo){
      print("DEBUG: renderPlot")
      print(a.plot$p)
    }
  }, height = 600, width = 600)

}
