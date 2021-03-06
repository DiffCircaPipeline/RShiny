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

  output$UIStudyOneOrTwo <- renderUI({
    if(globalDB$studytype == "One"){
      output <- tagList()
      output[[1]] <- h3("Circos phase difference plot is not applicable to one-group study. ")
    }else if(globalDB$studytype == "Two"){
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
        output[[1]] <- h5("Choose a TOJR from main panel")
        output[[2]] <- br()
        output[[3]] <- h5("More settings: ")
        output[[4]] <- br()
      }
      output[[5]] <- numericInput(ns("diffTimeStart"), "Starting Time: ", -6)
      output[[6]] <- numericInput(ns("diffTimeBandWidth"), "Band width around 0: ", 4)
      output[[7]] <- numericInput(ns("diffY"), "Set clock grid jump: ", 4)
      output[[8]] <- numericInput(ns("diffX"), "Set peak difference grid jump: ", 4)
      output[[9]] <- numericInput(ns("diffTextSize"), "Set text size: ", 12)
      output[[10]] <- colourpicker::colourInput(ns("diffColorBand"), "Select color for the band around 0", "darkgreen")
      output[[11]] <- colourpicker::colourInput(ns("diffColorCon"), "Select color for the peak difference terminals", "grey80")
      output[[12]] <- colourpicker::colourInput(ns("diffColor0"), "Select color for 0 peak difference reference", "blue")
      output[[13]] <- selectInput(ns("diffLegendPosition"), "Set legend position",
                                  c("right" = "right",
                                    "left" = "left",
                                    "top" = "top",
                                    "bottom" = "bottom",
                                    "no legend" = "none"))
      output[[14]] <- actionButton(ns('plotDiff'), 'Plot', icon=icon("play"), class="btn-success")
      output[[15]] <- h2("Export to PDF file")
      output[[16]] <- textInput(ns("p_phase_name"), "file name: ", value = "")
      output[[17]] <- numericInput(ns("p_phase_wid"), "plot width: ", value = 8)
      output[[18]] <- numericInput(ns("p_phase_hei"), "plot height: ", value = 8)
      output[[19]] <- actionButton(ns('savePlots'), 'Export', icon=icon("play"), class="btn-success")
    }
    output
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
  #     stop("Please select a TOJR") #later a pop up message
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
      stop("Please select a TOJR") #later a pop up message
    }else{
      TOJR.sel <- intDB$TOJR[[input$nTOJR_rows_selected]]
      a.dPhase <- ifelse2(input$diffColorCut, 
                          ifelse2( #if selected to plot diffColorCut, check if the the selected TOJR has performed diff phase 
                            TOJR.sel[["dPhase"]] == "N", 
                            "diffColorCut_error", readRDS(paste0(globalDB$working.dir, "/save/DR_parameter", "_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue,  "_Amp", TOJR.sel$ampCut, ".rds"))),
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
      a.plot$p <- DiffCircaPipeline::DCP_PlotPeakLink(x = CP.obj, TOJR = readRDS(paste0(globalDB$working.dir,  "/save/TOJR_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue,  "_Amp", TOJR.sel$ampCut, ".rds"))$TOJR$TOJR,
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
      stop("Please select a TOJR") #later a pop up message
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
    if(globalDB$studytype == "Two"){
      # print("DEBUG: renderPlot")
      print(a.plot$p)
    }
  }, height = 600, width = 600)

}
