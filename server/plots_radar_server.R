plots_radar_server <- function(input, output, session, globalDB) {

  ns <- NS("plots_radar")

  # ##########################
  # # Reactive Values        #
  # ##########################
  UI_server_com <- reactiveValues(RVplotType2 = "circos_diff",
                                  renderPlotType2 = FALSE,
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

  output$radar_histogram <- renderUI({
    if(globalDB$studytype == "One"&input$plotType == "hist"){
      output <- tagList()
      output[[1]] <- h3("Make a one-group peak time histogram")
      output[[2]] <- selectInput(ns("hist1sigCutParam"), "Cutoff criterion: ",
                                 c("p-value" = "pvalue",
                                   "q-value" = "qvalue",
                                   "R^2" = "R2"))
      output[[3]] <- selectInput(ns("hist1sigCutFun"), "direction ",
                      c("<" = "<",
                        ">" = ">"))
      output[[4]] <- numericInput(ns("hist1sigCutValue"), "Cutoff value: ", 0.05)
      output[[5]] <- colourpicker::colourInput(ns("colorHist"), "Select color: ", "#3374b0")
      output[[6]] <-  h5("More subtle settings: ")
      output[[7]] <- numericInput(ns("hist1TimeStart"), "Starting Time: ", -6)
      output[[8]] <- numericInput(ns("hist1BinWidth"), "Set binwidth: ", 1)
      output[[9]] <- numericInput(ns("hist1Y"), "Set clock grid jump: ", 4)
      output[[10]] <- numericInput(ns("hist1TextSize"), "Set tick size: ", 12)
      output[[11]] <- selectInput(ns("hist1LegendPosition"), "Set legend position",
                      c("right" = "right",
                        "left" = "left",
                        "top" = "top",
                        "bottom" = "bottom",
                        "no legend" = "none"),
                      )
      output[[12]] <- actionButton(ns('plotHist1'), 'Plot', icon=icon("play"), class="btn-success")
    }else if(globalDB$studytype == "Two"&input$plotType == "hist"){
      output <- tagList()
      output[[1]] <- h3("Make a peak time histogram ")
      output[[2]] <- h5("Select a TOJR from the right panel: ")
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

    }else if(globalDB$studytype == "One"&input$plotType == "radar"){
      output <- tagList()
      output[[1]] <- h3("Make a one-group radar plot of peak time")
      output[[2]] <- selectInput(ns("radar1sigCutParam"), "Cutoff criterion: ",
                                 c("p-value" = "pvalue",
                                   "q-value" = "qvalue",
                                   "R^2" = "R2"))
      output[[3]] <- selectInput(ns("radar1sigCutFun"), "direction ",
                                 c("<" = "<",
                                   ">" = ">"))
      output[[4]] <- numericInput(ns("radar1sigCutValue"), "Cutoff value: ", 0.05)
      output[[5]] <- colourpicker::colourInput(ns("colorRadar"), "Select color: ", "#3374b0")
      output[[6]] <-  h5("More settings: ")
      output[[7]] <- numericInput(ns("radar1TimeStart"), "Starting Time: ", -6)
      output[[8]] <- numericInput(ns("radar1BinWidth"), "Set binwidth: ", 1)
      # output[[9]] <- numericInput(ns("radar1Y"), "Set clock grid jump: ", 4)
      output[[9]] <- numericInput(ns("radar1TextSize"), "Set tick size: ", 12)
      output[[10]] <- selectInput(ns("radar1LegendPosition"), "Set legend position",
                                  c("right" = "right",
                                    "left" = "left",
                                    "top" = "top",
                                    "bottom" = "bottom",
                                    "no legend" = "none"),
      )
      output[[11]] <- actionButton(ns('plotRadar1'), 'Plot', icon=icon("play"), class="btn-success")
    }else if(globalDB$studytype == "Two"&input$plotType == "radar"){
      output <- tagList()
      output[[1]] <- h3("Make a radar plot of peak time")
      output[[2]] <- h5("Select a TOJR from the panel one the right: ")
      output[[3]] <- checkboxInput(ns("radar2BothOnly"), "Only plot RhyBoth genes? ", FALSE)
      # output[[4]] <- checkboxInput(ns("radar2GroupSplit"), "Separate plots for two groups? ", FALSE)
      output[[4]] <- colourpicker::colourInput(ns("radar2color1"), "Select group I color: ", "#F8766D")
      output[[5]] <- colourpicker::colourInput(ns("radar2color2"), "Select group II color", "#00BFC4")
      output[[6]] <- h5("More subtle settings: ")
      output[[7]] <- numericInput(ns("radar2TimeStart"), "Starting Time: ", -6)
      output[[8]] <- numericInput(ns("radar2BinWidth"), "Set binwidth: ", 1)
      # output[[9]] <- numericInput(ns("hist2Y"), "Set clock grid jump: ", 4)
      output[[10]] <- numericInput(ns("radar2TextSize"), "Set tick size: ", 12)
      output[[11]] <- selectInput(ns("radar2LegendPosition"), "Set legend position",
                                  c("right" = "right",
                                    "left" = "left",
                                    "top" = "top",
                                    "bottom" = "bottom",
                                    "no legend" = "none"),
      )
      output[[12]] <- actionButton(ns('plotRadar2'), 'Plot', icon=icon("play"), class="btn-success")
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
  observeEvent(input$plotHist1, {
    a.plot$p = DiffCircaPipeline::DCP_PlotPeakHist(x = CP.obj, sig.cut = list(param = input$hist1sigCutParam,
                                                                             fun = input$hist1sigCutFun,
                                                                             val = input$hist1sigCutValue),
                                                  time.start = input$hist1TimeStart,
                                                  Info1 = db$studyname,
                                                  color.hist = input$colorHist,
                                                  cir.y.breaks = seq(input$hist1TimeStart, input$hist1TimeStart+CP.obj$P, input$hist1Y),
                                                  single.binwidth = input$hist1BinWidth,
                                                  axis.text.size = input$hist1TextSize,
                                                  legend.position = input$hist1LegendPosition
    )
    # print("DEBUG print hist1 plot")
    print(a.plot$p)
    print("Histogram ploted")
  })

  observeEvent(input$plotHist2, {
    if(is.null(input$nTOJR_rows_selected)){
      showNotification(ui = "Please select a cutoff from TOJR list",
                       type = "error",
                       duration = 3)
    }else{
      # print("selected color")
      # print(input$hist2color1)
      # print(input$hist2color2)
      TOJR.sel = db$TOJR[[input$nTOJR_rows_selected]]
      TOJR.sel.load <- readRDS(paste0(globalDB$working.dir,  "/save/TOJR_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue,  "_Amp", TOJR.sel$ampCut, ".rds"))
      a.plot$p = DiffCircaPipeline::DCP_PlotPeakHist(x = CP.obj, TOJR = TOJR.sel.load$TOJR[match(CP.obj$gname_overlap, TOJR.sel.load$TOJR$gname), "TOJR"],
                                                    RhyBothOnly = input$hist2BothOnly,
                                                    time.start = input$hist2TimeStart,
                                                    Info1 = globalDB$gIinfo,
                                                    Info2 = globalDB$gIIinfo,
                                                    GroupSplit = input$hist2GroupSplit,
                                                    color.hist = c(input$hist2color1, input$hist2color2),
                                                    cir.y.breaks = seq(input$hist2TimeStart, input$hist2TimeStart+CP.obj[[1]]$P, input$hist2Y),
                                                    single.binwidth = input$hist2BinWidth,
                                                    axis.text.size = input$hist2TextSize,
                                                    legend.position = input$hist2LegendPosition
      )
      # print("DEBUG print hist2 plot")
      print(a.plot$p)
      print("Histogram ploted")
    }

  })

  observeEvent(input$plotRadar1, {
    a.plot$p = DiffCircaPipeline::DCP_PlotPeakRadar(x = CP.obj, sig.cut = list(param = input$radar1sigCutParam,
                                                                             fun = input$radar1sigCutFun,
                                                                             val = input$radar1sigCutValue),
                                                  time.start = input$radar1TimeStart,
                                                  Info1 = db$studyname,
                                                  color = input$colorRadar,
                                                  # cir.y.breaks = seq(input$radar1TimeStart, input$radar1TimeStart+CP.obj$P, input$radar1Y),
                                                  single.binwidth = input$radar1BinWidth,
                                                  axis.text.size = input$radar1TextSize,
                                                  legend.position = input$radar1LegendPosition
    )
    # print("DEBUG print radar1 plot")
    print(a.plot$p)
  })

  observeEvent(input$plotRadar2, {
    if(is.null(input$nTOJR_rows_selected)){
      showNotification(ui = "Please select a cutoff from TOJR list",
                       type = "error",
                       duration = 3)
    }else{
      # print("selected color")
      # print(input$radar2color1)
      # print(input$radar2color2)
      TOJR.sel = db$TOJR[[input$nTOJR_rows_selected]]
      TOJR.sel.load <- readRDS(paste0(globalDB$working.dir,  "/save/TOJR_", TOJR.sel$method, "_", TOJR.sel$cutoffType, "_", TOJR.sel$cutoffValue,  "_Amp", TOJR.sel$ampCut, ".rds"))
      a.plot$p = DiffCircaPipeline::DCP_PlotPeakRadar(x = CP.obj, TOJR = TOJR.sel.load$TOJR[match(CP.obj$gname_overlap, TOJR.sel.load$TOJR$gname), "TOJR"],
                                                    RhyBothOnly = input$radar2BothOnly,
                                                    time.start = input$radar2TimeStart,
                                                    Info1 = globalDB$gIinfo,
                                                    Info2 = globalDB$gIIinfo,
                                                    # GroupSplit = input$radar2GroupSplit,
                                                    color = c(input$radar2color1, input$radar2color2),
                                                    # cir.y.breaks = seq(input$radar2TimeStart, input$radar2TimeStart+CP.obj[[1]]$P, input$radar2Y),
                                                    single.binwidth = input$radar2BinWidth,
                                                    axis.text.size = input$radar2TextSize,
                                                    legend.position = input$radar2LegendPosition
      )
      # print("DEBUG print radar2 plot")
      print(a.plot$p)
      print("Radar plot ploted")
    }

  })


  observeEvent(input$savePlots, {
    pdf(paste0(db$working.dir, "/", input$p_phase_name, ".pdf"))
    print(a.plot$p)
    dev.off()
    print(paste0("Histogram/Radarplot saved to ", db$working.dir))
  })


  # ##########################
  # # Render output/UI       #
  # ##########################

  output$PeakPlot <- renderPlot({
    a.plot$p
  })

}
