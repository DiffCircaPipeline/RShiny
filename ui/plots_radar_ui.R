plots_radar_ui <- function(id, label = "Plots_radar"){
  ns <- NS(id)

  tabPanel("Radar plot", value=id,
           sidebarLayout(
             sidebarPanel(
               # shinyjs::useShinyjs(),
               ##########################
               # Parameters for the phase plot
               ##########################
               # uiOutput(ns("UIStudyOneOrTwo")),
               # uiOutput(ns("UIStudyTwo")),

               selectInput(ns("plotType"), "Radar plot or histogram?",
                           c("Radar plot" = "radar",
                             "Circos histogram" = "hist")),
               uiOutput(ns("radar_histogram")),

               # conditionalPanel(
               #   # condition = paste0("output.studytype == 'Two'"),
               #   # condition = paste0("input['", ns("studyType2"), "'] == '2' "),
               #   condition = "input.studyType2 == '2' ",
               #   h3("Select plot type for Two"),
               #   selectInput(ns("plotType2"), "Plot type: ",
               #               c("Peak time difference" = "circos_diff",
               #                 "Peak time linked" = "circos_linked",
               #                 "Peak time histogram" = "circos_hist2"))
               # ),

               # conditionalPanel(
               #   condition = paste0("input['", NS("plots_phase", "inputType"), "'] == '2' "),
               #   selectInput(ns("plotType2"), "Plot type: ",
               #               c("Peak time difference" = "circos_diff",
               #                 "Peak time linked" = "circos_linked",
               #                 "Peak time histogram" = "circos_hist2"))
               # ),

               # conditionalPanel(
               #   condition = paste0("output.studytype == 'One'"),
               #   selectInput(ns("plotType1"), "Plot type: ",
               #               c("Peak time histogram" = "circos_hist1"))
               # ),

             #   conditionalPanel(
             #     # condition = paste0("input['", ns("plotType1"), "'] == 'circos_hist1' "),
             #     # condition = paste0("output.studytype == 'One'"),
             #     # condition = paste0("input['", ns("studyType2"), "'] == '1' "),
             #     condition = "input.studyType2 == '1' ",
             #     h3("Make a one-group peak time histogram"),
             #     selectInput(ns("hist1sigCutParam"), "Cutoff criterion: ",
             #                 c("p-value" = "pvalue",
             #                   "q-value" = "qvalue",
             #                   "R^2" = "R2")),
             #     selectInput(ns("hist1sigCutFun"), "direction ",
             #                 c("<" = "<",
             #                   ">" = ">")),
             #     numericInput(ns("hist1sigCutValue"), "Cutoff value: ", 0.05),
             #     colourpicker::colourInput(ns("colorHist"), "Select color: ", "#3374b0"),
             #     h5("More subtle settings: "),
             #     numericInput(ns("hist1TimeStart"), "Starting Time: ", -6),
             #     numericInput(ns("hist1BinWidth"), "Set binwidth: ", 1),
             #     numericInput(ns("hist1Y"), "Set clock grid jump: ", 4),
             #     numericInput(ns("hist1TextSize"), "Set tick size: ", 12),
             #     selectInput(ns("hist1LegendPosition"), "Set legend position",
             #                 c("right" = "right",
             #                   "left" = "left",
             #                   "top" = "top",
             #                   "bottom" = "bottom",
             #                   "no legend" = "none"),
             #                 ),
             #     actionButton(ns('plotHist1'), 'Plot', icon=icon("play"), class="btn-success")
             # ),

             # conditionalPanel(
             #   condition = paste0("input['", ns("plotType2"), "'] == 'circos_hist2' "),
             #   h3("Make a peak time histogram "),
             #   h5("Select a TOJR from right. "),
             #   checkboxInput(ns("hist2BothOnly"), "Only plot RhyBoth genes? ", FALSE),
             #   checkboxInput(ns("hist2GroupSplit"), "Separate plots for two groups? ", FALSE),
             #   colourpicker::colourInput(ns("hist2color1"), "Select group I color: ", "#F8766D"),
             #   colourpicker::colourInput(ns("hist2color2"), "Select group II color", "#00BFC4"),
             #   h5("More subtle settings: "),
             #   numericInput(ns("hist2TimeStart"), "Starting Time: ", -6),
             #   numericInput(ns("hist2BinWidth"), "Set binwidth: ", 1),
             #   numericInput(ns("hist2Y"), "Set clock grid jump: ", 4),
             #   numericInput(ns("hist2TextSize"), "Set tick size: ", 12),
             #   selectInput(ns("hist2LegendPosition"), "Set legend position",
             #               c("right" = "right",
             #                 "left" = "left",
             #                 "top" = "top",
             #                 "bottom" = "bottom",
             #                 "no legend" = "none"),
             #   ),
             #   actionButton(ns('plotHist2'), 'Plot', icon=icon("play"), class="btn-success")
             # ),

             # conditionalPanel(
             #   condition = paste0("input['", ns("plotType2"), "'] == 'circos_linked' "),
             #
             #   h3("Make a peak time histogram "),
             #   #about point color
             #   conditionalPanel(
             #     condition = paste0("db$dPhase == TRUE "),
             #     checkboxInput(ns("linkColorCut"), "Different color for genes by differential phase result?", FALSE),
             #     conditionalPanel(
             #       condition = paste0("input['", ns("linkColorCut"), "'] == TRUE "),
             #       uiOutput(ns("linksigCutParamColumns")),
             #       selectInput(ns("linksigCutFun"), "direction ",
             #                   c("<" = "<",
             #                     ">" = ">")),
             #       numericInput(ns("linksigCutValue"), "Cutoff value: ", 0.05),
             #       colourpicker::colourInput(ns("linkColorPointSig"), "Select color for genes that pass the criterion: ", "#03a1fc"),
             #       colourpicker::colourInput(ns("linkColorPointNone"), "Select color for genes that do not pass the criterion: ", "dark grey"),
             #     )
             #   ),
             #   colourpicker::colourInput(ns("linkColor.line.low"), "Select line color for small |peak difference|: ", "#e8f7ff"),
             #   colourpicker::colourInput(ns("linkColor.line.high"), "Select line color for large |peak difference|: ", "#03a1fc"),
             #   h5("More subtle settings: "),
             #   numericInput(ns("linkTimeStart"), "Starting Time: ", -6),
             #   numericInput(ns("linkY"), "Set clock grid jump: ", 4),
             #   numericInput(ns("linkTextSize"), "Set tick size: ", 12),
             #   selectInput(ns("linkLegendPosition"), "Set legend position",
             #               c("right" = "right",
             #                 "left" = "left",
             #                 "top" = "top",
             #                 "bottom" = "bottom",
             #                 "no legend" = "none"),
             #   ),
             #   actionButton(ns('plotLinked'), 'Plot', icon=icon("play"), class="btn-success")
             # ),
             #
             # conditionalPanel(
             #   condition = paste0("input['", ns("plotType2"), "'] == 'circos_diff' "),
             #
             #   #about point color
             #   conditionalPanel(
             #     condition = paste0("db$dPhase == TRUE "),
             #     checkboxInput(ns("diffColorCut"), "Different color for genes by differential phase result?", FALSE),
             #     conditionalPanel(
             #       condition = paste0("input['", ns("diffColorCut"), "'] == TRUE "),
             #       uiOutput(ns("diffsigCutParamColumns")
             #       ),
             #       selectInput(ns("diffsigCutFun"), "direction ",
             #                   c("<" = "<",
             #                     ">" = ">")),
             #       numericInput(ns("diffsigCutValue"), "Cutoff value: ", 0.05),
             #       colourpicker::colourInput(ns("diffColorPointSig"), "Select color for genes that pass the criterion: ", "#03a1fc"),
             #       colourpicker::colourInput(ns("diffColorPointNone"), "Select color for genes that do not pass the criterion: ", "dark grey"),
             #     )
             #   ),
             #
             #   h5("More subtle settings: "),
             #   numericInput(ns("diffTimeStart"), "Starting Time: ", -6),
             #   numericInput(ns("diffTimeBandWidth"), "Band width around 0: ", 4),
             #   numericInput(ns("diffY"), "Set clock grid jump: ", 4),
             #   numericInput(ns("diffX"), "Set peak difference grid jump: ", 4),
             #   numericInput(ns("diffTextSize"), "Set tick size: ", 12),
             #   colourpicker::colourInput(ns("diffColorBand"), "Select color for the band around 0", "darkgreen"),
             #   colourpicker::colourInput(ns("diffColorCon"), "Select color for the peak difference terminals", "grey80"),
             #   colourpicker::colourInput(ns("diffColor0"), "Select color for 0 peak difference reference", "blue"),
             #   selectInput(ns("diffLegendPosition"), "Set legend position",
             #               c("right" = "right",
             #                 "left" = "left",
             #                 "top" = "top",
             #                 "bottom" = "bottom",
             #                 "no legend" = "none"),
             #   ),
             #   actionButton(ns('plotDiff'), 'Plot', icon=icon("play"), class="btn-success")
             # ),

             h2("Export to PDF file"),
             textInput(ns("p_phase_name"), "file name: ", value = ""),
             numericInput(ns("p_phase_wid"), "plot width: ", value = 8),
             numericInput(ns("p_phase_hei"), "plot height: ", value = 8),
             actionButton(ns('savePlots'), 'Export', icon=icon("play"), class="btn-success")
             ),

             mainPanel(
               h3("TOJR list"),
               DT::dataTableOutput(ns("nTOJR")),
               h3("Peak time plot"),
               plotOutput(ns("PeakPlot"))
             )

  )
  )
}
