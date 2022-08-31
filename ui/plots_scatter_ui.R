plots_scatter_ui <- function(id, label = "plots_scatter"){
  ns <- NS(id)

  tabPanel("Scatter Plots", value=id,
           sidebarLayout(
             sidebarPanel(
               # useShinyjs(),
               ##########################
               # Parameters for the scatter plot
               ##########################
               # conditionalPanel(
               #   condition = paste0("db$studytype == 'Two' "),
               #   textInput(ns("p_scatter_gIinfo"), "Group I label", value = db$gIinfo),
               #   textInput(ns("p_scatter_gIinfo"), "Group II label", value = db$gIIinfo)
               # ),
               h2("Export to PDF file"),
               textInput(ns("p_scatter_name"), "file name: ", placeholder = "Selected Gene"),
               numericInput(ns("p_scatter_wid"), "plot width: ", value = 8),
               numericInput(ns("p_scatter_hei"), "plot height: ", value = 4),
               actionButton(ns('saveScatter'), 'Export', icon=icon("play"), class="btn-success")
             ),

             mainPanel(
               h3("Select a gene from: "),
               uiOutput(ns("SelectGeneForScatter")),
               h3("The scatter plot"),
               uiOutput(ns("scatterPlot_ui"))
               # plotOutput(ns("scatterPlot"))
             )
           )
  )
}
