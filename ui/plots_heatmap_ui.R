plots_heatmap_ui <- function(id, label = "plots_heatmap"){
  ns <- NS(id)

  tabPanel("Make A Heatmap", value=id,
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
               numericInput(ns("p_scatter_hei"), "plot height: ", value = 8),
               actionButton(ns('saveHM'), 'Export', icon=icon("play"), class="btn-success")
             ),

             mainPanel(
               h3("Select genes to plot from: "),
               uiOutput(ns("SelectGeneForHM")),
               h3("Heatmap"),
               uiOutput(ns("heatmap_ui"))
               # plotOutput(ns("heatmap"))
             )
           )
  )
}
