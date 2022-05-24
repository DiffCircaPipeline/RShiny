plots_scatter_ui <- function(id, label = "plots_scatter"){
  ns <- NS(id)

  tabPanel("Make A Scatter Plot", value=id,
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
               textInput(ns("p_scatter_name"), "file name: ", value = "Selected Gene"),
               numericInput(ns("p_scatter_wid"), "plot width: ", value = 8),
               numericInput(ns("p_scatter_hei"), "plot height: ", value = 8),
               actionButton(ns('saveScatter'), 'Export', icon=icon("play"), class="btn-success")
             ),

             mainPanel(
               h3("Select a gene from: "),
               tabsetPanel(
                 tabPanel("Parameter estimates",
                          fluidRow(width = 12,
                                   column(6, h5("Group I"), DT::dataTableOutput(ns("Rhythm1"), width = "auto")),
                                   column(6, h5("Group II"), DT::dataTableOutput(ns("Rhythm2"), width = "auto"))
                                   )
                          ),
                 tabPanel("DR parameter result",
                          DT::dataTableOutput(ns("DRPres"))
                          ),
                 tabPanel("DR fitness result",
                          DT::dataTableOutput(ns("DRFres"))
                          )
               ),
               h3("The scatter plot"),
               plotOutput(ns("scatterPlot"))
             )
           )
  )
}
