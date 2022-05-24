plots_heatmap_ui <- function(id, label = "plots_heatmap"){
  ns <- NS(id)

  selectAllRowsSource.choice = c("Rhythm1", "Rhythm2", "DRPres", "DRFres")
  names(selectAllRowsSource.choice) = c(paste0("Parameter estimates of ", db$gIinfo),
                                        paste0("Parameter estimates of ", db$gIIinfo),
                                        "DR parameter result",
                                        "DR fitness result")
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
               textInput(ns("p_scatter_name"), "file name: ", value = "Selected Gene"),
               numericInput(ns("p_scatter_wid"), "plot width: ", value = 8),
               numericInput(ns("p_scatter_hei"), "plot height: ", value = 8),
               actionButton(ns('saveHM'), 'Export', icon=icon("play"), class="btn-success")
             ),

             mainPanel(
               h3("Select gene lists from: "),
               tabsetPanel(
                 tabPanel("Parameter estimates",
                          fluidRow(width = 12,
                                   column(6, DT::dataTableOutput(ns("Rhythm1"), width = "auto")),
                                   column(6, DT::dataTableOutput(ns("Rhythm2"), width = "auto"))
                          )
                 ),
                 tabPanel("DR parameter result",
                          DT::dataTableOutput(ns("DRPres"))
                 ),
                 tabPanel("DR fitness result",
                          DT::dataTableOutput(ns("DRFres"))
                 )
               ),
               checkboxInput(ns("selectAllRows"), label = "Select all", value = FALSE),
               conditionalPanel(
                 condition = paste0("input['", ns("selectAllRows"), "'] == TRUE "),
                 selectInput(ns("selectAllRowsSource"), "Which gene list to plot:",
                             selectAllRowsSource.choice
                             # choices = c("Parameter estimates of group I" = "Rhythm1",
                             #             "Parameter estimates of group II" = "Rhythm2",
                             #             "DR parameter result" = "DRPres",
                             #             "DR fitness result" = "DRFres"
                             #             )
                             )
               ),
               h3("The heatmap"),
               plotOutput(ns("heatmap"))
             )
           )
  )
}
