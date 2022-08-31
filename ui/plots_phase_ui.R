plots_phase_ui <- function(id, label = "Plots_phase"){
  ns <- NS(id)

  tabPanel("Circos phase difference plot", value=id,
           sidebarLayout(
             sidebarPanel(
               # shinyjs::useShinyjs(),
               ##########################
               # Parameters for the phase plot
               ##########################
               uiOutput(ns("UIStudyColorByDPhase")),
               uiOutput(ns("UIStudyOthers"))
             ),

             mainPanel(

              tabsetPanel(
                tabPanel("Summary of TOJR",
                 DT::dataTableOutput(ns("nTOJR"))
                 ),
                tabPanel("DR parameter result",
                 DT::dataTableOutput(ns("DRPres"))
                 ),
                ),

               # h3("Summary of TOJR"),
               # DT::dataTableOutput(ns("nTOJR")),
               # h3("DR parameter result"),
               # DT::dataTableOutput(ns("DRPres")),
               h3("Peak time summary plot"),
               plotOutput(ns("PeakPlot"))
             )

  )
  )
}
