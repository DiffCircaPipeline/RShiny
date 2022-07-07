dr_fit_ui <- function(id, label= "dr fitness") {
  ns <- NS(id)

  tabPanel("Differential Rhythm Fitness", value=id,
           sidebarLayout(
             sidebarPanel(
               # useShinyjs(),
               ##########################
               # Upload Data
               ##########################
               uiOutput(ns("DR_fit_ui"))
             ),

             mainPanel(
               h3("TOJR list"),
               DT::dataTableOutput(ns("nTOJR")),
               h3("DR fitness result"),
               DT::dataTableOutput(ns("DRF"))
             )
           )
  )
}
