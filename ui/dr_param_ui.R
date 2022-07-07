dr_param_ui <- function(id, label= "dr parameter") {
  ns <- NS(id)

  tabPanel("Differential Rhythm Parameter", value=id,
           sidebarLayout(
             sidebarPanel(
               # useShinyjs(),
               ##########################
               # Upload Data
               ##########################
               uiOutput(ns("DR_param_ui"))
             ),

             mainPanel(
               h3("TOJR list"),
               DT::dataTableOutput(ns("nTOJR")),
               h3("DR parameter result"),
               DT::dataTableOutput(ns("DRP"))
             )
           )
  )
}
