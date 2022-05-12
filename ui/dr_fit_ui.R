dr_fit_ui <- function(id, label= "dr fitness") {
  ns <- NS(id)

  tabPanel("Differential Rhythm Fitness", value=id,
           sidebarLayout(
             sidebarPanel(
               # useShinyjs(),
               ##########################
               # Upload Data
               ##########################
               numericInput(ns("DR_parallel.ncores"), "Number of cores for computation:", value = 1, min = 1,
                            max = ifelse(.Platform$OS.type != "windows", parallel::detectCores(), 1)),

               h2("Differential Fitness"),
               selectInput(ns("DRR_method"), HTML("Select method for differential R<sup>2</sup> test"), 
                           choices = c("likelihood ratio test (Default)" = "LR",
                                       "permutation" = "permutation",
                                       "bootstrap" = "bootstrap"),
                           selected = "likelihood ratio test (Default)"),
               conditionalPanel(
                 condition = paste0("input['", ns("DRR_method"), "'] != 'LR' "),
                 numericInput(ns("DRF_nsamp"), label = "number of samplings", value = 1000, min = 10)
                 #leave the rest empty first ...
               ),
               actionButton(ns('startDRF'), 'DR fitness analysis', icon=icon("play"), class="btn-success"),



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
