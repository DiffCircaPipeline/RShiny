dr_fit_ui <- function(id, label= "dr fitness") {
  ns <- NS(id)

  tabPanel("Differential Rhythm Fitness", value=id,
           sidebarLayout(
             sidebarPanel(
               # useShinyjs(),
               ##########################
               # Upload Data
               ##########################
               h2("Differential Fitness"),
               selectInput(ns("DRR_method"), "Method of differential $$R^2$$ test",
                           choices = c("likelihood ratio test (Highly recommended)" = "LR",
                                       "permutation" = "permutation",
                                       "bootstrap" = "bootstrap"),
                           selected = "likelihood ratio test (Highly recommended)"),
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
