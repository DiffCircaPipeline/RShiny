dr_analysis_ui <- function(id, label= "dr analysis") {
  ns <- NS(id)

  tabPanel("Differential Rhythmicity Analysis", value=id,
           sidebarLayout(
             sidebarPanel(
               # useShinyjs(),
               ##########################
               # Upload Data
               ##########################
               numericInput(ns("DR_parallel.ncores"), "Number of cores for computation:", value = 1, min = 1,
                            max = ifelse(.Platform$OS.type != "windows", parallel::detectCores(), 1)),
               h2("Differential Parameters"),
               selectInput(ns("DRP_par"), "Parameters to test",
                           choices = c("amplitude" = "A",
                                       "phase" = "phase",
                                       "MESOR" = "M",
                                       "amplitude and phase" = "A&phase",
                                       "amplitude and phase and MESOR" = "A&phase&M"),
                           selected = "phase"),
               numericInput(ns("DRP_criticalValue"), label = "Critical value", value = 0.05, min = 0, max = 1),
               actionButton(ns('startDRP'), 'DR parameter analysis', icon=icon("play"), class="btn-success"),

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
               h3("DR parameter result"),
               DT::dataTableOutput(ns("DRP")),
               h3("DR fitness result"),
               DT::dataTableOutput(ns("DRF"))
             )
           )
  )
}
