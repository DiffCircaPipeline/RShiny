joint_rhythm_ui <- function(id, label= "joint rhythm"){
  ns <- NS(id)

  tabPanel("Joint Rhythmicity", value=id,
           sidebarLayout(
             sidebarPanel(
               uiOutput(ns("UIStudyOneOrTwo"))
               # useShinyjs(),
               ##########################
               # # Upload Data
               # ##########################
               # h3("Data"),
               # checkboxInput("inputCPobj", "Use data from previous analysis", FALSE), ##This place needs question mark, the working directory will be changed accordingly
               # conditionalPanel(
               #   condition = paste0("input['", ns("inputCPobj"), "'] == 'TRUE' "),
               #   directoryInput(ns('directory'), label='select working directory of the previous analysis')
               # ), #I think starting from data input is fine...This analysis is not taking long.

               # h3("Set parameters: "),
               # #### choose parameters for analysis
               # numericInput(ns("rhythmPeriod"), label = "Period", value = 24, min = 1, step = 1),
               # checkboxInput(ns("outputCI"), label = "Output confidence interval (CI)?", value = FALSE),
               # #remember to add that if it is single study analysis, the critical value is only used for CI coverage
               # numericInput(ns("criticalValue"), label = "Critical value", value = 0.05, min = 0, max = 1, step = 0.05),
               # hr(),
               # h6("Settings below are only used for rhythmimicy comparison of two groups: "),
               # numericInput(ns("criticalValueFDR"), label = "FDR", value = 0.05, min = 0, max = 1, step = 0.05),
               # selectInput(ns("TOJRmethod"), "Method for joint rhythmicity categorization:",
               #             choices = c("Sidak_FS" = "Sidak_FS", #left is shown in UI and right is the value for function.
               #               "Sidak_BS" = "Sidak_BS",
               #               "VDA" = "VDA",
               #               "AWFisher" = "AWFisher"),
               #             selected = "Sidak_FS"),
               # numericInput(ns("parallel.ncores"), "Number of cores for computation:", value = 1, min = 1,
               #              max = ifelse(.Platform$OS.type != "windows", parallel::detectCores(), 1)),
               # actionButton(ns('startTOJR'), 'Start analysis', icon=icon("play"), class="btn-success"),
               #
               # h3("Try other cutoffs"),
               # selectInput(ns("newTOJRcutType"), "type of cutoff:",
               #             choices = c("p-value" = "p-value",
               #               "q-value" = "q-value")),
               # selectInput(ns("newTOJRmethod"), "Method for joint rhythmicity categorization:",
               #             choices = c("Sidak_FS" = "Sidak_FS",
               #               "Sidak_BS" = "Sidak_BS",
               #               "VDA" = "VDA",
               #               "AWFisher" = "AWFisher"),
               #             selected = "Sidak_FS"),
               # numericInput(ns("newCriticalValue"), label = "Critical value", value = 0.05, min = 0, max = 1, step = 0.05),
               # actionButton(ns('startTOJR2'), 'Try the new cutoff', icon=icon("play"), class="btn-success")
             ),

             mainPanel(
               tabsetPanel(
                 tabPanel("Summary of joint rhythmicity", DT::dataTableOutput(ns("TOJRsummary"))),
                 # tabPanel("Parameter estimates",
                 #   h4("Parameter estimates I"),
                 #   DT::dataTableOutput(ns("Rhythm1")),
                 #   h4("Parameter estimates II"),
                 #   DT::dataTableOutput(ns("Rhythm2"))
                 # )
                 uiOutput("RhythmEst")
               )

             )
           )
  )
}
