plots_phase_ui2 <- function(id, label = "Plot phase"){
  ns <- NS(id)
  tabPanel("Make A circos phase plot", value = id,
           sidebarLayout(
             sidebarPanel(



               radioButtons(ns("intype"), 'Data or Statistics?',
                            c(Data="d", Statistics="s")),

               #Only show this if sample data are to be entered
               # conditionalPanel(
               #   # condition = "input.intype == 'd'",
               #   # condition = paste0("input['", ns("intype"), "'] == 'd' "),
               #   condition = paste0("input['", ns("intype"), "'] == 'd' "),
               #   textInput(ns('x'), 'Sample Data (Comma Separated)', value='')),

               #Only shows this panel for summary statitics (sd or sigma if known)
               # conditionalPanel(
               #   # condition = "input.intype == 's'",
               #   # condition = paste0("input['", ns("intype"), "'] == 's' "),
               #   condition = paste0("input['", ns("intype"), "'] == 's' "),
               #   numericInput(ns('m'),'Sample Mean',value=''),
               #   numericInput(ns('sd'),'SD (Population or Sample)',value=''),
               #   numericInput(ns('n'),'Sample size',value='')),

               uiOutput(ns('dynamic1'))

             ),
             mainPanel(
               # plotOutput(ns("APlot")),
               textOutput(ns("intype2"))
             )
           )
           )
}
