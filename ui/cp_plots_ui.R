cp_plots_ui <- function(id, label = "cp_plots"){
  ns <- NS(id)
  tabPanel("cp_plots")
  fluidPage(

    title = 'Visualization',

    h1('A Client-side Table'),

    fluidRow(
      column(6, DT::dataTableOutput('x1')),
      column(6, plotOutput('x2', height = 500))
    ),

    hr(),

    h1('A Server-side Table'),

    fluidRow(
      column(9, DT::dataTableOutput('x3')),
      column(3, verbatimTextOutput('x4'))
    )

  )
}
