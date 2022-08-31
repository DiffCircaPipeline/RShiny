setting_ui <- function(id, label = "global setting"){
  ns <- NS(id)
  tabPanel("Welcome", value=id,
           h2("Welcome to DiffCircaPipeline", align = "middle", style="primary"),
           tags$hr(),
           fluidRow(
             HTML('<center><img src="RShiny_flowchart.pdf"  width="1500"></center>'),
             HTML('<p>DiffCircaPipeline is designed for comparing circadian rhythm of two contrasting conditions. We model the rhythmicity signal by cosinor model. The rhythmicity characteristics of interest are oscillation amplitude (A), phase (&phi;), the midline-estimating statistic of rhythm (MESOR, later M), and the rhythm fitness (R<sup>2</sup>) to the cosinor model. </p>'),
             HTML('<p><width="1200">The DiffCircaPipeline Rshiny contains four modules:  <ul><li>Data input and output: input data and time points and set output directory. </li><li>Joint Rhythmicity: (flowchart step (a)) Categorize genes to four joint rhythmicity types. This step prepares gene sets for biological meaningful differential rhythmicity (DR) tests since DR parameter is only meaningful for genes rhythmic in both groups (RhyBoth) and DR fitness is only meaningful for genes rhythmic in at least one group. </li><li>Differential rhythmicity tests: (flowchart step (b)) perform DR fitness and/or DR parameter test to selected gene sets from step a. </li><li>Visualization: makes scatter plots, heatmaps, radar plots, or circos phase difference plots. </li></ul> </p>'),
             style="text-indent: 20px; font-size: 16px; margin-left: 20px; margin-right: 20px"),
           tags$hr(),
           mainPanel(
             h2("Session Information"),
             verbatimTextOutput(ns("urlText")),
           )
  )
}
