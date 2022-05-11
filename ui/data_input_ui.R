data_input_ui <- function(id, label= "data input") {
  ns <- NS(id)

  tabPanel("Data Input and Result Output", value=id,
           sidebarLayout(
             sidebarPanel(
               # useShinyjs(),
               ##########################
               # Upload Data
               ##########################
               helpIcon("data_input_help", HTML('<p>For each group, upload a data file and a time file. <ul><li>The data file has gene in rows and samples in columns. The top row should be sample ID and the first column should be gene names.</li><li>The time file has at least two columns with header "time" and "SampleID". The column SampleID should contain the same identifiers as in the data file. </li></ul> </p>')),
               h3("Upload data", style="display:inline"),

               radioButtons(ns("uploadOrLoad"), label = "Continue from existing analysis? ",
                            choices = list("Yes and choose directory" = 1,
                                           "No and upload data" = 2),
                            selected = 2),

               conditionalPanel(
                 condition = paste0("input['", ns("uploadOrLoad"), "'] == '1' "),
                 h3("Load study", style="display:inline"),
                 directoryInput(ns('directoryLoad'), label='choose directory for existing study'),
                 actionButton(ns('loadStudy'), 'Load', icon=icon("download"), class="btn-success")

               ),

               conditionalPanel(
                 condition = paste0("input['", ns("uploadOrLoad"), "'] == '2' "),


                 #### choose single study analysis or comparison analysis
                 radioButtons(ns("inputType"), label = "Choose study type: ",
                              choices = list("Rhythmimicy comparison of two groups" = 1,
                                             "Rhythmicity analysis of a single group (no differential rhythmicity analysis)" = 2),
                              selected = 1),

                 #### if input two data sets
                 conditionalPanel(
                   condition = paste0("input['", ns("inputType"), "'] == '1' "),

                   textInput(ns("gIinfo"), "Group I label: ", value = ""),
                   fileInput(ns("expr1"), 'Upload group I gene expression data file (.csv)',
                             accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
                   ),
                   fileInput(ns("time1"), 'Upload group I time of expression file (.csv)',
                             accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
                   ),
                   textInput(ns("gIIinfo"), "Group II label: ", value = ""),
                   fileInput(ns("expr2"), 'Upload group II gene expression data file (.csv)',
                             accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
                   ),
                   fileInput(ns("time2"), 'Upload group II time of expression file (.csv)',
                             accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
                   ),
                 ),

                 #### if input one data set
                 conditionalPanel(
                   condition = paste0("input['", ns("inputType"), "'] == '2' "),
                   fileInput(ns("expr0"), 'Upload group I gene expression data file (.csv)',
                             accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
                   ),
                   fileInput(ns("time0"), 'Upload group I time of expression file (.csv)',
                             accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
                   )
                 ),

                 ##########################
                 # Save the study         #
                 ##########################
                 # helpIcon("working_dir_help", "During the computation, some output files or images are automatically saved to this directory."),
                 h2("Output directory:", style="display:inline"),
                 directoryInput(ns('directory'), label='select an output directory for the study'),
                 textInput(ns("studyName"), "Study name:", value = ""),

                 actionButton(ns('saveStudy'), 'Save', icon=icon("save"), class="btn-success")
               )

             ),

             mainPanel(
               h3("Study summary"),
               # DT::dataTableOutput(ns("designSummary")),
               tableOutput(ns("designSummary")),
               # fluidRow(column(12, tableOutput(ns("designSummary")), allign = "center")), #do not know how to center
               hr(),
               h3("Distribution of sampling time"),
               plotOutput(ns('timeSummary')),
             )
           )
  )
}
