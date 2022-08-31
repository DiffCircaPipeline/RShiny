shinyUI(
  navbarPage("DiffCircaPipeline", id="nav",
             # h2("Welcome to CircadianPipeline", align = "middle", style="primary"),
             # header=tagList(
             #   tags$div(id="working-dir",
             #            tagList(
             #              tags$p("Working Directory", class="header-label"),
             #              tags$p(textOutput("setting-working.dir"))
             #            )
             #   ),
             #   tags$div(id="active-study",
             #            tagList(
             #              tags$p("Active Study", class="header-label"),
             #              tags$div(tags$p(textOutput("saved_data-activated")))
             #            )
             #   )
             # ),
             # tab for global settings
             setting_ui("setting"), #actually not setting rather welcome page.
             # tab for data input
             data_input_ui("data_input"), #input processed data -> transform into formatted CP_Rhythmicity input and calculate. Could choose input 1 or 2. Could choose input CP_Rhythmicity output.
             # tab for performing joint rhythmicity analysis
             joint_rhythm_ui("joint_rhythm"), #output results for joint_rhythm. You can choose the method and the p-value cutoff. Would produce a summary table for number
             # tab for DR tests
             navbarMenu("Differential Rhythmicity Analysis",
                        dr_param_ui("dr_param"),
                        dr_fit_ui("dr_fit")
             ),
             # tab for visualization
             navbarMenu("Visualization",
                        plots_scatter_ui("plots_scatter"),
                        plots_heatmap_ui("plots_heatmap"),
                        plots_radar_ui("plots_radar"),#radar plot and histogram
                        plots_phase_ui("plots_phase") #mainly phase difference
                        )
             # toolsets,
             # tags$div(
             #   tags$div(id="loading",
             #            tags$div(id="loadingcontent",
             #                     tags$p(id="loadingspinner", "loading......")
             #            )
             #   )
             # ),
             # # Including css and javascripts in head section
             # ,tags$head(
             #   tags$link(rel = "stylesheet", type = "text/css", href = "css/messenger.css"),
             #   tags$link(rel = "stylesheet", type = "text/css", href = "css/messenger-theme-future.css"),
             #   tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
             #   tags$script(src="js/spin.min.js"),
             #   tags$script(src="js/messenger.min.js"),
             #   tags$script(src="js/message-handler.js")
             # )
             ,tags$head(
               tags$style(
                 HTML(".shiny-notification {
             position:fixed;
             top: calc(80%);
             left: calc(60%);
             }
             "
                 )
               )
             )
  )
)
