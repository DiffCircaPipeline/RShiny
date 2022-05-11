# Setting the maximum file upload limit to 100 MB
options(shiny.maxRequestSize=100*1024^2)
shinyServer(function(input, output, session) {
  # installed=installed.packages()[,"Package"]

  #set global reactiveValues
  rvDB <- reactiveValues(working.dir = paste(getwd(), DB.dir, sep = "/"),
                         studyname = "STUDYNAME", studytype = "Two",
                         gIinfo = "Group I", gIIinfo = "Group II",
                         printCP.obj = FALSE, #will be TRUE after parameter estimation
                         TOJR = list(),
                         TOJRsummary = data.frame(NULL),
                         printDRP = FALSE, #will be TRUE after DRP analysis
                         printDRF = FALSE, #will be TRUE after DRF analysis
                         dA = FALSE, dPhase = FALSE, dM = FALSE, #will be TRUE if the parameter is included in the DRP analysis
                         update = 0)

  # output$urlText <- renderText({
  #   server.type <- ""
  #   if (session$clientData$url_hostname == "127.0.0.1")
  #     server.type <- "local"
  #   else
  #     server.type <- "remote"
  #   paste(sep = "",
  #         "protocol: ", session$clientData$url_protocol, "\n",
  #         "hostname: ", session$clientData$url_hostname, "\n",
  #         "port: ",     session$clientData$url_port,     "\n",
  #         "server type: ", server.type,     "\n"
  #   )
  # })

  callModule(setting_server, "setting")
  output_data_input <- callModule(data_input_server, "data_input", rvDB)
  output_joint_rhythm <- callModule(joint_rhythm_server, "joint_rhythm", rvDB)
  output_dr_anaylsis <- callModule(dr_analysis_server, "dr_anaylsis", rvDB)
  # callModule(cp_plots_server, "cp_plots")
  callModule(plots_scatter_server, "plots_scatter", rvDB)
  callModule(plots_heatmap_server, "plots_heatmap", rvDB)
  callModule(plots_radar_server, "plots_radar", rvDB)
  callModule(plots_phase_server, "plots_phase", rvDB) #mainly phase difference

  observeEvent(output_data_input$trigger, {
    # print("update rvDB in data_input")
    rvDB <- update_rvDB(rvDB, output_data_input)
    db <<- update_rvDB(db, rvDB)
    suppressWarnings(saveX(db, db, "db.rds"))
    # print("DUBUG GLOBAL data_input db")
    # print(db)
    #updateTabsetPanel(session, "mainTabSetPanel", selected = "Read")
  }, ignoreInit = TRUE)
  observeEvent(output_joint_rhythm$trigger, {
    # print("update rvDB in joint_rhythm")
    rvDB <- update_rvDB(rvDB, output_joint_rhythm)
    db <<- update_rvDB(db, rvDB)
    suppressWarnings(saveX(db, db, "db.rds"))
    # print("DUBUG GLOBAL joint_rhythm db")
    #updateTabsetPanel(session, "mainTabSetPanel", selected = "Read")
    # print(db)
  }, ignoreInit = TRUE)
  observeEvent(output_dr_anaylsis$trigger, {
    # print("update rvDB in dr_anaylsis")
    rvDB <- update_rvDB(rvDB, output_dr_anaylsis)
    db <<- update_rvDB(db, rvDB)
    suppressWarnings(saveX(db, db, "db.rds"))
    # print("DUBUG GLOBAL dr_anaylsis db")
    #updateTabsetPanel(session, "mainTabSetPanel", selected = "Read")
    # print(db)
  }, ignoreInit = TRUE)


  #  if (TOOLSET.de %in% installed)
  #    callModule(meta_de_server, "meta_de")
  #  if (TOOLSET.clust %in% installed)
  #    callModule(meta_clust_server, "meta_clust")
  #  if (TOOLSET.path %in% installed)
  #    callModule(meta_path_server, "meta_path")
  #  if (TOOLSET.dcn %in% installed)
  #    callModule(meta_dcn_server, "meta_dcn")

  # callModule(meta_pca_server,"meta_pca")
})
