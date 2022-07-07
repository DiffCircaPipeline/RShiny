plots_heatmap_server <- function(input, output, session, globalDB) {

  ns <- NS("plots_heatmap")
  selectAllRowsSource.choice = c("none", "Rhythm1", "Rhythm2", "DRPres", "DRFres")
  names(selectAllRowsSource.choice) = c("none",
                                        paste0("Parameter estimates of ", db$gIinfo),
                                        paste0("Parameter estimates of ", db$gIIinfo),
                                        "DR parameter result",
                                        "DR fitness result")

  output$Rhythm1 <- DT::renderDataTable({if(globalDB$printCP.obj&globalDB$studytype == "Two"){
    DT::datatable(round2(CP.obj[[1]]$rhythm, 3), options = list(scrollX = TRUE), filter = "top", rownames = FALSE)
  }else if(globalDB$printCP.obj&globalDB$studytype == "One"){
    DT::datatable(round2(CP.obj$rhythm, 3), options = list(scrollX = TRUE), filter = "top", rownames = FALSE)
  }})
  output$Rhythm2 <- DT::renderDataTable({if(globalDB$printCP.obj&globalDB$studytype == "Two"){
    DT::datatable(round2(CP.obj[[2]]$rhythm, 3), options = list(scrollX = TRUE), filter = "top", rownames = FALSE)
  }})
  output$DRPres <- DT::renderDataTable({if(globalDB$printDRP){
    DT::datatable(round2(res.DRP, 3), options = list(scrollX = TRUE), filter = "top", rownames = FALSE)
  }})
  output$DRFres <- DT::renderDataTable({if(globalDB$printDRF){
    DT::datatable(round2(res.DRF, 3), options = list(scrollX = TRUE), filter = "top", rownames = FALSE)
  }})



  # ##########################
  # # Reactive Values        #
  # ##########################
  row.select <- reactiveValues(Rhythm1 = NULL,
                               Rhythm2 = NULL,
                               DRPres = NULL,
                               DRFres = NULL)
  a.plot <- reactiveValues(g = NULL,
                           p = NULL)

  # ##########################
  # # Observers              #
  # ##########################
  observeEvent(input$Rhythm1_rows_selected, {
    print("1.in")
    print(input$Rhythm1_rows_selected)
    if(globalDB$studytype == "One"){
      a.plot$g = CP.obj$rhythm$gname[input$Rhythm1_rows_selected]
    }else if(globalDB$studytype == "Two"){
      a.plot$g = CP.obj[[1]]$rhythm$gname[input$Rhythm1_rows_selected]
    }
    print("1.g")
    print(paste0("The gene name: ", a.plot$g))
    shinyjs::reset("Rhythm2_rows_selected"); shinyjs::reset("DRPres_rows_selected"); shinyjs::reset("DRFres_rows_selected");
  })
  observeEvent(input$Rhythm2_rows_selected, {
    # print(input$Rhythm2_rows_selected)
    # check.plot(globalDB, "CP.obj")
    a.plot$g = CP.obj[[2]]$rhythm$gname[input$Rhythm2_rows_selected]
    # print("2.g")
    # print(paste0("The gene name: ", a.plot$g))
    shinyjs::reset("Rhythm1_rows_selected"); shinyjs::reset("DRPres_rows_selected"); shinyjs::reset("DRFres_rows_selected");
  })
  observeEvent(input$DRPres_rows_selected, {
    # print(input$DRPres_rows_selected)
    # check.plot(globalDB, "CP.obj")
    a.plot$g = res.DRP$gname[input$DRPres_rows_selected]
    # print("DRP.g")
    # print(paste0("The gene name: ", a.plot$g))
    shinyjs::reset("Rhythm1_rows_selected"); shinyjs::reset("Rhythm2_rows_selected"); shinyjs::reset("DRFres_rows_selected");
  })
  observeEvent(input$DRFres_rows_selected, {
    # print(input$DRFres_rows_selected)
    # check.plot(globalDB, "CP.obj")
    a.plot$g = res.DRF$gname[input$DRFres_rows_selected]
    # print("DRF.g")
    # print(paste0("The gene name: ", a.plot$g))
    shinyjs::reset("Rhythm1_rows_selected"); shinyjs::reset("Rhythm2_rows_selected"); shinyjs::reset("DRFres_rows_selected");
  })

  observeEvent(c(input$selectAllRows,
                 input$selectAllRowsSource,
                 input$Rhythm1_rows_all,
                 input$Rhythm2_rows_all,
                 input$DRPres_rows_all,
                 input$DRFres_rows_all), {
                   # print("DEBUG: entered select all rws")
                   if(input$selectAllRows){
                     if(globalDB$studytype == "One"){
                       a.plot$g = CP.obj$rhythm$gname[input$Rhythm1_rows_all]
                     }else if(globalDB$studytype == "Two"){
                       if(input$selectAllRowsSource == "Rhythm1"){
                         a.plot$g = CP.obj[[1]]$rhythm$gname[input$Rhythm1_rows_all]
                       }else if(input$selectAllRowsSource == "Rhythm2"){
                         a.plot$g = CP.obj[[2]]$rhythm$gname[input$Rhythm2_rows_all]
                       }else if(input$selectAllRowsSource == "DRPres"){
                         a.plot$g = res.DRP$gname[input$DRPres_rows_all]
                       }else if(input$selectAllRowsSource == "DRFres"){
                         a.plot$g = res.DRF$gname[input$DRFres_rows_all]
                       }
                     }
                     
                   }

                 })


  observeEvent(input$plotPasteHM, {
    if(input$pasteHM==""){
      showNotification(
        ui = paste0("Please input a gene list (a gene at a line) or copy and paste from an excel spreadsheet column. "),
        type = "error",
        duration = NULL
      )
    }else{
      a.plot$g = unlist(strsplit(input$pasteHM, "\n"))
    }
  })

  # observeEvent(input$selectAllRows, {
  #   # check.plot(globalDB, "CP.obj")
  #   if(input$selectAllRows){
  #     # print("selectAllRows TRUE")
  #     if(input$selectAllRowsSource == "Rhythm1"){
  #       if(globalDB$studytype == "One"){
  #         a.plot$g = CP.obj$rhythm$gname[input$Rhythm1_rows_all]
  #       }else if(globalDB$studytype == "Two"){
  #         a.plot$g = CP.obj[[1]]$rhythm$gname[input$Rhythm1_rows_all]
  #       }
  #     }else if(input$selectAllRowsSource == "Rhythm2"){
  #       a.plot$g = CP.obj[[2]]$rhythm$gname[input$Rhythm2_rows_all]
  #     }else if(input$selectAllRowsSource == "DRPres"){
  #       a.plot$g = res.DRP$gname[input$DRPres_rows_all]
  #     }else if(input$selectAllRowsSource == "DRFres"){
  #       a.plot$g = res.DRF$gname[input$DRFres_rows_all]
  #     }
  #     # print(paste0("The gene name: ", a.plot$g))
  #     # g.sel.reset(DRF = input$DRF_rows_selected)
  #   }
  # })


  observeEvent(input$saveHM, {
    if(globalDB$studytype == "One"){
      pdf(paste0(db$working.dir, "/", input$p_scatter_name, ".pdf"))
      print(a.plot$p)
      dev.off()
    }else if(globalDB$studytype == "Two"){
      pdf(paste0(globalDB$working.dir, "/", input$p_scatter_name, ".pdf"), width = 14)
      print(a.plot$p)
      dev.off()
    }
  })


  # ##########################
  # # Render output/UI       #
  # ##########################
  output$SelectGeneForHM <- renderUI({
    if(globalDB$studytype == "One"){
      output <- tagList()
      output[[1]] <- tabsetPanel(
        tabPanel("Parameter estimates",
                 DT::dataTableOutput(ns("Rhythm1"))
        ),
        tabPanel("Self-defined gene list",
                 textAreaInput(ns("pasteHM"), "Paste a gene list: ", placeholder = "Input a gene list (a gene at a line) or copy and paste from an excel spreadsheet column"),
                 actionButton(ns('plotPasteHM'), 'Plot self-defined gene list', icon=icon("play"), class="btn-success")
        )
      )
      output[[2]] <- checkboxInput(ns("selectAllRows"), label = "Select all", value = FALSE)
      
    }else if(globalDB$studytype == "Two"){
      output <- tagList()
      output[[1]] <- tabsetPanel(
        tabPanel("Parameter estimates",
                 fluidRow(width = 12,
                          column(6,
                                 h4(paste0("Parameter estimates of ", db$gIinfo)),
                                 DT::dataTableOutput(ns("Rhythm1"), width = "auto")),
                          column(6,
                                 h4(paste0("Parameter estimates of ", db$gIIinfo)),
                                 DT::dataTableOutput(ns("Rhythm2"), width = "auto"))
                 )
        ),
        tabPanel("DR parameter result",
                 DT::dataTableOutput(ns("DRPres"))
        ),
        tabPanel("DR fitness result",
                 DT::dataTableOutput(ns("DRFres"))
        ),
        tabPanel("Self-defined gene list",
                 textAreaInput(ns("pasteHM"), "Paste a gene list: ", placeholder = "Input a gene list (a gene at a line) or copy and paste from an excel spreadsheet column"),
                 actionButton(ns('plotPasteHM'), 'Plot self-defined gene list', icon=icon("play"), class="btn-success")
        )
      )
      output[[2]] <- checkboxInput(ns("selectAllRows"), label = "Select all", value = FALSE)
      output[[3]] <- conditionalPanel(
        condition = paste0("input['", ns("selectAllRows"), "'] == TRUE "),
        selectInput(ns("selectAllRowsSource"), "Which gene list to plot all:",
                    selectAllRowsSource.choice
                    # choices = c("Parameter estimates of group I" = "Rhythm1",
                    #             "Parameter estimates of group II" = "Rhythm2",
                    #             "DR parameter result" = "DRPres",
                    #             "DR fitness result" = "DRFres"
                    #             )
        )
      )
    }
    output
  })
  
  output$heatmap <- renderPlot({
    print("DEBUG: which one?")
    print(globalDB$studytype)
    if(globalDB$studytype == "One"){
      print("DEBUG: renderPlot a.plot$g")
      print(a.plot$g)
      a.plot$p = DiffCircaPipeline::DCP_PlotHeatmap(CP.obj, genes.plot = a.plot$g, Info1 = db$studyname)
      print("Heatmap plotted. ")
      a.plot$p
    }else if(globalDB$studytype == "Two"){
      # print("DEBUG: renderPlot a.plot$g")
      a.plot$p = DiffCircaPipeline::DCP_PlotHeatmap(CP.obj, genes.plot = a.plot$g, Info1 = db$gIinfo, Info2 = db$gIIinfo)
      print("Heatmap plotted. ")
      a.plot$p
    }
    })
  
  output$heatmap_ui <- renderUI({
    if(!is.null(a.plot$g)){
      plotOutput(ns("heatmap"), height = 400, width = ifelse(db$studytype == "One", 400, 800))
    }
  })

}
