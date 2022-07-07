plots_scatter_server <- function(input, output, session, globalDB) {

  ns <- NS("plots_scatter")
  output$Rhythm1 <- DT::renderDataTable({if(globalDB$printCP.obj&globalDB$studytype == "Two"){
    DT::datatable(round2(CP.obj[[1]]$rhythm, 3), selection = 'single', options = list(scrollX = TRUE), rownames = FALSE)
  }else if(globalDB$printCP.obj&globalDB$studytype == "One"){
    DT::datatable(round2(CP.obj$rhythm, 3), selection = 'single', options = list(scrollX = TRUE), rownames = FALSE)
  }})
  output$Rhythm2 <- DT::renderDataTable({if(globalDB$printCP.obj&globalDB$studytype == "Two"){
    DT::datatable(round2(CP.obj[[2]]$rhythm, 3), selection = 'single', options = list(scrollX = TRUE), rownames = FALSE)
  }})
  output$DRPres <- DT::renderDataTable({if(globalDB$printDRP){
    DT::datatable(round2(res.DRP, 3), selection = 'single', options = list(scrollX = TRUE), rownames = FALSE)
  }})
  output$DRFres <- DT::renderDataTable({if(globalDB$printDRF){
    DT::datatable(round2(res.DRF, 3), selection = 'single', options = list(scrollX = TRUE), rownames = FALSE)
  }})

  # ##########################
  # # Reactive Values        #
  # ##########################
  row.select <- reactiveValues(Rhythm1 = NULL,
                               Rhythm2 = NULL,
                               DRPres = NULL,
                               DRFres = NULL)
  a.plot <- reactiveValues(g = NULL,
                           p = NULL,
                           p2 = NULL)

  # ##########################
  # # Observers              #
  # ##########################
  observeEvent(input$Rhythm1_rows_selected, {
    # check.plot(globalDB, "CP.obj")
    # print("1.in")
    # print(input$Rhythm1_rows_selected)
    if(globalDB$studytype == "One"){
      a.plot$g = CP.obj$rhythm$gname[input$Rhythm1_rows_selected]
      }else if(globalDB$studytype == "Two"){
      a.plot$g = CP.obj[[1]]$rhythm$gname[input$Rhythm1_rows_selected]
    }
    # print(paste0("The gene name: ", a.plot$g))
    # print("1.g")
    shinyjs::reset("Rhythm2_rows_selected"); shinyjs::reset("DRPres_rows_selected"); shinyjs::reset("DRFres_rows_selected");
    # print("1.sel")
    # a.plot$p = DiffCircaPipeline::DCP_ScatterPlot(CP.obj, genes.plot = a.plot$g, Info1 = db$gIinfo, Info2 = db$gIIinfo)
    # print(a.plot$p)
    # a.plot$p = gridExtra::grid.arrange(a.plot$p[[1]][[1]], a.plot$p[[2]][[1]], ncol = 2)
    # print("1.plot")
  })
  observeEvent(input$Rhythm2_rows_selected, {
    # check.plot(globalDB, "CP.obj")
    # print(input$Rhythm2_rows_selected)
    a.plot$g = CP.obj[[2]]$rhythm$gname[input$Rhythm2_rows_selected]
    # print(paste0("The gene name: ", a.plot$g))
    # g.sel.reset(Rhythm2 = input$Rhythm2_rows_selected)
    shinyjs::reset("Rhythm1_rows_selected"); shinyjs::reset("DRPres_rows_selected"); shinyjs::reset("DRFres_rows_selected");
    # a.plot$p = DiffCircaPipeline::DCP_ScatterPlot(CP.obj, genes.plot = a.plot$g, Info1 = db$gIinfo, Info2 = db$gIIinfo)
    # print(a.plot$p)
    # a.plot$p = gridExtra::grid.arrange(a.plot$p[[1]][[1]], a.plot$p[[2]][[1]], ncol = 2)
  })
  observeEvent(input$DRPres_rows_selected, {
    # check.plot(globalDB, "CP.obj")
    # print(input$DRPres_rows_selected)
    a.plot$g = res.DRP$gname[input$DRPres_rows_selected]
    # print(paste0("The gene name: ", a.plot$g))
    # g.sel.reset(DRP = input$DRP_rows_selected)
    shinyjs::reset("Rhythm1_rows_selected"); shinyjs::reset("Rhythm2_rows_selected"); shinyjs::reset("DRFres_rows_selected");
    # a.plot$p = DiffCircaPipeline::DCP_ScatterPlot(CP.obj, genes.plot = a.plot$g, Info1 = db$gIinfo, Info2 = db$gIIinfo)
    # a.plot$p = gridExtra::grid.arrange(a.plot$p[[1]][[1]], a.plot$p[[2]][[1]], ncol = 2)
    # print(a.plot$p)
  })
  observeEvent(input$DRFres_rows_selected, {
    # check.plot(globalDB, "CP.obj")
    # print(input$DRFres_rows_selected)
    a.plot$g = res.DRF$gname[input$DRFres_rows_selected]
    # print(paste0("The gene name: ", a.plot$g))
    # g.sel.reset(DRF = input$DRF_rows_selected)
    shinyjs::reset("Rhythm1_rows_selected"); shinyjs::reset("Rhythm2_rows_selected"); shinyjs::reset("DRFres_rows_selected");
    # a.plot$p = DiffCircaPipeline::DCP_ScatterPlot(CP.obj, genes.plot = a.plot$g, Info1 = db$gIinfo, Info2 = db$gIIinfo)
    # a.plot$p = gridExtra::grid.arrange(a.plot$p[[1]][[1]], a.plot$p[[2]][[1]], ncol = 2)
    # print(a.plot$p)
  })
  observeEvent(input$plotPasteScatter, {
    if(input$pasteScatter==""){
      showNotification(
        ui = paste0("Please input a gene list (a gene at a line) or copy and paste from an excel spreadsheet column. "),
        type = "error",
        duration = NULL
      )
    }else{
      a.plot$g = unlist(strsplit(input$pasteScatter, "\n"))
    }
  })

  observeEvent(input$saveScatter, {
    if(globalDB$studytype == "One"){
      pdf(paste0(db$working.dir, "/", input$p_scatter_name, ".pdf"),
          height = input$p_scatter_hei, width = input$p_scatter_wid, onefile = TRUE)
      print(a.plot$p)
      dev.off()
    }else if(globalDB$studytype == "Two"){
      pdf(paste0(db$working.dir, "/", input$p_scatter_name, ".pdf"),
          height = input$p_scatter_hei, width = input$p_scatter_wid, onefile = TRUE)
      print(lapply(1:length(a.plot$p[[1]]), function(a.g){
        gridExtra::grid.arrange(a.plot$p[[1]][[a.g]], a.plot$p[[2]][[a.g]], ncol = 2)
      }))
      dev.off()
    }
    print(paste0("The plot is saved to ", db$working.dir))
  })


  # ##########################
  # # Render output/UI       #
  # ##########################
  output$SelectGeneForScatter <- renderUI({
    if(globalDB$studytype == "One"){
      output <- tagList()
      output[[1]] <- tabsetPanel(
        tabPanel("Parameter estimates",
                 DT::dataTableOutput(ns("Rhythm1"))
        ),
        tabPanel("Self-defined gene list",
                 textAreaInput(ns("pasteScatter"), "Paste a gene list: ", placeholder = "Input a gene list (a gene at a line) or copy and paste from an excel spreadsheet column"),
                 actionButton(ns('plotPasteScatter'), 'Plot self-defined gene list', icon=icon("play"), class="btn-success")
        )
      )
    }else if(globalDB$studytype == "Two"){
      output <- tagList()
      output[[1]] <- tabsetPanel(

        tabPanel("Parameter estimates",
                 fluidRow(width = 12,
                          column(6, h5("Group I"), DT::dataTableOutput(ns("Rhythm1"), width = "auto")),
                          column(6, h5("Group II"), DT::dataTableOutput(ns("Rhythm2"), width = "auto"))
                 )
        ),
        tabPanel("DR parameter result",
                 DT::dataTableOutput(ns("DRPres"))
        ),
        tabPanel("DR fitness result",
                 DT::dataTableOutput(ns("DRFres"))
        ),
        tabPanel("Self-defined gene list",
                 textAreaInput(ns("pasteScatter"), "Paste a gene list: ", placeholder = "Input a gene list (a gene at a line) or copy and paste from an excel spreadsheet column"),
                 actionButton(ns('plotPasteScatter'), 'Plot self-defined gene list', icon=icon("play"), class="btn-success")
        )
      )
    }
    output
  })
  
  output$scatterPlot <- renderPlot({
    if(!is.null(a.plot$g)){
      print(paste0("Genes being plotted are: ", a.plot$g))
      if(globalDB$studytype == "One"){
        a.plot$p = DiffCircaPipeline::DCP_ScatterPlot(CP.obj, genes.plot = a.plot$g, Info1 = db$studyname)
        gridExtra::grid.arrange(grobs = a.plot$p, ncol = 1)
      }else if(globalDB$studytype == "Two"){
        a.plot$p = DiffCircaPipeline::DCP_ScatterPlot(CP.obj, genes.plot = a.plot$g, Info1 = db$gIinfo, Info2 = db$gIIinfo)
        a.plot$p2 <- DiffCircaPipeline::DCP_PlotDisplay(a.plot$p)
        gridExtra::grid.arrange(grobs = a.plot$p2, ncol = 1)
      }
    }
  })
  
  output$scatterPlot_ui <- renderUI({
    if(!is.null(a.plot$g)){
      plotOutput(ns("scatterPlot"), height = 400*length(a.plot$g), width = ifelse(db$studytype == "One", 400, 800))
    }
})

}
