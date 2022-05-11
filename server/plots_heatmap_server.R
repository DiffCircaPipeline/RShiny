plots_heatmap_server <- function(input, output, session, globalDB) {

  ns <- NS("plots_heatmap")
  output$Rhythm1 <- DT::renderDataTable({if(globalDB$printCP.obj){
    DT::datatable(CP.obj[[1]]$rhythm, options = list(scrollX = TRUE), filter = "top")
  }})
  output$Rhythm2 <- DT::renderDataTable({if(globalDB$printCP.obj){
    DT::datatable(CP.obj[[2]]$rhythm, options = list(scrollX = TRUE))
  }})
  output$DRPres <- DT::renderDataTable({if(globalDB$printDRP){
    DT::datatable(res.DRP, options = list(scrollX = TRUE))
  }})
  output$DRFres <- DT::renderDataTable({if(globalDB$printDRF){
    DT::datatable(res.DRF, options = list(scrollX = TRUE))
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
    print(input$Rhythm1_rows_selected)
    # check.plot(globalDB, "CP.obj")
    a.plot$g = CP.obj[[1]]$rhythm$gname[input$Rhythm1_rows_selected]
    print(paste0("The gene name: ", a.plot$g))
    print("1.g")
    shinyjs::reset("Rhythm2_rows_selected"); shinyjs::reset("DRPres_rows_selected"); shinyjs::reset("DRFres_rows_selected");
  })
  observeEvent(input$Rhythm2_rows_selected, {
    print(input$Rhythm2_rows_selected)
    # check.plot(globalDB, "CP.obj")
    a.plot$g = CP.obj[[2]]$rhythm$gname[input$Rhythm2_rows_selected]
    print(paste0("The gene name: ", a.plot$g))
    # g.sel.reset(Rhythm2 = input$Rhythm2_rows_selected)
    shinyjs::reset("Rhythm1_rows_selected"); shinyjs::reset("DRPres_rows_selected"); shinyjs::reset("DRFres_rows_selected");
  })
  observeEvent(input$DRPres_rows_selected, {
    print(input$DRPres_rows_selected)
    # check.plot(globalDB, "CP.obj")
    a.plot$g = res.DRP$gname[input$DRPres_rows_selected]
    print(paste0("The gene name: ", a.plot$g))
    # g.sel.reset(DRP = input$DRP_rows_selected)
    shinyjs::reset("Rhythm1_rows_selected"); shinyjs::reset("Rhythm2_rows_selected"); shinyjs::reset("DRFres_rows_selected");
  })
  observeEvent(input$DRFres_rows_selected, {
    print(input$DRFres_rows_selected)
    # check.plot(globalDB, "CP.obj")
    a.plot$g = res.DRF$gname[input$DRFres_rows_selected]
    print(paste0("The gene name: ", a.plot$g))
    # g.sel.reset(DRF = input$DRF_rows_selected)
    shinyjs::reset("Rhythm1_rows_selected"); shinyjs::reset("Rhythm2_rows_selected"); shinyjs::reset("DRFres_rows_selected");
  })
  observeEvent(input$selectAllRows, {
    # check.plot(globalDB, "CP.obj")
    if(input$selectAllRows){
      print("selectAllRows TRUE")
      if(input$selectAllRowsSource == "Rhythm1"){
        a.plot$g = CP.obj[[1]]$rhythm$gname[input$Rhythm1_rows_all]
      }else if(input$selectAllRowsSource == "Rhythm2"){
        a.plot$g = CP.obj[[2]]$rhythm$gname[input$Rhythm2_rows_all]
      }else if(input$selectAllRowsSource == "DRPres"){
        a.plot$g = res.DRP$gname[input$DRPres_rows_all]
      }else if(input$selectAllRowsSource == "DRFres"){
        a.plot$g = res.DRF$gname[input$DRFres_rows_all]
      }
      print(paste0("The gene name: ", a.plot$g))
      # g.sel.reset(DRF = input$DRF_rows_selected)
    }
  })


  observeEvent(input$saveHM, {
    pdf(paste0(globalDB$working.dir, "/", input$p_scatter_name, ".pdf"))
    print( a.plot$p)
    # gridExtra::grid.arrange(a.plot$p[[1]][[1]], a.plot$p[[2]][[1]], ncol = 2)
    dev.off()
  })


  # ##########################
  # # Render output/UI       #
  # ##########################
  output$heatmap <- renderPlot({
    a.plot$p = DiffCircaPipeline::DCP_PlotHeatmap(CP.obj, genes.plot = a.plot$g, Info1 = db$gIinfo, Info2 = db$gIIinfo)
    a.plot$p
  })

}
