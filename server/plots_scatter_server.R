plots_scatter_server <- function(input, output, session, globalDB) {

  ns <- NS("plots_scatter")
  output$Rhythm1 <- DT::renderDataTable({if(globalDB$printCP.obj){
    DT::datatable(round2(CP.obj[[1]]$rhythm, 3), selection = 'single', options = list(scrollX = TRUE), rownames = FALSE)
  }})
  output$Rhythm2 <- DT::renderDataTable({if(globalDB$printCP.obj){
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
                           p = NULL)

  # ##########################
  # # Observers              #
  # ##########################
  observeEvent(input$Rhythm1_rows_selected, {
    # check.plot(globalDB, "CP.obj")
    print("1.in")
    print(input$Rhythm1_rows_selected)
    a.plot$g = CP.obj[[1]]$rhythm$gname[input$Rhythm1_rows_selected]
    print(paste0("The gene name: ", a.plot$g))
    print("1.g")
    shinyjs::reset("Rhythm2_rows_selected"); shinyjs::reset("DRPres_rows_selected"); shinyjs::reset("DRFres_rows_selected");
    # print("1.sel")
    # a.plot$p = DiffCircaPipeline::DCP_ScatterPlot(CP.obj, genes.plot = a.plot$g, Info1 = db$gIinfo, Info2 = db$gIIinfo)
    # print(a.plot$p)
    # a.plot$p = gridExtra::grid.arrange(a.plot$p[[1]][[1]], a.plot$p[[2]][[1]], ncol = 2)
    # print("1.plot")
  })
  observeEvent(input$Rhythm2_rows_selected, {
    # check.plot(globalDB, "CP.obj")
    print(input$Rhythm2_rows_selected)
    a.plot$g = CP.obj[[2]]$rhythm$gname[input$Rhythm2_rows_selected]
    print(paste0("The gene name: ", a.plot$g))
    # g.sel.reset(Rhythm2 = input$Rhythm2_rows_selected)
    shinyjs::reset("Rhythm1_rows_selected"); shinyjs::reset("DRPres_rows_selected"); shinyjs::reset("DRFres_rows_selected");
    # a.plot$p = DiffCircaPipeline::DCP_ScatterPlot(CP.obj, genes.plot = a.plot$g, Info1 = db$gIinfo, Info2 = db$gIIinfo)
    # print(a.plot$p)
    # a.plot$p = gridExtra::grid.arrange(a.plot$p[[1]][[1]], a.plot$p[[2]][[1]], ncol = 2)
  })
  observeEvent(input$DRPres_rows_selected, {
    # check.plot(globalDB, "CP.obj")
    print(input$DRPres_rows_selected)
    a.plot$g = res.DRP$gname[input$DRPres_rows_selected]
    print(paste0("The gene name: ", a.plot$g))
    # g.sel.reset(DRP = input$DRP_rows_selected)
    shinyjs::reset("Rhythm1_rows_selected"); shinyjs::reset("Rhythm2_rows_selected"); shinyjs::reset("DRFres_rows_selected");
    # a.plot$p = DiffCircaPipeline::DCP_ScatterPlot(CP.obj, genes.plot = a.plot$g, Info1 = db$gIinfo, Info2 = db$gIIinfo)
    # a.plot$p = gridExtra::grid.arrange(a.plot$p[[1]][[1]], a.plot$p[[2]][[1]], ncol = 2)
    # print(a.plot$p)
  })
  observeEvent(input$DRFres_rows_selected, {
    # check.plot(globalDB, "CP.obj")
    print(input$DRFres_rows_selected)
    a.plot$g = res.DRF$gname[input$DRFres_rows_selected]
    print(paste0("The gene name: ", a.plot$g))
    # g.sel.reset(DRF = input$DRF_rows_selected)
    shinyjs::reset("Rhythm1_rows_selected"); shinyjs::reset("Rhythm2_rows_selected"); shinyjs::reset("DRFres_rows_selected");
    # a.plot$p = DiffCircaPipeline::DCP_ScatterPlot(CP.obj, genes.plot = a.plot$g, Info1 = db$gIinfo, Info2 = db$gIIinfo)
    # a.plot$p = gridExtra::grid.arrange(a.plot$p[[1]][[1]], a.plot$p[[2]][[1]], ncol = 2)
    # print(a.plot$p)
  })

  observeEvent(input$saveScatter, {
    # check.plot(globalDB, "CP.obj")
    pdf(paste0(db$working.dir, "/", input$p_scatter_name, ".pdf"))
    gridExtra::grid.arrange(a.plot$p[[1]][[1]], a.plot$p[[2]][[1]], ncol = 2)
    dev.off()
  })


  # ##########################
  # # Render output/UI       #
  # ##########################
  output$scatterPlot <- renderPlot({
    a.plot$p = DiffCircaPipeline::DCP_ScatterPlot(CP.obj, genes.plot = a.plot$g, Info1 = db$gIinfo, Info2 = db$gIIinfo)
    gridExtra::grid.arrange(a.plot$p[[1]][[1]], a.plot$p[[2]][[1]], ncol = 2)
})

}
