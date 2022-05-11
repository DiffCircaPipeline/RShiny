plots_phase_server2 <- function(input, output, session) {
  ns <- NS("plots_phase")
  xxx = reactiveValues(intype3 = "")

  observeEvent(input$intype, {
    if(input$intype == "s"){
      print("This is s")
      xxx$intype3 = input$intype
      # print(paste0("input$x = ", input$x))
    }else{
      print("This is d")
      xxx$intype3 = input$intype
      # print(paste0("input$m = ", input$m))
    }
  })

  # output$APlot<-renderPlot({hist(runif(1000))})
  output$intype2<-renderText(input$intype)

  output$dynamic1 <- renderUI({
    if(xxx$intype3 == "s"){

      output = tagList()
      output[[1]] <- p("This is UI s. ")
      output[[2]] <- numericInput(ns('m'),'Sample Mean',value='')
      output[[3]] <- numericInput(ns('sd'),'SD (Population or Sample)',value='')
      output[[4]] <- numericInput(ns('n'),'Sample size',value='')

      output
    }else if(xxx$intype3 == "d"){
      output = tagList()
      output[[1]] <- p("This is UI d. ")
      output[[2]] <- textInput(ns('x'), 'Sample Data (Comma Separated)', value='')

      output
    }else{
      p("This is weird. ")
    }
  })

}

