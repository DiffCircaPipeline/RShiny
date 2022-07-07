data_input_server <- function(input, output, session, globalDB) {

  ns <- NS("data_input")
  ##########################
  # Reactive Values        #
  ##########################
  # DB   <- reactiveValues(names=DB.ls(db))
  intDB <- reactiveValues(working.dir = NULL, working.dir0 = NULL,
                          studyname = NULL, studytype = "",
                          gIinfo_0 = "groupI", gIinfo = "groupI", gIIinfo_0 = "groupII", gIIinfo = "groupII",
                          trigger = 0)
  intRV <- reactiveValues(update = 0, action = "",
                          expr1 = NULL,  time1 = NULL, expr2 = NULL,  time2 = NULL
                          )
  SUMMARY <- reactiveValues(designSummary=data.frame(NULL),
                            timeSummary=data.frame(NULL),
                            nSampleStack = 0)


  ##########################
  # Observers              #
  ##########################
  observeEvent(input$directoryLoad, {
    if(input$directoryLoad>0){
      # print("DEBUG selected load study")
      path2 = rstudioapi::selectDirectory(path = readDirectoryInput(session, 'directoryLoad'))
      if(length(path2) == 0) {
        sendErrorMessage(session, MSG.no.working.dir) #later: may need to change to another message
      } else {
        intDB$working.dir <- path2
        updateDirectoryInput(session, 'directoryLoad', value = path2)
        output$working.dir <- renderText({db$working.dir})
      }
    }
  })

  observeEvent(input$loadStudy, {
    path2 <- intDB$working.dir
    if(length(path2) == 0){
      sendErrorMessage(session, MSG.no.working.dir) #later: may need to change to another message
    } else {
      # print("upload study")
      previousSaves <- loadX(path2)
      db <<- previousSaves$db
      intDB <- update_rvDB(intDB, db, "take2")
      if(db$working.dir!=path2){
        warning("The existing study had a different working directory from the current path. Will update to the current path. ")
        db$working.dir <<- path2
      }
      CP.obj <<- previousSaves$CP.obj
      # print("CP.obj data_input")
      # print(CP.obj)
      res.DRP <<- previousSaves$DRP
      # print("res.DRP data_input")
      # print(res.DRP)
      res.DRF <<- previousSaves$DRF
      # print("res.DRF data_input")
      # print(res.DRF)
      intDB$trigger <- intDB$trigger + 1
      showNotification(
        ui = paste0("Study ", intDB$studyname, " is successfully loaded from ", intDB$working.dir),
        type = "message",
        duration = 5
      )
    }
  })

  # watch for selection of study type: single group or comaprison?
  observeEvent(input$inputType, {
    if(input$inputType==1){
      # print("selected 1")
      intDB$studytype <- "Two"
      # intDB$trigger <- intDB$trigger + 1 #should start triggering after input the working.directory
      # db$studytype <<- "Two"
      SUMMARY$designSummary = data.frame(NULL)
      SUMMARY$timeSummary = data.frame(NULL)
    }else if(input$inputType==2){
      # print("selected 2")
      intDB$studytype <- "One"
      # intDB$trigger <- intDB$trigger + 1
      # db$studytype <<- "One"
      SUMMARY$designSummary = data.frame(NULL)
      SUMMARY$timeSummary = data.frame(NULL)
    }
  })

  # watch for two-group data upload
  observeEvent(c(input$expr1, input$time1), {
    if(!(is.null(input$expr1)|is.null(input$time1))){
      # print("eval upload 1")
      intRV$update <- intRV$update + 1
      Try({
        # print("read upload 1")
        intRV$expr1 = read.csv(input$expr1$datapath, row.names = 1)
        intRV$time1 = read.csv(input$time1$datapath)
        X_checkinput(yy = intRV$expr1, tt = intRV$time1)

        SUMMARY$designSummary = data.frame(group = c(intDB$gIinfo, ifelse2(!is.null(intRV$expr2), intDB$gIIinfo, NULL)),
                                           nSample = c(ncol(intRV$expr1), ifelse2(!is.null(intRV$expr2), ncol(intRV$expr2), NULL)),
                                           nGene = c(nrow(intRV$expr1), ifelse2(!is.null(intRV$expr2), nrow(intRV$expr2), NULL)))

        SUMMARY$timeSummary = data.frame(group = c(rep(intDB$gIinfo, nrow(intRV$time1)), ifelse2(!is.null(intRV$time2), rep(intDB$gIIinfo, nrow(intRV$time2)),
                                                                                            NULL)),
                                         time = c(intRV$time1$time, intRV$time2$time))

      }, session)

    }

  })

  observeEvent(c(input$expr2, input$time2), {
    if(!(is.null(input$expr2)|is.null(input$time2))){
      intRV$update = intRV$update + 1
      Try({
        intRV$expr2 = read.csv(input$expr2$datapath, row.names = 1)
        intRV$time2 = read.csv(input$time2$datapath)
        X_checkinput(yy = intRV$expr2, tt = intRV$time2)

        SUMMARY$designSummary = data.frame(group = c(ifelse2(!is.null(intRV$expr1), intDB$gIinfo, NULL), intDB$gIIinfo),
                                           nSample = c(ifelse2(!is.null(intRV$expr1), ncol(intRV$expr1), NULL), ncol(intRV$expr2)),
                                           nGene = c(ifelse2(!is.null(intRV$expr1), nrow(intRV$expr1), NULL), nrow(intRV$expr2)))
        SUMMARY$timeSummary = data.frame(group = c(ifelse2(!is.null(intRV$time1), rep(intDB$gIinfo, nrow(intRV$time2)),
                                                           NULL),
                                                   rep(intDB$gIIinfo, nrow(intRV$time2))),
                                         time = c(intRV$time1$time, intRV$time2$time))

      }, session)

    }

  })

  observeEvent(input$gIinfo, {
    if (!is.null(input$gIinfo)) {
      intDB$gIinfo <- input$gIinfo
      # intDB$trigger <- intDB$trigger + 1
      # print("input gIinfo")
      if(any(SUMMARY$designSummary$group==intDB$gIinfo_0)){
        SUMMARY$designSummary$group[SUMMARY$designSummary$group==intDB$gIinfo_0] = intDB$gIinfo
      }
      if(any(SUMMARY$timeSummary$group==intDB$gIinfo_0)){
        SUMMARY$timeSummary$group[SUMMARY$timeSummary$group==intDB$gIinfo_0] = intDB$gIinfo
      }
      intDB$gIinfo_0 <- intDB$gIinfo
    }
  }, label="group I label")

  observeEvent(input$gIIinfo, {
    if (!is.null(input$gIIinfo)) {
      intDB$gIIinfo <- input$gIIinfo
      # intDB$trigger <- intDB$trigger + 1
      # print("input gIIinfo")
      if(any(SUMMARY$designSummary$group==intDB$gIIinfo_0)){
        SUMMARY$designSummary$group[SUMMARY$designSummary$group==intDB$gIIinfo_0] = intDB$gIIinfo
      }
      if(any(SUMMARY$timeSummary$group==intDB$gIIinfo_0)){
        SUMMARY$timeSummary$group[SUMMARY$timeSummary$group==intDB$gIIinfo_0] = intDB$gIIinfo
      }
      intDB$gIIinfo_0 <- intDB$gIIinfo
    }
  }, label="group II label")

  # watch for single-group data upload
  observeEvent(c(input$expr0, input$time0), {
    if(!(is.null(input$expr0)|is.null(input$time0))){
      intRV$update = intRV$update + 2
      Try({
        intRV$expr1 = read.csv(input$expr0$datapath, row.names = 1)
        intRV$time1 = read.csv(input$time0$datapath)
        X_checkinput(yy = intRV$expr1, tt = intRV$time1)
        # print(intRV$expr1)
        # print(intRV$expr2)
        # print(intRV$time1)
        # print(intRV$time2)
        SUMMARY$designSummary = rbind.data.frame.fixed1(SUMMARY$designSummary,
                                                 data.frame(nSample = ncol(intRV$expr1), nGene = nrow(intRV$expr1)), 1)
        SUMMARY$timeSummary = rbind.data.frame.fixed1(SUMMARY$timeSummary,
                                               data.frame(time = intRV$time1$time), 1:nrow(intRV$time1))
      }, session)
    }
  })

  # Setting working directory: for intermediate output and final output
  observeEvent(input$directory, {
    if(input$directory>0){
      path = rstudioapi::selectDirectory(path = readDirectoryInput(session, 'directory'))
      intDB$working.dir0 = path
      print(paste0("chosen Input directory: ", path))
      if(length(path) == 0) {
        sendErrorMessage(session, MSG.no.working.dir)
      } else {
        intRV$update= intRV$update+1
        updateDirectoryInput(session, 'directory', value = path)
      }
    }
  }
  )

  observeEvent(input$studyName, {
    if(!is.null(input$studyName)&!input$studyName==""){
      intDB$studyname = input$studyName
      # db$studyname <<- input$studyName
      intRV$update=intRV$update+1
      db$working.dir <<- paste(intDB$working.dir0, input$studyName, sep = "/")
      intDB$working.dir <<- paste(intDB$working.dir0, input$studyName, sep = "/")
      output$working.dir <- renderText({intDB$working.dir})
    }
  })

  #save data rds file to save folder
  observeEvent(input$saveStudy, {
    wait(session, "Saving study")
    if(intRV$update>=4){
      Try({
        validate.study(intRV, intDB)
        CP.obj <<- toX(list(expr1 = intRV$expr1,
                            time1 = intRV$time1,
                            expr2 = intRV$expr2,
                            time2 = intRV$time2,
                            groupIinfo = intDB$gIinfo,
                            groupIIinfo = intDB$gIIinfo,
                            studytype = intDB$studytype,
                            studyname = intDB$studyname))
        saveX(CP.obj, intDB) #save CP.obj to global environment.
        # db$gIinfo <<- intDB$gIinfo
        # db$gIIinfo <<- intDB$gIIinfo
        # db$update <<- db$update + 1
        # print("DEBUG before pulling the trigger: ")
        # print(intDB$gIinfo)
        # print(intDB$gIIinfo)
        # print(intDB$studytype)
        # print(intDB$studyname)
        intDB$trigger <- intDB$trigger + 1
        # db$working.dir <<- paste0(db$working.dir, db$studyname, sep = "/")
        sendSuccessMessage(session, paste("Study", db$studyname, "saved to ", db$working.dir))
        shinyjs::reset("expr0")
        shinyjs::reset("expr1")
        shinyjs::reset("expr2")
        shinyjs::reset("time0")
        shinyjs::reset("time1")
        shinyjs::reset("time2")
        shinyjs::reset("gIinfo")
        shinyjs::reset("gIIinfo")
        shinyjs::reset("inputType")
        shinyjs::reset("studyName")
        intRV$update = 0
      }, session)
      done(session)
      showNotification(
        ui = paste0("Study ", intDB$studyname, " is successfully uploaded to ", intDB$working.dir),
        type = "message",
        duration = 5
      )
    }
  }, label="save study")

  ##########################
  # Render output/UI       #
  ##########################

  # studySummary
  output$designSummary <- #DT::renderDataTable({
    renderTable({
    if(nrow(SUMMARY$designSummary)>0){
      SUMMARY$designSummary
    }
  })

  output$timeSummary <- renderPlot({
    if(nrow(SUMMARY$timeSummary)>0){
      if(intDB$studytype == "Two"){
        data_input_plot_time(SUMMARY$timeSummary, "Two")
      }else{
        data_input_plot_time(SUMMARY$timeSummary, "One")
      }
    }
  })

  return(intDB)
}
