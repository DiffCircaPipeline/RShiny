# sendWarningMessage <- function(session, msg, unique=F) {
#   session$sendCustomMessage(type = 'warningMessage', list(message=msg, unique=unique))
# }

sendErrorMessage <- function(session, msg, unique=F) {
  session$sendCustomMessage(type='errorMessage', list(message=msg, unique=unique))
}

sendSuccessMessage <- function(session, msg, unique=F) {
  session$sendCustomMessage(type = 'successMessage', list(message=msg, unique=unique))
}

sendInfoMessage <- function(session, msg, unique=F) {
  session$sendCustomMessage(type = 'infoMessage', list(message=msg, unique=unique))
}

wait <- function(session, msg) {
  session$sendCustomMessage(type = 'wait', message=msg)
}

done <- function(session) {
  session$sendCustomMessage(type = 'done', message="loading...")
}

helpIcon <- function(id, msg) {
  tagList(
    htmlOutput(id, container=tags$i, class="fa fa-question help"),
    shinyBS::bsTooltip(id, msg, "right", trigger = "hover", option=list(html=T)) #I really do not know what to set to make text left-alligned.
  # style = "text-align:left" This will make extra text one the UI
  )
}

Try <- function(code, session) {
  tryCatch(code,
           # warning=function(w) {
           #   sendWarningMessage(session, w$message)
           #   done(session)
           # },
           error=function(e) {
             sendErrorMessage(session, e$message)
             print(e)
             done(session)
           }
  )
}


NS <- function(prefix) { function(name) {paste(prefix, name, sep="-")} }
