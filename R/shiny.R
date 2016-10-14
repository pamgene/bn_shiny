library(bnutil)

#' startBNWsShiny
#' @import rtson
#' @import bnutil
#' @import R6
#' @import XML
#' @export
startBNWsShiny = function() {

  options(shiny.launch.browser = FALSE)
  options(shiny.maxRequestSize = 1024 ^ 3)
    
  appDispatcher <- BNShinySessionDispatcher$new()
  bnShinyApp <- ShinySessionApp$new(appDispatcher)
  shiny::runApp(
    bnShinyApp$shinyApp,
    host = getOption("shiny.host", "127.0.0.1"),
    port = getOption("shiny.port", default = 7042)
  )
}

#' @export
startBNTestShiny = function(packageName, sessionType='run', bnMessageHandler=NULL) {
  
  library(Biobase)
  
  library(packageName, character.only = TRUE)
   
  options(shiny.maxRequestSize = 1024 ^ 3)
  
  appDispatcher <- BNTestShinySessionDispatcher$new(createOperatorFromPackage(packageName),
                                                    sessionType = sessionType,
                                                    bnMessageHandler=bnMessageHandler)
  bnShinyApp <- ShinySessionApp$new(appDispatcher)
  shiny::runApp(
    bnShinyApp$shinyApp,
    host = getOption("shiny.host", "127.0.0.1"),
    port = getOption("shiny.port", default = 7142)
  )
}
 
#' ShinySessionDispatcher
#'
ShinySessionDispatcher = R6Class(
  "ShinySessionDispatcher",
  public = list(
    dispatch = function(input, output, session) {
      stop('subclass responsability')
    }
  )
)

#' ShinySessionApp
#'
ShinySessionApp = R6Class(
  "ShinySessionApp",
  public = list(
    sessionDispatcher = NA,
    shinyApp = NA,
    
    initialize = function(sessionDispatcher) {
      if (is.null(sessionDispatcher)) {
        self$sessionDispatcher = ShinySessionDispatcher$new()
      } else {
        if (!inherits(sessionDispatcher, "ShinySessionDispatcher"))
          stop("'sessionDispatcher' is not a ShinySessionDispatcher object.")
        self$sessionDispatcher = sessionDispatcher
      }
      
      self$shinyApp = shiny::shinyApp(
        ui = shiny::fluidPage(shiny::uiOutput("body")),
        server = function(input, output, session) {
          self$sessionDispatcher$dispatch(input,output,session)
        }
      )
    }
  )
)