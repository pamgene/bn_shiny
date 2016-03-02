#' startBNShiny
#' @import rtson R6
#' @export
startBNShiny = function() {
  options(shiny.launch.browser = FALSE)
  options(shiny.maxRequestSize = 1024 ^ 3)
  
  bnClientUri = getOption(bn.uri, default = "http://127.0.0.1:6040")
  
  bn <- BNClient$new(bnClientUri)
  operatorServerBuilder <- OperatorServerBuilderImpl$new(bn)
  appDispatcher <-
    OperatorServerDispatcher$new(operatorServerBuilder)
  bnShinyApp <- ShinySessionApp$new(appDispatcher)
  shiny::runApp(
    bnShinyApp$shinyApp, host = getOption("shiny.host", "127.0.0.1"), port = getOption("shiny.port", default = 7042)
  )
}

#' startBNWsShiny
#' Use websocket for transport channel
#' @export
startBNWsShiny = function() {

  options(shiny.launch.browser = FALSE)
  options(shiny.maxRequestSize = 1024 ^ 3)
    
  # use bn_shiny2.R
  appDispatcher <- BNShinySessionDispatcher$new()
  bnShinyApp <- ShinySessionApp$new(appDispatcher)
  shiny::runApp(
    bnShinyApp$shinyApp, host = getOption("shiny.host", "127.0.0.1"), port = getOption("shiny.port", default = 7042)
  )
}


#' SessionShinyServer
#'
SessionShinyServer =
  R6Class("SessionShinyServer",
          public = list(
            shinyServer = function(input, output, session) {
              
            }
          ))

#' ShinySessionDispatcher
#'
ShinySessionDispatcher = R6Class(
  "ShinySessionDispatcher",
  public = list(
    dispatch = function(input, output, session) {
      sessionShinyServer <- self$getSessionShinyServer(session)
      if (!is.null(sessionShinyServer)) {
        if (!inherits(sessionShinyServer, "SessionShinyServer"))
          stop("'sessionShinyServer' is not a SessionShinyServer object.")
        sessionShinyServer$shinyServer(input, output, session)
      }
    },
    getSessionShinyServer = function(session) {
      return (SessionShinyServer$new())
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