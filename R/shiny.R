#' startBNShiny 
#' @import rtson R6
#' @export
startBNShiny = function(){
  options(shiny.launch.browser=FALSE)
  options(shiny.maxRequestSize = 100*1024^2)
  
  bn <- BNClient$new("http://192.168.1.43:6040")
  operatorServerBuilder <- OperatorServerBuilderImpl$new(bn)
  appDispatcher <- OperatorServerDispatcher$new(operatorServerBuilder)
  bnShinyApp <- ShinySessionApp$new(appDispatcher)
  shiny::runApp(bnShinyApp$shinyApp, host='0.0.0.0', port = 6042)
}

#' SessionShinyServer 
#'
SessionShinyServer <- R6Class("SessionShinyServer",
    public = list( 
      shinyServer = function(input, output, session){}
    )
)

#' ShinySessionDispatcher 
#'
ShinySessionDispatcher <- R6Class("ShinySessionDispatcher",
    public = list( 
      dispatch = function(input, output, session){
        sessionShinyServer <- self$getSessionShinyServer(session)
        if (!is.null(sessionShinyServer)){
          if (!inherits(sessionShinyServer, "SessionShinyServer"))
            stop("'sessionShinyServer' is not a SessionShinyServer object.")
          sessionShinyServer$shinyServer(input, output, session)
        }
      },
      getSessionShinyServer = function(session){
        return (SessionShinyServer$new())
      }
    )
)

#' ShinySessionApp 
#'
ShinySessionApp <- R6Class("ShinySessionApp",
     public = list( 
       sessionDispatcher = NA,
       shinyApp = NA,
       
       initialize = function(sessionDispatcher) {
         if (is.null(sessionDispatcher)){
           self$sessionDispatcher = ShinySessionDispatcher$new()
         } else {
           if (!inherits(sessionDispatcher, "ShinySessionDispatcher"))
             stop("'sessionDispatcher' is not a ShinySessionDispatcher object.")
           self$sessionDispatcher = sessionDispatcher
         }
          
         self$shinyApp = shiny::shinyApp(
           ui = shiny::fluidPage(
             shiny::uiOutput("body")
           ),
           server = function(input, output, session) {
             self$sessionDispatcher$dispatch(input,output,session)
           }
         ) 
       }
     )
)