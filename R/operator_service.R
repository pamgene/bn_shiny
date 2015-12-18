library(rtson)

OperatorServer<- R6Class(
  "OperatorServer",
  inherit = SessionShinyServer,
  public = list(
    operator = NA,
    client = NA,
    initialize = function(client, operator) {
      if (!inherits(client, "BNClient"))
        stop("'client' is not a BNClient object.")
      if (!inherits(operator, "Operator"))
        stop("'operator' is not a Operator object.")
      self$operator <- operator
      self$client <- client
    },
    
    shinyServer = function(input, output, session){
      query <- isolate(parseQueryString(session$clientData$url_search))
      context = BNContext$new(query$workflowId, as.integer(query$stepId), query$contextId, self$client)
      sessionType = query[["sessionType"]]
      if (identical(sessionType,"run")){
        self$runShinyServer(input, output, session, context)
      } else if (identical(sessionType,"show")){
        self$showShinyServer(input, output, session, context)
      } else {
        context$error(Error$new(500,"shiny.server.session.type.wrong.value","sessionType must be run or show"))
        stop("sessionType must be run or show")
      }
    },
    runShinyServer = function(input, output, session, context){
      self$operator$shinyServerRun(input, output, session, context)
    },
    showShinyServer = function(input, output, session, context){
      self$operator$shinyServerShowResults(input, output, session, context)
    },
    operatorPropertiesHttpHandler = function(request){
      body = jsonlite::toJSON(self$operator$operatorProperties())
      return(list(status = 200L,
                  headers = list('Content-Type' = 'application/json'),
                  body = body))
      
    },
    operatorCapabilityHttpHandler= function(request){
      return(list(status = 200L,
                  headers = list('Content-Type' = 'application/octet-stream'),
                  body = toTSON(self$operator$capability())))
      
    },
    dataFrameOperatorHttpHandler = function(request){
      if (!inherits(request, "HttpRequest"))
        stop("'request' is not a HttpRequest object.")
      
      
      bytes = request$read()
 
      params = fromTSON(bytes)
 
      
      properties = params$properties
      folder = params$folder
      data = annotated.data.frame.fromTSON(params$data)
      
      
      result = self$operator$dataFrameOperator(DataFrameOperatorParam$new(data,properties,folder))
       
      if (!is.null(result)){
        if (class(result) == "data.frame"){
          list <- data.frame.asTSON(result)
        } else if (class(result) == "AnnotatedDataFrame"){
          list <- annotated.data.frame.asTSON(result)
        } else {
          stop("result : unknown class ")
        }
      } else {
        stop("result : null ")
      }
       
      return(list(status = 200L,
                  headers = list('Content-Type' = 'application/octet-stream'),
                  body = toTSON(list)))
      
    },
    curveFitOperatorHttpHandler = function(request){
      if (!inherits(request, "HttpRequest"))
        stop("'request' is not a HttpRequest object.")
      bytes = request$read()
      params = fromTSON(bytes)
      curveFittingParam = CurveFittingTableParam$new(data.frame.fromTSON(params$data),params$xValues, params$properties)
      result = self$operator$curveFittingTable(curveFittingParam)
      body = toTSON(result)
      return(list(status = 200L,
                  headers = list('Content-Type' = 'application/octet-stream'),
                  body = body))
    },
    showResultsHttpHandler = function(request){
      if (!inherits(request, "HttpRequest"))
        stop("'request' is not a HttpRequest object.")
      params = fromTSON(request$read())
      showResultParam = ShowResultParam$new(params$properties,params$folder)
      self$operator$showResults(showResultParam)
      return(list(status = 200L,
                  headers = list('Content-Type' = 'application/json'),
                  body = ''))
    },
    runAppHttpHandler = function(request){
      if (!inherits(request, "HttpRequest"))
        stop("'request' is not a HttpRequest object.")
      query = request$queryParameters()
      context = BNContext$new(query$workflowId, as.integer(query$stepId), query$contextId, self$client)
      self$operator$runApp(context)
      return(list(status = 200L,
                  headers = list('Content-Type' = 'application/json'),
                  body = ''))
      
    }
  )
)










