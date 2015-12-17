library(R6)
library(rtson)

#' ServerClient 
#'
ServerClient <- R6Class("ServerClient",
                        public = list( 
                          getOperatorSourceCode = function(operatorId){}
                        )
)

#' ServerClientDev 
#'
ServerClientDev <- R6Class("ServerClientDev",
                           inherit = ServerClient,
                           public = list( 
                             getOperatorSourceCode = function(operatorId){
                               sourceCode <- readUTF8(operatorId)
                               return (sourceCode)
                             }
                           )
)

#' BNClient 
#'
BNClient <- R6Class(
  'BNClient',
  inherit = ServerClientDev,
  public = list(
    uri = NULL,
    initialize = function(uri) {
      self$uri = uri;
    },
    errorResponse = function(msg, response){
      stop(paste0("Http error : ", msg , " : response$status " , response$status))
    },
    getQueryUrl = function() paste0(self$uri,"/query"),
    getOperatorProperties = function(context){
      print("getOperatorProperties")
      url = self$getQueryUrl()
      params = list(type=tson.scalar("operatorProperties"), context=context$toTson())
      
      response <- httr::POST(url, httr::add_headers('Content-Type' = 'application/octet-stream'), body=toTSON(params), httr::verbose())
      if (response$status != 200){
        self$errorResponse("getOperatorProperties", response)
      }
      json = httr::content(response, "raw")
      return (fromTSON(json))
    },
    getOperatorPropertiesAsMap = function(context){
      print("getOperatorPropertiesAsMap")
      url = self$getQueryUrl()
      params = list(type=tson.scalar("operatorPropertiesAsMap"), context=context$toTson())
      response <- httr::POST(url, httr::add_headers('Content-Type' = 'application/octet-stream'), body=toTSON(params), httr::verbose())
      if (response$status != 200){
        self$errorResponse("getOperatorProperties", response)
      }
      json = httr::content(response, "raw")
      return (fromTSON(json))
    },
    getStepFolder = function(context){
      print("getStepFolder")
      url = self$getQueryUrl()
      params = list(type=tson.scalar("stepFolder"), context=context$toTson())
      response <- httr::POST(url, httr::add_headers('Content-Type' = 'application/octet-stream'), body=toTSON(params), httr::verbose())
      if (response$status != 200){
        self$errorResponse("getStepFolder", response)
      }
      json = httr::content(response, "raw")
      return (fromTSON(json))
    },
    getData = function(context){
      print("getData")
      url = self$getQueryUrl()
      params = list(type=tson.scalar("data"), context=context$toTson())
      response <- httr::POST(url, httr::add_headers('Content-Type' = 'application/octet-stream'), body=toTSON(params), httr::verbose())
      if (response$status != 200){
        self$errorResponse("getData", response)
      }
      json = httr::content(response, "raw")
      return (annotated.data.frame.fromTSON(fromTSON(json)))
    },
    getCurveFitParams= function(context){
      print("getCurveFitParams")
      url = self$getQueryUrl()
      params = list(type=tson.scalar("curveFitParams"), context=context$toTson())
      response <- httr::POST(url, httr::add_headers('Content-Type' = 'application/octet-stream'), body=toTSON(params), httr::verbose())
      if (response$status != 200){
        self$errorResponse("getData", response)
      }
      json = httr::content(response, "raw")
      params = fromTSON(json)
      params$data = data.frame.fromTSON(params$data)
      return (params)
    },
    sendError = function(context, error){
      print("sendError")
      url = self$getQueryUrl()
      params = list(type=tson.scalar("error"), context=context$toTson(), error=error$toTson())
      response <- httr::POST(url, httr::add_headers('Content-Type' = 'application/octet-stream'), body=toTSON(params), httr::verbose())
      if (response$status != 200){
        self$errorResponse("sendError", response)
      }
    },
    sendOrders = function(context, bnresult){
      print("sendOrders")
      url = self$getQueryUrl()
      params = list(type=tson.scalar("orders"), context=context$toTson(), orders=bnresult)
      response <- httr::POST(url, httr::add_headers('Content-Type' = 'application/octet-stream'), body=toTSON(params), httr::verbose())
      if (response$status != 200){
        self$errorResponse("sendOrders", response)
      }
    },
    sendResult = function(context, dataFrame){
      print("sendResult")
      url = self$getQueryUrl()
      dfTson = NULL
      if (!is.null(dataFrame)){
        dfTson = object.asTSON(dataFrame)
      }
      params = list(type=tson.scalar("result"), context=context$toTson(), result=dfTson)
      response <- httr::POST(url, httr::add_headers('Content-Type' = 'application/octet-stream'), body=toTSON(params), httr::verbose())
      if (response$status != 200){
        self$errorResponse("sendResult", response)
      }
    }
  )
)

BNContext  <- R6Class(
  'BNContext',
  public = list(
    workflowId = NULL,
    stepId = NULL,
    contextId = NULL,
    client = NULL,
    initialize = function(workflowId, stepId, contextId, client){
      self$workflowId = workflowId
      self$stepId = stepId
      self$contextId = contextId
      self$client = client
    },
    toTson = function() list(workflowId=tson.scalar(self$workflowId), stepId=tson.scalar(self$stepId), contextId=tson.scalar(self$contextId)),
    getProperties = function() self$client$getOperatorProperties(self),
    getPropertiesAsMap = function() self$client$getOperatorPropertiesAsMap(self),
    getFolder = function() self$client$getStepFolder(self),
    getSourceCode = function(){
      return (self$getPropertiesAsMap()[["R function"]])
    },
    getCurveFitParams = function() self$client$getCurveFitParams(self),
    getData = function() self$client$getData(self),
    sendError = function(error) self$client$sendError(self, error),
    sendOrders = function(bnresult) self$client$sendOrders(self, bnresult)
  )
)