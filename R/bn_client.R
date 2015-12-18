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
    sendMessage = function(params){
      print("sendMessage")
      url = self$getQueryUrl()
      response <- httr::POST(url, httr::add_headers('Content-Type' = 'application/octet-stream'), body=toTSON(params), httr::verbose())
      if (response$status != 200){
        self$errorResponse(paste0("sendMessage : " , params$type ), response)
      }
    },
    sendError = function(context, error){
      print("sendError")
      print(error)
      err = error
      if (!inherits(error, 'Error')){
        err = Error$new(error=toString(error))
      }
      params = list(type=tson.scalar("error"), context=context$toTson(), error=err$toTson())
      self$sendMessage(params)
    },
    sendOrders = function(context, bnresult){
      params = list(type=tson.scalar("orders"), context=context$toTson(), orders=bnresult)
      self$sendMessage(params)
    },
    sendResult = function(context, dataFrame){
      dfTson = NULL
      if (!is.null(dataFrame)){
        dfTson = object.asTSON(dataFrame)
      }
      params = list(type=tson.scalar("result"), context=context$toTson(), result=dfTson)
      self$sendMessage(params)
    },
    sendPrint = function(context, msg){
      params = list(type=tson.scalar("print"), context=context$toTson(), msg=tson.scalar(as.character(msg)))
      self$sendMessage(params)
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
      if (is.null(self$workflowId)) stop("BNContext : workflowId is null")
      if (is.null(self$stepId)) stop("BNContext : stepId is null")
      if (is.null(self$contextId)) stop("BNContext : contextId is null")
      if (is.null(self$client)) stop("BNContext : client is null")
      if (!inherits(client, "BNClient"))
        stop("BNContext : 'client' is not a BNClient object.")
      
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
    error = function(error) self$client$sendError(self, error),
    setOrders = function(bnresult) self$client$sendOrders(self, bnresult),
    print = function(msg) self$client$sendPrint(self, msg),
    setResult = function(data) self$client$sendResult(self, data)
  )
)