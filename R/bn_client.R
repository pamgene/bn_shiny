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
      url = self$getQueryUrl()
      params = list(type=tson.scalar("operatorPropertiesAsMap"), context=context$toTson())
      response <- httr::POST(url, httr::add_headers('Content-Type' = 'application/octet-stream'), body=toTSON(params))
      if (response$status != 200){
        self$errorResponse("getOperatorProperties", response)
      }
      json = httr::content(response, "raw")
      return (fromTSON(json))
    },
    getStepFolder = function(context){
      url = self$getQueryUrl()
      params = list(type=tson.scalar("stepFolder"), context=context$toTson())
      response <- httr::POST(url, httr::add_headers('Content-Type' = 'application/octet-stream'), body=toTSON(params))
      if (response$status != 200){
        self$errorResponse("getStepFolder", response)
      }
      json = httr::content(response, "raw")
      return (fromTSON(json))
    },
    getData = function(context){
      url = self$getQueryUrl()
      params = list(type=tson.scalar("data"), context=context$toTson())
      response <- httr::POST(url, httr::add_headers('Content-Type' = 'application/octet-stream'), body=toTSON(params))
      if (response$status != 200){
        self$errorResponse("getData", response)
      }
      json = httr::content(response, "raw")
      return (annotated.data.frame.fromTSON(fromTSON(json)))
    },
    getCurveFitParams= function(context){
      url = self$getQueryUrl()
      params = list(type=tson.scalar("curveFitParams"), context=context$toTson())
      response <- httr::POST(url, httr::add_headers('Content-Type' = 'application/octet-stream'), body=toTSON(params))
      if (response$status != 200){
        self$errorResponse("getData", response)
      }
      json = httr::content(response, "raw")
      params = fromTSON(json)
      params$data = data.frame.fromTSON(params$data)
      return (params)
    }
  )
)

BNContext  <- R6Class(
  'BNContext',
  public = list(
    workflowId = NULL,
    stepId = NULL,
    client = NULL,
    initialize = function(workflowId, stepId, client){
      self$workflowId = workflowId
      self$stepId = stepId
      self$client = client
    },
    toTson = function() list(workflowId=tson.scalar(self$workflowId), stepId=tson.scalar(self$stepId)),
    getProperties = function() self$client$getOperatorProperties(self),
    getPropertiesAsMap = function() self$client$getOperatorPropertiesAsMap(self),
    getFolder = function() self$client$getStepFolder(self),
    getSourceCode = function(){
      return (self$getPropertiesAsMap()[["R function"]])
    },
    getCurveFitParams = function() self$client$getCurveFitParams(self),
    getData = function() self$client$getData(self)
  )
)