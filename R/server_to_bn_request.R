responseFromJson = function(json){
  type = json[["type"]]
  if (identical(type, "BNReactiveResponse")){
    return(BNReactiveResponse$new(json=json))
  } else if (identical(type, "BNAnnotatedDataFrameReactiveResponse")){
    return(BNAnnotatedDataFrameReactiveResponse$new(json=json))
  }   else {
    stop(paste0("unknown response type : ", type))  
  }
}

GlobalRequestId = 0

newRequestId = function() newGuid()

GlobalRequests = Map$new()

registerRequest = function(request){
  if (!inherits(request, "BNRequest"))
    stop("registerRequest : 'request' is not a BNRequest object.")
  GlobalRequests$set(request$id, request)
}

getRegisteredRequest = function(requestId) GlobalRequests$get(requestId)
removeRegisteredRequest = function(requestId) GlobalRequests$remove(requestId)

BNReactiveRequest = R6Class(
  "BNReactiveRequest",
  inherit = BNRequest,
  public = list(
    reactiveValues = NULL,
    domain = NULL,
    initialize = function(){
      super$initialize()
      self$domain = getDefaultReactiveDomain()
      self$id = newRequestId()
      self$type = self$getType()
      self$reactiveValues = reactiveValues()
      # withReactiveDomain(self$domain, {self$reactiveValues = reactiveValues()})
      
    },
    getType = function() stop('subclass responsability')
  )
)

BNContextRequest = R6Class(
  'BNContextRequest',
  inherit = BNReactiveRequest,
  public = list(
    getType = function() 'BNGetFolderRequest',
    processContext = function(context){
      if (!inherits(context, "BNSessionContext"))
        stop("BNContextRequest : 'context' is not a BNSessionContext object.")
      self$context = context$toTson()
      context$session$sendRequest(self$json)
      registerRequest(self)
    }
  ),
  active = list(
    context = function(value){
      if (missing(value)) return(self$json$context)
      else self$json$context <- value
    }
  )
)

BNGetFolderRequest = R6Class(
  'BNGetFolderRequest',
  inherit = BNContextRequest,
  public = list(
    getType = function() 'BNGetFolderRequest'
  )
)

BNGetPropertiesRequest = R6Class(
  'BNGetPropertiesRequest',
  inherit = BNContextRequest,
  public = list(
    getType = function() 'BNGetPropertiesRequest'
  )
)

BNGetPropertiesAsMapRequest = R6Class(
  'BNGetPropertiesAsMapRequest',
  inherit = BNContextRequest,
  public = list(
    getType = function() 'BNGetPropertiesAsMapRequest'
  )
)

BNGetDataRequest = R6Class(
  'BNGetDataRequest',
  inherit = BNContextRequest,
  public = list(
    getType = function() 'BNGetDataRequest'
  )
)

BNSetOrderRequest = R6Class(
  'BNSetOrderRequest',
  inherit = BNContextRequest,
  public = list(
    getType = function() 'BNSetOrderRequest',
    processContext = function(context){
      if (!inherits(context, "BNSessionContext"))
        stop("BNContextRequest : 'context' is not a BNSessionContext object.")
      self$context = context$toTson()
      context$session$sendRequest(self$json)
      # no response is expected so don't register the request
    }
  ),
  active = list(
    value = function(value){
      if (missing(value)) return(self$json$value)
      else self$json$value <- value
    }
  )
)

###################################################################################
# response

BNReactiveResponse = R6Class(
  "BNReactiveResponse",
  inherit = BNResponse,
  public = list(
    processOn = function(bnSession){
      request = removeRegisteredRequest(self$id)
      shinysession = request$domain
      value = self$getValue()
      if (is.null(shinysession)){
        request$reactiveValues$value=function() value
        shiny:::flushReact()
      } else {
        withReactiveDomain(shinysession, {
          request$reactiveValues$value=function() value
          shiny:::flushReact()
          shinysession$flushOutput()
        })
      }
    },
    getValue = function() self$value
  ),
  active = list(
    value = function(value){
      if (missing(value)) return(self$json$value)
      else self$json$value <- value
    }
  )
)

BNAnnotatedDataFrameReactiveResponse = R6Class(
  "BNAnnotatedDataFrameReactiveResponse",
  inherit = BNReactiveResponse,
  public = list(
    getValue = function() annotated.data.frame.fromTSON(self$value)
  )
)