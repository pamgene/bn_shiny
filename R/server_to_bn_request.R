responseFromJson = function(json){
  type = json[["type"]]
  if (identical(type, "BNReactiveResponse")){
    return(BNReactiveResponse$new(json=json))
  } else if (identical(type, "BNAnnotatedDataFrameReactiveResponse")){
    return(BNAnnotatedDataFrameReactiveResponse$new(json=json))
  }   else {
    stop(paste0("responseFromJson : unknown response type : ", type))  
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
removeRegisteredRequest = function(requestId) {
  GlobalRequests$remove(requestId)
}

reactiveRequestFromJson = function(json){
  type = json[["type"]]
   
  if (type == "BNGetFolderRequest"){
    return(BNGetFolderRequest$new(json=json))
  } else if (type ==  "BNGetPropertiesRequest"){
    return(BNGetPropertiesRequest$new(json=json))
  } else if (type ==  "BNGetPropertiesAsMapRequest"){
    return(BNGetPropertiesAsMapRequest$new(json=json))
  } else if (type == "BNGetDataRequest"){
    return(BNGetDataRequest$new(json=json))
  } else if (type ==  "BNSetOrderRequest"){
    return(BNSetOrderRequest$new(json=json))
  } else if (type == "BNSetResultRequest"){
    return(BNSetResultRequest$new(json=json))
  }   else {
    stop(paste0("reactiveRequestFromJson : unknown reactiveRequest type : ", type))  
  }
}

BNReactiveRequest = R6Class(
  "BNReactiveRequest",
  inherit = BNRequest,
  public = list(
    reactiveValues = NULL,
    domain = NULL,
    initialize = function(json=NULL){
      if (is.null(json)){
        super$initialize()
        self$domain = getDefaultReactiveDomain()
        self$id = newRequestId()
        self$type = self$getType()
        self$reactiveValues = reactiveValues()
      } else {
        super$initialize(json=json)
      }
    },
    getType = function() stop('subclass responsability')
  ) 
)

 
BNOpenUrlRequest = R6Class(
  'BNOpenUrlRequest',
  inherit = BNRequest,
  public = list(
    initialize = function(url, dialog=FALSE){
      super$initialize()
      self$type = self$getType()
      self$id = newRequestId()
      self$url = url
      self$dialog = dialog
    },
    getType = function() 'BNOpenUrlRequest'
  ),
  active = list(
    url = function(value){
      if (missing(value)) return(self$json$url)
      else self$json$url <- tson.scalar(value)
    },
    dialog = function(value){
      if (missing(value)) return(self$json$dialog)
      else self$json$dialog <- tson.scalar(value)
    }
  )
)

BNCloseUrlRequest = R6Class(
  'BNCloseUrlRequest',
  inherit = BNRequest,
  public = list(
    initialize = function(url){
      super$initialize()
      self$type = self$getType()
      self$id = newRequestId()
      self$url = url
    },
    getType = function() 'BNCloseUrlRequest'
  ),
  active = list(
    url = function(value){
      if (missing(value)) return(self$json$url)
      else self$json$url <- tson.scalar(value)
    } 
  )
)


BNContextRequest = R6Class(
  'BNContextRequest',
  inherit = BNReactiveRequest,
  public = list(
    getType = function() 'BNContextRequest',
    processContext = function(context){
      if (!inherits(context, "BNSessionContext"))
        stop("BNContextRequest : 'context' is not a BNSessionContext object.")
      self$context = context$toTson()
      registerRequest(self)
      context$session$sendRequest(self$json)
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
    initialize = function(json=NULL, isBiobase=FALSE){
      super$initialize(json=json)
      if (is.null(json)){
        self$isBiobase = isBiobase
      }
    },
    getType = function() 'BNGetDataRequest'
  ),
  active = list(
    isBiobase = function(value){
      if (missing(value)) return(self$json$isBiobase)
      else self$json$isBiobase <- value
    }
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

BNSetResultRequest = R6Class(
  'BNSetResultRequest',
  inherit = BNContextRequest,
  public = list(
    getType = function() 'BNSetResultRequest',
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
      else self$json$value <- object.asTSON(value)
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
    },
    id = function(value){
      if (missing(value)) return(self$json$id)
      else self$json$id <- value
    }
  )
)

BNAnnotatedDataFrameReactiveResponse = R6Class(
  "BNAnnotatedDataFrameReactiveResponse",
  inherit = BNReactiveResponse,
  public = list(
    request = NULL,
    processOn = function(bnSession){
      self$request = getRegisteredRequest(self$id)
      super$processOn(bnSession)
    },
    getValue = function(){
      if (self$request$isBiobase){
        return(annotated.data.frame.fromTSON(self$value))
      } else {
        return(AnnotatedData$new(json=self$value))
      }
    }
  )
)