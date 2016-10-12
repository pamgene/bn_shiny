BNShinySessionDispatcher <- R6Class(
  "BNShinySessionDispatcher",
  inherit = ShinySessionDispatcher,
  public = list(
    
    bnSessionByIds = NULL,
    
    initialize = function(){
      self$bnSessionByIds = Map$new() 
      shiny:::handlerManager$addWSHandler(shiny:::routeWSHandler("/bn",self$bnWSHandler) , "bn")
      shiny:::handlerManager$addHandler(shiny:::routeHandler("/ping",self$pingHttpHandler) , "ping")
    },
    
    bnWSHandler = function(ws){
      sessionId = paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")
      self$bnSessionByIds$set(sessionId, BNSession$new(self, sessionId, ws))
    },
    
    pingHttpHandler= function(req){
      return(list(status = 200L,
                  headers = list('Content-Type' = 'application/json'),
                  body = '[]'))  
    },
    
    dispatch = function(input, output, session) {
      query <- isolate(parseQueryString(session$clientData$url_search))
      sessionId = query$sessionId
      if (is.null(sessionId)) stop("sessionId is required")
      bnSession = self$bnSessionByIds$get(sessionId)
      if (is.null(bnSession)) stop("unknown sessionId")
      bnSession$dispatchSiny(input, output, session)
    }
  )
)

BNTestShinySessionDispatcher = R6Class(
  "BNTestShinySessionDispatcher",
  inherit = ShinySessionDispatcher,
  public = list(
    
    bnSession = NULL,
    bnMessageHandler = NULL,
    
    initialize = function(operator, sessionType = 'run', bnMessageHandler=BNMessageHandler$new()){
      self$bnMessageHandler = bnMessageHandler
      self$bnSession = BNTestSession$new(self, operator, sessionType, self$messageHandler)
    },
     
    dispatch = function(input, output, session) {
      self$bnSession$dispatchSiny(input, output, session)
    },
    
    messageHandler = function(msgEnveloppe){
        
      kind = msgEnveloppe[['kind']]
      if (kind == 'contextError' || kind == 'noContextError'){
        stop(msgEnveloppe[['error']])
      }
      msg = msgEnveloppe[['message']]
      if (is.null(msg)) {
        stop(paste('BNTestShinySessionDispatcher : messageHandler is.null(msg) : msgEnveloppe',msgEnveloppe))
      }
      
      if (kind == 'request'){
        self$requestHandler(msg)
      } else if  (kind == 'response') {
        self$responseHandler(msg)
      } else {
        stop(paste('BNTestShinySessionDispatcher : messageHandler unknown message type',type))
      }
    },
    
    requestHandler = function(msg){
      request = reactiveRequestFromJson(msg)
      json = NULL
      if (inherits(request, "BNGetFolderRequest")){
        json = list(value=self$bnMessageHandler$getFolder())
      } else if (inherits(request, "BNGetRunFolderRequest")){
        json = list(value=self$bnMessageHandler$getRunFolder())
      } else if (inherits(request, "BNGetPropertiesRequest")){
        json = list(value=self$bnMessageHandler$getProperties())
      } else if (inherits(request, "BNGetPropertiesAsMapRequest")){
        json = list(value=self$bnMessageHandler$getPropertiesAsMap())
      } else if (inherits(request, "BNGetDataRequest")){
        data = self$bnMessageHandler$getData()
        json = list(value=object.asTSON(data))
        res = BNAnnotatedDataFrameReactiveResponse$new(json=json)
        res$id = request$id
        self$processResponse(res)
        return()
      } else if (inherits(request, "BNSetOrderRequest")){
        self$bnMessageHandler$setOrder(request$value)
        return()
      } else if (inherits(request, "BNSetResultRequest")){
        result = AnnotatedData$new(json=request$value)
        self$bnMessageHandler$setResult(result)
        return()
      } else {
        stop('BNTestShinySessionDispatcher : requestHandler unknown request')
      }
       
      res = BNReactiveResponse$new(json=json)
      res$id = request$id
      self$processResponse(res)
    },
    
    processResponse = function(response){
      self$bnSession$processResponse(response)
    },
    
    responseHandler = function(msg){
      stop('BNTestShinySessionDispatcher : responseHandler : not yet implemented ')
    }
  )
)

#' @export
BNMessageHandler = R6Class(
  "BNMessageHandler",
  public = list(
    
    getFolderHandler = NULL,
    getRunFolderHandler = NULL,
    getPropertiesHandler = NULL,
    getPropertiesAsMapHandler = NULL,
    getDataHandler = NULL,
    setOrderHandler = NULL,
    setResultHandler = NULL,
    
    initialize = function(){},
    
    getFolder = function(){
      if (!is.null(self$getFolderHandler)) return(self$getFolderHandler())
      return (getwd())
    },
    
    getRunFolder = function(){
      if (!is.null(self$getRunFolderHandler)) return(self$getRunFolderHandler())
      return (getwd())
    },
    
    getProperties = function(){
      if (!is.null(self$getPropertiesHandler)) return(self$getPropertiesHandler())
      return (list(list('prop1', 'value1')))
    },
    
    getPropertiesAsMap = function(){
      if (!is.null(self$getPropertiesAsMapHandler)) return(self$getPropertiesAsMapHandler())
      return (list(prop1='value1'))
    },
    
    getData = function(){
      if (!is.null(self$getDataHandler)) return(self$getDataHandler())
      data = data.frame(rowSeq=c(1,1,1,1),
                        colSeq=c(1,1,2,2),
                        myXAxis=c(1.0,2.0,1.0,2.0),
                        value=c(0.42,42.0,1.42,42.1),
                        IsOutlier=c(F,F,F,F))
      
      varMetadata = data.frame(labelDescription=c("rowSeq",
                                                  "colSeq",
                                                  "myXAxis",
                                                  "value",
                                                  "IsOutlier"),
                               groupingType=c("rowSeq",
                                              "colSeq",
                                              "xAxis",
                                              "value",
                                              "IsOutlier"))
      
      annotatedDataFrame = new("AnnotatedDataFrame",
                               data=data,
                               varMetadata=varMetadata)
      return(annotatedDataFrame)
    },
    
    setOrder = function(order){
      if (!is.null(self$setOrderHandler)) return(self$setOrderHandler(order))
      print(order)
    },
    
    setResult = function(annotatedDataFrame){
      if (!is.null(self$setResultHandler)) return(self$setResultHandler(annotatedDataFrame))
      print(annotatedDataFrame)
    }
  )
)

