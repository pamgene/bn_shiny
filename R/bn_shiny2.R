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
    }
  )
)

BNSession = R6Class(
  "BNSession",
  public = list(
    
    sessionDispatch = NULL,
    sessionId = NULL,
    ws = NULL,
    operatorByIds = NULL,
    
    initialize = function(sessionDispatch, sessionId, ws) {
      self$operatorByIds = Map$new()
      self$sessionDispatch = sessionDispatch
      self$sessionId = sessionId
      self$ws = ws
      self$ws$onMessage(self$onMessage)
    },
    
    onMessage = function(binary, message) {
      request = NULL
      tryCatch({
        messageEnvelope = fromTSON(message)
        kind = messageEnvelope[["kind"]]
        if (identical(kind, "request")){
          request = requestFromJson(messageEnvelope[["message"]])
          request$processOn(self)
        } else if (identical(kind, "response")){
          response = responseFromJson(messageEnvelope[["message"]])
          response$processOn(self)
        } else {
          stop(paste0("unknown message kind : "), kind)
        }
      }, error = function(e) {
        self$sendError(request$id, e)
      })
    },
    
    send = function(msg){
      self$ws$send(toTSON(msg))
    },
    
    sendResponse = function(msg){
      self$send(list(kind=tson.scalar("response"), message=msg))
    },
    
    sendRequest = function(msg){
      self$send(list(kind=tson.scalar("request"), message=msg))
    },
    
    sendContextError = function(contextId, error){
      print(error)
      self$sendResponse(list(contextId=tson.scalar(contextId),
                             type=tson.scalar("contextError"),
                             error= tson.scalar(toString(error)) ))
    },
    
    sendError = function(id, error){
      print(error)
      self$sendResponse(list(id=tson.scalar(id), type=tson.scalar("error"), error= tson.scalar(toString(error)) ))
    },
    
    sendVoid = function(id){
      self$sendResponse(list(id=tson.scalar(id), type=tson.scalar("void") ))
    },
    
    addOperator = function(operatorId, operator){
      self$operatorByIds$set(operatorId, operator)
    },
    
    getOperator = function(operatorId){
      return(self$operatorByIds$get(operatorId))
    }
    
  )
)

BNSessionContext  <- R6Class(
  'BNSessionContext',
  public = list(
    workflowId = NULL,
    stepId = NULL,
    contextId = NULL,
    session = NULL,
    initialize = function(workflowId, stepId, contextId, session){
      self$workflowId = workflowId
      self$stepId = stepId
      self$contextId = contextId
      self$session = session
      if (is.null(self$workflowId)) stop("BNContext : workflowId is null")
      if (is.null(self$stepId)) stop("BNContext : stepId is null")
      if (is.null(self$contextId)) stop("BNContext : contextId is null")
      if (is.null(self$session)) stop("BNContext : session is null")
      if (!inherits(session, "BNSession"))
        stop("BNContext : 'session' is not a BNSession object.")
      
    },
    processRequest = function(reactiveRequest){
      if (!inherits(reactiveRequest, "BNReactiveRequest"))
        stop("BNSessionContext processRequest : 'reactiveRequest' is not a BNReactiveRequest object.")
      reactiveRequest$processContext(self)
      return(reactiveRequest$reactiveValues)
    },
    toTson = function() list(workflowId=tson.scalar(self$workflowId), stepId=tson.scalar(self$stepId), contextId=tson.scalar(self$contextId)),
    getProperties = function() {
      stop('BNSessionContext getProperties not yet implemented')
    },
    getPropertiesAsMap = function() {
      stop('BNSessionContext getPropertiesAsMap not yet implemented')
    },
    
    getFolder = function() self$processRequest(BNGetFolderRequest$new()),
    
    getCurveFitParams = function(){
      stop('BNSessionContext getCurveFitParams not yet implemented')
    },
    getData = function() self$processRequest(BNGetDataRequest$new()),
    error = function(error){
      self$session$sendContextError(self$contextId, error)
    },
    setOrders = function(rowOrder,colOrder){
      request = BNSetOrderRequest$new()
      orders = list()
      if (!is.null(rowOrder)) orders[['rowOrder']] = as.integer(rowOrder)
      if (!is.null(colOrder)) orders[['colOrder']] = as.integer(colOrder)
      
      request$value = orders
      self$processRequest(request)
    },
    setResult = function(data){
      stop('BNSessionContext setResult not yet implemented')
    }
  )
)

BNMessage = R6Class(
  "BNMessage",
  public = list(
    json = NULL,
    initialize = function(json=list()){
      self$json = json
    },
    processOn = function(bnSession){}
  ),
  active = list(
    id = function(value){
      if (missing(value)) return(self$json$id)
      else self$json$id <- tson.scalar(value)
    },
    type = function(value){
      if (missing(value)) return(self$json$type)
      else self$json$type <- tson.scalar(value)
    }
  )
)

BNRequest = R6Class(
  "BNRequest",
  inherit = BNMessage
)

BNResponse = R6Class(
  "BNResponse",
  inherit = BNMessage
)
