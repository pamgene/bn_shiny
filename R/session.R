BNSession = R6Class(
  "BNSession",
  public = list(
    
    sessionDispatch = NULL,
    sessionId = NULL,
    ws = NULL,
    operatorByIds = NULL,
    endPointByIds = NULL,
    
    initialize = function(sessionDispatch, sessionId, ws) {
      self$endPointByIds = Map$new()
      self$operatorByIds = Map$new()
      self$sessionDispatch = sessionDispatch
      self$sessionId = sessionId
      self$ws = ws
      self$ws$onMessage(self$onWSMessage)
    },
    
    registerEndPoint = function(endPointId, fun){
      self$endPointByIds$set(endPointId, fun)
    },
    
    removeEndPoint = function(endPointId){
      self$endPointByIds$remove(endPointId)
    },
     
    dispatchSiny = function(input, output, session) {
       
      contextId = NULL
      query = NULL
      
      tryCatch({
        query <- isolate(parseQueryString(session$clientData$url_search))
        
        endPointId = query$endPointId
        if (!is.null(endPointId))  {
          endpoint = self$endPointByIds$get(endPointId)
          return (endpoint(input, output, session, self))
        }
        
        contextId = query$contextId
        if (is.null(contextId)) stop("BNSession : contextId is required")
      }, error = function(e) {
        self$sendNoContextError(e)
        stop(e)
      })
      
      tryCatch({
        operatorId = query[["operatorId"]]
        if (is.null(operatorId)) stop("BNSession : operatorId is required")
        operator = self$operatorByIds$get(operatorId)
        if (is.null(operator)) stop("BNSession : operator is unknown")
        
        context = BNSessionContext$new(query$workflowId, as.integer(query$stepId), contextId, self)
        sessionType = query[["sessionType"]]
        
        if (identical(sessionType,"run")){
          operator$shinyServerRun(input, output, session, context)
        } else if (identical(sessionType,"show")){
          operator$shinyServerShowResults(input, output, session, context)
        } else {
          stop("sessionType must be run or show")
        }
      }, error = function(e) {
        self$sendContextError(contextId, e)
        stop(e)
      })
    },
    
    onWSMessage = function(binary, message) {
      tryCatch({
        self$onMessage(fromTSON(message))
      }, error = function(e) {
        self$sendNoContextError(e)
      })
    },
    
    onMessage = function(messageEnvelope) {
      tryCatch({
        kind = messageEnvelope[["kind"]]
        if (identical(kind, "request")){
          request = requestFromJson(messageEnvelope[["message"]])
          self$processRequest(request)
        } else if (identical(kind, "response")){
          response = responseFromJson(messageEnvelope[["message"]])
          self$processResponse(response)
        } else {
          stop(paste0("unknown message kind : "), kind)
        }
      }, error = function(e) {
        self$sendNoContextError(e)
      })
    },
    
    processRequest = function(request){
      tryCatch({
        request$processOn(self)
      }, error = function(e) {
        # traceback()
        if (is.null(request)){
          self$sendNoContextError(e)
        } else {
          self$sendError(request$id, e)
        }
      })
    },
    
    processResponse = function(response){
      tryCatch({
        response$processOn(self)
      }, error = function(e) {
        self$sendNoContextError(e)
      })
    },
    
    send = function(msg){
      self$ws$send(toTSON(msg))
    },
    
    sendResponse = function(msg){
      self$send(list(kind=tson.scalar("response"),
                     message=msg))
    },
    
    sendRequest = function(msg){
      self$send(list(kind=tson.scalar("request"),
                     message=msg))
    },
    
    sendContextError = function(contextId, error){
      self$send(list(
        kind=tson.scalar("contextError"),
        contextId=tson.scalar(contextId),
        error= tson.scalar(toString(error)) ))
    },
    
    sendNoContextError = function(error){
      self$send(list(kind=tson.scalar("noContextError"),
                     error= tson.scalar(toString(error)) ))
    },
    
    sendError = function(id, error){
      self$sendResponse(list(id=tson.scalar(id),
                             type=tson.scalar("error"),
                             error= tson.scalar(toString(error)) ))
    },
    
    sendVoid = function(id){
      self$sendResponse(list(id=tson.scalar(id),
                             type=tson.scalar("void") ))
    },
    
    addApp = function(operatorId, pamAppDefinition){
      app = PamApp$new(operatorId, pamAppDefinition)
      app$initializeWithSession(self)
      self$addOperator(operatorId, app)
    },
    
    addSourceCodeOperator = function(operatorId, sourceCode){
      operator = Operator$new(operatorId)
      operator$sourceCode(sourceCode)
    },
    
    addOperator = function(operatorId, operator){
      self$operatorByIds$set(operatorId, operator)
    },
    
    getOperator = function(operatorId){
      return(self$operatorByIds$get(operatorId))
    } 
    
  )
)

BNTestSession = R6Class(
  "BNSession",
  public = list(
    
    sessionDispatch = NULL,
    operator = NULL,
    sendMessageHandler = NULL,
    sessionType = NULL,
    
    initialize = function(sessionDispatch, operator, sessionType, sendMessageHandler) {
      self$sessionType = sessionType
      self$sessionDispatch = sessionDispatch
      self$operator = operator
      self$sendMessageHandler = sendMessageHandler
    },
    
    dispatchSiny = function(input, output, session) {
         
      query = isolate(parseQueryString(session$clientData$url_search))
      contextId = 'testContextId'
      tryCatch({
         
        context = BNSessionContext$new( 'wokflowId',
                                       'stepId',
                                       contextId, self)
         
        if (self$sessionType == "run"){
          self$operator$shinyServerRun(input, output, session, context)
        } else if (self$sessionType == "show"){
          self$operator$shinyServerShowResults(input, output, session, context)
        } else {
          self$operator$shinyServerRun(input, output, session, context)
        }
      }, error = function(e) {
        self$sendContextError(contextId, e)
        stop(e)
      })
    },
    
    processRequest = function(request){
      tryCatch({
        request$processOn(self)
      }, error = function(e) {
        # traceback()
        if (is.null(request)){
          self$sendNoContextError(e)
        } else {
          self$sendError(request$id, e)
        }
      })
    },
    
    processResponse = function(response){
      tryCatch({
        response$processOn(self)
      }, error = function(e) {
        self$sendNoContextError(e)
      })
    },
    
    send = function(messageEnvelope){
      self$sendMessageHandler(messageEnvelope)
    },
    
    sendResponse = function(msg){
      self$send(list(kind=tson.scalar("response"),
                     message=msg))
    },
    
    sendRequest = function(msg){
      self$send(list(kind=tson.scalar("request"),
                     message=msg))
    },
    
    sendContextError = function(contextId, error){
      self$send(list(
        kind=tson.scalar("contextError"),
        contextId=tson.scalar(contextId),
        error= tson.scalar(toString(error)) ))
    },
    
    sendNoContextError = function(error){
      self$send(list(kind=tson.scalar("noContextError"),
                     error= tson.scalar(toString(error)) ))
    },
    
    sendError = function(id, error){
      self$sendResponse(list(id=tson.scalar(id),
                             type=tson.scalar("error"),
                             error= tson.scalar(toString(error)) ))
    },
    
    sendVoid = function(id){
      self$sendResponse(list(id=tson.scalar(id),
                             type=tson.scalar("void") ))
    }
#     ,
#     
#     addOperator = function(operatorId, operator){
#       self$operatorByIds$set(operatorId, operator)
#     },
#     
#     getOperator = function(operatorId){
#       return(self$operatorByIds$get(operatorId))
#     }
    
  )
)