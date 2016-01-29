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
          print(paste0("unknown message kind : "), kind)
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
      else self$json$id <- value
    },
    type = function(value){
      if (missing(value)) return(self$json$type)
      else self$json$type <- value
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
