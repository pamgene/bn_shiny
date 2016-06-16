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


