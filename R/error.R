Error <- R6Class(
  "Error",
  public = list(
    statusCode = NA,
    error = NA,
    reason = NA,
    initialize = function(statusCode,error,reason) {
      self$statusCode <- statusCode
      self$error <- error
      self$reason <- reason
    },
    toTson = function(){
      return (list(statusCode=tson.scalar(self$statusCode),
                   error=tson.scalar(self$error),
                   reason=tson.scalar(self$reason)))
    }
  )
)