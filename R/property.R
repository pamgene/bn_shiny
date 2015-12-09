Property <- R6Class(
  "Property",
  public = list(
    name = NA,
    value = NA,
    initialize = function(name,value) {
      self$name <- name
      self$value <- value
    },
    as.list = function(){
      return (list(self$name,self$value))
    }
  )
)