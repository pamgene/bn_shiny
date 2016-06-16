# library(httpuv)
library(rtson)
library(R6)

TestClient <- R6Class(
  "TestClient",
  public = list(
    uri = NA,
    initialize = function(uri) {
       self$uri = uri;
    },
    errorResponse = function(msg, response){
      stop(paste0("Http error : ", msg , " : response$status " , response$status))
    },
    getProperties = function(operatorId){
      response <- GET(paste0(self$uri,"/operator/properties/?operatorId=",operatorId))
      if (response$status != 200){
        self$errorResponse("getProperties", response)
      }
      json = httr::content(response, "text")
      return (fromJSON(json))
    },
    showResults = function(operatorId){
      GET(paste0(self$uri,"/operator/showResults/?operatorId=",operatorId))
    },
    addOperator = function(operatorId, code) {
      
      params = list(operatorId=tson.character(operatorId), code=tson.character(code))
      url = paste0(self$uri,"/operator/addOperator/")
      response <- POST(url, add_headers('Content-Type' = 'application/octet-stream'), body=toTSON(params))
      if (response$status != 200){
        self$errorResponse("addOperator", response)
      }
    },
    dataFrameOperator = function(operatorId, annotatedDataframe, properties, folder){
      params = list(properties=properties, folder=folder, data=annotated.data.frame.asTSON(annotatedDataframe))
      url = paste0(self$uri,"/operator/dataFrameOperator/?operatorId=", operatorId)
      response <- POST(url, add_headers('Content-Type' = 'application/octet-stream'), body=toTSON(params))
      if (response$status != 200){
        self$errorResponse("dataFrameOperator", response)
      }
      answer <- data.frame.asTSON(fromTSON(httr::content(response, as = "raw")))
      return (answer)
    }
  )
)