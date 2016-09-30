requestFromJson = function(json){
  type = json[["type"]]
  if (identical(type, "BNSessionIdRequest")){
    return(BNSessionIdRequest$new(json=json))
  } else if (identical(type, "BNAddOperatorRequest")){
    return(BNAddOperatorRequest$new(json=json))
  } else if (identical(type, "BNOperatorPropertiesRequest")){
    return(BNOperatorPropertiesRequest$new(json=json))
  } else if (identical(type, "BNHasOperatorRequest")){
    return(BNHasOperatorRequest$new(json=json))
  } else if (identical(type, "BNOperatorCapabilityRequest")){
    return(BNOperatorCapabilityRequest$new(json=json))
  } else if (identical(type, "BNDataFrameOperatorRequest")){
    return(BNDataFrameOperatorRequest$new(json=json))
  } else if (identical(type, "BNShowResultRequest")){
    return(BNShowResultRequest$new(json=json))
  }else if (identical(type, "BNFittingTableRequest")){
    return(BNFittingTableRequest$new(json=json))
  } else if (identical(type, "BNRunAppRequest")){
    return(BNRunAppRequest$new(json=json))
  } else if (identical(type, "BNAddAppRequest")){
    return(BNAddAppRequest$new(json=json))
  }  else {
    stop(paste0("unknown request type : ", type))  
  }
}

BNSessionIdRequest = R6Class(
  "BNSessionIdRequest",
  inherit = BNRequest,
  public =list(
    processOn = function(bnSession){
      response = list(id=tson.scalar(self$id),
                      type=tson.scalar(self$type),
                      sessionId=tson.scalar(bnSession$sessionId))
      bnSession$sendResponse(response)
    }
  )
)

# abstract
BNOperatorRequest = R6Class(
  "BNOperatorRequest",
  inherit = BNRequest,
  active = list(
    operatorId = function(value){
      if (missing(value)) return(self$json$operatorId)
      else self$json$operatorId <- value
    }
  )
)

BNAddOperatorRequest = R6Class(
  "BNAddOperatorRequest",
  inherit = BNOperatorRequest,
  public =list(
    processOn = function(bnSession){
       
      bnSession$addSourceCodeOperator(self$operatorId, self$code)
      bnSession$sendVoid(self$id)
    }
  ),
  active = list(
    code = function(value){
      if (missing(value)) return(self$json$code)
      else self$json$code <- value
    }
  )
)

BNAddAppRequest = R6Class(
  "BNAddAppRequest",
  inherit = BNOperatorRequest,
  public =list(
    processOn = function(bnSession){
      
      bnSession$addApp(self$operatorId, self$pamAppDefinition, self)
      # response will be send async, once setup complete
      # bnSession$sendVoid(self$id)
    }
  ),
  active = list(
    pamAppDefinition = function(value){
      if (missing(value)) return(PamAppDefinition$new(json=self$json$pamAppDefinition))
      else self$json$pamAppDefinition <- value$toJson()
    }
  )
)

BNOperatorPropertiesRequest = R6Class(
  "BNOperatorPropertiesRequest",
  inherit = BNOperatorRequest,
  public =list(
    processOn = function(bnSession){
      operator = bnSession$getOperator(self$operatorId)
      props = operator$operatorProperties()
      
#       props = lapply(props, function(prop){
#         
#         
#         
#       })
      
      
      response = list(id=tson.scalar(self$id),
                      type=tson.scalar(self$type),
                      properties=operator$operatorProperties())
      bnSession$sendResponse(response)
    }
  )
)

BNHasOperatorRequest = R6Class(
  "BNHasOperatorRequest",
  inherit = BNOperatorRequest,
  public =list(
    processOn = function(bnSession){
      flag = bnSession$hasOperator(self$operatorId)
      response = list(id=tson.scalar(self$id),
                      type=tson.scalar(self$type),
                      hasOperator=tson.scalar(flag))
      bnSession$sendResponse(response)
    }
  )
)

BNOperatorCapabilityRequest = R6Class(
  "BNOperatorCapabilityRequest",
  inherit = BNOperatorRequest,
  public =list(
    processOn = function(bnSession){
       
      operator = bnSession$getOperator(self$operatorId)
       
      operator$capability()
      
      response = list(id=tson.scalar(self$id),
                      type=tson.scalar(self$type),
                      capability=operator$capability())
       
      bnSession$sendResponse(response)
    }
  )
)

BNDataFrameOperatorRequest = R6Class(
  "BNDataFrameOperatorRequest",
  inherit = BNOperatorRequest,
  public =list(
    processOn = function(bnSession){
      operator = bnSession$getOperator(self$operatorId)
      data = annotated.data.frame.fromTSON(self$data)
      result = operator$dataFrameOperator(DataFrameOperatorParam$new(data,self$properties,self$folder))
      if (!is.null(result)){
        if (class(result) == "data.frame"){
          result <- data.frame.asTSON(result)
        } else if (class(result) == "AnnotatedDataFrame"){
          result <- annotated.data.frame.asTSON(result)
        } else {
          stop("result : unknown class ")
        }
      } else {
        stop("result : null ")
      }
      
      response = list(id=tson.scalar(self$id),
                      type=tson.scalar(self$type),
                      data=result)
      bnSession$sendResponse(response)
    }
  ),
  active = list(
    properties = function(value){
      if (missing(value)) return(self$json$properties)
      else self$json$properties <- value
    },
    folder = function(value){
      if (missing(value)) return(self$json$folder)
      else self$json$folder <- value
    },
    data = function(value){
      if (missing(value)) return(self$json$data)
      else self$json$data <- value
    }
  ) 
)

BNShowResultRequest = R6Class(
  "BNShowResultRequest",
  inherit = BNOperatorRequest,
  public =list(
    processOn = function(bnSession){
      operator = bnSession$getOperator(self$operatorId)
      operator$showResults(ShowResultParam$new(self$properties,self$folder))
      bnSession$sendVoid(self$id)
    }
  ),
  active = list(
    properties = function(value){
      if (missing(value)) return(self$json$properties)
      else self$json$properties <- value
    },
    folder = function(value){
      if (missing(value)) return(self$json$folder)
      else self$json$folder <- value
    }
  ) 
)

BNFittingTableRequest = R6Class(
  "BNFittingTableRequest",
  inherit = BNOperatorRequest,
  public =list(
    processOn = function(bnSession){
      operator = bnSession$getOperator(self$operatorId)
      result = operator$curveFittingTable(CurveFittingTableParam$new(data.frame.fromTSON(self$data),self$xValues, self$properties))
      
      response = list(id=tson.scalar(self$id),
                      type=tson.scalar(self$type),
                      data=result)
      
      bnSession$sendResponse(response)
    }
  ),
  active = list(
    properties = function(value){
      if (missing(value)) return(self$json$properties)
      else self$json$properties <- value
    },
    xValues = function(value){
      if (missing(value)) return(self$json$xValues)
      else self$json$xValues <- value
    },
    data = function(value){
      if (missing(value)) return(self$json$data)
      else self$json$data <- value
    }
  ) 
)


BNRunAppRequest = R6Class(
  "BNRunAppRequest",
  inherit = BNOperatorRequest,
  public =list(
    processOn = function(bnSession){
      operator = bnSession$getOperator(self$operatorId)
      context = BNSessionContext$new(self$workflowId, self$stepId, self$contextId, bnSession, operator)
      bnSession$sendVoid(self$id)
      operator$runApp(context)
      shiny:::flushReact()
    }
  ),
  active = list(
    workflowId = function(value){
      if (missing(value)) return(self$json$workflowId)
      else self$json$workflowId <- value
    },
    stepId = function(value){
      if (missing(value)) return(self$json$stepId)
      else self$json$stepId <- value
    },
    contextId = function(value){
      if (missing(value)) return(self$json$contextId)
      else self$json$contextId <- value
    }
  ) 
)


