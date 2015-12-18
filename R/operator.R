Operator <- R6Class(
  "Operator",
  public = list(
    env = NA,
    initialize = function(){
      
    },
    sourceCode = function(lines){
      self$env = new.env(parent = globalenv())
      fun <- sourceUTF8FromCode(lines, envir = self$env)
    },
    showResultWithContext = function(context){
      fun = self$env$showResults
      if (is.null(fun)){
        return (NULL)
      } else {
        fun(properties=context$getProperties(), folder=context$getFolder())
        return (NULL)
      }
    },
    dataFrameOperatorWithContext = function(context){
#       fun = self$env$dataFrameOperator
#       if (is.null(fun)){
#         return (NULL)
#       } else {
#         data = context$getData()
#         print(data)
#         properties=context$getProperties()
#         print(properties)
#         folder=context$getFolder()
#         print(folder)
#         result = fun(data=data, properties=properties, folder=folder)
#         print("result")
#         print(result)
#         return (result)
#       }
    },
    curveFitOperatorWithContext = function(context){
#       fun = self$env$curveFitOperatorFunction
#       if (is.null(fun)){
#         return (NULL)
#       } else {
#         
#         param = context$getCurveFitParams()
#         xValues = param$xValues
#         properties = param$properties
#         data = as.data.frame(t(param$data))
#         
#         fit <- function(x) {
#           yValues <- try( fun(xValues , x ,  properties) , silent = TRUE)
#           if(inherits(yValues, "try-error")){
#             yValues <- xValues+NaN
#           }  
#           return (yValues)
#         } 
#         list = as.double(unlist(lapply(data, fit)))
#         
#         return (list)
#       }
    },
     capability = function(){
      answer = list()
       
      
      if (!is.null(self$env$shinyServerRun)){
        answer[["run"]] = tson.scalar("shiny")
      } else if (!is.null(self$env$dataFrameOperator)){
        answer[["run"]] = tson.scalar("default")
      }
       
      if (!is.null(self$env$shinyServerShowResults)){
        answer[["showResults"]] = tson.scalar("shiny")
      } else if (!is.null(self$env$showResults)){
        answer[["showResults"]] = tson.scalar("default")
      }
      if (!is.null(self$env$curveFitOperatorFunction)){
        answer[["hasCurveFitting"]] = tson.scalar(TRUE)
      }
      return(answer)
    },
    curveFittingTable = function(param){
      if (!inherits(param, "CurveFittingTableParam"))
        stop("'param' is not a CurveFittingTableParam object.")
      curveFitOperatorFunction = self$env$curveFitOperatorFunction
      xValues = param$xValues
      properties = param$properties
      data = as.data.frame(t(param$data))
       
      fit <- function(x) {
        yValues <- try( curveFitOperatorFunction(xValues , x ,  properties) , silent = TRUE)
        if(inherits(yValues, "try-error")){
          yValues <- xValues+NaN
        }  
        return (yValues)
      } 
      list = as.double(unlist(lapply(data, fit)))
      
      return (list)
    },
    dataFrameOperator = function(operatorParam){
 
      if (!inherits(operatorParam, "DataFrameOperatorParam"))
        stop("'operatorParam' is not a DataFrameOperatorParam object.")
      fun = self$env$dataFrameOperator
      if (is.null(fun)){
        return (NULL)
      } else {
        dataFrame <- fun(operatorParam$data, properties=operatorParam$properties,folder=operatorParam$folder)
        print("dataFrameOperator done")
        return (dataFrame)
      }
    },
    operatorProperties = function(){
      fun = self$env$operatorProperties
      if (is.null(fun)){
        return (list())
      } else {
        return (fun())
      }
    },
    showResults = function(showResultParam){
      if (!inherits(showResultParam, "ShowResultParam"))
        stop("'showResultParam' is not a ShowResultParam object.")
      fun = self$env$showResults
      if (is.null(fun)){
        return (NULL)
      } else {
        fun(properties=showResultParam$properties, folder=showResultParam$folder)
        return (NULL)
      }
    },
    shinyServerRun = function(input, output, session, context){
      fun = self$env$shinyServerRun
      if (is.null(fun)){
        context$error(Error$new(500,"operator.shinyServerRun.missing","shinyServerRun function is undefined"))
      } else {
        fun(input, output, session, context)
      }
    },
    shinyServerShowResults = function(input, output, session, context){
      fun = self$env$shinyServerShowResults
      if (is.null(fun)){
        context$error(Error$new(500,"operator.shinyServerShowResults.missing","shinyServerShowResults function is undefined"))
      } else {
        fun(input, output, session, context)
      }
    },
    runApp = function(context){
      print("runApp")
      fun = self$env$runApp
      if (is.null(fun)){
        context$error(Error$new(500,"operator.runApp.missing","runApp function is undefined"))
      } else {
        tryCatch({
          fun(context)
        }, error = function(e) {
          context$error(Error$new(500,"operator.runApp.runtime.error",toString(e)))
        })
      }
    }
  )
)

DataFrameOperatorParam<- R6Class(
  "DataFrameOperatorParam",
  public = list(
    data = NA,
    properties = NA,
    folder = NA,
    initialize = function(data,properties,folder) {
      self$data <- data
      self$properties <- properties
      self$folder <- folder
    }
  )
)

ShowResultParam<- R6Class(
  "ShowResultParam",
  public = list(
    properties = NULL,
    folder = NULL,
    initialize = function(properties,folder) {
      if (is.null(properties)) stop("properties must not be null")
      if (!is.list(properties)) stop("properties must be a list")
      if (is.null(folder)) stop("folder must not be null")
      if (!is.character(folder)) stop("folder must be a character")
      if (length(folder) != 1) stop("folder must be a scalar")
      self$properties <- properties
      self$folder <- folder
    }
  )
)

CurveFittingTableParam<- R6Class(
  "CurveFittingTableParam",
  public = list(
    data = NULL,
    xValues = NULL,
    properties = NULL,
    initialize = function(data,xValues,properties) {
      if (is.null(data)) stop("data must not be null")
      if (!is.data.frame(data)) stop("data must be a data.frame")
      if (is.null(xValues)) stop("xValues must not be null")
      if (!is.numeric(xValues)) stop("data must be a numeric")
      if (is.null(properties)) stop("properties must not be null")
      if (!is.list(properties)) stop("properties must be a list")
      self$data = data
      self$xValues = xValues
      self$properties = properties
    }
  )
)