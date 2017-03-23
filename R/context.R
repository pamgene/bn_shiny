BNSessionContext  <- R6Class(
  'BNSessionContext',
  public = list(
    workflowId = NULL,
    stepId = NULL,
    contextId = NULL,
    session = NULL,
    operator = NULL,
    initialize = function(workflowId, stepId, contextId, session, operator){
      self$workflowId = workflowId
      self$stepId = stepId
      self$contextId = contextId
      self$session = session
      self$operator = operator
      
      if (is.null(self$workflowId)) stop("BNContext : workflowId is null")
      if (is.null(self$stepId)) stop("BNContext : stepId is null")
      if (is.null(self$contextId)) stop("BNContext : contextId is null")
      if (is.null(self$session)) stop("BNContext : session is null")
      if (is.null(self$operator)) stop("BNContext : operator is null")
      if (!inherits(session, "BNSession"))
        stop("BNContext : 'session' is not a BNSession object.")
      if (!inherits(operator, "Operator"))
        stop("BNContext : 'operator' is not a Operator object.")
      
    },
    processRequest = function(reactiveRequest){
      if (!inherits(reactiveRequest, "BNReactiveRequest"))
        stop("BNSessionContext processRequest : 'reactiveRequest' is not a BNReactiveRequest object.")
      reactiveRequest$processContext(self)
      return(reactiveRequest$reactiveValues)
    },
    toTson = function() list(workflowId=tson.scalar(self$workflowId), stepId=tson.scalar(self$stepId), contextId=tson.scalar(self$contextId)),
    getProperties = function() self$processRequest(BNGetPropertiesRequest$new()), 
    getPropertiesAsMap = function() self$processRequest(BNGetPropertiesAsMapRequest$new()), 
    getFolder = function() self$processRequest(BNGetFolderRequest$new()),
    getRunFolder = function() self$processRequest(BNGetRunFolderRequest$new()),
    # getCurveFitParams = function()self$processRequest(BNGetCurveFitParamsRequest$new()),
    getData = function() {
      return(self$processRequest(BNGetDataRequest$new(isBiobase=!inherits(self$operator, "PackageOperator"))))
    },
    error = function(error) self$session$sendContextError(self$contextId, error),
    
    setOrders = function(rowOrder,colOrder){
      request = BNSetOrderRequest$new()
      orders = list()
      if (!is.null(rowOrder)) orders[['rowOrder']] = as.integer(rowOrder)
      if (!is.null(colOrder)) orders[['colOrder']] = as.integer(colOrder)
      
      request$value = orders
      self$processRequest(request)
    },
    
    saveFile = function(file=NULL, object){
      save(file = file, object)
    },
    loadFile = function(file){
      load(file)
    },
    
    setResult = function(result){
      if (is.null(result)){
        stop('BNSessionContext setResult : result cannot be null')
      }
      if (inherits(self$operator, "PackageOperator")){
        if (inherits(result, "AnnotatedData") || inherits(result, "data.frame")|| inherits(result, "Cube")){
          request = BNSetResultRequest$new()
          request$validate(result)
          request$value = result
          self$processRequest(request)
        }  else {
          stop("BNSessionContext setResult :  a data.frame or AnnotatedData or Cube is expected")
        }
      } else {
        if (inherits(result, "data.frame") || inherits(result, "AnnotatedDataFrame")){
          request = BNSetResultRequest$new()
          result = backwardCheckResult(result)
          request$value = result
          self$processRequest(request)
        }  else {
          stop("BNSessionContext setResult : a data.frame or an AnnotatedDataFrame is expected")
        }
      }
    }
  )
)