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