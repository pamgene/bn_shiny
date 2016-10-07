
#' @export
validateResult = function(result){
  if (inherits(result, "AnnotatedData")){
    validateAnnotatedDataResult(result)
  } else if (inherits(result, "data.frame")){
    validateDataFrameResult(result)
  } else {
    stop("Result validation : wrong object type : result must be an AnnotatedData or a data.frame object")
  }
}

#' @export
validateAnnotatedDataResult = function(annotatedResult){
  
  data = annotatedResult$data
  
  # validate the data.frame
  validateDataFrameResult(data)
   
  # ensure that qts are double
  qtColNames = annotatedResult$qtColumnNames
  
  for (cname in qtColNames){
    if (!is.double(data[[cname]])) stop(paste('Result validation : qt columns must be of type double :', cname))
  }
}

#' @export
validateDataFrameResult = function(df){
  
  # ensure that column are factor or character or double
  cnames = colnames(df)
  for (cname in cnames){
    c = df[[cname]]
    if (!(is.factor(c) || is.character(c) || is.double(c))){
      stop(paste('Result validation : columns must be of type (factor | character | double) :',cname))
    }
  }
   
  if (!("colSeq" %in% colnames(df))){
    stop('Result validation : column colSeq is required')
  }
  
  if (!("rowSeq" %in% colnames(df))){
    stop('Result validation : column rowSeq is required')
  }
  
  
  if (min(df[["colSeq"]]) < 1){
    stop('Result validation : colSeq values must be greater or equals to 1')
  }
  if (min(df[["rowSeq"]]) < 1){
    stop('Result validation : rowSeq values must be greater or equals to 1')
  }
}

