
#' @export
validateResult = function(result){
  if (inherits(result, "AnnotatedData")){
    validateAnnotatedDataResult(result)
  } else if (inherits(result, "data.frame")){
    validateDataFrameResult(result)
  } else if (inherits(result, "Cube")) {
    
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
    c = data[[cname]]
    if (!is.double(c)) stop(paste('Result validation : qt columns must be of type double :', cname, 'class', class(c)))
  }
}

#' @export
validateDataFrameResult = function(df){
  
  # ensure that column are factor or character or double
  cnames = colnames(df)
  for (cname in cnames){
    c = df[[cname]]
    if (!(is.factor(c) || is.character(c) || is.double(c))){
      stop(paste('Result validation : columns must be of type (factor | character | double) :',cname, 'class', class(c)))
    } 
    if (is.double(c)){
      if(any(is.na(c) & !is.nan(c))){
        stop(paste('Result validation : columns of type double must not contains NA :',cname))
      } 
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

