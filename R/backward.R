#' @export
backwardCheckResult = function(result){
  if (inherits(result, "AnnotatedData")){
    result = backwardCheckAnnotatedDataResult(result)
  } else if (inherits(result, "data.frame")){
    result = backwardCheckDataFrameResult(result)
  } else {
    stop("Result validation : wrong object type : result must be an AnnotatedData or a data.frame object")
  }
}

#' @export
backwardCheckAnnotatedDataResult = function(result){
  metaData = varMetadata(result)
  bQtCols = metaData$groupingType == "QuantitationType"
  for(cname in colnames(result)[bQtCols]){
    result[cname] = na2nan(result[cname])
  }
  bAnnCols = metatData$groupingType == "Spot"| metaData$groupingType == "Array"
  for(cname in colnames(result)[bAnnCols]){
    if(is.numeric(result[cname])){
      result[cname] = na2nan(result[cname])
    }
  }
  return(result)
}

#' @export
backwardCheckDataFrameResult = function(result){
  # Check all the columns that are not rowSeq and Colseq, replace all NA with NaN
  qtCols = !(colnames(result) %in% c("rowSeq", "colSeq"))
  for(cname in colnames(result)[qtCols]){
     result[cname] = na2nan(result[cname])
  }
  return(result)
}

na2nan = function(x){
  x[is.na(x)] = NaN
}
