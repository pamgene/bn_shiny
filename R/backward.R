#' @export
backwardCheckResult = function(result){
  if (inherits(result, "AnnotatedDataFrame")){
    result = backwardCheckAnnotatedDataResult(result)
  } else if (inherits(result, "data.frame")){
    result = backwardCheckDataFrameResult(result)
  } else {
    stop("Result validation : wrong object type : result must be an AnnotatedData or a data.frame object")
  }
  return(result)
}

#' @export
backwardCheckAnnotatedDataResult = function(result){
  metaData = varMetadata(result)
  df = pData(result)
  bQtCols = metaData$groupingType == "QuantitationType"
  for(cname in colnames(df)[bQtCols]){
    df[[cname]] = na2nan(df[[cname]])
  }
  bAnnCols = metaData$groupingType == "Spot"| metaData$groupingType == "Array"
  for(cname in colnames(df)[bAnnCols]){
    if (all( is.na(df[[cname]]) )){
      df[[cname]] = na2nan(df[[cname]])
    } else if (is.numeric(df[[cname]]) ){
      df[[cname]] = na2nan(df[[cname]])
    }
  }
  pData(result) = df
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
  bna = is.na(x)
  x[bna] = NaN
  return(x)
}
