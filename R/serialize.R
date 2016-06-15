library(rtson)
    
data.frame.asTSON <- function(df){
  data = lapply(df, function(c){
    if (is.factor(c)){
      return (list(kind=tson.character("factor"), values=as.integer(c), labels=levels(c)))
    } else if (is.logical(c)){
      return (list(kind=tson.character("bool"), values=as.integer(c)))
    } else {
      return (as.vector(c))
    }
  })
  
  return (list(kind=tson.character("data.frame") ,  data=data ))
}

data.frame.fromTSON <- function(tson){
  list = tson$data
  dfc = list()
  for (c in list){
    if (is.list(c)){
      kind = c$kind
      if (!is.null(kind)){
        if (kind == "bool"){
          values = c$values
          if (is.null(values)) stop("data.frame.fromTSON failed values missing")
          dfc[[length(dfc) + 1]] = as.logical(c$values)
        } else if (kind == 'factor'){
          dfc[[length(dfc) + 1]] = factor(c$values, labels=c$labels)
        } else {
          stop(paste0("data.frame.fromTSON failed kind is unknown : ", kind))
        }
      } else {
        stop("data.frame.fromTSON failed kind is null")
      }
    } else {
      dfc[[length(dfc) + 1]] = c
    }
  }
  d <- data.frame(dfc)
  
  colnames(d) <- make.names(names(list))
   
  if (!is.null(tson$attr)){
    attributes(d) <- c(attributes(d), tson$attr)
  }
 
  return (d)
}

annotated.data.frame.asTSON <- function(annotatedDataframe){
  return (list(kind=tson.character("AnnotatedDataFrame"), varMetadata=data.frame.asTSON(Biobase::varMetadata(annotatedDataframe)), data=data.frame.asTSON(Biobase::pData(annotatedDataframe))))
}

annotated.data.frame.fromTSON <- function(tson){
   
  data = data.frame.fromTSON(tson$data)
  
  varMetadata=data.frame.fromTSON(tson$varMetadata)
   
  ann = (Biobase::AnnotatedDataFrame(data=data, varMetadata=varMetadata))
   
  return (ann)
}

object.asTSON <- function(result){
  
  if (class(result) == "data.frame"){
    list <- data.frame.asTSON(result)
  } else if (class(result) == "AnnotatedDataFrame"){
    list <- annotated.data.frame.asTSON(result)
  } else {
    stop("object.asTSON failed : unknown class ")
  }
}
