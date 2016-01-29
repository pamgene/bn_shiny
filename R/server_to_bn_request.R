responseFromJson = function(json){
  print("responseFromJson")
  type = json[["type"]]
  print(type)
  if (identical(type, "")){
    stop(paste0("unknown response type : ", type))  
  }   else {
    stop(paste0("unknown response type : ", type))  
  }
}