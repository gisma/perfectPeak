#'@title Special Tools
#'@export trim
#'@export rowMin
#'
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

rowMin = function(x) {
  # Construct a call pmin(x[,1],x[,2],...x[,NCOL(x)])
  code = paste("x[,",1:(NCOL(x)),"]",sep="",collapse=",")
  code = paste("pmin(",code,")")
  return(eval(parse(text=code)))
} 
