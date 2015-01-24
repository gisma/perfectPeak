#'@title Special Tools
#'@export trim
#'@export rowMin
#'@export InstalledPackage
#'@export CRANChoosen
#'@export UsePackage
#'
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

rowMin = function(x) {
  # Construct a call pmin(x[,1],x[,2],...x[,NCOL(x)])
  code = paste("x[,",1:(NCOL(x)),"]",sep="",collapse=",")
  code = paste("pmin(",code,")")
  return(eval(parse(text=code)))
} 

###  Juan Antonio Cano http://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them/8863460#8863460
InstalledPackage <- function(package) 
{
  available <- suppressMessages(suppressWarnings(sapply(package, require, quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE)))
  missing <- package[!available]
  if (length(missing) > 0) return(FALSE)
  return(TRUE)
}

CRANChoosen <- function()
{
  return(getOption("repos")["CRAN"] != "@CRAN@")
}

UsePackage <- function(package, defaultCRANmirror = "http://cran.at.r-project.org") 
{
  if(!InstalledPackage(package))
  {
    if(!CRANChoosen())
    {       
      chooseCRANmirror()
      if(!CRANChoosen())
      {
        options(repos = c(CRAN = defaultCRANmirror))
      }
    }
    
    suppressMessages(suppressWarnings(install.packages(package)))
    if(!InstalledPackage(package)) return(FALSE)
  }
  return(TRUE)
}
###  Juan Antonio Cano http://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them/8863460#8863460
