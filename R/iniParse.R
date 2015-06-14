#'@name iniParse
#'
#'@title Parse an Windows like ini file to get the the entries
#'@description  Any Windows type ini file will be parsed so that the sections and the variables may be used within R for all kind of settings.
#'
#'@usage iniParse(fname.control)
#'@author Gabor Grothendieck <ggrothendieck at gmail.com>,  
#' \cr
#' \emph{Maintainer:} Chris Reudenbach \email{giswerk@@gis-ma.org}
#'
#'@references \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#'            \url{https://stat.ethz.ch/pipermail/r-help/2007-June/134115.html}
#' 
#'@param fname.control file name of control input file
#'
#'@return iniParse returns a list ordered by sections variablenames and values 
#'
#'@export iniParse
#'@examples   
#'#### Example to parse an windows type of ini file
#'#### create a list for each ini section containing the variables and params
#'       
#' ini.example=system.file("demo.ini", package="Rpeak")
#' iniParse(ini.example)


iniParse <- function(fname.control)
{
  ini.file <- file(fname.control)
  Lines  <- readLines(ini.file)
  close(ini.file)
  
  Lines <- chartr("[]", "==", Lines)  # change section headers
  
  ini.file <- textConnection(Lines)
  d <- read.table(ini.file, as.is = TRUE, sep = "=", fill = TRUE)
  close(ini.file)
  
  L <- d$V1 == ""                    # location of section breaks
  d <- subset(transform(d, V3 = V2[which(L)[cumsum(L)]])[1:3],
              V1 != "")
  
  to.parse  <- trim(paste("ini.list$", trim(d$V3), "$",  trim(d$V1), " <- '",
                          trim(d$V2), "'", sep=""))
  
  ini.list <- list()
  eval(parse(text=to.parse))
  
  return(ini.list)
}
