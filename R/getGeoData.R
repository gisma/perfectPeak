#'@name getGeoData
#'
#'@title Retrieves online geodata and converts it to raster/sp objects
#'
#'@description Downloads some geodatasets and and converts them in raster or spatial objects . The data is downloaded if necessary and then read from these files into raster objects. The function \code{\link{ccodes}} returns country names and the ISO codes. Based on the raster package getData function from Robert J. Hijmans it provides more global and regional geodata
#' 
#'@usage getGeoData(name, download=TRUE, path='', ...)
#' ccodes()
#'
#'@param name Data set name, currently supported are 'GADM', 'countries', 'SRTM', 'alt', 'worldclim', 'harrylist', 'OSM', 'tiroldem'. See Details for more info
#'@param download Logical \code{TRUE} data will be downloaded if not locally available
#'@param path Character Path name indicating where to store the data. Default is the current working directory 
#'@param ... Additional required (!) parameters. These are data set specific. See Details
#'@author Robert J. Hijmans 
#' \cr
#' \emph{Maintainer:} Chris Reudenbach \email{giswerk@@gis-ma.org}
#'
#'@return A spatial object (Raster* or Spatial*)
#'@details
#' \code{alt} stands for altitude (elevation); the data were aggregated from SRTM 90 m resolution data between -60 and 60 latitude. \cr
#' \code{GADM} is a database of global administrative boundaries. \cr
#' \code{worldclim} is a database of global interpolated climate data. \cr
#' \code{SRTM} refers to the hole-filled CGIAR-SRTM (90 m resolution). \cr
#' \code{countries} has polygons for all countries at a higher resolution than the 'wrld_simpl' data\cr in the maptools pacakge . \cr
#' \code{harrylist} is a list of world wide about 60.000 coordinates altitudes and names of summits\cr
#' \code{OSMp} is the OSM Point Data from the current OSM database\cr
#' \code{tiroldem} refers to the 10 m Lidar based DEM as provided by the Authorithy of Tirol. For Copyright and further information  see: \link{DEM}\cr
#'



#'If  \code{name}='alt' or \code{name}='GADM' you must provide a 'country=' argument. Countries are specified by their 3 letter ISO codes. Use getData('ISO3') to see these codes. In the case of GADM you must also provide the level of administrative subdivision (0=country, 1=first level subdivision). In the case of alt you can set 'mask' to FALSE. If it is TRUE values for neighbouring countries are set to NA. For example:\cr
#'     \code{getGeoData('GADM', country='FRA', level=1)}\cr
#'     \code{getGeoData('alt', country='FRA', mask=TRUE)}\cr
#' \cr
#'If  \code{name}='SRTM' you must provide 'lon' and 'lat' arguments (longitude and latitude). These should be single numbers somewhere within the SRTM tile that you want.\cr
#'    \code{getGeoData('SRTM', lon=5, lat=45)}\cr
#' \cr
#'If  \code{name}='worldclim' you must also provide a variable name 'var=', and a resolution 'res='. Valid variables names are 'tmin', 'tmax', 'prec' and 'bio'. Valid resolutions are 0.5, 2.5, 5, and 10 (minutes of a degree). In the case of res=0.5, you must also provide a lon and lat argument for a tile; for the lower resolutions global data will be downloaded. In all cases there are 12 (monthly) files for each variable except for 'bio' which contains 19 files.\cr
#'    \code{getGeoData('worldclim', var='tmin', res=0.5, lon=5, lat=45)} \cr
#'    \code{getGeoData('worldclim', var='bio', res=10)}\cr
#' \cr
#'If  \code{name=}'harrylist' you will download and clean the complete list\cr
#'    \code{getGeoData('harrylist')}\cr
#' \cr
#'If  \code{name}='OSMp' you must provide lat_min,lat_max,lon_min,lon_max for the boundig box. Additionally you must set  the switch 'all' to \code{FALSE} if you just want to download a specified item. Then you have to  provide the content of the desired items in the 'key' and 'val' argument. According to this combination you have to provide a tag list containing the Tags of the element c('name','ele').\cr
#'    \code{getGeoData('OSMp', extent=c(11.35547,11.40009,47.10114,47.13512), key='natural',val='peak',taglist=c('name','ele'))}\cr
#' \cr
#'If  \code{name}='tiroldem' you must set the switch 'all' to \code{FALSE} if you just want to download a specified item you have to set data=item. The list of allowd items is: \code{ibk_10m} Innsbruck, \code{il_10m} Innsbruck Land, \code{im_10m} Imst, \code{kb_10m} Kitzbühl, \code{ku_10m} Kufstein, \code{la_10m} Landeck, \code{re_10m} Reutte, \code{sz_10m} Schwaz, \code{lz_10m} Lienz (Osttirol). The data is correctly georeferenced. However you MUST use the following proj4 strings if you want to project other data acccording to the Austrian Datum. DO NOT USE the default EPSG Code string! All datasets except Lienz are projected with: ''+proj=tmerc +lat_0=0 +lon_0=10.33333333333333 +k=1 +x_0=0 +y_0=-5000000 +ellps=bessel +towgs84=577.326, 90.129, 463.919, 5.137, 1.474, 5.297, 2.4232 +units=m'. Item=lz_10m (Lienz) has an different 
#' Central_Meridian. You have to change it to 13.333333.\cr
#'    \code{getGeoData('tiroldem', data='ku_10m')} \cr
#'
#'@references
#'\url{http://www.worldclim.org}
#'\url{http://www.gadm.org}
#'\url{http://srtm.csi.cgiar.org/}
#'\url{http://diva-gis.org/gdata}
#'\url{http://www.tourenwelt.info}
#'\url{https://www.tirol.gv.at/data/datenkatalog/geographie-und-planung/digitales-gelaendemodell-tirol/}
#'\url{http://www.openstreetmap.org}


#'@export getGeoData
#'
#'@examples   
#'#### Example to initialize the enviroment and GIS bindings for use with R
#'#### uses the ini list from an ini file
#'       
#' getGeoData('tiroldem', data=item, all=FALSE)
#' getGeoData('OSMp', extent=c(11.35547,11.40009,47.10114,47.13512), key='natural',val='saddle',taglist=c('name','ele','direction'))
#' getGeoData('harrylist')
#' getGeoData('worldclim', var='tmin', res=0.5, lon=5, lat=45)
#' getGeoData('SRTM', lon=5, lat=45)
#' getGeoData('GADM', country='FRA', level=1) 


getGeoData <- function(name='GADM', download=TRUE, path='', ...) {
  library(raster)
  library(osmar)
  library(sp)
  library(maptools)
  path <- .getDataPath(path)
  if (name=='GADM') {
    .GADM(..., download=download, path=path)
  } else if (name=='SRTM') {
    .SRTM(..., download=download, path=path)
  } else if (name=='harrylist') {
    .harrylist(..., download=download, path=path)
  } else if (name=='tiroldem') {
    .tiroldem(..., download=download, path=path)
  } else if (name=='OSMp') {
    .OSMp(..., download=download, path=path)
  } else if (name=='alt') {
    .raster(..., name=name, download=download, path=path)
  } else if (name=='worldclim') {
    .worldclim(..., download=download, path=path)
  } else if (name=='CMIP5') {
    .cmip5(..., download=download, path=path)
  } else if (name=='ISO3') {
    ccodes()[,c(2,1)]
  } else if (name=='countries') {
    .countries(download=download, path=path, ...)
  } else {
    stop(name, ' not recognized as a valid name.')
  }
}


.download <- function(aurl, filename) {
  fn <- paste(tempfile(), '.download', sep='')
  res <- download.file(url=aurl, destfile=fn, method="wget", quiet = FALSE, mode = "wb", cacheOK = TRUE)
  if (res == 0) {
    w <- getOption('warn')
    on.exit(options('warn' = w))
    options('warn'=-1) 
    if (! file.rename(fn, filename) ) { 
      # rename failed, perhaps because fn and filename refer to different devices
      file.copy(fn, filename)
      file.remove(fn)
    }
  } else {
    stop('could not download the file' )
  }
}

.ISO <- function() {
  ccodes()
}

ccodes <- function() {
  path <- paste(system.file(package="raster"), "/external", sep='')
  d <- read.csv(paste(path, "/countries.csv", sep=""), stringsAsFactors=FALSE, encoding="UTF-8")
  return(as.matrix(d))
}


.getCountry <- function(country='') {
  country <- toupper(trim(country[1]))
  #  if (nchar(country) < 3) {
  #  	stop('provide a 3 letter ISO country code')
  #	}
  cs <- ccodes()
  try (cs <- toupper(cs))
  
  iso3 <- substr(toupper(country), 1, 3)
  if (iso3 %in% cs[,2]) {
    return(iso3)
  } else {
    iso2 <- substr(toupper(country), 1, 3)
    if (iso2 %in% cs[,3]) {
      i <- which(country==cs[,3])
      return( cs[i,2] )
    } else if (country %in% cs[,1]) {
      i <- which(country==cs[,1])
      return( cs[i,2] )
    } else {
      stop('provide a valid name or 3 letter ISO country code; you can get a list with: getData("ISO3")')
    }
  }
}


.getDataPath <- function(path) {
  path <- trim(path)
  if (path=='') {
    path <- .dataloc()
  } else {
    if (substr(path, nchar(path)-1, nchar(path)) == '//' ) {
      p <- substr(path, 1, nchar(path)-2)		
    } else if (substr(path, nchar(path), nchar(path)) == '/'  | substr(path, nchar(path), nchar(path)) == '\\') {
      p <- substr(path, 1, nchar(path)-1)
    } else {
      p <- path
    }
    if (!file.exists(p) & !file.exists(path)) {
      stop('path does not exist: ', path)
    }
  }
  if (substr(path, nchar(path), nchar(path)) != '/' & substr(path, nchar(path), nchar(path)) != '\\') {
    path <- paste(path, "/", sep="")
  }
  return(path)
}


.GADM <- function(country, level, download, path) {
  #	if (!file.exists(path)) {  dir.create(path, recursive=T)  }
  
  country <- .getCountry(country)
  if (missing(level)) {
    stop('provide a "level=" argument; levels can be 0, 1, or 2 for most countries, and higer for some')
  }
  
  filename <- paste(path, country, '_adm', level, ".RData", sep="")
  if (!file.exists(filename)) {
    if (download) {
      theurl <- paste("http://biogeo.ucdavis.edu/data/gadm2/R/", country, '_adm', level, ".RData", sep="")
      .download(theurl, filename)
      if (!file.exists(filename))	{ 
        cat("\nCould not download file -- perhaps it does not exist \n") 
      }
    } else {
      cat("\nFile not available locally. Use 'download = TRUE'\n")
    }
  }	
  if (file.exists(filename)) {
    thisenvir = new.env()
    data <- get(load(filename, thisenvir), thisenvir)
    return(data)
  } 
}




.countries <- function(download, path, ...) {
  #	if (!file.exists(path)) {  dir.create(path, recursive=T)  }
  filename <- paste(path, 'countries.RData', sep="")
  if (!file.exists(filename)) {
    if (download) {
      theurl <- paste("http://biogeo.ucdavis.edu/data/diva/misc/countries.RData", sep="")
      .download(theurl, filename)
      if (!file.exists(filename)) {
        cat("\nCould not download file -- perhaps it does not exist \n") 
      }
    } else {
      cat("\nFile not available locally. Use 'download = TRUE'\n")
    }
  }	
  if (file.exists(filename)) {
    thisenvir = new.env()
    data <- get(load(filename, thisenvir), thisenvir)
    return(data)
  } 
}


.cmip5 <- function(var, model, rcp, year, res, lon, lat, path, download=TRUE) {
  if (!res %in% c(2.5, 5, 10)) {
    stop('resolution should be one of: 2.5, 5, 10')
  }
  if (res==2.5) { res <- '2-5' }
  var <- tolower(var[1])
  vars <- c('tmin', 'tmax', 'prec', 'bio')
  stopifnot(var %in% vars)
  var <- c('tn', 'tx', 'pr', 'bi')[match(var, vars)]
  
  model <- toupper(model)
  models <- c('AC', 'BC', 'CC', 'CE', 'CN', 'GF', 'GD', 'GS', 'HD', 'HG', 'HE', 'IN', 'IP', 'MI', 'MR', 'MC', 'MP', 'MG', 'NO')
  stopifnot(model %in% models)
  
  rcps <- c(26, 45, 60, 85)
  stopifnot(rcp %in% rcps)
  stopifnot(year %in% c(50, 70))
  
  m <- matrix(c(0,1,1,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,0,1,0,1,1,1,0,0,1,1,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1), ncol=4)
  i <- m[which(model==models), which(rcp==rcps)]
  if (!i) {
    warning('this combination of rcp and model is not available')
    return(invisible(NULL))
  }
  
  path <- paste(path, '/cmip5/', res, 'm/', sep='')
  dir.create(path, recursive=TRUE, showWarnings=FALSE)
  
  zip <- tolower(paste(model, rcp, var, year, '.zip', sep=''))
  theurl <- paste('http://biogeo.ucdavis.edu/data/climate/cmip5/', res, 'm/', zip, sep='')
  
  zipfile <- paste(path, zip, sep='')
  if (var == 'bi') {
    n <- 19
  } else {
    n <- 12
  }
  tifs <- paste(extension(zip, ''), 1:n, '.tif', sep='')
  files <- paste(path, tifs, sep='')
  fc <- sum(file.exists(files))
  if (fc < n) {
    if (!file.exists(zipfile)) {
      if (download) {
        .download(theurl, zipfile)
        if (!file.exists(zipfile))	{ 
          cat("\n Could not download file -- perhaps it does not exist \n") 
        }
      } else {
        cat("\nFile not available locally. Use 'download = TRUE'\n")
      }
    }	
    unzip(zipfile, exdir=dirname(zipfile))
  }
  stack(paste(path, tifs, sep=''))
}

#.cmip5(var='prec', model='BC', rcp=26, year=50, res=10, path=getwd())


.worldclim <- function(var, res, lon, lat, path, download=TRUE) {
  if (!res %in% c(0.5, 2.5, 5, 10)) {
    stop('resolution should be one of: 0.5, 2.5, 5, 10')
  }
  if (res==2.5) { res <- '2-5' }
  stopifnot(var %in% c('tmean', 'tmin', 'tmax', 'prec', 'bio', 'alt'))
  path <- paste(path, 'wc', res, '/', sep='')
  dir.create(path, showWarnings=FALSE)
  
  if (res==0.5) {
    lon <- min(180, max(-180, lon))
    lat <- min(90, max(-60, lat))
    rs <- raster(nrows=5, ncols=12, xmn=-180, xmx=180, ymn=-60, ymx=90 )
    row <- rowFromY(rs, lat) - 1
    col <- colFromX(rs, lon) - 1
    rc <- paste(row, col, sep='') 
    zip <- paste(var, '_', rc, '.zip', sep='')
    zipfile <- paste(path, zip, sep='')
    if (var  == 'alt') {
      bilfiles <- paste(var, '_', rc, '.bil', sep='')
      hdrfiles <- paste(var, '_', rc, '.hdr', sep='')			
    } else if (var  != 'bio') {
      bilfiles <- paste(var, 1:12, '_', rc, '.bil', sep='')
      hdrfiles <- paste(var, 1:12, '_', rc, '.hdr', sep='')
    } else {
      bilfiles <- paste(var, 1:19, '_', rc, '.bil', sep='')
      hdrfiles <- paste(var, 1:19, '_', rc, '.hdr', sep='')		
    }
    theurl <- paste('http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/tiles/cur/', zip, sep='')
  } else {
    zip <- paste(var, '_', res, 'm_bil.zip', sep='')
    zipfile <- paste(path, zip, sep='')
    if (var  == 'alt') {
      bilfiles <- paste(var, '.bil', sep='')
      hdrfiles <- paste(var, '.hdr', sep='')			
    } else if (var  != 'bio') {
      bilfiles <- paste(var, 1:12, '.bil', sep='')
      hdrfiles <- paste(var, 1:12, '.hdr', sep='')
    } else {
      bilfiles <- paste(var, 1:19, '.bil', sep='')
      hdrfiles <- paste(var, 1:19, '.hdr', sep='')	
    }
    theurl <- paste('http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/', zip, sep='')
  }
  files <- c(paste(path, bilfiles, sep=''), paste(path, hdrfiles, sep=''))
  fc <- sum(file.exists(files))
  if (fc < 24) {
    if (!file.exists(zipfile)) {
      if (download) {
        .download(theurl, zipfile)
        if (!file.exists(zipfile))	{ 
          cat("\n Could not download file -- perhaps it does not exist \n") 
        }
      } else {
        cat("\nFile not available locally. Use 'download = TRUE'\n")
      }
    }	
    unzip(zipfile, exdir=dirname(zipfile))
    for (h in paste(path, hdrfiles, sep='')) {
      x <- readLines(h)
      x <- c(x[1:14], 'PIXELTYPE     SIGNEDINT', x[15:length(x)])
      writeLines(x, h)
    }
  }
  if (var  == 'alt') {
    st <- raster(paste(path, bilfiles, sep=''))
  } else {
    st <- stack(paste(path, bilfiles, sep=''))
  }
  projection(st) <- "+proj=longlat +datum=WGS84"
  return(st)
}



.raster <- function(country, name, mask=TRUE, path, download, keepzip=FALSE, ...) {
  
  country <- .getCountry(country)
  path <- .getDataPath(path)
  if (mask) {
    mskname <- '_msk_'
    mskpath <- 'msk_'
  } else {
    mskname<-'_'
    mskpath <- ''		
  }
  filename <- paste(path, country, mskname, name, ".grd", sep="")
  if (!file.exists(filename)) {
    zipfilename <- filename
    extension(zipfilename) <- '.zip'
    if (!file.exists(zipfilename)) {
      if (download) {
        theurl <- paste("http://biogeo.ucdavis.edu/data/diva/", mskpath, name, "/", country, mskname, name, ".zip", sep="")
        .download(theurl, zipfilename)
        if (!file.exists(zipfilename))	{ 
          cat("\nCould not download file -- perhaps it does not exist \n") 
        }
      } else {
        cat("\nFile not available locally. Use 'download = TRUE'\n")
      }
    }
    ff <- unzip(zipfilename, exdir=dirname(zipfilename))
    if (!keepzip) {
      file.remove(zipfilename)
    }
  }	
  if (file.exists(filename)) { 
    rs <- raster(filename)
  } else {
    #patrn <- paste(country, '.', mskname, name, ".grd", sep="")
    #f <- list.files(path, pattern=patrn)
    f <- ff[substr(ff, nchar(ff)-3, nchar(ff)) == '.grd']
    if (length(f)==0) {
      warning('something went wrong')
      return(NULL)
    } else if (length(f)==1) {
      rs <- raster(f)
    } else {
      rs <- sapply(f, raster)
      cat('returning a list of RasterLayer objects\n')
      return(rs)
    }
  }
  projection(rs) <- "+proj=longlat +datum=WGS84"
  return(rs)	
}



.SRTM <- function(lon, lat, download, path) {
  stopifnot(lon >= -180 & lon <= 180)
  stopifnot(lat >= -60 & lat <= 60)
  
  rs <- raster(nrows=24, ncols=72, xmn=-180, xmx=180, ymn=-60, ymx=60 )
  rowTile <- rowFromY(rs, lat)
  colTile <- colFromX(rs, lon)
  if (rowTile < 10) { rowTile <- paste('0', rowTile, sep='') }
  if (colTile < 10) { colTile <- paste('0', colTile, sep='') }
  
  f <- paste('srtm_', colTile, '_', rowTile, sep="")
  zipfilename <- paste(path, "/", f, ".ZIP", sep="")
  tiffilename <- paste(path, "/", f, ".TIF", sep="")
  
  if (!file.exists(tiffilename)) {
    if (!file.exists(zipfilename)) {
      if (download) { 
        theurl <- paste("ftp://xftp.jrc.it/pub/srtmV4/tiff/", f, ".zip", sep="")
        test <- try (.download(theurl, zipfilename) , silent=TRUE)
        if (class(test) == 'try-error') {
          theurl <- paste("http://hypersphere.telascience.org/elevation/cgiar_srtm_v4/tiff/zip/", f, ".ZIP", sep="")
          test <- try (.download(theurl, zipfilename) , silent=TRUE)
          if (class(test) == 'try-error') {
            theurl <- paste("http://srtm.csi.cgiar.org/SRT-ZIP/SRTM_V41/SRTM_Data_GeoTiff/", f, ".ZIP", sep="")
            .download(theurl, zipfilename)
          }
        }
      } else {cat('file not available locally, use download=TRUE\n') }	
    }
    if (file.exists(zipfilename)) { 
      unzip(zipfilename, exdir=dirname(zipfilename))
      file.remove(zipfilename)
    }	
  }
  if (file.exists(tiffilename)) { 
    rs <- raster(tiffilename)
    projection(rs) <- "+proj=longlat +datum=WGS84"
    return(rs)
  } else {
    stop('file not found')
  }
}

.tiroldem <- function(item, download, path) {
  stopifnot( (item == 'ibk_10m') | (item == 'il_10m') | (item == 'im_10m') | (item == 'kb_10m') |(item == 'ku_10m') |(item == 'la_10m') |(item == 're_10m') |(item == 'sz_10m') |(item == 'lz_10m') )
  
  f <- item
  zipfilename <- paste(path,  f, ".zip", sep="")
  ascfilename <- paste(path,  f, "_float.asc", sep="")
  
  if (!file.exists(zipfilename)) {
    if (download) { 
      theurl <- paste("https://gis.tirol.gv.at/ogd/geografie_planung/", f,".zip", sep="")
      test <- try (.download(theurl, zipfilename) , silent=TRUE)
    } else {cat('file not available locally, use download=TRUE\n') }  
  }
  if (file.exists(zipfilename)) { 
    unzip(zipfilename,junkpath=TRUE, exdir=dirname(zipfilename))
    file.remove(zipfilename)
  }	
  
  if (file.exists(ascfilename)) { 
    rs <- raster(ascfilename)
    projection(rs) <- '+proj=tmerc +lat_0=0 +lon_0=10.33333333333333 +k=1 +x_0=0 +y_0=-5000000 +ellps=bessel +towgs84=577.326,90.129,463.919,5.137,1.474,5.297,2.4232 +units=m +no_defs'
    return(rs)
  } else {
    stop('file not found')
  }
}

.harrylist <- function(download, path) {
  # use the download.file function to access online content. note we change already the filename and 
  # we also pass the .php extension of the download address
  zipfilename='bergliste-komplett.kmz'
  kmlfilename='bergliste-komplett.kml'
  
  if (!file.exists(zipfilename)) {
    if (download) { 
      theurl <-'http://www.tourenwelt.info/commons/download/bergliste-komplett.kmz.php'
      test <- try (.download(theurl) , silent=TRUE)
    } else {cat('file not available locally, use download=TRUE\n') }  
  }
  if (file.exists(zipfilename)) { 
    unzip(zipfilename,junkpath=TRUE, exdir=dirname(zipfilename))
    file.remove(zipfilename)
  }  
  
  if (file.exists(kmlfilename)) { 
    
    # convert to csv file with babel (you need to install the babel binaries on your system)
    system("gpsbabel -i kml -f bergliste-komplett.kml -o unicsv -F bergliste-komplett.csv")
    
    # read into data.frame
    df=read.csv("bergliste-komplett.csv",  header = TRUE, sep = ",", dec='.')
    
    # extract altitude out of Description column that is full of  html garbage
    altitude<-as.numeric(substring(df$Description, regexpr('H&ouml;he:</td><td>', df$Description)+19,regexpr("</td></tr>", df$Description)-1))
    
    # delete the unused cols
    df$Description <- NULL
    df$No <- NULL
    
    # and put altitude values into df
    df$Altitude<- altitude
    # making a subset of the for reaonable Lat Lon Values
    df.sub = subset(df, df$Longitude >= -180 & df$Longitude <= 180 & df$Latitude >= -90 & df$Latitude  <= 90)
    
    # first we have to assign lat lon geographic coordinates
    hblist<-SpatialPointsDataFrame(data.frame(df.sub$Longitude,df.sub$Latitude),data.frame(df.sub$Name,df.sub$Altitude), proj4string = CRS("+proj=longlat +datum=WGS84"))
    t<-as.data.frame(hblist)
    colnames(t)<-c('Lat','Lon','Name','Altitude')
    coordinates(t)<- ~Lat+Lon
    proj4string(t)<- "+proj=longlat +datum=WGS84"
    
    return(t)
  } else {
    stop('file not found')
  }
}



.OSMp <- function(extent,key,val,taglist,download, path) {
  # use the download.file function to access online content. note we change already the filename and 
  # we also pass the .php extension of the download address
  
  # define the spatial extend of the OSM data we want to retrieve
  osm.extend <- corner_bbox(extent[1],extent[3],extent[2],extent[4])
  
  # download all osm data inside this area, note we have to declare the api interface with source
  print('Retrieving OSM data. Be patient...')
  osm <- get_osm(osm.extend, source = osmsource_api())
  # find the first attribute key&val
  node.id <- find(osm, node(tags(k == key & v == val)))
  
  # find downwards (according to the osmar object level hierarchy) 
  # all other items that have the same attributes
  all.nodes <- find_down(osm, node(node.id))
  
  ### to keep it clear and light we make subsets corresponding to the identified objects of all  data
  .sub <- subset(osm, node_ids = all.nodes$node_ids)
  
  # now we need to extract the corresponding variables and values separately
  # create sub-subsets of the tags 'name' and 'ele' and attrs 'lon' , 'lat'
  
  .coords<- subset(.sub$nodes$attrs[c('id',"lon", "lat")],)
  i=1   
  for(elements in taglist){
    .tmp<- subset(.sub$nodes$tags,(k==elements ))[,-2]
    names(.tmp)[2]<-elements
    if (i==1){
      .stmp<- merge(.coords,.tmp, by="id",all.x=TRUE)
      i=i+1
    }else{
      .stmp<- merge(.stmp,.tmp, by="id",all.x=TRUE)
    }
  }
  
  # clean the df and rename the cols
  m.df<-.stmp
  
  # convert the osm.peak df to a SpatialPoints object and assign reference system
  coordinates(m.df) <- ~lon+lat
  proj4string(m.df)<-"+proj=longlat +datum=WGS84"
  # save to shapefile
  writePointsShape(m.df,"OSMNode.shp")
  
  # return Spatial Point Object projected in target projection and clipped by the DEM extent
  return(m.df)
}




.dataloc <- function() {
  d <- getOption('rasterDataDir')
  if (is.null(d) ) {
    d <- getwd()
  } else {
    d <- trim(d)
    if (d=='') {
      d <- getwd()
    }
  }
  return(d)
}  
