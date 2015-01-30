#'@name initEnvironGIS
#'
#'@title Function that initializes RSAGA, GRASS GDAL and the R packages 
#'
#'@description Function that initializes environment pathes, SAGA, GRASS and GDAL support (and the bindings to the corresponding R packages and the R packages *NOTE* you probably have to customize some settings ini file
#' 
#'@usage initEnvironGIS(fname.control)
#'
#'@param fname.control name of the session ini.file
#'@param fname.DEM name of used raster DEM (GDAL format)
#'
#'@author Chris Reudenbach 
#'
#'@details For Further Information look at:\link{INI}
#'
#'@return initEnvironGIS initializes the usage of the GIS packages and other utilities
#'@export initEnvironGIS
#'
#'@examples   
#'#### Example to initialize the enviroment and GIS bindings for use with R
#'#### uses the ini list from an ini file
#'       
#' ini.example=system.file("data","demo.ini", package="perfectPeak")
#' dem.example=system.file("data"," test.asc", package="perfectPeak")
#' initEnvironGIS(ini.example,dem.example)
#' gmeta6()
#' 

initEnvironGIS <- function(fname.control,fname.DEM){
  
  # check for packages and if necessary install libs 
  libraries<-c("downloader","sp","maptools","osmar",
               "RSAGA","rgeos","gdata","Matrix","igraph",
               "rgdal","gdistance","OpenStreetMap","spgrass6", 
               "gdalUtils","raster","plotKML","maps","ggplot2",
               "webmaps","leafletR","magrittr")
  
  # Install CRAN packages (if not already installed)
  inst <- libraries %in% installed.packages()
  if(length(libraries[!inst]) > 0) install.packages(libraries[!inst])
  
  # Load packages into session 
  lapply(libraries, require, character.only=TRUE)
  
  # get environment
  ini<-iniParse(fname.control)  
  
  # (R) assign local vars for working folder 
  root.dir <- ini$Pathes$workhome               # project folder 
  working.dir <- ini$Pathes$runtimedata         # working folder 
  
  ### assign correct projection information
  # To derive the correct proj.4 string for Austria MGI (EPSG:31254) is very NOT straightforward
  # please refer to: http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:code-examples:ag-ce-09-01
  # taget EPSG code
  epsg.code<-ini$Projection$targetepsg
  
  # target projection (actually the projection of the DEM)
  target.proj4<-ini$Projection$targetproj4
  # we will also need the  basic latlon wgs84 proj4 string
  latlon.proj4<-ini$Projection$latlonproj4
  
  # (raster) read GDAL data set
  dem<- raster(fname.DEM)
  projection(dem)<-target.proj4
  # we reproject it to get the geographical coordinates
  dem.latlon<-projectRaster(dem, crs=latlon.proj4, method="ngb")
  # we put it in the variable extent and name cols and rows
  extent<-data.frame(cbind(dem.latlon@extent@xmin,dem.latlon@extent@xmax, dem.latlon@extent@ymin, dem.latlon@extent@ymax))
  colnames(extent)<-c('xmin','xmax','ymin', 'ymax')
  rownames(extent)<-c(basename(fname.DEM))
  
  
  ### now starting the setup of the packages bindings
  
  ## (gdalUtils) check for a valid GDAL binary installation on your system
  gdal_setInstallation()
  valid.install<-!is.null(getOption("gdalUtils_gdalPath"))
  if (!valid.install){stop('no valid GDAL/OGR found')} else{print('gdalUtils status is ok')}
  
  #--- set environment variables for RSAGA and GRASS
  # (spgrass6) define GRASS variables
  grass.epsg.code<- as.numeric(epsg.code)  # grass needs the projection numeric
  grass.loc<- paste0('loc',epsg.code)      # define corresponding folder name
  grass.mapset<- 'PERMANENT'                               # NOTE PERMANENT" is the default one and it has to be in upper cases
  
  
  # (R) set pathes  of SAGA/GRASS modules and binaries depending on OS
  if(Sys.info()["sysname"] == "Windows"){
    os.saga.path<-ini$SysPath$wossaga
    saga.modules<-ini$SysPath$wsagamodules
    grass.gis.base<-ini$SysPath$wgrassgisbase
  }else if (Sys.info()["sysname"] == "Linux"){
    os.saga.path<-ini$SysPath$lossaga
    saga.modules<-ini$SysPath$lsagamodules
    grass.gis.base<-ini$SysPath$lgrassgisbase
  }
  if (!file.exists(file.path(root.dir, working.dir))){
    dir.create(file.path(root.dir, 'run'),recursive = TRUE)
    dir.create(file.path(root.dir, 'run',grass.loc),recursive = TRUE)
    dir.create(file.path(root.dir, 'run',grass.loc,grass.mapset),recursive = TRUE)
    
  }  
  # (R) set R working directory
  setwd(file.path(root.dir, working.dir))
  getwd()
  # (RSAGA) set SAGA environment 
  myenv=rsaga.env(check.libpath=FALSE,
                  check.SAGA=FALSE,
                  workspace=file.path(root.dir, working.dir),
                  os.default.path=os.saga.path,
                  modules=saga.modules)
  
  # Create a new grass location with epsg from the data import dataset as a
  # subfolder of the working folder
  initGRASS(gisBase=grass.gis.base,  home=tempdir(),
            gisDbase=file.path(root.dir, working.dir),
            location=grass.loc,
            override=TRUE,
            ifelse(Sys.info()["sysname"] == "Windows", use_g.dirseps.exe=FALSE, use_g.dirseps.exe=TRUE))
  
  execGRASS("g.gisenv", parameters=list(set=paste("'MAPSET=", grass.mapset,"'", sep="")))
  
  # get extent BE CAREFULL this works only if the INPUT ASCII FILE CONTAINS TRULY THIS Projection
  # (1) import data in GRASS
  execGRASS('r.in.gdal',  flags=c('o',"overwrite"), input=fname.DEM,  output='rastgrass', band=1)
  # (2) use the derived informations to complete the location settings
  
  execGRASS('g.region',rast="rastgrass")
  execGRASS('g.proj', flags=c('c') ,  epsg=grass.epsg.code)
  
  # provide myenv and parameterlist for common use
  result=list(ini,myenv,extent)
  names(result)=c('ini','myenv','extent')
  return (result)  
}
