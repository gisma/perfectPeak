#'@name initEnvironGIS
#'
#'@title Function that initializes RSAGA, GRASS GDAL and the R packages 
#'
#'@description Function that initializes environment pathes, SAGA, GRASS and GDAL support (and the bindings to the corresponding R packages and the R packages *NOTE* you probably have to customize some settings ini file
#' 
#'@usage initEnvironGIS(ini.file)
#'
#'@param ini.file name of the session ini.file
#'@param DEMfname name of used raster DEM (GDAL format)
#'
#'@author Chris Reudenbach 
#'
#'
#'@return initEnvironGIS initializes the usage of the GIS packages and other utilities
#'@export initEnvironGIS
#'
#'@examples   
#'#### Example to initialize the enviroment and GIS bindings for use with R
#'#### uses the ini list from an ini file
#'       
#' ini.example=system.file("demo.ini", package="perfectPeak")
#' dem.example=system.file("test.asc", package="perfectPeak")
#' initEnvironGIS(ini.example,dem.example)
#' gmeta6()
#' 

initEnvironGIS <- function(fname,DEMfname){
  
  # check for packages and if necessary install libs 
  libraries<-c("downloader","sp","raster","maptools",
               "RSAGA","rgeos","gdata","Matrix","igraph",
               "rgdal","gdistance", "spgrass6", "gdalUtils")
  
  
  # Install CRAN packages (if not already installed)
  inst <- libraries %in% installed.packages()
  if(length(libraries[!inst]) > 0) install.packages(libraries[!inst])
  
  # Load packages into session 
  lapply(libraries, require, character.only=TRUE)
  
  # get environment
  ini<-iniParse(fname)  
  # (R) define working folder 
  root.dir <- trim(ini$Pathes$workhome)               # project folder 
  working.dir <- trim(ini$Pathes$runtimedata)         # working folder 
  
  ### set up of the correct projection information
  # To derive the correct proj.4 string for Austria MGI (EPSG:31254) is very NOT straightforward
  # due to the fact that the datum transformation from WGS84 to Bessel has to meet the data 
  # the implemented Transformation Parameters is a translation for "Hermannskogel" 
  # that means  '+ellps=bessel +towgs84=653.0,-212.0,449.0'
  #
  # Unfortunately this is not the best fitting transformation parameter set for the Tirol DEM data.
  # if you look at qgis or at spatialreference.org you will find that
  # +towgs84=577.326,90.129,463.919,5.137,1.474,5.297,2.4232 (Helmert) works best
  #
  # So we have to workaround. We take the basic string as derived from the make_EPSG() function
  # this is without +towgs84 parameters (EPSG Code rarely provides the +towgs84 parameters due to authorithy 'problems') 
  # and paste then the correct +towgs84 string
  # we can take it from the internal proj.4 EPSG function list using make_EPSG()
  # NOTE This is an example for AUSTRIA EPSG31254 
  # bessel.helmert.towgs84    <-'+towgs84=577.326,90.129,463.919,5.137,1.474,5.297,2.4232'
  # bessel.molodensky.towgs84 <-'+towgs84=653.0,-212.0,449.0'
  ## this option will generate the same basic string but WITH the bessel.molodensky.towgs84 parameter set
  ## unfortunately this is not correct for the used Tirol-DEM data
  # target.proj4<-as.character(CRS(paste0("+init=epsg:",epsg.code)))
  ### WORKAROUND
  # we generate the internal proj.4 EPSG dataframe 
  ##df.epsg <- make_EPSG()
  # by using 'grep' and the epsg.code 
  # NOTE this provides the basic string without +towgs84 therefore we add the correct helmert.towgs84 transformation
  ##target.proj4<-as.character(paste(df.epsg[grep(epsg.code, df.epsg$code),3],helmert.towgs84))
  #### for better understanding of this special case
  # you may want to dive in the wild field of confusion corresponding to projection issues starting here:
  # https://stat.ethz.ch/pipermail/r-sig-geo/2009-July/006058.html 
  
  # projection of the data as provided by the meta data  
  epsg.code<-trim(ini$Projection$targetepsg)
  
  # we take the corrrect string from the ini file
  target.proj4<-trim(ini$Projection$targetproj4)
  
  # we will also need the  basic latlon wgs84 proj4 string
  latlon.proj4<-as.character(CRS("+init=epsg:4326")) 
  
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
    os.saga.path<-trim(ini$SysPath$wossaga)
    saga.modules<-trim(ini$SysPath$wsagamodules)
    grass.gis.base<-trim(ini$SysPath$wgrassgisbase)
  }else if (Sys.info()["sysname"] == "Linux"){
    os.saga.path<-trim(ini$SysPath$lossaga)
    saga.modules<-trim(ini$SysPath$lsagamodules)
    grass.gis.base<-trim(ini$SysPath$lgrassgisbase)
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
  execGRASS('r.in.gdal',  flags=c('o',"overwrite"), input=DEMfname,  output='rastgrass', band=1)
  # (2) use the derived informations to complete the location settings
  
  execGRASS('g.region',rast="rastgrass")
  execGRASS('g.proj', flags=c('c') ,  epsg=grass.epsg.code)
  
  # provide myenv and parameterlist for common use
  result=list(ini,myenv)
  names(result)=c('ini','myenv')
  return (result)  
}


