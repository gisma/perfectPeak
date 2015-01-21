inputParam<- function(){

  # (R) define working folder 
  root.dir <- "/home/creu/MOC/aGIS" # root folder 
  working.dir <- "wise2014"         # working folder 
  
# (R) set filenames 
  peak.list<-"peaklist.txt"         # outputname of peaklist
  dem.in<-"test.asc"                # input DEM (has to be GDAL conform)
  if (dem.in==''){                  # if not provided you get an interactive choice to to so
    dem.in<-file.choose()}
  
# (R) set runtime arguments
  ext.peak<-'harry'                 # harry= Harrys Peaklist osm= OSM peak data
  run.makePeak<-TRUE                # if TRUE run makePeak
  kernel.size<-5                    # size of filter for mode=1; range 3-30; default=5 
  make.peak.mode<-2                 #  mode:1=minmax,2=wood&Co.
  exact.enough<-5                   # vertical exactness of flooding in meter
  
  ### set up of the correct projection information
  
  # To derive the correct proj.4 string for Austria MGI (EPSG:31254) is very NOT straightforward
  # due to the fact that the datum transformation from WGS84 to Bessel has to meet the data 
  # the implemented Transformation Parameters is a translation for "Hermannskogel" 
  # with +ellps=bessel +towgs84=653.0,-212.0,449.0
  # unfortunately this is not the best fitting transformation parameter set for the Tirol DEM data.
  # if you look at qgis or at spatialreference.org you will find:
  # +towgs84=577.326,90.129,463.919,5.137,1.474,5.297,2.4232 (Helmert)
  # this works fine so we have to be tricky we take the basic string as derived from the make_EPSG() function
  # this is without +towgs84 parameters and paste then the correct +towgs84 string
  
  # we may take it from the internal proj.4 EPSG function list
  # NOTE This is an example for AUSTRIA EPSG31254 for better understanding of this special case
  # read for example: https://stat.ethz.ch/pipermail/r-sig-geo/2009-July/006058.html 
  #
  epsg.code<-'31254'     # we take the projection of the data as provided by the meta data  
  helmert.towgs84    <-'+towgs84=577.326,90.129,463.919,5.137,1.474,5.297,2.4232'
  #molodensky.towgs84 <-'+towgs84=653.0,-212.0,449.0'
  # this option will generate the same bsic string but WITH the molodensky.towgs84 parameter
  # unfortunately this is not correct for the used Tirol-DEM data
  # target.proj4<-as.character(CRS(paste0("+init=epsg:",epsg.code)))
  ### WORKAROUND
  # we generate the internal proj.4 EPSG dataframe 
  df.epsg <- make_EPSG()
  # by using 'grep' and the epsg.code 
  # NOTE this provides the basic string without +towgs84 therefore we add the correct helmert.towgs84 transformation
  target.proj4<-as.character(paste(df.epsg[grep(epsg.code, df.epsg$code),3],helmert.towgs84))
  
  # we will also need the  basic latlon wgs84 proj4 string
  latlon.proj4<-as.character(CRS("+init=epsg:4326")) 
}
