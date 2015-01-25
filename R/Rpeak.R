#' Demo Digital Elevation Model DEM
#'
#' The example Digital Elevation Model DEM is taken from the Authority of Tirol
#' it is dervied from LIDAR data and can downloaded for Tirol. 
#' The demo data is an Arc ASCII Grid with 324 cols by 360 rows and the following extent:
#' lon_min 11.35547, lon_max 11.40009, lat_min 47.10114, lat_max 47.13512
#'
#' \itemize{
#'   \item resolution : 10 Meter,10 Meter
#'   \item datatype, 32 Bit floating point
#'   \item projection, MGI_Austria_GK_West
#'   \item EPSG-Code, 31254
#'   \item unit, Meter
#'   \item datum, D_MGI
#'   \item Copyright:   \url{https://www.tirol.gv.at/data/nutzungsbedingungen/},  \url{Creative Commons Namensnennung 3.0 Österreich Lizenz (CC BY 3.0 AT).}
#'   }
#' @source Data source: \url{https://www.tirol.gv.at/data/datenkatalog/geographie-und-planung/digitales-gelaendemodell-tirol/}
#'         
#' @docType data
#' @keywords datasets
#' @name input.DEM
#' @usage raster('stubai.asc')
#' @format Arc ASCII Grid
NULL

#' Demo Ini File
#'
#' The example control file provides all necessary settings for using the perfectPeak package
#'
#' \itemize{
#'   \item [Pathes]
#'   \item workhome='/home/creu/MOC/aGIS/Rpeak'
#'   \item rawdata='./data'
#'   \item runtimedata='run'
#'   \item src='./src'
#'}
#' \itemize{   
#'   \item [Files]
#'   \item peaklist='peaklist.txt'       output fname for ASCII peaklist
#'   \item fndem='stubai.asc'            input DEM (has to be GDAL conform)
#'}
#' \itemize{   
#'   \item [Projection]
#'   \item targetepsg='31254'             epsg code of DEM data 
#'   \item targetproj4='+proj=tmerc +lat_0=0 +lon_0=10.33333333333333 +k=1 +x_0=0 +y_0=-5000000 +ellps=bessel +towgs84=577.326,90.129,463.919,5.137,1.474,5.297,2.4232 +units=m +no_defs'  
#'   \item latlonproj4='+proj=longlat +datum=WGS84 +no_defs'
#'}
#' \itemize{
#'   \item [Params]
#'   \item makepeakmode=3        1=simple local minmax, 2=wood 3=fuzzylandforms
#'   \item filterkernelsize=9    size of filter for makepeak=1; range 3-30; default=9 
#'   \item externalpeaks='osm'   harry= Harrys Peaklist osm= OSM peak data
#'   \item mergemode=1           merging peak names from harry/OSM/user data 1=distance 2=costpath (slow)
#'   \item exactenough=5         prominnence treshold of vertical matching exactness (m)
#'   \item domthres=100          threshold of minimum dominance distance (makepeak=2&3)
#'}
#' \itemize{
#'   \item [Wood]               Wood parameters refers to SAGA
#'   \item WSIZE=11
#'   \item TOL_SLOPE=14.000000
#'   \item TOL_CURVE=0.00001
#'   \item EXPONENT=0.000000
#'   \item ZSCALE=1.000000
#'}
#' \itemize{
#'   \item [FuzzyLf]            FuzzyLandforms parameters refers to SAGA
#'   \item SLOPETODEG=0
#'   \item T_SLOPE_MIN=0.0000001
#'   \item T_SLOPE_MAX=25.000000
#'   \item T_CURVE_MIN=0.00000001
#'   \item T_CURVE_MAX=0.001
#'}
#'
#' \itemize{
#'   \item [SysPath]             SYSPAth for Linux and Windows 
#'   \item wossaga='C:/MyApps/GIS_ToGo/QGIS_portable_Chugiak_24_32bit/QGIS/apps/saga'
#'   \item wsagamodules='C:/MyApps/GIS_ToGo/QGIS_portable_Chugiak_24_32bit/QGIS/apps/saga/modules'
#'   \item wgrassgisbase='C:/MyApps/GIS_ToGo/GRASSGIS643/bin'
#'   \item lossaga='/home/creu/SAGA-2.1.0-fixed/initial/bin'
#'   \item lsagamodules='/home/creu/SAGA-2.1.0-fixed/initial/lib/saga'
#'   \item lgrassgisbase='/usr/lib/grass64'
#'   }
#'         
#' @docType data
#' @keywords datasets
#' @name input.INI
#' @usage iniparse('demo.ini')
#' @format Windows style INI file
NULL

#'@name Rpeak
#'@title Example script that can be used as a wrapper function to start the 
#'perfect peak analysis 
#'
#'@description Organises all necessary processing steps for calculating the 
#'perfect peak  parameters and generating the output. It  performs preprocessing 
#'and controls the calculations of dominance, prominence, independence (E) value 
#'for a given georeferencend Digital Elevation Model (DEM)
#'
#'You can use the function as it is or alternatively use it as skeleton control
#' script that you cab adapt to your needs.
#'
#'@seealso Rauch. C. (2012): Der perfekte Gipfel.  Panorama, 2/2012, S. 112.
#'Leonhard, W. (2012): Eigenständigkeit von Gipfeln. - 


#'
#'@usage Rpeak(fname.control,fname.DEM)


#'
#' 
#'@param fname.control name of control file containing all setting and parameters for analysis
#'@param fname.DEM name Digtial Elevation Model has to be a GDAL raster file
#'



#'@author Chris Reudenbach 
#'
#'
#'@references Marburg Open Courseware Advanced GIS: \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#'@references Rauch. C. (2012): Der perfekte Gipfel.  Panorama, 2/2012, S. 112 \url{http://www.alpenverein.de/dav-services/panorama-magazin/dominanz-prominenz-eigenstaendigkeit-eines-berges_aid_11186.html}
#'@references Leonhard, W. (2012): Eigenständigkeit von Gipfeln.\url{http://www.thehighrisepages.de/bergtouren/na_orogr.htm}
#'@return Rpeak returns the complete list as a dataframe of all parameters and results and 
#' generates some output (maps and tables)
#'
#' @seealso
#' \code{\link{initEnvironGIS}}, \code{\link{calculateDominance}}, 
#' \code{\link{calculateProminence}}, \code{\link{calculateEValue}}, 
#' \code{\link{makePeak}},
#'
#'@export Rpeak
#'@examples   
#'#### Example to use Rpeak for a common analysis run
#'
#'# You obligatory need a georeferenced DEM (GDAL format) as data input. 
#' Except the origin georefence all parameters are read from the demo.ini 
#' file. You will find some more comments in the file. NOTE the real projection of
#' the DEM has to meet the projection string in the ini file
#'
#'
#' ini.example=system.file("data","demo.ini", package="perfectPeak")
#' dem.example=system.file("data","test.asc", package="perfectPeak")
#' Rpeak(ini.example,dem.example)
#' 
#' 

Rpeak <-function(fname.control,fname.DEM){
# rename environ and runtime vars
i<-initEnvironGIS(fname.control,fname.DEM)
ini<-i$ini
myenv<-i$myenv
extent<-i$extent

### assign varnames to runtime varnames 

# set working folder 
root.dir <- ini$Pathes$workhome               # project folder 
working.dir <- ini$Pathes$runtimedata         # working folder 

# (R) set filenames 
peak.list<- ini$Files$peaklist                      # output file name of peaklist
dem.in<-    fname.DEM                                       # input DEM (has to be GDAL conform)
if (dem.in==''){
  dem.in<-  ini$Files$fndem
  fname.DEM<-dem.in}                       
# (R) set runtime arguments
ext.peak<-      ini$Params$externalpeaks              # harry= Harrys Peaklist osm= OSM peak data
kernel.size<-   ini$Params$filterkernelsize           # size of filter for mode=1; range 3-30; default=5 
make.peak.mode<-ini$Params$makepeakmode               #  mode:1=minmax,2=wood&Co.
exact.enough<-  as.numeric(ini$Params$exactenough)    # vertical exactness of flooding in meter
epsg.code<-     as.numeric(ini$Projection$targetepsg) # projection of the data as provided by the meta data  
target.proj4<-  ini$Projection$targetproj4            # corrrect string from the ini file
latlon.proj4<-  as.character(CRS("+init=epsg:4326"))        # basic latlon wgs84 proj4 string

# preprocessing of all data 
final.peak.list<-makePeak(fname.DEM=dem.in,iniparam=ini,myenv=myenv,extent=extent)

### (R) final analysis and calculatuin of dominance, prominence, and independence

# do it for each peak coordinate
for (i in 1: nrow(final.peak.list)){
  # functions retrieve the value and put it into the corresponding dataframe field.
  # i>1 because of the highest peak is the reference and can not be calculated
  if (i>1){
    final.peak.list[i,4]<-calculateDominance(final.peak.list[i,1], final.peak.list[i,2],final.peak.list[i,3],exact.enough=exact.enough,myenv=myenv,root.dir=root.dir, working.dir=working.dir)
    final.peak.list[i,5]<-calculateProminence(final.peak.list,final.peak.list[i,1], final.peak.list[i,2],final.peak.list[i,3],exact.enough=exact.enough,myenv=myenv,root.dir=root.dir, working.dir=working.dir)
    final.peak.list[i,7]<-calculateIndependence(final.peak.list[i,])
  }}

### make it a spatialObject 

# set the xy coordinates
coordinates(final.peak.list) <- ~xcoord+ycoord
# set the projection
proj4string(final.peak.list) <- target.proj4

# write it to a shape file
writePointsShape(final.peak.list,"finalpeaklist.shp")

# just plot it for your convinience
plot(final.peak.list)

# delete all runtime files with filenames starting with run_, mp_
file.remove(list.files(file.path(root.dir, working.dir), pattern =('mp_'), full.names = TRUE, ignore.case = TRUE))
file.remove(list.files(file.path(root.dir, working.dir), pattern =('run'), full.names = TRUE, ignore.case = TRUE))

print("That's it")
}
