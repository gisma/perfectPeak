#' perfectPeak
#'
#' A collection of functions to facilitate the processing steps for calculating the 
#' "perfect peak"  parameters and generating the output. There are functions for 
#' preprocessing and retrieving the values of dominance, prominence, independence  
#' from a given georeferencend Digital Elevation Model (DEM).
#'
#' @name perfectPeak
#' @docType package
#' @title perfectPeak
#' @author Chris Reudenbach\cr
#' \cr
#' \emph{Maintainer:} Chris Reudenbach \email{giswerk@@gis-ma.org}
#'
#' @keywords package
#'@references
#' Marburg Open Courseware Advanced GIS: \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}\cr
#' Rauch. C. (2012): Der perfekte Gipfel.  Panorama, 2/2012, S. 112 \url{http://www.alpenverein.de/dav-services/panorama-magazin/dominanz-prominenz-eigenstaendigkeit-eines-berges_aid_11186.html}\cr
#' Leonhard, W. (2012): Eigenständigkeit von Gipfeln.\url{http://www.thehighrisepages.de/bergtouren/na_orogr.htm}\cr
#'
#' @seealso \pkg{perfectPeak} is using heavily SAGA and GRASS GIS via the \code{\link{RSAGA-package}} and 
#'  \code{\link{spgrass6-package}}.
#' Please see their documentation for special questions dealing with the algorithms and parameters etc.
#' @import downloader maptools osmar rgeos gdata Matrix igraph rgdal gdistance  gdalUtils spgrass6 RSAGA sp raster
#'
NULL
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
#' @name DEM
#' @usage raster('stubai.asc')
#' @format Arc ASCII Grid
NULL

#' Demo Ini File
#'
#'The example control file provides all necessary settings for using the perfectPeak package
#'\tabular{ll}{
#'[Pathes]\tab\cr
#'workhome='/home/MOC/aGIS/Rpeak' \tab project root directory\cr
#'rawdata='./data' \tab data directrory relative to 'workhome'\cr
#'runtimedata='run' \tab runtime directory relative to 'workhome'\cr
#'src='./src' \tab script directory relative to 'workhome'\cr
#'}
#'\tabular{ll}{
#'[Files]\tab\cr
#'peaklist='peaklist.txt' \tab       output fname for ASCII peaklist\cr
#'fndem='stubai.asc' \tab            input \link{DEM} (has to be GDAL conform)\cr
#'}
#'\tabular{ll}{
#'[Projection] \tab\cr
#'targetepsg='31254' \tab EPSG code for Austian DEM data NOTE you have to use the targetproj4 string for all projections  \cr
#'targetproj4='+proj=tmerc +lat_0=0 +lon_0=10.33333333333333 +k=1 +x_0=0 +y_0=-5000000 +ellps=bessel +towgs84=577.326,90.129,463.919,5.137,1.474,5.297,2.4232 +units=m +no_defs' \tab special proj4 string for Austrian DEM data set\cr
#'latlonproj4='+proj=longlat +datum=WGS84 +no_defs' \tab common geographic coordinates =EPSG:4326 \cr
#'}
#'\tabular{ll}{
#'[Params]\tab\cr
#'makepeakmode=3 \tab 1=simple local minmax, 2=wood 3=fuzzylandforms\cr
#'filterkernelsize=9 \tab    size of filter for makepeak=1; range 3-30; default=9 \cr
#'externalpeaks='osm' \tab   'harry'= Harrys Peaklist \link{PeakList} osm= OSM peak data\cr
#'mergemode=1 \tab           merging peak names from harry/OSM/user data 1=distance (default) 2=costpath (slow)\cr
#'exactenough=5 \tab         treshold of vertical matching exactness (meter) during prominence analysis\cr
#'domthres=100 \tab          threshold of minimum dominance distance (makepeak=2&3)\cr
#'}
#'[Wood]                Wood parameters refers to SAGA\cr
#'WSIZE=11\cr
#'TOL_SLOPE=14.000000\cr
#'TOL_CURVE=0.00001\cr
#'EXPONENT=0.000000\cr
#'ZSCALE=1.000000\cr
#'\cr
#'[FuzzyLf]             FuzzyLandforms parameters refers to SAGA\cr
#'SLOPETODEG=0\cr
#'T_SLOPE_MIN=0.0000001\cr
#'T_SLOPE_MAX=25.000000\cr
#'T_CURVE_MIN=0.00000001\cr
#'T_CURVE_MAX=0.001\cr
#'\cr
#'SYSPath for Linux/Windows are correct according to \url{http://giswerk.org/doku.php?id=projekte:gis-software:gis-distros}\cr
#'wossaga='C:/MyApps/GIS_ToGo/QGIS_portable_Chugiak_24_32bit/QGIS/apps/saga'\cr
#'[SysPath]            
#'wsagamodules='C:/MyApps/GIS_ToGo/QGIS_portable_Chugiak_24_32bit/QGIS/apps/saga/modules'\cr
#'wgrassgisbase='C:/MyApps/GIS_ToGo/GRASSGIS643/bin'\cr
#'lossaga='/home/creu/SAGA-2.1.0-fixed/initial/bin'\cr
#'lsagamodules='/home/creu/SAGA-2.1.0-fixed/initial/lib/saga'\cr
#'lgrassgisbase='/usr/lib/grass64'\cr
#'
#'         
#' @docType data
#' @keywords datasets
#' @name INI
#' @usage iniparse('demo.ini')
#' @format Windows style INI file
NULL

#' Harry's Peak List
#'
#'Harry's mountain list is a collection of peaks in the world. In the database are 58532 entries at the moment (15.1.2014). This is equivalent to 52218 summits (6314 aliases) in different mountain ranges all over the world. 
#'
#'EPSG-Code: 4326
#'
#'Copyright:   \url{http://www.tourenwelt.info/impressum.php}
#'  
#' @source Data source: \url{http://www.tourenwelt.info/bergliste/bergliste.php}
#'         
#' @docType data
#' @keywords datasets
#' @name PeakList
#' @usage getGeoData('harrylist')
#' @format KMZ File
NULL
