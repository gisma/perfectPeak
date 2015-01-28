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
