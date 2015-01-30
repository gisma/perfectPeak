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
#'@usage Rpeak(fname.control,fname.DEM)
#' 
#'@param fname.control name of control file containing all setting and parameters for analysis
#'@param fname.DEM name Digtial Elevation Model has to be a GDAL raster file
#'
#'@author Chris Reudenbach 
#'
#'@references
#' Marburg Open Courseware Advanced GIS: \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}\cr
#' Rauch. C. (2012): Der perfekte Gipfel.  Panorama, 2/2012, S. 112 \url{http://www.alpenverein.de/dav-services/panorama-magazin/dominanz-prominenz-eigenstaendigkeit-eines-berges_aid_11186.html}\cr
#' Leonhard, W. (2012): Eigenständigkeit von Gipfeln.\url{http://www.thehighrisepages.de/bergtouren/na_orogr.htm}\cr
#' 
#'@return peaklist with ycoord ycoord,altitude,dominance,prominence,independence name 
#' generates some output (maps and tables)
#'
#'@seealso
#' \code{\link{initEnvironGIS}}, \code{\link{calculateDominance}}, 
#' \code{\link{calculateProminence}}, \code{\link{calculateEValue}}, 
#' \code{\link{makePeak}},
#'
#'@export Rpeak
#'@examples   
#'#### Example to use Rpeak for a common analysis of the 
#'     dominance, prominenece and independence values of an specifified area
#'
#' # You need a georeferenced DEM (GDAL format) as data input. 
#' # All parameters are read from the control INI file - use the 'demo.ini' as a template  
#' # NOTE the existing projection of the data file has to be exactly the same 
#' # as provided in target.proj4  variable in the ini file
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
root.dir <- ini$Pathes$workhome                       # project folder 
working.dir <- ini$Pathes$runtimedata                 # working folder 

# (R) set filenames 
peak.list<- ini$Files$peaklist                        # output file name of peaklist
dem.in<-    fname.DEM                                 # input DEM (has to be GDAL conform)
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
latlon.proj4<-  ini$Projection$latlonproj4            # basic latlon wgs84 proj4 string

# analysis of peaks and preprocessing of all data 
final.peak.list<-makePeak(fname.DEM=dem.in,iniparam=ini,myenv=myenv,extent=extent)

### final analysis and calculatuin of dominance, prominence, and independence
# the called functions retrieve the corresponding values and put it into the corresponding dataframe field.
# do it for each peak 
for (i in 1: nrow(final.peak.list)){
  # we want to know at least the prominence of the highest peak
  final.peak.list[i,6]<-calculateProminence(final.peak.list,final.peak.list[i,1], final.peak.list[i,2],final.peak.list[i,3],exact.enough=exact.enough,myenv=myenv,root.dir=root.dir, working.dir=working.dir)
  final.peak.list[i,7]<-9.999
  # i>1 because the highest peak is relative reference point so dominance and independence is not computable 
  if (i>1){
    final.peak.list[i,5]<-calculateDominance(final.peak.list[i,1], final.peak.list[i,2],final.peak.list[i,3],exact.enough=exact.enough,myenv=myenv,root.dir=root.dir, working.dir=working.dir)
    final.peak.list[i,7]<-calculateIndependence(final.peak.list[i,])
  }}

### make it a spatialObject 
# set the xy coordinates
coordinates(final.peak.list) <- ~xcoord+ycoord
# set the projection
proj4string(final.peak.list) <- target.proj4

### to have somthing as a result
# write it to a shape file
writePointsShape(final.peak.list,"finalpeaklist.shp")

# visualize it for your convinience
plot(final.peak.list)

# delete all runtime files with filenames starting with run_, mp_
file.remove(list.files(file.path(root.dir, working.dir), pattern =('mp_'), full.names = TRUE, ignore.case = TRUE))
file.remove(list.files(file.path(root.dir, working.dir), pattern =('run'), full.names = TRUE, ignore.case = TRUE))

print("That's it")
return(final.peak.list)
}
