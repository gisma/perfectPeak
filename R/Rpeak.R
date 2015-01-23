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
#'@usage Rpeak(INI.file,DEMfname)


#'
#' 
#'@param INI ini file containing all setting and parameters for analysis
#'@param DEMfname Digtial Elevation Model has to be a GDAL raster file
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
#' ini.example=system.file("demo.ini", package="perfectPeak")
#' dem.example=system.file("test.asc", package="perfectPeak")
#' Rpeak(ini.example,dem.example)
#' 
#' 

Rpeak <-function(fname,DEMfname){
# rename environ and runtime vars
i<-initEnvironGIS(fname,DEMfname)
ini<-i$ini
myenv<-i$myenv

### assign varnames to runtime varnames 

# set working folder 
root.dir <- trim(ini$Pathes$workhome)               # project folder 
working.dir <- trim(ini$Pathes$runtimedata)         # working folder 

# (R) set filenames 
peak.list<- trim(ini$Files$peaklist)                       # output file name of peaklist
dem.in<-    DEMfname                                       # input DEM (has to be GDAL conform)
if (dem.in==''){
  dem.in<-  trim(ini$Files$fndem)}                       
# (R) set runtime arguments
ext.peak<-      trim(ini$Params$externalpeaks)              # harry= Harrys Peaklist osm= OSM peak data
kernel.size<-   trim(ini$Params$filterkernelsize)           # size of filter for mode=1; range 3-30; default=5 
make.peak.mode<-trim(ini$Params$makepeakmode)               #  mode:1=minmax,2=wood&Co.
exact.enough<-  as.numeric(trim(ini$Params$exactenough))    # vertical exactness of flooding in meter
epsg.code<-     as.numeric(trim(ini$Projection$targetepsg)) # projection of the data as provided by the meta data  
target.proj4<-  trim(ini$Projection$targetproj4)            # corrrect string from the ini file
latlon.proj4<-  as.character(CRS("+init=epsg:4326"))        # basic latlon wgs84 proj4 string

# preprocessing of all data 
final.peak.list<-makePeak(DEMfname=dem.in,iniparam=ini,myenv=myenv)

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
