#'@name Rpeak
#'@title Wrapper function that organises all necessary processing steps for calculating the perfect peak  parameters and generating the output
#'@description Preprocessing and controlling of the calculations of dominance, prominence, Eigensteandigkeit value for a given DEM
#' Sources:    Rauch. C. (2012): Der perfekte Gipfel.  Panorama, 2/2012, S. 112 
#'             http://www.alpenverein.de/dav-services/panorama-magazin/dominanz-prominenz-eigenstaendigkeit-eines-berges_aid_11186.html#             (Zugriff: 20.05.2012)
#'             Leonhard, W. (2012): Eigenständigkeit von Gipfeln. - 
#'
#'@usage Rpeak(ini.file)
#'@author Chris Reudenbach 
#'
#'@source 
#'\tabular{ll}{
#'Package: \tab Rpeak\cr
#'Type: \tab Package\cr
#'Version: \tab 0.2\cr
#'License: \tab GPL (>= 2)\cr
#'LazyLoad: \tab yes\cr
#'}
#'
#'@references \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#' 
#'@param fname file storing the setup parameters as ini file
#'@param DEMfname Digtial Elevation Model has to be a GDAL raster file
#'
#'@return Rpeak returns the complete list of all parameters and results and generates some output (maps and tables)
#'
#'@export Rpeak
#'@examples   
#'#### Example to Rpeak
#'
#'Rpeak provides an example script that calculates the prominence, dominance and independence value according to the paramaters that are set up in the ini file.
#'It needs a DEM as obligatory input, Additionally it can use the OSM peak data base and/or  Harrys Bergliste for naming and selcecting of the peaks.
#' For more comprehensive information look at: http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description
#'
#'ini.example=system.file("demo.ini", package="Rpeak")
#'dem.example=system.file("test.asc", package="Rpeak")
#'Rpeak(ini.example,dem.example)



Rpeak <-function(fname,DEMfname){
# (R) call MakePeak if necessary
ini<-initEnvironGIS(fname,DEMfname)
iniparam<-ini$iniparam
myenv<-ini$myenv
# (R) define working folder 
root.dir <- trim(iniparam$Pathes$workhome)               # project folder 
working.dir <- trim(iniparam$Pathes$runtimedata)         # working folder 
# (R) set filenames 
peak.list<-trim(iniparam$Files$peaklist)                 # output file name of peaklist
dem.in<-DEMfname
if (dem.in==''){
  dem.in<-trim(iniparam$Files$fndem)}                       # input DEM (has to be GDAL conform)
# (R) set runtime arguments
ext.peak<-trim(iniparam$Params$externalpeaks)            # harry= Harrys Peaklist osm= OSM peak data
kernel.size<-trim(iniparam$Params$filterkernelsize)      # size of filter for mode=1; range 3-30; default=5 
make.peak.mode<-trim(iniparam$Params$makepeakmode)       #  mode:1=minmax,2=wood&Co.
exact.enough<-trim(iniparam$Params$exactenough)          # vertical exactness of flooding in meter
epsg.code<-trim(iniparam$Projection$targetepsg)          # projection of the data as provided by the meta data  
target.proj4<-trim(iniparam$Projection$targetproj4)      # corrrect string from the ini file
latlon.proj4<-as.character(CRS("+init=epsg:4326"))       # basic latlon wgs84 proj4 string

# preprocessing of all data
final.peak.list<-makePeak(fname,DEMfname,iniparam=iniparam,myenv=myenv)

# (R) calculate dominance and prominence
for (i in 1: nrow(final.peak.list)){
  # call calculate functions and put retrieved value into the dataframe field.
  if (i>1){
    final.peak.list[i,4]<-calculateDominance(final.peak.list[i,1], final.peak.list[i,2],final.peak.list[i,3],myenv=myenv,root.dir=root.dir, working.dir=working.dir)
    final.peak.list[i,5]<-calculateProminence(final.peak.list,final.peak.list[i,1], final.peak.list[i,2],final.peak.list[i,3],exact.enough,myenv=myenv,root.dir=root.dir, working.dir=working.dir)
    final.peak.list[i,7]<-calculateEValue(final.peak.list[i,])
  }}






# write result to peaklist.txt
write.table(final.peak.list,'peaklist.txt',row.names=F)
# set the xy coordinates
coordinates(final.peak.list) <- ~xcoord+ycoord
# set the projection
proj4string(final.peak.list) <- target.proj4
writePointsShape(final.peak.list,"finalpeaklist.shp")
plot(final.peak.list)
# (R) delete all runtime files with filenames starting with run_
file.remove(list.files(file.path(root.dir, working.dir), pattern =('mp_'), full.names = TRUE, ignore.case = TRUE))
file.remove(list.files(file.path(root.dir, working.dir), pattern =('run'), full.names = TRUE, ignore.case = TRUE))

}
