#'@name calculateProminence
#'@title Calculates the prominence Value for a given tuple of coordinates and altitude
# as derived from DEM and provided by the peaklist
#'@description Calculates the prominence Value for a given tuple of coordinates and altitude
#' as derived from DEM d = dominance horizontal distance to the next higher altirude in the sourrounding
#'
#'@usage calculateProminence(x.coord, y.coord, altitude)
#'
#'@author Chris Reudenbach 
#'
#'@references Marburg Open Courseware Advanced GIS: \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#'@references Rauch. C. (2012): Der perfekte Gipfel.  Panorama, 2/2012, S. 112 \url{http://www.alpenverein.de/dav-services/panorama-magazin/dominanz-prominenz-eigenstaendigkeit-eines-berges_aid_11186.html}
#'@references Leonhard, W. (2012): Eigenständigkeit von Gipfeln.\url{http://www.thehighrisepages.de/bergtouren/na_orogr.htm}
#' 
#'@param peaks   list of all peaks 
#'@param xcoord  xcoordinate of current peak (mapunits)
#'@param ycoord  ycoordinate of current peak (mapunits)
#'@param altitude  altitude in meter
#'@param exact.enough = vertical excactness of notch altitude in meter
#'
#'@return calculateProminence returns the prominence value of the current peak
#'
#'@export calculateProminence
#'@examples   
#'#### Example to calculateProminence

#'       
#' calculateProminence(peaklist.tupel)

calculateProminence <- function(peaks,x.coord, y.coord, altitude,exact.enough=5,int=TRUE,myenv,root.dir, working.dir){
  
  #--- doing some prepcocessing
  
  # first deleting all files that are related to the prominence function
  # due to the SAGA behaviour that is appending instead of overwriting the .mgrd xml files
  # (R) delete temporary files
  file.remove(list.files(file.path(root.dir, working.dir), pattern =('run_pro_'), full.names = TRUE, ignore.case = TRUE))
  
  #  create shapefiles for 'current peak' and 'all peaks' and 'current_peak_poly'
  #- we need to create a mask file nodata=no peaks/255= current peak to derive a
  #  polygon file of the current_peak pixel
  # the prominence:
  # (1-2)  create point shape files from peaklist
  # (1a)   write x.coord, y.coord, altitude to an ASCII file (current peak)
  # (1b)   write x.coord, y.coord, altitude to an ASCII file (all peaks)
  # (2a)   convert ASCII current peak file to SHP file
  # (2b)   convert ASCII all peaks file to SHP file
  # (3a-c) create a 'current_peak' polygon shape file (obligatory for 'intersect' query)
  # (3a) create a raw raster with : all cells=0
  # (3b) rasterize current_peak (value=255) position into raw file
  # (3c)  write the position of the current peak into the raster with the cellvalue= 255
  
  # (1a) (R) write current peak tupel to xyz ASCII file
  write.table(list(x.coord, y.coord, altitude), file = "run.xyz",  row.names = FALSE, col.names = c('1','2','3') , dec = ".",sep ='\t')
  # (1b) (R) from filtered df peaks create all_peaks point xyz ASCII file
  write.table(peaks[-c(4:8)],file = "run_peaks.xyz", row.names = FALSE, col.names = c('1','2','3') , dec = ".",sep ='\t')
  # (2a) (RSAGA) create current_peak shape file
  rsaga.geoprocessor('io_shapes',3,env=myenv,
                     list(POINTS='run_pro_current_peak.shp',
                          HEADLINE=1,
                          FILENAME='run.xyz'))
  
  # (2b) (RSAGA) create all_peak point shapefile
  rsaga.geoprocessor('io_shapes',3,env=myenv,
                     list(POINTS='run_pro_all_peaks.shp',
                          HEADLINE=1,
                          FILENAME='run_peaks.xyz'))
  
  # (3a-c) create current_peak polygon shape file for intersecting
  # (3a) (RSAGA) create empty raster with value=0
  rsaga.grid.calculus('mp_dem.sgrd', 'run_pro_current_peak_marker',
                      ~(a*0), env=myenv)
  
  # (3b) (gdalUtils)rasterize current_peak (value=255) position into raw file
  gdal_rasterize('run_pro_current_peak.shp', 'run_pro_current_peak_marker.sdat', burn=255)
  # (3c) (RSAGA) vectorize the 'current_peak' to a polygon shape
  rsaga.geoprocessor('shapes_grid', 'Vectorising Grid Classes', env=myenv,
                     list(POLYGONS='run_pro_current_peak_poly.shp',
                          GRID='run_pro_current_peak_marker.sgrd',
                          CLASS_ALL=0,
                          CLASS_ID=255.000000,
                          SPLIT=1))
  
  
  # all files for flooding loop are prepared 
  # to accelerate the search we use a binary tree search
  # we simply take as flooding level the middle of the minimum altitude of the DEM
  # and the current_peak altitude. if connected==TRUE we have to decide if the 
  # result is exact enough i.e. if the difference between min and max is smaler than
  # a defined value. if connected==FALSE we have to lower the level
  
  # (gdalUtils) derive infos from filtered dem
  file.info<-gdalinfo('mp_dem.sdat', mm=T, approx_stats=T)
  # (R) obtain minimum flood altitude from 'fil_dem'
  min.flood.altitude<-as.numeric(substring(file.info[29], regexpr("Min/Max=", file.info[29])+8,regexpr(",", file.info[29])-1))
  # (R) set current altitude to max flood altitude
  max.flood.altitude<-floor(altitude)
  # (R) while starting current_peak is not connected
  connected<-FALSE
  
  # (R) start flooding repeat until connecetd
  while (connected == FALSE) {
    # deleting all files flooding related files to clean up
    # necessary due to the SAGA behaviour to append informtations at the .mgrd xml files
    # this slows down the process
    # (R) delete temporary files
    file.remove(list.files(file.path(root.dir, working.dir), pattern =('flood_'), full.names = TRUE, ignore.case = TRUE))
    
    # for acceleration we use a binary tree search
    # the guess is always the middle of minvalue of DEM and current peak altitude
    # to we have to decide if next step we have to lift or lower the flooding altitude
    # until we are exact.enough
    
    # The loop idea we virtually flood the DEM until we got a landbrige bewteen 
    # the "current_peak" and any other highr area. 
    # implementation: 
    # set flooding level
    new.flood.altitude<-(max.flood.altitude+min.flood.altitude)/2
    
    # (R) create formula string for mask command
    formula<-paste0('ifelse(gt(a,', new.flood.altitude ,'),1,0)')
    # (RSAGA) mask level>peak set rest to nodata
    rsaga.grid.calculus('mp_dem.sgrd', 'flood_run_level.sgrd',formula,env=myenv)
    
    # (RSAGA) make polygon shape from current floodlevel note altitude was set
    rsaga.geoprocessor('shapes_grid', 'Vectorising Grid Classes', env=myenv,
                       list(POLYGONS='flood_run_level.shp',
                            GRID='flood_run_level.sgrd',
                            CLASS_ALL=1,
                            CLASS_ID=1.000000,
                            SPLIT=1))
    
    # (RSAGA) write "marker value 255" to the flood_run_level shapefile to mark
    # the single polygon that contains the position of current_peak
    # if we dont "split the flood_run_level.shp we can not use a specified query
    rsaga.geoprocessor("shapes_grid","Grid Statistics for Polygons", env=myenv,
                       list(GRIDS="run_pro_current_peak_marker.sgrd" ,
                            POLYGONS="flood_run_level.shp",
                            MAX=T,
                            QUANTILE=0,
                            RESULT="flood_run_result.shp"))
    
    # (gdalUtils) select the current_peak polygon
    ogr2ogr('flood_run_result.shp', 'flood_run_select.shp',
            f="ESRI Shapefile",
            select='run_pro_cur', where="run_pro_cur = 255",
            overwrite=TRUE)
    
    # (RSAGA) count how much peaks are inside the selceted polygon item
    rsaga.geoprocessor('shapes_points',1, env=myenv,
                       list(POINTS="run_pro_all_peaks.shp",
                            POLYGONS="flood_run_result.shp"))
    
    # (RSAGA) convert this to a ASCII csv file
    rsaga.geoprocessor('io_shapes', 2 ,env=myenv,
                       list(POINTS='flood_run_result.shp',
                            FIELD=TRUE,
                            HEADER=TRUE,
                            SEPARATE=0,
                            FILENAME='flood_run_result.txt'))
    
    # (R) read it into data frame
    result=read.csv(file = 'flood_run_result.txt', header=T, sep="\t",dec='.')
    
    # (R) check if the table has correct dimensions
    #if (ncol(result)!=7) {stop('no results during selection of the peak polygon -> have to stop')}
    
    # (R) name the cols
    colnames(result)=c("c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12","c13","c14" )
    
    # (R) filter if c6=255 and c7 > 1 (= peak_polygon contains more than one peak => landbridge is closed)
    if (nrow(subset(result,result$c8 == 255 & result$c9 > 1)) > 0){
      # landbrige is closed but maybe in avery coarse way so check if the difference is small enough default=5
      if((max.flood.altitude-min.flood.altitude) < exact.enough){
        # closed landbrige is found
        connected<- TRUE
      }else{
        # if are connected but we flooded to deep (i.e. > exact enough) we rise flooding level half the way up
        min.flood.altitude<- new.flood.altitude}
    }else{
      # if we are not conneced we will lower the flooding level half the way down
      max.flood.altitude<- new.flood.altitude
    }
  }
  ## we just created the mask to derive the notch value lets get this value
  # (RSAGA) create raster with value=0
  rsaga.grid.calculus(c('mp_dem.sgrd;flood_run_level.sgrd'), 'run_level_raw.sgrd', ~(a*b), env=myenv)
  
  # (RSAGA) forces nodata reclass to derive true minimum
  rsaga.grid.calculus('run_level_raw.sgrd', 'run_level.sgrd','ifelse(eq(a,0),-99999,a)',env=myenv)
  
  # (R) dirty but have to to this if you open run_level.sgrd in qgis this file
  # is created and shifts the index value of gdalinfo
  file.remove(list.files(file.path(root.dir, working.dir), pattern =('.aux.xml'), full.names = TRUE, ignore.case = TRUE))
  
  #- min calculation
  # (gdalUtils) extract info -mm calculates the min max info -approx_stats force
  file.info<-gdalinfo('run_level.sdat', mm=T, approx_stats=T)
  
  # (R) get the prominence (min)value
  notch<-as.numeric(substring(file.info[29], regexpr("Min/Max=", file.info[29])+8,regexpr(",", file.info[29])-1))
  
  # (R) calculate the prominence
  prominence<- ceiling(altitude)-notch
  return (prominence)
}
