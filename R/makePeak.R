#'@name makePeak
#'@title Wrapper function that perform some morphometric Digital Elevation Model 
#'(DEM) analysis to generate a set of reliable peaks. Additionally external point
#' data from several sources can be used to name these unknown peaks. 
#' Currently two different approaches are available. First a simple approach using a filtered 
#' DEM that is then analysed for local maxima (SAGA's minmax module) is used. Second a more
#' sophisticaded approach that classify the landforms of a DEM is utilized and with a first
#' estimation of prominence and dominance analysis fileterd. If you choose this approach 
#' names of external data like Hattys Bergliste or the OSM data are merged to the generic
#' DEM peaks. For further interest look in the reference section.
#'

#'@usage makePeak(dem.in, peak.list,make.peak.mode,epsg,target.proj4,kernel.size=5,int=TRUE)
#'@author Chris Reudenbach 
#'@references \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#' 
#' 
#'@param dem.in DEM file in a common GDAL Format
#'@param peak.list name of the  the ASCII file containing all peak parameters
#'@param peak.mode (1) minmax: extracts local maxima altitudes from an arbitrary Digital
#'                     Elevation Model (DEM) (optionally you may filter the data) 
#'                 (2) merged approach: extract peaks from a DEM by analyzing morphometry and
#'                     landscape forms using the algorithm of Wood et others. The peak landforms are 
#'                     analyzed for MAximunm heights and this location is related 
#'                     to external peak data from Harrys peaklist or OSM
#'@param kernel.size size of filter kernel in pixel, default=3
#'@param epsg EPSG Code of the input data
#'@param target.proj4 proj.4 string for the target projection

#'@return makepeak basically returns the following parameters:
#'\tabular{ll}{
#'xcoord \tab xcoordinate (mapunits)\cr
#'ycoord \tab ycoordinate (mapunits)\cr
#'altitude \tab altitude in meter\cr
#'name \tab name of the peak\cr
#'dominance \tab name of the peak\cr
#'prominence \tab name of the peak\cr
#'EValue \tab name of the peak\cr
#'}  
#'@export makePeak
#'@examples   
#'  #### Example to makePeaks in a specified ROI and 
#'  #### create a dataframe containing coordinates, altitude and name and corresponding parameters
#'       
#' exampledata=system.file("dem.asc", package="peRfectPeak")
#' target.projection<-'+proj=tmerc +lat_0=0 +lon_0=10.33333333333333 +k=1 +x_0=0 +y_0=-5000000 +ellps=bessel +towgs84=577.326,90.129,463.919,5.137,1.474,5.297,2.4232 +units=m +no_defs'
#' peak.list=makePeak(exampledata,'peak.list'peaklist.txt',2,'31254',target.projection')
#' view(peak.list)
#'
makePeak <- function(fname,DEMfname,iniparam,myenv,int=TRUE){
 
  ### global settings
  # set environment
  # (R) define working folder 
  root.dir <- trim(iniparam$Pathes$workhome)               # project folder 
  working.dir <- trim(iniparam$Pathes$runtimedata)         # working folder 
  
  # (R) set filenames 
  peak.list<-trim(iniparam$Files$peaklist)                 # output file name of peaklist
  dem.in<-DEMfname
  if (dem.in==''){
    dem.in<-trim(iniparam$Files$fndem)}                       # input DEM (has to be GDAL conform)                       # input DEM (has to be GDAL conform)
  # (R) set runtime arguments
  ext.peak<-trim(iniparam$Params$externalpeaks)                        # harry= Harrys Peaklist osm= OSM peak data
  kernel.size<-as.numeric(trim(iniparam$Params$filterkernelsize))      # size of filter for mode=1; range 3-30; default=5 
  make.peak.mode<-trim(iniparam$Params$makepeakmode)                   #  mode:1=minmax,2=wood&Co.
  exact.enough<-as.numeric(trim(iniparam$Params$exactenough))          # vertical exactness of flooding in meter
  epsg.code<-trim(iniparam$Projection$targetepsg)                      # epsg code of the target data
  target.proj4<-trim(iniparam$Projection$targetproj4)                  # proj4 proejction string as set in ini file
  latlon.proj4<-as.character(CRS("+init=epsg:4326"))                   # latlon wgs84 proj4 string
  
  ### local settings 
  # internal SAGA name of DEM data file  
  fname='mp_dem.sdat'
  
  # (GDAL) gdalwarp is used to (1) convert the data format (2) assign the
  # projection information to the data.
  gdalwarp(dem.in, fname, overwrite=TRUE, s_srs=paste0('EPSG:',epsg.code), of='SAGA')  
  
  # (raster) read GDAL data set
  dem<- raster(fname)
  
  # we reproject it to get the geographical coordinates
  dem.latlon<-projectRaster(dem, crs=latlon.proj4, method="ngb")
  
  
  # (1=MinMax) (2=Merged Wood/peaklist, 3= not implemented)
  if (make.peak.mode==1){
    #-- option 1 SAGA  makes use of the system() function of R to run commands
    # in the shell of the used OS This is very straightforward and usually there
    # is no connection between 'outside R' and 'inside R' you just start the
    # command line commands from R insteas of using the shell
    
    # (SAGA) filter DEM
    print('Filtering the DEM - may take a while...')
    rsaga.geoprocessor('grid_filter', 0 ,env=myenv,
                       list(INPUT='mp_dem.sgrd',
                            MODE=0,
                            RADIUS=as.character(kernel.size),
                            RESULT='mp_dem.sgrd'))
    
    # (SAGA) extract local minimum and maximum coordinates and altitude values from "fil_dem"
    rsaga.geoprocessor('shapes_grid', 9 ,env=myenv,
                       list(GRID='mp_dem.sgrd',
                            MINIMA='mp_min',
                            MAXIMA='mp_max'))
    
    # (SAGA) convert shp 2 ASCII 
    rsaga.geoprocessor('io_shapes', 2 ,env=myenv, 
                       list(FIELD='Z',
                            SEPARATE=0,
                            SHAPES='mp_max.shp',
                            FILENAME='run_peak_list.txt'))
    
    ### generate peaklist from make.peak.mode=1
    # (R) read the converted max data was stored in "run_peak_list.txt" into a data frame
    df=read.csv("run_peak_list.txt",  header = FALSE, sep = "\t",dec='.')
    # (R) delete headline
    df<-df[-c(1), ]
    # (R) name the cols
    colnames(df)=c("xcoord","ycoord","altitude")
    # (R) sort by altitude
    df<-df[order(df$altitude, decreasing=TRUE),]
    # (R) add required cols
    df['dominance'] <-NA
    df['prominence'] <-NA
    df['name'] <-NA
    df['E'] <-NA
    write.table(df,peak.list,row.names=F)
  }  
  
  ### if mode = 2   
  
  else if (make.peak.mode==2){  
  
    # calculate wood's terrain indices   wood= 1=planar,2=pit,3=channel,4=pass,5=ridge,6=peak
    rsaga.geoprocessor('ta_morphometry',"Morphometric Features",env=myenv,
                       list(DEM='mp_dem.sgrd',
                            FEATURES='mp_wood.sgrd',
                            SLOPE='mp_slope.sgrd',
                            LONGC='mp_longcurv.sgrd',
                            CROSC='mp_crosscurv.sgrd',
                            MINIC='mp_mincurv.sgrd',
                            MAXIC='mp_maxcurv.sgrd',
                            SIZE=9,
                            TOL_SLOPE=15.000000,
                            TOL_CURVE=0.00001,
                            EXPONENT=0.000000,
                            ZSCALE=1.000000))
    
    # Calculate Jochen Schmidt's fuzzy landforms (https://faculty.unlv.edu/buckb/GEOL%20786%20Photos/NRCS%20data/Fuzz/felementf.aml) fuzzylandoforms are:  
    # PLAIN     , 100  # PIT       , 111  # PEAK      , 122  # RIDGE     , 120  # CHANNEL   , 101	
    # SADDLE    , 121	# BSLOPE    ,   0	# FSLOPE    ,  10	# SSLOPE    ,  20	# HOLLOW    ,   1	
    # FHOLLOW   ,  11	# SHOLLOW   ,  21	# SPUR      ,   2	# FSPUR     ,  12	# SSPUR     ,  22	
    # wood SIZE=9,TOL_SLOPE=10.000000,TOL_CURVE=0.00001,EXPONENT=0.000000,ZSCALE=1.000000 
    # fuzzy SLOPETODEG='0',T_SLOPE_MIN=0.0000001,T_SLOPE_MAX=20.000000,T_CURVE_MIN=0.00000001,T_CURVE_MAX=0.0001))
    # generates the same peaks
    rsaga.geoprocessor('ta_morphometry',"Fuzzy Landform Element Classification",env=myenv,
                       list(SLOPE='mp_slope.sgrd',
                            MINCURV='mp_mincurv.sgrd',
                            MAXCURV='mp_maxcurv.sgrd',
                            PCURV='mp_longcurv.sgrd',
                            TCURV='mp_crosscurv.sgrd',
                            FORM='mp_fuzzylandform.sgrd',
                            PEAK='mp_fuzzy_peak.sgrd',
                            SLOPETODEG='0',
                            T_SLOPE_MIN=0.0000001,
                            T_SLOPE_MAX=25.000000,
                            T_CURVE_MIN=0.00000001,
                            T_CURVE_MAX=0.001))
    
    # https://faculty.unlv.edu/buckb/GEOL%20786%20Photos/NRCS%20data/Fuzz/tophat.aml 
    # DEM input DEM'radius_hill [map units] is used to cut hills,radius_valley [map units] is used to fill valleys
    # threshold is used to identify hills/valleys
    # HILL_IDX   - grid idenitifying hills, fuzzy index [1]
    rsaga.geoprocessor('ta_morphometry',"Valley and Ridge Detection (Top Hat Approach)",env=myenv,
                       list(DEM='mp_dem.sgrd',
                            HILL_IDX='mp_tophathill.sgrd',
                            RADIUS_VALLEY=35.000000,
                            RADIUS_HILL=35.000000,
                            THRESHOLD=0.5000000,
                            METHOD=0))

    dem<-raster('mp_dem.sdat')
    crs(dem)<-target.proj4
    wood<-raster('mp_wood.sdat')
    crs(wood)<-target.proj4
    tophat<-raster('mp_tophathill.sdat')
    crs(tophat)<-target.proj4
    fuzzy.landform<-raster('mp_fuzzylandform.sdat')
    crs(fuzzy.landform)<-target.proj4
    # we just take the fuzzylandforms as an example
    # reclassify fuzzylandforms to binary peak mask
    fuzzy.peak<-reclassify(fuzzy.landform, c(0,121,0, 121.1,123,1 ))
    # mask dem with fuzzy.peak areas
    mpeak<-fuzzy.peak*dem
    # clump all fuzzy.peak areas i.e. give each distinct area a unique ID
    clump.peak<-clump(fuzzy.peak)
    # create SpatialDataPolygon from the ID Areas
    SPpoly.clump.peak <- rasterToPolygons(clump.peak, dissolve=TRUE)
    #plot(SPpoly.clump.peak, col = seq_along(SPpoly.clump.peak))
    # use extract to get the cellnumber and altitude for each unique peak area
    xy = extract(dem, SPpoly.clump.peak, cellnumbers = TRUE)
    # now we use lapply to iterate tthrough each ID
    # i is the runtime variable for each polygon 
    
    ls_spdf_max <- lapply(xy, function(i) {
      # identify position of max Altitude 
      id_max <- which.max(i[, 2])
      # get max Altitude in list (=peak altitude)
      val_max <- max(i[, 2])
      # get coordinate of this cell (=peak coordinate)
      coord <- xyFromCell(dem, i[id_max, 1])
      # put  it in a data frame
      df_coord <- data.frame(coord, val_max)
      # and make it spatial
      # set the xy coordinates
      coordinates(df_coord) <- ~x+y
      # set the projection
      proj4string(df_coord) <- target.proj4
      return(df_coord)
    })
    # merge it and stuff it in a data.frame
    df<- as.data.frame(do.call("rbind", ls_spdf_max))    
    # (R) name the cols
    colnames(df)=c("xcoord","ycoord","altitude")
    
    # (R) sort by altitude
    df<-df[order(df$altitude, decreasing=TRUE),]
    # (R) add required cols
    df['dominance'] <-NA
    df['prominence'] <-NA
    df['name'] <-NA
    df['E'] <-NA
    # first estimation run to derive values for a more sophisticed filter
    final.peak.list<-df
    # (R) calculate dominance and prominence & independence (EValue)
    for (i in 1: nrow(final.peak.list)){
      # call calculate functions and put retrieved value into the dataframe field.
      if (i>1){
        final.peak.list[i,4]<-calculateDominance(final.peak.list[i,1], final.peak.list[i,2],final.peak.list[i,3],myenv=myenv,root.dir=root.dir, working.dir=working.dir)
        final.peak.list[i,5]<-calculateProminence(final.peak.list,final.peak.list[i,1], final.peak.list[i,2],final.peak.list[i,3],exact.enough=exact.enough,myenv=myenv,root.dir=root.dir, working.dir=working.dir)
        final.peak.list[i,7]<-calculateEValue(final.peak.list[i,])
      }}
    fp<-final.peak.list
    df<-subset(fp,fp$dominance > 100 & fp$prominence > 150)
    
    SP<-df
    write.table(SP,peak.list,row.names=F)
    coordinates(SP) <- ~xcoord+ycoord
    # set the projection
    proj4string(SP) <- target.proj4
    writePointsShape(SP,"DEMpeaklist.shp")    
    
    if (ext.peak=='harry') {
      harry<-extractHarry(dem.latlon,latlon.proj4,target.proj4)
      # call distance based merging of the peaks
      df.dist<-distMergePeaks(SP,harry)
      # call distance based merging of the peaks
      df.cost<-costMergePeaks(SP,harry,dem)
    }else if (ext.peak=='osm') {
           print('not implemented external peak data input')
    }else {print('not implemented external peak data input')}
   

    # assign result to df
    df<-df.cost
    
    
  }  else {
    stop("not implemented yet")
  }
  return(df)
}
