#'@name makePeak
#'@title Wrapper function that perform some morphometric Digital Elevation Model 
#'(DEM) analysis to generate a set of morphometric and in realreliable peaks 
#'
#'@description 
#' Currently two different approaches are available. First a simple approach using a filtered 
#' DEM that is then analysed for local maxima (SAGA's minmax module) is used. Second a more
#' sophisticaded approach that classify the landforms of a DEM is utilized and with a first
#' estimation of prominence and dominance analysis fileterd. If you choose this approach 
#' names of external data like Hattys Bergliste or the OSM data are merged to the generic
#' DEM peaks. Additionally external point data from several sources can be used to 
#' name these unknown peaks. For further information look in the reference section and the INI file.
#'@details
#' coming soon
#' 
#'@usage makePeak(fname.DEM,iniparam,myenv)
#'@author Chris Reudenbach 
#'
#'@references Marburg Open Courseware Advanced GIS: \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#'@references Rasemann, S., (2004), Geomorphometrische Struktur eines mesoskaligen alpinen Geosystems, Asgard-Verlag, St. Augustin, Bonner Geographische Abhandlungen, Heft Nr. 111, URL: \url{http://hss.ulb.uni-bonn.de/2003/0211/0211.htm}
#'@references Wood, J.D., (1996), The geomorphological characterisation of digital elevation models. PhD Thesis, University of Leicester, UK 
#'@references Schmidt, J, & A. Hewitt, (2004). Fuzzy land element classification from DTMs based on geometry and terrain position. Geoderma 121.3, S.243-256. URL: \url{http://home.shirazu.ac.ir/~kompani/geomorphology/geomorphology-lec-papers-mehr88/schmidt-fuzzylandsurfaceclassifi-geoderma2004.pdf}
#'@references Breitkreutz, H.: Gipfelliste. URL: \url{http://www.tourenwelt.info/commons/download/bergliste-komplett.kmz.php}
#'@references OSM natural: keys \url{http://wiki.openstreetmap.org/wiki/Key:natural}


#' 
#' 
#'@param iniparam the full set of params as provided initEnvironGIS(). please look into the INI file for further settings of the makePEak() mode and special settings. 
#'peak.mode
#'
#'(1) minmax: extracts local maxima altitudes from an arbitrary Digital
#'                     Elevation Model (DEM) (optionally you may filter the data)
#'
#'(2) merged approach: extract peaks from a DEM by analyzing morphometry and
#'                     landscape forms using the algorithm of Wood et others. The peak landforms are 
#'                     analyzed for MAximunm heights and this location is related 
#'                     to external peak data from Harrys peaklist or OSM
#'@param myenv SAGA environment variables provided by initEnvironGIS()
#'@param fname.DEM name of georeferenced DEM data in GDAL format




#'@return makepeak basically returns a list of coordinates altitudes (and names) 
#'that will be used to calculate the independence value.
#'
#' 
#'
#'@export makePeak
#'@examples   
#'  #### Example to makePeaks in a specified ROI and 
#'  #### create a dataframe containing coordinates, altitude and name and corresponding parameters
#' # assign file names
#' ini.demo=system.file("data","demo.ini", package="perfectPeak")
#' dem.demo=system.file("data","test.asc", package="perfectPeak")
#'
#' # get ini params and myenv
#' tmp<-initEnvironGIS(ini.demo,dem.edemo)
#' ini<-tmp$ini
#' myenv<-tmp$myenv
#' #define traget projection
#' target.projection<-'+proj=tmerc +lat_0=0 +lon_0=10.33333333333333 +k=1 +x_0=0 +y_0=-5000000 +ellps=bessel +towgs84=577.326,90.129,463.919,5.137,1.474,5.297,2.4232 +units=m +no_defs'
#' peak.list=makePeak(fname.DEM=dem.demo,iniparam=ini,myenv=myenv)
#' peak.list
#'

makePeak <- function(fname.DEM,iniparam,myenv,extent,int=TRUE){
  
  ### global settings
  # set environment
  # (R) define working folder 
  root.dir <- iniparam$Pathes$workhome               # project folder 
  working.dir <- iniparam$Pathes$runtimedata         # working folder 
  
  # (R) set filenames 
  peak.list<-iniparam$Files$peaklist                 # output file name of peaklist
  dem.in<-fname.DEM
  if (dem.in==''){
    dem.in<-iniparam$Files$fndem}                       # input DEM (has to be GDAL conform)                       # input DEM (has to be GDAL conform)
  # (R) set runtime arguments
  ext.peak<-iniparam$Params$externalpeaks                        # harry= Harrys Peaklist osm= OSM peak data
  kernel.size<-as.numeric(iniparam$Params$filterkernelsize)      # size of filter for mode=1; range 3-30; default=5 
  make.peak.mode<-iniparam$Params$makepeakmode                   #  mode:1=minmax,2=wood&Co.
  exact.enough<-as.numeric(iniparam$Params$exactenough)          # vertical exactness of flooding in meter
  epsg.code<-iniparam$Projection$targetepsg                      # epsg code of the target data
  target.proj4<-iniparam$Projection$targetproj4                  # proj4 proejction string as set in ini file
  latlon.proj4<-as.character(CRS("+init=epsg:4326"))                   # latlon wgs84 proj4 string
  domthres<-as.numeric(iniparam$Params$domthres)                 # threshold for accepted range of dominance used in makepeak=2
  merge<-as.numeric(iniparam$Params$mergemode)                   # threshold for accepted range of dominance used in makepeak=2
  
  
  #Wood
  wsize<-as.numeric(iniparam$Wood$WSIZE)
  tol.slope<-as.numeric(iniparam$Wood$TOL_SLOPE)
  tol.curve<-as.numeric(iniparam$Wood$TOL_CURVE)
  exponent<-as.numeric(iniparam$Wood$EXPONENT)
  zscale<-as.numeric(iniparam$Wood$ZSCALE)
  # fuzzylandform
  slope.to.deg<-iniparam$FuzzyLf$SLOPETODEG
  t.slope.min<-as.numeric(iniparam$FuzzyLf$T_SLOPE_MIN)
  t.slope.max<-as.numeric(iniparam$FuzzyLf$T_SLOPE_MAX)
  t.curve.min<-as.numeric(iniparam$FuzzyLf$T_CURVE_MIN)
  t.curve.max<-as.numeric(iniparam$FuzzyLf$T_CURVE_MAX)
  
  ### local settings 
  # internal SAGA name of DEM data file  
  dem.out='mp_dem.sdat'
  
  # (GDAL) gdalwarp is used to (1) convert the data format (2) assign the
  # projection information to the data.
  gdalwarp(dem.in, dem.out, overwrite=TRUE, s_srs=paste0('EPSG:',epsg.code), of='SAGA')  
  
  # (raster) read GDAL data set
  dem<- raster(dem.out)
  
  
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
    # add required cols
    df['name'] <-NA
    df['dominance'] <-NA
    df['prominence'] <-NA
    df['independence'] <-NA
    write.table(df,peak.list,row.names=F)
  }  
  
  ### if mode = 2 or 3   
  
  else if (make.peak.mode==2 | make.peak.mode==3){  
    
    # calculate wood's terrain indices   wood= 1=planar,2=pit,3=channel,4=pass,5=ridge,6=peak
    rsaga.geoprocessor('ta_morphometry',"Morphometric Features",env=myenv,
                       list(DEM='mp_dem.sgrd',
                            FEATURES='mp_wood.sgrd',
                            SLOPE='mp_slope.sgrd',
                            LONGC='mp_longcurv.sgrd',
                            CROSC='mp_crosscurv.sgrd',
                            MINIC='mp_mincurv.sgrd',
                            MAXIC='mp_maxcurv.sgrd',
                            SIZE=wsize,
                            TOL_SLOPE=tol.slope,
                            TOL_CURVE=tol.curve,
                            EXPONENT=exponent,
                            ZSCALE=zscale))
    
    
    peak.area<-raster('mp_wood.sdat')
    crs(peak.area)<-target.proj4
    # reclassify wood to binary peak mask
    peak.area<-reclassify(peak.area, c(0,5,0, 5.1,7,1 ))
    
    if (make.peak.mode==3){
      # calculate Jochen Schmidt's fuzzy landforms (https://faculty.unlv.edu/buckb/GEOL%20786%20Photos/NRCS%20data/Fuzz/felementf.aml) fuzzylandoforms are:  
      # using SAGA 'ta_morphometry',"Fuzzy Landform Element Classification" The result is classified as follows:
      # PLAIN     , 100  # PIT       , 111  # PEAK      , 122  # RIDGE     , 120  # CHANNEL   , 101	
      # SADDLE    , 121	# BSLOPE    ,   0	# FSLOPE    ,  10	# SSLOPE    ,  20	# HOLLOW    ,   1	
      # FHOLLOW   ,  11	# SHOLLOW   ,  21	# SPUR      ,   2	# FSPUR     ,  12	# SSPUR     ,  22	
      # NOTEwood SIZE=9,TOL_SLOPE=10.000000,TOL_CURVE=0.00001,EXPONENT=0.000000,ZSCALE=1.000000 
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
                              SLOPETODEG=slope.to.deg,
                              T_SLOPE_MIN=t.slope.min,
                              T_SLOPE_MAX=t.slope.max,
                              T_CURVE_MIN=t.curve.min,
                              T_CURVE_MAX=t.curve.max))
      
      # read sdat file into raster object
      peak.area<-raster('mp_fuzzylandform.sdat')
      # we have to reassign correct projection due to some troubles in twgs84 transformations
      crs(peak.area)<-target.proj4
      # reclassify fuzzylandforms to get a binary peak mask
      peak.area<-reclassify(peak.area, c(0,121,0, 121.1,123,1 ))
      
    }
    # read sdat file into raster object
    dem<-raster('mp_dem.sdat')
    crs(dem)<-target.proj4
    
    # mask dem with peak.area
    mpeak<-peak.area*dem
    
    # clump all peak.area  i.e. give each distinct area a unique ID
    clump.peak<-clump(peak.area)
    
    # create SpatialDataPolygon from the ID Areas
    SPpoly.clump.peak <- rasterToPolygons(clump.peak, dissolve=TRUE)
    
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
      # set the xy coordinates
      coordinates(df_coord) <- ~x+y
      # set the projection
      proj4string(df_coord) <- target.proj4
      return(df_coord)
    })
    # merge and stuff it in a data.frame
    df<- as.data.frame(do.call("rbind", ls_spdf_max))    
    
    # name the cols
    colnames(df)=c("xcoord","ycoord","altitude")
    # sort by altitude
    df<-df[order(df$altitude, decreasing=TRUE),]
    # add required cols
    df['name'] <-NA
    df['dominance'] <-NA
    df['prominence'] <-NA
    df['independence'] <-NA
    write.table(df,peak.list,row.names=F)    
    # duplicate dataframe
    final.peak.list<-df
    
    # calculate dominance and prominence
    # as a first estimate to reduce the peaklist to more 'true' peaks 
    for (i in 1: nrow(final.peak.list)){
      # call calculate functions and put retrieved value into the corresponding dataframe field
      final.peak.list[i,5]<-9999
      if (i>1){
        final.peak.list[i,5]<-calculateDominance(final.peak.list[i,1], final.peak.list[i,2],final.peak.list[i,3],exact.enough=exact.enough,myenv=myenv,root.dir=root.dir, working.dir=working.dir)
      }}
    # put result in fp
    fp<-final.peak.list
    # make a subset  with the tresholds as derived by the ini file for dominance
    # because the list is sorted higher peak will "mask" lower pweaks n their neighborhood
    final.peak.list<-subset(fp,fp$dominance > domthres  )
    # duplicate it 
    SP<-final.peak.list
    # make it spatial
    coordinates(SP) <- ~xcoord+ycoord
    # set the projection
    proj4string(SP) <- target.proj4
    writePointsShape(SP,"DEMpeaklist.shp")    
    
    
    if (ext.peak=='harry') {
      XHP<-extractHarry(extent,latlon.proj4,target.proj4)
      # call distance based merging of the peaks
      if(merge==1){df<-distMergePeaks(SP,XHP)}
      # call cost based merging of the peaks
      if(merge==2){df<-costMergePeaks(SP,XHP,dem,domthres)}
    }
    else if (ext.peak=='osm') {
      XOP<-extractOSMPoints(extent,iniparam)
      # call distance based merging of the peaks
      if(merge==1){df<-distMergePeaks(SP,XOP)}
      # call cost based merging of the peaks
      if(merge==2){df<-costMergePeaks(SP,XOP,dem,domthres)}
    }
    else {print('not implemented external peak data input')}
  }
  else {stop("not implemented yet")}
  
  names(df)<-c('xcoord', 'ycoord', 'altitude', 'name','dominance', 'prominence','independence')
  SP<-df
  ### write to shape for control purpose
  # make it spatial
  coordinates(SP) <- ~xcoord+ycoord
  # set the projection
  proj4string(SP) <- target.proj4
  # write shapefile
  writePointsShape(SP,"MergePeaks.shp")    
  
  # return merged peak data frames for common use
  return(df)
}
