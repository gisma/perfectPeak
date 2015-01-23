#'@title Extract peak position and altitude from Harry's Bergliste
#'@description  The current Kmz file of Harry's peak list is downloaded and will be cleaned to derive coordinates altitude and name of all available peaks within a region of interest 


#'@name extractHarry
#'@aliases extractHarry

#'@usage harry(dem,current.proj4)
#'@author Chris Reudenbach 
#'
#'@references Marburg Open Courseware Advanced GIS: \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#'@references Breitkreutz, H.: Gipfelliste. URL: \url{http://www.tourenwelt.info/commons/download/bergliste-komplett.kmz.php}
#' 
#'@param dem is a Digital Elevation Model with geographic coordinates  
#'@param target.projection is a valid proj.4 string containing the correct target crs  
#'

#'@export extractHarry
#'@examples   
#'  #### Example to extract the ROI and 
#'  #### create a dataframe containing coordinates, altitude and name
#'       
#' exampledem=system.file("dem.asc", package="perfectPeak")
#' target.projection<-'+proj=tmerc +lat_0=0 +lon_0=10.33333333333333 +k=1 +x_0=0 +y_0=-5000000 +ellps=bessel +towgs84=577.326,90.129,463.919,5.137,1.474,5.297,2.4232 +units=m +no_defs'
#' extractHarry(exampledem,'target.projection')
#' 

extractHarry <- function(dem.latlon,latlon.proj4,current.proj4){
  
  
  # use the download.file function to access online content. note we change already the filename and 
  # we also pass the .php extension of the download address
  download("http://www.tourenwelt.info/commons/download/bergliste-komplett.kmz.php",dest="bergliste.zip", mode = "wb") 
  
  # use r unzip to convert it to a kml file
  unzip ("bergliste.zip",exdir = "./")
  
  # convert to csv file with babel (you need to install the babel binaries on your system)
  system("gpsbabel -i kml -f bergliste-komplett.kml -o unicsv -F bergliste-komplett.csv")
  
  # read into data.frame
  df=read.csv("bergliste-komplett.csv",  header = TRUE, sep = ",", dec='.')
  
  # extract altitude out of Description column that is full of  html garbage
  altitude<-as.numeric(substring(df$Description, regexpr('H&ouml;he:</td><td>', df$Description)+19,regexpr("</td></tr>", df$Description)-1))
  
  # delete the unused cols
  df$Description <- NULL
  df$No <- NULL
  
  # and put altitude values into df
  df$Altitude<- altitude
  
  # making a subset of the ROI (that is the current extent)
  df.sub = subset(df, Longitude >=dem.latlon@extent@xmin & Longitude<=dem.latlon@extent@xmax & Latitude>=dem.latlon@extent@ymin & Latitude  <=dem.latlon@extent@ymax)
  
  #Now it's getting spatial
  # first we have to assign lat lon geographic coordinates
  harry.bergliste.ll<-SpatialPointsDataFrame(data.frame(df.sub$Longitude,df.sub$Latitude),data.frame(df.sub$Name,df.sub$Altitude), proj4string = CRS(latlon.proj4))
  writePointsShape(harry.bergliste.ll,"bergliste_latlon.shp")
  # then we project it to MGI
  harry.bergliste.mgi<-spTransform(harry.bergliste.ll,CRS(current.proj4))
  writePointsShape(harry.bergliste.mgi,"bergliste_projected.shp")
  # convert back to dataframe
  mgi.df <- as.data.frame(harry.bergliste.mgi)
  return(harry.bergliste.mgi) 
  
}
