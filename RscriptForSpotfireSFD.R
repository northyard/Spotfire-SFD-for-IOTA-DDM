token <- "" #enter authorization key here
dn <- "" #enter device network here
base <- "" #base url here
if(!exists("deviceid")) {
  deviceid<-"urn:phoneid:00000000=AndroidPhoneInternalSensor"
}
if(!exists("timestart")) {
  timestart<-"1513209600000"
}
if(!exists("timeend")) {
  timeend<-"1514878886000"
}
timestart<-"1513209600000"
timeend<-"1514878886000"
deviceid<-"urn:phoneid:00000000=AndroidPhoneInternalSensor"
library("httr")
library("jsonlite")
library("plyr")
httr::set_config( config(ssl_verifypeer = 0L) )
query <- paste(base,"/devices?filter[0].Key=deviceidentifier&filter[0].Value=",deviceid,sep="")
getdata <- GET(query, add_headers(Authorization=token,"X-DeviceNetwork"=dn))
getdatatext <- content(getdata, "text")
getdatarawjson <- fromJSON(getdatatext, flatten = TRUE)
devid <- getdatarawjson$Rows$Id
query2 <- paste(base,"/devices/",devid,sep="")
getdata2 <- GET(query2, add_headers(Authorization=token,"X-DeviceNetwork"=dn))
getdatatext2 <- content(getdata2, "text")
getdatarawjson2 <- fromJSON(getdatatext2, flatten = TRUE)
smartobj <- getdatarawjson2$SmartObjects$Id
#smartobj <- data.frame(as.list(getdatarawjson2$SmartObjects$Id))
#smartobj <- data.frame(lapply(getdatarawjson2$SmartObjects$Id, type.convert), stringsAsFactors=FALSE)
for(i in 1:length(smartobj)){
 id <- smartobj[i]
 query3 <- paste(base,"/resources?filter[0].Key=smartobjectid&filter[0].Value=",id, sep="") 
 getdata3 <- GET(query3, add_headers(Authorization=token,"X-DeviceNetwork"=dn))
 getdatatext3 <- content(getdata3, "text")
 getdatarawjson3 <- fromJSON(getdatatext3, flatten = TRUE,simplifyVector = TRUE, simplifyDataFrame =TRUE)
 if(i==1) {
   resources <- getdatarawjson3$Rows
 } else {
   resources <- rbind.fill(resources, getdatarawjson3$Rows)
 }   
}
resourcedf<- as.data.frame(resources)
resourcedf<-resourcedf[,-grep("LatestMeasurement",colnames(resourcedf))] #remove LatestMeasurement col as due to unsupported list type
smartobjdf <- as.data.frame(getdatarawjson2$SmartObjects)
smartobjdf<-smartobjdf[,-grep("Tags",colnames(smartobjdf))] #remove Tags col as due to unsupported list type
#now load healthSensor data
#find matching resourceid
targetresource <- resourcedf[grep("\\bHealthSensor\\b", resourcedf$Name),] #\\b for exact match
targetresourceid <- targetresource$Id #find matching resourceid
query2 <- paste(base,"/measurements/",targetresourceid,"/since/",timestart, "/to/",timeend,sep="")
getdata2 <- GET(query2, add_headers(Authorization=token,"X-DeviceNetwork"=dn))
getdatatext2 <- content(getdata2, "text")
getdatarawjson2 <- fromJSON(getdatatext2, flatten = TRUE,simplifyVector = TRUE, simplifyDataFrame =TRUE)
HealthSensor<- as.data.frame(getdatarawjson2)
targetresource <- resourcedf[grep("\\bacHealthSensor\\b", resourcedf$Name),] #\\b for exact match
targetresourceid <- targetresource$Id #find matching resourceid
query2 <- paste(base,"/measurements/",targetresourceid,"/since/",timestart, "/to/",timeend,sep="")
getdata2 <- GET(query2, add_headers(Authorization=token,"X-DeviceNetwork"=dn))
getdatatext2 <- content(getdata2, "text")
getdatarawjson2 <- fromJSON(getdatatext2, flatten = TRUE,simplifyVector = TRUE, simplifyDataFrame =TRUE)
acHealthSensor<- as.data.frame(getdatarawjson2)
#now load bleSensor data
#find matching resourceid
targetresource <- resourcedf[grep("\\bBleSensor\\b", resourcedf$Name),] #\\b for exact match
targetresourceid <- targetresource$Id #find matching resourceid
query2 <- paste(base,"/measurements/",targetresourceid,"/since/",timestart, "/to/",timeend,sep="")
getdata2 <- GET(query2, add_headers(Authorization=token,"X-DeviceNetwork"=dn))
getdatatext2 <- content(getdata2, "text")
getdatarawjson2 <- fromJSON(getdatatext2, flatten = TRUE,simplifyVector = TRUE, simplifyDataFrame =TRUE)
BleSensor<- as.data.frame(getdatarawjson2)
#now load Pressure data
#find matching resourceid
targetresource <- resourcedf[grep("\\bPressure\\b", resourcedf$Name),] #\\b for exact match
targetresourceid <- targetresource$Id #find matching resourceid
query2 <- paste(base,"/measurements/",targetresourceid,"/since/",timestart, "/to/",timeend,sep="")
getdata2 <- GET(query2, add_headers(Authorization=token,"X-DeviceNetwork"=dn))
getdatatext2 <- content(getdata2, "text")
getdatarawjson2 <- fromJSON(getdatatext2, flatten = TRUE,simplifyVector = TRUE, simplifyDataFrame =TRUE)
Pressure<- as.data.frame(getdatarawjson2)
#now load Altitude data
targetresource <- resourcedf[grep("\\bAltutude\\b", resourcedf$Name),] #\\b for exact match
targetresourceid <- targetresource$Id #find matching resourceid
query2 <- paste(base,"/measurements/",targetresourceid,"/since/",timestart, "/to/",timeend,sep="")
getdata2 <- GET(query2, add_headers(Authorization=token,"X-DeviceNetwork"=dn))
getdatatext2 <- content(getdata2, "text")
getdatarawjson2 <- fromJSON(getdatatext2, flatten = TRUE,simplifyVector = TRUE, simplifyDataFrame =TRUE)
Altitude<- as.data.frame(getdatarawjson2)
#now load Volume data
targetresource <- resourcedf[grep("\\bVolume\\b", resourcedf$Name),] #\\b for exact match
targetresourceid <- targetresource$Id #find matching resourceid
query2 <- paste(base,"/measurements/",targetresourceid,"/since/",timestart, "/to/",timeend,sep="")
getdata2 <- GET(query2, add_headers(Authorization=token,"X-DeviceNetwork"=dn))
getdatatext2 <- content(getdata2, "text")
getdatarawjson2 <- fromJSON(getdatatext2, flatten = TRUE,simplifyVector = TRUE, simplifyDataFrame =TRUE)
Volume<- as.data.frame(getdatarawjson2)
#now load Battery data
targetresource <- resourcedf[grep("\\bBattery\\b", resourcedf$Name),] #\\b for exact match
targetresourceid <- targetresource$Id #find matching resourceid
query2 <- paste(base,"/measurements/",targetresourceid,"/since/",timestart, "/to/",timeend,sep="")
getdata2 <- GET(query2, add_headers(Authorization=token,"X-DeviceNetwork"=dn))
getdatatext2 <- content(getdata2, "text")
getdatarawjson2 <- fromJSON(getdatatext2, flatten = TRUE,simplifyVector = TRUE, simplifyDataFrame =TRUE)
Battery<- as.data.frame(getdatarawjson2)
#now load Light data
targetresource <- resourcedf[grep("\\bLight\\b", resourcedf$Name),] #\\b for exact match
targetresourceid <- targetresource$Id #find matching resourceid
query2 <- paste(base,"/measurements/",targetresourceid,"/since/",timestart, "/to/",timeend,sep="")
getdata2 <- GET(query2, add_headers(Authorization=token,"X-DeviceNetwork"=dn))
getdatatext2 <- content(getdata2, "text")
getdatarawjson2 <- fromJSON(getdatatext2, flatten = TRUE,simplifyVector = TRUE, simplifyDataFrame =TRUE)
Light<- as.data.frame(getdatarawjson2)
#now load Azimuth data
targetresource <- resourcedf[grep("\\bAzimuth\\b", resourcedf$Name),] #\\b for exact match
targetresourceid <- targetresource$Id #find matching resourceid
query2 <- paste(base,"/measurements/",targetresourceid,"/since/",timestart, "/to/",timeend,sep="")
getdata2 <- GET(query2, add_headers(Authorization=token,"X-DeviceNetwork"=dn))
getdatatext2 <- content(getdata2, "text")
getdatarawjson2 <- fromJSON(getdatatext2, flatten = TRUE,simplifyVector = TRUE, simplifyDataFrame =TRUE)
Azimuth<- as.data.frame(getdatarawjson2)
#now load isUeMoving data
targetresource <- resourcedf[grep("\\bisUeMoving\\b", resourcedf$Name),] #\\b for exact match
targetresourceid <- targetresource$Id #find matching resourceid
query2 <- paste(base,"/measurements/",targetresourceid,"/since/",timestart, "/to/",timeend,sep="")
getdata2 <- GET(query2, add_headers(Authorization=token,"X-DeviceNetwork"=dn))
getdatatext2 <- content(getdata2, "text")
getdatarawjson2 <- fromJSON(getdatatext2, flatten = TRUE,simplifyVector = TRUE, simplifyDataFrame =TRUE)
isUeMoving<- as.data.frame(getdatarawjson2)
#now load isInLift data
targetresource <- resourcedf[grep("\\bisInLift\\b", resourcedf$Name),] #\\b for exact match
targetresourceid <- targetresource$Id #find matching resourceid
query2 <- paste(base,"/measurements/",targetresourceid,"/since/",timestart, "/to/",timeend,sep="")
getdata2 <- GET(query2, add_headers(Authorization=token,"X-DeviceNetwork"=dn))
getdatatext2 <- content(getdata2, "text")
getdatarawjson2 <- fromJSON(getdatatext2, flatten = TRUE,simplifyVector = TRUE, simplifyDataFrame =TRUE)
isInLift<- as.data.frame(getdatarawjson2)
#now load Proximity data
targetresource <- resourcedf[grep("\\bProximity\\b", resourcedf$Name),] #\\b for exact match
targetresourceid <- targetresource$Id #find matching resourceid
query2 <- paste(base,"/measurements/",targetresourceid,"/since/",timestart, "/to/",timeend,sep="")
getdata2 <- GET(query2, add_headers(Authorization=token,"X-DeviceNetwork"=dn))
getdatatext2 <- content(getdata2, "text")
getdatarawjson2 <- fromJSON(getdatatext2, flatten = TRUE,simplifyVector = TRUE, simplifyDataFrame =TRUE)
Proximity<- as.data.frame(getdatarawjson2)



