root = "C:\\Users\\Christian Erikson\\OneDrive - Dartmouth College\\Research\\Runoff_Ratio\\ERA5"

library(data.table)
library(raster)
library(maptools)
library(tictoc)

#### Extract Data ####
# Import Shapefiles
USGS = readShapePoly(paste0(root,'\\Input\\HCDN_Watersheds.shp'))
WSC = readShapePoly(paste0(root,'\\Input\\WSC_WGS84.shp'))

# Convert NetCDF to Raster
precip_raster = brick(paste0(root,'\\Input\\NARR_Precip.nc'),varname='apcp')
temperature_raster = brick(paste0(root,'\\Input\\NARR_Temp.nc'),varname='air')

# Reproject to WGS84
precip_raster = projectRaster(precip_raster, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
temperature_raster = projectRaster(temperature_raster, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

# Calculate USGS Watershed Means
# Precipitation
tic()
USGS_Watersheds = data.table(site_number=USGS$identifier)
for (i in 1:524) {
  column = extract(precip_raster, USGS, fun=mean, na.rm=TRUE, df=TRUE, layer=i,nl=1)
  USGS_Watersheds = cbind(USGS_Watersheds,column)
}
USGS_Watersheds = USGS_Watersheds[,!('ID')]
saveRDS(USGS_Watersheds,paste0(root,'\\Input\\USGS_NARR_Precip.RDS'))
toc()

# Temperature
tic()
USGS_Temp = data.table(site_number=USGS$identifier)
for (i in 1:524) {
  column = extract(temperature_raster, USGS, fun=mean, na.rm=TRUE, df=TRUE, layer=i,nl=1)
  USGS_Temp = cbind(USGS_Temp,column)
}
USGS_Temp = USGS_Temp[,!('ID')]
saveRDS(USGS_Temp,paste0(root,'\\Input\\USGS_NARR_Temp.RDS'))
toc()

# Calculate WSC Watershed Means
# Precipitation
tic()
WSC_Watersheds = data.table(site_number=WSC$StationNum)
for (i in 1:524) {
  column = extract(precip_raster, WSC, fun=mean, na.rm=TRUE, df=TRUE, layer=i,nl=1)
  WSC_Watersheds = cbind(WSC_Watersheds,column)
}
WSC_Watersheds = WSC_Watersheds[,!('ID')]
saveRDS(WSC_Watersheds,paste0(root,'\\Input\\WSC_NARR_Precip.RDS'))
toc()

# Temperature
tic()
WSC_Temp = data.table(site_number=WSC$StationNum)
for (i in 1:524) {
  column = extract(temperature_raster, WSC, fun=mean, na.rm=TRUE, df=TRUE, layer=i,nl=1)
  WSC_Temp = cbind(WSC_Temp,column)
}
WSC_Temp = WSC_Temp[,!('ID')]
saveRDS(WSC_Temp,paste0(root,'\\Input\\WSC_NARR_Temp.RDS'))
toc()

#### Average by Water year ####
USGS_Watersheds = readRDS(paste0(root,'\\Input\\USGS_NARR_Precip.RDS'))
USGS_Temp = readRDS(paste0(root,'\\Input\\USGS_NARR_Temp.RDS'))
WSC_Watersheds = readRDS(paste0(root,'\\Input\\WSC_NARR_Precip.RDS'))
WSC_Temp = readRDS(paste0(root,'\\Input\\WSC_NARR_Temp.RDS'))

# USGS Precip
PrecipData = USGS_Watersheds[,11:514]
HCDN_precip = data.table()
for (i in 0:41) {
  Subset = PrecipData[,(1+(12*i)):(12+(12*i))]
  SubsetA = Subset[,c(2,7,9,12)]*30
  SubsetB = Subset[,c(1,3,4,6,8,10,11)]*31
  SubsetC = Subset[,c(5)]*28
  Subset = cbind(SubsetA,SubsetB,SubsetC)
  YearAverage = data.table(rowSums(Subset))
  HCDN_precip = cbind(HCDN_precip,YearAverage)
}

colnames(HCDN_precip) = c(as.character((1980:2021)))
HCDN_precip = HCDN_precip/1000
HCDN_precip = cbind(USGS_Watersheds$site_number,HCDN_precip)

setnames(HCDN_precip,'V1','site_number')
HCDN_precip = HCDN_precip[,site_number:=gsub('USGS-','',HCDN_precip$site_number)]

# USGS Temp
TempData = USGS_Temp[,11:514]
HCDN_temp = data.table()
for (i in 0:41) {
  Subset = TempData[,(1+(12*i)):(12+(12*i))]
  YearAverage = data.table(rowMeans(Subset))
  HCDN_temp = cbind(HCDN_temp,YearAverage)
}

colnames(HCDN_temp) = c(as.character((1980:2021)))
HCDN_temp = cbind(USGS_Watersheds$site_number,HCDN_temp)

setnames(HCDN_temp,'V1','site_number')
HCDN_temp = HCDN_temp[,site_number:=gsub('USGS-','',HCDN_temp$site_number)]

# WSC Precip
PrecipData = WSC_Watersheds[,11:514]
WSC_precip = data.table()
for (i in 0:41) {
  Subset = PrecipData[,(1+(12*i)):(12+(12*i))]
  SubsetA = Subset[,c(2,7,9,12)]*30
  SubsetB = Subset[,c(1,3,4,6,8,10,11)]*31
  SubsetC = Subset[,c(5)]*28
  Subset = cbind(SubsetA,SubsetB,SubsetC)
  YearAverage = data.table(rowSums(Subset))
  WSC_precip = cbind(WSC_precip,YearAverage)
}

colnames(WSC_precip) = c(as.character((1980:2021)))
WSC_precip = WSC_precip/1000
WSC_precip = cbind(WSC_Watersheds$site_number,WSC_precip)

setnames(WSC_precip,'V1','site_number')

# WSC Temp
TempData = WSC_Temp[,11:514]
WSC_temp = data.table()
for (i in 0:41) {
  Subset = TempData[,(1+(12*i)):(12+(12*i))]
  YearAverage = data.table(rowMeans(Subset))
  WSC_temp = cbind(WSC_temp,YearAverage)
}

colnames(WSC_temp) = c(as.character((1980:2021)))
WSC_temp = cbind(WSC_Watersheds$site_number,WSC_temp)

setnames(WSC_temp,'V1','site_number')

# Merge Agencies
NARR_precip = rbind(HCDN_precip,WSC_precip)
NARR_temp = rbind(HCDN_temp,WSC_temp)

# Save
saveRDS(NARR_precip,paste0(root,'\\NARR_precip.RDS'))
saveRDS(NARR_temp,paste0(root,'\\NARR_temp.RDS'))

#### Seasonal ####

# USGS Precip
PrecipData = USGS_Watersheds[,13:516]
DJF_precip = data.table()
MAM_precip = data.table()
JJA_precip = data.table()
SON_precip = data.table()
for (i in 0:41) {
  Subset = PrecipData[,(1+(12*i)):(12+(12*i))]
  SubsetA = Subset[,c(4,6,9,11)]*30
  SubsetB = Subset[,c(1,3,5,7,8,10,12)]*31
  SubsetC = Subset[,c(2)]*28
  Subset = cbind(SubsetA,SubsetB,SubsetC)
  DJF = data.table(rowSums(Subset[,1:3]))
  MAM = data.table(rowSums(Subset[,4:6]))
  JJA = data.table(rowSums(Subset[,7:9]))
  SON = data.table(rowSums(Subset[,10:12]))
  DJF_precip = cbind(DJF_precip,DJF)
  MAM_precip = cbind(MAM_precip,MAM)
  JJA_precip = cbind(JJA_precip,JJA)
  SON_precip = cbind(SON_precip,SON)
}

colnames(DJF_precip) = c(paste('DJF',as.character((1980:2021)),sep = '_'))
colnames(MAM_precip) = c(paste('MAM',as.character((1980:2021)),sep = '_'))
colnames(JJA_precip) = c(paste('JJA',as.character((1980:2021)),sep = '_'))
colnames(SON_precip) = c(paste('SON',as.character((1980:2021)),sep = '_'))

MonthlyUSGS_precip = cbind(DJF_precip,MAM_precip,JJA_precip,SON_precip)
MonthlyUSGS_precip = MonthlyUSGS_precip/1000
MonthlyUSGS_precip = cbind(USGS_Watersheds$site_number,MonthlyUSGS_precip)

setnames(MonthlyUSGS_precip,'V1','site_number')
MonthlyUSGS_precip = MonthlyUSGS_precip[,site_number:=gsub('USGS-','',MonthlyUSGS_precip$site_number)]

# USGS Temp
TempData = USGS_Watersheds[,13:516]
DJF_temp = data.table()
MAM_temp = data.table()
JJA_temp = data.table()
SON_temp = data.table()
for (i in 0:41) {
  Subset = TempData[,(1+(12*i)):(12+(12*i))]
  SubsetA = Subset[,c(4,6,9,11)]*30
  SubsetB = Subset[,c(1,3,5,7,8,10,12)]*31
  SubsetC = Subset[,c(2)]*28
  Subset = cbind(SubsetA,SubsetB,SubsetC)
  DJF = data.table(rowSums(Subset[,1:3]))
  MAM = data.table(rowSums(Subset[,4:6]))
  JJA = data.table(rowSums(Subset[,7:9]))
  SON = data.table(rowSums(Subset[,10:12]))
  DJF_temp = cbind(DJF_temp,DJF)
  MAM_temp = cbind(MAM_temp,MAM)
  JJA_temp = cbind(JJA_temp,JJA)
  SON_temp = cbind(SON_temp,SON)
}

colnames(DJF_temp) = c(paste('DJF',as.character((1980:2021)),sep = '_'))
colnames(MAM_temp) = c(paste('MAM',as.character((1980:2021)),sep = '_'))
colnames(JJA_temp) = c(paste('JJA',as.character((1980:2021)),sep = '_'))
colnames(SON_temp) = c(paste('SON',as.character((1980:2021)),sep = '_'))

MonthlyUSGS_temp = cbind(DJF_temp,MAM_temp,JJA_temp,SON_temp)
MonthlyUSGS_temp = MonthlyUSGS_temp/1000
MonthlyUSGS_temp = cbind(USGS_Watersheds$site_number,MonthlyUSGS_temp)

setnames(MonthlyUSGS_temp,'V1','site_number')
MonthlyUSGS_temp = MonthlyUSGS_temp[,site_number:=gsub('USGS-','',MonthlyUSGS_temp$site_number)]


# WSC Precip
PrecipData = WSC_Watersheds[,13:516]
DJF_precip = data.table()
MAM_precip = data.table()
JJA_precip = data.table()
SON_precip = data.table()
for (i in 0:41) {
  Subset = PrecipData[,(1+(12*i)):(12+(12*i))]
  SubsetA = Subset[,c(4,6,9,11)]*30
  SubsetB = Subset[,c(1,3,5,7,8,10,12)]*31
  SubsetC = Subset[,c(2)]*28
  Subset = cbind(SubsetA,SubsetB,SubsetC)
  DJF = data.table(rowSums(Subset[,1:3]))
  MAM = data.table(rowSums(Subset[,4:6]))
  JJA = data.table(rowSums(Subset[,7:9]))
  SON = data.table(rowSums(Subset[,10:12]))
  DJF_precip = cbind(DJF_precip,DJF)
  MAM_precip = cbind(MAM_precip,MAM)
  JJA_precip = cbind(JJA_precip,JJA)
  SON_precip = cbind(SON_precip,SON)
}

colnames(DJF_precip) = c(paste('DJF',as.character((1980:2021)),sep = '_'))
colnames(MAM_precip) = c(paste('MAM',as.character((1980:2021)),sep = '_'))
colnames(JJA_precip) = c(paste('JJA',as.character((1980:2021)),sep = '_'))
colnames(SON_precip) = c(paste('SON',as.character((1980:2021)),sep = '_'))

MonthlyWSC_precip = cbind(DJF_precip,MAM_precip,JJA_precip,SON_precip)
MonthlyWSC_precip = MonthlyWSC_precip/1000
MonthlyWSC_precip = cbind(WSC_Watersheds$site_number,MonthlyWSC_precip)

setnames(MonthlyWSC_precip,'V1','site_number')

# WSC Temp
TempData = WSC_Watersheds[,13:516]
DJF_temp = data.table()
MAM_temp = data.table()
JJA_temp = data.table()
SON_temp = data.table()
for (i in 0:41) {
  Subset = TempData[,(1+(12*i)):(12+(12*i))]
  SubsetA = Subset[,c(4,6,9,11)]*30
  SubsetB = Subset[,c(1,3,5,7,8,10,12)]*31
  SubsetC = Subset[,c(2)]*28
  Subset = cbind(SubsetA,SubsetB,SubsetC)
  DJF = data.table(rowSums(Subset[,1:3]))
  MAM = data.table(rowSums(Subset[,4:6]))
  JJA = data.table(rowSums(Subset[,7:9]))
  SON = data.table(rowSums(Subset[,10:12]))
  DJF_temp = cbind(DJF_temp,DJF)
  MAM_temp = cbind(MAM_temp,MAM)
  JJA_temp = cbind(JJA_temp,JJA)
  SON_temp = cbind(SON_temp,SON)
}

colnames(DJF_temp) = c(paste('DJF',as.character((1980:2021)),sep = '_'))
colnames(MAM_temp) = c(paste('MAM',as.character((1980:2021)),sep = '_'))
colnames(JJA_temp) = c(paste('JJA',as.character((1980:2021)),sep = '_'))
colnames(SON_temp) = c(paste('SON',as.character((1980:2021)),sep = '_'))

MonthlyWSC_temp = cbind(DJF_temp,MAM_temp,JJA_temp,SON_temp)
MonthlyWSC_temp = MonthlyWSC_temp
MonthlyWSC_temp = cbind(WSC_Watersheds$site_number,MonthlyWSC_temp)

setnames(MonthlyWSC_temp,'V1','site_number')

saveRDS(MonthlyWSC_precip,paste0(root,'WSC_Monthly_NARR.RDS'))
saveRDS(MonthlyWSC_temp,paste0(root,'WSC_MonthlyTemp_NARR.RDS'))
