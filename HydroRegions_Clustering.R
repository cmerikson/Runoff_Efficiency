library(dataRetrieval)
library(data.table)
library(lubridate)
library(tidyhydat)
library(ggplot2)
library(openxlsx)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggspatial)
library(cluster)
library(clValid)

Group_Names = c('1' = 'Northern Parallel',
                '2' = 'Central Canada',
                '3' = 'Rocky Mountains',
                '4' = 'Appalachians',
                '5' = 'Northern Pacific Coast',
                '6' = 'Southern Plains',
                '7' = 'Northern Plains',
                '8' = 'Central East',
                '9' = 'Northern Atlantic',
                '10' = 'Southwest',
                '11' = 'Southern Pacific Coast',
                '12' = 'Western Canada',
                '13' = 'Rocky Lowland',
                '14' = 'Pacific Northwest',
                '15' = 'Great Lakes',
                '16' = 'Southeast')

# USGS
setwd("C:\\Users\\Christian Erikson\\OneDrive - Dartmouth College\\Research\\Runoff_Ratio\\HydroRegions")
setwd("C:\\Users\\cmeri\\OneDrive - Dartmouth College\\Research\\Runoff_Ratio\\HydroRegions")


USGS_sites = fread('HCDN-2009_Station_Info.csv', colClasses = c("STATION ID" = "character"))
USGS_sites = USGS_sites[!(STATE%in%c('AK','HI','PR'))]
setnames(USGS_sites,'STATION ID','site_number')

# Add missing leading zeros
for (i in 1:nrow(USGS_sites)) {
  string_length = nchar(USGS_sites[i,site_number])
  if (string_length < 8) {
    USGS_sites = USGS_sites[i,site_number:=paste0('0',site_number)]
  }
}

Sites = USGS_sites

DischargeData = data.table()
for (i in 1:nrow(Sites)) { 
  df = setDT(readNWISdv(siteNumbers=Sites[i,site_number],parameterCd = '00060',statCd = '00003'))
  DischargeData = rbind(DischargeData,df,fill=T)
}
setnames(DischargeData,'X_00060_00003','Qcms')

DischargeData = DischargeData[,Qcms:=Qcms*0.028316846592]
DischargeData = DischargeData[Qcms>0]

DischargeData = DischargeData[,c('Month','Year'):=.(month(Date),ifelse(month(Date) > 9, as.numeric(year(Date) + 1), as.numeric(year(Date))))]
DischargeData = DischargeData[,YearPeak:=max(Qcms),by=c('site_no','Year')]
DischargeData = DischargeData[,MonthPeak:=max(Qcms),by=c('site_no','Month','Year')]
DischargeData = unique(DischargeData,by=c('site_no','Year','Month'))

Data = DischargeData[,NormalizedQ:=MonthPeak/YearPeak]
Data = Data[,MeanNormQ:=mean(NormalizedQ,na.rm=T),by=c('site_no','Month')]
Data = unique(Data,by=c('site_no','Month'))

WideData = reshape(Data[,c('site_no','Month','MeanNormQ')],idvar = 'site_no',timevar = 'Month',direction = 'wide')
WideData = WideData[,agency:='USGS']
setnames(WideData,'site_no','site_number',skip_absent = T)
setnames(WideData,'site_no','site_number',skip_absent = T)
setnames(WideData,'MeanNormQ.1','jan')
setnames(WideData,'MeanNormQ.2','feb')
setnames(WideData,'MeanNormQ.3','mar')
setnames(WideData,'MeanNormQ.4','apr')
setnames(WideData,'MeanNormQ.5','may')
setnames(WideData,'MeanNormQ.6','jun')
setnames(WideData,'MeanNormQ.7','jul')
setnames(WideData,'MeanNormQ.8','aug')
setnames(WideData,'MeanNormQ.9','sep')
setnames(WideData,'MeanNormQ.10','oct')
setnames(WideData,'MeanNormQ.11','nov')
setnames(WideData,'MeanNormQ.12','dec')

# Canada
setwd("C:\\Users\\Christian Erikson\\OneDrive - Dartmouth College\\Research\\Runoff_Ratio\\HydroRegions\\Canada")
setwd("C:\\Users\\cmeri\\OneDrive - Dartmouth College\\Research\\Runoff_Ratio\\HydroRegions\\Canada")

#download_hydat()

# Get data.table of all stations with sampling metadata
hydat_station_info <- data.table(hy_stations())
# Get data.table of unregulated stations. Start and end date are unreliable.
hydat_q_unregulated <- data.table(hy_stn_regulation())[REGULATED == F]
# Get data.table of date ranges for each station, select only unregulated stations with long records
hydat_station_daterange <- data.table(hy_stn_data_range())[
  STATION_NUMBER %chin% hydat_q_unregulated$STATION_NUMBER & # unregulated subset
    Year_from < 1961 & Year_to > 2015] # long record subset

# Merge station info metadata table with station date range table
setkey(hydat_station_daterange, STATION_NUMBER)
setkey(hydat_station_info, STATION_NUMBER)
hydat_unreg_stns <- hydat_station_info[hydat_station_daterange[DATA_TYPE == 'Q']]
hydat_unreg_site_nos <- unique(hydat_unreg_stns$STATION_NUMBER)

# Function for downloading, cleaning, and saving to file all daily flow data from a hydat station
CanadaStations = data.table()
for (hydat_site_no in hydat_unreg_site_nos){
  download <- data.table(hy_daily_flows(hydat_site_no))[
    ,':='(
      agency_cd = 'WSC',
      site_no = STATION_NUMBER,
      Date = ymd(Date),
      Q_cms = Value, # WSC discharge already in cms units
      water_year = ifelse(month(Date) > 9, as.numeric(year(Date) + 1), as.numeric(year(Date)))
    )
  ][,.(agency_cd,site_no, Date, water_year,Q_cms)]
  
  CanadaStations = rbind(CanadaStations,download)
}

CanadaStations = CanadaStations[Q_cms>0]

CanadaStations = CanadaStations[,c('Month','Year'):=.(month(Date),water_year)]
CanadaStations = CanadaStations[,YearPeak:=max(Q_cms),by=c('site_no','Year')]
CanadaStations = CanadaStations[,MonthPeak:=max(Q_cms),by=c('site_no','Month','Year')]
CanadaStations = unique(CanadaStations,by=c('site_no','Year','Month'))

CanadaData = CanadaStations[,NormalizedQ:=MonthPeak/YearPeak]
CanadaData = CanadaData[,MeanNormQ:=mean(NormalizedQ,na.rm=T),by=c('site_no','Month')]
CanadaData = unique(CanadaData,by=c('site_no','Month'))

WideCanadaData = reshape(CanadaData[,c('site_no','Month','MeanNormQ')],idvar = 'site_no',timevar = 'Month',direction = 'wide')
WideCanadaData = WideCanadaData[,agency:='WSC']
setnames(WideCanadaData,'site_no','site_number')
setnames(WideCanadaData,'MeanNormQ.1','jan')
setnames(WideCanadaData,'MeanNormQ.2','feb')
setnames(WideCanadaData,'MeanNormQ.3','mar')
setnames(WideCanadaData,'MeanNormQ.4','apr')
setnames(WideCanadaData,'MeanNormQ.5','may')
setnames(WideCanadaData,'MeanNormQ.6','jun')
setnames(WideCanadaData,'MeanNormQ.7','jul')
setnames(WideCanadaData,'MeanNormQ.8','aug')
setnames(WideCanadaData,'MeanNormQ.9','sep')
setnames(WideCanadaData,'MeanNormQ.10','oct')
setnames(WideCanadaData,'MeanNormQ.11','nov')
setnames(WideCanadaData,'MeanNormQ.12','dec')

# Combind USGS and WSC data
setwd("C:\\Users\\Christian Erikson\\OneDrive - Dartmouth College\\Research\\Runoff_Ratio\\HydroRegions")
setwd("C:\\Users\\cmeri\\OneDrive - Dartmouth College\\Research\\Runoff_Ratio\\HydroRegions")

CombinedData = rbind(WideData,WideCanadaData)

# Import Elevation, Latitude, Longitude
HCDN_Elevation = fread('HCDN_Elevations.csv')
HCDN_Elevation = HCDN_Elevation[,site_number:=gsub('USGS-','',HCDN_Elevation$shedID)]

WSC_Elevation = fread('WSC_Elevations.csv')
WSC_Elevation = WSC_Elevation[,site_number:=shedID]

Elevations = rbind(HCDN_Elevation[,c('site_number','elev')],WSC_Elevation[,c('site_number','elev')])

CombinedData = merge(CombinedData,Elevations,by='site_number')

setnames(USGS_sites,'LAT_GAGE','LATITUDE')
setnames(USGS_sites,'LONG_GAGE','LONGITUDE')
setnames(hydat_unreg_stns,'STATION_NUMBER','site_number')
Coordinates = rbind(USGS_sites[,c('site_number','LATITUDE','LONGITUDE')],hydat_unreg_stns[,c('site_number','LATITUDE','LONGITUDE')])

CombinedData = merge(CombinedData,Coordinates,by='site_number')

# Prepare for clustering
ClusteringData = CombinedData[,c('site_number','jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec','LATITUDE','LONGITUDE','elev')]
ClusteringData = na.omit(ClusteringData)

sites = ClusteringData$site_number
ClusteringData = ClusteringData[,c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec','LATITUDE','LONGITUDE','elev')]

Scaled_ClustersData=scale(ClusteringData)

Scaled_ClustersData = data.frame(Scaled_ClustersData)

rownames(Scaled_ClustersData) <- sites

set.seed(0)
Kmeans = kmeans(Scaled_ClustersData,5,25)

Scaled_ClustersData = cbind(Scaled_ClustersData,Kmeans$cluster)
names(Scaled_ClustersData)[names(Scaled_ClustersData)=='Kmeans$cluster'] = 'cluster'

#Unscale for plotting
ClusteringData = cbind(ClusteringData,Scaled_ClustersData['cluster'])

ClusteringData = cbind(ClusteringData,sites)
setnames(ClusteringData,'sites','site_number')

# Split Geographically Incoherent Clusters
ClusteringData = ClusteringData[cluster==13,cluster:=ifelse((LATITUDE>38 & LONGITUDE>-97.5),15,13)]
ClusteringData = ClusteringData[cluster==10,cluster:=ifelse((LONGITUDE>-97.5),16,10)]

saveRDS(ClusteringData,'ClusteringData.RDS')
ClusteringData = readRDS('ClusteringData.RDS')

# Silhouette 
Silouette = silhouette(Scaled_ClustersData$cluster,dist = dist(Scaled_ClustersData[,c(1:15)]))
windows()
plot(Silouette)

# Dunn Index
Dunn = dunn(clusters = Scaled_ClustersData$cluster,Data = as.matrix(Scaled_ClustersData))

Dunn_Results = read.xlsx(r"(C:\Users\cmeri\OneDrive - Dartmouth College\Research\Runoff_Ratio\HydroRegions\Dunn_Index.xlsx)")
ggplot(Dunn_Results,aes(clusters,Dunn))+
  geom_point()+
  geom_line(linetype='dashed')+
  labs(x='Number of Clusters',y='Dunn Index')+
  theme_bw()

# Centroids
Centroids = ClusteringData[,c('cen_lat','cen_long'):=.(mean(LATITUDE),mean(LONGITUDE)),by='cluster']
Centroids = Centroids[,Euclidean:=sqrt((LATITUDE-cen_lat)^2 + (LONGITUDE-cen_long)^2)]
Centroids = Centroids[Euclidean<25,c('cen_lat2','cen_long2'):=.(mean(LATITUDE),mean(LONGITUDE)),by='cluster']
Centroids = Centroids[,Euclidean2:=sqrt((LATITUDE-cen_lat2)^2 + (LONGITUDE-cen_long2)^2)]

# Vizualize Clusters
ClusterMap =  ggplot(data = NULL) +  
  geom_sf(data=ne_countries(country = 'United States of America',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") +
  geom_sf(data=ne_countries(country = 'Canada',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") + 
  coord_sf(xlim = c(-142, -50), ylim = c(24.5, 70), expand = FALSE)+
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(2.5, 'cm'), width = unit(2.5,'cm'),
                         pad_x = unit(45, "pt"), pad_y = unit(25, "pt"),
                         style = north_arrow_nautical()) +
  geom_point(data=ClusteringData,aes(x=LONGITUDE,y=LATITUDE,fill=as.character(cluster)),color = 'transparent', pch = 21, size = 2)+
  scale_fill_manual(values=c("#c79ae2", "#378811", "#cb1775", "#a7e831", "#c6dbae", "#d0cc36", "#0f1f5f", "#5648d3", "#5d1800", "#6ceac0", "#f24219", "#2cf52b", "#b00bd9", "#0b4512", "#fa7ee3", "#6a7d54"),labels = c('1' = 'Northern Parallel','2' = 'Central Canada','3' = 'Rocky Mountains','4' = 'Appalachians','5' = 'Northern Pacific Coast','6' = 'Southern Plains','7' = 'Northern Plains','8' = 'Central East','9' = 'Northern Atlantic','10' = 'Southwest','11' = 'Southern Pacific Coast','12' = 'Western Canada','13' = 'Rocky Lowland','14' = 'Pacific Northwest','15' = 'Great Lakes','16' = 'Southeast'), breaks=c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16'))+
  #scale_color_manual(values=c('#cab2d6','#a6cee3','#1f78b4','#419486',"violetred4",'darksalmon','red','aquamarine','purple4','antiquewhite3','magenta','navy','goldenrod','khaki','sienna','darkseagreen'),labels = c('1' = 'Northern Parallel','2' = 'Central Canada','3' = 'Rocky Mountains','4' = 'Appalachians','5' = 'Northern Pacific Coast','6' = 'Southern Plains','7' = 'Northern Plains','8' = 'Central East','9' = 'Northern Atlantic','10' = 'Southwest','11' = 'Southern Pacific Coast','12' = 'Western Canada','13' = 'Rocky Lowland','14' = 'Pacific Northwest','15' = 'Great Lakes','16' = 'Southeast'))+
  #scale_fill_manual(values=c('#cab2d6','#a6cee3','#1f78b4','#419486',"violetred4",'darksalmon','red','aquamarine','purple4','antiquewhite3','magenta','navy','goldenrod','khaki','sienna','darkseagreen'),labels = c('1' = 'Northern Parallel','2' = 'Central Canada','3' = 'Rocky Mountains','4' = 'Appalachians','5' = 'Northern Pacific Coast','6' = 'Southern Plains','7' = 'Northern Plains','8' = 'Central East','9' = 'Northern Atlantic','10' = 'Southwest','11' = 'Southern Pacific Coast','12' = 'Western Canada','13' = 'Rocky Lowland','14' = 'Pacific Northwest','15' = 'Great Lakes','16' = 'Southeast'))+
  #geom_text(data=ClusteringData,aes(x=cen_long,y=cen_lat,label=paste(cluster,Group_Names[cluster])),size=5)+
  labs(x='Longitude',y='Latitude',fill='Cluster')+
  theme(axis.title = element_text(size=20), axis.text = element_text(size=16),legend.text = element_text(size=12),legend.title = element_text(size=18))

write.csv(ClusteringData[,c('site_number','cluster','LATITUDE','LONGITUDE')],'ClusteredStations.csv')

ClusteringData = readRDS('ClusteringData.RDS')
Rockies = ClusteringData[cluster%in%c(3,13)]
