root = "C:\\Users\\Christian Erikson\\OneDrive - Dartmouth College\\Research\\Runoff_Ratio"
root = "C:\\Users\\Cmeri\\OneDrive - Dartmouth College\\Research\\Runoff_Ratio"

library(data.table)
library(ggplot2)
library(ggpubr)
library(broom)
library(openxlsx)
library(patchwork)
library(dataRetrieval)
library(lubridate)
library(tidyhydat)
library(raster)
library(forcats)
library(ggforce)
library(geomtextpath)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggnewscale)
library(scales)

#### Prepare Sites ####
All_Sites = fread(paste0(root,'\\Input_Data\\ClusteredStations.csv'),colClasses = c("site_number" = "character"))

# Remove Stations in Puerto Rico and Hawaii
All_Sites = All_Sites[!(site_number%in%c('50113800','50110900','50108000','50100450','50092000','50034000','50025155','16071500'))]
sites = All_Sites$site_number

# Subset Stations by Agency
HCDN_sites = All_Sites[nchar(site_number)>=8]
WSC_sites = All_Sites[nchar(site_number)<8]

# Download Discharge Data Mean Annual by Water Year
DischargeData = data.table(site_no=character(),year_nu=numeric(),mean_va=numeric())
for (i in 1:nrow(HCDN_sites)) { # For loop needed because request size exceeds limit
  df = setDT(readNWISdata(siteNumbers=HCDN_sites[i,site_number],parameterCd = '00060',service='stat',statReportType='annual',statYearType='water'))
  Data = df[,c('site_no','year_nu','mean_va')]
  DischargeData = rbind(DischargeData,Data)
}

# Convert Discharge to cubic meters per second
DischargeData = DischargeData[,mean_Qcms:=(mean_va*0.028316846592)]
DischargeData = na.omit(DischargeData[mean_Qcms>0])

# Add Coordinates back to table
setnames(DischargeData, 'site_no', 'site_number', skip_absent = T)
DischargeData = merge(DischargeData[,!('mean_va')], All_Sites[,c('site_number','LATITUDE','LONGITUDE')],by='site_number')

setnames(DischargeData,'year_nu','year')

CanadaDischarge = data.table()
for (i in WSC_sites$site_number){
  download <- data.table(hy_daily_flows(i))[
    ,':='(
      site_number = STATION_NUMBER,
      Date = ymd(Date),
      Q_cms = Value, # WSC discharge already in cms units
      water_year = ifelse(month(Date) > 9, as.numeric(year(Date) + 1), as.numeric(year(Date)))
    )
  ][,.(site_number, Date, water_year,Q_cms)]
  
  CanadaDischarge = rbind(CanadaDischarge,download)
}

# Average Discharge Values for WSC sites
CanadaDischarge = na.omit(CanadaDischarge[Q_cms>0])
CanadaDischarge = CanadaDischarge[,mean_Qcms:=mean(Q_cms,na.rm=T),by=c('site_number','water_year')]
CanadaDischarge = unique(CanadaDischarge[,c('site_number','water_year','mean_Qcms')],by=c('site_number','water_year'))
setnames(CanadaDischarge, 'water_year', 'year')
CanadaDischarge = merge(CanadaDischarge, All_Sites[,c('site_number','LATITUDE','LONGITUDE')],by='site_number')

# Merge USGS and WSC
DischargeData = rbind(DischargeData,CanadaDischarge)
saveRDS(DischargeData,paste0(root,'\\Input_Data\\DischargeData.Rds'))
DischargeData = readRDS(paste0(root,'\\Input_Data\\DischargeData.Rds'))
DischargeData = DischargeData[year>=1950 & year<=2021]

# Remove sites with discontinuities greater than 3 years
Sites_Truncated = data.table()
for (i in unique(DischargeData$site_number)) {
  Data = DischargeData[site_number==i & year>=1950 & year <= 2021]
  Truncated = max(diff(Data$year,lag=1))
  minimum = min(Data$year)
  maximum = max(Data$year)
  row = cbind(i,Truncated,minimum)
  row = cbind(row,maximum)
  Sites_Truncated = rbind(Sites_Truncated,row)
}

Sites_Truncated = Sites_Truncated[as.numeric(Truncated)<=3 & minimum<=1980 & maximum>=2020]

RecordLength = Sites_Truncated
setnames(RecordLength,'i','site_number')

#### Import PRISM and NARR data ####

# Shapefile Drainage Area
GEE_Sites_Drainage = setDT(read.xlsx(paste0(root,'\\Input_Data\\GEE_Sites_Areas.xlsx')))
site_number = gsub('USGS-','',GEE_Sites_Drainage$identifier)
GEE_Sites_Drainage = GEE_Sites_Drainage[,GEEArea:=drainage_area_km2*1000000]
GEEArea = cbind(GEE_Sites_Drainage,site_number)

WSC_Areas = setDT(read.xlsx(paste0(root,'\\Input_Data\\WSC_Areas.xlsx')))
setnames(WSC_Areas,'StationNum','site_number')
WSC_Areas = WSC_Areas[,GEEArea:=Area_km2*1000000]

GEEArea = rbind(GEEArea[,c('site_number','GEEArea')],WSC_Areas[,c('site_number','GEEArea')])

# PRISM Precip
PRISM_precip = setDT(read.xlsx(paste0(root,'\\Input_Data\\HCDNPcptAll.xlsx')))
PRISM_sites = gsub('USGS-','',PRISM_precip$ID)
PRISM_precip = PRISM_precip[,6:131]/1000
PRISM_precip = cbind(PRISM_precip,PRISM_sites)
setnames(PRISM_precip,'PRISM_sites','site_number')

# PRISM Temp
PRISM_Temp = fread(paste0(root,'\\Input_Data\\PRISM_Temperature.csv'))
PRISM_Temp = PRISM_Temp[,MeanTemp_K:=(MeanTemp_C + 273.15)]
PRISM_Temp = PRISM_Temp[,site_number:=gsub('USGS-','',PRISM_Temp$shedID)]
PRISM_Temp = PRISM_Temp[,!(c('shedID','system:index'))]
PRISM_Temp = PRISM_Temp[site_number%in%HCDN_sites$site_number]

# NARR Precip
NARR_precip = readRDS(paste0(root,'\\Input_Data\\NARR_precip.RDS'))
NARR_precip = NARR_precip[site_number%in%WSC_sites$site_number]

# NARR Temperature
NARR_Temp = readRDS(paste0(root,'\\Input_Data\\NARR_temp.RDS'))
NARR_Temp = NARR_Temp[site_number%in%WSC_sites$site_number]

meltNARR = melt(NARR_Temp, id.vars = c("site_number"),
                  measure.vars = c(as.character(1980:2021)))
meltNARR = meltNARR[,year:=as.numeric(as.character(meltNARR$variable))]
setnames(meltNARR,'value','MeanTemp_K',skip_absent = T)

# Merge Agencies
PRISM_precip = rbind(PRISM_precip,NARR_precip,fill=T)
PRISM_Temp = rbind(PRISM_Temp,meltNARR,fill=T)

# Merge Precipitation and Drainage Area
PRISM_precip = merge(PRISM_precip, GEEArea[,c('GEEArea','site_number')],by='site_number')

#### Calculate Yearly Data ####
PrecipSeries = PRISM_precip

# Change table structure
meltPrecip = melt(PrecipSeries, id.vars = c("site_number","GEEArea"),
                  measure.vars = c(as.character(1950:2021)))
meltPrecip = meltPrecip[,year:=as.numeric(as.character(meltPrecip$variable))]

# Merge Discharge and Precipitation Data
YearlyRatios = merge(DischargeData,meltPrecip,by=c('site_number','year'))
setnames(YearlyRatios,'value','Precip_m')

# Calculate Runoff Ratios
YearlyRatios = YearlyRatios[,RunoffRatio:=(((mean_Qcms*31536000)/GEEArea)/(Precip_m))]

YearlyRatios = merge(YearlyRatios,PRISM_Temp[,c('MeanTemp_K','site_number','year')],by=c('site_number','year'))

# Remove extra columns
YearlyRatios = YearlyRatios[,!(c('variable'))]

# Save Dataset
saveRDS(YearlyRatios,paste0(root,'\\Input_Data\\YearlyData.Rds'))
YearlyRatios = readRDS(paste0(root,'\\Input_Data\\YearlyData.Rds'))

# Add Clusters
HydroRegions = fread(paste0(root,'\\Input_Data\\ClusteredStations.csv'),colClasses = c("site_number" = "character"))

YearlyRatios = merge(YearlyRatios, HydroRegions[,c('site_number','cluster')], by='site_number')

# Mean and Standard Devation of Runoff Ratio By Cluster
RunoffStats = YearlyRatios[,c('RunoffMean','RunoffSD'):=.(mean(RunoffRatio),sd(RunoffRatio)),by='cluster']
RunoffStats = unique(RunoffStats,by='cluster')
RunoffStats = RunoffStats[,c('cluster','RunoffMean','RunoffSD')]
setorder(RunoffStats,by='cluster')

#### Visualize Data ####
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

# Runoff Ratio Time Series
RunoffPlot = ggplot(YearlyRatios[mean_Qcms>0 & year>=1950],aes(year,RunoffRatio))+
  facet_wrap(vars(cluster),scales='free_y',labeller = as_labeller(Group_Names))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_y_log10()+
  labs(x='Year',y = 'Runoff Ratio')+
  theme_bw()

# Precipitation Time Series
PrecipPlot = ggplot(YearlyRatios[mean_Qcms>0 & year>=1950],aes(year,Precip_m))+
  facet_wrap(vars(cluster),scales='free_y',labeller = as_labeller(Group_Names))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(x='Year',y = 'Precipitation (m)')+
  theme_bw()

# Temperature Time Series
TempPlot = ggplot(YearlyRatios[mean_Qcms>0 & year>=1950],aes(year,MeanTemp_K))+
  facet_wrap(vars(cluster),scales='free_y',labeller = as_labeller(Group_Names))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(x='Year',y = expression('Mean Temperature ' (degree~C)))+
  theme_bw()

# Discharge Time Series
DischargePlot = ggplot(YearlyRatios[year>=1950 & mean_Qcms>0],aes(year,mean_Qcms))+
  facet_wrap(vars(cluster),scales='free_y',labeller = as_labeller(Group_Names))+
  geom_point()+
  geom_smooth(method = 'lm')+
  #stat_cor(label.y.npc = 'bottom')+
  scale_y_log10()+
  labs(x='Year',y = 'Discharge (cms)')+
  theme_bw()

# Combined Plot
Plot = (DischargePlot + RunoffPlot) / (PrecipPlot + TempPlot)
ggsave(filename = paste0(root,'\\Figures\\Raw_TimeSeries.png'),Plot,width = 12.5,height = 8)

#### Original Regression ####
All_Data=YearlyRatios[mean_Qcms>0,]

# Merge Temp and Precip year offset
All_temp = copy(YearlyRatios)
All_temp = All_temp[,TempYear:=(year+1)]
All_temp = All_temp[,OffsetTemp:=MeanTemp_K]
setnames(All_temp,'year','original_year')
setnames(All_temp,'TempYear','year')
All_Data = merge(All_Data,All_temp[,c('site_number','year','OffsetTemp')],by=c('site_number','year'))

All_precip = copy(YearlyRatios)
All_precip = All_precip[,PrecipYear:=(year+1)]
All_precip = All_precip[,OffsetPrecip:=Precip_m]
setnames(All_precip,'year','original_year')
setnames(All_precip,'PrecipYear','year')
All_Data = merge(All_Data,All_precip[,c('site_number','year','OffsetPrecip')],by=c('site_number','year'))

All_Data = unique(All_Data,by=c('site_number','year'))
saveRDS(All_Data,paste0(root,'\\Code_Exports\\Clustered_AllData.Rds'))

# Prepare Regression
Zscores = All_Data[,c('site_number','cluster','year','mean_Qcms','Precip_m','OffsetPrecip','OffsetTemp','MeanTemp_K','RunoffRatio')]

# Log transform data
Zscores = Zscores[,c('mean_Qcms','Precip_m','OffsetPrecip','OffsetTemp','MeanTemp_K','LogRunoff'):=.(log10(mean_Qcms),log10(Precip_m),log10(OffsetPrecip),log10(OffsetTemp),log10(MeanTemp_K),log10(RunoffRatio))]

ZscoreSummary = data.table(site_number=as.character(),cluster=as.numeric(),year=as.numeric(),zQ=as.numeric(),zLogRunoff=as.numeric(),zRunoff=as.numeric(),zPrecip=as.numeric(),zOffPrecip=as.numeric(),zTemp=as.numeric(),zOffTemp=as.numeric())
for (n in unique(Zscores$cluster)) {
  #Statistics Table
  Stats = Zscores[cluster==n]
  Stats = Stats[,c('meanQ','meanLogRunoff','meanRunoff','meanPrecip','meanTemp','meanOffPrecip','meanOffTemp','sdQ','sdLogRunoff','sdRunoff','sdPrecip','sdTemp','sdOffPrecip','sdOffTemp'):=
                  .(mean(mean_Qcms,na.rm=T),mean(LogRunoff,na.rm=T),mean(RunoffRatio,na.rm=T),mean(Precip_m,na.rm=T),mean(MeanTemp_K,na.rm=T),mean(OffsetPrecip,na.rm=T),mean(OffsetTemp,na.rm=T),sd(mean_Qcms,na.rm=T),sd(LogRunoff,na.rm=T)
                    ,sd(RunoffRatio,na.rm=T),sd(Precip_m,na.rm=T),sd(MeanTemp_K,na.rm=T),sd(OffsetPrecip,na.rm=T),sd(OffsetTemp,na.rm=T)),by='site_number']
  Stats = unique(Stats[,c('site_number','cluster','meanQ','meanLogRunoff','meanRunoff','meanPrecip','meanTemp','meanOffPrecip','meanOffTemp','sdQ','sdLogRunoff','sdRunoff','sdPrecip','sdTemp','sdOffPrecip','sdOffTemp')],by='site_number')
  
  # Calculate Z-scores
  for (i in unique(Stats$site_number)) {
    Zscores = Zscores[,c('zQ','zLogRunoff','zRunoff','zPrecip','zOffPrecip','zTemp','zOffTemp'):=
                        .((mean_Qcms-Stats[site_number==i,meanQ])/Stats[site_number==i,sdQ],(LogRunoff-Stats[site_number==i,meanLogRunoff])/Stats[site_number==i,sdLogRunoff],(RunoffRatio-Stats[site_number==i,meanRunoff])/Stats[site_number==i,sdRunoff],
                          (Precip_m-Stats[site_number==i,meanPrecip])/Stats[site_number==i,sdPrecip],(OffsetPrecip-Stats[site_number==i,meanOffPrecip])/Stats[site_number==i,sdOffPrecip],
                          (MeanTemp_K-Stats[site_number==i,meanTemp])/Stats[site_number==i,sdTemp],(OffsetTemp-Stats[site_number==i,meanOffTemp])/Stats[site_number==i,sdOffTemp])]
    Z = Zscores[site_number==i,c('site_number','cluster','year','zQ','zLogRunoff','zRunoff','zPrecip','zOffPrecip','zTemp','zOffTemp')]
    ZscoreSummary = rbind(Z,ZscoreSummary)
  }
}


# Average Z-scores
RegressionData = unique(ZscoreSummary[,c('zQAv','zRunoffAv','zLogRunoffAv','zPrecipAv','zOffPrecipAv','zTempAv','zOffTempAv'):=
                                        .(mean(zQ,na.rm=T),mean(zRunoff,na.rm=T),mean(zLogRunoff,na.rm=T),mean(zPrecip,na.rm=T),mean(zOffPrecip,na.rm=T),mean(zTemp,na.rm=T),mean(zOffTemp,na.rm=T)),by=c('year','cluster')],by=c('year','cluster'))
saveRDS(RegressionData,paste0(root,'\\Input_Data\\RegressionData.RDS'))
RegressionData = readRDS(paste0(root,'\\Input_Data\\RegressionData.RDS'))

#RegressionSubset = unique(ZscoreSummary[site_number%in%RecordLength$site_number,c('zRunoffAv','zLogRunoffAv','zPrecipAv','zOffPrecipAv','zTempAv','zOffTempAv'):=
                                         # .(mean(zRunoff,na.rm=T),mean(zLogRunoff,na.rm=T),mean(zPrecip,na.rm=T),mean(zOffPrecip,na.rm=T),mean(zTemp,na.rm=T),mean(zOffTemp,na.rm=T)),by=c('year','cluster')],by=c('year','cluster'))

# Regress
ClusterResults = data.table(cluster=numeric(),zPrecipAv=numeric(),zOffTempAv=numeric(),Precip_pvalue=numeric(),OffTemp_pvalue=numeric())
R_squared = data.table(adj.r.squared=numeric(),p.value=numeric())
sequence = (seq(from= 1, to = 16))
R_squared = cbind(R_squared,sequence)
setnames(R_squared,'sequence','cluster')
RegressionData = setorder(RegressionData, cols='cluster')

lmp <- function (model) {
  if (class(model) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

for (G in unique(RegressionData$cluster)) {
  TestFit = lm(data=RegressionData[cluster==G,], zLogRunoffAv ~ zPrecipAv + zTempAv + zOffTempAv + zOffPrecipAv)
  Groupcoefficients = setDT(tidy(TestFit))
  Groupcoefficients=Groupcoefficients[,c('cluster','zPrecipAv','zOffTempAv','Precip_pvalue','OffTemp_pvalue'):=.(G,Groupcoefficients[2,(estimate)],Groupcoefficients[3,estimate],Groupcoefficients[2,p.value],Groupcoefficients[3,p.value])]
  Groupcoefficients = Groupcoefficients[,c('cluster','zPrecipAv','zOffTempAv','Precip_pvalue','OffTemp_pvalue')]
  ClusterResults = rbind(ClusterResults,Groupcoefficients)
  R = summary(TestFit)
  R = R[['adj.r.squared']]
  R_squared = R_squared[cluster==G,adj.r.squared:=R]
  pvalue = lmp(TestFit)
  R_squared = R_squared[cluster==G,p.value:=pvalue]
}

ClusterResults = unique(ClusterResults,by='cluster')

write.csv(R_squared,paste0(root,'\\Code_Exports\\R_squared_Values.csv'))
write.csv(ClusterResults,paste0(root,'\\Code_Exports\\ClusterResults.csv'))

# Compare Offset and Regular Temperature
TempFit = lm(data=RegressionData[cluster==10], zLogRunoffAv ~ zTempAv)
summary(TempFit)

OffTempFit = lm(data=RegressionData[cluster==10], zLogRunoffAv ~ zOffTempAv)
summary(OffTempFit)

#### Stepwise Regression ####

# First Regression
Step1Results = data.table(cluster=numeric(),intercept=numeric(),Precip_pvalue=numeric(),OffPrecip_pvalue=numeric(),Temp_pvalue=numeric(),OffTemp_pvalue=numeric())
Step1_R_squared = data.table(adj.r.squared=numeric(),p.value=numeric())
sequence = (seq(from= 1, to = 16))
Step1_R_squared = cbind(Step1_R_squared,sequence)
setnames(Step1_R_squared,'sequence','cluster')
RegressionData = setorder(RegressionData, cols='cluster')

for (G in unique(RegressionData$cluster)) {
  Step1 = lm(data=RegressionData[cluster==G], zLogRunoffAv ~ zPrecipAv + zTempAv + zOffPrecipAv + zOffTempAv)
  Groupcoefficients = setDT(tidy(Step1))
  Groupcoefficients=Groupcoefficients[,c('cluster','intercept','zPrecipAv','zOffPrecipAv','zTempAv','zOffTempAv','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue'):=.(G,Groupcoefficients[1,estimate],Groupcoefficients[2,(estimate)],Groupcoefficients[4,estimate],Groupcoefficients[3,estimate],Groupcoefficients[5,estimate],Groupcoefficients[2,p.value],Groupcoefficients[4,p.value],Groupcoefficients[3,p.value],Groupcoefficients[5,p.value])]
  Groupcoefficients = Groupcoefficients[,c('cluster','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue')]
  Step1Results = rbind(Step1Results,Groupcoefficients)
  R = summary(Step1)
  R = R[['adj.r.squared']]
  Step1_R_squared = Step1_R_squared[cluster==G,adj.r.squared:=R]
  pvalue = lmp(Step1)
  Step1_R_squared = Step1_R_squared[cluster==G,p.value:=pvalue]
}

Step1Results = unique(Step1Results,by='cluster')
write.csv(Step1Results,paste0(root,'\\Code_Exports\\Stepwise_Regression\\Step1_pvalues.csv'))

Step2Results = data.table()
row = data.table()
for (i in unique(Step1Results$cluster)) {
  if (Step1Results[cluster==i,Precip_pvalue]<0.15 & Step1Results[cluster==i,OffPrecip_pvalue]<0.15 & Step1Results[cluster==i,Temp_pvalue]<0.15 & Step1Results[cluster==i,OffTemp_pvalue]<0.15) {
    Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~ zPrecipAv + zOffPrecipAv + zTempAv + zOffTempAv))
    Regress = setDT(tidy(Regress0))
    row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],Regress[2,p.value],Regress[3,p.value],Regress[4,p.value],Regress[5,p.value],Regress[2,estimate],Regress[3,estimate],Regress[4,estimate],Regress[5,estimate])]
    Step2Results = rbind(Step2Results,row)
    row = data.table()
  }
  if (max(c(Step1Results[cluster==i,Precip_pvalue],Step1Results[cluster==i,OffPrecip_pvalue],Step1Results[cluster==i,Temp_pvalue],Step1Results[cluster==i,OffTemp_pvalue]))==Step1Results[cluster==i,Temp_pvalue]) {
    if (((Step1Results[cluster==i,Temp_pvalue]))>0.15) {
      Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~ zPrecipAv + zOffPrecipAv + zOffTempAv))
      Regress = setDT(tidy(Regress0))
      row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                  .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],Regress[2,p.value],Regress[3,p.value],NA_real_,Regress[4,p.value],Regress[2,estimate],Regress[3,estimate],NA_real_,Regress[4,estimate])]
      Step2Results = rbind(Step2Results,row)
      row = data.table()
    }
  }
  if (max(c(Step1Results[cluster==i,Precip_pvalue],Step1Results[cluster==i,OffPrecip_pvalue],Step1Results[cluster==i,Temp_pvalue],Step1Results[cluster==i,OffTemp_pvalue]))==Step1Results[cluster==i,OffTemp_pvalue]) {
    if (((Step1Results[cluster==i,OffTemp_pvalue]))>0.15) {
      Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~ zPrecipAv + zOffPrecipAv + zTempAv))
      Regress = setDT(tidy(Regress0))
      row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                  .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],Regress[2,p.value],Regress[3,p.value],Regress[4,p.value],NA_real_,Regress[2,estimate],Regress[3,estimate],Regress[4,estimate],NA_real_)]
      Step2Results = rbind(Step2Results,row)
      row = data.table()
    }
  }
  if (max(c(Step1Results[cluster==i,Precip_pvalue],Step1Results[cluster==i,OffPrecip_pvalue],Step1Results[cluster==i,Temp_pvalue],Step1Results[cluster==i,OffTemp_pvalue]))==Step1Results[cluster==i,Precip_pvalue]) {
    if (((Step1Results[cluster==i,Precip_pvalue]))>0.15) {
      Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~ zOffPrecipAv + zTempAv + zOffTempAv))
      Regress = setDT(tidy(Regress0))
      row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                  .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],NA_real_,Regress[2,p.value],Regress[3,p.value],Regress[4,p.value],NA_real_,Regress[2,estimate],Regress[3,estimate],Regress[4,estimate])]
      Step2Results = rbind(Step2Results,row)
      row = data.table()
    }
  }
  if (max(c(Step1Results[cluster==i,Precip_pvalue],Step1Results[cluster==i,OffPrecip_pvalue],Step1Results[cluster==i,Temp_pvalue],Step1Results[cluster==i,OffTemp_pvalue]))==Step1Results[cluster==i,OffPrecip_pvalue]) {
    if (((Step1Results[cluster==i,OffPrecip_pvalue]))>0.15) {
      Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~ zPrecipAv + zTempAv + zOffTempAv))
      Regress = setDT(tidy(Regress0))
      row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                  .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],Regress[2,p.value],NA_real_,Regress[3,p.value],Regress[4,p.value],Regress[2,estimate],NA_real_,Regress[3,estimate],Regress[4,estimate])]
      Step2Results = rbind(Step2Results,row)
      row = data.table()
    }
  }
} 

# Step 3
Step3Results = data.table()
row = data.table()
for (i in unique(Step2Results$cluster)) {
  # Precip from Step 2 Insignificant
  if (is.na(Step2Results[cluster==i,Precip_pvalue])) {
    if (max(c(Step2Results[cluster==i,OffPrecip_pvalue],Step2Results[cluster==i,Temp_pvalue],Step2Results[cluster==i,OffTemp_pvalue]),na.rm=T)==ifelse(is.na(Step2Results[cluster==i,Temp_pvalue]),-9999,Step2Results[cluster==i,Temp_pvalue])) {
      if ((Step2Results[cluster==i,Temp_pvalue])>0.15) {
        Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~  zOffPrecipAv + zOffTempAv))
        Regress = setDT(tidy(Regress0))
        row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],NA_real_,Regress[2,p.value],NA_real_,Regress[3,p.value],NA_real_,Regress[2,estimate],NA_real_,Regress[3,estimate])]
        Step3Results = rbind(Step3Results,row)
        row = data.table()
      }
    }
    if (max(c(Step2Results[cluster==i,OffPrecip_pvalue],Step2Results[cluster==i,Temp_pvalue],Step2Results[cluster==i,OffTemp_pvalue]),na.rm=T)==ifelse(is.na(Step2Results[cluster==i,OffTemp_pvalue]),-9999,Step2Results[cluster==i,OffTemp_pvalue])) {
      if ((Step2Results[cluster==i,OffTemp_pvalue])>0.15) {
        Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~  zOffPrecipAv + zTempAv))
        Regress = setDT(tidy(Regress0))
        row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                    .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],NA_real_,Regress[2,p.value],Regress[3,p.value],NA_real_,NA_real_,Regress[2,estimate],Regress[3,estimate],NA_real_)]
        Step3Results = rbind(Step3Results,row)
        row = data.table()
      }
    }
    if (max(c(Step2Results[cluster==i,OffPrecip_pvalue],Step2Results[cluster==i,Temp_pvalue],Step2Results[cluster==i,OffTemp_pvalue]),na.rm=T)==ifelse(is.na(Step2Results[cluster==i,OffPrecip_pvalue]),-9999,Step2Results[cluster==i,OffPrecip_pvalue])) {
      if ((Step2Results[cluster==i,OffPrecip_pvalue])>0.15) {
        Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~  zTempAv + zOffTempAv))
        Regress = setDT(tidy(Regress0))
        row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                    .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],NA_real_,NA_real_,Regress[2,p.value],Regress[3,p.value],NA_real_,NA_real_,Regress[2,estimate],Regress[3,estimate])]
        Step3Results = rbind(Step3Results,row)
        row = data.table()
      }
    }
  } 
  # Off Precip from Step 2 Insignificant
  if (is.na(Step2Results[cluster==i,OffPrecip_pvalue])) {
    if (max(c(Step2Results[cluster==i,Precip_pvalue],Step2Results[cluster==i,Temp_pvalue],Step2Results[cluster==i,OffTemp_pvalue]),na.rm=T)==ifelse(is.na(Step2Results[cluster==i,Temp_pvalue]),-9999,Step2Results[cluster==i,Temp_pvalue])) {
      if ((Step2Results[cluster==i,Temp_pvalue])>0.15) {
        Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~  zPrecipAv + zOffTempAv))
        Regress = setDT(tidy(Regress0))
        row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                    .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],Regress[2,p.value],NA_real_,NA_real_,Regress[3,p.value],Regress[2,estimate],NA_real_,NA_real_,Regress[3,estimate])]
        Step3Results = rbind(Step3Results,row)
        row = data.table()
      }
    }
    if (max(c(Step2Results[cluster==i,Precip_pvalue],Step2Results[cluster==i,Temp_pvalue],Step2Results[cluster==i,OffTemp_pvalue]),na.rm=T)==ifelse(is.na(Step2Results[cluster==i,OffTemp_pvalue]),-9999,Step2Results[cluster==i,OffTemp_pvalue])) {
      if ((Step2Results[cluster==i,OffTemp_pvalue])>0.15) {
        Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~  zPrecipAv + zTempAv))
        Regress = setDT(tidy(Regress0))
        row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                    .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],Regress[2,p.value],NA_real_,Regress[3,p.value],NA_real_,Regress[2,estimate],NA_real_,Regress[3,estimate],NA_real_)]
        Step3Results = rbind(Step3Results,row)
        row = data.table()
      }
    }
    if (max(c(Step2Results[cluster==i,Precip_pvalue],Step2Results[cluster==i,Temp_pvalue],Step2Results[cluster==i,OffTemp_pvalue]),na.rm=T)==ifelse(is.na(Step2Results[cluster==i,OffPrecip_pvalue]),-9999,Step2Results[cluster==i,OffPrecip_pvalue])) {
      if ((Step2Results[cluster==i,Precip_pvalue])>0.15) {
        Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~  zTempAv + zOffTempAv))
        Regress = setDT(tidy(Regress0))
        row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                    .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],NA_real_,NA_real_,Regress[2,p.value],Regress[3,p.value],NA_real_,NA_real_,Regress[2,estimate],Regress[3,estimate])]
        Step3Results = rbind(Step3Results,row)
        row = data.table()
      }
    }
  } 
  # Temp from Step 2 Insignificant
  if (is.na(Step2Results[cluster==i,Temp_pvalue])) {
    if (max(c(Step2Results[cluster==i,Precip_pvalue],Step2Results[cluster==i,OffTemp_pvalue],Step2Results[cluster==i,OffPrecip_pvalue]),na.rm=T)==ifelse(is.na(Step2Results[cluster==i,Precip_pvalue]),-9999,Step2Results[cluster==i,Precip_pvalue])) {
      if ((Step2Results[cluster==i,Precip_pvalue])>0.15) {
        Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~  zOffPrecipAv + zOffTempAv))
        Regress = setDT(tidy(Regress0))
        row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                    .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],NA_real_,Regress[2,p.value],NA_real_,Regress[3,p.value],NA_real_,Regress[2,estimate],NA_real_,Regress[3,estimate])]
        Step3Results = rbind(Step3Results,row)
        row = data.table()
      }
    }
    if (max(c(Step2Results[cluster==i,Precip_pvalue],Step2Results[cluster==i,OffTemp_pvalue],Step2Results[cluster==i,OffPrecip_pvalue]),na.rm=T)==ifelse(is.na(Step2Results[cluster==i,OffTemp_pvalue]),-9999,Step2Results[cluster==i,OffTemp_pvalue])) {
      if ((Step2Results[cluster==i,OffTemp_pvalue])>0.15) {
        Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~  zPrecipAv + zOffPrecipAv))
        Regress = setDT(tidy(Regress0))
        row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                    .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],Regress[2,p.value],Regress[3,p.value],NA_real_,NA_real_,Regress[2,estimate],Regress[3,estimate],NA_real_,NA_real_)]
        Step3Results = rbind(Step3Results,row)
        row = data.table()
      }
    }
    if (max(c(Step2Results[cluster==i,Precip_pvalue],Step2Results[cluster==i,OffTemp_pvalue],Step2Results[cluster==i,OffPrecip_pvalue]),na.rm=T)==ifelse(is.na(Step2Results[cluster==i,OffPrecip_pvalue]),-9999,Step2Results[cluster==i,OffPrecip_pvalue])) {
      if ((Step2Results[cluster==i,OffPrecip_pvalue])>0.15) {
        Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~  zPrecipAv + zOffTempAv))
        Regress = setDT(tidy(Regress0))
        row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                    .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],Regress[2,p.value],NA_real_,NA_real_,Regress[3,p.value],Regress[2,estimate],NA_real_,NA_real_,Regress[3,estimate])]
        Step3Results = rbind(Step3Results,row)
        row = data.table()
      }
    }
  }
  # OffTemp from Step 2 Insignificant
  if (is.na(Step2Results[cluster==i,OffTemp_pvalue])) {
    if (max(c(Step2Results[cluster==i,Precip_pvalue],Step2Results[cluster==i,Temp_pvalue],Step2Results[cluster==i,OffPrecip_pvalue]),na.rm=T)== ifelse(is.na(Step2Results[cluster==i,Precip_pvalue]),-9999,Step2Results[cluster==i,Precip_pvalue])) {
      if ((Step2Results[cluster==i,Precip_pvalue])>0.15) {
        Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~  zOffPrecipAv + zTempAv))
        Regress = setDT(tidy(Regress0))
        row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                    .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],NA_real_,Regress[2,p.value],Regress[3,p.value],NA_real_,NA_real_,Regress[2,estimate],Regress[3,estimate],NA_real_)]
        Step3Results = rbind(Step3Results,row)
        row = data.table()
      }
    }
    if (max(c(Step2Results[cluster==i,Precip_pvalue],Step2Results[cluster==i,Temp_pvalue],Step2Results[cluster==i,OffPrecip_pvalue]),na.rm=T)==ifelse(is.na(Step2Results[cluster==i,Temp_pvalue]),-9999,Step2Results[cluster==i,Temp_pvalue])) {
      if ((Step2Results[cluster==i,Temp_pvalue])>0.15) {
        Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~  zPrecipAv + zOffPrecipAv))
        Regress = setDT(tidy(Regress0))
        row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                    .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],Regress[2,p.value],Regress[3,p.value],NA_real_,NA_real_,Regress[2,estimate],Regress[3,estimate],NA_real_,NA_real_)]
        Step3Results = rbind(Step3Results,row)
        row = data.table()
      }
    }
    if (max(c(Step2Results[cluster==i,Precip_pvalue],Step2Results[cluster==i,Temp_pvalue],Step2Results[cluster==i,OffPrecip_pvalue]),na.rm=T)==ifelse(is.na(Step2Results[cluster==i,OffPrecip_pvalue]),-9999,Step2Results[cluster==i,OffPrecip_pvalue])) {
      if ((Step2Results[cluster==i,OffPrecip_pvalue])>0.15) {
        Regress0 = (lm(data=RegressionData[cluster==i], zLogRunoffAv ~  zPrecipAv + zTempAv))
        Regress = setDT(tidy(Regress0))
        row = row[,c('cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Temp_pvalue','OffTemp_pvalue','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=
                    .(i,summary(Regress0)$adj.r.squared,Regress[1,estimate],Regress[2,p.value],NA_real_,Regress[3,p.value],NA_real_,Regress[2,estimate],NA_real_,Regress[3,estimate],NA_real_)]
        Step3Results = rbind(Step3Results,row)
        row = data.table()
      }
    }
  }
}

# Combine Regression Results
StepwiseResults = rbind(Step2Results[cluster%in%c(1,3,5,6,7,8,9,10,12,15)],Step3Results) # Edit list based on Step3Results

setorder(StepwiseResults,by='cluster')

Pie_Column = rbind(StepwiseResults[,c('cluster','Precip_coeff')],StepwiseResults[,c('cluster','OffPrecip_coeff')],StepwiseResults[,c('cluster','Temp_coeff')],StepwiseResults[,c('cluster','OffTemp_coeff')],use.names=F)
write.csv(Pie_Column[,Precip_coeff],paste0(root,'\\Input_Data\\Coeff_column.csv'))

# Interaction Terms
Step4 = lm(data=RegressionData[cluster==1], zLogRunoffAv ~ zPrecipAv + zOffPrecipAv + zTempAv + zPrecipAv:zTempAv)
Step4 = setDT(tidy(Step4))

#### Z score Plots ####

zPrecip = ggplot(ZscoreSummary[year>=1950 & site_number%in%RecordLength$site_number],aes(year,zPrecip))+
  facet_wrap(vars(cluster),scales='free_y',labeller = as_labeller(Group_Names))+
  geom_point()+
  labs(x='Year',y='Z-score from Cluster Mean',title = 'Precipitation')+
  geom_smooth(method='lm')+
  theme_bw()

zTemp = ggplot(ZscoreSummary[year>=1950 & site_number%in%RecordLength$site_number],aes(year,zTemp))+
  facet_wrap(vars(cluster),scales='free_y',labeller = as_labeller(Group_Names))+
  geom_point()+
  labs(x='Year',y='Z-score from Cluster Mean',title = 'Temperature')+
  geom_smooth(method='lm')+
  theme_bw()

zQ = ggplot(ZscoreSummary[year>=1950],aes(year,zQ))+
  facet_wrap(vars(cluster),scales='free_y',labeller = as_labeller(Group_Names))+
  geom_point()+
  labs(x='Year',y='Z-score from Cluster Mean',title = 'Mean Annual Discharge')+
  geom_smooth(method='lm')+
  theme_bw()

zLogRunoff = ggplot(ZscoreSummary[year>=1950],aes(year,zLogRunoff))+
  facet_wrap(vars(cluster),scales='free_y',labeller = as_labeller(Group_Names))+
  geom_point()+
  labs(x='Year',y='Z-score from Cluster Mean',title = 'Log Runoff Ratio')+
  geom_smooth(method='lm')+
  theme_bw()

zTemp + zPrecip

#### Yearly Statistics ####
# Table of zStats
All_Data = All_Data[,LogRunoffRatio:=log10(RunoffRatio)]
zStats = data.table(cluster=numeric(),year=numeric(),muZLogRunoff=numeric(),sdZLogRunoff=numeric(),muZPrecip=numeric(),sdZPrecip=numeric(),muZTemp=numeric(),sdZTemp=numeric())
for (i in unique(All_Data$cluster)) {
  StatData = ZscoreSummary[cluster==i]
  StatTable = StatData[,c('muZLogRunoff','sdZLogRunoff','muZPrecip','sdZPrecip','muZTemp','sdZTemp'):=.(mean(zLogRunoff,na.rm=T),sd(zLogRunoff,na.rm=T),mean(zPrecip,na.rm=T),sd(zPrecip,na.rm=T),mean(zTemp,na.rm=T),sd(zTemp,na.rm=T)),by='year']
  StatTable = StatTable[,c('cluster','year','muZLogRunoff','sdZLogRunoff','muZPrecip','sdZPrecip','muZTemp','sdZTemp')]
  zStats = unique(rbind(zStats,StatTable),by=c('cluster','year'))
}


# Table of RawStats
RawStats = data.table(cluster=numeric(),year=numeric(),muRunoff=numeric(),sdRunoff=numeric(),muLogRunoff=numeric(),sdLogRunoff=numeric(),muPrecip=numeric(),sdPrecip=numeric(),muTemp=numeric(),sdTemp=numeric())
for (i in unique(All_Data$cluster)) {
  StatData = All_Data[cluster==i]
  StatTable = StatData[,c('muRunoff','sdRunoff','muLogRunoff','sdLogRunoff','muPrecip','sdPrecip','muTemp','sdTemp'):=.(mean(RunoffRatio,na.rm=T),sd(RunoffRatio,na.rm=T),mean(LogRunoffRatio,na.rm=T),sd(LogRunoffRatio,na.rm=T),mean(Precip_m,na.rm=T),sd(Precip_m,na.rm=T),mean(MeanTemp_K,na.rm=T),sd(MeanTemp_K,na.rm=T)),by='year']
  StatTable = StatTable[,c('cluster','year','muRunoff','sdRunoff','muLogRunoff','sdLogRunoff','muPrecip','sdPrecip','muTemp','sdTemp')]
  RawStats = unique(rbind(RawStats,StatTable),by=c('cluster','year'))
}

StatisticsTable = merge(zStats,RawStats,by=c('cluster','year'))

write.xlsx(StatisticsTable,paste0(root,'\\Code_Exports\\StatisticsTable.xlsx'))

#### Spatial Plot ####
Pre = ggplot(RegressionData[year>=1950 & year<=2000],aes(year,zLogRunoffAv))+
  facet_wrap(vars(cluster),scales = 'free_y',labeller = as_labeller(Group_Names))+
  geom_point()+
  geom_smooth(method = 'lm')+
  theme_bw()
Post = ggplot(RegressionData[year>=2000],aes(year,zLogRunoffAv))+
  facet_wrap(vars(cluster),scales = 'free_y',labeller = as_labeller(Group_Names))+
  geom_point()+
  geom_smooth(method = 'lm')+
  theme_bw()

zLogRunoff = ggplot(ZscoreSummary[year>=1950],aes(year,zLogRunoff))+
  facet_wrap(vars(cluster),scales = 'free_y',labeller = as_labeller(Group_Names))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(x='Year',y='Z-score from Cluster Mean',title = 'Runoff Ratio')+
  theme_bw()

RunoffEras = Pre + Post

Discharge_Runoff = zQ + zLogRunoff

Plots = (zQ + zLogRunoff) / (zPrecip + zTemp)
ggsave(filename = paste0(root,'\\Figures\\Zscore_TimeSeries.png'),Plots,width = 12.5,height = 8)

# Find centroid Lon and Lat by ID, as required
Centroids = setDT(read.xlsx(paste0(root,'\\Input_Data\\ClusterCentroids.xlsx')))

#Create Individual Plots by Cluster
cluster_list = Centroids$cluster

Pie_Data = setDT(read.xlsx(paste0(root,'\\Input_Data\\Pie_Data.xlsx')))

#### Paneled Regression Plots ####

for (i in unique(All_Data$cluster)) {
  A = ggplot(ZscoreSummary[cluster==i],aes(zOffTempAv,zLogRunoffAv))+
    geom_point()+
    geom_smooth(method = 'lm')+
    stat_cor(label.y=-2.5,aes(label=paste(..rr.label..,..p.label..,sep='~`,`~')))+
    facet_wrap(vars(cluster),labeller = as_labeller(Group_Names))+
    labs(x='Offset Temperature Z-score',y='Logrithmic Runoff Z-score')+
    theme_bw()
  B = ggplot(ZscoreSummary[cluster==i],aes(zTempAv,zLogRunoffAv))+
    geom_point()+
    geom_smooth(method = 'lm')+
    stat_cor(label.y=-2.5,aes(label=paste(..rr.label..,..p.label..,sep='~`,`~')))+
    facet_wrap(vars(cluster),labeller = as_labeller(Group_Names))+
    labs(x='Temperature Z-score',y='Logrithmic Runoff Z-score')+
    theme_bw()
  C = ggplot(ZscoreSummary[cluster==i],aes(zOffPrecipAv,zLogRunoffAv))+
    geom_point()+
    geom_smooth(method = 'lm')+
    stat_cor(label.y=-2.5,aes(label=paste(..rr.label..,..p.label..,sep='~`,`~')))+
    facet_wrap(vars(cluster),labeller = as_labeller(Group_Names))+
    labs(x='Offset Precipitation Z-score',y='Logrithmic Runoff Z-score')+
    theme_bw()
  D = ggplot(ZscoreSummary[cluster==i],aes(zPrecipAv,zLogRunoffAv))+
    geom_point()+
    geom_smooth(method = 'lm')+
    stat_cor(label.y=-2.5,aes(label=paste(..rr.label..,..p.label..,sep='~`,`~')))+
    facet_wrap(vars(cluster),labeller = as_labeller(Group_Names))+
    labs(x='Precipitation Z-score',y='Logrithmic Runoff Z-score')+
    theme_bw()
  
  PanelPlot <- (A + B) / (C + D)
  
  Name = paste('PanelCluster',i,sep = '_')
  assign(Name,PanelPlot)
  
  PanelPlot <<- Name
}

PanelCluster_12


#### Attribution by Cluster ####
ggplot(ZscoreSummary,aes(year,zLogRunoff))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(vars(cluster),labeller = as_labeller(Group_Names))

write.csv(StepwiseResults[,c('Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff')], paste0(root,'\\Input_Data\\Cluster_Regression_Coefficients.csv'))
Cluster_Regression_Coefficients = fread(paste0(root,'\\Input_Data\\Cluster_Regression_Coefficients.csv'))
setnames(Cluster_Regression_Coefficients,'Cluster','cluster',skip_absent = T)

PrecipFactors = data.table(cluster=numeric(),Precip=numeric(),OffPrecip=numeric(),PIntercept=numeric())
TempFactors = data.table(cluster=numeric(),Temp=numeric(),OffTemp=numeric(),TIntercept=numeric())
row = data.table()
for (c in unique(RegressionData$cluster)){
  Data = RegressionData[cluster==c]
    # Precipitation
    if (Pie_Data[cluster==c & Variable=='Precip',AbsCoefficient]>0 & Pie_Data[cluster==c & Variable=='OffPrecip',AbsCoefficient]>0){
      model = lm(data = Data, zLogRunoffAv ~ zPrecipAv + zOffPrecipAv)
      Coeff1 = setDT(tidy(model))
      row = row[,c('cluster','Precip','OffPrecip','PIntercept'):=.(c,Coeff1[2,estimate],Coeff1[3,estimate],Coeff1[1,estimate])]
      PrecipFactors = rbind(PrecipFactors,row)
      row = data.table()
    } 
    if (Pie_Data[cluster==c & Variable=='Precip',AbsCoefficient]>0 & Pie_Data[cluster==c & Variable=='OffPrecip',AbsCoefficient]<=0) {
      model = lm(data = Data, zLogRunoffAv ~ zPrecipAv)
      Coeff1 = setDT(tidy(model))
      row = row[,c('cluster','Precip','OffPrecip','PIntercept'):=.(c,Coeff1[2,estimate],NA_real_,Coeff1[1,estimate])]
      PrecipFactors = rbind(PrecipFactors,row)
      row = data.table()
    } 
    if (Pie_Data[cluster==c & Variable=='Precip',AbsCoefficient]<=0 & Pie_Data[cluster==c & Variable=='OffPrecip',AbsCoefficient]>0) {
      model = lm(data = Data, zLogRunoffAv ~ zOffPrecipAv)
      Coeff1 = setDT(tidy(model))
      row = row[,c('cluster','Precip','OffPrecip','PIntercept'):=.(c,NA_real_,Coeff1[2,estimate],Coeff1[1,estimate])]
      PrecipFactors = rbind(PrecipFactors,row)
      row = data.table()
    } else if (Pie_Data[cluster==c & Variable=='Precip',AbsCoefficient]<=0 & Pie_Data[cluster==c & Variable=='OffPrecip',AbsCoefficient]<=0){
      row = row[,c('cluster','Precip','OffPrecip','PIntercept'):=.(c,NA_real_,NA_real_,NA_real_)]
      PrecipFactors = rbind(PrecipFactors,row)
      row = data.table()}
    # Temperature
    if (Pie_Data[cluster==c & Variable=='Temp',AbsCoefficient]>0 & Pie_Data[cluster==c & Variable=='OffTemp',AbsCoefficient]>0){
      model = lm(data = Data, zLogRunoffAv ~ zTempAv + zOffTempAv)
      Coeff2 = setDT(tidy(model))
      row = row[,c('cluster','Temp','OffTemp','TIntercept'):=.(c,Coeff2[2,estimate],Coeff2[3,estimate],Coeff2[1,estimate])]
      TempFactors = rbind(TempFactors,row)
      row = data.table()
    } 
    if (Pie_Data[cluster==c & Variable=='OffTemp',AbsCoefficient]>0 & Pie_Data[cluster==c & Variable=='Temp',AbsCoefficient]<=0) {
      model = lm(data = Data, zLogRunoffAv ~ zOffTempAv)
      Coeff2 = setDT(tidy(model))
      row = row[,c('cluster','Temp','OffTemp','TIntercept'):=.(c,NA_real_,Coeff2[2,estimate],Coeff2[1,estimate])]
      TempFactors = rbind(TempFactors,row)
      row = data.table()
    } 
    if (Pie_Data[cluster==c & Variable=='OffTemp',AbsCoefficient]<=0 & Pie_Data[cluster==c & Variable=='Temp',AbsCoefficient]>0) {
    model = lm(data = Data, zLogRunoffAv ~ zTempAv)
    Coeff2 = setDT(tidy(model))
    row = row[,c('cluster','Temp','OffTemp','TIntercept'):=.(c,Coeff2[2,estimate],NA_real_,Coeff2[1,estimate])]
    TempFactors = rbind(TempFactors,row)
    row = data.table()
    } else if (Pie_Data[cluster==c & Variable=='Temp',AbsCoefficient]<=0 & Pie_Data[cluster==c & Variable=='OffTemp',AbsCoefficient]<=0){
      row = row[,c('cluster','Temp','OffTemp','TIntercept'):=.(c,NA_real_,NA_real_,NA_real_)]
      TempFactors = rbind(TempFactors,row)
      row = data.table()}
}

Factors = merge(PrecipFactors,TempFactors,by=('cluster'))

Projections = RegressionData[,c('cluster','site_number','year','zPrecipAv','zOffPrecipAv','zTempAv','zOffTempAv')]

#Set no data equal to zero
Factors = unique(Factors[,c('Precip','OffPrecip','PIntercept','Temp','OffTemp','TIntercept'):=.(ifelse(is.na(Precip),0,Precip),ifelse(is.na(OffPrecip),0,OffPrecip),ifelse(is.na(PIntercept),0,PIntercept),
                                                               ifelse(is.na(Temp),0,Temp),ifelse(is.na(OffTemp),0,OffTemp),ifelse(is.na(TIntercept),0,TIntercept))],by='cluster')
# Model
for (i in unique(Projections$cluster)) {
  Projections = Projections[cluster==i,c('ModelPrecip','ModelOffPrecip','PIntercept','ModelTemp','ModelOffTemp','TIntercept'):=
                              .(zPrecipAv*(Factors[cluster==i,Precip]),zOffPrecipAv*(Factors[cluster==i,OffPrecip]),Factors[cluster==i,PIntercept],                                                                                                          
                                                                                                           zTempAv*(Factors[cluster==i,Temp]),zOffTempAv*(Factors[cluster==i,OffTemp]),Factors[cluster==i,TIntercept])]
}

ProjectionTable = Projections[,c('PModelLogRunoff','TModelLogRunoff'):=.((ModelPrecip + ModelOffPrecip + PIntercept),(ModelTemp + ModelOffTemp + TIntercept))]
ProjectionTable = ProjectionTable[,c('cluster','site_number','year','PModelLogRunoff','TModelLogRunoff')]
ProjectionTable = ProjectionTable[,FullModel:=(PModelLogRunoff + TModelLogRunoff)]

ModelData = cbind(RegressionData,ProjectionTable[,c('FullModel','PModelLogRunoff','TModelLogRunoff')])

ModelSlopes = data.table(cluster=numeric(),Slope=numeric())
row = data.table()
for (i in unique(ProjectionTable$cluster)) {
  ModelRunoffSlope = lm(data=ProjectionTable[cluster==i], FullModel ~ year)
  Summary = setDT(tidy(ModelRunoffSlope))
  ModelRunoffSlope = Summary[2,estimate]
  row = row[,c('cluster','Slope'):=.(i,ModelRunoffSlope)]
  ModelSlopes = rbind(ModelSlopes,row)
  row=data.table()
}

ProjectionResults = data.table(cluster=ModelSlopes$cluster)
for (i in unique(ProjectionTable$cluster)){
  data = ProjectionTable[cluster==i]
    if (data[1,PModelLogRunoff]!=0) {
      PModel = lm(data=data, PModelLogRunoff ~ year)
      PModel = setDT(tidy(PModel))
      PModel = PModel[2,estimate]
      ProjectionResults = ProjectionResults[cluster==i,c('PrecipSlope'):=.(PModel)]
    }
    if (data[1,TModelLogRunoff]!=0) {
      TModel = lm(data=data, TModelLogRunoff ~ year)
      TModel = setDT(tidy(TModel))
      TModel = TModel[2,estimate]
      ProjectionResults = ProjectionResults[cluster==i,c('TempSlope'):=.(TModel)]
    }
}

ChangeFractions = cbind(ProjectionResults,ModelSlopes$Slope)
setnames(ChangeFractions,'V2','OriginalSlope')

ChangeFractions = ChangeFractions[,c('PrecipSlope','TempSlope'):=.(ifelse(is.na(PrecipSlope),0,PrecipSlope),ifelse(is.na(TempSlope),0,TempSlope))]
ChangeFractions = ChangeFractions[,c('PrecipRatio','TempRatio'):=.(PrecipSlope/(abs(PrecipSlope)+abs(TempSlope)),TempSlope/(abs(PrecipSlope)+abs(TempSlope)))]

ChangeFractions = ChangeFractions[,c('NormPRatio','NormTRatio'):=.(PrecipRatio/(abs(PrecipRatio)+abs(TempRatio)),TempRatio/(abs(PrecipRatio)+abs(TempRatio)))]
setorder(ChangeFractions,by='cluster')
write.csv(ChangeFractions,paste0(root,'\\Code_Exports\\ChangeFractions.csv'))

write.xlsx(ProjectionTable,paste0(root,'\\Code_Exports\\Modeled_RE_Slopes.xlsx'))

slope_plots = function(cluster_number) {
  RE = ggplot()+
    geom_point(data=ProjectionTable[cluster==cluster_number],aes(year,FullModel),color='black')+
    geom_smooth(data=ProjectionTable[cluster==cluster_number],aes(year,FullModel),color='gray50',method = 'lm',se=F)+
    geom_smooth(data=ProjectionTable[cluster==cluster_number & year>1960],aes(year,FullModel),color='red',method = 'lm',se=F)+
    geom_smooth(data=ProjectionTable[cluster==cluster_number & year>1970],aes(year,FullModel),color='orange',method = 'lm',se=F)+
    geom_smooth(data=ProjectionTable[cluster==cluster_number & year>1980],aes(year,FullModel),color='green',method = 'lm',se=F)+
    geom_smooth(data=ProjectionTable[cluster==cluster_number & year>1990],aes(year,FullModel),color='blue',method = 'lm',se=F)+
    geom_smooth(data=ProjectionTable[cluster==cluster_number & year>2000],aes(year,FullModel),color='purple',method = 'lm',se=F)+
    labs(x='Year',y='Runoff Efficiency Z-scores')+
    theme_bw()+theme(plot.title=element_text(hjust=0.5))
  
  Precip = ggplot()+
    geom_point(data=ProjectionTable[cluster==cluster_number],aes(year,PModelLogRunoff),color='royalblue')+
    geom_smooth(data=ProjectionTable[cluster==cluster_number],aes(year,PModelLogRunoff),color='royalblue',method = 'lm',se=F)+
    geom_smooth(data=ProjectionTable[cluster==cluster_number & year>1960],aes(year,PModelLogRunoff),color='black',method = 'lm',se=F)+
    geom_smooth(data=ProjectionTable[cluster==cluster_number & year>1970],aes(year,PModelLogRunoff),color='green',method = 'lm',se=F)+
    geom_smooth(data=ProjectionTable[cluster==cluster_number & year>1980],aes(year,PModelLogRunoff),color='orange',method = 'lm',se=F)+
    geom_smooth(data=ProjectionTable[cluster==cluster_number & year>1990],aes(year,PModelLogRunoff),color='purple',method = 'lm',se=F)+
    geom_smooth(data=ProjectionTable[cluster==cluster_number & year>2000],aes(year,PModelLogRunoff),color='navyblue',method = 'lm',se=F)+
    labs(x='Year',y='Precipitation Modeled R.E.')+
    theme_bw()
   
   Temp = ggplot()+ 
    geom_point(data=ProjectionTable[cluster==cluster_number],aes(year,TModelLogRunoff),color='red')+
    geom_smooth(data=ProjectionTable[cluster==cluster_number],aes(year,TModelLogRunoff),color='red',method = 'lm',se=F)+
    geom_smooth(data=ProjectionTable[cluster==cluster_number & year>1960],aes(year,TModelLogRunoff),color='black',method = 'lm',se=F)+
    geom_smooth(data=ProjectionTable[cluster==cluster_number & year>1970],aes(year,TModelLogRunoff),color='green',method = 'lm',se=F)+
    geom_smooth(data=ProjectionTable[cluster==cluster_number & year>1980],aes(year,TModelLogRunoff),color='orange',method = 'lm',se=F)+
    geom_smooth(data=ProjectionTable[cluster==cluster_number & year>1990],aes(year,TModelLogRunoff),color='purple',method = 'lm',se=F)+
    geom_smooth(data=ProjectionTable[cluster==cluster_number & year>2000],aes(year,TModelLogRunoff),color='navyblue',method = 'lm',se=F)+
    labs(x='Year',y='Temperature Modeled R.E.')+
    theme_bw()
   
    Output <<- RE / (Precip + Temp) + 
      theme(plot.title = element_text(hjust=0.5)) +
      plot_annotation(title = Group_Names[cluster_number])
    
    Output
}

lapply(1:16, slope_plots)

#Southwest Slope Plot
SW_RE = ggplot()+
  geom_point(data=ProjectionTable[cluster==10],aes(year,FullModel),color='black')+
  geom_smooth(data=ProjectionTable[cluster==10],aes(year,FullModel),color='gray50',method = 'lm',se=F)+
  geom_smooth(data=ProjectionTable[cluster==10 & year>1960],aes(year,FullModel),color='red',method = 'lm',se=F)+
  geom_smooth(data=ProjectionTable[cluster==10 & year>1970],aes(year,FullModel),color='orange',method = 'lm',se=F)+
  geom_smooth(data=ProjectionTable[cluster==10 & year>1980],aes(year,FullModel),color='green',method = 'lm',se=F)+
  geom_smooth(data=ProjectionTable[cluster==10 & year>1990],aes(year,FullModel),color='blue',method = 'lm',se=F)+
  geom_smooth(data=ProjectionTable[cluster==10 & year>2000],aes(year,FullModel),color='purple',method = 'lm',se=F)+
  labs(x='Year',y='Combined Model R.E.')+
  theme_bw()

SW_Precip = ggplot()+
  geom_point(data=ProjectionTable[cluster==10],aes(year,PModelLogRunoff),color='black')+
  geom_smooth(data=ProjectionTable[cluster==10],aes(year,PModelLogRunoff),color='gray50',method = 'lm',se=F)+
  geom_smooth(data=ProjectionTable[cluster==10 & year>1960],aes(year,PModelLogRunoff),color='red',method = 'lm',se=F)+
  geom_smooth(data=ProjectionTable[cluster==10 & year>1970],aes(year,PModelLogRunoff),color='orange',method = 'lm',se=F)+
  geom_smooth(data=ProjectionTable[cluster==10 & year>1980],aes(year,PModelLogRunoff),color='green',method = 'lm',se=F)+
  geom_smooth(data=ProjectionTable[cluster==10 & year>1990],aes(year,PModelLogRunoff),color='blue',method = 'lm',se=F)+
  geom_smooth(data=ProjectionTable[cluster==10 & year>2000],aes(year,PModelLogRunoff),color='purple',method = 'lm',se=F)+
  labs(x='Year',y='Precip. Model R.E.')+
  theme_bw()

SW_Temp = ggplot()+ 
  geom_point(data=ProjectionTable[cluster==10],aes(year,TModelLogRunoff),color='black')+
  geom_smooth(data=ProjectionTable[cluster==10],aes(year,TModelLogRunoff),color='gray50',method = 'lm',se=F)+
  geom_smooth(data=ProjectionTable[cluster==10 & year>1960],aes(year,TModelLogRunoff),color='red',method = 'lm',se=F)+
  geom_smooth(data=ProjectionTable[cluster==10 & year>1970],aes(year,TModelLogRunoff),color='orange',method = 'lm',se=F)+
  geom_smooth(data=ProjectionTable[cluster==10 & year>1980],aes(year,TModelLogRunoff),color='green',method = 'lm',se=F)+
  geom_smooth(data=ProjectionTable[cluster==10 & year>1990],aes(year,TModelLogRunoff),color='blue',method = 'lm',se=F)+
  geom_smooth(data=ProjectionTable[cluster==10 & year>2000],aes(year,TModelLogRunoff),color='purple',method = 'lm',se=F)+
  labs(x='Year',y='Temp. Model R.E.')+
  theme_bw()

# Map of Slopes
slope_year = function(start_year,InputTable){
  fit = lm(data=InputTable[year>start_year], FullModel ~ year)
  Pfit = lm(data=InputTable[year>start_year], PModelLogRunoff ~ year)
  Tfit = lm(data=InputTable[year>start_year], TModelLogRunoff ~ year)
  cf = coef(fit)
  Pcf = coef(Pfit)
  Tcf = coef(Tfit)
  slope = cf[2]
  Pslope = Pcf[2]
  Tslope = Tcf[2]
  
  slope_summary = data.table("Start_year"=start_year,'FullModel'=slope,'PrecipSlope'=Pslope,'TempSlope'=Tslope)
  slope_summary = slope_summary[,c('PrecipSlope','TempSlope'):=.(ifelse(is.na(PrecipSlope),0,PrecipSlope),ifelse(is.na(TempSlope),0,TempSlope))]
  slope_summary = slope_summary[,c('PrecipRatio','TempRatio'):=.(PrecipSlope/((PrecipSlope)+(TempSlope)),TempSlope/((PrecipSlope)+(TempSlope)))]
  
  slope_summary <<- slope_summary[,c('NormPRatio','NormTRatio'):=.(sign(FullModel)*(PrecipRatio/(abs(PrecipRatio)+abs(TempRatio))),sign(FullModel)*(TempRatio/(abs(PrecipRatio)+abs(TempRatio))))]
}

slopes = function(cluster_number) {
  InputTable = ProjectionTable[cluster==cluster_number]
  SlopeTable = lapply(seq(from=1950,to=2000,by=10), slope_year, InputTable=InputTable)
  SlopeTable = rbindlist(SlopeTable)
  SlopeTable <<- SlopeTable[,cluster:=cluster_number]
}

SlopeTable = lapply(1:16, slopes)
SlopeTable = rbindlist(SlopeTable)

Centroids = setDT(read.xlsx(paste0(root,'\\Input_Data\\ClusterCentroids_Shift.xlsx')))

GrobSample = ggplot()+
  #facet_wrap(vars(cluster),labeller = as_labeller(Group_Names))+
  geom_col(data=SlopeTable[cluster==10],aes(Start_year-1.5,NormPRatio/20,fill=abs(PrecipSlope)),width=3,color='grey70')+
  scale_fill_gradient(low = 'lightblue',high ='navyblue')+
  labs(fill='Precipitation Slope Magnitude')+
  new_scale_fill()+
  geom_col(data=SlopeTable[cluster==10],aes(Start_year+1.5,NormTRatio/20,fill=abs(TempSlope)),width=3,color='grey70')+
  scale_fill_gradient(low = 'orange',high='darkred')+
  geom_hline(yintercept = 0,color='gray70',linetype='dashed')+
  geom_point(data=SlopeTable[cluster==10],aes(Start_year,FullModel))+
  geom_line(data=SlopeTable[cluster==10],aes(Start_year,FullModel))+
  scale_y_continuous(limits = c(-0.05,0.05),sec.axis = sec_axis(~.*20,name='Fraction of Combined Model'))+
  labs(x='Start Year', y='Combined Model Slope',fill='Temperature Slope Magnitude')+
  theme_bw()+theme(legend.key.size = unit(10,'pt'))

Cluster_slopes <- function(cluster_number){
  long <- Centroids[cluster == cluster_number]$Cent_lon
  lat <- Centroids[cluster == cluster_number]$Cent_lat
  plot_sel <- SlopeTable[cluster == cluster_number]
  return(annotation_custom(grob = ggplotGrob(
    ggplot(plot_sel)+
      geom_hline(yintercept = 1/20,color='grey80')+
      geom_hline(yintercept = -1/20,color='grey80')+
      geom_col(aes(Start_year-1.5,NormPRatio/20,fill=abs(PrecipSlope)),width=3)+
      scale_fill_gradient(low = 'lightblue',high='navyblue')+
      labs(fill='Precipitation Slope',title = Group_Names[cluster_number])+
      guides(fill='none')+
      new_scale_fill()+
      geom_col(aes(Start_year+1.5,NormTRatio/20,fill=abs(TempSlope)),width=3)+
      scale_fill_gradient(low = 'orange',high='darkred')+
      geom_hline(yintercept = 0,color='gray70',linetype='dashed')+
      geom_point(aes(Start_year,FullModel))+
      geom_line(aes(Start_year,FullModel))+
      scale_y_continuous(limits = c(-0.05,0.05),sec.axis = sec_axis(~.*20,name='Fraction of Combined Model'))+
      labs(x='Start Year', y='Combined Model R.E. Trend',fill='Temperature Slope')+
      guides(fill='none')+
      theme_minimal()+
      theme(plot.title = element_text(size=8,hjust = 0.5),panel.grid = element_blank(),
            axis.title.x = element_blank(),axis.title.y = element_text(size=6),axis.ticks = element_blank(),
            axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),
            plot.subtitle = element_text(size=8,hjust = 0.5))
  ),
  xmin = long - 5.5, xmax = long + 5.5,
  ymin = lat - 4, ymax = lat + 4))
}

extract_legend <- function(GrobObject) {
  step1 <- ggplot_gtable(ggplot_build(GrobObject))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

MapSlope = function(){
  bar_sel = lapply(cluster_list,Cluster_slopes)
  
  Map = ggplot(data = NULL) +  
    geom_sf(data=ne_countries(country = 'United States of America',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") +
    geom_sf(data=ne_countries(country = 'Canada',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") + 
    coord_sf(xlim = c(-142, -50), ylim = c(24.5, 60), expand = FALSE)+
    annotation_scale(location = "bl", width_hint = 0.25) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(55, "pt"), pad_y = unit(25, "pt"),
                           style = north_arrow_nautical()) +
    labs(x='Longitude',y='Latitude')
  
  Combined = Map + bar_sel + inset_element(extract_legend(GrobSample),left = 0.8,bottom = 0.1,right = 0.98,top = 0.45)
  
  return(Combined)
}

MapSlope()


#Southwest
SW_Slope = ggplot()+
  geom_col(data=SlopeTable[cluster==10],aes(Start_year-1.5,NormPRatio/20,fill=abs(PrecipSlope)),width=3,color='grey70')+
  scale_fill_gradient(low = 'lightblue',high ='navyblue')+
  labs(fill='Precipitation Slope Magnitude')+
  #guides(fill='none')+
  new_scale_fill()+
  geom_col(data=SlopeTable[cluster==10],aes(Start_year+1.5,NormTRatio/20,fill=abs(TempSlope)),width=3,color='grey70')+
  scale_fill_gradient(low = 'orange',high='darkred')+
  geom_hline(yintercept = 0,color='gray70',linetype='dashed')+
  geom_point(data=SlopeTable[cluster==10],aes(Start_year,FullModel))+
  geom_line(data=SlopeTable[cluster==10],aes(Start_year,FullModel))+
  scale_y_continuous(limits = c(-0.05,0.05),sec.axis = sec_axis(~.*20,name='Fraction of Combined Model'))+
  #guides(fill='none')+
  labs(x='Start Year', y='Combined Model R.E. Trend',fill='Temperature Slope Magnitude')+
  theme_bw()+theme(legend.key.size = unit(10,'pt'))

Southwest_Case = (SW_RE + (SW_Precip / SW_Temp)) / SW_Slope +
  plot_layout(guides='keep')+
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size=20))
  

# Relative Forcing
PrecipAverages = All_Data[,SiteMeanPrecip:=mean(Precip_m),by='site_number']
PrecipAverages = PrecipAverages[,ClusterMeanPrecip:=mean(Precip_m),by='cluster']

PrecipAverages = unique(PrecipAverages[,c('site_number','cluster','SiteMeanPrecip','ClusterMeanPrecip')],by=c('site_number','cluster'))
write.xlsx(PrecipAverages,paste0(root,'\\Code_Exports\\PrecipAverages.xlsx'))

print(ChangeFractions[,NormTRatio])
RF_Data = setDT(read.xlsx(paste0(root,'\\Input_Data\\RF_Data.xlsx')))

GrobSample = ggplot(RF_Data[cluster==10],aes(Variable,RelativeForcing))+
  geom_col(aes(fill=Variable),color='black',width=0.5)+
  geom_hline(yintercept = 0, linetype='dotted')+
  annotate('text',x=1.5,y=0,label='Temperature')+
  coord_flip()+
  labs(fill='Variable')+
  scale_fill_manual(values = c('- Temp.'='darkred','+ Temp.'='red','+ Precip.'='blue','- Precip.'='darkblue'))+
  #scale_fill_manual(values = c('Negative'='#93003a','Positive'='#00429d'))+
  theme_bw()

Cluster_rf <- function(cluster_number){
  long <- Centroids[cluster == cluster_number]$Cent_lon
  lat <- Centroids[cluster == cluster_number]$Cent_lat
  plot_sel <- RF_Data[cluster == cluster_number]
  return(annotation_custom(grob = ggplotGrob(
    ggplot(plot_sel,aes(Variable,RelativeForcing))+
      geom_hline(yintercept = 0,color='grey60')+
      geom_hline(yintercept = 1,color='grey80')+
      geom_hline(yintercept = -1,color='grey80')+
      geom_col(aes(fill=Color_Code),color='black',width=0.5)+
      scale_y_continuous(limits = c(-1.5,1.5))+
      guides(fill='none')+
      #annotate('text',x=1.5,y=0,label='Precipitation',size=3,fontface=2)+
      #annotate('text',x=2.5,y=0,label='Temperature',size=3,fontface=2)+
      labs(title = Group_Names[cluster_number],fill='Variable', y='Fraction of Change',
           subtitle = paste(paste(paste('R.E. \u03BC=',format(round(RunoffStats[cluster_number,RunoffMean],2),nsmall=2),sep = ' '),paste('\u00B1',format(round(RunoffStats[cluster_number,RunoffSD],2),nsmall=2),sep=' '),sep = ' '),paste('R\U00B2 =',paste(format(round(StepwiseResults[cluster==cluster_number,r.squared],2),nsmall=2))),sep='  '))+
      scale_fill_manual(values = c('- Temp.'='darkred','+ Temp.'='red','+ Precip.'='blue','- Precip.'='darkblue'))+
      theme_minimal()+
      theme(plot.title = element_text(size=10,hjust = 0.5),panel.grid = element_blank(),
            axis.title.x = element_blank(),axis.title.y = element_text(size=6),axis.ticks = element_blank(),axis.text.x = element_blank(),
            plot.subtitle = element_text(size=8,hjust = 0.5))
  ),
  xmin = long - 4, xmax = long + 4,
  ymin = lat - 3.5, ymax = lat + 3.5))
}

extract_legend <- function(GrobObject) {
  step1 <- ggplot_gtable(ggplot_build(GrobObject))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

MapRF = function(){
  bar_sel = lapply(cluster_list,Cluster_rf)
  
  Map = ggplot(data = NULL) +  
    geom_sf(data=ne_countries(country = 'United States of America',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") +
    geom_sf(data=ne_countries(country = 'Canada',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") + 
    coord_sf(xlim = c(-142, -50), ylim = c(24.5, 60), expand = FALSE)+
    annotation_scale(location = "bl", width_hint = 0.25) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(55, "pt"), pad_y = unit(25, "pt"),
                           style = north_arrow_nautical()) +
    labs(x='Longitude',y='Latitude')
  
  Combined = Map + bar_sel + inset_element(extract_legend(GrobSample),left = 0.9,bottom = 0.1,right = 0.98,top = 0.3)
  
  return(Combined)
}

MapRF()

# Rose Diagram
Centroids = setDT(read.xlsx(paste0(root,'\\Input_Data\\Rose_ClusterCentroids.xlsx')))

Rose_Data = setDT(read.xlsx(paste0(root,'\\Input_Data\\Rose_Data.xlsx')))
Rose_Data = Rose_Data[,logCoeff:=(log10(AbsCoefficient))]
Rose_Data = Rose_Data[,logCoeff:=ifelse(logCoeff<(-9999),0,logCoeff)]

Rose_Data = Rose_Data[,Scaled:=(logCoeff+2)]
Rose_Data = Rose_Data[,Scaled:=ifelse(Scaled==2,0,Scaled)]

SampleData=copy(Rose_Data)
SampleData = SampleData[,Scaled:=0]

Rose_Scale  = ggplot(SampleData[cluster==5],aes(fct_reorder(Variable,Order),Scaled/2))+
  geom_col(aes(fill=Sign,alpha=Significance),color='black',width=1)+
  geom_hline(yintercept = 2,linetype='dashed')+
  geom_hline(yintercept = 1, linetype='dashed')+
  guides(color='none',fill='none',alpha='none')+
  coord_curvedpolar(clip='off')+
  scale_y_continuous(limits = c(0,2), breaks=0)+
  scale_x_discrete(labels=c('Temp.','Prior Temp.','Prior Precip.','Precip.'))+
  theme_minimal()+ theme(plot.title = element_text(size=10,hjust = 0.5,vjust = 0.5),
                         axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),
                         panel.grid.major = element_blank(),
                         axis.text.x = element_text(vjust=0,size=35),plot.subtitle = element_text(size=8))


GrobSample = ggplot(Rose_Data[cluster==5],aes(fct_reorder(Variable,Order),Scaled/2))+
  geom_col(aes(fill=Sign,alpha=Significance),color='black',width=1)+
  geom_hline(yintercept = 0.5,linetype='dashed')+
  geom_hline(yintercept = 1, linetype='dashed')+
  #geom_hline(yintercept = 2, linetype='dashed')+
  #guides(color='none',fill='none',alpha='none')+
  coord_curvedpolar(clip='off')+
  labs(fill='Relationship')+
  scale_fill_manual(values = c('Inverse'='red','Positive'='royalblue'))+
  scale_alpha_manual(values = c('Marginally'=0.3,'Significant'=1))+
  scale_y_continuous(limits = c(0,1), breaks=0)+
  scale_x_discrete(labels=c('Temp.','Prior Temp.','Prior Precip.','Precip.'))+
  theme_minimal()+ theme(plot.title = element_text(size=10,hjust = 0.5,vjust = 0.5),
                         axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),
                         panel.grid.major = element_blank(),
                         axis.text.x = element_text(vjust=-1,size=20),plot.subtitle = element_text(size=8,hjust=0.5,margin = margin(0,0,12,0)))

Cluster_rose <- function(cluster_number){
  long <- Centroids[cluster == cluster_number]$Cent_lon
  lat <- Centroids[cluster == cluster_number]$Cent_lat
  plot_sel <- Rose_Data[cluster == cluster_number]
  return(annotation_custom(grob = ggplotGrob(
    ggplot(plot_sel,aes(fct_reorder(Variable,Order),Scaled))+
      geom_col(aes(fill=Sign,alpha=Significance),color='black',width=1)+
      coord_curvedpolar(clip='off')+
      guides(fill='none',alpha='none')+
      labs(title = Group_Names[cluster_number],fill='Trend',
           subtitle = paste(paste(paste('R.E. \u03BC=',format(round(RunoffStats[cluster_number,RunoffMean],2),nsmall=2),sep = ' '),paste('\u00B1',format(round(RunoffStats[cluster_number,RunoffSD],2),nsmall=2),sep=' '),sep = ' '),paste('R\U00B2 =',paste(format(round(StepwiseResults[cluster==cluster_number,r.squared],2),nsmall=2))),sep='  '))+
      scale_fill_manual(values = c('Inverse'='red','Positive'='royalblue'))+
      scale_alpha_manual(values = c('Marginally'=0.3,'Significant'=1))+
      scale_y_continuous(limits = c(0,2),breaks = 0)+
      scale_x_discrete(labels=c('Temp.','Prior Temp.','Prior Preip.','Precip.'))+
      geom_hline(yintercept = 2,linetype='dashed')+
      geom_hline(yintercept = 1, linetype='dashed')+
      theme_minimal()+
      theme(plot.title = element_text(size=10,hjust = 0.5,vjust = 0.5),panel.grid = element_blank(),
            axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),
            panel.grid.major = element_blank(),
            axis.text.x = element_text(vjust=-1),plot.subtitle = element_text(size=8,hjust=0.5,margin = margin(0,0,12,0)))
  ),
  xmin = long - 4.5, xmax = long + 4.5,
  ymin = lat - 3.5, ymax = lat + 3.5))
}

extract_legend <- function(GrobObject) {
  step1 <- ggplot_gtable(ggplot_build(GrobObject))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

MapRose = function(){
  bar_sel = lapply(cluster_list,Cluster_rose)
  
  Map = ggplot(data = NULL) +  
    geom_sf(data=ne_countries(country = 'United States of America',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") +
    geom_sf(data=ne_countries(country = 'Canada',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") + 
    coord_sf(xlim = c(-142, -50), ylim = c(24.5, 60), expand = FALSE)+
    annotation_scale(location = "bl", width_hint = 0.25) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(55, "pt"), pad_y = unit(25, "pt"),
                           style = north_arrow_nautical()) +
    labs(x='Longitude',y='Latitude')
  
  Combined = Map + bar_sel + inset_element(extract_legend(GrobSample),left = 0.9,bottom = 0.1,right = 0.98,top = 0.4)
  
  return(Combined)
}

MapRose()
ggsave('Rose_Diagrams.pdf',MapRose(), width = 14.25, height = 8.85, units = 'in')

#### Contribution Trends ####
RegressionCoeff = StepwiseResults[,c('cluster','intercept','Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff')]
RegressionCoeff = RegressionCoeff[,c('Precip_coeff','OffPrecip_coeff','Temp_coeff','OffTemp_coeff'):=.(ifelse(is.na(Precip_coeff),0,Precip_coeff),ifelse(is.na(OffPrecip_coeff),0,OffPrecip_coeff),ifelse(is.na(Temp_coeff),0,Temp_coeff),ifelse(is.na(OffTemp_coeff),0,OffTemp_coeff))]

Zsub = Zscores[site_number%in%RecordLength$site_number]
ModelData = ZscoreSummary[site_number%in%RecordLength$site_number,c('site_number','cluster','year','zLogRunoff','zPrecip','zOffPrecip','zTemp','zOffTemp')]
for (i in as.numeric(unique(All_Data$cluster))) {
  Stats = setorder(Zsub[,c('meanLogRunoff','sdLogRunoff'):=.(mean(LogRunoff,na.rm=T),sd(LogRunoff,na.rm=T)),by='site_number'],by='site_number')
  
  ModelData = ModelData[cluster==i,Full:=(RegressionCoeff[cluster==i,intercept] + (zPrecip*RegressionCoeff[cluster==i,Precip_coeff]) + (zOffPrecip*RegressionCoeff[cluster==i,OffPrecip_coeff]) + (zTemp*RegressionCoeff[cluster==i,Temp_coeff]) + (zOffTemp*RegressionCoeff[cluster==i,OffTemp_coeff]))]
  ModelData = ModelData[cluster==i,InterceptModel:=(RegressionCoeff[cluster==i,intercept])]
  ModelData = ModelData[cluster==i,PrecipPartial:=(RegressionCoeff[cluster==i,intercept] + (zPrecip*RegressionCoeff[cluster==i,Precip_coeff]) + (zOffPrecip*RegressionCoeff[cluster==i,OffPrecip_coeff]))]
  ModelData = setorder(ModelData[cluster==i,TempPartial:=(RegressionCoeff[cluster==i,intercept] + (zTemp*RegressionCoeff[cluster==i,Temp_coeff]) + (zOffTemp*RegressionCoeff[cluster==i,OffTemp_coeff]))],by='site_number')
  
  ModelResults = ModelData[cluster==i,c('Observed','Full','InterceptModel','PrecipPartial','TempPartial'):=
                             .((zLogRunoff*Stats[cluster==i,sdLogRunoff]+Stats[cluster==i,meanLogRunoff]),(Full*Stats[cluster==i,sdLogRunoff]+Stats[cluster==i,meanLogRunoff]),(InterceptModel*Stats[cluster==i,sdLogRunoff]+Stats[cluster==i,meanLogRunoff]),(PrecipPartial*Stats[cluster==i,sdLogRunoff]+Stats[cluster==i,meanLogRunoff]),(TempPartial*Stats[cluster==i,sdLogRunoff]+Stats[cluster==i,meanLogRunoff]))]
}

ModelResults = ModelResults[,c('ObservedAv','FullAv','InterceptAv','PrecipAv','TempAv'):=.(mean(Observed),mean(Full),mean(InterceptModel),mean(PrecipPartial),mean(TempPartial)),by=c('cluster','year')]
ModelResults = unique(ModelResults[,c('cluster','year','ObservedAv','FullAv','InterceptAv','PrecipAv','TempAv')],by=c('cluster','year'))

ModelResults = ModelResults[,Deviation:=(FullAv - InterceptAv)]
ModelResults = ModelResults[,PrecipModel:=(PrecipAv - InterceptAv)]
ModelResults = ModelResults[,TempModel:=(TempAv - InterceptAv)]
ModelResults = ModelResults[,Observed:=(ObservedAv-InterceptAv)]

# 5 year average
SmoothedResults = setorder(ModelResults[,c('SmoothDeviation','SmoothPrecip','SmoothTemp','SmoothObserved'):=.(frollmean(Deviation,5,align = 'center'),frollmean(PrecipModel,5,align = 'center'),frollmean(TempModel,5,align = 'center'),frollmean(Observed,5,align = 'center')),by='cluster'],by=cluster)


# Plot Model
GrobSample = ggplot(SmoothedResults[cluster==8])+
  geom_area(aes(year,SmoothPrecip),color='blue',fill='royalblue',alpha=0.5)+
  geom_area(aes(year,SmoothTemp),color='red',fill='red',alpha=0.5)+
  geom_line(aes(year,SmoothObserved),linetype='dashed')+
  geom_line(aes(year,SmoothDeviation),color='black',size=1)+
  labs(x='Year',y='Runoff Efficiency Z-score')+
  theme_bw()

Centroids = setDT(read.xlsx(paste0(root,'\\Input_Data\\ClusterCentroids_Shift.xlsx')))

Cluster_model <- function(cluster_number){
  long <- Centroids[cluster == cluster_number]$Cent_lon
  lat <- Centroids[cluster == cluster_number]$Cent_lat
  plot_sel <- SmoothedResults[cluster == cluster_number]
  return(annotation_custom(grob = ggplotGrob(
    ggplot(plot_sel)+
      geom_area(aes(year,SmoothPrecip),color='blue',fill='royalblue',alpha=0.5)+
      geom_area(aes(year,SmoothTemp),color='red',fill='red',alpha=0.5)+
      geom_line(aes(year,SmoothObserved),color='gray')+
      geom_line(aes(year,SmoothDeviation),color='black',size=0.5)+
      #scale_y_continuous(limits = c(-0.35,0.35))+
      guides(fill='none')+
      labs(title = Group_Names[cluster_number],y='R.E. Z-score',x='Year')+
           #subtitle = paste(paste('R.E. \u03BC=',format(round(RunoffStats[cluster_number,RunoffMean],2),nsmall=2),sep = ' '),paste('\u03c3',format(round(RunoffStats[cluster_number,RunoffSD],2),nsmall=2),sep='='),sep = '   '))+
      theme_minimal()+
      theme(plot.title = element_text(size=9,hjust = 0.5),axis.title = element_text(size = 7),
            axis.text = element_text(size=6,vjust = 0.5),plot.subtitle = element_text(size=8,hjust = 0.5))
  ),
  xmin = long - 5.5, xmax = long + 5.5,
  ymin = lat - 4, ymax = lat + 4))
}

MapModel = function(){
  bar_sel = lapply(cluster_list,Cluster_model)
  
  Map = ggplot(data = NULL) +  
    geom_sf(data=ne_countries(country = 'United States of America',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") +
    geom_sf(data=ne_countries(country = 'Canada',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") + 
    coord_sf(xlim = c(-142, -50), ylim = c(24.5, 60), expand = FALSE)+
    annotation_scale(location = "bl", width_hint = 0.25) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(55, "pt"), pad_y = unit(25, "pt"),
                           style = north_arrow_nautical()) +
    labs(x='Longitude',y='Latitude')
  
  Combined = Map + bar_sel
  
  return(Combined)
}

MapModel()

#### Groundwater ####
# Download HCDN Discharge
MonthlyDischargeData = data.table()
HCDN_Discharge = function(site_number) {
  df = setDT(readNWISdata(siteNumbers=site_number,parameterCd = '00060',service='stat',statReportType='monthly'))
  MonthlyDischargeData <<- df[,c('site_no','year_nu','month_nu','mean_va')]
}

MonthlyDischargeData = lapply(HCDN_sites[site_number!=13010065,site_number], HCDN_Discharge)
MonthlyDischargeData = rbindlist(MonthlyDischargeData)

MonthlyDischargeData = MonthlyDischargeData[,mean_Qcms:=(mean_va*0.028316846592)]
MonthlyDischargeData = na.omit(MonthlyDischargeData[mean_Qcms>0])

# Download WSC Discharge Data 
MonthlyCanadaDischarge = data.table()
for (i in WSC_sites$site_number){
  download <- data.table(hy_daily_flows(i))[
    ,':='(
      site_number = STATION_NUMBER,
      Date = ymd(Date),
      Month = month(Date),
      Q_cms = Value, # WSC discharge already in cms units
      water_year = ifelse(month(Date) > 9, as.numeric(year(Date) + 1), as.numeric(year(Date)))
    )
  ][,.(site_number, Date,Month,water_year,Q_cms)]
  
  MonthlyCanadaDischarge = rbind(MonthlyCanadaDischarge,download)
}

monthly_flow = function(month_number) {
  HCDN_month = MonthlyDischargeData[month_nu==month_number]
  HCDN_month = HCDN_month[,water_year:=ifelse(month_nu > 9, as.numeric(year_nu + 1), as.numeric(year_nu))]
  HCDN_month = HCDN_month[,mean_va:=mean_va*0.028316846592]
  setnames(HCDN_month,'site_no','site_number')
  WSC_month = MonthlyCanadaDischarge[Month==month_number]
  WSC_month = unique(WSC_month[,mean_va:=mean(Q_cms),by=c('site_number','water_year')],by=c('site_number','water_year'))
  WSC_month = WSC_month[,month:=month_number]
  
  month_Flow = rbind(HCDN_month[,c('site_number','water_year','mean_va')],WSC_month[,c('site_number','water_year','mean_va')])
  setnames(month_Flow,'mean_va','meanQ_cms')
  month_Flow = merge(month_Flow,All_Sites[,c('site_number','cluster')],by='site_number')
  month_Flow = month_Flow[,monthQ_cms:=log10(meanQ_cms)]
  month_Flow = month_Flow[,month:=month_number]
  setnames(month_Flow,'water_year','year')
  
  month_Flow = month_Flow[meanQ_cms>0]
  
  month_Flow = merge(All_Data[,c('site_number','cluster','year')],month_Flow[,c('site_number','monthQ_cms','year','month')],by=c('site_number','year'))
}

monthly_data = lapply(1:12,monthly_flow)
monthly_data = rbindlist(monthly_data)

# monthZsummary = data.table()
# for (n in unique(monthly_data$cluster)) {
#   #Statistics Table
#   Stats = monthly_data[cluster==n]
#   Stats = Stats[,c('meanQ','meanPrecip','meanOffPrecip','sdQ','sdPrecip','sdOffPrecip'):=
#                   .(mean(monthQ_cms,na.rm=T),mean(Precip_m,na.rm=T),mean(OffsetPrecip,na.rm=T),sd(monthQ_cms,na.rm=T)
#                     ,sd(Precip_m,na.rm=T),sd(OffsetPrecip,na.rm=T)),by=c('site_number','year')]
#   Stats = unique(Stats[,c('site_number','cluster','year','meanQ','meanPrecip','meanOffPrecip','sdQ','sdPrecip','sdOffPrecip')],by=c('site_number','year'))
#   
#   # Calculate Z-scores
#   for (s in unique(monthly_data$year)) {
#     monthZ = monthly_data[year==s & cluster==n]
#     for (i in unique(monthZ$site_number)) {
#       monthZ = monthZ[,c('zQ'):=
#                           .((monthQ_cms-Stats[site_number==i & year==s,meanQ])/Stats[site_number==i & year==s,sdQ])]
#       Z = monthZ[site_number==i & year==s,c('site_number','cluster','year','month','zQ')]
#       monthZsummary = rbind(monthZsummary,Z)
#     }
#   }
# }
# 
# monthly_data = merge(monthly_data,monthZsummary[,c('site_number','cluster','zQ','month','year')],by=c('site_number','cluster','month','year'))

# monthly_regression = function(month_number,cluster_number) {
#   dataset = monthly_data[cluster==cluster_number]
#   Regress0 = (lm(data=dataset[month==month_number], zQ ~ Precip_m + OffsetPrecip))
#   Regress = setDT(tidy(Regress0))
#   MonthlyRegress = data.table()
#   MonthlyRegress = MonthlyRegress[,c('month','cluster','r.squared','intercept','Precip_pvalue','OffPrecip_pvalue','Precip_coeff','OffPrecip_coeff'):=
#               .(month_number,cluster_number,summary(Regress0)$adj.r.squared,Regress[1,estimate],Regress[2,p.value],Regress[3,p.value],Regress[2,estimate],Regress[3,estimate])]
# }
# 
# Month_CoeffResults = data.table()
# for (cluster_number in 1:16) {
#   dataset = monthly_data[cluster==cluster_number]
#   Month_Coefficients = lapply(1:12, monthly_regression,cluster_number=cluster_number)
#   Month_Coefficients = rbindlist(Month_Coefficients)
#   Month_CoeffResults = rbind(Month_CoeffResults,Month_Coefficients)
# }
# 
# ggplot(Month_CoeffResults,aes(month,Precip_coeff))+
#   geom_point()+
#   scale_x_continuous(breaks = 1:12)+
#   geom_hline(yintercept = 0,linetype='dashed')+
#   facet_wrap(vars(cluster),scales='free_y',labeller = as_labeller(Group_Names))+
#   labs(x='Month',y='Regression Coefficient',title = 'Current Year Precipitation')+
#   theme_bw()+
#   theme(panel.grid.minor.x = element_blank())
# 
# ggplot(Month_CoeffResults,aes(month,OffPrecip_coeff))+
#   geom_point()+
#   scale_x_continuous(breaks = 1:12)+
#   geom_hline(yintercept = 0,linetype='dashed')+
#   facet_wrap(vars(cluster),scales='free_y',labeller = as_labeller(Group_Names))+
#   labs(x='Month',y='Regression Coefficient',title = 'Previous Year Precipitation')+
#   theme_bw()+
#   theme(panel.grid.minor.x = element_blank())

#### Seasonal ####

monthly_data = na.omit(monthly_data[monthQ_cms>0])
for (i in 1:12) {
  if (i %in% c(12,1,2)) {monthly_data = monthly_data[month==i,Season:='DJF']}
  if (i %in% c(3,4,5)) {monthly_data = monthly_data[month==i,Season:='MAM']}
  if (i %in% c(6,7,8)) {monthly_data = monthly_data[month==i,Season:='JJA']}
  if (i %in% c(9,10,11)) {monthly_data = monthly_data[month==i,Season:='SON']}
}

#WSC
WSC_Watersheds = readRDS(paste0(root,'\\ERA5\\Input\\WSC_NARR_Precip.RDS'))
WSC_temp = readRDS(paste0(root,'\\ERA5\\Input\\WSC_NARR_Temp.RDS'))
WSC_site = WSC_Watersheds$site_number

PrecipData = WSC_Watersheds[,11:514]
WSC_precip = data.table()
for (i in 0:41) {
  Subset = PrecipData[,(1+(12*i)):(12+(12*i))]
  SubsetA = Subset[,c(2,7,9,12)]*30
  SubsetB = Subset[,c(1,3,4,6,8,10,11)]*31
  SubsetC = Subset[,c(5)]*28
  Subset = cbind(SubsetA,SubsetB,SubsetC)
  WSC_precip = cbind(WSC_precip,Subset)
}

WSC_precip = cbind(WSC_site,WSC_precip)

WSC_temp = WSC_temp[,c(1,11:514)]

molten_WSC_precip = melt(WSC_precip, id=c('WSC_site'))
molten_WSC_precip = molten_WSC_precip[,Date:=(gsub('X','',variable))]
molten_WSC_precip = molten_WSC_precip[,Date:=strptime(Date,'%Y.%m.%d')]
molten_WSC_precip = molten_WSC_precip[,c('month','calYear'):=.(month(Date),year(Date))]
molten_WSC_precip = molten_WSC_precip[,year:=ifelse(month > 9, as.numeric(calYear + 1), as.numeric(calYear))]

molten_WSC_temp = melt(WSC_temp, id=c('site_number'))
molten_WSC_temp = molten_WSC_temp[,Date:=(gsub('X','',variable))]
molten_WSC_temp = molten_WSC_temp[,Date:=strptime(Date,'%Y.%m.%d')]
molten_WSC_temp = molten_WSC_temp[,c('month','calYear'):=.(month(Date),year(Date))]
setnames(molten_WSC_temp,'value','Temp_K')

MonthlyCanadaDischarge = setnames(MonthlyCanadaDischarge,'water_year','year')
MonthlyCanadaDischarge = setnames(MonthlyCanadaDischarge,'Month','month')
MonthlyCanadaDischarge = unique(MonthlyCanadaDischarge[,mean_Qcms:=mean(Q_cms,na.rm=T),by=c('year','month','site_number')],by=c('year','month','site_number'))

for (i in 1:12) {
  if (i %in% c(12,1,2)) {molten_WSC_precip = molten_WSC_precip[month==i,Season:='DJF']}
  if (i %in% c(3,4,5)) {molten_WSC_precip = molten_WSC_precip[month==i,Season:='MAM']}
  if (i %in% c(6,7,8)) {molten_WSC_precip = molten_WSC_precip[month==i,Season:='JJA']}
  if (i %in% c(9,10,11)) {molten_WSC_precip = molten_WSC_precip[month==i,Season:='SON']}
}

WSCmeltedSeasons = molten_WSC_precip
WSCmeltedSeasons = setnames(WSCmeltedSeasons,'WSC_site','site_number')
WSCmeltedSeasons = WSCmeltedSeasons[,Precip_m:=value/1000]
WSCmeltedSeasons = merge(WSCmeltedSeasons,molten_WSC_temp[,c('site_number','month','Temp_K','calYear')],by=c('site_number','calYear','month'))

# Merge Discharge and Precipitation Data
WSCSeasonRatios = merge(MonthlyCanadaDischarge,WSCmeltedSeasons[,c('site_number','year','calYear','month','Season','Precip_m','Temp_K')],by=c('site_number','year','month'))
WSCSeasonRatios = merge(WSCSeasonRatios,WSC_Areas[,c('site_number','GEEArea')],by='site_number')
WSCSeasonRatios = merge(WSCSeasonRatios, HydroRegions[,c('site_number','cluster')], by='site_number')
WSCSeasonRatios = WSCSeasonRatios[,c('site_number','year','calYear','month','Season','mean_Qcms','Precip_m','Temp_K','GEEArea','cluster')]

#HCDN
HCDN_Monthly = fread(paste0(root,'\\Input_Data\\hcdnSeasonalPT.csv'))
HCDN_Monthly_precip = HCDN_Monthly[,c(2:13,26:27)]
HCDN_Monthly_precip = HCDN_Monthly_precip[,site_number:=gsub('USGS-','',shedID)]
HCDN_site = HCDN_Monthly_precip$site_number

HCDN_Monthly_temp = HCDN_Monthly[,c(14:27)]
HCDN_Monthly_temp = HCDN_Monthly_temp[,site_number:=gsub('USGS-','',shedID)]
colnames(HCDN_Monthly_temp)[1:12] = c(1:12)

melt_HCDNprecip = melt(HCDN_Monthly_precip,id=c('site_number','calYear','shedID'),variable.name = 'month')
melt_HCDNprecip = melt_HCDNprecip[,Precip_m:=value/1000]

melt_HCDNtemp = melt(HCDN_Monthly_temp,id=c('site_number','calYear','shedID'),variable.name = 'month')
setnames(melt_HCDNtemp,'value','Temp_K')
melt_HCDNtemp = melt_HCDNtemp[,month:=as.numeric(month)]
melt_HCDNtemp = melt_HCDNtemp[,Temp_K:=Temp_K+273.15]

for (i in 1:12) {
  if (i %in% c(12,1,2)) {melt_HCDNprecip = melt_HCDNprecip[month==i,Season:='DJF']}
  if (i %in% c(3,4,5)) {melt_HCDNprecip = melt_HCDNprecip[month==i,Season:='MAM']}
  if (i %in% c(6,7,8)) {melt_HCDNprecip = melt_HCDNprecip[month==i,Season:='JJA']}
  if (i %in% c(9,10,11)) {melt_HCDNprecip = melt_HCDNprecip[month==i,Season:='SON']}
}

melt_HCDNprecip = melt_HCDNprecip[,year:=ifelse(as.numeric(month) > 9, as.numeric(calYear + 1), as.numeric(calYear))]

meltedSeasons = melt_HCDNprecip[,c('site_number','year','calYear','month','Precip_m','Season')]
meltedSeasons = meltedSeasons[,month:=as.numeric(month)]
meltedSeasons = merge(meltedSeasons,melt_HCDNtemp[,c('site_number','calYear','month','Temp_K')],by=c('site_number','calYear','month'))

# Merge Discharge and Precipitation Data
MonthlyDischargeData = MonthlyDischargeData[,year:=ifelse(as.numeric(month_nu) > 9, as.numeric(year_nu + 1), as.numeric(year_nu))]
MonthlyDischargeData = MonthlyDischargeData[,month:=as.numeric(month_nu)]
MonthlyDischargeData = setnames(MonthlyDischargeData,'site_no','site_number')
MonthlyDischargeData = MonthlyDischargeData[,c('site_number','month','year','mean_Qcms')]

for (i in 1:12) {
  if (i %in% c(12,1,2)) {MonthlyDischargeData = MonthlyDischargeData[month==i,Season:='DJF']}
  if (i %in% c(3,4,5)) {MonthlyDischargeData = MonthlyDischargeData[month==i,Season:='MAM']}
  if (i %in% c(6,7,8)) {MonthlyDischargeData = MonthlyDischargeData[month==i,Season:='JJA']}
  if (i %in% c(9,10,11)) {MonthlyDischargeData = MonthlyDischargeData[month==i,Season:='SON']}
}

SeasonRatios = merge(MonthlyDischargeData,meltedSeasons,by=c('site_number','year','month','Season'))
SeasonRatios = merge(SeasonRatios,GEEArea[,c('site_number','GEEArea')],by='site_number')
SeasonRatios = merge(SeasonRatios, HydroRegions[,c('site_number','cluster')], by='site_number')

# Merge WSC & HCDN
SeasonRatios = rbind(SeasonRatios,WSCSeasonRatios)
SeasonRatios = SeasonRatios[Precip_m>0 & mean_Qcms>0]

# Log Transform Season Ratios
SeasonRatios = SeasonRatios[,MonthRunoff:=(((mean_Qcms*2628002.88)/GEEArea)/(Precip_m))]
SeasonRatios = SeasonRatios[month%in%c(1,2,3,4,5,6,7,8,12),SeasonRunoff:=log10(mean(MonthRunoff)),by=c('site_number','year','Season')]
SeasonRatios = SeasonRatios[month%in%c(9,10,11),SeasonRunoff:=log10(mean(MonthRunoff)),by=c('site_number','calYear','Season')]
SeasonRatios = SeasonRatios[month%in%c(1,2,3,4,5,6,7,8,12),SeasonQ:=log10(mean(mean_Qcms)),by=c('site_number','year','Season')]
SeasonRatios = SeasonRatios[month%in%c(9,10,11),SeasonQ:=log10(mean(mean_Qcms)),by=c('site_number','calYear','Season')]
SeasonRatios = SeasonRatios[month%in%c(1,2,3,4,5,6,7,8,12),SeasonTemp:=log10(mean(Temp_K)),by=c('site_number','year','Season')]
SeasonRatios = SeasonRatios[month%in%c(9,10,11),SeasonTemp:=log10(mean(Temp_K)),by=c('site_number','calYear','Season')]

SeasonRatios = SeasonRatios[month%in%c(9,10,11),Fall_precip:=sum(Precip_m),by=c('calYear','site_number')]
SeasonRatios = SeasonRatios[month%in%c(9,10,11),Fall_precip:=mean(Fall_precip),by=c('year','site_number')]
Fall_Precip = unique(SeasonRatios[month==10,c('Fall_precip','year','site_number')],by=c('site_number','year'))
Fall_Precip = setnames(Fall_Precip,'Fall_precip','Fall_Precip')

SeasonRatios = SeasonRatios[month%in%c(12,1,2),Winter_precip:=sum(Precip_m),by=c('year','site_number')]
SeasonRatios = SeasonRatios[month%in%c(12,1,2),Winter_precip:=mean(Winter_precip),by=c('year','site_number')]
Winter_Precip = unique(SeasonRatios[month==1,c('Winter_precip','year','site_number')],by=c('site_number','year'))
Winter_Precip = setnames(Winter_Precip,'Winter_precip','Winter_Precip')

SeasonRatios = SeasonRatios[month%in%c(3,4,5),Spring_precip:=sum(Precip_m),by=c('year','site_number')]
SeasonRatios = SeasonRatios[month%in%c(3,4,5),Spring_precip:=mean(Spring_precip),by=c('year','site_number')]
Spring_Precip = unique(SeasonRatios[month==4,c('Spring_precip','year','site_number')],by=c('site_number','year'))
Spring_Precip = setnames(Spring_Precip,'Spring_precip','Spring_Precip')

SeasonRatios = SeasonRatios[month%in%c(6,7,8),Summer_precip:=sum(Precip_m),by=c('year','site_number')]
SeasonRatios = SeasonRatios[month%in%c(6,7,8),Summer_precip:=mean(Summer_precip),by=c('year','site_number')]
Summer_Precip = unique(SeasonRatios[month==7,c('Summer_precip','year','site_number')],by=c('site_number','year'))
Summer_Precip = setnames(Summer_Precip,'Summer_precip','Summer_Precip')

SeasonRatios = merge(SeasonRatios,Fall_Precip,by=c('year','site_number'))
SeasonRatios = merge(SeasonRatios,Winter_Precip,by=c('year','site_number'))
SeasonRatios = merge(SeasonRatios,Spring_Precip,by=c('year','site_number'))
SeasonRatios = merge(SeasonRatios,Summer_Precip,by=c('year','site_number'))

SeasonRatios = SeasonRatios[,Fall_Precip:=log10(Fall_Precip)]
SeasonRatios = SeasonRatios[,Winter_Precip:=log10(Winter_Precip)]
SeasonRatios = SeasonRatios[,Spring_Precip:=log10(Spring_Precip)]
SeasonRatios = SeasonRatios[,Summer_Precip:=log10(Summer_Precip)]

SeasonRatios = SeasonRatios[month%in%c(9,10,11),SeasonPrecip:=(Fall_Precip)]
SeasonRatios = SeasonRatios[month%in%c(12,1,2),SeasonPrecip:=(Winter_Precip)]
SeasonRatios = SeasonRatios[month%in%c(3,4,5),SeasonPrecip:=(Spring_Precip)]
SeasonRatios = SeasonRatios[month%in%c(6,7,8),SeasonPrecip:=(Summer_Precip)]

SeasonPrecip = setorder(SeasonRatios[,c('site_number','year','cluster','Season','month','Fall_Precip','Winter_Precip','Spring_Precip','Summer_Precip')],by='site_number')
SeasonPrecip = SeasonPrecip[,year:=(year+1)]

setnames(SeasonPrecip,'Fall_Precip','PriorFall_Precip')
setnames(SeasonPrecip,'Spring_Precip','PriorSpring_Precip')
setnames(SeasonPrecip,'Winter_Precip','PriorWinter_Precip')
setnames(SeasonPrecip,'Summer_Precip','PriorSummer_Precip')

SeasonRatios = merge(SeasonRatios,SeasonPrecip[,c('site_number','Season','cluster','year','month','PriorFall_Precip','PriorSpring_Precip','PriorSummer_Precip','PriorWinter_Precip')],by=c('site_number','year','month','cluster','Season'),all.x=T)
SeasonRatios = na.omit(SeasonRatios,cols = 'PriorSummer_Precip')

SeasonValues = unique(SeasonRatios,by=c('site_number','year','Season'))

# Z-score
ZSeason = SeasonValues[,c('site_number','year','calYear','cluster','Season','SeasonQ','Winter_Precip','Fall_Precip','Summer_Precip','PriorSummer_Precip','Spring_Precip','SeasonRunoff')]

ZSeasonSummary = data.table(site_number=as.character(),cluster=as.numeric(),year=as.numeric(),calYear=as.numeric(),Season=as.character(),zQ=as.numeric(),zSeasonRunoff=as.numeric(),zWinter_Precip=as.numeric(),zSpring_Precip=as.numeric(),zFall_Precip=as.numeric(),zSummer_Precip=as.numeric(),zPrior_Summer=as.numeric())
for (n in unique(Zscores$cluster)) {
  for (season in c('DJF','MAM','JJA','SON')) {
    #Statistics Table
    SeasonStats = ZSeason[cluster==n & Season==season]
    SeasonStats = SeasonStats[,c('meanQ','meanRunoff','meanFall','meanWinter','meanSpring','meanSummer','meanPSummer','sdQ','sdRunoff','sdFall','sdWinter','sdSpring','sdSummer','sdPSummer'):=
                    .(mean(SeasonQ,na.rm=T),mean(SeasonRunoff,na.rm=T),mean(Fall_Precip,na.rm=T),mean(Winter_Precip,na.rm=T),mean(Spring_Precip,na.rm=T),mean(Summer_Precip,na.rm=T),mean(PriorSummer_Precip,na.rm=T),
                      sd(SeasonQ,na.rm=T),sd(SeasonRunoff,na.rm=T),sd(Fall_Precip,na.rm=T),sd(Winter_Precip,na.rm=T),sd(Spring_Precip,na.rm=T),sd(Summer_Precip,na.rm=T),sd(PriorSummer_Precip,na.rm=T)),by='site_number']
    SeasonStats = unique(SeasonStats[,c('site_number','cluster','Season','meanQ','meanRunoff','meanFall','meanWinter','meanSpring','meanSummer','meanPSummer','sdQ','sdRunoff','sdFall','sdWinter','sdSpring','sdSummer','sdPSummer')],by='site_number')
    
    # Calculate Z-scores
    for (i in unique(SeasonStats$site_number)) {
      ZSeason = ZSeason[,c('zQ','zSeasonRunoff','zFall_Precip','zWinter_Precip','zSpring_Precip','zSummer_Precip','zPrior_Summer'):=
                          .((SeasonQ-SeasonStats[site_number==i,meanQ])/SeasonStats[site_number==i,sdQ],
                            (SeasonRunoff-SeasonStats[site_number==i,meanRunoff])/SeasonStats[site_number==i,sdRunoff],
                            (Fall_Precip-SeasonStats[site_number==i,meanFall])/SeasonStats[site_number==i,sdFall],
                            (Winter_Precip-SeasonStats[site_number==i,meanWinter])/SeasonStats[site_number==i,sdWinter],
                            (Spring_Precip-SeasonStats[site_number==i,meanSpring])/SeasonStats[site_number==i,sdSpring],
                            (Summer_Precip-SeasonStats[site_number==i,meanSummer])/SeasonStats[site_number==i,sdSummer],
                            (PriorSummer_Precip-SeasonStats[site_number==i,meanPSummer])/SeasonStats[site_number==i,sdPSummer])]
      Z = ZSeason[site_number==i,c('site_number','cluster','year','calYear','Season','zQ','zSeasonRunoff','zFall_Precip','zWinter_Precip','zSpring_Precip','zSummer_Precip','zPrior_Summer')]
      ZSeasonSummary = rbind(Z,ZSeasonSummary)
    }
  }
}


# Average Z-scores
SeasonZA = ZSeasonSummary[Season%in%c('DJF',"MAM",'JJA')]
SeasonZA = SeasonZA[,c('zQAv','zSeasonRunoffAv','zFallPrecipAv','zWinterPrecipAv','zSpringPrecipAv','zSummerPrecipAv','zPrior_SummerAv'):=
                 .(mean(zQ,na.rm=T),mean(zSeasonRunoff,na.rm=T),mean(zFall_Precip,na.rm=T),mean(zWinter_Precip,na.rm=T),mean(zSpring_Precip,na.rm=T),mean(zSummer_Precip,na.rm=T),mean(zPrior_Summer)),by=c('year','cluster','Season')]
SeasonZB = ZSeasonSummary[Season%in%c('SON')]
SeasonZB = SeasonZB[,c('zQAv','zSeasonRunoffAv','zFallPrecipAv','zWinterPrecipAv','zSpringPrecipAv','zSummerPrecipAv','zPrior_SummerAv'):=
                                  .(mean(zQ,na.rm=T),mean(zSeasonRunoff,na.rm=T),mean(zFall_Precip,na.rm=T),mean(zWinter_Precip,na.rm=T),mean(zSpring_Precip,na.rm=T),mean(zSummer_Precip,na.rm=T),mean(zPrior_Summer)),by=c('calYear','cluster','Season')]
SeasonZ = unique(rbind(SeasonZA,SeasonZB),by=c('year','cluster','Season'))

SpringFit = function(cluster_number) {
  Fit1 = lm(data=SeasonZ[cluster==cluster_number & Season=='MAM'], zSeasonRunoffAv ~  zFallPrecipAv)
  Fit2 = setDT(tidy(Fit1))
  FitTable <<- data.table('cluster'=Group_Names[cluster_number],'r.squared'=summary(Fit1)$adj.r.squared,'slope'=Fit2[2,estimate],'p.value'=lmp(Fit1))
}

#write.xlsx(SeasonZ[Season=='MAM'],paste0(root,'\\Code_Exports\\Season_Values.xlsx'))


SpringStats = lapply(1:16, SpringFit)
SpringStats = rbindlist(SpringStats)
write.csv(SpringStats,paste0(root,'\\Code_Exports\\SpringRE_FallPrecip_Stats.csv'))

SeasonFit = function(cluster_number,season_code) {
  # Same water_year
  Fit_Fall = lm(data=SeasonZ[cluster==cluster_number & Season==season_code], zSeasonRunoffAv ~  zFallPrecipAv)
  Fit_Spring = lm(data=SeasonZ[cluster==cluster_number & Season==season_code], zSeasonRunoffAv ~  zSpringPrecipAv)
  Fit_Summer = lm(data=SeasonZ[cluster==cluster_number & Season==season_code], zSeasonRunoffAv ~  zSummerPrecipAv)
  Fit_Winter = lm(data=SeasonZ[cluster==cluster_number & Season==season_code], zSeasonRunoffAv ~  zWinterPrecipAv)
  Fit_Fall2 = setDT(tidy(Fit_Fall))
  Fit_Spring2 = setDT(tidy(Fit_Spring))
  Fit_Summer2 = setDT(tidy(Fit_Summer))
  Fit_Winter2 = setDT(tidy(Fit_Winter))
  FallTable = data.table('cluster'=Group_Names[cluster_number],'SeasonPrecip'='SON','SeasonRunoff'=season_code,'r.squared'=summary(Fit_Fall)$adj.r.squared,'slope'=Fit_Fall2[2,estimate],'p.value'=lmp(Fit_Fall))
  SpringTable = data.table('cluster'=Group_Names[cluster_number],'SeasonPrecip'='MAM','SeasonRunoff'=season_code,'r.squared'=summary(Fit_Spring)$adj.r.squared,'slope'=Fit_Spring2[2,estimate],'p.value'=lmp(Fit_Spring))
  WinterTable = data.table('cluster'=Group_Names[cluster_number],'SeasonPrecip'='DJF','SeasonRunoff'=season_code,'r.squared'=summary(Fit_Winter)$adj.r.squared,'slope'=Fit_Winter2[2,estimate],'p.value'=lmp(Fit_Winter))
  SummerTable = data.table('cluster'=Group_Names[cluster_number],'SeasonPrecip'='JJA','SeasonRunoff'=season_code,'r.squared'=summary(Fit_Summer)$adj.r.squared,'slope'=Fit_Summer2[2,estimate],'p.value'=lmp(Fit_Summer))
  # Previous water_year
  # PFit_Fall = lm(data=SeasonZ[cluster==cluster_number & Season==season_code], ClusterRunoff ~  PriorFall_Precip)
  # PFit_Spring = lm(data=SeasonZ[cluster==cluster_number & Season==season_code], ClusterRunoff ~  PriorSpring_Precip)
  # PFit_Summer = lm(data=SeasonZ[cluster==cluster_number & Season==season_code], ClusterRunoff ~  PriorSummer_Precip)
  # PFit_Winter = lm(data=SeasonZ[cluster==cluster_number & Season==season_code], ClusterRunoff ~  PriorWinter_Precip)
  # PFit_Fall2 = setDT(tidy(Fit_Fall))
  # PFit_Spring2 = setDT(tidy(Fit_Spring))
  # PFit_Summer2 = setDT(tidy(Fit_Summer))
  # PFit_Winter2 = setDT(tidy(Fit_Winter))
  # PFallTable = data.table('cluster'=Group_Names[cluster_number],'SeasonPrecip'='PSON','SeasonRunoff'=season_code,'r.squared'=summary(PFit_Fall)$adj.r.squared,'slope'=PFit_Fall2[2,estimate],'p.value'=lmp(PFit_Fall))
  # PSpringTable = data.table('cluster'=Group_Names[cluster_number],'SeasonPrecip'='PMAM','SeasonRunoff'=season_code,'r.squared'=summary(PFit_Spring)$adj.r.squared,'slope'=PFit_Spring2[2,estimate],'p.value'=lmp(PFit_Spring))
  # PWinterTable = data.table('cluster'=Group_Names[cluster_number],'SeasonPrecip'='PDJF','SeasonRunoff'=season_code,'r.squared'=summary(PFit_Winter)$adj.r.squared,'slope'=PFit_Winter2[2,estimate],'p.value'=lmp(PFit_Winter))
  # PSummerTable = data.table('cluster'=Group_Names[cluster_number],'SeasonPrecip'='PJJA','SeasonRunoff'=season_code,'r.squared'=summary(PFit_Summer)$adj.r.squared,'slope'=PFit_Summer2[2,estimate],'p.value'=lmp(PFit_Summer))
  # 
  SeasonalTableA = rbind(FallTable,SpringTable)
  SeasonalTableB = rbind(WinterTable,SummerTable)
  SeasonalTableC = rbind(SeasonalTableA,SeasonalTableB)
  
  # SeasonalTableD = rbind(PFallTable,PSpringTable)
  # SeasonalTableE = rbind(PWinterTable,PSummerTable)
  # SeasonalTableF = rbind(SeasonalTableD,SeasonalTableE)
  # 
  # SeasonTableG = rbind(SeasonalTableC,SeasonalTableF)
}

BindSeasons = function (season) {
  A = lapply(1:16, SeasonFit,season_code=season)
  B <<- rbindlist(A)
}


C = lapply(c('MAM','JJA',"SON",'DJF'), BindSeasons)
SeasonalTable = rbindlist(C)

write.xlsx(SeasonalTable[p.value<=0.1],paste0(root,'\\Code_Exports\\Significant_Seasonal_Regressions.xlsx'))

SubsequentSeasons = SeasonalTable[(SeasonPrecip=='SON' & SeasonRunoff%in%c('SON','DJF','MAM','JJA')) | 
                (SeasonPrecip=='DJF' & SeasonRunoff%in%c('DJF','MAM','JJA')) | 
                (SeasonPrecip=='MAM' & SeasonRunoff%in%c('MAM','JJA')) |
                (SeasonPrecip=='JJA' & SeasonRunoff%in%c('JJA'))]

write.xlsx(SubsequentSeasons[p.value<=0.1],paste0(root,'\\Code_Exports\\Significant_SubsequentSeasonal_Regressions.xlsx'))

SummerQ = na.omit(SeasonZ[Season=='JJA',SummerPQ:=zQAv])
SummerQ = SummerQ[,year:=year+1]
SeasonZ = merge(SeasonZ,SummerQ[Season=='JJA',c('SummerPQ','year','site_number')],by=c('year','site_number'))

#Seasonal Map
GrobSample = ggplot(SeasonZ[cluster==1&Season%in%c('MAM')],aes(zFallPrecipAv,zSeasonRunoffAv))+
  facet_wrap(vars(Season))+
  geom_point(aes(logPrecip,zSeasonRunoff,color=Season))+
  geom_smooth(aes(logPrecip,zSeasonRunoff,color=Season),method='lm',se=F)+
  stat_cor(label.y=-1,aes(label=paste(..rr.label..,..p.label..,sep='~`,`~')))+
  scale_color_manual(values = c('MAM'='cornflowerblue'),labels=c('MAM'='Spring'))+
  labs(x='Fall Precip',y='Cluster Runoff Efficiency')+
  theme_bw()

Centroids = setDT(read.xlsx(paste0(root,'\\Input_Data\\ClusterCentroids_Shift.xlsx')))
season_labels=c('JJA'='Summer','MAM'='Spring','DJF'='Winter','SON'='Fall')
season_columns=c('JJA'='zSummerPrecipAv','MAM'='zSpringPrecipAv','DJF'='zWinterPrecipAv','SON'='zFallPrecipAv')

Cluster_seasons <- function(cluster_number,season_code){
  long <- Centroids[cluster == cluster_number]$Cent_lon
  lat <- Centroids[cluster == cluster_number]$Cent_lat
  plot_sel <- SeasonZ[cluster == cluster_number & Season==season_code]
  column <- plot_sel[,get(season_columns[season_code])]
  return(annotation_custom(grob = ggplotGrob(
    ggplot(plot_sel,aes(column,zSeasonRunoffAv))+
      {if(lmp(lm(data = plot_sel, zSeasonRunoffAv ~ column))<=0.05) geom_point(aes(column,zSeasonRunoffAv,color=Season),size=1)}+
      {if(lmp(lm(data = plot_sel, zSeasonRunoffAv ~ column))>0.05) geom_point(aes(column,zSeasonRunoffAv,color=Season),alpha=0.5,size=1)}+
      {if(lmp(lm(data = plot_sel, zSeasonRunoffAv ~ column))<=0.05) geom_smooth(aes(column,zSeasonRunoffAv),color='black',method='lm',se=F,size=1.25)}+
      {if(lmp(lm(data = plot_sel, zSeasonRunoffAv ~ column))<=0.05) stat_cor(size=2.5,label.y.npc='top',label.x.npc = 'left',aes(label=paste(..rr.label..,ifelse(readr::parse_number(..p.label..) < 0.001, "p<0.001", ..p.label..),sep='~`,`~')))}+
    
      {if(lmp(lm(data = plot_sel, zSeasonRunoffAv ~ column))<=0.15 & lmp(lm(data = plot_sel, zSeasonRunoffAv ~ column))>0.05) geom_point(aes(column,zSeasonRunoffAv,color=Season),size=1)}+
      {if(lmp(lm(data = plot_sel, zSeasonRunoffAv ~ column))>0.15) geom_point(aes(column,zSeasonRunoffAv,color=Season),alpha=0.5,size=1)}+
      {if(lmp(lm(data = plot_sel, zSeasonRunoffAv ~ column))<=0.15 & lmp(lm(data = plot_sel, zSeasonRunoffAv ~ column))>0.05) geom_smooth(aes(column,zSeasonRunoffAv),color='grey50',method='lm',se=F,size=1.25)}+
      {if(lmp(lm(data = plot_sel, zSeasonRunoffAv ~ column))<=0.15 & lmp(lm(data = plot_sel, zSeasonRunoffAv ~ column))>0.05) stat_cor(size=2.5,label.y.npc='top',label.x.npc = 'left',aes(label=paste(..rr.label..,ifelse(readr::parse_number(..p.label..) < 0.001, "p<0.001", ..p.label..),sep='~`,`~')))}+
      guides(color='none')+
      scale_color_manual(values = c('JJA'='red','DJF'='cornflowerblue','MAM'='mediumseagreen','SON'='orange'),labels=c('JJA'='Summer','MAM'='Spring','DJF'='Winter','SON'='Fall'))+
      labs(x=paste(season_labels[season_code],'Precipitation',sep = ' '),y=paste(season_labels[season_code],'Runoff Efficiency',sep = ' '),title=Group_Names[cluster_number])+
      theme_minimal()+
      theme(plot.title = element_text(size=9,hjust = 0.5),axis.title = element_text(size = 7),
            axis.text = element_text(size=6,vjust = 0.5),plot.subtitle = element_text(size=8,hjust = 0.5))
  ),
  xmin = long - 5.5, xmax = long + 5.5,
  ymin = lat - 4, ymax = lat + 4))
}

Cluster_IndividualSeason <- function(cluster_number){
  long <- Centroids[cluster == cluster_number]$Cent_lon
  lat <- Centroids[cluster == cluster_number]$Cent_lat
  plot_sel <- SeasonZ[cluster == cluster_number & Season=='JJA']
  column <- plot_sel[,zQAv]
  return(annotation_custom(grob = ggplotGrob(
    ggplot(plot_sel,aes(column,zSeasonRunoffAv))+
      {if(lmp(lm(data = plot_sel, zSeasonRunoffAv ~ column))<=0.05) geom_point(aes(column,zSeasonRunoffAv,color=Season),size=1)}+
      {if(lmp(lm(data = plot_sel, zSeasonRunoffAv ~ column))>0.05) geom_point(aes(column,zSeasonRunoffAv,color=Season),alpha=0.5,size=1)}+
      {if(lmp(lm(data = plot_sel, zSeasonRunoffAv ~ column))<=0.05) geom_smooth(aes(column,zSeasonRunoffAv),color='black',method='lm',se=F,size=1.25)}+
      {if(lmp(lm(data = plot_sel, zSeasonRunoffAv ~ column))<=0.05) stat_cor(size=2.5,label.y.npc='top',label.x.npc = 'left',aes(label=paste(..rr.label..,ifelse(readr::parse_number(..p.label..) < 0.001, "p<0.001", ..p.label..),sep='~`,`~')))}+
      guides(color='none')+
      scale_color_manual(values = c('JJA'='red','DJF'='cornflowerblue','MAM'='mediumseagreen','SON'='slateblue4'),labels=c('JJA'='Summer','MAM'='Spring','DJF'='Winter','SON'='Fall'))+
      labs(x=paste('Summer','Discharge',sep = ' '),y=paste('Summer','Runoff Efficiency',sep = ' '),title=Group_Names[cluster_number])+
      theme_minimal()+
      theme(plot.title = element_text(size=9,hjust = 0.5),axis.title = element_text(size = 7),
            axis.text = element_text(size=6,vjust = 0.5),plot.subtitle = element_text(size=8,hjust = 0.5))
  ),
  xmin = long - 5.5, xmax = long + 5.5,
  ymin = lat - 4, ymax = lat + 4))
}

MapSeasons = function(season_code){
  bar_sel = lapply(cluster_list,Cluster_seasons,season_code=season_code)
  
  Map = ggplot(data = NULL) +  
    geom_sf(data=ne_countries(country = 'United States of America',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") +
    geom_sf(data=ne_countries(country = 'Canada',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") + 
    coord_sf(xlim = c(-142, -50), ylim = c(24.5, 60), expand = FALSE)+
    annotation_scale(location = "bl", width_hint = 0.25) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(55, "pt"), pad_y = unit(25, "pt"),
                           style = north_arrow_nautical()) +
    labs(x='Longitude',y='Latitude')
  
  Combined = Map + bar_sel #+ inset_element(extract_legend(GrobSample),left = 0.9,bottom = 0.1,right = 0.98,top = 0.4)
  
  return(Combined)
}

MapVariableSeason = function(){
  bar_sel = lapply(cluster_list,Cluster_IndividualSeason)
  
  Map = ggplot(data = NULL) +  
    geom_sf(data=ne_countries(country = 'United States of America',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") +
    geom_sf(data=ne_countries(country = 'Canada',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") + 
    coord_sf(xlim = c(-142, -50), ylim = c(24.5, 60), expand = FALSE)+
    annotation_scale(location = "bl", width_hint = 0.25) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(55, "pt"), pad_y = unit(25, "pt"),
                           style = north_arrow_nautical()) +
    labs(x='Longitude',y='Latitude')
  
  Combined = Map + bar_sel #+ inset_element(extract_legend(GrobSample),left = 0.9,bottom = 0.1,right = 0.98,top = 0.4)
  
  return(Combined)
}

SeasonOverlay = function(){
  Final = lapply(c('SON','DJF','MAM','JJA'), MapSeasons)
  return(Final)
}

SeasonOverlay()

MapVariableSeason()

# Precipitation and Temperature Comparison
InternalSignificant = SubsequentSeasons[SeasonPrecip==SeasonRunoff & p.value<0.1]
setnames(InternalSignificant,'cluster','cluster_name')
InternalSignificant = InternalSignificant[,Season:=SeasonRunoff]

SeasonValues = SeasonValues[,cluster_name:=Group_Names[cluster]]
SeasonValues = merge(SeasonValues,InternalSignificant[,c('cluster_name','p.value','r.squared','Season','slope')],by=c('cluster_name','Season'),all.x=T)

write.xlsx(SeasonValues,paste0(root,'\\Code_Exports\\Significant_InternalSeasonal.xlsx'))

ClusterSeasonValues = SeasonValues[,c('ClusterPrecip','Cluster_Temp'):=.(mean(SeasonPrecip),mean(SeasonTemp)),by=c('cluster','Season')]
ClusterSeasonValues = unique(ClusterSeasonValues,by=c('Season','cluster'))
ClusterSeasonValues = ClusterSeasonValues[,Significance:=ifelse(is.na(p.value),'Insignificant','Significant')]

#Remove log -transformation
ClusterSeasonValues = ClusterSeasonValues[,c('SeasonTemp','SeasonPrecip'):=.((10^SeasonTemp)-273.15,10^SeasonPrecip)]

ggplot(ClusterSeasonValues[Significance=='Significant'],aes(SeasonTemp,SeasonPrecip))+
  geom_point(aes(SeasonTemp,SeasonPrecip,color=slope),size=3)+
  scale_color_gradientn(colors=c('#93003a', '#ffffe0','#00429d'),values = rescale(c(-1.5,-0.2,0,0.2,1.5)),limits=c(-1.5,1.5))+
  labs(x=paste('Seasonal Average Temperature','(\u00B0C)'),y='Seasonal Average Precipitation (m)')+
  theme_bw()

# Season Rose Diagram
Centroids = setDT(read.xlsx(paste0(root,'\\Input_Data\\ClusterCentroids_Shift.xlsx')))

SigSeasons =ClusterSeasonValues[,c('cluster_name','cluster','Season','slope','Significance','p.value','r.squared')]
SigSeasons = SigSeasons[Significance=='Significant',Significance:=ifelse(p.value>0.05,'Marginally','Significant')]
SigSeasons = SigSeasons[,Sign:=ifelse(sign(slope)>0,'Positive','Inverse')]
SigSeasons = SigSeasons[Season=='SON',Order:=3]
SigSeasons = SigSeasons[Season=='DJF',Order:=4]
SigSeasons = SigSeasons[Season=='MAM',Order:=1]
SigSeasons = SigSeasons[Season=='JJA',Order:=2]

SigSeasons = SigSeasons[cluster_name=='Northern Pacific Coast' & Season=='JJA',slope:=-1]

SampleData = copy(SigSeasons)
SampleData = SampleData[,slope:=0]

Scale = ggplot(SampleData[cluster==3],aes(fct_reorder(Season,Order),abs(slope)))+
  geom_col(aes(fill=Sign,alpha=Significance),color='black',width=1)+
  geom_hline(yintercept = 0.5,linetype='dashed')+
  geom_hline(yintercept = 1, linetype='dashed')+
  guides(color='none',fill='none',alpha='none')+
  coord_curvedpolar(clip='off',start=pi/4)+
  scale_x_discrete(labels=c('SON'='Fall','DJF'='Winter','MAM'='Spring','JJA'='Summer'))+
  theme_minimal()+ theme(plot.title = element_text(size=10,hjust = 0.5,vjust = 0.5),
                         axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),
                         panel.grid.major = element_blank(),
                         axis.text.x = element_text(vjust=0.7,size=35),plot.subtitle = element_text(size=8,hjust=0.5))

GrobSample = ggplot(SigSeasons[cluster==11],aes(fct_reorder(Season,Order),abs(slope)))+
  geom_col(aes(fill=Sign,alpha=Significance),color='black',width=1)+
  geom_hline(yintercept = 0.1,linetype='dashed')+
  geom_hline(yintercept = .3, linetype='dashed')+
  #geom_hline(yintercept = 2, linetype='dashed')+
  #guides(color='none',fill='none',alpha='none')+
  coord_curvedpolar(clip='off')+
  labs(fill='Relationship')+
  scale_fill_manual(values = c('Inverse'='red','Positive'='royalblue'))+
  scale_alpha_manual(values = c('Marginally'=0.3,'Significant'=1))+
  #scale_y_continuous(limits = c(0,1), breaks=0)+
  scale_x_discrete(labels=c('SON','DJF','MAM','JJA'))+
  theme_minimal()+ theme(plot.title = element_text(size=10,hjust = 0.5,vjust = 0.5),
                         axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),
                         panel.grid.major = element_blank(),
                         axis.text.x = element_text(vjust=-1,size=20),plot.subtitle = element_text(size=8,hjust=0.5,margin = margin(0,0,5,0)))

Cluster_SeasonRose <- function(cluster_number){
  long <- Centroids[cluster == cluster_number]$Cent_lon
  lat <- Centroids[cluster == cluster_number]$Cent_lat
  plot_sel <- SigSeasons[cluster == cluster_number]
  return(annotation_custom(grob = ggplotGrob(
    ggplot(plot_sel,aes(fct_reorder(Season,Order),abs(slope)))+
      geom_col(aes(fill=Sign,alpha=Significance),color='black',width=1)+
      coord_curvedpolar(clip='off',start = pi/4)+
      guides(fill='none',alpha='none')+
      labs(title = Group_Names[cluster_number],fill='Trend',)+
           #subtitle = paste('R\U00B2 =',paste(format(round(plot_sel[cluster==cluster_number,r.squared],2),nsmall=2)),sep=' ')+
      scale_fill_manual(values = c('Inverse'='red','Positive'='royalblue'))+
      scale_alpha_manual(values = c('Marginally'=0.3,'Significant'=1))+
      scale_y_continuous(limits = c(0,1))+
      scale_x_discrete(labels=c('SON'='Fall','DJF'='Winter','MAM'='Spring','JJA'='Summer'))+
      geom_hline(yintercept = 0.5,linetype='dashed')+
      geom_hline(yintercept = 1, linetype='dashed')+
      theme_minimal()+
      theme(plot.title = element_text(size=10,hjust = 0.5,vjust = 0.5),panel.grid = element_blank(),
            axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),
            panel.grid.major = element_blank(),
            axis.text.x = element_text(vjust=1),plot.subtitle = element_text(size=8,hjust=0.5,margin = margin(0,0,12,0)))
  ),
  xmin = long - 4.5, xmax = long + 4.5,
  ymin = lat - 4, ymax = lat + 4))
}

extract_legend <- function(GrobObject) {
  step1 <- ggplot_gtable(ggplot_build(GrobObject))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

MapSeasonRose = function(){
  bar_sel = lapply(cluster_list,Cluster_SeasonRose)
  
  Map = ggplot(data = NULL) +  
    geom_sf(data=ne_countries(country = 'United States of America',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") +
    geom_sf(data=ne_countries(country = 'Canada',type = 'countries',scale = 'medium', returnclass = 'sf'), fill='white',color = "gray60") + 
    coord_sf(xlim = c(-142, -50), ylim = c(24.5, 60), expand = FALSE)+
    annotation_scale(location = "bl", width_hint = 0.25) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(55, "pt"), pad_y = unit(25, "pt"),
                           style = north_arrow_nautical()) +
    labs(x='Longitude',y='Latitude')
  
  Combined = Map + bar_sel + inset_element(extract_legend(GrobSample),left = 0.9,bottom = 0.1,right = 0.98,top = 0.4)
  
  return(Combined)
}

MapSeasonRose()

FlowMagnitude = All_Data[,c('ClusterRE','ClusterQ'):=.(mean(RunoffRatio),mean(mean_Qcms)),by=c('cluster','year')]
FlowMagnitude = unique(FlowMagnitude[,Q_Residual:=ClusterQ-mean(ClusterQ),by=cluster],by=c('year','cluster'))

ggplot(FlowMagnitude,aes(ClusterRE,Q_Residual))+
  facet_wrap(vars(cluster),scales='free',labeller = as_labeller(Group_Names))+
  geom_point()+
  labs(x='Cluster Mean Runoff Efficiecny', y='Yearly Residual from Cluster Mean Discharge (cms)')+
  theme_bw()
