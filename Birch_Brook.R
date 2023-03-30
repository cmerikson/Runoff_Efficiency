library(data.table)
library(ggplot2)
library(openxlsx)
library(lubridate)
library(openxlsx)
library(ggpubr)
library(patchwork)
library(forcats)

setwd("C:\\Users\\Christian Erikson\\OneDrive - Dartmouth College\\Research\\Runoff_Ratio\\Birch_Brook\\")
setwd("C:\\Users\\cmeri\\OneDrive - Dartmouth College\\Research\\Runoff_Ratio\\Birch_Brook\\")


Weather = setDT(read.xlsx('Birch_Brook.xlsx'))
Weather = Weather[,Date:=convertToDate(Weather$Date, origin = '1900-01-01')]

Discharge = setDT(read.xlsx('Birch_Brook_Discharge.xlsx'))
Discharge = Discharge[,Date:=convertToDate(Discharge$Date, origin = '1900-01-01')]
setnames(Discharge,'Birch.Brook.daily.AVG.discharge.ft3s-1','Daily_Avg')
setnames(Discharge, 'Birch.Brook.daily.MAX.discharge.ft3s-1','Daily_Peak')
Discharge = Discharge[,Daily_Avg:=(Daily_Avg*0.028316846592)]
Discharge = Discharge[,Daily_Peak:=(Daily_Peak*0.028316846592)]

Birch_Brook = merge(Discharge[,c('Date','Daily_Peak','Daily_Avg')],Weather[,c('Date','Precip.(in)','Deep.Well.(ft)','Shallow.Well.(ft)')],by='Date')
Birch_Brook = Birch_Brook[,`Precip.(in)`:=`Precip.(in)`/39.37]
setnames(Birch_Brook,'Precip.(in)','Precip_m')

Birch_Brook = Birch_Brook[,Month:=month(Date)]
Birch_Brook = Birch_Brook[,year:=year(Date)]

for (i in 1:12) {
  if (i %in% c(12,1,2)) {Birch_Brook = Birch_Brook[Month==i,Season:='DJF']}
  if (i %in% c(3,4,5)) {Birch_Brook = Birch_Brook[Month==i,Season:='MAM']}
  if (i %in% c(6,7,8)) {Birch_Brook = Birch_Brook[Month==i,Season:='JJA']}
  if (i %in% c(9,10,11)) {Birch_Brook = Birch_Brook[Month==i,Season:='SON']}
}

Birch_Brook = Birch_Brook[,water_year:=ifelse(month(Date) > 8, as.numeric(year(Date) + 1), as.numeric(year(Date)))]

Seasonal = Birch_Brook[!(is.na(Daily_Peak)),Q_cms:=mean(Daily_Avg,na.rm=T),by=c('Season','water_year')]
Seasonal = Seasonal[,Season_Precip:=sum(Precip_m),by=c('Season','water_year')]
Seasonal = Seasonal[,RE_season:=Q_cms*(86400*(30*3))/3962682/Season_Precip,by=c('Season','water_year')]
Seasonal = Seasonal[,RE_season:=ifelse(RE_season>=9999 | RE_season<=-9999, NA, RE_season)]

ggplot()+
  facet_wrap(vars(Season))+
  geom_point(data = Seasonal,aes(Date,RE))+
  geom_point(data = Seasonal,aes(Date,RE_season),color='red',size=3)+
  scale_y_continuous(limits = c(0,1.5))+
  geom_hline(yintercept = 1,linetype='dashed',color='gray50')+
  theme_bw()

Wells = Seasonal[,Season_Well:=mean(`Shallow.Well.(ft)`),by=c('water_year','Season')]

ggplot()+
  geom_point(data=Wells,aes(Date,-Season_Well),color='red')+
  geom_point(data=Wells,aes(Date,Season_Precip),color='blue')

Well_Regression = lm(data=Wells, Season_Well ~ Season_Precip)
summary(Well_Regression)

# Cold Month Groundwater
Cold_Months = merge(Discharge[,c('Date','Daily_Peak','Daily_Avg')],Weather[,c('Date','Precip.(in)','Deep.Well.(ft)','Shallow.Well.(ft)','Avg.Temp.Celcius')],by='Date')
Cold_Months = Cold_Months[,c('month','year'):=.(month(Date),year(Date))]
Cold_Months = Cold_Months[,water_year:=ifelse(month(Date) > 9, as.numeric(year(Date) + 1), as.numeric(year(Date)))]
August = Cold_Months[month==9]
Winter = copy(Cold_Months)
Spring = copy(Cold_Months)
Summer = copy(Cold_Months)
Fall = copy(Cold_Months)
Cold_Months = Cold_Months[,Month_Temp:=mean(`Avg.Temp.Celcius`),by=c('month','water_year')]
Cold_Months = Cold_Months[Month_Temp<=0]

Cold_Months = Cold_Months[,Shallow_Well_m:=`Shallow.Well.(ft)`/3.281]
Cold_Months = Cold_Months[,Precip_m:=`Precip.(in)`/39.37]

ggplot()+
  facet_wrap(vars(year),scales = 'free')+
  #geom_point(data=Cold_Months,aes(Date,`Deep.Well.(ft)`),color='navyblue')+
  geom_point(data=Cold_Months[as.numeric(month)%in%c(1,2,3,4)],aes(Date,-log(Shallow_Well_m),color='Shallow_Well_m'))+
  #geom_point(data=Cold_Months[as.numeric(month)%in%c(1,2,3,4)& `Avg.Temp.Celcius`>0],aes(Date,log(Shallow_Well_m),color='Avg.Temp.Celcius'))+
  geom_rect(data=Cold_Months[as.numeric(month)%in%c(1,2,3,4) & Avg.Temp.Celcius>0],aes(xmin=Date-.5,xmax=Date+.5,ymin=-Inf,ymax=Inf,fill='Avg.Temp.Celcius'),alpha=0.2)+
  geom_point(data=Cold_Months[as.numeric(month)%in%c(1,2,3,4)],aes(Date,log(Daily_Avg),color='Daily_Avg'))+
  scale_color_manual(values = c('Shallow_Well_m'='grey40','Daily_Avg'='blue','Precip_m'='navyblue'),labels=c('Shallow Well (m)','Daily Average Flow (cms)', "Precipitation (m)"))+
  scale_fill_manual('',values = c('Avg.Temp.Celcius'='red'),labels=c('Above Freezing'))+
  geom_point(data=Cold_Months[as.numeric(month)%in%c(1,2,3,4) & Precip_m>0],aes(Date,log(Precip_m),color='Precip_m'))+
  labs(x='Date',y='Log-Transformed Value',color='Measurement',title='Cold Months Daily Time Series')+
  theme_bw()

Cold_Months = Cold_Months[,month_well:=mean(Shallow_Well_m),by=c('month','water_year')]
Cold_Months = Cold_Months[,precip_sum:=sum(Precip_m),by=c('month','water_year')]
Cold_Months = Cold_Months[,month_flow:=mean(Daily_Avg),by=c('month','water_year')]

write.csv(Cold_Months,'Birch_Brook_Monthly.csv')

Cold_Monthly = unique(Cold_Months,by=c('month','water_year'))
Cold_Monthly = Cold_Monthly[,RE:=(month_flow*(2.628*10^6)/3962682)/precip_sum]

Winter = Winter[month%in%c(1,2,3)]

Cold_Fraction = data.table(water_year=numeric(),Fraction_Cold=numeric())
row = data.table()
for (i in 2006:2015){
  Cold_Days = nrow(Winter[year==i & Avg.Temp.Celcius<=0])
  Total_Days = nrow(Winter[year==i])
  Fraction_Cold = Cold_Days/Total_Days
  row = data.table(water_year=i,Fraction_Cold=Fraction_Cold)
  Cold_Fraction = rbind(Cold_Fraction,row)
  row=data.table()
}

Winter = na.omit(Winter)
Winter = Winter[,Shallow_Well_m:=`Shallow.Well.(ft)`/3.281]
Winter = Winter[,Precip_m:=`Precip.(in)`/39.37]
Winter = Winter[,c('Winter_Precip','Winter_Flow','Winter_Well'):=.(sum(Precip_m),mean(Daily_Avg),mean(Shallow_Well_m)),by=c('water_year')]
Winter= Winter[,Winter_RE:=Winter_Flow*5097600/3962682/Winter_Precip]
Winter = unique(Winter,by=c('water_year'))

Spring = na.omit(Spring)
Spring = Spring[month%in%c(3,4,5)]
Spring = Spring[,Shallow_Well_m:=`Shallow.Well.(ft)`/3.281]
Spring = Spring[,Precip_m:=`Precip.(in)`/39.37]
Spring = Spring[,c('Spring_Precip','Spring_Flow','Spring_Well'):=.(sum(Precip_m),mean(Daily_Avg),mean(Shallow_Well_m)),by=c('water_year')]
Spring= Spring[,Spring_RE:=Spring_Flow*7948800/3962682/Spring_Precip]
Spring = unique(Spring,by=c('water_year'))

Summer = na.omit(Summer)
Summer = Summer[month%in%c(6,7,8)]
Summer = Summer[,Shallow_Well_m:=`Shallow.Well.(ft)`/3.281]
Summer = Summer[,Precip_m:=`Precip.(in)`/39.37]
Summer = Summer[,c('Summer_Precip','Summer_Flow','Summer_Well'):=.(sum(Precip_m),mean(Daily_Avg),mean(Shallow_Well_m)),by=c('water_year')]
Summer= Summer[,Summer_RE:=Summer_Flow*7948800/3962682/Summer_Precip]
Summer = unique(Summer,by=c('water_year'))

Fall = na.omit(Fall)
Fall = Fall[month%in%c(10,11)]
Fall = Fall[,Shallow_Well_m:=`Shallow.Well.(ft)`/3.281]
Fall = Fall[,Precip_m:=`Precip.(in)`/39.37]
Fall = Fall[,c('Fall_Precip','Fall_Flow','Fall_Well'):=.(sum(Precip_m),mean(Daily_Avg),mean(Shallow_Well_m)),by=c('water_year')]
Fall= Fall[,Fall_RE:=Fall_Flow*7948800/3962682/Fall_Precip]
Fall = unique(Fall,by=c('water_year'))

August = na.omit(August)
August = August[,Shallow_Well_m:=`Shallow.Well.(ft)`/3.281]
August = August[,Precip_m:=`Precip.(in)`/39.37]
August = August[,c('August_Precip','August_Flow','August_Well'):=.(sum(Precip_m),mean(Daily_Avg),mean(Shallow_Well_m)),by=c('water_year')]
August= August[,August_RE:=August_Flow*2678400/3962682/August_Precip]

ggplot()+
  geom_point(data=unique(August,by='water_year'),aes(water_year,August_Flow,color='August_Flow'))+
  geom_point(data=Fall,aes(water_year,Fall_RE,color='Fall_RE'))+
  geom_line(data=unique(August,by='water_year'),aes(water_year,August_Flow,color='August_Flow'))+
  geom_line(data=Fall,aes(water_year,Fall_RE,color='Fall_RE'))+
  scale_color_manual(values = c('Fall_RE'='black','August_Flow'='blue'))+
  labs(x='Water Year',y='Flow / RE',color='Variable')+
  theme_bw()

ggplot()+
  geom_point(data=unique(August,by='water_year'),aes(water_year,-August_Well,color='August_Well'))+
  geom_point(data=Fall,aes(water_year,Fall_RE,color='Fall_RE'))+
  geom_line(data=unique(August,by='water_year'),aes(water_year,-August_Well,color='August_Well'))+
  geom_line(data=Fall,aes(water_year,Fall_RE,color='Fall_RE'))+
  scale_color_manual(values = c('Fall_RE'='black','August_Well'='blue'))+
  labs(x='Water Year',y='Well / RE',color='Variable')+
  theme_bw()

ggplot()+
  geom_point(data=unique(August,by='water_year'),aes(water_year,August_Precip,color='August_Precip'))+
  geom_point(data=Fall,aes(water_year,Fall_RE,color='Fall_RE'))+
  geom_line(data=unique(August,by='water_year'),aes(water_year,August_Precip,color='August_Precip'))+
  geom_line(data=Fall,aes(water_year,Fall_RE,color='Fall_RE'))+
  scale_color_manual(values = c('Fall_RE'='black','August_Precip'='blue'))+
  labs(x='Water Year',y='Flow / RE',color='Variable')+
  theme_bw()

August_Fall = cbind(unique(August[,c('August_Flow','August_Well','August_Precip','water_year')],by='water_year'),Fall[water_year<2016,Fall_RE])
setnames(August_Fall,'V2','Fall_RE',skip_absent = T)
V1 = August_Fall[,Fall_RE]
V1 = append(V1,NA)
V1 = V1[2:11]
August_Fall = cbind(August_Fall,V1)

ggplot(data=August_Fall,aes(August_Well,V1))+
  geom_point(data=August_Fall,aes(August_Well,V1),size=3)+
  geom_smooth(data=August_Fall,aes(August_Well,V1),method = 'lm')+
  stat_cor(label.x=1,label.y=0.25,aes(label=paste(..rr.label..,..p.label..,sep='~`,`~')))+
  labs(x='September Depth to Water',y='Fall Runoff Efficiency')+
  theme_bw()

Seasons = cbind(Winter[,c('water_year','Winter_RE','Winter_Well','Winter_Precip','Winter_Flow')],
                Spring[,c('Spring_RE','Spring_Well','Spring_Precip')],
                Summer[,c('Summer_RE','Summer_Well','Summer_Precip')],
                Fall[,c('Fall_RE','Fall_Well','Fall_Precip')])
setorder(Seasons,by='water_year')
Seasons = unique(Seasons,by='water_year')
Seasons = Seasons[,NormRE:=Summer_RE/Winter_RE]

ggplot(data=Seasons,aes((Spring_Precip+Winter_Precip),Summer_RE))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_cor(label.x=.5,label.y=0.1,aes(label=paste(..rr.label..,..p.label..,sep='~`,`~')))+
  theme_bw()

ggplot(data=Seasons,aes(Spring_Well,Summer_RE))+
  #geom_point(data=Seasons,aes(Spring_Well,Spring_RE),color='darkgreen',size=3)+
  #geom_smooth(data=Seasons,aes(Spring_Well,Spring_RE),se=F,color='darkgreen')+
  geom_point(data=Seasons,aes(Spring_Well,Summer_RE),size=3)+
  geom_smooth(data=Seasons,aes(Spring_Well,Summer_RE),method = 'lm')+
  stat_cor(label.x=.5,label.y=0.1,aes(label=paste(..rr.label..,..p.label..,sep='~`,`~')))+
  labs(x='Spring Depth to Water',y='Summer Runoff Efficiency')+
  theme_bw()

ggplot()+
  geom_point(data=Seasons,aes(Winter_Flow,Spring_RE),color='darkgreen',size=3)+
  #geom_point(data=Seasons,aes(Winter_Flow,Summer_RE),color='blue')+
  #geom_line(data=Seasons,aes(Winter_RE,Spring_RE),color='darkgreen')+
  #geom_point(data=Seasons,aes(water_year,NormRE))+
  #geom_line(data=Seasons,aes(water_year,Winter_RE))+
  theme_bw()

ggplot()+
  #facet_wrap(vars(water_year),scales='free')+
  geom_point(data = Cold_Monthly,aes(Date,log(RE),color='RE'))+
  geom_point(data=Cold_Monthly,aes(Date,-log(precip_sum),color='precip_sum'))+
  geom_point(data=Cold_Monthly,aes(Date,log(month_well),color='month_well'))+
  geom_point(data=Cold_Monthly,aes(Date,log(month_flow),color='month_flow'))+
  geom_line(data=Cold_Monthly,aes(Date,log(RE),color='RE'),linetype='dashed')+
  geom_line(data=Cold_Monthly,aes(Date,log(month_flow),color='month_flow'),linetype='dashed')+
  geom_line(data=Cold_Monthly,aes(Date,-log(precip_sum),color='precip_sum'),linetype='dashed')+
  geom_line(data=Cold_Monthly,aes(Date,log(month_well),color='month_well'),linetype='dashed')+
  scale_color_manual(values = c('precip_sum'='navyblue','month_well'='grey40','month_flow'='blue','RE'='red'),labels=c('precip_sum'='Precipitation','month_well'='Well Height','month_flow'='Monthly Mean Flow','RE'='Runoff Efficiency'))+
  labs(y='log-transformed value',title='Birch Brook Monthly Time Series',color='Measurement')+
  theme_bw()

Eddy_Flux = setDT(read.xlsx("C:\\Users\\Christian Erikson\\OneDrive - Dartmouth College\\Research\\Runoff_Ratio\\Evapotranspiration\\Eddy_Covariance_Flux.xlsx"))

ggplot(data=Eddy_Flux,aes(Precip_mm,Evapotranspiration_mm))+
  geom_smooth(data=Eddy_Flux,method='lm',se=F,color='black')+
  stat_cor(label.y=500,label.x = 1200,aes(label=paste(..rr.label..,..p.label..,sep='~`,`~')))+
  geom_point(data=Eddy_Flux,aes(color=Source),size=3)+
  scale_color_manual(values = c('Denager et al. 2020'='#00429d','Kosugi et al. 2007'='#93003a','Ryu et al. 2008'='#a49f6d'))+
  labs(x='Precipitation (mm)',y='Evapotranspiration (mm)')+
  theme_bw()

Groundwater_Seasons = setDT(read.xlsx('Groundwater_RE_Seasons.xlsx'))

Groundwater_Precip = Groundwater_Seasons[,c('water_year','Fall_groundwater','Winter_groundwater','OffsetSummer_groundwater','OffSummerPrecip')]
setnames(Groundwater_Precip,'Fall_groundwater','Fall')
setnames(Groundwater_Precip,'Winter_groundwater','Winter')
setnames(Groundwater_Precip,'OffsetSummer_groundwater','OffSummer')
Groundwater_Precip = melt(Groundwater_Precip,id=c('water_year','OffSummerPrecip'))
Groundwater_Precip = Groundwater_Precip[,value:=5-value]
Groundwater_Precip = Groundwater_Precip[variable%in%c('Fall','Winter'),Order:=ifelse(variable=='Winter',1,2)]
Groundwater_Precip = Groundwater_Precip[variable%in%c('OffSummer'),Order:=3]

Groundwater_Seasons = melt(Groundwater_Seasons,id=c('water_year','Fall_groundwater','OffsetSummer_groundwater','OffSummerPrecip'))

Groundwater_Seasons = Groundwater_Seasons[,c('Fall_groundwater','OffsetSummer_groundwater'):=.(5-Fall_groundwater,5-OffsetSummer_groundwater)]
Groundwater_Seasons = Groundwater_Seasons[variable%in%c('Fall','Winter'),Order:=ifelse(variable=='Winter',1,2)]
Groundwater_Seasons = Groundwater_Seasons[variable%in%c('OffSummer'),Order:=3]

Fall_Summer = ggplot(Groundwater_Seasons[variable=='Summer'],aes(Fall_groundwater,value,color=variable))+
  geom_point(size=3)+
  geom_smooth(method='lm',se=F)+
  stat_cor(label.x.npc='left',label.y.npc = 'top',aes(label=paste(..rr.label..,..p.label..,sep='~`,`~')))+
  scale_color_manual(values = c('Fall'='#00429d','Summer'='#93003a'))+
  scale_y_continuous(limits = c(0,1.25))+
  labs(x='Fall Depth to Groundwater (m)',y='Summer Runoff Efficiecny',color='Season')+
  theme_bw()
Summer_Winter_Fall = ggplot(Groundwater_Seasons[variable%in%c('Winter','Fall','OffSummer')],aes(OffsetSummer_groundwater,value,color=variable))+
  geom_point(size=3.5)+
  geom_smooth(method='lm',se=F)+
  stat_cor(label.x.npc=0.6,label.y.npc = 'top',digits = 1,aes(color=fct_reorder(variable,Order),label=paste(..rr.label..,ifelse(readr::parse_number(..p.label..) < 0.001, "italic(p)<0.001", ..p.label..),sep='~`,`~')))+
  scale_color_manual(values = c('Fall'='orange','Winter'='cornflowerblue','OffSummer'='red'),labels = c('Fall'='Fall','Winter'='Winter','OffSummer'='Summer'))+
  #guides(color='none')+
  #scale_y_continuous(limits = c(0,1.25))+
  labs(x='Summer Groundwater Elevation (m)',y='Season Runoff Efficiency',color='Season')+
  theme_bw()

Figure_5 = Summer_Winter_Fall + Fall_Summer + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size=20))

scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}

Summer_Precip_Fall_Winter_GW = ggplot(Groundwater_Precip[variable%in%c('Winter','Fall','OffSummer')],aes(OffSummerPrecip,value,color=variable))+
  geom_point(size=3.5)+
  geom_smooth(method='lm',se=F)+
  stat_cor(label.x.npc=0.6,label.y.npc = 'top',digits = 1,aes(color=fct_reorder(variable,Order),label=paste(..rr.label..,ifelse(readr::parse_number(..p.label..) < 0.001, "italic(p)<0.001", ..p.label..),sep='~`,`~')))+
  scale_color_manual(values = c('Fall'='orange','Winter'='cornflowerblue', 'OffSummer'='red'),labels = c('Fall'='Fall','Winter'='Winter','OffSummer'='Summer'))+
  guides(color='none')+
  scale_x_continuous(labels=scientific)+
  labs(x='Summer Precipitation (m)',y='Season Groundwater Elevation (m)',color='Season')+
  theme_bw()

Groundwater_Effect = Summer_Precip_Fall_Winter_GW + Summer_Winter_Fall +
  plot_layout(guides='collect', heights = unit(c(15),c('cm')), widths = unit(c(15),c('cm')))+
  plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size=20))
  

Annual = setDT(read.xlsx('Annual.xlsx'))

ggplot(Annual[!(water.year==1992)],aes(`TotPcpt(m)`,`AveRunoff.(m/yr)`))+
  geom_point(size=3)+
  geom_smooth(method='lm',se=F, formula = -0.0233983604644608+ y ~ x)+
  annotate('text',label=('y = 0.53x'),x=1.6,y=0.8)+
  #stat_regline_equation(label.x.npc='left',label.y.npc = 'top', formula =  -0.0233983604644608 + y ~ x)+
  #stat_cor(label.x.npc='left',label.y.npc = 'top',digits = 1,aes(label=paste(..rr.label..,ifelse(readr::parse_number(..p.label..) < 0.001, "italic(p)<0.001", ..p.label..),sep='~`,`~')))+
  labs(x='Annual Precipitation (m)', y='Annual Runoff (m)')+
  theme_bw()
