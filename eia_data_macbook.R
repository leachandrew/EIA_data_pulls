#library(plyr)
require(dplyr)
require(tidyr)
require(reshape2)
library(openxlsx)
library(ggplot2)
library(lubridate)
library(zoo)
library(ggmap)
library(ggridges)
library(viridis)
library(RColorBrewer)
library(pdfetch)
library(EIAdata)
library(scales)
library(jsonlite)



#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive")
print(getwd())
#Set EIA API Key




 ## The function is currently defined as
 data_fetch<-function(key, cat=2227122){
   key <- unlist(strsplit(key, ";"))
   ifelse(cat==999999999,
          url <- paste("http://api.eia.gov/category?api_key=",
                       key, "&out=xml", sep="" ),
          url <- paste("http://api.eia.gov/category?api_key=",
                       key, "&category_id=", cat, "&out=xml", sep="" )
   )
   doc <- xmlParse(file=url, isURL=TRUE)
   Parent_Category <- tryCatch(xmlToDataFrame(nodes =
                                                XML::getNodeSet(doc, "//category/parent_category_id")),
                               warning=function(w) FALSE, error=function(w) FALSE)
   Sub_Categories <- xmlToDataFrame(nodes =
                                      XML::getNodeSet(doc, "//childcategories/row"))
   Series_IDs <- xmlToDataFrame(nodes =
                                  XML::getNodeSet(doc, "///childseries/row"))
   Categories <- list(Parent_Category, Sub_Categories, Series_IDs)
   names(Categories) <- c("Parent_Category", "Sub_Categories", "Series_IDs")
   return(Categories)
 }
 

 set_png<-function(file_sent,w_sent=1400,h_sent=750,res_sent=130){
   #MAC
   if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
     png(file=file_sent, width = w_sent, height = h_sent,res=res_sent)
   #PC
   if(R.version$platform ==  "x86_64-w64-mingw32")
     png(file=file_sent, width = w_sent, height = h_sent,res=res_sent,type='cairo')
 }
 
 
solar_data_18<-getEIA(ID = "AEO.2018.REF2018.GEN_NA_ELEP_NA_SLR_PHTVL_NA_BLNKWH.A", key = KEY) 
solar_data_17<-getEIA(ID = "AEO.2017.REF2017.GEN_NA_ELEP_NA_SLR_PHTVL_NA_BLNKWH.A", key = KEY) 
 #series_id=AEO.2017.REF2017.GEN_NA_ELEP_NA_SLR_PHTVL_NA_BLNKWH.A 
solar_data_16<-getEIA(ID = "AEO.2016.REF2016.GEN_NA_ELEP_NA_SLR_PHTVL_NA_BLNKWH.A", key = KEY)  
solar_data_15<-getEIA(ID = "AEO.2015.REF2015.GEN_NA_ELEP_NA_SLR_PHTVL_NA_BLNKWH.A", key = KEY)  
solar_data_14<-getEIA(ID = "AEO.2014.REF2014.GEN_NA_ELEP_NA_SOPH_NA_NA_BLNKWH.A", key = KEY)  

solar_data<-merge(solar_data_18,solar_data_17)
solar_data<-merge(solar_data,solar_data_16)
solar_data<-merge(solar_data,solar_data_15)
solar_data<-merge(solar_data,solar_data_14)
names(solar_data)<-c("solar_2018","solar_2017","solar_2016","solar_2015","solar_2014")
solar_data<-data.frame(date=index(solar_data), coredata(solar_data))
solar_melt <- melt(solar_data,id="date") 


png<-1
if(png==1)
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file="solar_eia.png", width = 1400, height = 750,res=130,type='cairo')
  else(R.version$platform ==  "x86_64-w64-mingw32")
    png(file="solar_eia.png", width = 1400, height = 750,res=130)

ggplot(solar_melt) +
  geom_line(aes(date,value,colour=variable),size=2) +
  scale_color_viridis("",labels = c("2018 AEO", "2017 AEO","2016 AEO","2015 AEO","2014 AEO"),discrete = TRUE)+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+    labs(y="Generation (BkWh)",x="\nTime",
             title="EIA Annual Energy Outlook Solar Generation Forecasts",
             caption="Source: EIA API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

 
#oil outlooks



oil_data_18<-getEIA(ID = "AEO.2018.REF2018.PRD_NA_NA_NA_CR_NA_L48_MILLBRLPDY.A", key = KEY)  
oil_data_17<-getEIA(ID = "AEO.2017.REF2017.PRD_NA_NA_NA_CR_NA_L48_MILLBRLPDY.A", key = KEY)  
oil_data_16<-getEIA(ID = "AEO.2016.REF2016.PRD_NA_NA_NA_CR_NA_L48_MILLBRLPDY.A", key = KEY)  
oil_data_15<-getEIA(ID = "AEO.2015.REF2015.PRD_NA_NA_NA_CR_NA_L48_MILLBRLPDY.A", key = KEY)  
oil_data_14<-getEIA(ID = "AEO.2014.REF2014.PRD_NA_NA_NA_CO_NA_L48_MILLBRLPDY.A", key = KEY)  

oil_data<-merge(oil_data_18,oil_data_17)
oil_data<-merge(oil_data,oil_data_16)
oil_data<-merge(oil_data,oil_data_15)
oil_data<-merge(oil_data,oil_data_14)
names(oil_data)<-c("oil_2018","oil_2017","oil_2016","oil_2015","oil_2014")
oil_data<-data.frame(date=index(oil_data), coredata(oil_data))
oil_melt <- melt(oil_data,id="date") 


png<-1
if(png==1)
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file="oil_eia.png", width = 1400, height = 750,res=130,type='cairo')
else(R.version$platform ==  "x86_64-w64-mingw32")
png(file="oil_eia.png", width = 1400, height = 750,res=130)

ggplot(oil_melt) +
  geom_line(aes(date,value,colour=variable),size=2) +
  scale_color_viridis("",labels = c("2018 AEO", "2017 AEO","2016 AEO","2015 AEO","2014 AEO"),discrete = TRUE)+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+    labs(y="Production (millions of barrels per day)",x="\nTime",
             title="EIA Annual Energy Outlook Lower-48 Crude Oil Production Forecasts",
             caption="Source: EIA API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


gas_imp_data_18<-getEIA(ID = "AEO.2018.REF2018.TRAD_NETIMP_NA_NA_NG_NA_USA_TRLCF.A", key = KEY)  
gas_imp_data_17<-getEIA(ID = "AEO.2017.REF2017.TRAD_NETIMP_NA_NA_NG_NA_USA_TRLCF.A", key = KEY)  
gas_imp_data_16<-getEIA(ID = "AEO.2016.REF2016.TRAD_NETIMP_NA_NA_NG_NA_USA_TRLCF.A", key = KEY)  
gas_imp_data_15<-getEIA(ID = "AEO.2015.REF2015.TRAD_NETIMP_NA_NA_NG_NA_USA_TRLCF.A", key = KEY)  
gas_imp_data_14<-getEIA(ID = "AEO.2014.REF2014.TRAD_NETIMP_NA_NA_NG_NA_USA_TRLCF.A", key = KEY)  

gas_imp_data<-merge(gas_imp_data_18,gas_imp_data_17)
gas_imp_data<-merge(gas_imp_data,gas_imp_data_16)
gas_imp_data<-merge(gas_imp_data,gas_imp_data_15)
gas_imp_data<-merge(gas_imp_data,gas_imp_data_14)
names(gas_imp_data)<-c("gas_imp_2018","gas_imp_2017","gas_imp_2016","gas_imp_2015","gas_imp_2014")
gas_imp_data<-data.frame(date=index(gas_imp_data), coredata(gas_imp_data))
gas_imp_melt <- melt(gas_imp_data,id="date") 


png<-1
if(png==1)
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file="gas_imp_eia.png", width = 1400, height = 750,res=130,type='cairo')
else(R.version$platform ==  "x86_64-w64-mingw32")
png(file="gas_imp_eia.png", width = 1400, height = 750,res=130)

ggplot(gas_imp_melt) +
  geom_line(aes(date,value/365*1000,colour=variable),size=2) +
  scale_color_viridis("",labels = c("2018 AEO", "2017 AEO","2016 AEO","2015 AEO","2014 AEO"),discrete = TRUE)+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+    labs(y="Net Imprts of Natural Gas (billions of cubic feet/dat)",x="\nTime",
             title="EIA Annual Energy Outlook Lower-48 Natural Gas Import Forecasts",
             caption="Source: EIA API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


gas_prod_data_18<-getEIA(ID = "AEO.2018.REF2018.SUP_NA_TOT_NA_NG_NA_USA_TRLCF.A", key = KEY)  
gas_prod_data_17<-getEIA(ID = "AEO.2017.REF2017.SUP_NA_TOT_NA_NG_NA_USA_TRLCF.A", key = KEY)  
gas_prod_data_16<-getEIA(ID = "AEO.2016.REF2016.SUP_NA_TOT_NA_NG_NA_USA_TRLCF.A", key = KEY)  
gas_prod_data_15<-getEIA(ID = "AEO.2015.REF2015.SUP_NA_TOT_NA_NG_NA_USA_TRLCF.A", key = KEY)  
gas_prod_data_14<-getEIA(ID = "AEO.2014.REF2014.SUP_NA_TOT_NA_NG_NA_USA_TRLCF.A", key = KEY)  

gas_prod_data<-merge(gas_prod_data_18,gas_prod_data_17)
gas_prod_data<-merge(gas_prod_data,gas_prod_data_16)
gas_prod_data<-merge(gas_prod_data,gas_prod_data_15)
gas_prod_data<-merge(gas_prod_data,gas_prod_data_14)
names(gas_prod_data)<-c("gas_prod_2018","gas_prod_2017","gas_prod_2016","gas_prod_2015","gas_prod_2014")
gas_prod_data<-data.frame(date=index(gas_prod_data), coredata(gas_prod_data))
gas_prod_melt <- melt(gas_prod_data,id="date") 


png<-1
if(png==1)
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file="gas_prod_eia.png", width = 1400, height = 750,res=130,type='cairo')
else(R.version$platform ==  "x86_64-w64-mingw32")
png(file="gas_prod_eia.png", width = 1400, height = 750,res=130)

ggplot(gas_prod_melt) +
  geom_line(aes(date,value/365*1000,colour=variable),size=2) +
  scale_color_viridis("",labels = c("2018 AEO", "2017 AEO","2016 AEO","2015 AEO","2014 AEO"),discrete = TRUE)+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+    labs(y="Total Supply of Natural Gas (billions of cubic feet/dat)",x="\nTime",
             title="EIA Annual Energy Outlook Total Natural Gas Supply Forecasts",
             caption="Source: EIA API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




wti_price_data_18<-getEIA(ID = "AEO.2018.REF2018.PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A", key = KEY)  
wti_price_data_17<-getEIA(ID = "AEO.2017.REF2017.PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A", key = KEY)  
wti_price_data_16<-getEIA(ID = "AEO.2016.REF2016.PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A", key = KEY)  
wti_price_data_15<-getEIA(ID = "AEO.2015.REF2015.PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A", key = KEY)  
wti_price_data_14<-getEIA(ID = "AEO.2014.REF2014.PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A", key = KEY)  

wti_price_data<-merge(wti_price_data_18,wti_price_data_17)
wti_price_data<-merge(wti_price_data,wti_price_data_16)
wti_price_data<-merge(wti_price_data,wti_price_data_15)
wti_price_data<-merge(wti_price_data,wti_price_data_14)
names(wti_price_data)<-c("wti_price_2018","wti_price_2017","wti_price_2016","wti_price_2015","wti_price_2014")
wti_price_data<-data.frame(date=index(wti_price_data), coredata(wti_price_data))
wti_price_melt <- melt(wti_price_data,id="date") 


png<-1
if(png==1)
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file="wti_price_eia.png", width = 1400, height = 750,res=130,type='cairo')
else(R.version$platform ==  "x86_64-w64-mingw32")
png(file="wti_price_eia.png", width = 1400, height = 750,res=130)

ggplot(wti_price_melt) +
  geom_line(aes(date,value,colour=variable),size=2) +
  scale_color_viridis("",labels = c("2018 AEO", "2017 AEO","2016 AEO","2015 AEO","2014 AEO"),discrete = TRUE)+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+    labs(y="WTI Crude Oil Prices ($/bbl)",x="\nTime",
             title="EIA Annual Energy Outlook Reference Case Crude Oil Forecasts",
             caption="Source: EIA API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()









#buidling maya

#maya FOB
#resid https://www.eia.gov/opendata/qb.php?category=1039857&sdid=STEO.RFTCUUS.M
#crudes category_id=1039852
#wts and #lls http://api.eia.gov/category/?api_key=YOUR_API_KEY_HERE&category_id=293643
#import FOB http://api.eia.gov/category/?api_key=YOUR_API_KEY_HERE&category_id=293720

#refiner sales: http://api.eia.gov/category/?api_key=YOUR_API_KEY_HERE&category_id=243460

#formula
#U.S. GULF COAST MAYA = 0.40 * West Texas Sour (WTS) + 0.10 * Louisiana Light Sweet (LLS)
#
#+ 0.10 * Dated Brent (BRENT DTD) + 0.40 * No. 6 Fuel Oil 3%S + K



#AEO2017 category_id=2227112
 #AEO2016 category_id=2102233
 #AEO2015 category_id=1370522
 #AEO2014 category_id=964165
 
 
 

#NYMEX SPOTS 
subs<-data_fetch(KEY,cat=241335)
testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)
test <- setNames(test, t(subs$Series_IDs$name))
test2<-data.frame(date=index(test), coredata(test))
test2$date<-as.Date(test2$date,format = "%m/%d/%Y")

annual_crude<-cbind(test2$date,test2[,grep("Annual", colnames(test2))])  #all annual columns
annual_crude<-na.omit(annual_crude)
names(annual_crude)[1]<-paste("Date")  

monthly_crude<-cbind(test2$date,test2[,grep("Monthly", colnames(test2))])  #all monthly columns
monthly_crude<-na.omit(monthly_crude)
names(monthly_crude)[1]<-paste("Date")

weekly_crude<-cbind(test2$date,test2[,grep("Weekly", colnames(test2))])  #all weekly_crude columns
#weekly_crude<-na.omit(weekly_crude)
names(weekly_crude)[1]<-paste("Date")
weekly_crude<-subset(weekly_crude, Date > as.Date("2007-01-01"))
weekly_crude<-na.omit(weekly_crude)

weekly_crude$WTI_Brent_diff<- weekly_crude$Europe.Brent.Spot.Price.FOB..Weekly-weekly_crude$Cushing..OK.WTI.Spot.Price.FOB..Weekly


df1<-melt(weekly_crude,id=c("Date"),measure.vars = c("Europe.Brent.Spot.Price.FOB..Weekly","Cushing..OK.WTI.Spot.Price.FOB..Weekly","WTI_Brent_diff"))
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_WTI_brent.png")
ggplot(df1,aes(Date,value,group = variable,colour=variable)) +
  geom_line(data=subset(df1,variable %in% c("Europe.Brent.Spot.Price.FOB..Weekly","Cushing..OK.WTI.Spot.Price.FOB..Weekly")), size=1.7) +
  geom_col(data=subset(df1,variable %in% c("WTI_Brent_diff")),size=2)+
  geom_line(data=subset(df1,variable %in% c("WTI_Brent_diff")),size=2)+
  #geom_point(size=1) +
  #scale_color_viridis(labels=c("Brent","WTI"),discrete=TRUE)+   
  scale_fill_brewer(NULL,labels=c("Brent","WTI","Brent Premium or\nDiscount to WTI"),type = "seq", palette = "Paired", direction = -1)+
  scale_colour_brewer(NULL,labels=c("Brent","WTI","Brent Premium or\nDiscount to WTI"),type = "seq", palette = "Paired", direction = -1)+
  scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y",limits=c(max(df1$Date)-years(10),max(df1$Date)),expand=c(0.04,0)) +
  #scale_colour_manual(labels=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  scale_y_continuous(limits=c(-15,150),expand=c(0,0))+
  #guides(colour=guide_legend(ncol =3,byrow=FALSE),fill=NULL)+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=60, hjust=1),
  )+    labs(y="Weekly Closing Spot Price ($US/bbl)",x="Year",
             title="WTI and Brent Spot Prices",
             caption="Source: EIA Data\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
dev.off()

weekly_crude$NYcrack<- 42*weekly_crude$New.York.Harbor.Ultra.Low.Sulfur.No.2.Diesel.Spot.Price..Weekly/3+weekly_crude$New.York.Harbor.Conventional.Gasoline.Regular.Spot.Price.FOB..Weekly*42*2/3-weekly_crude$Cushing..OK.WTI.Spot.Price.FOB..Weekly
weekly_crude$GCcrack<- 42*weekly_crude$U.S..Gulf.Coast.Ultra.Low.Sulfur.No.2.Diesel.Spot.Price..Weekly/3+weekly_crude$U.S..Gulf.Coast.Conventional.Gasoline.Regular.Spot.Price.FOB..Weekly*42*2/3-weekly_crude$Cushing..OK.WTI.Spot.Price.FOB..Weekly
weekly_crude$GCBcrack<- 42*weekly_crude$U.S..Gulf.Coast.Ultra.Low.Sulfur.No.2.Diesel.Spot.Price..Weekly/3+weekly_crude$U.S..Gulf.Coast.Conventional.Gasoline.Regular.Spot.Price.FOB..Weekly*42*2/3-weekly_crude$Europe.Brent.Spot.Price.FOB..Weekly

df1<-melt(weekly_crude,id=c("Date"),measure.vars = c("Cushing..OK.WTI.Spot.Price.FOB..Weekly","NYcrack","Europe.Brent.Spot.Price.FOB..Weekly","GCBcrack"))
df1$Date<-as.Date(df1$Date,format = "%m/%d/%Y")

test_date=as.numeric(as.POSIXlt("09/13/2016", format="%m/%d/%Y"))

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_WTI_cracks.png")
#jpeg(file="AESO_LTO.jpg", width = 1400, height = 750)
ggplot(df1,aes(Date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  #scale_color_viridis(labels=c("Brent","WTI"),discrete=TRUE)+   
  scale_colour_brewer(NULL,labels=c("WTI Prices","WTI-Based NY Harbour Crack Spread","Brent Prices","Brent-Based Gulf Coast Crack Spread"),type = "seq", palette = "Paired", direction = -1)+
  scale_x_date(date_breaks = "6 months", date_labels =  "%m %Y",limits=c(as.Date("2007-07-01"),as.Date("2018-01-01"))) +
  #scale_colour_manual(labels=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  scale_y_continuous(limits=c(0,150),expand=c(0,0))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=60, hjust=1),
  )+
  labs(y="Prices and 3:2:1 Crack Spreads ($US/bbl)",x="Year",
             title="WTI and Brent Crude Prices and Gulf Coast and NY Harbour Crack Spreads",
             subtitle="Source: EIA Data, graph by Andrew Leach.")

dev.off()

weekly_crude$NYref<- 42*weekly_crude$New.York.Harbor.Ultra.Low.Sulfur.No.2.Diesel.Spot.Price..Weekly/3+weekly_crude$New.York.Harbor.Conventional.Gasoline.Regular.Spot.Price.FOB..Weekly*42*2/3
weekly_crude$GCref<- 42*weekly_crude$U.S..Gulf.Coast.Ultra.Low.Sulfur.No.2.Diesel.Spot.Price..Weekly/3+weekly_crude$U.S..Gulf.Coast.Conventional.Gasoline.Regular.Spot.Price.FOB..Weekly*42*2/3


df1<-melt(weekly_crude,id=c("Date"),measure.vars = c("Cushing..OK.WTI.Spot.Price.FOB..Weekly","NYref",
                                                     "Europe.Brent.Spot.Price.FOB..Weekly","GCref"))
df1$Date<-as.Date(df1$Date,format = "%m/%d/%Y")

test_date=as.numeric(as.POSIXlt("09/13/2016", format="%m/%d/%Y"))


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_crude_gas.png")
#jpeg(file="AESO_LTO.jpg", width = 1400, height = 750)
ggplot(df1,aes(Date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  #scale_color_viridis(labels=c("Brent","WTI"),discrete=TRUE)+   
  scale_colour_brewer(NULL,labels=c("WTI Crude Prices","NY Harbour Refined Products","Brent Crude Prices","Gulf Coast Refined Products"),type = "seq", palette = "Paired", direction = -1)+
  scale_x_date(date_breaks = "6 months", date_labels =  "%m %Y",limits=c(as.Date("2007-07-01"),as.Date("2018-01-01"))) +
  #scale_colour_manual(labels=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  scale_y_continuous(limits=c(0,150),expand=c(0,0))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=60, hjust=1),
  )+
  labs(y="Prices and 3:2:1 Crack Spreads ($US/bbl)",x="Year",
       title="WTI and Brent Crude Prices and Gulf Coast and NY Harbour Refined Products",
       caption="Source: EIA Data, graph by Andrew Leach.\nRefined Product Prices use 3:2:1 Assumptions")

dev.off()



#nat gas 

#NYMEX NAT GAS SPOTS 
subs<-data_fetch(KEY,cat=462457)
testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)
test <- setNames(test, t(subs$Series_IDs$name))
test2<-data.frame(date=index(test), coredata(test))
test2$date<-as.Date(test2$date,format = "%m/%d/%Y")

annual_gas<-cbind(test2$date,test2[,grep("Annual", colnames(test2))])  #all annual columns
annual_gas<-na.omit(annual_gas)
names(annual_gas)[1]<-paste("Date")  

monthly_gas<-cbind(test2$date,test2[,grep("Monthly", colnames(test2))])  #all monthly columns
monthly_gas<-na.omit(monthly_gas)
names(monthly_gas)[1]<-paste("Date")

weekly_gas<-cbind(test2$date,test2[,grep("Weekly", colnames(test2))])  #all weekly_gas columns
weekly_gas<-na.omit(weekly_gas)
names(weekly_gas)[1]<-paste("Date")
weekly_gas<-subset(weekly_gas, Date > as.Date("2007-07-18"))

monthly<-merge(monthly_crude,monthly_gas,by="Date")

monthly$HHBOE<-monthly$Henry.Hub.Natural.Gas.Spot.Price..Monthly*5.5513652248856 
monthly$NGLBOE<-monthly$U.S..Natural.Gas.Liquid.Composite.Price..Monthly*5.5513652248856
monthly$ULSD<-monthly$New.York.Harbor.Ultra.Low.Sulfur.No.2.Diesel.Spot.Price..Monthly*42
monthly$gas<-monthly$New.York.Harbor.Conventional.Gasoline.Regular.Spot.Price.FOB..Monthly*42
df1<-melt(monthly,id=c("Date"),measure.vars = c("Europe.Brent.Spot.Price.FOB..Monthly","Cushing..OK.WTI.Spot.Price.FOB..Monthly","HHBOE","NGLBOE","ULSD","gas"))
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png(file="boe_oil_gas_ngls.png")
ggplot(df1,aes(Date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  #scale_color_viridis(labels=c("Brent","WTI"),discrete=TRUE)+   
  scale_colour_brewer(NULL,labels=c("Brent Crude","WTI Crude","Natural Gas","NGL Composite","Diesel","Gasoline"),type = "seq", palette = "Paired", direction = -1)+
  scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y",limits=c(as.Date("2009-01-01"),max(df1$Date))) +
  #scale_colour_manual(labels=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  scale_y_continuous(limits=c(0,150),expand=c(0,0))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=60, hjust=1),
  )+    labs(y="Spot Price ($US/boe)",x="Year",
             title="Crude Oil, Natural Gas, NGLs and Refined Product Spot Prices",
             subtitle="Source: EIA Data, graph by Andrew Leach.")
dev.off()


df1<-melt(weekly_gas,id=c("Date"),measure.vars = c("Natural.Gas.Futures.Contract.1..Weekly"))
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png(file="hh_spots.png")
ggplot(df1,aes(Date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  #scale_color_viridis(labels=c("Brent","WTI"),discrete=TRUE)+   
  scale_colour_brewer(NULL,labels=c("Henry Hub Natural Gas Prices"),type = "seq", palette = "Paired", direction = -1)+
  scale_x_date(date_breaks = "12 months", date_labels =  "%b\n%Y",limits=c(min(df1$Date),max(df1$Date))) +
  #scale_colour_manual(labels=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  scale_y_continuous(limits=c(0,15),expand=c(0,0))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black"),
  )+    labs(y="Spot Price ($US/mmbtu)",x="Year",
             title="Natural Gas Prices",
             caption="Source: EIA Data, graph by Andrew Leach.")
dev.off()



#US total gas and oil production
#gas : http://api.eia.gov/series/?api_key=YOUR_API_KEY_HERE&series_id=NG.N9070US2.M
#oil : http://api.eia.gov/series/?api_key=YOUR_API_KEY_HERE&series_id=PET.MCRFPUS2.M

#Crude oil data
subs<-data_fetch(KEY,cat=296686)
testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)
test <- setNames(test, t(subs$Series_IDs$name))
crude_data<-data.frame(date=index(test), coredata(test))
crude_data$date<-as.Date(crude_data$date,format = "%m/%d/%Y")

#Natural gas data
subs<-data_fetch(KEY,cat=457054)
testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)
test <- setNames(test, t(subs$Series_IDs$name))
gas_data<-data.frame(date=index(test), coredata(test))
gas_data$date<-as.Date(gas_data$date,format = "%m/%d/%Y")


gas_prod<-gas_data[,c(1,grep("U.S..Dry.Natural.Gas.Production..Monthly",names(gas_data)))]
oil_prod<-crude_data[,c(1,grep("U.S..Field.Production.of.Crude.Oil..Monthly.1",names(crude_data)))]
test<-na.omit(merge(gas_prod,oil_prod,by="date"))
names(test)<-c("date","gas_prod","oil_prod")

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png(file="oil_prod.png")
ggplot(test,aes(date,oil_prod)) +
  geom_line(size=1.7)+
  scale_colour_brewer(NULL,labels=c("US Field Production of Crude Oil (mmbbl/d)"),type = "seq", palette = "Paired", direction = -1)+
  scale_x_date(date_breaks = "24 months", date_labels =  "%b\n%Y",limits=c(min(test$date),max(test$date))) +
  #scale_colour_manual(labels=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  scale_y_continuous(breaks=seq(0,10000,2000),limits=c(0,11000),expand=c(0,0))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black"),
  )+    labs(y="US Field Production of Crude Oil (mmbbl/d)",x="",
             title="US Field Production of Crude Oil (mmbbl/d)",
             caption="Source: EIA Data, graph by Andrew Leach.")
dev.off()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png(file="gas_prod.png")
ggplot(test,aes(date,gas_prod/1000/days_in_month(month(date)))) +
  geom_line(size=1.7)+
  scale_colour_brewer(NULL,labels=c("US Dry Natural Gas Production"),type = "seq", palette = "Paired", direction = -1)+
  scale_x_date(date_breaks = "24 months", date_labels =  "%b\n%Y",limits=c(min(test$date),max(test$date))) +
  #scale_colour_manual(labels=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  scale_y_continuous(breaks=seq(0,80,10),limits=c(0,90),expand=c(0,0))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black"),
  )+    labs(y="Production (monthly, billion cubic feet per day)",x="",
             title="US Dry Natural Gas Production",
             caption="Source: EIA Data, graph by Andrew Leach.")
dev.off()




#Refinery acquisition costs


subs<-data_fetch(KEY,cat=293735)

testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
test2<-data.frame(date=index(test), coredata(test))
test2$date<-as.Date(test2$date,format = "%m/%d/%Y")

annuals<-cbind(test2$date,test2[,grep("Annual", colnames(test2))])  #all annual columns
#annuals<-na.omit(annuals)
names(annuals)[1]<-paste("Date")  


monthly<-cbind(test2$date,test2[,grep("Monthly", colnames(test2))])  #all monthly columns
monthly<-na.omit(monthly)
names(monthly)[1]<-paste("Date")

#name_list<-names(monthly)
#make list of names to correspond to commodity list
name_list<- c("20 degrees or less", "20 to 25 degrees", "25 to 30 degrees",
             "30 to 35 degrees", "35 to 40 degrees" ,"40 to 45 degrees", "45 degrees or more")


df1<-melt(monthly,id=c("Date"))
file_name<-"US_fob_costs_API.png"
png(file=file_name, width = 1400, height = 750,res=130,type='cairo')
ggplot(subset(df1,Date>"2007-01-01"),aes(Date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  scale_color_viridis("",labels=name_list[],discrete=TRUE,option="C")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2007-07-01"),min(df1$Date)),Sys.Date())) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
  )+
  labs(y="Crude Oil FOB Costs ($/bbl)",x="Year",
       title=paste("US Crude Refiner Acquisition Costs by API Gravity"),
       caption="Source: EIA API, graph by Andrew Leach.")
dev.off()

#refiner costs of imported crude by area
subs<-data_fetch(KEY,cat=293676)

testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
test2<-data.frame(date=index(test), coredata(test))
test2$date<-as.Date(test2$date,format = "%m/%d/%Y")

annuals<-cbind(test2$date,test2[,grep("Annual", colnames(test2))])  #all annual columns
#annuals<-na.omit(annuals)
names(annuals)[1]<-paste("Date")  


monthly<-cbind(test2$date,test2[,grep("Monthly", colnames(test2))])  #all monthly columns
monthly<-na.omit(monthly)
names(monthly)[1]<-paste("Date")

#name_list<-names(monthly)
#make list of names to correspond to commodity list
name_list<- c("East.Coast (PADD 1)","Midwest (PADD 2)", "Gulf Coast (PADD 3)",
               "Rocky Mountain (PADD 4)",
              "West Coast (PADD5)",
              "U.S Average")


df1<-melt(monthly,id=c("Date"))
file_name<-"US_fob_costs_PADD.png"
png(file=file_name, width = 1400, height = 750,res=130,type='cairo')
ggplot(subset(df1,Date>"2007-01-01"),aes(Date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  scale_color_viridis("",labels=name_list[],discrete=TRUE,option="C")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2007-07-01"),min(df1$Date)),Sys.Date())) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
  )+
  labs(y="Crude Oil Refinery Acquisition Costs ($/bbl)",x="Year",
       title=paste("US Crude Refiner Acquisition Costs for Imported Barrels by Region"),
       caption="Source: EIA API, graph by Andrew Leach.")
dev.off()


#API of imported crudes


subs<-data_fetch(KEY,cat=293787)

testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
test2<-data.frame(date=index(test), coredata(test))
test2$date<-as.Date(test2$date,format = "%m/%d/%Y")

annuals<-cbind(test2$date,test2[,grep("Annual", colnames(test2))])  #all annual columns
#annuals<-na.omit(annuals)
names(annuals)[1]<-paste("Date")  


monthly<-cbind(test2$date,test2[,grep("Monthly", colnames(test2))])  #all monthly columns
#monthly<-na.omit(monthly)
names(monthly)[1]<-paste("Date")

#name_list<-names(monthly)
#make list of names to correspond to commodity list
name_list<- c("20 degrees or less", "20 to 25 degrees", "25 to 30 degrees",
              "30 to 35 degrees", "35 to 40 degrees" ,"40 to 45 degrees", "45 degrees or more")


df1<-melt(monthly,id=c("Date"))

file_name<-"US_import_API.png"
png(file=file_name, width = 1400, height = 750,res=130,type='cairo')
ggplot(subset(df1,Date>"2007-01-01"),aes(Date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  scale_color_viridis("",labels=name_list[],discrete=TRUE,option="D",direction=-1)+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2007-07-01"),min(df1$Date)),Sys.Date())) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
  )+
  labs(y="Share of Imported Crude",x="Year",
       title=paste("US Crude Import Market Share by API Gravity"),
       caption="Source: EIA API, graph by Andrew Leach.")
dev.off()


#US imports by grade


subs<-data_fetch(KEY,cat=1292190)

testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
test2<-data.frame(date=index(test), coredata(test))
test2$date<-as.Date(test2$date,format = "%m/%d/%Y")

annuals<-cbind(test2$date,test2[,grep("Annual", colnames(test2))])  #all annual columns
#annuals<-na.omit(annuals)
names(annuals)[1]<-paste("Date")  


monthly<-cbind(test2$date,test2[,grep("Monthly", colnames(test2))])  #all monthly columns
#monthly<-na.omit(monthly)
names(monthly)[1]<-paste("Date")

#name_list<-names(monthly)
#make list of names to correspond to commodity list
name_list<- c("All grades","Heavy, sour crude","Heavy, sweet crude","Light, sour crude",   
              "Light, sweet crude","Medium crude")


df1<-melt(monthly,id=c("Date"))

file_name<-"US_import_by_grade.png"
png(file=file_name, width = 1400, height = 800,res=130,type='cairo')
ggplot(subset(df1,Date>"2007-01-01"),aes(Date,value/1000/days_in_month(Date),group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  scale_color_viridis("",labels=name_list[],discrete=TRUE,option="D",direction=-1)+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2007-07-01"),min(df1$Date)),Sys.Date())) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
  )+
  labs(y="Imports of Crude Oil (millions of barrels per day)",x="Year",
       title=paste("US Crude Imports by Grade"),
       caption="Source: EIA API, graph by Andrew Leach.")
dev.off()



#US Refinery yield


subs<-data_fetch(KEY,cat=304183)
testing<-t(subs$Series_IDs$series_id)

testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)
test <- setNames(test, t(subs$Series_IDs$name))
test2<-data.frame(date=index(test), coredata(test))
test2$date<-as.Date(test2$date,format = "%m/%d/%Y")

annuals<-cbind(test2$date,test2[,grep("Annual", colnames(test2))])  #all annual columns
annuals<-na.omit(annuals)
names(annuals)[1]<-paste("Date")  


monthly<-cbind(test2$date,test2[,grep("Monthly", colnames(test2))])  #all monthly columns
#monthly<-na.omit(monthly)
names(monthly)[1]<-paste("Date")

#monthly<-subset(monthly, Date > as.Date("2007-07-18"))



non_agg<- paste("U.S..Refinery.Yield.of.Distillate.Fuel.Oil..Monthly",
             "U.S..Refinery.Yield.of.Finished.Motor.Gasoline..Monthly",                   
             "U.S..Refinery.Yield.of.Kerosene.Type.Jet.Fuel..Monthly",                    
             "U.S..Refinery.Yield.of.Residual.Fuel.Oil..Monthly",sep="|")
monthly$Other<-100+monthly$U.S..Refinery.Processing.Gain..Monthly
monthly$Other<-monthly$Other-rowSums(monthly[,grep(non_agg,names(monthly), invert = FALSE)],na.rm=TRUE)

#totals<-paste("Date","U.S..Refinery.Yield.of.Hydrocarbon.Gas.Liquids..Monthly",sep="|")
#dates<-monthly$Date
#monthly<-monthly[,grep(totals,names(monthly), invert = TRUE)]*
#  monthly[,grep("U.S..Refinery.Yield.of.Hydrocarbon.Gas.Liquids..Monthly",names(monthly), invert = FALSE)]
#monthly<-cbind(dates,monthly)
#names(monthly)[1]<-paste("Date")

df1<-melt(monthly,id=c("Date"),measure.vars = c("U.S..Refinery.Yield.of.Distillate.Fuel.Oil..Monthly",
                                                "U.S..Refinery.Yield.of.Finished.Motor.Gasoline..Monthly",                   
                                                "U.S..Refinery.Yield.of.Kerosene.Type.Jet.Fuel..Monthly",                    
                                                "U.S..Refinery.Yield.of.Residual.Fuel.Oil..Monthly",                         
                                                #"U.S..Refinery.Yield.of.Asphalt.and.Road.Oil..Monthly",                      
                                                #"U.S..Refinery.Yield.of.Petroleum.Coke..Monthly",                            
                                                #"U.S..Refinery.Yield.of.Aviation.Gasoline..Monthly",                         
                                                #"U.S..Refinery.Yield.of.Kerosene..Monthly",                                  
                                                #"U.S..Refinery.Yield.of.Lubricants..Monthly",                                
                                                #"U.S..Refinery.Yield.of.Miscellaneous.Petroleum.Products..Monthly",          
                                                #"U.S..Refinery.Yield.of.Naphtha.for.Petrochemical.Feedstock.Use..Monthly",   
                                                #"U.S..Refinery.Yield.of.Special.Naphthas..Monthly",                          
                                                #"U.S..Refinery.Yield.of.Other.Oils.for.Petrochemical.Feedstock.Use..Monthly",
                                                #"U.S..Refinery.Processing.Gain..Monthly",                                    
                                                #"U.S..Refinery.Yield.of.Still.Gas..Monthly",                                 
                                                #"U.S..Refinery.Yield.of.Waxes..Monthly",                                     
                                                #"U.S..Refinery.Yield.of.Hydrocarbon.Gas.Liquids..Monthly",
                                                "Other"))


df1$Date<-as.Date(df1$Date,format = "%m/%d/%Y")


png(file="Ref_yield.png", width = 1770, height = 1018,res=130,type='cairo')
#jpeg(file="AESO_LTO.jpg", width = 1400, height = 750)
ggplot(df1,aes(Date,value,group = variable,colour=variable,fill=variable)) +
  #geom_line(size=1.7) +
  #geom_point(size=1) +
  geom_area(position="stack")+
  scale_fill_viridis("Product",labels=c("Diesel","Gasoline","Jet","Resid","Other"),discrete=TRUE,option="D")+   
  scale_colour_viridis("",labels=c("Diesel","Gasoline","Jet","Resid","Other"),discrete=TRUE,guide = 'none')+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
    #scale_colour_brewer(NULL,labels=c("Brent","WTI"),type = "seq", palette = "Paired", direction = -1)+
  scale_x_date(date_breaks = "1 years", date_labels =  "%Y",limits=c(as.Date("1997-07-01"),max(df1$Date))) +
  scale_y_continuous(limits=c(0,100),expand=c(0,0))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=60, hjust=1),
  )+    labs(y="Monthly Refinery Yield (%)",x="Year",
             title="US Refinery Yields",
             caption="Source: EIA Data Category 304183, graph by Andrew Leach.")

dev.off()


#weekly_supply_estimates
#235081
subs<-data_fetch(KEY,cat=235081)

testing<-t(subs$Series_IDs$series_id[-grep(".4",subs$Series_IDs$series_id)]) #take out the 4-week averages
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)
set_names<-t(subs$Series_IDs$name[-grep(".4",subs$Series_IDs$series_id)])
test <- setNames(test,set_names)
weekly<-data.frame(date=index(test), coredata(test))
weekly$date<-as.Date(weekly$date,format = "%m/%d/%Y")

name_list<- gsub("\\.", " ", names(weekly))
name_list<- gsub("\\U S", "US", name_list)
name_list<- gsub("  ", " ", name_list)
name_list<- gsub(" Weekly", "", name_list)
name_list<- gsub("US ", "", name_list)
weekly <- setNames(weekly,name_list)

weekly$max_imports<-as.data.frame(rollapply(weekly$`Imports of Crude Oil`,52*5,max,fill=NA,align = c("right")))[,1]
weekly$min_exports<-as.data.frame(rollapply(weekly$`Imports of Crude Oil`,52*5,min,fill=NA,align = c("right")))[,1]
weekly$avg_imports<-as.data.frame(rollapply(weekly$`Imports of Crude Oil`,52*5,mean,fill=NA,align = c("right")))[,1]

df1<-melt(weekly,id=c("date","min_imports","max_imports"),measure.vars = c("Imports of Crude Oil","avg_imports"))
df1<-na.omit(df1)




ggplot(df1,aes(date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  geom_ribbon(aes(x = date, ymin = min_imports, ymax = max_imports,fill="grey80"),alpha=.25, linetype = 0)+
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="C",labels=c("Weekly Imports", "5 Year Moving Average Imports"))+
  scale_fill_viridis("",discrete=TRUE,option="C",labels="5 Year Range")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2000-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
  )+
  labs(y="Crude Oil Imports \n(Weekly, Thousands of Barrels per Day)",x="Year",
       title=paste("US Imports of Crude Oil",sep=""),
       caption="Source: EIA API, graph by Andrew Leach.")




subs<-data_fetch(KEY,cat=235081)

testing<-t(subs$Series_IDs$series_id[-grep(".4",subs$Series_IDs$series_id)]) #take out the 4-week averages
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)
set_names<-t(subs$Series_IDs$name[-grep(".4",subs$Series_IDs$series_id)])
test <- setNames(test,set_names)
weekly<-data.frame(date=index(test), coredata(test))
weekly$date<-as.Date(weekly$date,format = "%m/%d/%Y")

name_list<- gsub("\\.", " ", names(weekly))
name_list<- gsub("\\U S", "US", name_list)
name_list<- gsub("  ", " ", name_list)
name_list<- gsub(" Weekly", "", name_list)
name_list<- gsub("US ", "", name_list)
weekly <- setNames(weekly,name_list)

weekly$max_imports<-as.data.frame(rollapply(weekly$`Imports of Crude Oil`,52*5,max,fill=NA,align = c("right")))[,1]
weekly$min_imports<-as.data.frame(rollapply(weekly$`Imports of Crude Oil`,52*5,min,fill=NA,align = c("right")))[,1]
weekly$avg_imports<-as.data.frame(rollapply(weekly$`Imports of Crude Oil`,52*5,mean,fill=NA,align = c("right")))[,1]

df1<-melt(weekly,id=c("date","min_imports","max_imports"),measure.vars = c("Imports of Crude Oil","avg_imports"))
df1<-na.omit(df1)



ggplot(df1,aes(date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  geom_ribbon(aes(x = date, ymin = min_imports, ymax = max_imports,fill="grey80"),alpha=.25, linetype = 0)+
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="C",labels=c("Weekly Imports", "5 Year Moving Average Imports"))+
  scale_fill_viridis("",discrete=TRUE,option="C",labels="5 Year Range")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2000-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
  )+
  labs(y="Crude Oil Imports \n(Weekly, Thousands of Barrels per Day)",x="Year",
       title=paste("US Imports of Crude Oil",sep=""),
       caption="Source: EIA API, graph by Andrew Leach.")



weekly$max_exports<-as.data.frame(rollapply(weekly$`Exports of Crude Oil`,52*5,max,fill=NA,align = c("right")))[,1]
weekly$min_exports<-as.data.frame(rollapply(weekly$`Exports of Crude Oil`,52*5,min,fill=NA,align = c("right")))[,1]
weekly$avg_exports<-as.data.frame(rollapply(weekly$`Exports of Crude Oil`,52*5,mean,fill=NA,align = c("right")))[,1]

df1<-melt(weekly,id=c("date","min_exports","max_exports"),measure.vars = c("Exports of Crude Oil","avg_exports"))
df1<-na.omit(df1)



ggplot(data=subset(df1,df1$date>"2013-01-01"),aes(date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  geom_ribbon(aes(x = date, ymin = min_exports, ymax = max_exports,fill="grey80"),alpha=.25, linetype = 0)+
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="C",labels=c("Weekly Exports", "5 Year Moving Average Exports"))+
  scale_fill_viridis("",discrete=TRUE,option="C",labels="5 Year Range")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Exports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "6 months", date_labels =  "%b\n%Y",limits=c(max(as.Date("2013-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Exports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black", hjust=1),
  )+
  labs(y="Crude Oil Exports \n(Weekly, Thousands of Barrels per Day)",x="Year",
       title=paste("US Exports of Crude Oil",sep=""),
       caption="Source: EIA API, graph by Andrew Leach.")



#US Imports and Exports

#Imports
subs<-data_fetch(KEY,cat=314563)

testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
imports<-data.frame(date=index(test), coredata(test))
imports$date<-as.Date(imports$date,format = "%m/%d/%Y")

#Exports

subs<-data_fetch(KEY,cat=315919)

testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
exports<-data.frame(date=index(test), coredata(test))
exports$date<-as.Date(exports$date,format = "%m/%d/%Y")

test2<-merge(exports,imports,by="date")




annuals<-cbind(test2$date,test2[,grep("Annual", colnames(test2))])  #all annual columns
#annuals<-na.omit(annuals)
names(annuals)[1]<-paste("Date")  


monthly<-cbind(test2$date,test2[,grep("Monthly", colnames(test2))])  #all monthly columns
#monthly<-na.omit(monthly)
names(monthly)[1]<-paste("Date")

weekly<-cbind(test2$date,test2[,grep("Weekly", colnames(test2))])  #all weekly columns
#weekly_crude<-na.omit(weekly_crude)
names(weekly)[1]<-paste("Date")

monthly[,"U.S..Exports.of.Petroleum.Products..Monthly.1"]<-monthly$U.S..Exports.of.Crude.Oil.and.Petroleum.Products..Monthly.1-monthly$U.S..Exports.of.Crude.Oil..Monthly.1

monthly[,"U.S..Imports.of.Petroleum.Products..Monthly.1"]<-monthly$U.S..Imports.of.Crude.Oil.and.Petroleum.Products..Monthly.1-monthly$U.S..Imports.of.Crude.Oil..Monthly.1


               
commod_list<-c("Conventional.Motor.Gasoline..Monthly.1",
               "Crude.Oil.and.Petroleum.Products..Monthly.1",
               "Residual.Fuel.Oil..Monthly.1",
               "Propane..Monthly.1",
               "Normal.Butane..Monthly.1",
               "Isobutane..Monthly.1",
               "Pentanes.Plus..Monthly.1",
               "Natural.Gas.Liquids..Monthly.1",
               "Natural.Gasoline..Monthly.1",
               "Kerosene.Type.Jet.Fuel..Monthly.1",
               "Distillate.Fuel.Oil..Monthly.1",
               "Distillate.Fuel.Oil..0.to.15.ppm.Sulfur..Monthly.1",
               "Crude.Oil..Monthly.1",
               "Petroleum.Products..Monthly.1"
               )
#make list of names to correspond to commodity list
 name_list<- gsub("\\.", " ", commod_list)
 name_list<- gsub("\\U S", "US", name_list)
 name_list<- gsub("  ", " ", name_list)
 name_list<- gsub(" Monthly 1", "", name_list)
 name_list<- gsub(" to US", "", name_list)
 
for(commod_base in seq(1,length(commod_list))){
#for(commod_base in c(4)){
#commod_base<-3
  export_field<-paste("U.S..Exports.of.",commod_list[commod_base],sep = "")
  import_field<-paste("U.S..Imports.of.",commod_list[commod_base],sep = "")
    if(commod_list[commod_base]=="Natural.Gas.Liquids..Monthly.1"){
      export_field<-paste("U.S..Exports.to.U.S..of.",commod_list[commod_base],sep = "")
      import_field<-paste("U.S..Imports.From.U.S..of.",commod_list[commod_base],sep = "")
      }
  if(commod_list[commod_base]=="Natural.Gasoline..Monthly.1"){
    export_field<-paste("U.S..Exports.to.U.S..of.",commod_list[commod_base],sep = "")
    import_field<-paste("U.S..Imports.From.U.S..of.",commod_list[commod_base],sep = "")
  }
  #use $ for fixed ending, to avoid subscripts
  commod<- paste(import_field,export_field,sep="|")
  sub_samp<-monthly[,c(1,grep(commod,names(monthly)))]
  names(sub_samp)[2]<-paste("Exports of ",name_list[commod_base],sep="")
  names(sub_samp)[3]<-paste("Imports of ",name_list[commod_base],sep="")
  sub_samp$NetExports<-sub_samp[,3]-sub_samp[,2]
  names(sub_samp)[4]<-paste("Net Imports of ",name_list[commod_base],sep="")
  file_name<-paste("trade_",name_list[commod_base],".png",sep="")
  sub_samp<-na.omit(sub_samp)
  #totals<-paste("Date","U.S..Refinery.Yield.of.Hydrocarbon.Gas.Liquids..Monthly",sep="|")
  #dates<-monthly$Date
  #monthly<-monthly[,grep(totals,names(monthly), invert = TRUE)]*
  #  monthly[,grep("U.S..Refinery.Yield.of.Hydrocarbon.Gas.Liquids..Monthly",names(monthly), invert = FALSE)]
  #monthly<-cbind(dates,monthly)
  #names(monthly)[1]<-paste("Date")
  
  df1<-melt(sub_samp,id=c("Date"))
  df1$Date<-as.Date(df1$Date,format = "%m/%d/%Y")
  
  
  
  #jpeg(file="AESO_LTO.jpg", width = 1400, height = 750)
  print("making graph")
  png(file=file_name, width = 1400, height = 750,res=130,type='cairo')
  graph<-ggplot(subset(df1,Date>"2007-01-01"),aes(Date,value,group = variable,colour=variable)) +
    geom_line(size=1.7) +
    #geom_point(size=1) +
    scale_color_viridis("",labels=c(names(sub_samp)[2],names(sub_samp)[3],names(sub_samp)[4]),discrete=TRUE,option="C")+   
    #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2007-07-01"),min(df1$Date)),Sys.Date())) +
    #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
    #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
    #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
    guides(colour=guide_legend(nrow=2,byrow=TRUE))+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 14,face = "bold"),
      axis.text.y =element_text(size = 14,face = "bold", colour="black"),
      axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
    )+
    labs(y="Imports and Exports\n(Monthly, Thousands of Barrels per Day)",x="Year",
         title=paste("US Imports and Exports of ",name_list[commod_base],sep=""),
         caption="Source: EIA API, graph by Andrew Leach.")
  print(graph)
  dev.off()
}



 #production
 
 
 subs<-data_fetch(KEY,cat=296686)
 
 testing<-t(subs$Series_IDs$series_id)
 testing<- as.character(testing)
 test<- pdfetch_EIA(testing,KEY)
 
 test <- setNames(test, t(subs$Series_IDs$name))
 production<-data.frame(date=index(test), coredata(test))
 production$date<-as.Date(production$date,format = "%m/%d/%Y")
 
 test2<-production
 
 annuals<-cbind(test2$date,test2[,grep("Annual", colnames(test2))])  #all annual columns
 #annuals<-na.omit(annuals)
 names(annuals)[1]<-paste("Date")  
 
 
 monthly<-cbind(test2$date,test2[,grep("Monthly", colnames(test2))])  #all monthly columns
 #monthly<-na.omit(monthly)
 names(monthly)[1]<-paste("Date")
 
 commod_list<-c("U.S..Field.Production.of.Crude.Oil..Monthly.1",
                "Texas.Field.Production.of.Crude.Oil..Monthly.1",  
                "North.Dakota.Field.Production.of.Crude.Oil..Monthly.1",
                "Alaska.North.Slope.Crude.Oil.Production..Monthly.1",
                #"Federal.Offshore..Gulf.of.Mexico.Field.Production.of.Crude.Oil..Monthly.1",
                #"Federal.Offshore.PADD.5.Field.Production.of.Crude.Oil..Monthly.1",
                "California.Field.Production.of.Crude.Oil..Monthly.1" 
                )
 
cols<-paste("U.S..Field.Production.of.Crude.Oil..Monthly.1",
                "Texas.Field.Production.of.Crude.Oil..Monthly.1",  
                "North.Dakota.Field.Production.of.Crude.Oil..Monthly.1",
                "Alaska.North.Slope.Crude.Oil.Production..Monthly.1",
                #"Federal.Offshore..Gulf.of.Mexico.Field.Production.of.Crude.Oil..Monthly.1",
                #"Federal.Offshore.PADD.5.Field.Production.of.Crude.Oil..Monthly.1",
                "California.Field.Production.of.Crude.Oil..Monthly.1",sep = "|")
 
 sub_samp<-monthly[,c(1,grep(cols,names(monthly)))]
  
 commod_list<-names(sub_samp)
 #make list of names to correspond to commodity list
 name_list<- gsub("\\.", " ", commod_list)
 name_list<- gsub("\\U S", "US", name_list)
 name_list<- gsub("  ", " ", name_list)
 name_list<- gsub(" Monthly 1", "", name_list)
 name_list<- gsub(" Field Production of Crude Oil", "", name_list)
 name_list<- gsub(" Crude Oil Production", "", name_list)
 df1<-melt(sub_samp,id=c("Date"))
 df1$Date<-as.Date(df1$Date,format = "%m/%d/%Y")
   
   
  file_name<-"US_production_10.png"
  png(file=file_name, width = 1400, height = 750,res=130,type='cairo')
   ggplot(subset(df1,Date>"2007-01-01"),aes(Date,value,group = variable,colour=variable)) +
     geom_line(size=1.7) +
     #geom_point(size=1) +
     scale_color_viridis("",labels=name_list[-1],discrete=TRUE,option="C")+   
     #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
     scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2007-07-01"),min(df1$Date)),Sys.Date())) +
     #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
     #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
     #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
     guides(colour=guide_legend(nrow=2,byrow=TRUE))+
     theme_minimal()+theme(
       legend.position = "bottom",
       legend.margin=margin(c(0,0,0,0),unit="cm"),
       legend.text = element_text(colour="black", size = 12),
       plot.caption = element_text(size = 14, face = "italic"),
       plot.title = element_text(face = "bold"),
       plot.subtitle = element_text(size = 14, face = "italic"),
       panel.grid.minor = element_blank(),
       text = element_text(size = 14,face = "bold"),
       axis.text.y =element_text(size = 14,face = "bold", colour="black"),
       axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
     )+
     labs(y="Crude Oil Production (1000s of barrels per day)",x="Year",
          title=paste("US Crude Oil Production, Selected States"),
          caption="Source: EIA API, graph by Andrew Leach.")
   dev.off()
   target_date<-as.Date("1983-01-01")
   
   file_name<-"US_production_long.png"
   png(file=file_name, width = 1400, height = 750,res=130,type='cairo')
   ggplot(subset(df1,Date>target_date),aes(Date,value,group = variable,colour=variable)) +
     geom_line(size=1.7) +
     #geom_point(size=1) +
     scale_color_viridis("",labels=name_list[-1],discrete=TRUE,option="C")+   
     #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
     scale_x_date(date_breaks = "5 year", date_labels =  "%Y",limits=c(max(target_date,min(df1$Date)),Sys.Date())) +
     #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
     #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
     #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
     guides(colour=guide_legend(nrow=2,byrow=TRUE))+
     theme_minimal()+theme(
       legend.position = "bottom",
       legend.margin=margin(c(0,0,0,0),unit="cm"),
       legend.text = element_text(colour="black", size = 12),
       plot.caption = element_text(size = 14, face = "italic"),
       plot.title = element_text(face = "bold"),
       plot.subtitle = element_text(size = 14, face = "italic"),
       panel.grid.minor = element_blank(),
       text = element_text(size = 14,face = "bold"),
       axis.text.y =element_text(size = 14,face = "bold", colour="black"),
       axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
     )+
     labs(y="Crude Oil Production (1000s of barrels per day)",x="Year",
          title=paste("US Crude Oil Production, Selected States"),
          caption="Source: EIA API, graph by Andrew Leach.")
   dev.off()
   
   target_date<-as.Date("2010-09-28")
   file_name<-"US_production_5.png"
   png(file=file_name, width = 1400, height = 750,res=130,type='cairo')
   ggplot(subset(df1,Date>target_date),aes(Date,value,group = variable,colour=variable)) +
     geom_line(size=1.7) +
     #geom_point(size=1) +
     scale_color_viridis("",labels=name_list[-1],discrete=TRUE,option="C")+   
     #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
     scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(target_date,max(df1$Date))) +
     #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
     #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
     #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
     guides(colour=guide_legend(nrow=2,byrow=TRUE))+
     theme_minimal()+theme(
       legend.position = "bottom",
       legend.margin=margin(c(0,0,0,0),unit="cm"),
       legend.text = element_text(colour="black", size = 12),
       plot.caption = element_text(size = 14, face = "italic"),
       plot.title = element_text(face = "bold"),
       plot.subtitle = element_text(size = 14, face = "italic"),
       panel.grid.minor = element_blank(),
       text = element_text(size = 14,face = "bold"),
       axis.text.y =element_text(size = 14,face = "bold", colour="black"),
       axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
     )+
     labs(y="Crude Oil Production (1000s of barrels per day)",x="Year",
          title=paste("US Crude Oil Production, Selected States"),
          caption="Source: EIA API, graph by Andrew Leach.")
   dev.off() 
 
 
   
  
   
#imports by country

#Imports from Canada
subs<-data_fetch(KEY,cat=323934)
testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
cda_imports<-data.frame(date=index(test), coredata(test))
cda_imports$date<-as.Date(cda_imports$date,format = "%m/%d/%Y")


#Exports to Canada
subs<-data_fetch(KEY,cat=316977)
testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
cda_exports<-data.frame(date=index(test), coredata(test))
cda_exports$date<-as.Date(cda_exports$date,format = "%m/%d/%Y")



test2<-merge(cda_exports,cda_imports,by="date")




annuals<-cbind(test2$date,test2[,grep("Annual", colnames(test2))])  #all annual columns
#annuals<-na.omit(annuals)
names(annuals)[1]<-paste("Date")  


monthly<-cbind(test2$date,test2[,grep("Monthly", colnames(test2))])  #all monthly columns
#monthly<-na.omit(monthly)
names(monthly)[1]<-paste("Date")


commod_list<-c("Conventional.Motor.Gasoline..Monthly",
               "Crude.Oil.and.Petroleum.Products..Monthly",
               "Residual.Fuel.Oil..Monthly",
               "Propane..Monthly",
               "Normal.Butane..Monthly",
               "Isobutane..Monthly",
               "Pentanes.Plus..Monthly",
               "Natural.Gas.Liquids..Monthly",
               #"Natural.Gasoline..Monthly",
               "Kerosene.Type.Jet.Fuel..Monthly",
               "Distillate.Fuel.Oil..Monthly",
               "Distillate.Fuel.Oil..0.to.15.ppm.Sulfur..Monthly",
               "Total.Petroleum.Products..Monthly",
               "Crude.Oil..Monthly"
               )
#make list of names to correspond to commodity list
name_list<- gsub("\\.", " ", commod_list)
name_list<- gsub("\\U S", "US", name_list)
name_list<- gsub("  ", " ", name_list)
name_list<- gsub(" Monthly", "", name_list)
name_list<- gsub(" to US", "", name_list)

for(commod_base in seq(1,length(commod_list))){
  #for(commod_base in c(4)){
  #commod_base<-7
  print(paste("Starting for ",commod_list[commod_base]))
  export_field<-paste("U.S..Exports.to.Canada.of.",commod_list[commod_base],".1",sep = "")
  import_field<-paste("U.S..Net.Imports.from.Canada.of.",commod_list[commod_base],"$",sep = "")
  #use $ for fixed ending, to avoid subscripts
  if(commod_list[commod_base]=="Natural.Gas.Liquids..Monthly")
        import_field<-paste("U.S..Net.Imports.From.Canada.of.",commod_list[commod_base],sep = "")
  
  
  commod<- paste(import_field,export_field,sep="|") #$for exact match
  sub_samp<-monthly[,c(1,grep(commod,names(monthly)))]
  names(sub_samp)[2]<-paste("Exports of ",name_list[commod_base],sep="")
  names(sub_samp)[3]<-paste("Net Imports of ",name_list[commod_base],sep="")
  sub_samp$Imports<-sub_samp[,3]+sub_samp[,2]
  names(sub_samp)[4]<-paste("Imports of ",name_list[commod_base],sep="")
  sub_samp<-na.omit(sub_samp)
  
  
  
  
  #totals<-paste("Date","U.S..Refinery.Yield.of.Hydrocarbon.Gas.Liquids..Monthly",sep="|")
  #dates<-monthly$Date
  #monthly<-monthly[,grep(totals,names(monthly), invert = TRUE)]*
  #  monthly[,grep("U.S..Refinery.Yield.of.Hydrocarbon.Gas.Liquids..Monthly",names(monthly), invert = FALSE)]
  #monthly<-cbind(dates,monthly)
  #names(monthly)[1]<-paste("Date")
  
  df1<-melt(sub_samp,id=c("Date"),measure.vars = c(names(sub_samp)[2],names(sub_samp)[4]))
  df1$Date<-as.Date(df1$Date,format = "%m/%d/%Y")
  
  
  file_name<-paste("Cda_trade_",name_list[commod_base],".png",sep="")
  #jpeg(file="AESO_LTO.jpg", width = 1400, height = 750)
  print("making graph")
  png(file=file_name, width = 1400, height = 750,res=130,type='cairo')
  graph<-ggplot(subset(df1,Date>"2007-01-01"),aes(Date,value,group = variable,colour=variable)) +
    geom_line(size=1.7) +
    #geom_line(size=1.7) +
    #geom_point(size=1) +
    scale_color_viridis("",labels=c(names(sub_samp)[2],names(sub_samp)[4]),discrete=TRUE,option="C")+   
    #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2007-07-01"),min(df1$Date)),Sys.Date())) +
    #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
    #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
    #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
    guides(colour=guide_legend(nrow=2,byrow=TRUE))+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 14,face = "bold"),
      axis.text.y =element_text(size = 14,face = "bold", colour="black"),
      axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
    )+
    labs(y="Imports and Exports\n(Monthly, Thousands of Barrels per Day)",x="Year",
         title=paste("US Petroleum Trade with Canada ",name_list[commod_base],sep=""),
         caption="Source: EIA API, graph by Andrew Leach.")
  print(graph)
  dev.off()

  
  df1<-melt(sub_samp,id=c("Date"),measure.vars = c(names(sub_samp)[3]))
  df1$Date<-as.Date(df1$Date,format = "%m/%d/%Y")
  
  
  file_name<-paste("Cda_net_",name_list[commod_base],".png",sep="")
  #jpeg(file="AESO_LTO.jpg", width = 1400, height = 750)
  print("making graph")
  png(file=file_name, width = 1400, height = 750,res=130,type='cairo')
  graph<-ggplot(subset(df1,Date>"2007-01-01"),aes(Date,value,group = variable,colour=variable)) +
    geom_line(size=1.7) +
    #geom_line(size=1.7) +
    #geom_point(size=1) +
    scale_color_viridis("",labels=c(names(sub_samp)[3]),discrete=TRUE,option="C")+   
    #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2007-07-01"),min(df1$Date)),Sys.Date())) +
    #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
    #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
    #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
    guides(colour=guide_legend(nrow=2,byrow=TRUE))+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 14,face = "bold"),
      axis.text.y =element_text(size = 14,face = "bold", colour="black"),
      axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
    )+
    labs(y="Imports and Exports\n(Monthly, Thousands of Barrels per Day)",x="Year",
         title=paste("US Petroleum Trade with Canada ",name_list[commod_base],sep=""),
         caption="Source: EIA API, graph by Andrew Leach.")
  print(graph)
  dev.off()
}






#Refinery acquisition costs


subs<-data_fetch(KEY,cat=459199)

testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
test2<-data.frame(date=index(test), coredata(test))
test2$date<-as.Date(test2$date,format = "%m/%d/%Y")

annuals<-data_frame(test2$date,test2[,grep("Annual", colnames(test2))])  #all annual columns
annuals<-na.omit(annuals)
colnames(annuals)<-c("Date","LNG Export Price")

monthly<-data_frame(test2$date,test2[,grep("Monthly", colnames(test2))])  #all monthly columns
monthly<-na.omit(monthly)
colnames(monthly)<-c("Date","LNG Export Price")

df1<-melt(monthly,id=c("Date"))
#file_name<-"US_fob_costs_API.png"
#png(file=file_name, width = 1400, height = 750,res=130,type='cairo')
ggplot(subset(df1,Date>"2007-01-01"),aes(Date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="C")+   
  #scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2007-07-01"),min(df1$Date)),Sys.Date())) +
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "none",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
  )+
  labs(y="US LNG Export Prices ($/MCF)",x="Year",
       title=paste("US LNG Export Prices"),
       caption="Source: EIA API, graph by Andrew Leach.")
#dev.off()





subs<-data_fetch(KEY,cat=475780)

testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
test2<-data.frame(date=index(test), coredata(test))
test2$date<-as.Date(test2$date,format = "%m/%d/%Y")

annuals<-cbind(test2$date,test2[,grep("Annual", colnames(test2))])  #all annual columns
#annuals<-na.omit(annuals)
names(annuals)[1]<-paste("Date")  


monthly<-cbind(test2$date,test2[,grep("Monthly", colnames(test2))])  #all monthly columns
#monthly<-na.omit(monthly)
names(monthly)[1]<-paste("Date")

by_dest<-cbind(monthly$Date,monthly[,-grep("Price.of", colnames(monthly))])#all monthly columns
lng_by_dest<-cbind(by_dest[,1],by_dest[,grep("Liquefied", colnames(by_dest))])  #all monthly columns
lng_by_dest<-cbind(lng_by_dest[,1],lng_by_dest[,grep("Vessel", colnames(lng_by_dest))]) #all monthly columns
names(lng_by_dest)[1]<-paste("Date")


df1<-melt(lng_by_dest,id=c("Date"))
df1<-na.omit(df1)

#df1<-df1[df1$Date<as.Date("2016-02-28"),]
#df1<-df1[df1$Date>as.Date("2015-10-31"),]
df1<-df1[df1$variable=="Liquefied.U.S..Natural.Gas.Exports.by.Vessel.and.Truck..Monthly",]

ggplot(subset(df1,Date>"2007-01-01"),aes(Date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="C")+   
  #scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2007-07-01"),min(df1$Date)),Sys.Date())) +
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "none",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
  )+
  labs(y="US LNG Export Monthly Volumes (MMCF/month)",x="Year",
       title=paste("US LNG Export Volumes"),
       caption="Source: EIA API, graph by Andrew Leach.")
#dev.off()

by_dest<-cbind(monthly$Date,monthly[,-grep("Price.of", colnames(monthly))])#all monthly columns
lng_by_dest<-cbind(by_dest[,1],by_dest[,grep("Liquefied", colnames(by_dest))])  #all monthly columns
lng_by_dest<-cbind(lng_by_dest[,1],lng_by_dest[,grep("Vessel", colnames(lng_by_dest))]) #all monthly columns
names(lng_by_dest)[1]<-paste("Date")


df1<-melt(lng_by_dest,id=c("Date"))
df1<-na.omit(df1)

#df1<-df1[df1$Date<as.Date("2016-02-28"),]
df1<-df1[df1$value>0,]
df1<-df1[df1$variable!="Liquefied.U.S..Natural.Gas.Exports.by.Vessel.and.Truck..Monthly",]
df1<-df1[df1$variable!="Liquefied.U.S..Natural.Gas.Exports.by.Vessel..Monthly",]

df1$variable<- gsub("Liquefied.U.S..Natural.Gas.Exports.by.Vessel.to.", "", df1$variable)
df1$variable<- gsub("Liquefied.U.S..Natural.Gas.Exports.by.Vessels.to.", "", df1$variable)
df1$variable<- gsub("..Monthly", "", df1$variable)
df1$variable<- gsub("\\.", " ",  df1$variable)

ggplot(subset(df1,Date>"2016-02-01"),aes(Date,value,group = variable,colour=variable)) +
  geom_bar(aes(y = value, x = Date, fill = variable),
           stat="identity")+
  #geom_point(size=1) +
  scale_fill_viridis("",discrete=TRUE,option="C")+   
  scale_colour_viridis("",discrete=TRUE,option="C")+   
  #scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2007-07-01"),min(df1$Date)),Sys.Date())) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
  )+
  labs(y="US LNG Export Monthly Volumes (MMCF/month)",x="Year",
       title=paste("US LNG Export Volumes"),
       caption="Source: EIA API, graph by Andrew Leach.")
#dev.off()



#refiner acquisition costs
subs<-data_fetch(KEY,cat=293662)
testing<-t(subs$Series_IDs$series_id) #take out the 4-week averages
testing<- as.character(testing)

#Loop to get all data from 
#US and PADDDs 1-5 weekly

series_ids<-c("235081","235213","235332","235420","235506","235589")
region<-c("United States","East Coast PADD 1","Midwest PADD 2","Gulf Coast PADD 3","Rocky Mountain PADD 4","West Coast PADD 5")
region_short<-c("US","PADD1","PADD2","PADD3","PADD4","PADD5")
#region<-region[6]
#region_short<-region_short[6]
#series_ids<-series_ids[6]
for(series_idnum in c(1:NROW(series_ids))){
  #for(series_idnum in c(1:1)){
  print(paste("Getting series for",region[series_idnum]))
  subs<-data_fetch(KEY,cat=series_ids[series_idnum])
  testing<-t(subs$Series_IDs$series_id[-grep("\\b.4\\b",subs$Series_IDs$series_id)]) #take out the 4-week averages
  testing<- as.character(testing)
  test<- pdfetch_EIA(testing,KEY)
  set_names<-t(subs$Series_IDs$name[-grep("\\b.4\\b",subs$Series_IDs$series_id)])
  test <- setNames(test,set_names)
  weekly<-data.frame(date=index(test), coredata(test))
  weekly$date<-as.Date(weekly$date,format = "%m/%d/%Y")
  name_list<- gsub("\\.", " ", names(weekly))
  name_list<- gsub("\\U S", "US", name_list)
  name_list<- gsub("  ", " ", name_list)
  name_list<- gsub(" Weekly", "", name_list)
  name_list<- gsub("US ", "", name_list)
  name_list<- gsub(region[series_idnum],region_short[series_idnum], name_list)
  weekly <- setNames(weekly,name_list)
  #weekly$max_imports<-as.data.frame(rollapply(weekly$`Imports of Crude Oil`,52*5,max,fill=NA,align = c("right")))[,1]
  #weekly$min_imports<-as.data.frame(rollapply(weekly$`Imports of Crude Oil`,52*5,min,fill=NA,align = c("right")))[,1]
  #weekly$avg_imports<-as.data.frame(rollapply(weekly$`Imports of Crude Oil`,52*5,mean,fill=NA,align = c("right")))[,1]
  weekly_set<-paste("weekly",region_short[series_idnum],sep="_")
  assign(weekly_set,weekly)
  #if(series_idnum>1)
  #  weekly_US<-merge(weekly_US,weekly_set)
}
weekly_test<-weekly_US
#weekly_US<-weekly_test
for(series_idnum in c(2:NROW(series_ids))){
  weekly_set<-paste("weekly",region_short[series_idnum],sep="_")
  test<-get(weekly_set)
  vname<-c(paste(region_short[series_idnum],"Imports of Crude Oil"),paste(region_short[series_idnum],"Imports of Crude Oil and Petroleum Products"),paste(region_short[series_idnum],"Imports of Total Petroleum Products"))
  test[,vname[1]]<- test[,vname[2]]-test[,vname[3]]
  weekly_US<-merge(weekly_US,test,by="date")
}

#sample code - use grep or other means to select the measure vars you want
#create df1 long form data frame
#graph

#here for imports by PADD

testing<-cbind(weekly_US$date,weekly_US[grep("Imports of Crude Oil$",names(weekly_US))])
names(testing)[1]<-c("date")
measures<-names(testing)[grep("date",names(testing),invert = TRUE)]
df1<-melt(testing,id=c("date"),measure.vars = measures)
df1<-na.omit(df1)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_padd_imports.png")
ggplot(subset(df1,date>"2007-01-01"),aes(date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  scale_color_viridis("",labels=measures,discrete=TRUE,option="C")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2007-07-01"),min(df1$date)),Sys.Date())) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
  )+
  labs(y="Crude Oil Imports (Thousands of Barrels Per Day)",x="Year",
       title=paste("US Crude Oil Imports By Region (Weekly)"),
       caption="Source: EIA API, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



#short term energy outlook prices
subs<-data_fetch(KEY,cat=1039852)
testing<-as.character(subs$Series_IDs$series_id) #take out the 4-week averages
names<-as.character(subs$Series_IDs$name)
subs<-data_fetch(KEY,cat=829727)
testing<-c(testing,as.character(subs$Series_IDs$series_id)) #take out the 4-week averages
names<-c(names,as.character(subs$Series_IDs$name)) #take out the 4-week averages
testing<- t(testing)
test<- pdfetch_EIA(testing,KEY)

names<- gsub("\\.", " ", names)
names<- gsub("\\crude oil", "", names)
names<- gsub("\\Crude Oil", "", names)
names<- gsub("^ *|(?<= ) | *$", "", names, perl = TRUE) #take out extra spaces
test <- setNames(test, t(names))

test2<-data.frame(date=index(test), coredata(test))
test2$date<-as.Date(test2$date,format = "%m/%d/%Y")

names(test2)[1]<-paste("Date")  
monthly_prices<-cbind(test2$Date,test2[,grep("Monthly", colnames(test2))])  #all monthly columns
names(monthly_prices)[1]<-paste("Date")  

names(monthly_prices)<- gsub("\\.", " ", names(monthly_prices))
names(monthly_prices)<- gsub("\\Crude Oil", "", names(monthly_prices))
names(monthly_prices)<- gsub("\\crude oil", "", names(monthly_prices))
names(monthly_prices)<- gsub("\\Monthly", "", names(monthly_prices))
names(monthly_prices)<- gsub("\\spot", "", names(monthly_prices))
names(monthly_prices)<- gsub("\\Spot", "", names(monthly_prices))
names(monthly_prices)<- gsub("\\price", "", names(monthly_prices))
names(monthly_prices)<- gsub("\\Price", "", names(monthly_prices))
names(monthly_prices)<- gsub("^ *|(?<= ) | *$", "", names(monthly_prices), perl = TRUE) #take out extra spaces


monthly_prices$WTI_Brent_diff<- monthly_prices$Brent - monthly_prices$`West Texas Intermediate`


measures<-grep(paste(c("Brent","West Texas","WTI_Brent_diff"),collapse="|"), 
               names(monthly_prices), value=TRUE)

df1<-melt(monthly_prices,id=c("Date"),measure.vars = measures)
df1<-na.omit(df1)


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("crude_outlook.png")
ggplot(subset(df1,Date>"2016-01-01"),aes(Date,value,group = variable,colour=variable,fill=variable)) +
  geom_line(data=subset(df1,Date>"2016-01-01" & variable %in% c("Brent","West Texas Intermediate")), size=1.7) +
  #geom_col(data=subset(df1,variable %in% c("WTI_Brent_diff")),size=2.3)+
  geom_area(data=subset(df1,Date>"2016-01-01" & variable %in% c("WTI_Brent_diff")))+
  #geom_point(size=1) +
  scale_fill_brewer(NULL,labels=c("Brent","WTI","Brent Premium or\nDiscount to WTI"),type = "seq", palette = "Paired", direction = -1)+
  scale_colour_brewer(NULL,labels=c("Brent","WTI","Brent Premium or\nDiscount to WTI"),type = "seq", palette = "Paired", direction = -1)+
  #scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y",limits=c(max(df1$Date)-years(10),max(df1$Date)),expand=c(0.04,0)) +
  #scale_colour_manual(labels=c("Brent","WTI"),values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  scale_y_continuous(limits=c(-5,70),expand=c(0,0))+
  #guides(colour=guide_legend(ncol =3,byrow=FALSE),fill=NULL)+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text.y =element_text(size = 16,face = "bold", colour="black"),
    axis.text.x=element_text(size = 16,face = "bold", colour="black",angle=60, hjust=1),
  )+    labs(y="Spot Price ($US/bbl)",x="Year",
             title="WTI and Brent Prices, EIA Short Term Energy Outlook",
             caption="Source: EIA Data\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

