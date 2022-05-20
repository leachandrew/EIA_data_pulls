library(plyr)
require(dplyr)
require(tidyr)
require(reshape2)
library(openxlsx)
library(ggplot2)
library(lubridate)
library(zoo)
library(ggmap)
library(ggjoy)
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

 KEY <- "91b4dca0b858df64a2279d82f71af240"



 ## The function is currently defined as
 test_func<-function(key, cat=2227122){
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
 
 
solar_data_17<-getEIA(ID = "AEO.2017.REF2017.GEN_NA_ELEP_NA_SLR_PHTVL_NA_BLNKWH.A", key = KEY) 
 #series_id=AEO.2017.REF2017.GEN_NA_ELEP_NA_SLR_PHTVL_NA_BLNKWH.A 
solar_data_16<-getEIA(ID = "AEO.2016.REF2016.GEN_NA_ELEP_NA_SLR_PHTVL_NA_BLNKWH.A", key = KEY)  
solar_data_15<-getEIA(ID = "AEO.2015.REF2015.GEN_NA_ELEP_NA_SLR_PHTVL_NA_BLNKWH.A", key = KEY)  
solar_data_14<-getEIA(ID = "AEO.2014.REF2014.GEN_NA_ELEP_NA_SOPH_NA_NA_BLNKWH.A", key = KEY)  

solar_data<-merge(solar_data_17,solar_data_16)
solar_data<-merge(solar_data,solar_data_15)
solar_data<-merge(solar_data,solar_data_14)
names(solar_data)<-c("solar_2017","solar_2016","solar_2015","solar_2014")
solar_data<-data.frame(date=index(solar_data), coredata(solar_data))
solar_melt <- melt(solar_data,id="date") 


png<-0
if(png==1)
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file="solar_eia.png", width = 1400, height = 750,res=130,type='cairo')
  else(R.version$platform ==  "x86_64-w64-mingw32")
    png(file="solar_eia.png", width = 1400, height = 750,res=130)

ggplot(solar_melt) +
  geom_line(aes(date,value,colour=variable),size=2) +
  scale_color_viridis("",labels = c("2017 Solar Generation Forecast","2016 Solar Generation Forecast","2015 Solar Generation Forecast","2014 Solar Generation Forecast"),discrete = TRUE)+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Generation (BkWh)",x="\nTime",
             title="EIA Annual Energy Outlook Solar Generation Forecasts",
             caption="Source: EIA API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

 

#AEO2017 category_id=2227112
 #AEO2016 category_id=2102233
 #AEO2015 category_id=1370522
 #AEO2014 category_id=964165
 
 
 

#NYMEX SPOTS 
subs<-test_func(KEY,cat=241335)
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
weekly_crude<-subset(weekly_crude, Date > as.Date("2007-07-18"))
weekly_crude<-na.omit(weekly_crude)
df1<-melt(weekly_crude,id=c("Date"),measure.vars = c("Europe.Brent.Spot.Price.FOB..Weekly","Cushing..OK.WTI.Spot.Price.FOB..Weekly"))
if(png==1)
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file="WTI_Brent.png", width = 1400, height = 750,res=130,type='cairo')
  else(R.version$platform ==  "x86_64-w64-mingw32")
    png(file="WTI_Brent.png", width = 1400, height = 750,res=130)


#jpeg(file="AESO_LTO.jpg", width = 1400, height = 750)
ggplot(df1,aes(Date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  #scale_color_viridis(labels=c("Brent","WTI"),discrete=TRUE)+   
  scale_colour_brewer(NULL,labels=c("Brent","WTI"),type = "seq", palette = "Paired", direction = -1)+
  scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y",limits=c(as.Date("2007-07-01"),as.Date("2017-07-18"))) +
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
  )+    labs(y="Weekly Closing Spot Price ($US/bbl)",x="Year",
             title="WTI and Brent Spot Prices",
             subtitle="Source: EIA Data, graph by Andrew Leach.")

dev.off()

weekly_crude$NYcrack<- 42*weekly_crude$New.York.Harbor.Ultra.Low.Sulfur.No.2.Diesel.Spot.Price..Weekly/3+weekly_crude$New.York.Harbor.Conventional.Gasoline.Regular.Spot.Price.FOB..Weekly*42*2/3-weekly_crude$Cushing..OK.WTI.Spot.Price.FOB..Weekly
weekly_crude$GCcrack<- 42*weekly_crude$U.S..Gulf.Coast.Ultra.Low.Sulfur.No.2.Diesel.Spot.Price..Weekly/3+weekly_crude$U.S..Gulf.Coast.Conventional.Gasoline.Regular.Spot.Price.FOB..Weekly*42*2/3-weekly_crude$Cushing..OK.WTI.Spot.Price.FOB..Weekly
weekly_crude$GCBcrack<- 42*weekly_crude$U.S..Gulf.Coast.Ultra.Low.Sulfur.No.2.Diesel.Spot.Price..Weekly/3+weekly_crude$U.S..Gulf.Coast.Conventional.Gasoline.Regular.Spot.Price.FOB..Weekly*42*2/3-weekly_crude$Europe.Brent.Spot.Price.FOB..Weekly

df1<-melt(weekly_crude,id=c("Date"),measure.vars = c("Cushing..OK.WTI.Spot.Price.FOB..Weekly","NYcrack","Europe.Brent.Spot.Price.FOB..Weekly","GCBcrack"))
df1$Date<-as.Date(df1$Date,format = "%m/%d/%Y")

test_date=as.numeric(as.POSIXlt("09/13/2016", format="%m/%d/%Y"))

png(file="WTI_cracks.png", width = 1400, height = 750,res=130,type='cairo')
#jpeg(file="AESO_LTO.jpg", width = 1400, height = 750)
ggplot(df1,aes(Date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  #scale_color_viridis(labels=c("Brent","WTI"),discrete=TRUE)+   
  scale_colour_brewer(NULL,labels=c("WTI Prices","WTI-Based NY Harbour Crack Spread","Brent Prices","Brent-Based Gulf Coast Crack Spread"),type = "seq", palette = "Paired", direction = -1)+
  scale_x_date(date_breaks = "6 months", date_labels =  "%m %Y",limits=c(as.Date("2007-07-01"),as.Date("2017-07-18"))) +
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


#nat gas 

#NYMEX NAT GAS SPOTS 
subs<-test_func(KEY,cat=462457)
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
png(file="boe_oil_gas_ngls.png", width = 1400, height = 750,res=130,type='cairo')
#jpeg(file="AESO_LTO.jpg", width = 1400, height = 750)
ggplot(df1,aes(Date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  #scale_color_viridis(labels=c("Brent","WTI"),discrete=TRUE)+   
  scale_colour_brewer(NULL,labels=c("Brent Crude","WTI Crude","Natural Gas","NGL Composite","Diesel","Gasoline"),type = "seq", palette = "Paired", direction = -1)+
  scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y",limits=c(as.Date("2009-01-01"),as.Date("2017-09-30"))) +
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




#Refinery acquisition costs


subs<-test_func(KEY,cat=293735)

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
subs<-test_func(KEY,cat=293676)

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


subs<-test_func(KEY,cat=293787)

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






#US Refinery yield


subs<-test_func(KEY,cat=304183)
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
  scale_x_date(date_breaks = "1 years", date_labels =  "%Y",limits=c(as.Date("1997-07-01"),as.Date("2017-06-30"))) +
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





#US Imports and Exports

#Imports
subs<-test_func(KEY,cat=314563)

testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
imports<-data.frame(date=index(test), coredata(test))
imports$date<-as.Date(imports$date,format = "%m/%d/%Y")

#Exports

subs<-test_func(KEY,cat=315919)

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
               "Crude.Oil..Monthly.1"
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
 
 
 subs<-test_func(KEY,cat=296686)
 
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
subs<-test_func(KEY,cat=323934)
testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
cda_imports<-data.frame(date=index(test), coredata(test))
cda_imports$date<-as.Date(cda_imports$date,format = "%m/%d/%Y")

#Exports to Canada
subs<-test_func(KEY,cat=316977)
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


subs<-test_func(KEY,cat=459199)

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



subs<-test_func(KEY,cat=475780)

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

df1<-df1[df1$Date>as.Date("2016-01-01"),]
df1<-df1[df1$value>=0,]
df1<-df1[df1$variable!="Liquefied.U.S..Natural.Gas.Exports.by.Vessel.and.Truck..Monthly",]
df1<-df1[df1$variable!="Liquefied.U.S..Natural.Gas.Exports.by.Vessel..Monthly",]

df1$variable<- gsub("Liquefied.U.S..Natural.Gas.Exports.by.Vessel.to.", "", df1$variable)
df1$variable<- gsub("Liquefied.U.S..Natural.Gas.Exports.by.Vessels.to.", "", df1$variable)
df1$variable<- gsub("..Monthly", "", df1$variable)
df1$variable<- gsub("\\.", " ",  df1$variable)
totals<-group_by(df1,Date)%>% summarise(total = sum(value))
df1<-merge(df1,totals,by="Date")
ggplot(subset(df1,Date>"2016-02-01")) +
  #geom_bar(aes(y = value, x = Date, fill = variable),
  #         stat="identity")+
  geom_line(data=subset(df1,Date>"2016-02-01" & variable=="Japan"),size=1,aes(Date,value,group = variable,colour=variable)) +
  geom_line(data=subset(df1,Date>"2016-02-01" & variable=="Mexico"),size=1,aes(Date,value,group = variable,colour=variable)) +
  geom_line(data=subset(df1,Date>"2016-02-01" & variable=="Chile"),size=1,aes(Date,value,group = variable,colour=variable)) +
  geom_line(data=subset(df1,Date>"2016-02-01" & variable=="China"),size=1,aes(Date,value,group = variable,colour=variable)) +
  geom_line(data=subset(df1,Date>"2016-02-01" & variable=="Argentina"),size=1,aes(Date,value,group = variable,colour=variable)) +
  geom_line(data=subset(df1,Date>"2016-02-01"),size=1,aes(Date,total)) +
  #  scale_color_viridis("",labels=c("Japan","Mexico","Chile","China","Argentina","Total"),discrete=TRUE,option="D")+   
  
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

