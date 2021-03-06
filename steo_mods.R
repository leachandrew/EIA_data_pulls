if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/EIA_data_pulls")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/EIA_data_pulls")
print(getwd())
source("../andrew_base.R")

library(tidyverse)
library(lubridate)
library(readxl)
library(reshape2)
library(scales)
library(viridis)
KEY <- "91b4dca0b858df64a2279d82f71af240"


set_png<-function(file_sent,width=1400,height=750,res=130){
  #MAC
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file=file_sent, width = width, height = height,res=res)
  #PC
  if(R.version$platform ==  "x86_64-w64-mingw32")
    png(file=file_sent, width = width, height = height,res=res,type='cairo')
}

weekly_graphs<-function(caption_align=1){
  theme_minimal()+theme(
    plot.margin = margin(.25, .75, .25, .75, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 10, face = "italic",hjust=caption_align),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.title.x = element_text(size = 14,face = "bold", colour="black",margin = margin(t = 15, b = 0)),
    axis.text = element_text(size = 14,face = "bold", colour="black",margin = margin(t = 10, b = 10)),
  )
}

weekly_small<-function(caption_align=1){
  theme_minimal()+theme(
    plot.margin = margin(.25, .75, .25, .75, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 9),
    plot.caption = element_text(size = 11, face = "italic",hjust=caption_align),
    plot.title = element_text(size = 16,face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 11,face = "bold"),
    axis.title.x = element_text(size = 11,face = "bold", colour="black",margin = margin(t = 15, b = 0)),
    axis.text = element_text(size = 11,face = "bold", colour="black",margin = margin(t = 10, b = 10)),
  )
}


blakes_blue<-"#4477AA"

work_theme<-function(){
  theme_hc(20)+
    theme(plot.subtitle = element_text(color="grey10",size=rel(.5)),
          plot.title = element_text(face="bold"),
          plot.caption = element_text(color="grey50",size=rel(.5)),
          legend.title = element_text(color="grey10",size=rel(1)),
          legend.text = element_text(color="grey10",size=rel(1)),
          strip.text = element_text(size=rel(1)),
          axis.title = element_text(size=rel(1)),
          axis.text = element_text(size=rel(1)),
          axis.ticks = element_blank(),
          panel.spacing = unit(2,"lines"),
          legend.position = "bottom",
          plot.margin = margin(t = .5, r = 1, b = .25, l = 1,unit= "cm"),
          axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
          
    )
}


eia_fix_dates<-function(data_sent)
{
  data_sent$date=ymd(rownames(data_sent))
  rownames(data_sent)<-NULL
  data_sent[]
}
 
steo_data_fetch<-function(date_sent){
#for any month and year, get the STEO Price outlook
#testing
  #date_sent<-ymd("2019-9-01")
  #month_sent should be lower case month.abb

#convert date to month/year notation used by eia
month_sent<-tolower(month.abb[month((date_sent))])
year_sent<-sprintf('%02d', year(date_sent)%% 100)

#before jun2013 they are xls files
file_date<-ymd(paste(year_sent,month_sent,1,sep="-"))
excel_file<-ifelse(file_date>ymd("2013-06-01"),
                   paste0("https://www.eia.gov/outlooks/steo/archives/",month_sent,year_sent,"_base.xlsx"),
                   paste0("https://www.eia.gov/outlooks/steo/archives/",month_sent,year_sent,"_base.xls"))
temp_file<-ifelse(file_date>ymd("2013-06-01"),
                  paste0("steo_",month_sent,"_",year_sent,".xlsx"),
                  paste0("steo_",month_sent,"_",year_sent,".xls"))
download.file(excel_file,mode = "wb",destfile = temp_file)
  dates<-read_excel(path=temp_file,sheet = "2tab",range = "C3:C4",col_names = F)
  names(dates)[1]<-"X__1"
year_start<-dates$X__1[1]
month_start<-grep(dates$X__1[2],month.abb)

#process price outlook
price_outlook<-read_excel(path=temp_file,sheet = "2tab",range = "A5:BV40",na="n/a")
names(price_outlook)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
#drop electricity and refined product headers
price_outlook<-price_outlook[-c(4,26),]
#rows which could be headers
headers<-grep("TRUE",is.na(price_outlook[,1]))
#for each header, the next x rows get a concatenated header
price_outlook$Header<-NA
price_outlook$Header[1]<-"Crude Oil"
for(j in headers){
  #print(price_outlook$Region[j])
  price_outlook$Header[[j]]<-price_outlook$Region[[j]]
  }
price_outlook<-price_outlook %>% fill(Header)
price_outlook<-price_outlook[!is.na(price_outlook$code),]
price_outlook<-melt(price_outlook,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
price_outlook$table<-"2tab"
price_outlook<-price_outlook %>% mutate(
  Date=ymd(Date),
  forecast=ifelse(Date>=file_date,1,0),
  version=file_date)
#file ends up with columns code, Region, Header, Date, value, forecast, version

#process non_opec_supply
crude_supply_data<-read_excel(path=temp_file,sheet = "3atab",range = "A5:BV47",na="n/a")
names(crude_supply_data)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
crude_supply_data<-crude_supply_data[rowSums(is.na(crude_supply_data)) != ncol(crude_supply_data),]
headers<-grep("TRUE",is.na(crude_supply_data[,1]))
#for each header, the next x rows get a concatenated header
crude_supply_data$Header<-NA
crude_supply_data$Header[1]<-"Supply (million barrels per day) (a)"
for(j in headers){
  #print(price_outlook$Region[j])
  crude_supply_data$Header[[j]]<-crude_supply_data$Region[[j]]
}
crude_supply_data<-crude_supply_data %>% fill(Header)
crude_supply_data<-crude_supply_data[!is.na(crude_supply_data$code),]
crude_supply_data<-melt(crude_supply_data,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
crude_supply_data$table<-"3atab"
crude_supply_data<-crude_supply_data %>% mutate(
  Date=ymd(Date),
  forecast=ifelse(Date>=file_date,1,0),
  version=file_date)
#file ends up with columns code, Region, Header, Date, value, forecast, version

#process non_opec_supply
non_opec_supply_data<-read_excel(path=temp_file,sheet = "3btab",range = "A5:BV50",na="n/a")
names(non_opec_supply_data)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
non_opec_supply_data<-non_opec_supply_data[rowSums(is.na(non_opec_supply_data)) != ncol(non_opec_supply_data),]
non_opec_supply_data$Header<-"Petroleum Supply  (million barrels per day)"
non_opec_supply_data<-melt(non_opec_supply_data,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
non_opec_supply_data$table<-"3btab"
non_opec_supply_data<-non_opec_supply_data %>% mutate(
  Date=ymd(Date),
  forecast=ifelse(Date>=file_date,1,0),
  version=file_date)

#process opec_data
opec_supply_data<-read_excel(path=temp_file,sheet = "3ctab",range = "A4:BV55",na=c("n/a","-"))
names(opec_supply_data)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
opec_supply_data<-opec_supply_data[rowSums(is.na(opec_supply_data)) != ncol(opec_supply_data),]
opec_supply_data$Header<-NA
headers<-grep("TRUE",is.na(opec_supply_data[,1]))
#for each header, the next x rows get a concatenated header
for(j in headers){
  #print(price_outlook$Region[j])
  opec_supply_data$Header[[j]]<-opec_supply_data$Region[[j]]
}
opec_supply_data<-opec_supply_data %>% fill(Header)
opec_supply_data<-opec_supply_data[!is.na(opec_supply_data$code),]
opec_supply_data<-melt(opec_supply_data,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
opec_supply_data$table<-"3ctab"
opec_supply_data<-opec_supply_data %>% mutate(
  Date=ymd(Date),
  forecast=ifelse(Date>=file_date,1,0),
  version=file_date)


#stack everthing

steo_data<-rbind(price_outlook,crude_supply_data,non_opec_supply_data,opec_supply_data)

steo_data
}


#steo_data<-steo_data_fetch(ymd("2018-12-1"))

#get historic data
#steo_data<-steo_data_fetch(ymd("2018-12-1"))

#find current issue of STEO - latest it could be out is the 12th, but let's use the 15th
steo_date<-as.Date(ifelse(day(Sys.Date())>=11,Sys.Date(),Sys.Date()-months(1)))
#steo_date<-Sys.Date()
steo_data0<-filter(steo_data_fetch(steo_date),Date>=ymd("2019-1-01"),forecast==0)
get_history<-0
if(get_history==1)
  {
  steo_data1<-filter(steo_data_fetch(ymd("2019-1-1")),Date>=ymd("2015-01-01"),forecast==0)
  #2011-2014 histories
  steo_data2<-filter(steo_data_fetch(ymd("2015-1-1")),forecast==0)
  #2007-2010 histories
  steo_data3<-filter(steo_data_fetch(ymd("2011-1-1")),forecast==0)
  #2004-2007 histories
  steo_data4<-filter(steo_data_fetch(ymd("2008-1-1")),Date<ymd("2007-01-01"),forecast==0)
  steo_history<-rbind(steo_data4,steo_data3,steo_data2,steo_data1)
  save(steo_history,file="steo_history.RData")
  }

load("steo_history.RData")
steo_history<-rbind(steo_history,steo_data0)

#add forecasts

steo_forecast<-filter(steo_data_fetch(steo_date),forecast==1)
steo_data<-rbind(steo_history,steo_forecast)




#global supply and demand
#the brackets mess up filter, so this is a fix
supply_demand<-steo_data %>%filter(code %in% c("patc_world","papr_world"))%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")))
#find forecast dates
min_forecast<-min(supply_demand$Date[supply_demand$forecast==1])
max_forecast<-max(supply_demand$Date[supply_demand$forecast==1])

#historical demand forecasts
steo_old_sd_forecasts<-filter(steo_data_fetch(ymd("2020-1-1")),Date>=ymd("2015-01-01"),forecast==1) %>%
  #rbind(filter(steo_data_fetch(ymd("2020-2-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  #rbind(filter(steo_data_fetch(ymd("2020-3-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-4-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  #rbind(filter(steo_data_fetch(ymd("2020-5-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-6-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  #rbind(filter(steo_data_fetch(ymd("2020-7-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  #rbind(filter(steo_data_fetch(ymd("2020-8-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-9-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  #rbind(filter(steo_data_fetch(ymd("2020-10-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  #rbind(filter(steo_data_fetch(ymd("2020-11-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  #rbind(filter(steo_data_fetch(ymd("2020-12-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2021-1-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  #rbind(filter(steo_data_fetch(ymd("2021-2-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  #rbind(filter(steo_data_fetch(ymd("2021-3-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  filter(code %in% c("patc_world","papr_world"))%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")),
         version=factor(paste(month.abb[month(version)],year(version),"forecast"),
                        levels=paste(month.abb[month(unique(version))],year(unique(version)),"forecast")))

graph_df<-supply_demand%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")),
         version=factor(paste(format(max(supply_demand$version), "%b %Y"), "forecast"),
                        levels=paste(month.abb[month(unique(version))],year(unique(version)),"forecast")))%>%
  bind_rows(steo_old_sd_forecasts)%>%
  mutate(version=mdy(paste(substr(as.character(version),1,3),1,substr(as.character(version),4,8),sep=" ")),
         version=factor(format(version,"%b %Y forecast"),
        levels=format(sort(unique(version)),"%b %Y forecast")),
  NULL
  )
         
forecast_label<-paste(format(max(supply_demand$version), "%b %Y"), "forecast")
other_versions<-graph_df %>% filter(forecast==1,version!=forecast_label) %>% select(version) %>% unique()




png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("demand_new.png")
ggplot(filter(graph_df,Region=="Total World Consumption",forecast==0,Date>=ymd("2018-01-01")))+
  geom_line(aes(Date,value,group=version,linetype="Historic Data"),size=1.25)+
  geom_line(data=filter(graph_df,Region=="Total World Consumption",forecast==1),
            aes(Date,value,group=version,colour=version),lty="11",size=1.25)+
  geom_point(data=filter(graph_df,Region=="Total World Consumption",forecast==1),
             aes(Date,value,group=version,shape=version,colour=version,fill=version),size=2.5)+
  
  #geom_line(data=filter(wti_fc,Date>ymd("2013-01-01"),forecast==0),aes(Date,value,linetype="A"),size=1.5,colour="black")+
  #geom_line(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020",linetype="AB_Budget_2020"),size=1.5)+
  #geom_point(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020"),shape=21,size=2,fill="white")+
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y",expand = c(0,0))+
  expand_limits(x=max(graph_df$Date+months(3)))+
  scale_shape_manual("",values=c(15,16,17,18,0,1,2))+
  scale_size_manual("",values=c(0,rep(2.5,6)))+
  scale_y_continuous(breaks=pretty_breaks())+
  #scale_linetype_manual("",values=c(1,1))+
  scale_color_viridis("",discrete = T,option="A",direction = -1,end = .9)+
  scale_fill_viridis("",discrete = T,option="A",direction = -1,end=.9)+
  scale_linetype_manual("",values=c(1,2),labels=c("Historical Data","Forecast"))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  #ajl_line()+
  theme_minimal()+weekly_graphs()+
  guides(shape = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         colour = guide_legend(keywidth = unit(1.6,"cm"),override.aes = list(lty = "11")  ,nrow = 2),
         fill = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2))+
  labs(y="Global Liquids Demand (million barrels per day)",x="",
       title=paste("Global Liquids Demand and EIA Forecasts"),
       subtitle=paste("Historic Values and Short Term Energy Outlook Forecasts"),
       caption="Source: Data via EIA STEO, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("supply_new.png")
ggplot(filter(graph_df,Region=="Total World Supply",forecast==0,Date>=ymd("2018-01-01")))+
  geom_line(aes(Date,value,group=version,linetype="Historic Data"),size=1.5)+
  geom_line(data=filter(graph_df,Region=="Total World Supply",forecast==1),
            aes(Date,value,group=version,colour=version),lty="11",size=1.5)+
  #geom_point(data=filter(graph_df,Region=="Total World Supply",forecast==1),
  #           aes(Date,value,group=version,shape=version,colour=version,fill=version),size=2.5)+
  
  #geom_line(data=filter(wti_fc,Date>ymd("2013-01-01"),forecast==0),aes(Date,value,linetype="A"),size=1.5,colour="black")+
  #geom_line(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020",linetype="AB_Budget_2020"),size=1.5)+
  #geom_point(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020"),shape=21,size=2,fill="white")+
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y",expand = c(0,0))+
  expand_limits(x=max(graph_df$Date+months(3)))+
  scale_shape_manual("",values=c(15,16,17,18,0,1,2))+
  scale_size_manual("",values=c(0,rep(2.5,6)))+
  scale_y_continuous(breaks=pretty_breaks())+
  expand_limits(y=c(80,110))+
  #scale_linetype_manual("",values=c(1,1))+
  scale_color_viridis("",discrete = T,option="A",direction = -1,end = .9)+
  scale_fill_viridis("",discrete = T,option="A",direction = -1,end=.9)+
  scale_linetype_manual("",values=c("solid","11"),labels=c("Historical Data","Forecast"))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  #ajl_line()+
  theme_minimal()+weekly_graphs()+
  guides(shape = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         colour = guide_legend(keywidth = unit(1.6,"cm"),override.aes = list(lty = "11")  ,nrow = 2),
         fill = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2))+
  labs(y="Global Crude Oil Supply (million barrels per day)",x="",
       title=paste("Global Liquids Supply and EIA Forecasts"),
       subtitle=paste("Historic Values and Short Term Energy Outlook Forecasts"),
       caption="Source: Data via EIA STEO, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



#macleans 2021

steo_old_sd_forecasts<-filter(steo_data_fetch(ymd("2020-01-01")),Date>=ymd("2015-01-01"),forecast==1) %>%
  #rbind(filter(steo_data_fetch(ymd("2020-2-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  #rbind(filter(steo_data_fetch(ymd("2020-3-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-4-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  #rbind(filter(steo_data_fetch(ymd("2020-5-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  #rbind(filter(steo_data_fetch(ymd("2020-6-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  #rbind(filter(steo_data_fetch(ymd("2020-7-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  #rbind(filter(steo_data_fetch(ymd("2020-8-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-11-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  filter(code %in% c("patc_world","papr_world"))%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")),
         version=factor(paste(month.abb[month(version)],year(version),"forecast"),
                        levels=paste(month.abb[month(unique(version))],year(unique(version)),"forecast")))

graph_df<-supply_demand%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")),
         version=factor(paste(format(max(supply_demand$version), "%b %Y"), "forecast"),
                        levels=paste(month.abb[month(unique(version))],year(unique(version)),"forecast")))%>%
  bind_rows(steo_old_sd_forecasts)%>%
  mutate(version=mdy(paste(substr(as.character(version),1,3),1,substr(as.character(version),4,8),sep=" ")),
         levels=sort(ymd(version)),
         version=factor(paste(month.abb[month(version)],year(version),"EIA STEO forecast"),
                        levels=format(sort(unique(levels)),"%b %Y EIA STEO forecast")))


forecast_label<-paste(format(max(supply_demand$version), "%b %Y"), "EIA STEO forecast")
other_versions<-graph_df %>% filter(forecast==1,version!=forecast_label) %>% select(version) %>% unique()



kathy_theme<-function(){
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank(),
    text = element_text(size = rel(4),face = "bold"),
    )
  
}


ggplot(filter(graph_df,Region=="Total World Consumption",forecast==0,Date>=ymd("2004-01-01")))+
  
  geom_line(data=filter(graph_df,Region=="Total World Consumption"),
            aes(Date,value,group=version,colour=version),linetype="21",size=rel(1.25))+
  geom_point(data=filter(graph_df,Region=="Total World Consumption"),
             aes(Date,ifelse(month(Date)%in% c(4,8,12)&forecast==1,value,NA),
                 group=version,shape=version,colour=version,fill=version),size=rel(3.5))+
  geom_line(aes(Date,value,group=version,linetype="Historic Data"),size=rel(1.25))+
  scale_x_date(breaks = "12 months",date_labels = "%Y",expand=c(0,0))+
  expand_limits(x=Sys.Date()+years(2))+
  scale_shape_manual("",values=c(15,16,1,2,17,18,1))+
  scale_size_manual("",values=c(0,rep(2.5,6)))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(.20,.20))+
  scale_color_manual("",values = colors_ua10())+
  scale_fill_viridis("",discrete = T,option="C",direction = 1,end=.9)+
  scale_linetype_manual("",values=c(1,2),labels=c("EIA Historical Data","Forecast"))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  #ajl_line()+
  #blake_theme()+
  kathy_theme()+
  guides(shape = guide_legend(keywidth = unit(2.6,"cm"),nrow = 2),
         linetype = guide_legend(keywidth = unit(2.6,"cm"),nrow = 2),
         colour = guide_legend(keywidth = unit(2.6,"cm"),nrow = 2),
         fill = guide_legend(keywidth = unit(2.6,"cm"),nrow = 2))+
  labs(y="Global Liquids Demand (million barrels per day)",x="",
       #title=paste("Will the world oil market recover?"),
       #subtitle=paste("In early 2020, the COVID-19 pandemic dropped global oil demand to levels not seen since 2004. The initially-forecast rapid recovery has not materialized. What will 2021 bring?"),
       #caption="Source: Historical data and forecasts via Energy Information Administration (EIA) Short-term Energy Outlook (STEO), graph by Andrew Leach."
       NULL)
ggsave("macleans_2021.png",width=16,height = 10,dpi=300)

#OPEC/Non-OPEC Split

#OPEC Supply is coded papr_opec
#Non-OPEC Supply is papr_nonopec
#US is papr_US
#Canada is papr_US


#get OPEC+ codes

OPEC_p<-c("Kazakhstan","Azerbaijan","Bahrain","Brunei","Malaysia","Mexico","Oman","Russia","South Sudan","Sudan")
OPEC_p_codes<-c("papr_MX","papr_AJ","papr_KZ","papr_RS","papr_MU","papr_MY","papr_OD")

supply_demand<-steo_data %>%filter(code %in% c("papr_nonopec","papr_opec","papr_US","papr_CA","papr_RS",
                                               OPEC_p_codes))%>%
  filter(Header!="Total OPEC Supply")%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")),
         Region=fct_collapse(Region,`OPEC` = c("OPEC (d)", "OPEC")),
         Region_p=fct_collapse(Region,`OPEC` = c("OPEC (d)", "OPEC")))

old_forecasts<- steo_old_sd_forecasts%>% filter(code %in% c("papr_nonopec","papr_opec","papr_US","papr_CA"))%>%
  filter(Header!="Total OPEC Supply")%>%
mutate(Region=as_factor(Region),
       Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")),
       Region=fct_collapse(Region,`OPEC` = c("OPEC (d)", "OPEC")))




graph_df<-supply_demand%>% 
  mutate(version=factor(paste(format(max(supply_demand$version), "%b %Y"), "forecast"),
         levels=paste(month.abb[month(unique(version))],year(unique(version)),"forecast")))%>%
  bind_rows(old_forecasts)%>%
  mutate(version=mdy(paste(substr(as.character(version),1,3),1,substr(as.character(version),4,8),sep=" ")),
         version=factor(paste(month.abb[month(version)],year(version),"forecast"),
                        levels=paste(month.abb[month(sort(unique(version)))],(year(sort(unique(version)))),"forecast")))

top_panel<-ggplot(filter(graph_df,Region=="OPEC",forecast==0,Date>ymd("2015-01-01")))+
  geom_line(aes(Date,value,group=version,linetype="Historic Data"),size=1.25)+
  geom_line(data=filter(graph_df,Region=="OPEC",forecast==1),
            aes(Date,value,group=version,colour=version,linetype="STEO Forecast"),size=1.25)+
  geom_point(data=filter(graph_df,Region=="OPEC",forecast==1),
             aes(Date,ifelse(month(Date)%%3==0,value,NA),group=version,shape=version,colour=version,fill=version),size=2.5)+
  geom_point(data=filter(graph_df,Region=="OPEC",forecast==0,Date==ymd("2020-04-01")),
             aes(Date,value,group=version),shape=21,size=7.5)+
  annotate("text", x =ymd("2020-04-01"), y =37, label = "Price War!",size=3.25,hjust=0.5,vjust=0.5)+  
  #geom_line(data=filter(wti_fc,Date>ymd("2013-01-01"),forecast==0),aes(Date,value,linetype="A"),size=1.5,colour="black")+
  #geom_line(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020",linetype="AB_Budget_2020"),size=1.5)+
  #geom_point(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020"),shape=21,size=2,fill="white")+
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y")+
  scale_shape_manual("",values=c(15,16,17,18,0,1,2))+
  scale_size_manual("",values=c(0,rep(2.5,6)))+
  scale_y_continuous(breaks=pretty_breaks())+
  #scale_linetype_manual("",values=c(1,1))+
  scale_color_viridis("",discrete = T,option="A",direction = -1,end = .9)+
  scale_fill_viridis("",discrete = T,option="A",direction = -1,end=.9)+
  scale_linetype_manual("",values=c(1,2),labels=c("Historical Data","Forecast"))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  #ajl_line()+
  #theme_minimal()+weekly_graphs()+
  guides(shape = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         colour = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         fill = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2))+
  labs(y="Total Supply (mm bbl/d)",x="",
       title=paste("Estimated Total OPEC Liquids Supply and EIA Forecasts"),
       subtitle=paste("Historic Values and Forecasts from EIA Short Term Energy Outlook"),
       caption="Source: Data via EIA STEO, graph by Andrew Leach.")



bottom_panel<-ggplot(filter(graph_df,Region=="Total non-OPEC liquids",forecast==0,Date>ymd("2015-01-01")))+
  geom_line(aes(Date,value,group=version,linetype="Historic Data"),size=1.25)+
  geom_line(data=filter(graph_df,Region=="Total non-OPEC liquids",forecast==1),
            aes(Date,value,group=version,colour=version,linetype="STEO Forecast"),size=1.25)+
  geom_point(data=filter(graph_df,Region=="Total non-OPEC liquids",forecast==1),
             aes(Date,ifelse(month(Date)%%2==0,value,NA),group=version,shape=version,colour=version,fill=version),size=2.5)+
  geom_point(data=filter(graph_df,Region=="Total non-OPEC liquids",forecast==0,Date==ymd("2017-09-01")),
           aes(Date,value,group=version),shape=21,size=20.5)+
  annotate("text", x =ymd("2017-09-01"), y =65, label = str_wrap("I wonder what would happen if we radically increased production...",40),size=3.25,hjust=0.5,vjust=0.5)+  
  geom_point(data=filter(graph_df,Region=="Total non-OPEC liquids",forecast==0,Date==ymd("2019-12-01")),
             aes(Date,value,group=version),shape=21,size=20.5)+
  annotate("text", x =ymd("2019-12-01"), y =71, label = "Definitely Not A Price War!",size=3.25,hjust=0.5,vjust=0.5)+  
  geom_point(data=filter(graph_df,version=="Jan 2020 forecast",Region=="Total non-OPEC liquids",forecast==1,Date==ymd("2021-12-01")),
             aes(Date,value,group=version),shape=21,size=10.5)+
  annotate("text", x =ymd("2021-12-01"), y =71, label = "Okay, maybe a bit",size=3.25,hjust=1,vjust=0.5)+  
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y")+
  scale_shape_manual("",values=c(15,16,17,18,0,1,2))+
  scale_size_manual("",values=c(0,rep(2.5,6)))+
  scale_y_continuous(breaks=pretty_breaks())+
  #scale_linetype_manual("",values=c(1,1))+
  scale_color_viridis("",discrete = T,option="A",direction = -1,end = .9)+
  scale_fill_viridis("",discrete = T,option="A",direction = -1,end=.9)+
  scale_linetype_manual("",values=c(1,2),labels=c("Historical Data","Forecast"))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  #ajl_line()+
  #theme_minimal()+weekly_graphs()+
  blake_theme()+
  guides(shape = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         colour = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         fill = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2))+
  labs(y="Total Supply (mm bbl/d)",x="",
       title=paste("Estimated Total Non-OPEC Liquids Supply and EIA Forecasts"),
       subtitle=paste("Historic Values and Forecasts from EIA Short Term Energy Outlook"),
       caption="Source: Data via EIA STEO, graph by Andrew Leach.")

library(grid)
library(gridExtra)
library(lemon)


set_png("opec_non_opec_supply.png",width=1600,height = 900)

grid.arrange(arrangeGrob(bottom_panel + blake_theme()+
                           theme(   legend.position="none",
                                    plot.title = element_text(face="bold",size=rel(.7)),
                                    plot.subtitle = element_text(face="bold",size=rel(.6)),
                                    #       legend.margin=margin(c(0,0,0,0),unit="cm"),
                                    #       legend.text = element_text(colour="black", size = 14, face = "bold"),
                                            plot.caption = element_blank(),
                                    #       plot.title = element_blank(),
                                    #       plot.subtitle = element_text(size = 14, face = "italic"),
                                    #       panel.grid.minor = element_blank(),
                                    #       text = element_text(size = 12,face = "bold"),
                                    #       axis.text = element_text(size = 12,face = "bold", colour="black"),
                                           axis.text.x = element_blank(),
                                           NULL ),
                           
top_panel+ blake_theme()+
            theme(legend.position="bottom",
                  plot.title = element_text(face="bold",size=rel(.7)),
              plot.subtitle = element_blank(),
                  NULL)
,
ncol=1,heights=c(3.75,5))
)
dev.off()

#WTI PRICE FORECASTS

#historical demand forecasts
steo_old_WTI_forecasts<-filter(steo_data_fetch(ymd("2008-1-1")),forecast==1) %>%
  rbind(filter(steo_data_fetch(ymd("2009-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2010-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2011-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2012-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2013-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2014-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2015-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2016-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2017-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2018-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2019-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-1-1")),forecast==1))%>%
  filter(code %in% c("WTIPUUS"))%>%
  mutate(value=as.numeric(value),
         Region=as_factor(Region),
         version=factor(paste(month.abb[month(version)],year(version),"STEO"),
                        levels=paste(month.abb[month(unique(version))],year(unique(version)),"STEO")))
#Oct 2013 STEO Apr 2017 STEO Jan 2020 STEO

wti_fc<-steo_data %>%filter(code %in% c("WTIPUUS"))%>%
  mutate(Region=as_factor(Region),
         version=factor(paste(format(max(version), "%b %Y"), "STEO"),
                        levels=paste(month.abb[month(unique(version))],year(unique(version)),"STEO")))%>%
  as.data.frame()%>%
  bind_rows(steo_old_WTI_forecasts)%>%
  mutate(version=mdy(paste(substr(as.character(version),1,3),1,substr(as.character(version),4,8),sep=" ")))%>%
  mutate(version=factor(paste(month.abb[month(version)],year(version),"EIA STEO forecast",sep = " "),
                        levels=paste(month.abb[month(unique(sort(version)))],year(unique(sort(version))),"EIA STEO forecast"))
  )

#wti_futs<-read_csv("nymex_wti_rec.csv")
#nymex_test<-wti_futs %>% filter(biz_dt==ymd("2020-04-29")) %>%
#  mutate(Date=ymd(paste(substr(mmy,1,4),substr(mmy,5,6),1,sep = "-")),
#         version=paste("NYMEX WTI Futures",format(biz_dt,"%b %d, %Y")),
#         forecast=3)%>%
#  rename("value"="settle_price")%>%
#  select(Date,value,version,forecast)%>% as.data.frame()

#forecasts<-levels(wti_fc$version)
#forwards<-unique(nymex_test$version)

#code to run off current nyumex
nymex_wti<-read_csv("ftp://ftp.cmegroup.com/pub/settle/nymex_future.csv")%>%clean_names()%>%
#code to run off specific date
#nymex_wti<-read_csv("nymex_futures_dec28.csv")%>%clean_names()%>%
#nymex_wti<-read_csv("nymex_futures_dec28.csv")%>%clean_names()%>%
  filter(product_description=="Crude Oil Last Day Financial Futures")%>%
  mutate(Date=ymd(paste(contract_year,contract_month,15)),
         version=paste("NYMEX WTI Futures",format(max(mdy(tradedate)),"%b %d, %Y")),
         forecast=3)%>%
  rename("value"="settle")%>%
  select(Date,value,version,forecast)%>%
  mutate(version=factor(version))

nymex_version<-as.character(unique(nymex_wti$version))


#wti_fcg<-wti_fc %>% bind_rows(nymex_wti) 

#WTI Historic prices
#PET.RWTC.M
wti_hist<-pdfetch_EIA(c("PET.RWTC.D"),KEY)
wti_hist<- setNames(wti_hist, "value")
wti_hist<-data.frame(date=index(wti_hist), coredata(wti_hist))
wti_hist <- wti_hist %>% mutate(date=ymd(date),
                                case = "NYMEX Historic WTI Price Data",
                                year=year(date),month=month(date))%>%
  group_by(month,year)%>% summarize(value=mean(value,na.rm = T))%>%
  mutate(date=ymd(paste(year,month,1,sep="-")))%>%
  filter(date>ymd("2005-01-01"))




shape_set<-c(0,1,2,5,6,15,16,17,18,19)

linetypes = c(apply(expand.grid(c(2,4), c(1,2,4,8,"A")), 1, paste, collapse=""), 
              apply(expand.grid(c(2,4,8), c(2,4), c(5,"F"), 2), 1, paste, collapse=""),
              "4284B4F4", "228F61A4")

wti_graph<-ggplot(filter(wti_fc,forecast==1))+
  #geom_line(data=filter(wti_fcg,Date>ymd("2010-01-01"),Date<ymd("2030-1-1")),aes(Date,value,group=version,colour=version),linetype="solid",size=1.15)+
  geom_line(data=wti_hist,aes(date,value,linetype="Monthly Average WTI Prices"),size=1.15)+
  geom_line(data=nymex_wti,aes(Date,value,group=version,linetype=version),size=1.15)+
  geom_line(aes(Date,value,group=version,colour=version),linetype="solid",size=1.15)+
  geom_point(aes(Date,ifelse(month(Date) %in% c(6,12),value,NA),group=version,colour=version,shape=version),size=2.15)+
  scale_x_date(breaks = "24 months",date_labels = "%Y",expand=c(0,0))+
  scale_shape_manual("",values=c(shape_set,shape_set))+ #skipped 19
  scale_y_continuous(breaks=pretty_breaks(),expand=c(.20,.20))+
  # expand_limits(y=0)+
  expand_limits(x=ymd("2022-01-01"))+
  #scale_linetype_manual("",values=c(1,1))+
  #scale_color_viridis("",discrete = T,option="C",direction = 1,end = .9)+
  scale_color_manual("",values = c(colors_tableau10()[-8],colors_tableau10_light()[-8],"grey80"))+
  #scale_color_manual("",values = c("grey80"))+
  
  scale_linetype_manual("",values=c("solid",linetypes[1]))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  #ajl_line()+
  weekly_graphs()+
  work_theme()+
  theme(plot.caption = element_blank())+#no caption
  guides(shape = guide_legend(keywidth = unit(1.6,"cm"),ncol = 2),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 4),
         colour = guide_legend(keywidth = unit(1.6,"cm"),ncol = 2),
         NA
  )+
  labs(y="WTI Spot Monthly Average ($/bbl)",x="",
       #title=paste("WTI Monthly Average Spot Price History and Forecasts"),
       #subtitle=paste("Historic Values, EIA STEO Forecasts through ",format(max(supply_demand$version), "%B %Y"),", and ",nymex_version," settlements.",sep=""),
       caption="Source: Data via CME Group and EIA, graph by Andrew Leach.")
wti_graph
ggsave("wti_fcast_nymex.png",width=16,height = 9,dpi=600)





budget_2020 <- data.frame("Date" = c("2017-10-1"	,"2018-10-01","2019-10-01","2020-10-01","	2021-10-01","	2022-10-01"), 
                          "WTI_CAD" = c(68.83,82.27,76.82,75.82,80.52,81.29),
                          "WTI" = c(53.69,62.77,58.00,58.00,62.00,63.00),stringsAsFactors = F)

budget_2020$Date<-ymd(budget_2020$Date)

budget_2020$version<-"Alberta Budget 2020"
budget_2020$version<-factor(budget_2020$version)


budget_2020 <- budget_2020 %>% rename("value"="WTI") %>% select(-WTI_CAD) %>%filter(Date>=ymd("2019-10-01"))%>%
  mutate(forecast=3)
fc_date<-unique(as.character(nymex_wti$version))

wti_graph+
  geom_line(data=budget_2020,aes(Date,value,group=version,linetype="Z"),size=1.15)+
  scale_linetype_manual("",values=c("solid","11","31"),labels=c("Historic WTI Prices",fc_date,"Alberta Budget 2020 Forecast"))
ggsave("wti_fcast_nymex_AB.png",width=16,height = 9,dpi=300)
#END WTI PRICE
