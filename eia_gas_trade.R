library(zoo)
library(lubridate)
library(readxl)
library(scales)
library(grid)
library(gridExtra)
library(janitor)
library(ggpubr)
library(cowplot)
library(patchwork)
library(ggthemes)
library(directlabels)
library(pdfetch)
library(gghighlight)
library(viridis)
library(tidyverse)
library(ggrepel)
library(xml2)
library(rvest)

data_fetch<-function(key, cat){
  #key<-KEY
  #cat=476336
  ifelse(cat==999999999,
         url <- paste("https://api.eia.gov/category/?api_key=",
                      key, "&out=xml", sep="" ),
         url <- paste("https://api.eia.gov/category/?api_key=",
                      key, "&category_id=", cat, "&out=xml", sep="" )
  )
  
 
  x <- read_xml(url)
  doc <- XML::xmlParse(file=x)
  
  
  Parent_Category <- tryCatch(XML::xmlToDataFrame(,stringsAsFactors = F,nodes =
                                               XML::getNodeSet(doc, "//category/parent_category_id")),
                              warning=function(w) FALSE, error=function(w) FALSE)
  Sub_Categories <- XML::xmlToDataFrame(,stringsAsFactors = F,nodes =
                                     XML::getNodeSet(doc, "//childcategories/row"))
  Series_IDs <- XML::xmlToDataFrame(nodes =
                                 XML::getNodeSet(doc, "///childseries/row"),stringsAsFactors = F)
  Categories <- list(Parent_Category, Sub_Categories, Series_IDs)
  names(Categories) <- c("Parent_Category", "Sub_Categories", "Series_IDs")
  Categories
}

 get_children<-function(category_id=476336){
   subs<-data_fetch(KEY,cat=category_id)
   sub_cats<-subs$Sub_Categories
   #build list from sub_cats
   cat_store <- list()
   cat_count<-1
   for (cat in sub_cats$category_id) {
     #cat<-sub_cats$category_id[1]
     series<-data_fetch(KEY,cat=cat)
     cat_store[[cat_count]]<-series$Series_IDs
     cat_count<-cat_count+1
   }
   data.frame(do.call(rbind,cat_store))
 }
 #get_children()
 
 get_series<-function(category_id=476336){
   #series,name,f,units,updated
   subs<-data_fetch(KEY,cat=category_id)
   subs$Series_IDs
 }
 #get_series()
 
 pd_fix<-function(data,name){
   data<-data.frame(date=index(data), coredata(data))
   data$date<-ymd(data$date)
   data <- setNames(data, c("date",name)) 
 }
 
 EIA_to_DF<-function(series_info){
   data<- pdfetch_EIA(series_info$series_id,KEY)
   pd_fix(data,series_info$name)
   }
 
 
#AEO Gas trade

 export_set<-c("Exports : Pipeline Exports to Canada",
               "Exports : Pipeline Exports to Mexico",
               "Exports : Liquefied Natural Gas Exports")
 import_set<-c("Imports : Pipeline Imports from Canada",
               "Imports : Pipeline Imports from Mexico",
               "Imports : Liquefied Natural Gas Imports")
 
 
 
#imports by data_series
#http://api.eia.gov/category/?api_key=YOUR_API_KEY_HERE&category_id=476336

import_series<-get_children(476336)
import_series<-filter(import_series,grepl("U.S. Natural Gas Pipeline Imports From",name)|grepl("U.S. Liquefied Natural Gas Imports,",name),!grepl("Price",name),!grepl("Annual",name))
       
#exports by data series
#http://api.eia.gov/category/?api_key=YOUR_API_KEY_HERE&category_id=476803
export_series<-get_children(476802)
export_series<-filter(export_series,grepl("U.S. Natural Gas Pipeline Exports to",name)|grepl("Liquefied U.S. Natural Gas Exports,",name),!grepl("Price",name),!grepl("Annual",name))

gas_trade<-rbind(import_series,export_series)
gas_trade_data<-EIA_to_DF(gas_trade)
#reset to match AEO Names
names(gas_trade_data)<-c("date",                                                  
                         "Imports : Pipeline Imports from Canada",
                         "Imports : Pipeline Imports from Mexico",
                         "Imports : Liquefied Natural Gas Imports",           
                         "Exports : Pipeline Exports to Canada",  
                         "Exports : Pipeline Exports to Mexico",  
                         "Exports : Liquefied Natural Gas Exports")

#gas_trade_data$`Imports : Pipeline Imports`<-gas_trade_data$`Imports : Pipeline Imports from Canada`+gas_trade_data$`Imports : Pipeline Imports from Mexico`
#gas_trade_data$`Exports : Pipeline Exports`<-gas_trade_data$`Exports : Pipeline Exports to Canada`+gas_trade_data$`Exports : Pipeline Exports to Mexico`

#gas_trade_data$`Pipeline Net Imports`<-gas_trade_data$`Imports : Pipeline Imports`- gas_trade_data$`Exports : Pipeline Exports`
#gas_trade_data$`Liquefied Natural Gas Net Imports`<-gas_trade_data$`Imports : Liquefied Natural Gas Imports` - gas_trade_data$`Exports : Liquefied Natural Gas Exports`

  
gas_trade_data<-gas_trade_data %>% pivot_longer(-date,names_to ="series") 

#make it annual
gas_trade_data<-gas_trade_data %>% mutate(year=year(date)) %>% group_by(year,series) %>%
  summarise(value=12*mean(value,na.rm = T)) %>% ungroup %>% #now annual values based on mean non-na month
  mutate(date=ymd(paste(year,12,31,sep = "-")),year=NULL)
#adjust to TCF per year
gas_trade_data$value<-gas_trade_data$value/10^6

history_dates<-tibble(date=unique(gas_trade_data$date))

gas_trade_data<-gas_trade_data %>% filter(series %in% import_set | series %in% export_set) %>%
  mutate(value=ifelse(series %in% import_set,-1*value,value))



#build AEO list

subs<-data_fetch(KEY,cat=3162260)

#AEO_data$test<-"SUP_IMP_LIQ_NA_NG_NA_NA_TRLCF.A"

AEO_data<-subs$Series_IDs
AEO_data<-NULL
for(j in seq(2014,2019)){
  print(paste("working on AEO",j,sep=""))
  work_data<-subs$Series_IDs
  work_data$series_id<-gsub("2019",j,work_data$series_id)
  work_data$name<-gsub("2019",j,work_data$name)
  AEO_data<-rbind(AEO_data,work_data)
}

#get the 2022 defns
subs<-data_fetch(KEY,cat=4442481)
for(j in seq(2020,2022)){
  print(paste("working on AEO",j,sep=""))
  work_data<-subs$Series_IDs
  work_data$series_id<-gsub("2022",j,work_data$series_id)
  work_data$name<-gsub("2022",j,work_data$name)
  AEO_data<-rbind(AEO_data,work_data)
}


AEO_data$name<-gsub("Natural Gas : Volumes : ","",AEO_data$name)


test<- pdfetch_EIA(AEO_data$series_id,KEY)
series_data<-data.frame(date=index(test), coredata(test))
series_data$date<-ymd(series_data$date)
series_data <- setNames(series_data, c("date",AEO_data$name))


series_data <-series_data %>% full_join(history_dates)#include the dates for which we have history. They will be NA now, but we'll stack them in later


#melt it
series_data<-series_data %>% pivot_longer(-c(date),names_to="variable") #%>% na.omit()


#series_data$`Pipeline Net Imports`<-series_data$`Imports : Pipeline Imports`- gas_trade_data$`Exports : Pipeline Exports`
#series_data$`Liquefied Natural Gas Net Imports`<-gas_trade_data$`Imports : Liquefied Natural Gas Imports` - gas_trade_data$`Exports : Liquefied Natural Gas Exports`



#get the year for each
name_split<-do.call(rbind,strsplit(as.character(series_data$variable),", "))

series_data$series<-name_split[,1]
#series_data$case<-name_split[,2]
series_data$aeo_year<-name_split[,3]




joint_data<-series_data%>%filter(year(date)>=2000)%>%left_join(gas_trade_data%>%rename(history=value))

joint_data<-joint_data %>% filter(series %in% import_set | series %in% export_set) %>%
  mutate(value=ifelse(series %in% import_set,-1*value,value),
         #history=ifelse(series %in% import_set,-1*history,history)
         NULL)

#strip out history after forecast date

joint_data<-joint_data %>% 
  mutate(year=as.numeric(gsub("AEO","",aeo_year)),
    history=ifelse(year(date)<as.numeric(gsub("AEO","",aeo_year)),history,NA),
    series=gsub("Exports : ","",series),
    series=gsub("Imports : ","",series),
    series=factor(series,levels=c("Liquefied Natural Gas Exports", "Pipeline Exports to Canada",  "Pipeline Exports to Mexico",
                                  "Liquefied Natural Gas Imports", "Pipeline Imports from Canada" ,"Pipeline Imports from Mexico" ))
    
    )


gas_trade_plot<-ggplot(joint_data)+
  geom_area(aes(date,value,fill=series),position="stack",alpha=0.6,color="black",size=0.25)+
  geom_area(aes(date,history,fill=series),position="stack",color="black",size=0.25)+
  facet_wrap(~aeo_year,nrow = 1)+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  scale_x_date(breaks=pretty_breaks(n=5),expand=c(0,0))+
  #scale_color_viridis("",discrete = T,option="A",direction = -1,end = .9)+
  scale_fill_viridis("",discrete = T,option="A",direction = -1,end = .9)+
  scale_size_manual("",values=c(1,1.5))+
  scale_linetype_manual("",values=c("solid"))+
  expand_limits(x=ymd("1999-10-01"))+
  theme_minimal()+weekly_graphs()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(axis.text.x = element_text(angle=90,hjust = 0.5,vjust = 0.5))+
  guides(linetype=guide_legend(order = 1,keywidth = unit(1.6,"cm")),
         size="none",
         #shape = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         #linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         #colour = guide_legend(keywidth = unit(1.6,"cm"),override.aes = list(lty = "11")  ,nrow = 2),
         fill = guide_legend(keywidth = unit(1,"cm"),nrow = 2,byrow = TRUE),
         #fill = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2)
         NULL)+
  labs(y=paste("Annual Net Outflows (TCF)",sep=""),x="",
       title=paste("EIA Annual Energy Outlook Natural Gas Trade Projections"),
       subtitle=paste("Historical data up to forecast date in darker shade, forecasts shown with more transparency"),
       caption="Source: Data via EIA, graph by Andrew Leach.")

gas_trade_plot

ggsave("gas_trade.png",width = 16,height=8)

 