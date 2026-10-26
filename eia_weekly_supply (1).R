#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/EIA_data_pulls")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/EIA_data_pulls")
print(getwd())
#Set EIA API Key
#you can get your own or use this one
source("../andrew_base.R")



get_children<-function(category_id){
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


get_series<-function(category_id){
  #series,name,f,units,updated
  subs<-data_fetch(KEY,cat=category_id)
  subs$Series_IDs
}

EIA_to_DF<-function(series_info){
  data<- pdfetch_EIA(series_info$series_id,KEY)
  data<-data.frame(date=index(data), coredata(data))
  data$date<-ymd(data$date)
  data <- setNames(data, c("date",series_info$name))
}



#code to grab weekly supply data

subs<-data_fetch(KEY,cat=235081)
testing<-t(subs$Series_IDs$series_id[-grep("\\b.4\\b",subs$Series_IDs$series_id)]) #take out the 4-week averages
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)
set_names<-t(subs$Series_IDs$name[-grep("\\b.4\\b",subs$Series_IDs$series_id)])
test <- setNames(test,set_names)

subs<-data_fetch(KEY,cat=235418)
testing<-t(subs$Series_IDs$series_id[])
testing<- as.character(testing)
cushing<- pdfetch_EIA(testing,KEY)
set_names<-t(subs$Series_IDs$name[])
cushing <- setNames(cushing,set_names)

subs<-data_fetch(KEY,cat=1709237)
weekly_gas_series<-as.character(t(subs$Series_IDs$series_id[]))
weekly_gas_data<- pdfetch_EIA(weekly_gas_series,KEY)
weekly_gas_data<-data.frame(date=index(weekly_gas_data), coredata(weekly_gas_data))
weekly_gas_data <- setNames(weekly_gas_data,t(c("date",subs$Series_IDs$name[])))

weekly<-data.frame(date=index(test), coredata(test))
weekly$date<-as.Date(weekly$date,format = "%m/%d/%Y")
cushing<-data.frame(date=index(cushing), coredata(cushing))
cushing$date<-as.Date(cushing$date,format = "%m/%d/%Y")
weekly<-merge(weekly,cushing,by="date")

name_list<- gsub("\\.", " ", names(weekly))
name_list<- gsub("\\U S ", "US", name_list)
name_list<- gsub("  ", " ", name_list)
name_list<- gsub(" Weekly", "", name_list)
name_list<- gsub("US ", "", name_list)
name_list<- gsub("U S ", "", name_list)
name_list<- gsub("^\\s+|\\s+$", "",name_list)
weekly <- setNames(weekly,name_list)

weekly$week<-week(weekly$date)
weekly$year<-year(weekly$date)

series_id<-"Ending Stocks of Crude Oil"

#calculate 5 year range of imports
weekly<-arrange(weekly,date)

series_ids<-c(
  "Field Production of Crude Oil",
  "Imports of Crude Oil",                                                                         
  "Net Imports of Crude Oil",                                                                     
  "Refiner Net Input of Crude Oil",                                                               
  "Ending Stocks of Crude Oil",
  "Ending Stocks of Crude Oil in SPR",
  "Operable Crude Oil Distillation Capacity",
  "Exports of Total Petroleum Products",                                                          
  "Imports of Total Petroleum Products",                                                          
  "Net Imports of Total Petroleum Products",
  "Cushing OK Ending Stocks excluding SPR of Crude Oil"
  )


units<-c(
  "thousands of barrels per day",
  "thousands of barrels per day",
  "thousands of barrels per day",
  "thousands of barrels per day",
  "thousands of barrels",
  "thousands of barrels",
  "thousands of barrels per day",
  "thousands of barrels per day",
  "thousands of barrels per day",
  "thousands of barrels per day",
  "thousands of barrels"
)

series_id<-"Imports of Crude Oil"
counts<-0
for(series_id in series_ids){
print(series_id)
counts<-counts+1
unit<-units[counts]
df1<-melt(weekly,id=c("date","week"),measure.vars = series_id)
df1<-na.omit(df1)

#truncate to 52 weeks for the years where there is a 53rd week.
df1$week[df1$week==53]<-52



df.years <- df1 %>% 
  mutate(year = year(date)) %>% 
  filter(year >= year(Sys.Date()) - 1)

df.year.range <- df1 %>% 
  mutate(year = year(date)) %>% 
  filter(year >= year(Sys.Date()) - 6 & year <= year(Sys.Date()) - 1) %>% 
  group_by(week) %>% 
  summarize(mean = mean(value), min = min(value), max = max(value))

#We can then trick ggplot into printing a nice title for the fill on the legend, by setting fill inside aes to the intended string. Because fill is set in aes(), we control its color with scale_fill_manual.

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png(paste("weekly_",series_id,".png",sep=""))
graph<- ggplot() +
  geom_ribbon(data = df.year.range, aes(x = week, ymin = min, ymax = max, fill = 'Previous 5 Year Range\nof Weekly Exports')) +
  geom_line(data = df.year.range, aes(x = week, y = mean,colour="3"),size=1.7) +
  geom_line(data = subset(df.years,year==year(Sys.Date())-1), aes(x = week, y = value, color =as.factor(year)),size=1.7)+ 
  geom_line(data = subset(df.years,year==year(Sys.Date())), aes(x = week, y = value, color =as.factor(year)),size=1.7) +
  geom_point(data = filter(df.years, year == year(Sys.Date())), aes(x = week, y = value, color = as.factor(year)),size=0.95) +
  #scale_fill_manual(values = '#ffccff')
  scale_fill_manual("",values = 'grey70',labels=paste(year(Sys.Date())-6,"-",year(Sys.Date())-1,"\nrange",sep =""))+     
  scale_color_viridis("",discrete=TRUE,option="C",labels=c(year(Sys.Date())-1, year(Sys.Date()),paste(year(Sys.Date())-6,"-",year(Sys.Date())-1,"\naverage",sep ="")))+
  #scale_fill_continuous()+
  scale_x_continuous(limits=c(min(df1$week),max(df1$week)),expand=c(.001,0))+
  theme_minimal()+
  theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.line = element_line(color="black", size = 1),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
  )+
  labs(y=paste(series_id,"\n(",unit,")",sep=""),x="Week",
       title=paste(series_id,sep=""),
       caption="Source: EIA API, graph by Andrew Leach.")
print(graph)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png(paste("history_",series_id,".png",sep=""))
graph<-ggplot(df1,aes(date,value)) +
  geom_line(size=1.7) +
  scale_x_date(date_breaks = "12 months", date_labels =  "%b\n%Y",limits=c(min(df1$date),max(df1$date)),expand=c(0,10)) +
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    legend.text = element_text(colour="black", size = 10),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
  )+
  labs(y=paste(series_id,"\n(",unit,")",sep=""),x="Date",
       title=paste(series_id,sep=""),
       caption="Source: EIA API, graph by Andrew Leach.")
print(graph)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


}




series_id<-"Cushing OK Ending Stocks excluding SPR of Crude Oil"
df1<-melt(weekly,id=c("date","week"),measure.vars = series_id)
df1<-na.omit(df1)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("cushing_history.png")
ggplot(df1,aes(date,value/1000)) +
  geom_line(size=1.7) +
  scale_x_date(date_breaks = "12 months", date_labels =  "%b\n%Y",limits=c(min(df1$date),max(df1$date)),expand=c(0,10)) +
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    legend.text = element_text(colour="black", size = 10),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black",angle=90, hjust=1),
  )+
  labs(y="Cushing Crude Oil Storage (Monthly, Million Barrels)",x="Week",
       title=paste("Cushing Crude Oil Storage",sep=""),
       caption="Source: EIA data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()











png<-0
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_us_imports.png")
ggplot(df1,aes(week,value)) +
  geom_point(data=subset(df1,year(date)==year(Sys.Date()) ),size=1.7,aes(colour="1"))+ 
  geom_line(data=subset(df1,year(date)==year(Sys.Date()) ),size=1.7,aes(colour="1"))+ 
  geom_line(data=subset(df1,year(date)==year(Sys.Date())-1 ),size=1.7,aes(colour="2"))+
  geom_point(data=subset(df1,year(date)==year(Sys.Date())-1 ),size=1.7,aes(colour="2"))+ 
  #stat_summary(data=subset(df1,year(date)<year(Sys.Date()) &year(date)>year(Sys.Date())-6),geom = 'smooth', alpha = 0.2,size=1.7,
  #             fun.data = median_hilow,aes(colour=c("1","2","3"),fill="range"))+
  stat_summary(data=subset(df1,year(date)<year(Sys.Date()) &year(date)>year(Sys.Date())-6),geom="smooth",fun.y = mean, fun.ymin = min, fun.ymax = max,size=1.7,aes(colour="c",fill="b"))+
  #stat_summary(fun.data=mean_cl_normal, geom='smooth', color='black')+
  scale_color_viridis("",discrete=TRUE,option="C",labels=c(year(Sys.Date()), year(Sys.Date())-1,paste(year(Sys.Date())-6,"-",year(Sys.Date())-1,"\naverage",sep ="")))+
  scale_fill_viridis("",discrete=TRUE,option="C",labels=paste(year(Sys.Date())-6,"-",year(Sys.Date())-1,"\nrange",sep =""))+     
  #scale_fill_continuous()+
  scale_x_continuous(limits=c(min(df1$week),max(df1$week)),expand=c(0,0))+
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
  labs(y="Crude Oil Imports \n(Weekly, Thousands of Barrels per Day)",x="Week",
       title=paste("US Imports of Crude Oil",sep=""),
       caption="Source: EIA API, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


weekly$avg_exports<-as.data.frame(rollapply(weekly$`Exports of Crude Oil`,52,mean,fill=NA,align = c("right")))[,1]

#calculate 5 year range of imports
weekly<-arrange(weekly,date)
weekly$avg_imports<-as.data.frame(rollapply(weekly$`Exports of Crude Oil`,52,mean,fill=NA,align = c("right")))[,1]
weekly5<-subset(weekly,year>year(Sys.Date())-6 & year<year(Sys.Date())-1)
weekly_max<-ddply(weekly5, .(week), summarize, max_exp=max(`Exports of Crude Oil`),min_exp=min(`Exports of Crude Oil`))
weekly<-merge(weekly,weekly_max,by="week")
df1<-melt(weekly,id=c("date","week","min_exp","max_exp"),measure.vars = c("Exports of Crude Oil","avg_exports"))
df1<-na.omit(df1)




png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_us_exports.png")

ggplot(data=df1,aes(week,value,group = variable,colour=variable)) +
  geom_line(data=subset(df1,year(date)==year(Sys.Date())),size=1.7) +
  geom_ribbon(aes(x = week, ymin = min_exp, ymax = max_exp,fill="grey80"),alpha=.25, linetype = 0)+
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="C",labels=c("Current Year\nWeekly Exports", "1 Year Trailing\nAverage Exports"))+
  #scale_fill_viridis("",discrete=TRUE,option="C",labels="Previous 5 Year Range\nof Weekly Exports")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Exports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  #scale_x_date(date_breaks = "6 months", date_labels =  "%b\n%Y",limits=c(max(as.Date("2013-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
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
  labs(y="Crude Oil Exports (Weekly, Thousands of Barrels per Day)",x="Week",
       title=paste("US Exports of Crude Oil",sep=""),
       caption="Source: EIA API, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


#calculate 5 year range of imports
weekly<-arrange(weekly,date)
weekly$avg_netimports<-as.data.frame(rollapply(weekly$`Imports of Crude Oil`-weekly$`Exports of Crude Oil`,52,mean,fill=NA,align = c("right")))[,1]
weekly$netimports<-weekly$`Imports of Crude Oil`-weekly$`Exports of Crude Oil`
weekly5<-subset(weekly,year>year(Sys.Date())-6 & year<year(Sys.Date())-1)
weekly_max<-ddply(weekly5, .(week), summarize, max_netimp=max(`Imports of Crude Oil`-`Exports of Crude Oil`),min_netimp=min(`Imports of Crude Oil`-`Exports of Crude Oil`))
weekly<-merge(weekly,weekly_max,by="week")
df1<-melt(weekly,id=c("date","week","min_netimp","max_netimp"),measure.vars = c("netimports","avg_netimports"))
df1<-na.omit(df1)




png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_us_net_imports.png")

ggplot(data=df1,aes(week,value,group = variable,colour=variable)) +
  geom_line(data=subset(df1,year(date)==year(Sys.Date())),size=1.7) +
  geom_ribbon(aes(x = week, ymin = min_netimp, ymax = max_netimp,fill="grey80"),alpha=.25, linetype = 0)+
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="C",labels=c("Current Year\nWeekly Net Imports", "1 Year Trailing\nAverage Net Imports"))+
  scale_fill_viridis("",discrete=TRUE,option="C",labels="Previous 5 Year Range\nof Weekly Net Imports")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Exports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  #scale_x_date(date_breaks = "6 months", date_labels =  "%b\n%Y",limits=c(max(as.Date("2013-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Exports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  scale_x_continuous(limits=c(1,52),expand=c(0,0),breaks=seq(0,52,by=10))+
  #scale_y_continuous(limits=c(0,100),breaks = seq(0,100,by=25))+
  #scale_x_discrete(breaks = levels(BP_totals$Year)[c(T, rep(F, 9))])+
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
  labs(y="Crude Oil Net Imports (Weekly, Thousands of Barrels per Day)",x="Week",
       title=paste("US Net Imports of Crude Oil",sep=""),
       caption="Source: EIA API, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()





#Get Cushing Stocks

#cushing stocks 235418
subs<-data_fetch(KEY,cat=235418)
testing<-as.character(subs$Series_IDs$series_id)
test<- pdfetch_EIA(testing,KEY)
set_names<-t(subs$Series_IDs$name[])
test <- setNames(test,set_names)
cushing_weekly<-data.frame(date=index(test), coredata(test))
cushing_weekly$date<-as.Date(cushing_weekly$date,format = "%m/%d/%Y")

name_list<- gsub("\\.", " ", names(cushing_weekly))
name_list<- gsub("\\U S", "US", name_list)
name_list<- gsub("  ", " ", name_list)
name_list<- gsub(" Weekly", "", name_list)
name_list<- gsub(" excluding SPR of Crude Oil", "", name_list)
cushing_weekly <- setNames(weekly,name_list)

#calculate 5 year range of stocks

cushing_weekly$max_stocks<-as.data.frame(rollapply(cushing_weekly$`Cushing OK Ending Stocks`,52*5,max,fill=NA,align = c("right")))[,1]
cushing_weekly$min_stocks<-as.data.frame(rollapply(cushing_weekly$`Cushing OK Ending Stocks`,52*5,min,fill=NA,align = c("right")))[,1]
cushing_weekly$avg_stocks<-as.data.frame(rollapply(cushing_weekly$`Cushing OK Ending Stocks`,52*5,mean,fill=NA,align = c("right")))[,1]

#cushing_weekly$max_stocks<-as.data.frame(lag(rollapply(cushing_weekly$`Cushing OK Ending Stocks`,52*5,max,fill=NA,align = c("right")),52))[,1]
#cushing_weekly$min_stocks<-as.data.frame(rollapply(cushing_weekly$`Cushing OK Ending Stocks`,52*5,min,fill=NA,align = c("right")))[,1]
#cushing_weekly$avg_stocks<-as.data.frame(rollapply(cushing_weekly$`Cushing OK Ending Stocks`,52*5,mean,fill=NA,align = c("right")))[,1]



df1<-melt(cushing_weekly,id=c("date","min_stocks","max_stocks"),measure.vars = c("Cushing OK Ending Stocks","avg_stocks"))
df1<-na.omit(df1)


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_cushing_stocks.png")
ggplot(data=subset(df1,df1$date>"2007-01-01"),aes(date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  geom_ribbon(aes(x = date, ymin = min_stocks, ymax = max_stocks,fill="grey80"),alpha=.25, linetype = 0)+
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="D",labels=c("Weekly Ending Stocks", "5 Year Moving Average Stocks"))+
  scale_fill_viridis("",discrete=TRUE,option="C",labels="5 Year Range")+   
  scale_x_date(date_breaks = "6 months", date_labels =  "%b\n%Y",limits=c(max(as.Date("2007-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
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
  labs(y="Crude Oil Stocks (Weekly Ending, Thousands of Barrels)",x="Year",
       title=paste("Cushing OK Stocks of Crude Oil",sep=""),
       caption="Source: EIA API, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

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

#sample code - use grep or other means to select the measure vars you want
#create df1 long form data frame
#graph

#here for refiner net inputs by PADD

testing<-cbind(weekly_US$date,weekly_US[grep("Refiner Net Input",names(weekly_US))])
names(testing)[1]<-c("date")
measures<-names(testing)[grep("date",names(testing),invert = TRUE)]
df1<-melt(testing,id=c("date"),measure.vars = measures)
df1<-na.omit(df1)


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_us_crude_inputs.png")
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
  labs(y="Refiner Net Input of Crude Oil (Thousands of Barrels Per Day)",x="Year",
       title=paste("US Crude Oil Net Refinery Input By Region (Weekly)"),
       caption="Source: EIA API, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



#here for refining capacity by PADD

testing<-cbind(weekly_US$date,weekly_US[grep("Operable Crude Oil Distillation",names(weekly_US))])
names(testing)[1]<-c("date")
measures<-names(testing)[grep("date",names(testing),invert = TRUE)]
df1<-melt(testing,id=c("date"),measure.vars = measures)
df1<-na.omit(df1)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_us_capacity.png")

ggplot(subset(df1,date>"2011-01-01"),aes(date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  scale_color_viridis("",labels=measures,discrete=TRUE,option="C")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2011-07-01"),min(df1$date)),Sys.Date())) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  guides(colour=guide_legend(nrow=3,byrow=TRUE))+
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
  labs(y="Distallation Capacity (Thousands of Barrels Per Day)",x="Year",
       title=paste("Operable Crude Oil Distillation Capacity By Region (Weekly)"),
       caption="Source: EIA API, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


#here for crude storage by PADD

testing<-cbind(weekly_US$date,weekly_US[grep("Stock of Crude",names(weekly_US))])
names(testing)[1]<-c("date")
measures<-names(testing)[grep("date",names(testing),invert = TRUE)]
df1<-melt(testing,id=c("date"),measure.vars = measures)
df1<-na.omit(df1)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_us_stocks.png")

ggplot(subset(df1,date>"2001-01-01"),aes(date,value/1000,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  scale_color_viridis("",labels=measures,discrete=TRUE,option="C")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2001-07-01"),min(df1$date)),Sys.Date())) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  guides(colour=guide_legend(nrow=3,byrow=TRUE))+
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
  labs(y="Crude Storage (Millions of Barrels)",x="Year",
       title=paste("Crude Oil Storage By Region (Weekly)"),
       caption="Source: EIA API, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


#here for gasoline storage by PADD

testing<-cbind(weekly_US$date,weekly_US[grep("Stocks of Total Gasoline",names(weekly_US))])
names(testing)[1]<-c("date")
measures<-names(testing)[grep("date",names(testing),invert = TRUE)]
df1<-melt(testing,id=c("date"),measure.vars = measures)
df1<-na.omit(df1)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_us_gasoline_stocks.png")

ggplot(subset(df1,date>"2001-01-01"),aes(date,value/1000,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  scale_color_viridis("",labels=measures,discrete=TRUE,option="C")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2001-07-01"),min(df1$date)),Sys.Date())) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  guides(colour=guide_legend(nrow=3,byrow=TRUE))+
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
  labs(y="Gasoline Storage (Millions of Barrels)",x="Year",
       title=paste("Total Gasoline Storage By Region (Weekly)"),
       caption="Source: EIA API, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


#here for diesel storage by PADD

testing<-cbind(weekly_US$date,weekly_US[grep("Stocks of Distillate Fuel Oil$",names(weekly_US))])
names(testing)[1]<-c("date")
measures<-names(testing)[grep("date",names(testing),invert = TRUE)]
df1<-melt(testing,id=c("date"),measure.vars = measures)
df1<-na.omit(df1)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_us_diesel_stocks.png")

ggplot(subset(df1,date>"2001-01-01"),aes(date,value/1000,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  scale_color_viridis("",labels=measures,discrete=TRUE,option="C")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2001-07-01"),min(df1$date)),Sys.Date())) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  guides(colour=guide_legend(nrow=3,byrow=TRUE))+
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
  labs(y="Diesel Storage (Millions of Barrels)",x="Year",
       title=paste("Distillate Fuel Oil Storage By Region (Weekly)"),
       caption="Source: EIA API, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




#here for diesel storage by PADD

testing<-cbind(weekly_US$date,weekly_US[grep("Stocks of Kerosene Type Jet Fuel",names(weekly_US))])
names(testing)[1]<-c("date")
measures<-names(testing)[grep("date",names(testing),invert = TRUE)]
df1<-melt(testing,id=c("date"),measure.vars = measures)
df1<-na.omit(df1)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_us_jet_stocks.png")

ggplot(subset(df1,date>"2001-01-01"),aes(date,value/1000,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  scale_color_viridis("",labels=measures,discrete=TRUE,option="C")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2001-07-01"),min(df1$date)),Sys.Date())) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  guides(colour=guide_legend(nrow=3,byrow=TRUE))+
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
  labs(y="Jet Fuel Storage (Millions of Barrels)",x="Year",
       title=paste("Kerosene Type Jet Fuel Storage By Region (Weekly)"),
       caption="Source: EIA API, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


testing<-cbind(weekly_US$date,weekly_US[grep("Field Production of Crude Oil",names(weekly_US))])
names(testing)[1]<-c("date")

testing$avg_prod<-as.data.frame(rollapply(testing$`Field Production of Crude Oil`,52*5,mean,fill=NA,align = c("right")))[,1]
measures<-names(testing)[grep("date",names(testing),invert = TRUE)]

#calculate 5 year range of imports
testing$max_prod<-as.data.frame(rollapply(testing$`Field Production of Crude Oil`,52*5,max,fill=NA,align = c("right")))[,1]
testing$min_prod<-as.data.frame(rollapply(testing$`Field Production of Crude Oil`,52*5,min,fill=NA,align = c("right")))[,1]




df1<-melt(testing,id=c("date","min_prod","max_prod"),measure.vars = measures)
df1<-na.omit(df1)



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_us_prodn.png")
ggplot(df1,aes(date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  geom_ribbon(aes(x = date, ymin = min_prod, ymax = max_prod,fill="grey80"),alpha=.25, linetype = 0)+
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="C",labels=c("Weekly Production", "5 Year Moving Average Production"))+
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
  labs(y="Crude Oil Production\n(Weekly, Thousands of Barrels per Day)",x="Year",
       title=paste("US Field Production of Crude Oil",sep=""),
       caption="Source: EIA API, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


testing<-cbind(weekly_US$date,weekly_US[grep("Field Production of Crude Oil",names(weekly_US))])
names(testing)[1]<-c("date")

testing$avg_prod<-as.data.frame(rollapply(testing$`Field Production of Crude Oil`,26,mean,fill=NA,align = c("right")))[,1]
measures<-names(testing)[grep("date",names(testing),invert = TRUE)]

#calculate 6 month range of imports
testing$max_prod<-as.data.frame(rollapply(testing$`Field Production of Crude Oil`,26,max,fill=NA,align = c("right")))[,1]
testing$min_prod<-as.data.frame(rollapply(testing$`Field Production of Crude Oil`,26,min,fill=NA,align = c("right")))[,1]




df1<-melt(testing,id=c("date","min_prod","max_prod"),measure.vars = measures)
df1<-na.omit(df1)



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_us_prodn_short.png")
ggplot(subset(df1,date>="2015-01-01"),aes(date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  geom_ribbon(aes(x = date, ymin = min_prod, ymax = max_prod,fill="grey80"),alpha=.25, linetype = 0)+
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="C",labels=c("Weekly Production", "5 Year Moving Average Production"))+
  scale_fill_viridis("",discrete=TRUE,option="C",labels="5 Year Range")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "6 months", date_labels =  "%b\n%Y",limits=c(max(as.Date("2015-01-01"),min(df1$date)),Sys.Date()+months(6)),expand=c(.001,0)) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  scale_y_continuous(limits=c(8000,10000),expand=c(0,0))+
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
  labs(y="Crude Oil Production\n(Weekly, Thousands of Barrels per Day)",x="Year",
       title=paste("US Field Production of Crude Oil",sep=""),
       caption="Source: EIA API, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


testing<-cbind(weekly_US$date,weekly_US[grep("Crude Oil in SPR",names(weekly_US))])
names(testing)[1]<-c("date")

testing$avg_spr<-as.data.frame(rollapply(testing$`Ending Stocks of Crude Oil in SPR`,52*5,mean,fill=NA,align = c("right")))[,1]
measures<-names(testing)[grep("date",names(testing),invert = TRUE)]

#calculate 5 year range of imports
testing$max_spr<-as.data.frame(rollapply(testing$`Ending Stocks of Crude Oil in SPR`,52*5,max,fill=NA,align = c("right")))[,1]
testing$min_spr<-as.data.frame(rollapply(testing$`Ending Stocks of Crude Oil in SPR`,52*5,min,fill=NA,align = c("right")))[,1]




df1<-melt(testing,id=c("date","min_spr","max_spr"),measure.vars = measures)
df1<-na.omit(df1)



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("weekly_us_spr.png")
ggplot(df1,aes(date,value/1000,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  geom_ribbon(aes(x = date, ymin = min_spr/1000, ymax = max_spr/1000,fill="grey80"),alpha=.25, linetype = 0)+
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="C",labels=c("Weekly Production", "5 Year Moving Average Production"))+
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
  labs(y="Crude Oil in SPR (Weekly, Millions of Barrels)",x="Year",
       title=paste("US Ending Stocks of Crude Oil in SPR",sep=""),
       caption="Source: EIA API, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




#US ALL 235081
#PADD 1 235213
#PADD 2 235332
#PADD 3 235420
#PADD 4 235506
#PADD 5 235589



#EIA DPR
#require(XLConnect)


dpr_file<-"https://www.eia.gov/petroleum/drilling/xls/dpr-data.xlsx"

download.file(dpr_file,"dpr-data.xlsx",mode="wb")
plays<-getSheetNames("dpr-data.xlsx")
plays<-plays[plays!="RegionCounties"]
wb <- loadWorkbook("dpr-data.xlsx")
cnames<-c("Month","Rigcount",	"Oil_Production_per_rig",	"Oil_Legacy_prod_chg",	"Total_oil_prod","Gas_Production_per_rig",	"Gas_Legacy_prod_chg",	"Total_gas_prod")


dpr_data<-data.frame()
for(play in plays){
  play_data <- read.xlsx(xlsxFile = "dpr-data.xlsx", sheet = play, startRow = 2,skipEmptyRows = TRUE,detectDates = TRUE)
  names(play_data)<-cnames
  play_data$play<-play
  dpr_data<-rbind(dpr_data,play_data)
}


df1<-melt(dpr_data,id=c("Month","Total_oil_prod","Total_gas_prod"),measure.vars = c("play"),value.name = "Play")
df1$variable<-NULL
df1<-na.omit(df1)



df1<-df1 %>% mutate(Play=fct_other(Play,keep=c("Permian Region","Eagle Ford Region","Bakken Region"),other_level = "Other Regions")) %>%
  group_by(Month,Play) %>% summarize(Total_oil_prod=sum(Total_oil_prod),Total_gas_prod=sum(Total_gas_prod))



bw<-F
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("lto_dpr.png")
p<-ggplot(df1,aes(Month,Total_oil_prod/1000,group = Play,fill=Play)) +
  geom_area(position = "stack",color="black") +
  #geom_point(size=1) +
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
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
    axis.text.x=element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(y="Crude Oil Production by Play \n(Monthly, Thousands of Barrels per Day)",x="Week",
       title=paste("US Production of Tight Oil",sep=""),
       caption="Source: EIA DPR, graph by Andrew Leach.")
if(bw==T)
  p<-p+scale_fill_grey("", start = .8, end = 0, na.value = "red")
if(bw==F)
  p<-p+scale_fill_viridis("",discrete=TRUE,option="C")
print(p)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("lto_dpr_no_permian.png")
ggplot(subset(df1,Play != "Permian Region"),aes(Month,Total_oil_prod/1000,group = Play,colour=Play,fill=Play)) +
  geom_area(position = "stack") +
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="D")+
  scale_fill_viridis("",discrete=TRUE,option="D")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  #scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2000-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
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
  labs(y="Crude Oil Production by Play \n(Monthly, Thousands of Barrels per Day)",x="Week",
       title=paste("US Production of Tight Oil ex Permian Basin",sep=""),
       caption="Source: EIA DPR, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("lto_play.png")
ggplot(df1,aes(Month,Total_oil_prod/1000)) +
  geom_area(position = "stack",fill=colors_ua10()[1]) +
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="D")+
  scale_fill_viridis("",discrete=TRUE,option="D")+ 
  facet_wrap(~Play,ncol = 2, scales = "fixed")+

  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  #scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2000-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
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
  labs(y="(Monthly Production, 1000 bbl/d)",x="Week",
       title=paste("US Production of Tight Oil by Play",sep=""),
       caption="Source: EIA DPR, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


df2<-melt(dpr_data,id=c("Month","Total_oil_prod","Total_gas_prod"),measure.vars = c("play"),value.name = "Play")
df2$variable<-NULL
df2<-na.omit(df2)

df2<-df2 %>% mutate(Play=fct_other(Play,keep=c("Permian Region","Appalachia Region","Haynesville Region"),other_level = "Other Regions")) %>%
  group_by(Month,Play) %>% summarize(Total_oil_prod=sum(Total_oil_prod),Total_gas_prod=sum(Total_gas_prod))




png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("gas_dpr.png")
ggplot(df2,aes(Month,Total_gas_prod/10^6,group = Play,colour=Play,fill=Play)) +
  geom_area(position = "stack") +
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="D")+
  scale_fill_viridis("",discrete=TRUE,option="D")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  #scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2000-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
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
  labs(y="Shale and Tight Gas Production by Play \n(Monthly, Billion Cubic Feet per Day)",x="Week",
       title=paste("US Production of Tight and Shale Gas",sep=""),
       caption="Source: EIA DPR, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()





#EIA Production Data by Play
#require(XLConnect)


#oil_prod_file<-"https://www.eia.gov/energyexplained/data/U.S.%20tight%20oil%20production.xlsx"
oil_prod_file<-"https://www.eia.gov/energyexplained/oil-and-petroleum-products/data/US-tight-oil-production.xlsx"


download.file(oil_prod_file,"play-data.xlsx",mode="wb")
#plays<-getSheetNames("play-data.xlsx")
#plays<-plays[plays!="RegionCounties"]
play_data <- read.xlsx(xlsxFile = "play-data.xlsx", sheet = "data", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
play_data$X17<-NULL
play_data$X12<-NULL
play_data$Date<-as.Date(play_data$Date,origin = "1899-12-30")
df1<-melt(play_data,id=c("Date"),variable.name = "play",value.name = "volume" )
df1$play<-gsub("\\.", " ", df1$play)
df1$play<-gsub("&N", " & N", df1$play)
df1$play<-gsub("  ", " ", df1$play)
df1$play<-as.factor(df1$play) %>% fct_relevel("Rest of US 'tight oil'",after = Inf)

df1 <- df1 %>% group_by(play) %>% mutate(max_vol=max(volume)) %>% ungroup() %>%
  mutate(play=fct_reorder(play,volume,min),
         play= fct_relevel(play,"Rest of US 'tight oil'",after = Inf))%>%
  group_by(play)%>%
  mutate(region=paste(strsplit(as.character(play), "\\(|\\)")[[1]][2]))%>% ungroup()



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("oil_play.png")
ggplot(df1,aes(Date,volume*1000,group = play,fill=play)) +
  geom_area(position = "stack",colour="black") +
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="C")+
  scale_fill_viridis("",discrete=TRUE,option="C")+
  #scale_fill_discrete("")+
  #scale_colour_discrete(guide = "none")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "12 months", date_labels =  "%b\n%Y",expand=c(0,0)) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    legend.text = element_text(colour="black", size = 8),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(y="Tight Oil Production by Play \n(Monthly, Thousands of Barrels per Day)",x="Date",
       title=paste("US tight oil production estimates by play",sep=""),
       caption="Source: EIA data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



gas_prod_file<-"https://www.eia.gov/energyexplained/data/U.S.%20dry%20shale%20gas%20production.xlsx"

gas_prod_file<-"https://www.eia.gov/naturalgas/weekly/img/shale_gas_201907.xlsx"

download.file(gas_prod_file,"play-data.xlsx",mode="wb")
#plays<-getSheetNames("play-data.xlsx")
#plays<-plays[plays!="RegionCounties"]
play_data <- read.xlsx(xlsxFile = "play-data.xlsx", sheet = "data", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
play_data$X13<-NULL
play_data$X14<-NULL
play_data$Date<-as.Date(play_data$Date,origin = "1899-12-30")
df1<-melt(play_data,id=c("Date"),variable.name = "play",value.name = "volume" )
df1$play<-gsub("\\.", " ", df1$play)
df1$play<-gsub("&N", " & N", df1$play)
df1$play<-gsub("  ", " ", df1$play)


df1 <- df1 %>%
  mutate(play=as_factor(play),play=fct_relevel(play,"Rest of US 'shale'",after = Inf))%>%
  group_by(play) %>% mutate(max_prod=max(volume)) %>% ungroup() %>%
  mutate(play=fct_reorder(play,-max_prod),play=fct_other(play,keep=levels(play)[1:5]))%>%
  group_by(Date,play) %>% summarise(volume=sum(volume))
  


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("gas_play.png")
ggplot(df1,aes(Date,volume,group = play,colour=play,fill=play)) +
  geom_area(position = "stack") +
  #geom_point(size=1) +
  #scale_color_viridis("",discrete=TRUE,option="D")+
  #scale_fill_viridis("",discrete=TRUE,option="D")+
  scale_fill_discrete("")+
  scale_colour_discrete(guide = "none")+   
  scale_x_date(date_breaks = "12 months", date_labels =  "%b\n%Y",limits=c(min(df1$Date),max(df1$Date))) +
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    legend.text = element_text(colour="black", size = 10),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(y="Shale Gas Production (Dry)\n(Monthly, Billion Cubic Feet per Day)",x="Week",
       title=paste("US shale gas production estimates by play",sep=""),
       caption="Source: EIA data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#Total US production http://api.eia.gov/category/?api_key=YOUR_API_KEY_HERE&category_id=229976

oil_prod_series<-get_series(232183) %>% filter(name=="U.S. Field Production of Crude Oil, Monthly",units=="Thousand Barrels per Day")
oil_prod_data<-EIA_to_DF(oil_prod_series)%>% mutate(Month=ymd(paste(year(date),month(date),1,sep ="-")))

df1<-melt(dpr_data,id=c("Month","Total_oil_prod","Total_gas_prod"),measure.vars = c("play"),value.name = "Play")
df1$variable<-NULL
df1<-na.omit(df1)

oil_prod_data<-df1%>%left_join(oil_prod_data,by=c("Month")) %>% 
  mutate(Play=fct_other(Play,keep=c("Permian Region","Eagle Ford Region","Bakken Region"),other_level = "Other Shale Plays")) %>%
  group_by(Month,Play) %>% summarize(Total_oil_prod=sum(Total_oil_prod),Total_gas_prod=sum(Total_gas_prod),US_prod=mean(`U.S. Field Production of Crude Oil, Monthly`))


  
bw<-F
p<-ggplot(oil_prod_data) +
  geom_line(data=filter(oil_prod_data,Play=="Bakken Region"),aes(Month,US_prod,color="US Total Oil Production"),size=1.5)+
  geom_area(aes(Month,Total_oil_prod/1000,group = Play,fill=Play),position = "stack") +
  #geom_point(size=1) +
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0.1))+
  #expand_limits(x=Sys.Date()+months(24))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    plot.margin=margin(c(0.5,0.5,0.5,0.5),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black",margin = margin(t = 5)),
  )+
  labs(y="Crude Oil Production by Play \n(Monthly, Thousands of Barrels per Day)",x="",
       title=paste("US Production of Tight Oil and Total Crude Oil",sep=""),
       caption="Source: EIA DPR, graph by Andrew Leach.")
if(bw==T)
  p<-p+scale_fill_grey("", start = .7, end = .2, na.value = "red")+
  scale_colour_grey("", start =.1, end = .15, na.value = "red")
if(bw==F)
  p<-p+scale_fill_viridis("",discrete=TRUE,option="D")+
  scale_colour_grey("", start =.1, end = .15, na.value = "black")
p
ggsave("lto_dpr_total.png",width = 16,height = 9,dpi=400)

