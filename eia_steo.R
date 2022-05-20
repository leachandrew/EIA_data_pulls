if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/EIA_data_pulls")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/EIA_data_pulls")
print(getwd())
source("../andrew_base.R")


 KEY <- "91b4dca0b858df64a2279d82f71af240"


eia_fix_dates<-function(data_sent)
{
  data_sent$date=ymd(rownames(data_sent))
  rownames(data_sent)<-NULL
  data_sent[]
}
 
 
#STEO Production Outlooks

price_series<-data_fetch(KEY,cat=829715)
petroleum_series<-data_fetch(KEY,cat=829726)
sub_cats<-petroleum_series$Sub_Categories$category_id

data_store <- list()
stack<-1
for (cat in sub_cats) {
  #for testing
  #cat<-sub_cats$category_id[2]
  subs<-data_fetch(KEY,cat=cat)
  series <-subs$Series_IDs %>% filter(grepl("Monthly",name))#,units=="Thousand Barrels per Day")
  #series$name<-as.character(series$name)
  #series$To<-"Export"
  #series$From<-do.call("rbind",strsplit(series$name, " Exports of "))[,1]
  #series$Mode<-"Export"
  #Rest<-do.call("rbind",strsplit(series$name, " Exports of "))[,2]
  #series<-series[!grepl("Exports to",Rest),]
  #Rest<-Rest[!grepl("Exports to",Rest)] #take out exports to the PADD itself - odd
  #series$Commodity<-do.call("rbind",strsplit(Rest, ", Monthly"))[,1]
  #series$freq<-"Monthly"
  data_store[[stack]]<-series
  stack<-stack+1
}
series_proc<-data.frame(do.call(rbind,data_store),stringsAsFactors = FALSE)
series_proc<-data.frame(lapply(series_proc,as.character),stringsAsFactors = FALSE)

price_data<-data.frame(pdfetch_EIA(series_proc$series_id,KEY),stringsAsFactors = F)

price_data$date=ymd(rownames(price_data))
rownames(price_data)<-NULL
price_data<-melt(price_data,id="date",variable.name = "series_id",value.name = "price") #convert data to long form with series ids
price_data$series_id<-as.character(price_data$series_id)


price_data<-price_data %>% left_join(series_proc,by=c("series_id"="series_id")) %>%
  na.omit()%>% mutate(price=ifelse(units=="cents per gallon",price/100*42,price),
                      units=ifelse(units=="cents per gallon","dollars per barrel",units)) %>%
                filter(date>=ymd("1990-01-01"))
#adjust everything to dollars per barrel

#figure out crack spread

#add the spot price for two barrels of Gulf Coast conventional gasoline to the spot price for one barrel of Gulf Coast ultra-low sulfur diesel. 

series_names<-c("Refiner Average Crude Oil Acquisition Cost, Monthly",
          "Refiner Wholesale Gasoline Price, Monthly",
          "Diesel Fuel Refiner Wholesale Price, Monthly"#,
          #"Heating Oil Refiner Wholesale Price, Monthly",
          #"No. 6 Residual Fuel Oil Price, Monthly")
)

crack_data<-dcast(price_data, date ~name,value.var="price",sum)%>%
  group_by(date) %>% summarize(price=`Refiner Wholesale Gasoline Price, Monthly`*2/3+
                                 `Diesel Fuel Refiner Wholesale Price, Monthly`*1/3-
                                 `Refiner Average Crude Oil Acquisition Cost, Monthly`)%>%
  mutate(name="crack_321",
         units="dollars per barrel",
         series_id="321_crack_calculated",
         f="M",
         updated=as.character(Sys.time()))

price_data<-rbind(price_data,crack_data) %>% mutate(fct_name=as_factor(name))


labels<-c("3:2:1 Crack Spread","Diesel Fuel Refiner Wholesale Price, Monthly",
          "Refiner Average Crude Oil Acquisition Cost, Monthly",
                                               "Refiner Wholesale Gasoline Price, Monthly"
                                               
)



ggplot(filter(price_data,date>=ymd("1998-01-01"),name %in%series_names),aes(group = fct_name,colour=fct_name,fill=name)) +
  geom_line(aes(date,price),size=1.7) +
  geom_area(data=filter(price_data,date>=ymd("1998-01-01"),fct_name =="crack_321"),aes(date,price)) +
  scale_color_viridis("",label=labels,discrete=TRUE,option="C")+
  scale_fill_viridis("",label=labels,discrete=TRUE,option="C")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y",expand=c(0,0)) +
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
  labs(y="Crude Oil and Refined Product Prices ($/bbl)",x="Year",
       title=paste("US Crude Oil Acquistion and Refined Product Wholesale Prices (Monthly)"),
       caption="Source: EIA STEO, graph by Andrew Leach.")



#STEO Production Outlooks

#Non-OPEC Supply
prod_series<-data_fetch(KEY,cat=829751)
#get regional categories
data_store <- list()
stack<-1
for (cat in prod_series$Sub_Categories$category_id) {
  #for testing
  #cat<-sub_cats$category_id[2]
  subs<-data_fetch(KEY,cat=cat)
  series <-subs$Series_IDs 
  series$name<-as.character(series$name)
  #series$To<-do.call("rbind",strsplit(series$name, " Imports of "))[,1]
  #Rest<-do.call("rbind",strsplit(series$name, " Imports of "))[,2]
  #series<-series[!grepl("Imports From ",Rest),]
  #Rest<-Rest[!grepl("Imports From",Rest)] #take out exports to the PADD itself - odd
  #series<-series[!grepl("Imports from ",Rest),]
  #Rest<-Rest[!grepl("Imports from",Rest)] #take out exports to the PADD itself - odd
  #series$From<-"Import"
  #series$Commodity<-do.call("rbind",strsplit(Rest, ", Monthly"))[,1]
  #series$freq<-"Monthly"
  data_store[[stack]]<-series
  stack<-stack+1
}
regional_data<-data.frame(do.call(rbind,data_store))
sub_cats<-rbind(regional_data)

supply_series<-t(as.character(sub_cats$series_id))
supply_data<- eia_fix_dates(data.frame(pdfetch_EIA(supply_series,KEY),stringsAsFactors = F))

supply_data<-melt(supply_data,id="date",variable.name = "series_id",value.name = "value") #convert data to long form with series ids
supply_data$series_id<-as.character(supply_data$series_id)

supply_data<-supply_data %>% left_join(sub_cats,by=c("series_id"="series_id")) %>%
  na.omit()

#Totals
total_names<-levels(supply_data$name)[grep("Total",levels(supply_data$name))]
total_data<-filter(supply_data,name %in% total_names)

#country_level
country_names<-levels(supply_data$name)[-grep("Total",levels(supply_data$name))]
country_data<-filter(supply_data,name %in% country_names)



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



#supply within OPEC
#OPEC Supply

prod_series<-data_fetch(KEY,cat=1039874)
#sub_cats<-data.frame(prod_series$Series_IDs,stringsAsFactors = F)
cat=as.character(prod_series$Series_IDs$series_id)
opec_supply_data<- eia_fix_dates(data.frame(pdfetch_EIA(cat,KEY),stringsAsFactors = F))
#opec_supply_data<- data.frame(pdfetch_EIA(cat,KEY),stringsAsFactors = F)
opec_supply_data<-melt(opec_supply_data,id="date",variable.name = "series_id",value.name = "value") #convert data to long form with series ids
opec_supply_data$series_id<-as.character(opec_supply_data$series_id)
opec_supply_data<-opec_supply_data %>% left_join(prod_series$Series_IDs,by=c("series_id"="series_id")) %>%
  na.omit() %>%
  filter(!(grepl("OPEC",name)|grepl("Total",name)|grepl("Unplanned",name))) %>%
  filter(f=="M")%>%
  mutate(name=as.character(name),
         name=gsub("Crude Oil Production, ","",name),
         name=gsub(", Monthly","",name),
         name=gsub("Crude Oil and Liquid Fuels Supply, ","",name)
         ) %>%
  group_by(name)%>% mutate(max_2018=max(value*(year(date)==2018))) %>% ungroup() %>%
  mutate(name=as.factor(name))%>%
  mutate(name=fct_reorder(name,-max_2018))
opec_supply_data$updated<-dmy_hms(as.character(opec_supply_data$updated))  
opec_supply_data$updated<-as.Date(opec_supply_data$updated,format = "%b")



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("opec_oil_prod_member.png")
ggplot(filter(opec_supply_data,date<Sys.Date()))+
  geom_area(aes(date,value,group=name,colour=name,fill=name),size=2,position = "stack")+
  #facet_grid( ~ Region)+
  #geom_col(aes(Prov,prov_ghgs/10^6,colour=Sector,fill=Sector,group=Ref_Year),size=1.5,position = "dodge")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  #scale_fill_viridis("",discrete=TRUE)+
  #scale_x_discrete(breaks = levels(test$Year)[c(T, rep(F, 9))])+
  scale_x_date(breaks = "36 months",date_labels = "%b\n%Y")+
  scale_fill_manual("",values = c(colors_tableau10(),colors_tableau10_light()),guide = "legend")+
  scale_colour_manual("",values=c(colors_tableau10(),colors_tableau10_light()),guide = "legend")+
  guides(fill=guide_legend(nrow =3,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold",colour="black"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    axis.title=element_text(size = 16,face = "bold", colour="black"),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90),
    #strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y="Oil Production (Million Barrels per Day)",
       title=paste0("Monthly OPEC Production by Member, ",year(min(opec_supply_data$date)),"-",year(max(opec_supply_data$date))),
       #subtitle="Excluding Electricity",
       caption=paste0("Source: EIA International Energy Outlook, updated ",
                      format(max(opec_supply_data$updated), "%b %d, %Y")
                      ,", graph by @andrew_leach"))
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


#STEO FIGURES



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
steo_data0<-filter(steo_data_fetch(steo_date),Date>=ymd("2019-1-01"),forecast==0)
get_history<-1
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

spare_capacity<-filter(steo_data,Header=="Surplus Crude Oil Production Capacity")
#find forecast dates
min_forecast<-min(spare_capacity$Date[spare_capacity$forecast==1])
max_forecast<-max(spare_capacity$Date[spare_capacity$forecast==1])
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("opec_spare_capacity.png")
ggplot(filter(spare_capacity,Region=="OPEC Total"))+
  geom_col(aes(Date,value,group=Region,fill="OPEC Total Spare Capacity"))+
  geom_line(data=filter(steo_data,Region=="West Texas Intermediate Spot Average"),aes(Date,value/40,colour="WTI Spot Price"),size=2)+
  annotate("rect", fill = "grey10", alpha = 0.25, 
           xmin = min_forecast, xmax =max_forecast+months(1), ymin = -Inf, ymax = Inf)+
  annotate("text", x = min_forecast+(max_forecast-min_forecast)/2+months(1), y = 4, label = "STEO\nForecast",size=3)+
  
    # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  scale_y_continuous(sec.axis = sec_axis(~.*40, name = "WTI Spot Price ($/bbl)"))+
  scale_color_manual("",values=colors_tableau10()[1])+
  scale_fill_manual("",values=colors_tableau10()[2])+
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y",expand=c(0,0))+
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
    axis.title.y.right = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.y = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.x = element_text(margin = margin(l = 10,r=10,t=10,b=10),size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(y="OPEC Total Spare Capacity (MMbbl/d)",x="Year",
       title=paste("OPEC Spare Capacity"),
       subtitle=paste("Historic Values and",format(max(spare_capacity$version), "%B %Y"), "STEO Forecast"),
       caption="Source: EIA STEO, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


#global supply and demand
#the brackets mess up filter, so this is a fix
supply_demand<-steo_data %>%filter(code %in% c("patc_world","papr_world"))%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")))

#find forecast dates
#min_forecast<-min(spare_capacity$Date[spare_capacity$forecast==1])
#max_forecast<-max(spare_capacity$Date[spare_capacity$forecast==1])
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("supply_demand.png")
ggplot(supply_demand)+
  geom_line(aes(Date,value,group=Region,colour=Region),size=1.5)+
  geom_line(data=filter(steo_data,Region=="West Texas Intermediate Spot Average"),aes(Date,value,colour="Crude Oil Price (WTI, Right Axis)"),size=1.5)+
  geom_line(data=filter(steo_data,Region=="Brent Spot Average"),aes(Date,value,colour="Crude Price (Brent, Right Axis)"),size=1.5)+
    annotate("rect", fill = "grey10", alpha = 0.25, 
           xmin = min_forecast, xmax =max_forecast+months(1), ymin = -Inf, ymax = Inf)+
  annotate("text", x = min_forecast+(max_forecast-min_forecast)/2+months(1), y = 120, label = "STEO\nForecast",size=3)+
  
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Crude Oil Spot Price ($/bbl)"))+
  scale_color_manual("",values=colors_tableau10())+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y",expand=c(0,0))+
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
    axis.title.y.right = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.y = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.x = element_text(margin = margin(l = 10,r=10,t=10,b=10),size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(y="Global Supply and Consumption (MMbbl/d)",x="Year",
       title=paste(" International Petroleum and Other Liquids Production and Consumption"),
       subtitle=paste("Historic Values and",format(max(supply_demand$version), "%B %Y"), "EIA Short Term Energy Outlook (STEO) Forecast"),
       caption="Source: EIA STEO, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


#historical demand forecasts
steo_old_sd_forecasts<-filter(steo_data_fetch(ymd("2020-1-1")),Date>=ymd("2015-01-01"),forecast==1) %>%
  rbind(filter(steo_data_fetch(ymd("2020-3-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-4-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  filter(code %in% c("patc_world","papr_world"))%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")),
  )

graph_df<-supply_demand%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")),
)%>%
  bind_rows(steo_old_sd_forecasts)%>%
    mutate(version=factor(paste(month.abb[month(version)],year(version),"forecast"),
                          levels=paste(month.abb[arrange(month(unique(version)))],year(unique(version)),"forecast")))

forecast_label<-paste(format(max(supply_demand$version), "%b %Y"), "forecast")
other_versions<-graph_df %>% filter(forecast==1,version!=forecast_label) %>% select(version) %>% unique()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("demand_only.png")
ggplot(filter(graph_df,Region=="Total World Consumption",forecast==0,Date>ymd("2018-01-01")))+
  #geom_line(data=filter(steo_old_sd_forecasts,Region=="Total World Supply"),aes(Date,value,colour="Oct 2019 Supply Forecast"),size=1.5)+
  #geom_line(aes(Date,value,colour="A"),size=1.5)+
  geom_line(aes(Date,value,group=version),size=1.5)+
  geom_line(data=filter(graph_df,Region=="Total World Consumption",forecast==1),
            aes(Date,value,group=version,colour=version),size=1.5)+
  
  #geom_line(data=filter(steo_data,Region=="West Texas Intermediate Spot Average"),aes(Date,value,colour="Crude Oil Price (WTI, Right Axis)"),size=1.5)+
  #geom_line(data=filter(steo_data,Region=="Brent Spot Average"),aes(Date,value,colour="Crude Price (Brent, Right Axis)"),size=1.5)+
  #annotate("rect", fill = "grey10", alpha = 0.25, 
  #         xmin = min_forecast, xmax =max_forecast+months(1), ymin = -Inf, ymax = Inf)+
  #annotate("text", x = min_forecast+(max_forecast-min_forecast)/2+months(1), y = 95, label = "Current STEO\nForecast\nRange",size=3)+
    # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  scale_y_continuous(breaks=pretty_breaks())+
  scale_color_manual("",values=colors_tableau10())+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y",expand=c(0,0),limits = c(ymd("2018-01-01"),ymd("2022-12-01")))+
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
    axis.title.y.right = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.y = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.x = element_text(margin = margin(l = 10,r=10,t=10,b=10),size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(y="Global Consumption (MMbbl/d)",x="Year",
       title=paste(" International Petroleum and Other Liquids Consumption"),
       subtitle=paste("Historic Values and",format(max(supply_demand$version), "%B %Y"), "EIA Short Term Energy Outlook (STEO) Forecast"),
       caption="Source: EIA STEO, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("supply_only.png")
ggplot(filter(supply_demand,Region=="Total World Supply"))+
  geom_line(aes(Date,value,group=Region,colour=Region),size=1.5)+
  #geom_line(data=filter(steo_old_sd_forecasts,Region=="Total World Supply"),aes(Date,value,colour="Oct 2019 Supply Forecast"),size=1.5)+
  geom_line(data=filter(steo_old_sd_forecasts,version==ymd("2019-10-01"),Region=="Total World Supply"),aes(Date,value,colour=factor(version)),size=1.5)+
  
  #geom_line(data=filter(steo_old_sd_forecasts,Region=="Total World Consumption"),aes(Date,value,colour="Oct 2019 Demand Forecast"),size=1.5)+
  #geom_line(data=filter(steo_data,Region=="West Texas Intermediate Spot Average"),aes(Date,value,colour="Crude Oil Price (WTI, Right Axis)"),size=1.5)+
  #geom_line(data=filter(steo_data,Region=="Brent Spot Average"),aes(Date,value,colour="Crude Price (Brent, Right Axis)"),size=1.5)+
  annotate("rect", fill = "grey10", alpha = 0.25, 
           xmin = min_forecast, xmax =max_forecast+months(1), ymin = -Inf, ymax = Inf)+
  annotate("text", x = min_forecast+(max_forecast-min_forecast)/2+months(1), y = 95, label = "STEO\nForecast",size=3)+
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  scale_y_continuous(breaks=pretty_breaks())+
  scale_color_manual("",values=colors_tableau10())+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y",expand=c(0,0),limits = c(ymd("2015-01-01"),ymd("2022-12-01")))+
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
    axis.title.y.right = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.y = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.x = element_text(margin = margin(l = 10,r=10,t=10,b=10),size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(y="Global Supply (MMbbl/d)",x="Year",
       title=paste(" International Petroleum and Other Liquids Production"),
       subtitle=paste("Historic Values and",format(max(spare_capacity$version), "%B %Y"), "EIA Short Term Energy Outlook (STEO) Forecast"),
       caption="Source: EIA STEO, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#WTI PRICE FORECASTS

#historical demand forecasts
steo_old_WTI_forecasts<-filter(steo_data_fetch(ymd("2020-1-1")),Date>=ymd("2020-01-01"),forecast==1) %>%
  rbind(filter(steo_data_fetch(ymd("2020-2-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-3-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  filter(code %in% c("WTIPUUS"))%>%
  mutate(Region=as_factor(Region),
         version=factor(paste(month.abb[month(version)],year(version),"STEO"),
                        levels=paste(month.abb[month(unique(version))],year(unique(version)),"STEO")))
wti_fc<-steo_data %>%filter(code %in% c("WTIPUUS"))%>%
  mutate(Region=as_factor(Region),
           version=factor(paste(format(max(supply_demand$version), "%b %Y"), "STEO"),
                          levels=paste(month.abb[month(unique(version))],year(unique(version)),"STEO")))%>%
  rbind(steo_old_WTI_forecasts)%>%
  mutate(version=factor(version,levels = unique(version)))


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("wti_fcast.png")
ggplot(filter(wti_fc,Date>ymd("2014-01-01")))+
  geom_line(aes(Date,value,colour=version,group=version,linetype=version),size=1.5)+
  geom_line(data=filter(wti_fc,Date>ymd("2014-01-01"),forecast==0),aes(Date,value),size=1.5,colour="black")+
  #geom_line(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020",linetype="AB_Budget_2020"),size=1.5)+
  #geom_point(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020"),shape=21,size=2,fill="white")+
  scale_y_continuous(breaks=pretty_breaks())+
  scale_color_manual("",values=c(head(colors_tableau10(),4),"grey40"))+
  scale_linetype_manual("",values=c(1,2,2,2,2))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  scale_x_date(breaks = "6 months",date_labels = "%b\n%Y",expand=c(0,0))+
  weekly_small()+
  guides(col = guide_legend(keywidth = unit(1.6,"cm")))+
  labs(y="WTI Spot Monthly Average ($/bbl)",x="",
       title=paste("WTI Monthly Average Spot Price Forecast"),
       subtitle=paste("Historic Values and",format(max(supply_demand$version), "%B %Y"), "EIA Short Term Energy Outlook (STEO) Forecast"),
       caption="Source: EIA STEO, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#END WTI PRICE

series<-paste("AEO.",seq(2020,2020),".REF",seq(2020,2020),".PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A",sep="")
series<-c(series,paste("AEO.",seq(2020,2020),".LOWPRICE.PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A",sep=""))
#series<-c(series,paste("AEO.",seq(2020,2020),".HIGHOGS.PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A",sep=""))

wti_data<-pdfetch_EIA(series,KEY) 
names<-c("AEO 2020 Reference Case","AEO 2020 Low Price Case")
wti_data <- setNames(wti_data, names)
wti_data<-data.frame(date=index(wti_data), coredata(wti_data))
wti_data$date<-ymd(wti_data$date)
wti_melt <- melt(wti_data,id="date",variable.name = "version")
wti_melt$version<-as.character(wti_melt$version)
wti_melt$version<-gsub("\\."," ",wti_melt$version)
wti_melt<-wti_melt %>% rename("Date"="date") %>% filter(year(Date)<2024)

#wti_AB<- bind_rows(wti_fc,wti_melt)%>%
#  mutate(version=factor(version,levels = c(levels(wti_fc$version),unique(wti_melt$version))))



budget_2020 <- data.frame("Date" = c("2017-10-1"	,"2018-10-01","2019-10-01","2020-10-01","	2021-10-01","	2022-10-01"), 
                          "WTI_CAD" = c(68.83,82.27,76.82,75.82,80.52,81.29),
                          "WTI" = c(53.69,62.77,58.00,58.00,62.00,63.00),stringsAsFactors = F)

budget_2020$Date<-ymd(budget_2020$Date)

budget_2020$version<-"Alberta Budget 2020"
budget_2020$version<-factor(budget_2020$version)


budget_2020 <- budget_2020 %>% rename("value"="WTI") %>% select(-WTI_CAD) %>%filter(Date>=ymd("2019-10-01"))
wti_AB<-bind_rows(wti_fc,budget_2020)%>%
  mutate(version=factor(version,levels = c(levels(wti_fc$version),"Alberta Budget 2020")))



nymex<-read_csv("ftp://ftp.cmegroup.com/pub/settle/nymex_future.csv")%>%clean_names()
nymex_test<-nymex %>%
  filter(product_description=="Crude Oil Last Day Financial Futures")%>%
  mutate(Date=ymd(paste(contract_year,contract_month,15)),
         version=paste("NYMEX WTI Futures",format(max(mdy(tradedate)),"%b %d, %Y")))%>%
  rename("value"="settle")%>%
  select(Date,value,version)%>%
  filter(year(Date)<2023)

wti_AB<- bind_rows(wti_AB,nymex_test)%>%
  mutate(version=factor(version,levels = c(levels(wti_AB$version),unique(nymex_test$version))))

#library(RSelenium)
#checkForServer()
#startServer()
##startServer(javaargs="\Users\aleach\Documents\R Files\chromedriver") #path to where chromedriver is located on local hard (downloaded from: https://sites.google.com/a/chromium.org/chromedriver/downloads)
#remDr <- remoteDriver(browserName = "chrome") 

#remDr <- remoteDriver(browserName="firefox")
#remDr$open()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("wti_fcast_AB.png")
ggplot(filter(wti_AB,Date>ymd("2014-01-01")))+
  geom_line(aes(Date,value,colour=version,group=version,linetype=version),size=1.5)+
  geom_line(data=filter(wti_AB,Date>ymd("2014-01-01"),forecast==0),aes(Date,value),size=1.5,colour="black")+
  #geom_line(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020",linetype="AB_Budget_2020"),size=1.5)+
  #geom_point(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020"),shape=21,size=2,fill="white")+
  scale_y_continuous(breaks=pretty_breaks())+
  scale_color_manual("",values=c(head(colors_tableau10(),4),"grey40","grey70"))+
  scale_linetype_manual("",values=c(1,2,2,2,1,1))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  scale_x_date(breaks = "6 months",date_labels = "%b\n%Y",expand=c(0,0))+
  weekly_small()+
  theme(plot.subtitle = element_text(size = 10))+
  guides(col = guide_legend(keywidth = unit(1.6,"cm")))+
  labs(y="WTI Price ($/bbl)",x="",
       title=paste("WTI outlook based on institutional forecasts and forward markets"),
       subtitle=paste("Historic values, EIA Short Term Energy Outlook (STEO) forecasts, NYMEX futures prices, and Alberta Budget 2020 assumptions"),
       caption="Sources: EIA STEO, Alberta Budget 2020, and CME. Graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#END WTI PRICE





















#historical demand forecasts
steo_old_supply_forecasts<-filter(steo_data_fetch(ymd("2019-10-1")),Date>=ymd("2015-01-01")) %>%
  filter(code %in% c("papr_nonopec"), Region=="Non-OPEC Supply")
  

#same chart but for change in non-opec production

non_opec_prod<-filter(steo_data,Region=="Non-OPEC Supply")


#find forecast dates
min_forecast<-min(non_opec_prod$Date[non_opec_prod$forecast==1])
max_forecast<-max(non_opec_prod$Date[non_opec_prod$forecast==1])
lims=c(min(non_opec_prod$Date),max(non_opec_prod$Date)+months(1))

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("non_opec_production.png")
ggplot(non_opec_prod)+
  geom_col(aes(Date,value-lag(value,n=12),fill="Yearly Additions to non-OPEC Supply"))+
  geom_line(data=filter(steo_data,Region=="West Texas Intermediate Spot Average"),aes(Date,value/40,colour="WTI Spot Price"),size=2)+
  geom_line(data=filter(steo_old_supply_forecasts),aes(Date,value-lag(value,n=12),colour="Oct 2019 Supply Forecast"),size=1.5)+
  annotate("rect", fill = "grey10", alpha = 0.25, 
           xmin = min_forecast, xmax =max_forecast+months(1), ymin = -Inf, ymax = Inf)+
  annotate("text", x = min_forecast+(max_forecast-min_forecast)/2+months(1), y = -0.5, label = "STEO\nForecast",size=3)+
  
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  scale_y_continuous(sec.axis = sec_axis(~.*40, name = "WTI Spot Price ($/bbl)"))+
  scale_color_manual("",values=colors_tableau10()[c(1,3)])+
  scale_fill_manual("",values=colors_tableau10()[2])+
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y",expand=c(0,0),limits = lims)+
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
    axis.title.y.right = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.y = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.x = element_text(margin = margin(l = 10,r=10,t=10,b=10),size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(margin = margin(t=10),size = 14,face = "bold", colour="black"),
  )+
  labs(y="Non-OPEC Oil Supply Additions (MMbbl/d)",x="Year",
       title=paste("Year-over-year Additions to non-OPEC Supply and WTI Prices"),
       subtitle=paste("Historic Values and",format(max(spare_capacity$version), "%B %Y"), "STEO Forecast"),
       caption="Source: EIA STEO, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("opec_production.png")
ggplot(data=filter(steo_data,Region=="OPEC"))+
  geom_col(aes(Date,value-lag(value,n=12),fill="Yearly Additions to OPEC Supply"))+
  geom_line(data=filter(steo_data,Region=="West Texas Intermediate Spot Average"),aes(Date,value/40,colour="WTI Spot Price"),size=2)+
  #geom_line(data=filter(steo_old_supply_forecasts),aes(Date,value-lag(value,n=12),colour="Oct 2019 Supply Forecast"),size=1.5)+
  annotate("rect", fill = "grey10", alpha = 0.25, 
           xmin = min_forecast, xmax =max_forecast+months(1), ymin = -Inf, ymax = Inf)+
  annotate("text", x = min_forecast+(max_forecast-min_forecast)/2+months(1), y = -0.5, label = "STEO\nForecast",size=3)+
  
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  scale_y_continuous(sec.axis = sec_axis(~.*40, name = "WTI Spot Price ($/bbl)"))+
  scale_color_manual("",values=colors_tableau10()[c(1,3)])+
  scale_fill_manual("",values=colors_tableau10()[2])+
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y",expand=c(0,0),limits = lims)+
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
    axis.title.y.right = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.y = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.x = element_text(margin = margin(l = 10,r=10,t=10,b=10),size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(margin = margin(t=10),size = 14,face = "bold", colour="black"),
  )+
  labs(y="OPEC Oil Supply Additions (MMbbl/d)",x="Year",
       title=paste("Year-over-year Additions to OPEC Supply and WTI Prices"),
       subtitle=paste("Historic Values and",format(max(spare_capacity$version), "%B %Y"), "STEO Forecast"),
       caption="Source: EIA STEO, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("non_opec_prod_raw.png")
ggplot(non_opec_prod)+
  geom_line(aes(Date,value,fill="Non-OPEC Supply"))+
  #geom_line(data=filter(steo_data,Region=="West Texas Intermediate Spot Average"),aes(Date,value/40,colour="WTI Spot Price"),size=2)+
  geom_line(data=filter(steo_old_supply_forecasts),aes(Date,value,colour="Oct 2019 Supply Forecast"),size=1.5)+
  annotate("rect", fill = "grey10", alpha = 0.25, 
           xmin = min_forecast, xmax =max_forecast+months(1), ymin = -Inf, ymax = Inf)+
  annotate("text", x = min_forecast+(max_forecast-min_forecast)/2+months(1), y = -0.5, label = "STEO\nForecast",size=3)+
  
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  scale_y_continuous(sec.axis = sec_axis(~.*40, name = "WTI Spot Price ($/bbl)"))+
  scale_color_manual("",values=colors_tableau10()[c(1,3)])+
  scale_fill_manual("",values=colors_tableau10()[2])+
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y",expand=c(0,0),limits = lims)+
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
    axis.title.y.right = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.y = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.x = element_text(margin = margin(l = 10,r=10,t=10,b=10),size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(margin = margin(t=10),size = 14,face = "bold", colour="black"),
  )+
  labs(y="Non-OPEC Oil Supply Additions (MMbbl/d)",x="Year",
       title=paste("Year-over-year Additions to non-OPEC Supply and WTI Prices"),
       subtitle=paste("Historic Values and",format(max(spare_capacity$version), "%B %Y"), "STEO Forecast"),
       caption="Source: EIA STEO, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



