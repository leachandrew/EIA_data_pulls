if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/EIA_data_pulls")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/EIA_data_pulls")
print(getwd())
source("../andrew_base.R")





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

price_data<-rbind(price_data,crack_data)


ggplot(filter(price_data,date>=ymd("1998-01-01"),name %in%series_names),aes(group = name,colour=name,fill=name)) +
  geom_line(aes(date,price),size=1.7) +
  geom_area(data=filter(price_data,date>=ymd("1998-01-01"),name =="crack_321"),aes(date,price)) +
  scale_color_viridis("",labels=labels ,discrete=TRUE,option="C")+
  scale_fill_viridis("",labels=labels ,discrete=TRUE,option="C")+   
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
sub_cats<-rbind(sub_cats,regional_data)

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
total_data<-filter(supply_data,name %in% total_names)



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
opec_supply_data<-melt(opec_supply_data,id="date",variable.name = "series_id",value.name = "value") #convert data to long form with series ids
opec_supply_data$series_id<-as.character(opec_supply_data$series_id)
opec_supply_data<-opec_supply_data %>% left_join(prod_series$Series_IDs,by=c("series_id"="series_id")) %>%
  na.omit() %>%
  filter(!(grepl("OPEC",name)|grepl("Total",name)|grepl("Unplanned",name))) %>%
  filter(f=="M")%>%
  mutate(name=as.character(name),
         name=gsub("Crude Oil Production, ","",name),
         name=gsub("Crude Oil and Liquid Fuels Supply, ","",name),
         name=gsub(", Monthly","",name)) %>%
  group_by(name)%>% mutate(max_2018=max(value*(year(date)==2018))) %>% ungroup() %>%
  mutate(name=as.factor(name))%>%
  mutate(name=fct_reorder(name,-max_2018))
opec_supply_data$updated<-dmy_hms(as.character(opec_supply_data$updated))  
opec_supply_data$updated<-as.Date(opec_supply_data$updated,format = "%b")



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("opec_oil_prod_member.png")
ggplot(opec_supply_data%>%filter(date<=Sys.Date()))+
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

#https://www.eia.gov/outlooks/steo/xls/chart-gallery.xlsx

#https://www.eia.gov/outlooks/steo/archives/jan17_base.xlsx

month_sent<-"dec"
year_sent<-"08"
xlsx_file<-paste0("https://www.eia.gov/outlooks/steo/archives/",month_sent,year_sent,"_base.xlsx")
steo_data_load <- read.xlsx(xlsxFile = xlsx_file, sheet = "3btab",startRow = 1,na.strings = "n/a",skipEmptyCols = T,skipEmptyRows = T)
year_start<-as.numeric(steo_data_load[2,3])
month_start<-grep(steo_data_load[3,3],month.abb)
end_row<-grep("- = no data available",steo_data_load[,2])-1
steo_data_load<-steo_data_load[4:end_row,]
names(steo_data_load)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
steo_data_load[,-c(1,2)]<-lapply(steo_data_load[,-c(1,2)],as.numeric)
steo_data_load$version<-paste0(month_sent,year_sent)

#for any month and year, get the STEO Production outlook

month_sent<-"dec"
year_sent<-"18"
#before jun2013 they are xls files
file_date<-ymd(paste(year_sent,month_sent,1,sep="-"))
excel_file<-ifelse(file_date>ymd("2013-06-01"),
  paste0("https://www.eia.gov/outlooks/steo/archives/",month_sent,year_sent,"_base.xlsx"),
  paste0("https://www.eia.gov/outlooks/steo/archives/",month_sent,year_sent,"_base.xls"))
temp_file<-ifelse(file_date>ymd("2013-06-01"),
                   paste0("steo_",month_sent,"_",year_sent,".xlsx"),
                  paste0("steo_",month_sent,"_",year_sent,".xls"))
download.file(excel_file,mode = "wb",destfile = temp_file)
dates<-read_excel(path=temp_file,sheet = "3btab",range = "C3:C4",col_names = F)
year_start<-dates$X__1[1]
month_start<-grep(dates$X__1[2],month.abb)
test_data<-read_excel(path=temp_file,sheet = "3btab",range = "A5:BV50",na="n/a")
#delete empty rows
test_data<-test_data[rowSums(is.na(test_data)) != ncol(test_data),]
names(test_data)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))


steo_data_fetch<-function(date_sent){
#for any month and year, get the STEO Price outlook
#testing
#  date_sent<-ymd("2008-12-01")
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

steo_data0<-filter(steo_data_fetch(ymd("2019-1-1")),Date>=ymd("2018-12-01"),forecast==0)
steo_data1<-filter(steo_data_fetch(ymd("2019-1-1")),Date>=ymd("2015-01-01"),forecast==0)
#2011-2014 histories
steo_data2<-filter(steo_data_fetch(ymd("2015-1-1")),forecast==0)
#2007-2010 histories
steo_data3<-filter(steo_data_fetch(ymd("2011-1-1")),forecast==0)
#2004-2007 histories
steo_data4<-filter(steo_data_fetch(ymd("2008-1-1")),Date<ymd("2007-01-01"),forecast==0)
steo_history<-rbind(steo_data4,steo_data3,steo_data2,steo_data1)

#current month


steo_forecast<-filter(steo_data_fetch(ymd("2019-1-1")),forecast==1)


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
  annotate("text", x = min_forecast+(max_forecast-min_forecast)/2+months(1), y = 2.5, label = "STEO\nForecast",size=3)+
  
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
  annotate("rect", fill = "grey10", alpha = 0.25, 
           xmin = min_forecast, xmax =max_forecast+months(1), ymin = -Inf, ymax = Inf)+
  annotate("text", x = min_forecast+(max_forecast-min_forecast)/2+months(1), y = -0.5, label = "STEO\nForecast",size=3)+
  
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  scale_y_continuous(sec.axis = sec_axis(~.*40, name = "WTI Spot Price ($/bbl)"))+
  scale_color_manual("",values=colors_tableau10()[1])+
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


steo_data<-steo_data_fetch(ymd("2019-1-1"))


