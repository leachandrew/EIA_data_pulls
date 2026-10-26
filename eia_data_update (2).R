if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/EIA_data_pulls")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/EIA_data_pulls")
print(getwd())
source("../andrew_base.R")

 KEY <- "91b4dca0b858df64a2279d82f71af240"
 
 
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
 
 
 

series<-paste("AEO.",seq(2015,2019),".REF",seq(2015,2019),".GEN_NA_ELEP_NA_SLR_PHTVL_NA_BLNKWH.A",sep="")
#2014 is different
series<-c("AEO.2014.REF2014.GEN_NA_ELEP_NA_SOPH_NA_NA_BLNKWH.A",series)
names<-rev(c("solar_2019","solar_2018","solar_2017","solar_2016","solar_2015","solar_2014"))
solar_data<-pdfetch_EIA(series,KEY) 
solar_data <- setNames(solar_data, names)
solar_data<-data.frame(date=index(solar_data), coredata(solar_data))
solar_data$date<-ymd(solar_data$date)
solar_melt <- melt(solar_data,id="date") 

png<-1
if(png==1)
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file="solar_eia.png", width = 1400, height = 750,res=130,type='cairo')
  if(R.version$platform ==  "x86_64-w64-mingw32")
    png(file="solar_eia.png", width = 1400, height = 750,res=130)

ggplot(solar_melt) +
  geom_line(aes(date,value,colour=variable),size=2) +
  scale_color_viridis("",labels = c("2014 AEO","2015 AEO","2016 AEO","2017 AEO","2018 AEO","2019 AEO"),discrete = TRUE)+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  guides(colour=guide_legend(byrow=FALSE))+
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




#oil production data in 
                                    
oil_production_data_19<-getEIA(ID = "AEO.2019.REF2019.SUP_NA_LFL_NA_DCP_NA_USA_MILLBRLPDY.A", key = KEY) 
oil_production_data_18<-getEIA(ID = "AEO.2018.REF2018.SUP_NA_LFL_NA_DCP_NA_USA_MILLBRLPDY.A", key = KEY) 
oil_production_data_17<-getEIA(ID = "AEO.2017.REF2017.SUP_NA_LFL_NA_DCP_NA_USA_MILLBRLPDY.A", key = KEY) 
#series_id=AEO.2017.REF2017.GEN_NA_ELEP_NA_SLR_PHTVL_NA_BLNKWH.A 
oil_production_data_16<-getEIA(ID = "AEO.2016.REF2016.SUP_NA_LFL_NA_DCP_NA_USA_MILLBRLPDY.A", key = KEY)  
oil_production_data_15<-getEIA(ID = "AEO.2015.REF2015.SUP_NA_LFL_NA_DCP_NA_USA_MILLBRLPDY.A", key = KEY)  
oil_production_data_14<-getEIA(ID = "AEO.2014.REF2014.SUP_NA_LFL_NA_DCP_NA_USA_MILLBRLPDY.A", key = KEY)  


oil_production_data<-merge(oil_production_data_19,oil_production_data_18)
oil_production_data<-merge(oil_production_data,oil_production_data_17)

oil_production_data<-merge(oil_production_data,oil_production_data_16)
oil_production_data<-merge(oil_production_data,oil_production_data_15)
oil_production_data<-merge(oil_production_data,oil_production_data_14)

names(oil_production_data)<-c("oil_production_2019","oil_production_2018","oil_production_2017","oil_production_2016","oil_production_2015","oil_production_2014")
oil_production_data<-data.frame(date=index(oil_production_data), coredata(oil_production_data))
oil_production_melt <- melt(oil_production_data,id="date") 


png<-1
if(png==1)
  set_png(file="oil_production_eia.png")
ggplot(oil_production_melt) +
  geom_line(aes(date,value,colour=variable),size=2) +
  scale_color_viridis("",labels = c("2019 AEO","2018 AEO","2017 AEO","2016 AEO","2015 AEO","2014 AEO"),discrete = TRUE)+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  guides(colour=guide_legend(byrow=FALSE))+
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
  )+    labs(y="Annual Crude Oil Production (mmbbl/d)",x="",
             title="EIA Annual Energy Outlook Crude Oil Production Forecasts",
             caption="Source: EIA API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


#oil price data in 

#AEO.2019.REF2019.PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A

series<-paste("AEO.",seq(2014,2019),".REF",seq(2014,2019),".PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A",sep="")
#2014 is different
#series<-c("AEO.2014.REF2014.PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A",series)
names<-c("wti_2014","wti_2015","wti_2016","wti_2017","wti_2018","wti_2019")
wti_data<-pdfetch_EIA(series,KEY) 
wti_data <- setNames(wti_data, names)
wti_data<-data.frame(date=index(wti_data), coredata(wti_data))
wti_data$date<-ymd(wti_data$date)
wti_melt <- melt(wti_data,id="date") 


png<-1
if(png==1)
  set_png(file="oil_price_eia.png")
ggplot(wti_melt) +
  geom_line(aes(date,value,colour=variable),size=2) +
  scale_color_viridis("",labels = c("2014 AEO","2015 AEO","2016 AEO","2017 AEO","2018 AEO","2019 AEO"),discrete = TRUE)+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  guides(colour=guide_legend(byrow=FALSE))+
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
  )+    labs(y="Nominal WTI Crude Oil Price ($/bbl)",x="",
             title="EIA Annual Energy Outlook Nominal WTI Crude Oil Price Forecasts",
             caption="Source: EIA API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

wti_melt$time<-difftime(wti_melt$date,ymd("2019-01-01"))/365
wti_melt$npv<-wti_melt$value*(.9)^as.numeric(wti_melt$time)
wti_npv<-wti_melt %>% filter(date>=ymd("2018-01-01"),date<=ymd("2040-12-31"))%>%
  group_by(variable)%>%summarize(npv=mean(npv))


#oil exports data in 
                                 
oil_exports_data_19<-getEIA(ID = "AEO.2019.REF2019.TRAD_NA_LFL_NA_EXP_NA_USA_MILLBRLPDY.A", key = KEY) 
oil_exports_data_18<-getEIA(ID = "AEO.2018.REF2018.TRAD_NA_LFL_NA_EXP_NA_USA_MILLBRLPDY.A", key = KEY) 
oil_exports_data_17<-getEIA(ID = "AEO.2017.REF2017.TRAD_NA_LFL_NA_EXP_NA_USA_MILLBRLPDY.A", key = KEY) 
#series_id=AEO.2017.REF2017.GEN_NA_ELEP_NA_SLR_PHTVL_NA_BLNKWH.A 
oil_exports_data_16<-getEIA(ID = "AEO.2016.REF2016.TRAD_NA_LFL_NA_EXP_NA_USA_MILLBRLPDY.A", key = KEY)  
oil_exports_data_15<-getEIA(ID = "AEO.2015.REF2015.TRAD_NA_LFL_NA_EXP_NA_USA_MILLBRLPDY.A", key = KEY)  
oil_exports_data_14<-getEIA(ID = "AEO.2014.REF2014.TRAD_NA_LFL_NA_EXP_NA_USA_MILLBRLPDY.A", key = KEY)  


oil_exports_data<-merge(oil_exports_data_19,oil_exports_data_18)
oil_exports_data<-merge(oil_exports_data,oil_exports_data_17)

oil_exports_data<-merge(oil_exports_data,oil_exports_data_16)
oil_exports_data<-merge(oil_exports_data,oil_exports_data_15)
oil_exports_data<-merge(oil_exports_data,oil_exports_data_14)

names(oil_exports_data)<-c("oil_exports_2019","oil_exports_2018","oil_exports_2017","oil_exports_2016","oil_exports_2015","oil_exports_2014")
oil_exports_data<-data.frame(date=index(oil_exports_data), coredata(oil_exports_data))
oil_exports_melt <- melt(oil_exports_data,id="date") 


png<-1
if(png==1)
  set_png(file="oil_exports_eia.png")
ggplot(oil_exports_melt) +
  geom_line(aes(date,value,colour=variable),size=2) +
  scale_color_viridis("",labels = c("2019 AEO","2018 AEO","2017 AEO","2016 AEO","2015 AEO","2014 AEO"),discrete = TRUE)+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  guides(colour=guide_legend(byrow=FALSE))+
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
  )+    labs(y="Annual Oil Exports (mmbbl/d)",x="",
             title="EIA Annual Energy Outlook Oil Exports Forecasts",
             caption="Source: EIA API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

 
oil_net_imports_data_19<-getEIA(ID = "AEO.2019.REF2019.TRAD_NA_LFL_NA_NETIMP_NETIMP_USA_MILLBRLPDY.A", key = KEY) 
oil_net_imports_data_18<-getEIA(ID = "AEO.2018.REF2018.TRAD_NA_LFL_NA_NETIMP_NETIMP_USA_MILLBRLPDY.A", key = KEY) 
oil_net_imports_data_17<-getEIA(ID = "AEO.2017.REF2017.TRAD_NA_LFL_NA_NETIMP_NETIMP_USA_MILLBRLPDY.A", key = KEY) 
oil_net_imports_data_16<-getEIA(ID = "AEO.2016.REF2016.TRAD_NA_LFL_NA_NETIMP_NETIMP_USA_MILLBRLPDY.A", key = KEY) 
oil_net_imports_data_15<-getEIA(ID = "AEO.2015.REF2015.TRAD_NA_LFL_NA_NETIMP_NETIMP_USA_MILLBRLPDY.A", key = KEY) 
oil_net_imports_data_14<-getEIA(ID = "AEO.2014.REF2014.TRAD_NA_LFL_NA_NETIMP_NETIMP_USA_MILLBRLPDY.A", key = KEY) 

oil_net_imports_data<-merge(oil_net_imports_data_19,oil_net_imports_data_18)
oil_net_imports_data<-merge(oil_net_imports_data,oil_net_imports_data_17)
oil_net_imports_data<-merge(oil_net_imports_data,oil_net_imports_data_16)
oil_net_imports_data<-merge(oil_net_imports_data,oil_net_imports_data_15)
oil_net_imports_data<-merge(oil_net_imports_data,oil_net_imports_data_14)

names(oil_net_imports_data)<-c("oil_net_imports_2019","oil_net_imports_2018","oil_net_imports_2017","oil_net_imports_2016","oil_net_imports_2015","oil_net_imports_2014")
oil_net_imports_data<-data.frame(date=index(oil_net_imports_data), coredata(oil_net_imports_data))
oil_net_imports_melt <- melt(oil_net_imports_data,id="date") 


png<-1
  set_png(file="oil_net_imports_eia.png")

ggplot(oil_net_imports_melt) +
  geom_line(aes(date,value,colour=variable),size=2) +
  scale_color_viridis("",labels = c("2019 AEO","2018 AEO","2017 AEO","2016 AEO","2015 AEO","2014 AEO"),discrete = TRUE)+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  guides(colour=guide_legend(byrow=FALSE))+
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
  )+    labs(y="Millions of Barrels Per Day",x="\nTime",
             title="EIA Annual Energy Outlook Net Oil Imports Forecasts",
             caption="Source: EIA API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

products_net_imports_data_19<-getEIA(ID = "AEO.2019.REF2019.SUP_NA_LFL_OPS_NPI_NA_NA_NA.A", key = KEY) 
products_net_imports_data_18<-getEIA(ID = "AEO.2018.REF2018.SUP_NA_LFL_OPS_NPI_NA_NA_NA.A", key = KEY) 
products_net_imports_data_17<-getEIA(ID = "AEO.2017.REF2017.SUP_NA_LFL_OPS_NPI_NA_NA_NA.A", key = KEY)
products_net_imports_data_16<-getEIA(ID = "AEO.2016.REF2016.SUP_NA_LFL_OPS_NPI_NA_NA_NA.A", key = KEY)
products_net_imports_data_15<-getEIA(ID = "AEO.2015.REF2015.SUP_NA_LFL_OPS_NPI_NA_NA_NA.A", key = KEY)
products_net_imports_data_14<-getEIA(ID = "AEO.2014.REF2014.SUP_NA_LFL_OPS_NPI_NA_NA_NA.A", key = KEY)
products_net_imports_data<-merge(products_net_imports_data_18,products_net_imports_data_17)
products_net_imports_data<-merge(products_net_imports_data,products_net_imports_data_16)
products_net_imports_data<-merge(products_net_imports_data,products_net_imports_data_15)
products_net_imports_data<-merge(products_net_imports_data,products_net_imports_data_14)

names(products_net_imports_data)<-c("products_net_imports_2019","products_net_imports_2018","products_net_imports_2017","products_net_imports_2016","products_net_imports_2015","products_net_imports_2014")
products_net_imports_data<-data.frame(date=index(products_net_imports_data), coredata(products_net_imports_data))
products_net_imports_melt <- melt(products_net_imports_data,id="date") 


png<-1
set_png(file="products_net_imports_eia.png")

ggplot(products_net_imports_melt) +
  geom_line(aes(date,value,colour=variable),size=2) +
  scale_color_viridis("",labels = c("2019 AEO","2018 AEO","2017 AEO","2016 AEO","2015 AEO","2014 AEO"),discrete = TRUE)+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  guides(colour=guide_legend(byrow=FALSE))+
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
  )+    labs(y="Millions of Barrels Per Day",x="\nTime",
             title="EIA Annual Energy Outlook Net Products Imports Forecasts",
             caption="Source: EIA API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



#AEO Gas trade
#http://api.eia.gov/category/?api_key=YOUR_API_KEY_HERE&category_id=3162260

#series<-paste("AEO.",seq(2015,2019),".REF",seq(2015,2019),".GEN_NA_ELEP_NA_SLR_PHTVL_NA_BLNKWH.A",sep="")
##2014 is different
#series<-c("AEO.2014.REF2014.GEN_NA_ELEP_NA_SOPH_NA_NA_BLNKWH.A",series)
#names<-rev(c("solar_2019","solar_2018","solar_2017","solar_2016","solar_2015","solar_2014"))

#2006 data
#old tab 13 https://www.eia.gov/outlooks/archive/aeo06/excel/aeotab_6.xls
#for(pipe in names){
#  file_name<-paste("http://www.neb-one.gc.ca/open/energy/",pipe,"-throughput-and-capacity-dataset-eng.csv",sep="")
  #inv_file<-"http://ecccdocs.techno-science.ca/documents/ECCC_STB_SRAD_GHG_ECON_NS_1990-2016_prelim_EN.xlsx"
 file_name<-"https://www.eia.gov/outlooks/archive/aeo06/excel/aeotab_13.xls"
 download.file(file_name,"aeo-data.xls",mode="wb")
 old_aeo_data <- read_excel("aeo-data.xls")
 old_aeo_data<-old_aeo_data[c(10,14,15,17,18,19,21),]
 old_aeo_data[1,1]<-"year"
 old_aeo_data<-t(old_aeo_data)
 data_06<-data.frame(old_aeo_data[-1,])#read all but first row
 names(data_06)<-old_aeo_data[1,]  #get names from first row
 old_aeo_data<-NULL
 names(data_06)<-gsub(" 1/","",names(data_06))
 names(data_06)<-gsub(" 2/","",names(data_06))
 names(data_06)<-gsub(" 3/","",names(data_06))
 rownames(data_06)<-NULL
 data_06$date<-ymd(paste(data_06$year,"12-31",sep=""))
 names(data_06)<-c("year","Dry Gas Production","Supplemental Natural Gas","Net Imports","Pipeline Net Imports","Liquefied Natural Gas Net Imports",   
                   "Total Supply","date" )
 data_06$year<-NULL
 data_06<-melt(data_06,id="date")
 data_06$year<-"AEO2006"
 data_06$case<-"Reference"
 data_06$series<-data_06$variable
 data_06$value<-as.numeric(data_06$value)
 
 
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

gas_trade_data$`Imports : Pipeline Imports`<-gas_trade_data$`Imports : Pipeline Imports from Canada`+gas_trade_data$`Imports : Pipeline Imports from Mexico`
gas_trade_data$`Exports : Pipeline Exports`<-gas_trade_data$`Exports : Pipeline Exports to Canada`+gas_trade_data$`Exports : Pipeline Exports to Mexico`

gas_trade_data$`Pipeline Net Imports`<-gas_trade_data$`Imports : Pipeline Imports`- gas_trade_data$`Exports : Pipeline Exports`
gas_trade_data$`Liquefied Natural Gas Net Imports`<-gas_trade_data$`Imports : Liquefied Natural Gas Imports` - gas_trade_data$`Exports : Liquefied Natural Gas Exports`

  
gas_trade_data<-gas_trade_data %>% melt(id="date",variable.name="series") 
#make it annual
gas_trade_data<-gas_trade_data %>% mutate(year=year(date)) %>% group_by(year,series) %>%
  summarise(value=12*mean(value,na.rm = T)) %>% ungroup %>% #now annual values based on mean non-na month
  mutate(date=ymd(paste(year,12,31,sep = "-")),year=NULL)
#adjust to TCF per year
gas_trade_data$value<-gas_trade_data$value/10^6


#build AEO list

subs<-data_fetch(KEY,cat=3162260)
AEO_data<-subs$Series_IDs
for(j in seq(2018,2014)){
  print(paste("working on AEO",j,sep=""))
  work_data<-subs$Series_IDs
  work_data$series_id<-gsub("2019",j,work_data$series_id)
  work_data$name<-gsub("2019",j,work_data$name)
  AEO_data<-rbind(AEO_data,work_data)
}
AEO_data$name<-gsub("Natural Gas : Volumes : ","",AEO_data$name)

test<- pdfetch_EIA(AEO_data$series_id,KEY)
series_data<-data.frame(date=index(test), coredata(test))
series_data$date<-ymd(series_data$date)
series_data <- setNames(series_data, c("date",AEO_data$name))

#melt it
series_data<-series_data %>% melt(id="date") %>% na.omit()


#series_data$`Pipeline Net Imports`<-series_data$`Imports : Pipeline Imports`- gas_trade_data$`Exports : Pipeline Exports`
#series_data$`Liquefied Natural Gas Net Imports`<-gas_trade_data$`Imports : Liquefied Natural Gas Imports` - gas_trade_data$`Exports : Liquefied Natural Gas Exports`



#get the year for each
name_split<-do.call(rbind,strsplit(as.character(series_data$variable),", "))

series_data$series<-name_split[,1]
series_data$case<-name_split[,2]
series_data$year<-name_split[,3]

gas_trade_data$year<-"Historical Data"
gas_trade_data$variable<-gas_trade_data$series

joint_data<-bind_rows(series_data,gas_trade_data,data_06)



totals<-c("Exports : Pipeline Exports to Canada",
              "Exports : Pipeline Exports to Mexico",
              "Exports : Liquefied Natural Gas Exports")




export_set<-c("Exports : Pipeline Exports to Canada",
             "Exports : Pipeline Exports to Mexico",
             "Exports : Liquefied Natural Gas Exports")
import_set<-c("Imports : Pipeline Imports from Canada",
              "Imports : Pipeline Imports from Mexico",
              "Imports : Liquefied Natural Gas Imports")
joint_data<-joint_data %>% filter(series %in% import_set | series %in% export_set) %>%
  mutate(value=ifelse(series %in% import_set,-1*value,value))

joint_data<-joint_data %>% mutate(year_test=as.numeric(gsub("AEO","",year))-1,
                                  cutoff_date=ifelse(year=="Historical Data","1901-12-31",paste(year_test,"-12-31")),
                                  cutoff_date=ymd(cutoff_date))
                                  

data_year<-2014
test_data<-joint_data %>% filter(year == paste("AEO",data_year,sep = "") | (year=="Historical Data" & year(date)<data_year))
plot_name<-paste("EIA_AEO_gas_trade",data_year,sep="_")
p<-ggplot() +
  geom_area(data=filter(test_data,year!="Historical Data"), aes(date,value,group=series,fill=series),position="stack")+
  geom_area(data=filter(test_data,year=="Historical Data"), aes(date,value,group=series,fill=series),position="stack")+
  scale_fill_manual("",values=c(colors_tableau10()[1:3],colors_tableau10_medium()[1:3]))+
  scale_y_continuous(limits = c(-7,11))+
  scale_x_date(date_breaks = "5 years",labels = date_format("%b\n%Y"))+
  weekly_small()+
  labs(x=NULL,y="Natural gas trade (trillion cubic feet)",
       title="Annual Natural Gas Trade",
       subtitle=paste("EIA Annual Energy Outlook ",data_year,", Reference Case",sep = ""),
       caption=paste("Source: EIA Annual Energy Outlook data ",format(as.Date(Sys.Date()), format="%B %e, %Y"),". Graph by Andrew Leach.",sep=""))
assign(plot_name,p)
set_png(paste(plot_name,".png",sep=""))
p
dev.off()


data_year<-2006
data_series<-c("Pipeline Net Imports","Liquefied Natural Gas Net Imports")
test_data<-joint_data %>% filter(year == paste("AEO",data_year,sep = "") | (year=="Historical Data" & year(date)<data_year))plot_name<-paste("EIA_AEO_gas_trade",data_year,sep="_")
plot_name<-paste("EIA_AEO_gas_trade",data_year,sep="_")
p<-ggplot() +
  geom_area(data=filter(data_06,variable %in% data_series), aes(date,-value,group=series,fill=series),position="stack")+
  #geom_area(data=filter(test_data,year=="Historical Data"), aes(date,value,group=series,fill=series),position="stack")+
  scale_fill_manual("",values=c(colors_tableau10()[1:3],colors_tableau10_medium()[1:3]),
                    labels=c("Pipeline Net Exports","LNG Net Exports"))+
  scale_y_continuous(limits = c(-7,11))+
  scale_x_date(date_breaks = "5 years",labels = date_format("%b\n%Y"))+
  weekly_small()+
  labs(x=NULL,y="Natural gas trade (trillion cubic feet)",
       title="Annual Natural Gas Trade",
       subtitle=paste("EIA Annual Energy Outlook ",data_year,", Reference Case",sep = ""),
       caption=paste("Source: EIA Annual Energy Outlook data ",format(as.Date(Sys.Date()), format="%B %e, %Y"),". Graph by Andrew Leach.",sep=""))
assign(plot_name,p)
set_png(paste(plot_name,".png",sep=""))
p
dev.off()


  #test_data<-joint_data %>% filter(year == "AEO2014" | year=="Historical Data") %>%
  #filter(date>cutoff_date)

#NEED TO CUT-OFF AEO DATA WHICH IS LESS THAN THE FORECAST DATE AND CUT OFF HISTORY WHICH IS GREATER THAN THE FORECAST DATE

#left joint historical data and AEO data, don't stack it?


#take out the AEO data for years where we have historicals

ggplot() +
  geom_area(data=test_data, aes(date,value,group=series,fill=series),position="stack")+
  geom_area(data=filter(test_data,year!="Historical Data"), aes(date,value,group=series,fill=series),position="stack")+
  geom_area(data=filter(test_data,year=="Historical Data"), aes(date,value,group=series,fill=series),position="stack")+
  scale_fill_manual(values=c(colors_tableau10()[1:3],colors_tableau10_medium()[1:3]))+
  
    
  
  facet_grid(~year)
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  guides(colour=guide_legend(byrow=FALSE))+
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
  )+    labs(y="Millions of Barrels Per Day",x="\nTime",
             title="EIA Annual Energy Outlook Net Products Imports Forecasts",
             caption="Source: EIA API\nGraph by Andrew Leach")





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
  scale_x_date(date_breaks = "6 months", date_labels =  "%m %Y",limits=c(as.Date("2007-07-01"),max(df1$Date))) +
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
set_png(file="boe_oil_gas_ngls.png")
#jpeg(file="AESO_LTO.jpg", width = 1400, height = 750)
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




#Refinery acquisition costs

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
set_png(file=file_name)
ggplot(subset(df1,Date>"2007-01-01"),aes(Date,value,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  scale_color_viridis("",labels=name_list[],discrete=TRUE,option="C")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y",limits=c(max(as.Date("2007-07-01"),min(df1$Date)),Sys.Date())) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12,face = "bold"),
    axis.text.y =element_text(size = 12,face = "bold", colour="black"),
    axis.text.x=element_text(size = 12,face = "bold", colour="black",angle=90, hjust=1),
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
#replace NAs with zeros

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

#replace NAs with zero
df1<-melt(monthly,id=c("Date"))

df1$value[is.na(df1$value)] <- 0

file_name<-"US_import_API.png"
set_png(file=file_name)
ggplot(subset(df1,Date>"2007-01-01"),aes(Date,value/100,group = variable,colour=variable)) +
  geom_line(size=1.7) +
  #geom_point(size=1) +
  scale_color_viridis("",labels=name_list[],discrete=TRUE,option="D",direction=-1)+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2007-07-01"),min(df1$Date)),Sys.Date())) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
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


set_png(file="Ref_yield.png")
#jpeg(file="AESO_LTO.jpg", width = 1400, height = 750)
ggplot(df1,aes(Date,value,group = variable,colour=variable,fill=variable)) +
  #geom_line(size=1.7) +
  #geom_point(size=1) +
  geom_area(position="stack")+
  scale_fill_viridis("Product",labels=c("Diesel","Gasoline","Jet","Resid","Other"),discrete=TRUE,option="D")+   
  scale_colour_viridis("",labels=c("Diesel","Gasoline","Jet","Resid","Other"),discrete=TRUE,guide = 'none')+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
    #scale_colour_brewer(NULL,labels=c("Brent","WTI"),type = "seq", palette = "Paired", direction = -1)+
  scale_x_date(date_breaks = "2 years", date_labels =  "%b\n%Y",expand=c(0,0)) +
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
    axis.text.x=element_text(size = 16,face = "bold", colour="black")
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


#how do US refineries get their crude?
subs<-data_fetch(KEY,cat=313604)
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
trans_padd<-data.frame(do.call(rbind,cat_store))
trans_data<-data.frame(pdfetch_EIA(trans_padd$series_id,KEY),stringsAsFactors = F)
trans_data$date<-ymd(row.names(trans_data))
row.names(trans_data)<-NULL
#calculate ranges
#weekly$max_exports<-as.data.frame(rollapply(weekly$`Exports of Crude Oil`,52*5,max,fill=NA,align = c("right")))[,1]
#weekly$min_exports<-as.data.frame(rollapply(weekly$`Exports of Crude Oil`,52*5,min,fill=NA,align = c("right")))[,1]
#weekly$avg_exports<-as.data.frame(rollapply(weekly$`Exports of Crude Oil`,52*5,mean,fill=NA,align = c("right")))[,1]

#trans_padd$test<-names(trans_data)

trans_data<-melt(trans_data,id="date",variable.name = "series_id",value.name = "quantity") #convert data to long form with series ids
trans_data$series_id<-as.character(trans_data$series_id)
trans_data<-trans_data %>% left_join(trans_padd,by=c("series_id"="series_id"))


df1<-trans_data %>% filter(date>=Sys.Date()-years(10),grepl("PADD",name))
df1$name<-as.character(df1$name)

#strip out PADD IDs
names_strp <-do.call("rbind",strsplit(df1$name, "PADD "))[,2]
names_strp <-do.call("rbind",strsplit(names_strp, ") "))[,1]
df1$Variable <-do.call("rbind",strsplit(df1$name, ") "))[,2]
df1$Variable <-do.call("rbind",strsplit(df1$Variable, ", Annual"))[,1]
df1$Region<-do.call("rbind",strsplit(df1$name, " \\("))[,1]
df1$PADD<-factor(names_strp,labels = paste("PADD ",levels(df1$PADD),sep = ""))
df1$PADD<-factor(paste(df1$Region,"\n(",as.character(df1$PADD),")",sep = ""))
df1$PADD<-fct_relevel(df1$PADD, "Midwest\n(PADD 2)", after = 1)
df1$Variable<-factor(df1$Variable)
df1$combo<-interaction(df1$Region,df1$Variable,sep=" ")
set_png(file="rail_receipts.png")
ggplot(subset(df1,Variable %in% c("Domestic Crude Oil Refinery Receipts by Tank Cars (Rail)", "Foreign Crude Oil Refinery Receipts by Tank Cars (Rail)")),
       aes(date,quantity/365,group = Variable,colour=Variable)) +
  geom_line(size=1.7) +
  facet_grid(~PADD)+
  #geom_ribbon(aes(x = date, ymin = min_imports, ymax = max_imports,fill="grey80"),alpha=.25, linetype = 0)+
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="D")+
  scale_fill_viridis("",discrete=TRUE,option="D")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y",expand=c(0,0)) +
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
    text = element_text(size = 12,face = "bold"),
    axis.text.y =element_text(size = 12,face = "bold", colour="black"),
    axis.text.x=element_text(size = 12,face = "bold", colour="black",angle=90, hjust=1),
  )+
  labs(y="Refinery Receipts by Rail (Annual, 1000s of Barrels per Day)",x="Year",
       title=paste("Receipts of Crude by Rail to US Refineries by Region",sep=""),
       caption="Source: EIA API, graph by Andrew Leach.")
dev.off()





#charge capacity by process
#Thermal cracking capacity in the US by regions/state 309981
subs<-data_fetch(KEY,cat=309930)
sub_cats<-subs$Sub_Categories[c(1,2,3),]
#build list from sub_cats
cat_store <- list()
cat_count<-1
  for (cat in sub_cats$category_id) {
    #cat<-sub_cats$category_id[1]
    series<-data_fetch(KEY,cat=cat)
    cat_store[[cat_count]]<-series$Series_IDs
    cat_count<-cat_count+1
  }
cap_padd<-data.frame(do.call(rbind,cat_store))
cap_data<-data.frame(pdfetch_EIA(cap_padd$series_id,KEY),stringsAsFactors = F)
cap_data$date<-ymd(row.names(cap_data))
row.names(cap_data)<-NULL
#calculate ranges
#weekly$max_exports<-as.data.frame(rollapply(weekly$`Exports of Crude Oil`,52*5,max,fill=NA,align = c("right")))[,1]
#weekly$min_exports<-as.data.frame(rollapply(weekly$`Exports of Crude Oil`,52*5,min,fill=NA,align = c("right")))[,1]
#weekly$avg_exports<-as.data.frame(rollapply(weekly$`Exports of Crude Oil`,52*5,mean,fill=NA,align = c("right")))[,1]


cap_data<-melt(cap_data,id="date",variable.name = "series_id",value.name = "quantity") #convert data to long form with series ids
cap_data$series_id<-as.character(cap_data$series_id)
cap_data<-cap_data %>% left_join(cap_padd,by=c("series_id"="series_id"))


df1<-cap_data %>% filter(date>=Sys.Date()-years(18),grepl("PADD",name))
df1$name<-as.character(df1$name)


#strip out PADD IDs
names_strp <-do.call("rbind",strsplit(df1$name, "PADD "))[,2]
names_strp <-do.call("rbind",strsplit(names_strp, ") "))[,1]
df1$Variable <-do.call("rbind",strsplit(df1$name, ") "))[,2]
df1$Variable <-do.call("rbind",strsplit(df1$Variable, " Downstream Charge Capacity "))[,1]
df1$interact<-do.call("rbind",strsplit(df1$name, " Downstream Charge Capacity "))[,1]
df1$Region<-do.call("rbind",strsplit(df1$name, " \\("))[,1]
df1$PADD<-factor(names_strp,labels = paste("PADD ",levels(df1$PADD),sep = ""))
df1$PADD<-factor(paste(df1$Region,"\n(",as.character(df1$PADD),")",sep = ""))
df1$PADD<-fct_relevel(df1$PADD, "Midwest\n(PADD 2)", after = 1)
df1$Variable<-factor(df1$Variable,levels=c("Refinery Vacuum Distillation","Refinery Thermal Cracking","Refinery Thermal Cracking, Coking"))
set_png(file="refining_capacity.png")
ggplot(subset(df1,Variable!="Refinery Thermal Cracking, Coking"),aes(date,quantity/1000,group = Variable,colour=Variable)) +
  geom_line(size=1.7) +
  facet_grid(~PADD)+
  #geom_ribbon(aes(x = date, ymin = min_imports, ymax = max_imports,fill="grey80"),alpha=.25, linetype = 0)+
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="D")+
  scale_fill_viridis("",discrete=TRUE,option="D")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y",expand=c(0,0)) +
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
    text = element_text(size = 12,face = "bold"),
    axis.text.y =element_text(size = 12,face = "bold", colour="black"),
    axis.text.x=element_text(size = 12,face = "bold", colour="black",angle=90, hjust=1),
  )+
  labs(y="Dowstream Charge Capacity (1000 Barrels per Day)",x="Year",
       title=paste("Downstream Charge Capacity of US Refineries by Region and Process",sep=""),
       caption="Source: EIA API, graph by Andrew Leach.")
dev.off()






#weekly inputs and utilization http://api.eia.gov/category/?api_key=YOUR_API_KEY_HERE&category_id=296823
#PADDs are sub_categories
subs<-data_fetch(KEY,cat=296823)
sub_cats<-subs$Sub_Categories

data_store <- list()
stack<-1
for (cat in sub_cats$category_id) {
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


util_padd<-data.frame(do.call(rbind,data_store))
util_data<-data.frame(pdfetch_EIA(util_padd$series_id,KEY),stringsAsFactors = F)
util_data$date<-ymd(row.names(util_data))
row.names(util_data)<-NULL
#calculate ranges
#weekly$max_exports<-as.data.frame(rollapply(weekly$`Exports of Crude Oil`,52*5,max,fill=NA,align = c("right")))[,1]
#weekly$min_exports<-as.data.frame(rollapply(weekly$`Exports of Crude Oil`,52*5,min,fill=NA,align = c("right")))[,1]
#weekly$avg_exports<-as.data.frame(rollapply(weekly$`Exports of Crude Oil`,52*5,mean,fill=NA,align = c("right")))[,1]


util_data<-melt(util_data,id="date",variable.name = "series_id",value.name = "quantity") #convert data to long form with series ids
util_data$series_id<-as.character(util_data$series_id)
util_data<-util_data %>% left_join(util_padd,by=c("series_id"="series_id"))


df1<-util_data %>% filter(date>=Sys.Date()-years(5),grepl("PADD",name),grepl("Net Input of Crude Oil, Weekly",name))


#strip out PADD IDs
names_strp <-do.call("rbind",strsplit(df1$name, "PADD "))[,2]
names_strp <-do.call("rbind",strsplit(names_strp, ") "))[,1]
df1$PADD<-factor(names_strp,labels = paste("PADD",levels(df1$PADD)))

set_png(file="refiner_inputs.png")
ggplot(df1,aes(date,quantity,group = PADD,colour=PADD)) +
  geom_line(size=1.7) +
  #geom_ribbon(aes(x = date, ymin = min_imports, ymax = max_imports,fill="grey80"),alpha=.25, linetype = 0)+
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="D")+
  scale_fill_viridis("",discrete=TRUE,option="D")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y",expand=c(0,0)) +
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
  labs(y="Crude Oil Inputs to Refineries \n(Weekly, Thousands of Barrels per Day)",x="Year",
       title=paste("Inputs of Crude Oil to US Refineries by Region",sep=""),
       caption="Source: EIA API, graph by Andrew Leach.")
dev.off()








#imports and exports

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
  set_png(file=file_name)
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






#Refinery acquisition costs 293683




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




#LNG

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



#movements between PADDs
#crude movement patterns within US
subs<-data_fetch(KEY,cat=330987)
series <-subs$Series_IDs %>% filter(grepl("Monthly",name))
series$name<-as.character(series$name)
series$To<-do.call("rbind",strsplit(series$name, " Receipts by "))[,1]
Rest<-do.call("rbind",strsplit(series$name, " Receipts by "))[,2]
series$Mode<-do.call("rbind",strsplit(Rest, " from "))[,1]
Rest<-do.call("rbind",strsplit(Rest, " from "))[,2]
series$From<-do.call("rbind",strsplit(Rest, " of "))[,1]
Rest<-do.call("rbind",strsplit(Rest, " of "))[,2]
series$Commodity<-do.call("rbind",strsplit(Rest, ", "))[,1]
series$freq<-do.call("rbind",strsplit(Rest, ", "))[,2]

movements_padd<-series

#production by PADD

#http://api.eia.gov/category/?api_key=YOUR_API_KEY_HERE&category_id=296686


subs<-data_fetch(KEY,cat=296686)
#sub_cats<-subs$Sub_Categories

  series <-subs$Series_IDs %>% filter(grepl("PADD",name),grepl("Monthly",name),units=="Thousand Barrels per Day")
  series$name<-as.character(series$name)
  series$To<-do.call("rbind",strsplit(series$name, " Field Production of "))[,1]
  series$Mode<-"Production"
  Rest<-do.call("rbind",strsplit(series$name, " Field Production of "))[,2]
  series$From<-series$To
  series$Commodity<-do.call("rbind",strsplit(Rest, ", Monthly"))[,1]
  series$freq<-"Monthly"
  
production_padd<-series


#imports to PADDs
#http://api.eia.gov/category/?api_key=YOUR_API_KEY_HERE&category_id=314562

subs<-data_fetch(KEY,cat=314562)
sub_cats<-subs$Sub_Categories

data_store <- list()
stack<-1
for (cat in sub_cats$category_id) {
  #for testing
  #cat<-sub_cats$category_id[2]
  subs<-data_fetch(KEY,cat=cat)
  series <-subs$Series_IDs %>% filter(grepl("Monthly",name),units=="Thousand Barrels per Day")
  series$name<-as.character(series$name)
  series$To<-do.call("rbind",strsplit(series$name, " Imports of "))[,1]
  series$Mode<-"Import"
  Rest<-do.call("rbind",strsplit(series$name, " Imports of "))[,2]
  series<-series[!grepl("Imports From ",Rest),]
  Rest<-Rest[!grepl("Imports From",Rest)] #take out exports to the PADD itself - odd
  series<-series[!grepl("Imports from ",Rest),]
  Rest<-Rest[!grepl("Imports from",Rest)] #take out exports to the PADD itself - odd
  series$From<-"Import"
  series$Commodity<-do.call("rbind",strsplit(Rest, ", Monthly"))[,1]
  series$freq<-"Monthly"
  data_store[[stack]]<-series
  stack<-stack+1
}


imports_padd<-data.frame(do.call(rbind,data_store))




#import processing by PADDs

subs<-data_fetch(KEY,cat=378012)
sub_cats<-subs$Sub_Categories

data_store <- list()
stack<-1
  for (cat in sub_cats$category_id) {
    subs<-data_fetch(KEY,cat=cat)
    series <-subs$Series_IDs %>% filter(grepl("Monthly",name),units=="Thousand Barrels per Day")
    series$name<-as.character(series$name)
    series$To<-do.call("rbind",strsplit(series$name, " Imports by PADD of Processing "))[,1]
    series$Mode<-"Import"
    Rest<-do.call("rbind",strsplit(series$name, " Imports by PADD of Processing "))[,2]
    Rest[Rest=="of Crude Oil, Monthly"]="from World of Crude Oil, Monthly" #insert world as source
    Rest<-do.call("rbind",strsplit(Rest, "from "))[,2]
    series$From<-do.call("rbind",strsplit(Rest, " of "))[,1]
    Rest<-do.call("rbind",strsplit(Rest, " of "))[,2]
    series$Commodity<-do.call("rbind",strsplit(Rest, ", "))[,1]
    series$freq<-do.call("rbind",strsplit(Rest, ", "))[,2]
    data_store[[stack]]<-series
    stack<-stack+1
  }


#exports from PADDs

subs<-data_fetch(KEY,cat=315918)
sub_cats<-subs$Sub_Categories

data_store <- list()
stack<-1
for (cat in sub_cats$category_id[-1]) {
#for testing
  #cat<-sub_cats$category_id[2]
  subs<-data_fetch(KEY,cat=cat)
  series <-subs$Series_IDs %>% filter(grepl("Monthly",name),units=="Thousand Barrels per Day")
  series$name<-as.character(series$name)
  series$To<-"Export"
  series$From<-do.call("rbind",strsplit(series$name, " Exports of "))[,1]
  series$Mode<-"Export"
  Rest<-do.call("rbind",strsplit(series$name, " Exports of "))[,2]
  series<-series[!grepl("Exports to",Rest),]
  Rest<-Rest[!grepl("Exports to",Rest)] #take out exports to the PADD itself - odd
  series$Commodity<-do.call("rbind",strsplit(Rest, ", Monthly"))[,1]
  series$freq<-"Monthly"
  data_store[[stack]]<-series
  stack<-stack+1
}


exports_padd<-data.frame(do.call(rbind,data_store))



padd_data<-rbind(imports_padd,movements_padd)
padd_data<-rbind(padd_data,exports_padd)
padd_data<-rbind(padd_data,production_padd)
padd_data$series_id<-as.character(padd_data$series_id)
padd_data$series_id2<-gsub('\\-','.',padd_data$series_id)
  
  
#get all the movements and imports data

crude_series<-padd_data %>% filter(Commodity=="Crude Oil")

#all_data<-data.frame(pdfetch_EIA(padd_data$series_id,KEY),stringsAsFactors = F)
crude_data<-data.frame(pdfetch_EIA(crude_series$series_id,KEY),stringsAsFactors = F)






library(dplyr)

#all_data<-data.frame(all_data)
crude_data$date=ymd(rownames(crude_data))


movements<-melt(crude_data,id="date",variable.name = "series_id",value.name = "quantity") #convert data to long form with series ids
movements$series_id<-as.character(movements$series_id)
movements<-movements %>% left_join(crude_series,by=c("series_id"="series_id2"))
movements$quantity[movements$units=="Thousand Barrels"]<-movements$quantity[movements$units=="Thousand Barrels"]/
  days_in_month(movements$date[movements$units=="Thousand Barrels"])
movements$units[movements$units=="Thousand Barrels"]<-"Thousand Barrels per Day"
movements$units<-factor(movements$units)
movements$quantity[is.na(movements$quantity)] <- 0


#test some graphing

keeps<-c("East Coast (PADD 1)","Midwest (PADD 2)","Gulf Coast (PADD 3)","Rocky Mountain (PADD 4)",
         "West Coast (PADD 5)","World","Canada")


df1<-movements %>% filter(date>=ymd("2000-01-01") & Commodity=="Crude Oil",To != "U.S.")%>%
  mutate(To=factor(To)) %>% mutate(To=fct_collapse(To,
                          "West Coast (PADD 5)" = c("Federal Offshore PADD 5", "West Coast (PADD 5)"),
                          "International Trade" = c("Export")
                          )
                          ) %>%
  mutate(From=factor(From)) %>% mutate(From=fct_collapse(From,
                          "West Coast (PADD 5)" = c("Federal Offshore PADD 5", "West Coast (PADD 5)"),
                          "International Trade" = c("Import")
                          )
                          ) %>%
  group_by(date,To,From,Commodity,Mode) %>% summarize(quantity=sum(quantity))

#df1$From<-fct_recode(df1$From, "Imports from Canada" = "Canada", "Other Imports" = "Other")
df1$From<- factor(df1$From, levels=c("East Coast (PADD 1)","Midwest (PADD 2)","Gulf Coast (PADD 3)","Rocky Mountain (PADD 4)",
"West Coast (PADD 5)","Production","International Trade"))
df1$To<- factor(df1$To, levels=c("East Coast (PADD 1)","Midwest (PADD 2)","Gulf Coast (PADD 3)","Rocky Mountain (PADD 4)",
                                     "West Coast (PADD 5)","International Trade"))

df_test<-df1 %>% filter(From=="East Coast (PADD 1)")

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("padd_imports.png")
moves_in<-ggplot(filter(df1,To!="International Trade"),aes(date,quantity,group = From,colour=From,fill=From)) +
  geom_area(position = "stack")+
  facet_grid(~To)+
scale_x_date()+
  scale_fill_manual("Source of Crude",values = colors_tableau10()[-7],guide = "legend")+
  scale_colour_manual("Source of Crude",values=colors_tableau10()[-7],guide = "legend")+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14),
    legend.title = element_text(colour="black", size = 14,vjust=0.5),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 18,face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 10, colour = "black", angle = 0)
  )+
labs(y="Crude Oil (MMbbl/d)",x="Year",
       title=paste("US Crude Oil Source By Region"),
        subtitle=paste("Production appears as sourced from within a PADD, so PADD 2 production is oil from PADD 2 in PADD 2"),
       caption="Source: EIA API, graph by Andrew Leach.")
print(moves_in)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("padd_exports.png")
moves_out<-ggplot(filter(df1,Mode!="Production",From!="International Trade"),aes(date,quantity,group = To,colour=To,fill=To)) +
  geom_area(position = "stack")+
  facet_grid(~From)+
  scale_x_date()+
  scale_fill_manual("Destination of Crude",values = colors_tableau10()[-7],guide = "legend")+
  scale_colour_manual("Destination of Crude",values=colors_tableau10()[-7],guide = "legend")+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14),
    legend.title = element_text(colour="black", size = 14,vjust=0.5),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 18,face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 10, colour = "black", angle = 0)
  )+
  labs(y="Crude Oil (MMbbl/d)",x="Year",
       title=paste("US Crude Oil Destination by Region"),
       #subtitle=paste("Production appears as sourced from within a PADD, so PADD 2 production is oil from PADD 2 in PADD 2"),
       caption="Source: EIA API, graph by Andrew Leach.")
print(moves_out)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


library(grid)
library(gridExtra)
library(lemon)
#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#grab the legend without a title
mylegend<-arrangeGrob(g_legend(moves_in + theme(legend.title = element_blank(),
                                                legend.spacing = unit(1.0, 'cm'),
                                                legend.text =  element_text(margin = margin(t = 0, r = 1, b = 0, l = 0)), nrow=1)))



png<-1
if(png==1)
  set_png(file=paste("movements.png",sep=""),height = 1200,width=2000)
grid.arrange(arrangeGrob(moves_in +labs(caption="Note: Production denoted as supply from within the PADD, i.e. PADD 2 production appears as PADD 2 supply from PADD 2.")+
                           theme(legend.position="none",
                                           legend.margin=margin(c(0,0,0,0),unit="cm"),
                                           legend.text = element_text(colour="black", size = 14, face = "bold"),
                                            plot.caption = element_text(size = 12,face = "italic", colour="black"),          
                                            plot.title = element_text(size = 12,face = "bold", colour="black"),
                                          plot.subtitle = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           text = element_text(size = 10,face = "bold"),
                                           axis.text = element_text(size =12,face = "bold", colour="black"),
                                           axis.text.x = element_blank(),
                                          axis.title.x = element_blank(),
                                          strip.text = element_text(colour="black", size = 8, face = "bold"),
                                          
),
moves_out+
  theme(legend.position="none",
        plot.title = element_text(size = 12,face = "bold", colour="black"),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 10,face = "bold"),
        axis.text = element_text(size = 12,face = "bold", colour="black"),
        axis.title.x = element_blank(),
        strip.text = element_text(colour="black", size = 8, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))),

ncol=1,heights=c(3,3)),
mylegend, 
nrow=2,heights=c(10, 1),bottom =text_grob(
  "Source: Data via EIA, graph by Andrew Leach",
  face = "italic", color = "black",size=14,just="center",lineheight = 2
)
#,
#top =text_grob(
#  "US PADD-to-PADD Crude Oil Movements",
#  face = "bold", color = "black",size=14,just="center",lineheight = 1
#)
#
)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



#get canadian import processing by PADD

imports_proc_padd<-data.frame(do.call(rbind,data_store))
imports_proc_canada<-imports_proc_padd %>% filter(From=="Canada")
can_crude_proc<-EIA_to_DF(imports_proc_canada)
names(can_crude_proc)<-gsub(" Imports by PADD of Processing from Canada of Crude Oil, Monthly","",names(can_crude_proc))
can_crude_proc<-melt(can_crude_proc,id="date",variable.name = "PADD",value.name = "volume")
set_png("crude_by_proc_padd.png")
ggplot(can_crude_proc,aes(date,volume,group = PADD,colour=PADD,fill=PADD)) +
  geom_area(position = "stack")+
  scale_fill_manual(name="",values=colors_ua10())+
  scale_colour_manual(name="",values=colors_ua10())+
  guides(colour = guide_legend(show = FALSE),fill = guide_legend(nrow = (2),byrow=F)) +
  scale_x_date(date_breaks = "2 years",date_labels = "%b\n%Y")+
  #scale_y_continuous(limits=c(0,300),expand=c(0,0))+
  annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2014-08-01"), xmax =as.Date("2014-08-10"),
           ymin = 0, ymax = 4200) +
  annotate("text", x = as.Date("2013-01-01"), y = 3500, label = "Enbridge and\n TransCanada\nGulf Coast access\n pipelines begin to\ncome into service",size=2.4)+
  
  
  weekly_small()+    labs(y="US Processing of Canadian Crude (1000 bbl/d)",x="Date",
             title="Crude Imports from Canada by US Processing Area",
             caption="Source: EIA Data\nGraph by Andrew Leach")
dev.off()
  



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



#west coast canadian crude

subs<-data_fetch(KEY,cat=1293024)

testing<-t(subs$Series_IDs$series_id)
names<-t(subs$Series_IDs$name)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
padd5_imports<-data.frame(date=index(test), coredata(test))
padd5_imports$date<-as.Date(padd5_imports$date,format = "%m/%d/%Y")

can_data<-cbind(padd5_imports$date,padd5_imports[,grep("Canada", colnames(padd5_imports))])  #all annual columns
can_data<-can_data[,c(1,grep("Monthly", colnames(can_data)))]  #all monthly columns
can_data<-can_data[,-grep("Region", colnames(can_data))]  #all monthly columns
names(can_data)[1]<-"Date"

#annuals<-na.omit(annuals)
names(annuals)[1]<-paste("Date")  
name_list<- gsub("\\.", " ", names(can_data))
name_list<- gsub("\\Imports.of.", "", name_list)
name_list<- gsub("from Canada to refineries in PADD5  West Coast   Monthly", "", name_list)
can_data <- setNames(can_data,name_list)


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
names(can_data)<-firstup(names(can_data))
source("tableau.R")
df1<-melt(can_data,id="Date",value.name = "Imports",variable.name = "Grade")
df1<-df1 %>% filter(!grepl("All",Grade))

hybrd.rplc_if    <- function(x) { mutate_if(x, is.numeric, funs(replace(., is.na(.), 0))) }
df1<-hybrd.rplc_if(df1)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("padd_5_imports.png")
ggplot(subset(df1,Grade!="All grades of crude oil"),aes(Date,Imports/days_in_month(Date),group = Grade,colour=Grade,fill=Grade)) +
  geom_area(position = "stack")+
  scale_fill_manual(name="",values=colors_tableau10())+
  scale_colour_manual(name="",values=colors_tableau10())+
  guides(colour = guide_legend(show = FALSE),fill = guide_legend(nrow = (2),byrow=F)) +
  scale_y_continuous(limits=c(0,300),expand=c(0,0))+
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
  )+    labs(y="Imports (thousands of barrels per day)",x="Date",
             title="Imports from Canada to US West Coast Refineries",
             caption="Source: EIA Data\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#PADD 2 canadian crude

subs<-data_fetch(KEY,cat=1293021)

testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
padd2_imports<-data.frame(date=index(test), coredata(test))
padd2_imports$date<-as.Date(padd2_imports$date,format = "%m/%d/%Y")

can_data<-cbind(padd2_imports$date,padd2_imports[,grep("Canada", colnames(padd2_imports))])  #all annual columns
can_data<-can_data[,c(1,grep("Monthly", colnames(can_data)))]  #all monthly columns
can_data<-can_data[,-grep("Region", colnames(can_data))]  #all monthly columns
names(can_data)[1]<-"Date"

#annuals<-na.omit(annuals)
names(annuals)[1]<-paste("Date")  
name_list<- gsub("\\.", " ", names(can_data))
name_list<- gsub("\\Imports.of.", "", name_list)
name_list<- gsub("from Canada to refineries in padd2  West Coast   Monthly", "", name_list)
can_data <- setNames(can_data,name_list)


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
names(can_data)<-firstup(names(can_data))
source("tableau.R")
df1<-melt(can_data,id="Date",value.name = "Imports",variable.name = "Grade")
df1<-df1 %>% filter(!grepl("All",Grade))

hybrd.rplc_if    <- function(x) { mutate_if(x, is.numeric, funs(replace(., is.na(.), 0))) }
df1<-hybrd.rplc_if(df1)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("padd_5_imports.png")
ggplot(subset(df1,Grade!="All grades of crude oil"),aes(Date,Imports/days_in_month(Date),group = Grade,colour=Grade,fill=Grade)) +
  geom_area(position = "stack")+
  scale_fill_manual(name="",values=colors_tableau10())+
  scale_colour_manual(name="",values=colors_tableau10())+
  guides(colour = guide_legend(show = FALSE),fill = guide_legend(nrow = (2),byrow=F)) +
  scale_y_continuous(limits=c(0,300),expand=c(0,0))+
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
  )+    labs(y="Imports (thousands of barrels per day)",x="Date",
             title="Imports from Canada to US West Coast Refineries",
             caption="Source: EIA Data\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#PADD 2


subs<-data_fetch(KEY,cat=1293021)

testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
padd2_imports<-data.frame(date=index(test), coredata(test))
padd2_imports$date<-as.Date(padd2_imports$date,format = "%m/%d/%Y")

can_data<-cbind(padd2_imports$date,padd2_imports[,grep("Canada", colnames(padd2_imports))])  #all annual columns
can_data<-can_data[,c(1,grep("Monthly", colnames(can_data)))]  #all monthly columns
can_data<-can_data[,-grep("Region", colnames(can_data))]  #all monthly columns
names(can_data)[1]<-"Date"

name_list<- gsub("\\.", " ", names(can_data))
name_list<- gsub("\\Imports.of.", "", name_list)
name_list<- gsub(" from Canada to refineries in PADD2  Midwest   Monthly", "", name_list)
can_data <- setNames(can_data,name_list)


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
names(can_data)<-firstup(names(can_data))
df1<-melt(can_data,id="Date",value.name = "Imports",variable.name = "Grade")
df1<-df1 %>% filter(!grepl("All",Grade))

hybrd.rplc_if    <- function(x) { mutate_if(x, is.numeric, funs(replace(., is.na(.), 0))) }
df1<-hybrd.rplc_if(df1)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("padd_2_imports.png")
ggplot(subset(df1,Grade!="All grades of crude oil"),aes(Date,Imports/days_in_month(Date),group = Grade,colour=Grade,fill=Grade)) +
    geom_area(position = "stack")+
  scale_fill_manual(name="",values=colors_tableau10())+
  scale_colour_manual(name="",values=colors_tableau10())+
  guides(colour = guide_legend(show = FALSE),fill = guide_legend(nrow = 2,byrow=F)) +
  scale_y_continuous(limits=c(0,3000),expand=c(0,0))+
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
  )+    labs(y="Imports (thousands of barrels per day)",x="Date",
             title="Imports from Canada to US Midwest Refineries",
             caption="Source: EIA Data\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#PADD 3


subs<-data_fetch(KEY,cat=1293022)

testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
padd3_imports<-data.frame(date=index(test), coredata(test))
padd3_imports$date<-as.Date(padd3_imports$date,format = "%m/%d/%Y")

can_data<-cbind(padd3_imports$date,padd3_imports[,grep("Canada", colnames(padd3_imports))])  #all annual columns
can_data<-can_data[,c(1,grep("Monthly", colnames(can_data)))]  #all monthly columns
can_data<-can_data[,-grep("Region", colnames(can_data))]  #all monthly columns
names(can_data)[1]<-"Date"

name_list<- gsub("\\.", " ", names(can_data))
name_list<- gsub("\\Imports.of.", "", name_list)
name_list<- gsub(" from Canada to refineries in PADD3  Gulf Coast   Monthly", "", name_list)
can_data <- setNames(can_data,name_list)


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
names(can_data)<-firstup(names(can_data))
df1<-melt(can_data,id="Date",value.name = "Imports",variable.name = "Grade")
df1<-df1 %>% filter(!grepl("All",Grade))

hybrd.rplc_if    <- function(x) { mutate_if(x, is.numeric, funs(replace(., is.na(.), 0))) }
df1<-hybrd.rplc_if(df1)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("padd_3_imports.png")
ggplot(subset(df1,Grade!="All grades of crude oil"),aes(Date,Imports/days_in_month(Date),group = Grade,colour=Grade,fill=Grade)) +
  geom_area(position = "stack")+
  scale_fill_manual(name="",values=colors_tableau10())+
  scale_colour_manual(name="",values=colors_tableau10())+
  guides(colour = guide_legend(show = FALSE),fill = guide_legend(nrow = 2,byrow=F)) +
  scale_y_continuous(limits=c(0,600),expand=c(0,0))+
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
  )+    labs(y="Imports (thousands of barrels per day)",x="Date",
             title="Imports from Canada to US Gulf Coast Refineries",
             caption="Source: EIA Data\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#PADD 4


subs<-data_fetch(KEY,cat=1293023)

testing<-t(subs$Series_IDs$series_id)
testing<- as.character(testing)
test<- pdfetch_EIA(testing,KEY)

test <- setNames(test, t(subs$Series_IDs$name))
padd4_imports<-data.frame(date=index(test), coredata(test))
padd4_imports$date<-as.Date(padd4_imports$date,format = "%m/%d/%Y")

can_data<-cbind(padd4_imports$date,padd4_imports[,grep("Canada", colnames(padd4_imports))])  #all annual columns
can_data<-can_data[,c(1,grep("Monthly", colnames(can_data)))]  #all monthly columns
can_data<-can_data[,-grep("Region", colnames(can_data))]  #all monthly columns
names(can_data)[1]<-"Date"

name_list<- gsub("\\.", " ", names(can_data))
name_list<- gsub("\\Imports.of.", "", name_list)
name_list<- gsub(" from Canada to refineries in PADD4  Rocky Mountain   Monthly", "", name_list)
can_data <- setNames(can_data,name_list)


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
names(can_data)<-firstup(names(can_data))
df1<-melt(can_data,id="Date",value.name = "Imports",variable.name = "Grade")
df1<-df1 %>% filter(!grepl("All",Grade))

hybrd.rplc_if    <- function(x) { mutate_if(x, is.numeric, funs(replace(., is.na(.), 0))) }
df1<-hybrd.rplc_if(df1)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("padd_4_imports.png")
ggplot(subset(df1,Grade!="All grades of crude oil"),aes(Date,Imports/days_in_month(Date),group = Grade,colour=Grade,fill=Grade)) +
  geom_area(position = "stack")+
  scale_fill_manual(name="",values=colors_tableau10())+
  scale_colour_manual(name="",values=colors_tableau10())+
  guides(colour = guide_legend(show = FALSE),fill = guide_legend(nrow = 2,byrow=F)) +
  scale_y_continuous(limits=c(0,400),expand=c(0,0))+
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
  )+    labs(y="Imports (thousands of barrels per day)",x="Date",
             title="Imports from Canada to US Rocky Mountains (PADD IV) Refineries",
             caption="Source: EIA Data\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


#Products Exports

# devtools::install_github("hadley/readxl") # development version
# install.packages("readxl") # CRAN version
library(readxl)

#https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R50_EPP0_EEX_MBBL_M.xls

download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R10_EPP0_EEX_MBBL_M.xls", destfile="EIA_PADD1_product_exp.xls",,mode = "wb")
eia_padd_1 <- read_excel("EIA_PADD1_product_exp.xls",sheet = "Data 1",skip=2)

download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R20_EPP0_EEX_MBBL_M.xls", destfile="EIA_PADD2_product_exp.xls",,mode = "wb")
eia_padd_2 <- read_excel("EIA_PADD2_product_exp.xls",sheet = "Data 1",skip=2)

download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R30_EPP0_EEX_MBBL_M.xls", destfile="EIA_PADD3_product_exp.xls",mode = "wb")
eia_padd_3 <- read_excel("EIA_PADD3_product_exp.xls",sheet = "Data 1",skip=2)

download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R40_EPP0_EEX_MBBL_M.xls", destfile="EIA_PADD4_product_exp.xls",mode = "wb")
eia_padd_4 <- read_excel("EIA_PADD4_product_exp.xls",sheet = "Data 1",skip=2)

download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R50_EPP0_EEX_MBBL_M.xls", destfile="EIA_PADD5_product_exp.xls",mode = "wb")
eia_padd_5 <- read_excel("EIA_PADD5_product_exp.xls",sheet = "Data 1",skip=2)

eia_prod_exports<-merge(eia_padd_1,eia_padd_2,by="Date")
eia_prod_exports<-merge(eia_prod_exports,eia_padd_3,by="Date")
eia_prod_exports<-merge(eia_prod_exports,eia_padd_4,by="Date")
eia_prod_exports<-merge(eia_prod_exports,eia_padd_5,by="Date")



#EIA Gasoline
#https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R30_EPM0F_EEX_MBBL_M.xls

download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R10_EPM0F_EEX_MBBL_M.xls", destfile="EIA_PADD1_gasoline_exp.xls",mode = "wb")
download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R20_EPM0F_EEX_MBBL_M.xls", destfile="EIA_PADD2_gasoline_exp.xls",mode = "wb")
download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R30_EPM0F_EEX_MBBL_M.xls", destfile="EIA_PADD3_gasoline_exp.xls",mode = "wb")
download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R40_EPM0F_EEX_MBBL_M.xls", destfile="EIA_PADD4_gasoline_exp.xls",mode = "wb")
download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R50_EPM0F_EEX_MBBL_M.xls", destfile="EIA_PADD5_gasoline_exp.xls",mode = "wb")


eia_padd_1 <- read_excel("EIA_PADD1_gasoline_exp.xls",sheet = "Data 1",skip=2)
eia_padd_2 <- read_excel("EIA_PADD2_gasoline_exp.xls",sheet = "Data 1",skip=2)
eia_padd_3 <- read_excel("EIA_PADD3_gasoline_exp.xls",sheet = "Data 1",skip=2)
eia_padd_4 <- read_excel("EIA_PADD4_gasoline_exp.xls",sheet = "Data 1",skip=2)
eia_padd_5 <- read_excel("EIA_PADD5_gasoline_exp.xls",sheet = "Data 1",skip=2)

eia_gasoline_exports<-merge(eia_padd_1,eia_padd_2,by="Date")
eia_gasoline_exports<-merge(eia_gasoline_exports,eia_padd_3,by="Date")
eia_gasoline_exports<-merge(eia_gasoline_exports,eia_padd_4,by="Date")
eia_gasoline_exports<-merge(eia_gasoline_exports,eia_padd_5,by="Date")


eia_gasoline_exports<-melt(eia_gasoline_exports,id="Date",value.name = "Exports",variable.name = "PADD")
eia_gasoline_exports$Commodity<-"Gasoline"

eia_gasoline<-arrange(rbind(process_trade(eia_padd_1,"East Coast (PADD 1)"),
                          process_trade(eia_padd_2,"Midwest (PADD 2)"),
                          process_trade(eia_padd_3,"Gulf Coast (PADD 3)"),
                          process_trade(eia_padd_4,"Rocky Mountains (PADD 4)"),
                          process_trade(eia_padd_2,"West Coast (PADD 5)")
),Date)

df1<-eia_diesel %>% group_by(Date,PADD) %>% summarize(Exports=sum(Exports))
ggplot(subset(df1,year(Date)>2011),aes(Date,Exports/days_in_month(Date),group = PADD,colour=PADD,fill=PADD)) +
  geom_area(position = "stack")+
  #geom_line(data=subset(df1,year(Date)>2011 & PADD== "U.S. Exports to Canada of Crude Oil (Thousand Barrels)"),aes(Date,Exports/days_in_month(Date),group = PADD,colour=PADD))
  scale_fill_manual(name="",values=colors_tableau10())+
  scale_colour_manual(name="",values=colors_tableau10())+
  guides(colour = guide_legend(show = FALSE),fill = guide_legend(nrow = 2,byrow=F)) +
  #scale_y_continuous(limits=c(0,400),expand=c(0,0))+
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
  )+    labs(y="Exports (thousands of barrels per day)",x="Date",
             title="Gasoline Exports by Originating US PADD",
             caption="Source: EIA Data\nGraph by Andrew Leach")





#EIA Diesel
#https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R30_EPD0_EEX_MBBL_M.xls

name_list<-levels(file_melt$Dest)
name_list<- gsub("Exports to", "#", name_list)
name_list<- gsub("of", "#", name_list)
name_list<- gsub("\\(Thousand Barrels)", "", name_list)
test<-data.frame(do.call(rbind, strsplit(name_list, ' # ')))
file_melt$Dest<-test$X2
file_melt$Fuel<-unique(test$X3)
file_melt$PADD<-unique(test$X1)
file_melt<-file_melt[!is.na(file_melt$Exports),]

download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R10_EPD0_EEX_MBBL_M.xls", destfile="EIA_PADD1_diesel_exp.xls",mode = "wb")
download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R20_EPD0_EEX_MBBL_M.xls", destfile="EIA_PADD2_diesel_exp.xls",mode = "wb")
download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R30_EPD0_EEX_MBBL_M.xls", destfile="EIA_PADD3_diesel_exp.xls",mode = "wb")
download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R40_EPD0_EEX_MBBL_M.xls", destfile="EIA_PADD4_diesel_exp.xls",mode = "wb")
download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R50_EPD0_EEX_MBBL_M.xls", destfile="EIA_PADD5_diesel_exp.xls",mode = "wb")

eia_padd_1 <- read_excel("EIA_PADD1_diesel_exp.xls",sheet = "Data 1",skip=2)
eia_padd_2 <- read_excel("EIA_PADD2_diesel_exp.xls",sheet = "Data 1",skip=2)
eia_padd_3 <- read_excel("EIA_PADD3_diesel_exp.xls",sheet = "Data 1",skip=2)
eia_padd_4 <- read_excel("EIA_PADD4_diesel_exp.xls",sheet = "Data 1",skip=2)
eia_padd_5 <- read_excel("EIA_PADD5_diesel_exp.xls",sheet = "Data 1",skip=2)

eia_diesel_exports<-merge(eia_padd_1,eia_padd_2,by=c("Date"))
eia_diesel_exports<-merge(eia_diesel_exports,eia_padd_3,by=c("Date"))
eia_diesel_exports<-merge(eia_diesel_exports,eia_padd_4,by=c("Date"))
eia_diesel_exports<-merge(eia_diesel_exports,eia_padd_5,by=c("Date"))


eia_diesel_exports<-melt(eia_diesel_exports,id=c("Date"),value.name = "Exports",variable.name = "PADD")
eia_diesel_exports<-eia_diesel_exports[!is.na(eia_diesel_exports$Exports),]
name_list<-as.character(eia_diesel_exports$PADD)

name_list<- gsub("Federated States of Micronesia", "Micronesia", name_list)

name_list<- gsub("Exports to", "#", name_list)
name_list<- gsub("Exports of", "# Total #", name_list)
name_list<- gsub("of", "#", name_list)
name_list<- gsub("\\(Thousand Barrels)", "", name_list)
#name_list<-as.data.frame(name_list)

test<-strsplit(name_list,split= "#")



eia_diesel_test<-cbind(eia_diesel_exports,str_split_fixed(name_list, " # ", 3))
names(eia_diesel_test)<-c("Date","PADD_old","Exports","PADD","Destination","Commodity")
eia_diesel_test$PADD<-factor(eia_diesel_test$PADD, levels= c("East Coast (PADD 1)", "Midwest (PADD 2)",  "Gulf Coast (PADD 3)",   
 "Rocky Mountain (PADD 4)","West Coast (PADD 5)") )

df1<-eia_diesel_test %>% filter(Destination!="Total") %>% group_by(Date,PADD) %>% summarize(Exports=sum(Exports))
ggplot(subset(df1,year(Date)>2011),aes(Date,Exports/days_in_month(Date),group = PADD,colour=PADD,fill=PADD)) +
  geom_area(position = "stack")+
  #geom_line(data=subset(df1,year(Date)>2011 & PADD== "U.S. Exports to Canada of Crude Oil (Thousand Barrels)"),aes(Date,Exports/days_in_month(Date),group = PADD,colour=PADD))
  scale_fill_manual(name="",values=colors_tableau10())+
  scale_colour_manual(name="",values=colors_tableau10())+
  guides(colour = guide_legend(show = FALSE),fill = guide_legend(nrow = 2,byrow=F)) +
  #scale_y_continuous(limits=c(0,400),expand=c(0,0))+
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
  )+    labs(y="Exports (thousands of barrels per day)",x="Date",
             title="Distallate Fuel Oil Exports by Originating US PADD",
             caption="Source: EIA Data\nGraph by Andrew Leach")




#EIA Crude
#https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R30_EPC0_EEX_MBBL_M.xls

download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R10_EPC0_EEX_MBBL_M.xls", destfile="EIA_PADD1_crude_exp.xls",mode = "wb")
download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R20_EPC0_EEX_MBBL_M.xls", destfile="EIA_PADD2_crude_exp.xls",mode = "wb")
download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R30_EPC0_EEX_MBBL_M.xls", destfile="EIA_PADD3_crude_exp.xls",mode = "wb")
download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R40_EPC0_EEX_MBBL_M.xls", destfile="EIA_PADD4_crude_exp.xls",mode = "wb")
download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPCP_A2_R50_EPC0_EEX_MBBL_M.xls", destfile="EIA_PADD5_crude_exp.xls",mode = "wb")





eia_padd_1 <- read_excel("EIA_PADD1_crude_exp.xls",sheet = "Data 1",skip=2)
eia_padd_2 <- read_excel("EIA_PADD2_crude_exp.xls",sheet = "Data 1",skip=2)
eia_padd_3 <- read_excel("EIA_PADD3_crude_exp.xls",sheet = "Data 1",skip=2)
eia_padd_4 <- read_excel("EIA_PADD4_crude_exp.xls",sheet = "Data 1",skip=2)
eia_padd_5 <- read_excel("EIA_PADD5_crude_exp.xls",sheet = "Data 1",skip=2)

eia_crude_exports<-merge(eia_padd_1,eia_padd_2,by="Date",all=T)
eia_crude_exports<-merge(eia_crude_exports,eia_padd_3,by="Date",all=T)
eia_crude_exports<-merge(eia_crude_exports,eia_padd_4,by="Date",all=T)
eia_crude_exports<-merge(eia_crude_exports,eia_padd_5,by="Date",all=T)

process_data<-function(file_sent){
  #  file_sent<-eia_crude_exports
  df1<-melt(file_sent,id="Date",value.name = "Exports",variable.name = "Code")
  df1$Code<- gsub("Exports to", "#", df1$Code)
  df1$Code<- gsub("Exports of", "# Total #", df1$Code)
  df1$Code<-gsub("Federated States of Micronesia", "Micronesia", df1$Code)
  df1$Code<- gsub("of", "#", df1$Code)
  df1$Code<- gsub(" \\(Thousand Barrels)", "", df1$Code)
  
  #df1<-df1 %>% mutate(str_split_fixed(Code, " # ", n = 3))
  test<-data.frame(str_split_fixed(df1$Code, " # ", n = 3))
  df1<-cbind(df1,test)
  names(df1)[4:6]<-c("From","Dest","Commodity")
  df1$From<-factor(df1$From, levels= c("East Coast (PADD 1)", "Midwest (PADD 2)",  "Gulf Coast (PADD 3)",   
                                                    "Rocky Mountain (PADD 4)","West Coast (PADD 5)") )
  return(df1)
}

exports_data<-rbind(process_data(eia_diesel_exports),process_data(eia_gasoline_exports),process_data(eia_crude_exports))

eia_crude_canada<-cbind(eia_crude_exports$Date,eia_crude_exports[,grep("Canada", colnames(eia_crude_exports))])  #all annual columns
names(eia_crude_canada)[1]<-"Date"
name_list<-names(eia_crude_canada)
name_list<- gsub(" Exports to Canada of Crude Oil ", "", name_list)
name_list<- gsub("\\(Thousand Barrels)", "", name_list)

name_list<- gsub(" Thousand Barrels", "", name_list)

eia_crude_canada <- setNames(eia_crude_canada,name_list)


download.file("https://www.eia.gov/dnav/pet/xls/PET_MOVE_EXPC_A_EPC0_EEX_MBBL_M.xls", destfile="EIA_total_crude.xls",,mode = "wb")
eia_total_crude <- read_excel("EIA_total_crude.xls",sheet = "Data 1",skip=2)
eia_total_crude_canada<-eia_total_crude[,c(1,8)]
names(eia_total_crude_canada)[1]<-"Date"

eia_crude_canada<-merge(eia_crude_canada,eia_total_crude_canada,by="Date")

eia_crude_canada$`East Coast (PADD 1)`[is.na(eia_crude_canada$`East Coast (PADD 1)`)]<-0
eia_crude_canada$`West Coast (PADD 5) Int`<-
  eia_crude_canada$`U.S.`-                   
  eia_crude_canada$`East Coast (PADD 1)`-   
  eia_crude_canada$`Midwest (PADD 2)`-
  eia_crude_canada$`Gulf Coast (PADD 3)`- 
  eia_crude_canada$`Rocky Mountain (PADD 4)`


df1<-melt(eia_crude_canada,id="Date",value.name = "Exports",variable.name = "PADD")
df1<-na.omit(df1)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("crude_to_canada_padd.png")
ggplot(subset(df1,year(Date)>2011 & PADD!= "U.S. Exports to Canada of Crude Oil (Thousand Barrels)" & PADD!=
              "West Coast (PADD 5)"),aes(Date,Exports/days_in_month(Date),group = PADD,colour=PADD,fill=PADD)) +
  #geom_area(position = "stack")+
  geom_area(position = "stack")+
  geom_line(data=subset(df1,year(Date)>2012 & PADD== "U.S. Exports to Canada of Crude Oil (Thousand Barrels)"))+
  #geom_line(data=subset(df1,year(Date)>2011 & PADD== "U.S. Exports to Canada of Crude Oil (Thousand Barrels)"),aes(Date,Exports/days_in_month(Date),group = PADD,colour=PADD))
  scale_fill_manual(name="",values=colors_tableau10())+
  scale_colour_manual(name="",values=colors_tableau10())+
  guides(colour = guide_legend(show = FALSE),fill = guide_legend(nrow = 2,byrow=F)) +
  scale_y_continuous(limits=c(0,400),expand=c(0,0))+
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
  )+    labs(y="Exports (thousands of barrels per day)",x="Date",
             title="Crude Oil Exports to Canada by Originating US PADD",
             caption="Source: EIA Data\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


df1<-exports_data %>% filter(Dest=="Canada" &year(Date)>2011) 

ggplot(subset(df1,year(Date)>2011),aes(Date,Exports/days_in_month(Date),group = From,fill=From)) +
  geom_area(position = "stack")+
  facet_wrap(~Commodity,ncol = 1, scales = "free")+
#  scale_color_viridis("",discrete=TRUE,option="E")+
  scale_fill_viridis("",discrete=TRUE,option="E")+ 
  guides(fill = guide_legend(nrow = (2)))+
  scale_x_datetime(name=NULL,date_breaks = "1 year",date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.title = element_text(size = 12),
        #axis.label.x = element_text(size=20,vjust=+5),
        plot.subtitle = element_text(size = 12,hjust=0.5),
        plot.caption = element_text(face="italic",size = 12,hjust=0),
        legend.key.width=unit(2,"line"),
        legend.key.height=unit(1,"line"),
        legend.position = "bottom",
        
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
        labs(y="Exports (thousands of barrels per day)",x="Date",
             title="Crude Oil Exports to Canada by Originating US PADD",
             caption="Source: EIA Data. Graph by Andrew Leach.")


df1<-exports_data %>% filter(Dest!="Total" & Commodity=="Crude Oil") 
keeps<-df1 %>% group_by(Dest) %>% summarise(exports=sum(Exports,na.rm = T)) %>% arrange(-exports) %>% head(5)
fct_other(df1$Dest,keep=keeps[,1])
df1$Dest<-fct_reorder(df1$Dest,df1$Exports, fun=mean)
levels(df1$Dest)


#refined products

#products data_series
#http://api.eia.gov/category/?api_key=YOUR_API_KEY_HERE&category_id=476336
ref_prod_series<-get_children(401931)
ref_prod_data<-EIA_to_DF(ref_prod_series)



#weekly_supply<-get_children(235678)
#weekly_ref_prod<-EIA_to_DF(weekly_ref_prod_series)

#weekly gasoline, diesel and ket data.
weekly_ref_prod_series<-filter(bind_rows(get_series(236376),get_series(236378),get_series(236380),get_series(236382),get_series(236384)),f=="W")
weekly_ref_prod<-EIA_to_DF(weekly_ref_prod_series)
names(weekly_ref_prod)<-gsub("U.S. ","",names(weekly_ref_prod))
names(weekly_ref_prod)<-gsub("Product Supplied of ","",names(weekly_ref_prod))
names(weekly_ref_prod)<-gsub(", Weekly","",names(weekly_ref_prod))


#series_names<-c("Product Supplied - Total Products",
#"Product Supplied - Finished Motor Gasoline",
#"Product Supplied - Kerosene-Type Jet Fuel",
##"Product Supplied - Distillate Fuel Oil",
#"Product Supplied - Residual Fuel Oil")
weekly_ref_prod<-weekly_ref_prod %>% melt(id=c("date"),value.name = "supply",variable.name="commodity") %>%
  na.omit()

set_png("weekly_supply.png")
ggplot(weekly_ref_prod)+
  geom_area(data=filter(weekly_ref_prod,commodity=="Petroleum Products"),aes(date,supply,group=commodity,fill="Other Products"))+
  geom_area(data=filter(weekly_ref_prod,commodity!="Petroleum Products"),aes(date,supply,group=commodity,fill=commodity),position="stack")+
  scale_fill_manual("",values=c(colors_ua10()))+
  scale_x_date(date_breaks = "2 year",labels = date_format("%b\n%Y"),limits = c(ymd("1992-01-01"),Sys.Date()),expand = c(0,0))+
  weekly_small()+
  labs(x=NULL,y="US Weekly Supply (1000 barrels per day)",
       title="Weekly Refined Product Supply")
dev.off()

       



weekly_sales_series<-filter(bind_rows(get_series(239227),get_series(405474)),f=="M",grepl("East Coast",name)|grepl("(PADD 2)",name)|grepl("PADD 3",name)|grepl("PADD 4",name)|grepl("PADD 5",name)|grepl("U.S.",name))
weekly_sales<-EIA_to_DF(weekly_sales_series)


names(weekly_sales)<-gsub(" All Sales/Deliveries by Prime Supplier, Monthly","",names(weekly_sales))
weekly_sales<-weekly_sales %>% melt(id=c("date"),value.name = "sales") %>%
  na.omit()
  
commodities<-c("U.S. Total Gasoline",
"U.S. Total Distillate plus Kerosene")
#test<-weekly_sales%>%mutate(test_col=commodities[grep(commodities,as.character(variable))])



  #%>%separate(variable, c("area", "commodity"),sep = " Total ") %>%

set_png("monthly_sales.png")
ggplot(weekly_sales)+
  geom_area(data=filter(weekly_sales,variable %in% commodities),aes(date,sales*1000/42/1000,group=variable,fill=variable))+
  scale_fill_manual("",values=c(colors_ua10()))+
  scale_x_date(date_breaks = "2 year",labels = date_format("%b\n%Y"),limits = c(ymd("1992-01-01"),Sys.Date()),expand = c(0,0))+
  weekly_small()  +
  labs(x=NULL,y="US Monthly Sales (thousand barrels per day)",
       title="Monthly Product Sales",
       subtitle=paste("EIA Prime Supplier Sales Volumes",sep = ""),
       caption=paste("Source: EIA data, graph by Andrew Leach.",sep=""))
dev.off()


commodities<-c("Midwest (PADD 2) Total Gasoline",
               "Midwest (PADD 2) Total Distillate plus Kerosene")
#test<-weekly_sales%>%mutate(test_col=commodities[grep(commodities,as.character(variable))])



#%>%separate(variable, c("area", "commodity"),sep = " Total ") %>%

set_png("monthly_sales_2.png")
ggplot(weekly_sales)+
  geom_area(data=filter(weekly_sales,variable %in% commodities),aes(date,sales*1000/42/1000,group=variable,fill=variable))+
  scale_fill_manual("",values=c(colors_ua10()))+
  scale_x_date(date_breaks = "2 year",labels = date_format("%b\n%Y"),limits = c(ymd("1992-01-01"),Sys.Date()),expand = c(0,0))+
  weekly_small()  +
  labs(x=NULL,y="PADD 2 Monthly Sales (thousand barrels per day)",
       title="Monthly Product Sales",
       subtitle=paste("EIA Prime Supplier Sales Volumes",sep = ""),
       caption=paste("Source: EIA data, graph by Andrew Leach.",sep=""))
dev.off()


commodities<-c("Gulf Coast (PADD 3) Total Gasoline",
               "Gulf Coast (PADD 3) Total Distillate plus Kerosene")
#test<-weekly_sales%>%mutate(test_col=commodities[grep(commodities,as.character(variable))])



#%>%separate(variable, c("area", "commodity"),sep = " Total ") %>%

set_png("monthly_sales_3.png")
ggplot(weekly_sales)+
  geom_area(data=filter(weekly_sales,variable %in% commodities),aes(date,sales*1000/42/1000,group=variable,fill=variable))+
  scale_fill_manual("",values=c(colors_ua10()))+
  scale_x_date(date_breaks = "2 year",labels = date_format("%b\n%Y"),limits = c(ymd("1992-01-01"),Sys.Date()),expand = c(0,0))+
  weekly_small()  +
  labs(x=NULL,y="PADD 2 Monthly Sales (thousand barrels per day)",
       title="Monthly Product Sales",
       subtitle=paste("EIA Prime Supplier Sales Volumes",sep = ""),
       caption=paste("Source: EIA data, graph by Andrew Leach.",sep=""))
dev.off()


