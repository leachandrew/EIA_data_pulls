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

data_fetch<-function(key, cat=2227122){
  key <- unlist(strsplit(key, ";"))
  ifelse(cat==999999999,
         url <- paste("https://api.eia.gov/category?api_key=",
                      key, "&out=xml", sep="" ),
         url <- paste("https://api.eia.gov/category?api_key=",
                      key, "&category_id=", cat, "&out=xml", sep="" )
  )
  doc <- xmlParse(file=url, isURL=TRUE)
  Parent_Category <- tryCatch(xmlToDataFrame(,stringsAsFactors = F,nodes =
                                               XML::getNodeSet(doc, "//category/parent_category_id")),
                              warning=function(w) FALSE, error=function(w) FALSE)
  Sub_Categories <- xmlToDataFrame(,stringsAsFactors = F,nodes =
                                     XML::getNodeSet(doc, "//childcategories/row"))
  Series_IDs <- xmlToDataFrame(nodes =
                                 XML::getNodeSet(doc, "///childseries/row"),stringsAsFactors = F)
  Categories <- list(Parent_Category, Sub_Categories, Series_IDs)
  names(Categories) <- c("Parent_Category", "Sub_Categories", "Series_IDs")
  return(Categories)
}


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
 
 pd_fix<-function(data,name){
   data<-data.frame(date=index(data), coredata(data))
   data$date<-ymd(data$date)
   data <- setNames(data, c("date",name)) 
 }
 
 EIA_to_DF<-function(series_info){
   data<- pdfetch_EIA(series_info$series_id,KEY)
   pd_fix(data,series_info$name)
   }
 
 
 



series<-paste("AEO.",seq(2015,2022),".REF",seq(2015,2022),".GEN_NA_ELEP_NA_SLR_PHTVL_NA_BLNKWH.A",sep="")
labels<-paste(seq(2014,2022)," AEO",sep="")

#2014 is different
series<-c("AEO.2014.REF2014.GEN_NA_ELEP_NA_SOPH_NA_NA_BLNKWH.A",series)
#names<-rev(labels)
solar_data<-pdfetch_EIA(series,KEY) 
solar_data<-data.frame(date=index(solar_data), coredata(solar_data))
solar_data <- setNames(solar_data, c("date",labels))
solar_data$date<-ymd(solar_data$date)
solar_melt <- pivot_longer(solar_data,-date,names_to = "variable")

png<-1
if(png==1)
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file="solar_eia.png", width = 1400, height = 750,res=130,type='cairo')
  if(R.version$platform ==  "x86_64-w64-mingw32")
    png(file="solar_eia.png", width = 1400, height = 750,res=130)

ggplot(solar_melt) +
  geom_line(aes(date,value,colour=variable),size=2) +
  scale_color_viridis("",labels = labels,discrete = TRUE)+
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
       

series<-paste("AEO.",seq(2014,2022),".REF",seq(2014,2022),".SUP_NA_LFL_NA_DCP_NA_USA_MILLBRLPDY.A",sep="")
labels<-paste(seq(2014,2022)," AEO",sep="")

oil_data<-pdfetch_EIA(series,KEY) 
oil_data<-data.frame(date=index(oil_data), coredata(oil_data))
oil_data <- setNames(oil_data, c("date",labels))
oil_data$date<-ymd(oil_data$date)
oil_melt <- pivot_longer(oil_data,-date,names_to = "variable")


png<-1
if(png==1)
  set_png(file="oil_production_eia.png")
ggplot(oil_melt) +
  geom_line(aes(date,value,colour=variable),size=2) +
  #scale_color_viridis("",labels = c("2019 AEO","2018 AEO","2017 AEO","2016 AEO","2015 AEO","2014 AEO"),discrete = TRUE)+
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

eia_aeo_comp<-function(start_year=2014,end_year=2022,api_series=".SUP_NA_LFL_NA_DCP_NA_USA_MILLBRLPDY.A",
                   label="US Total Crude Oil Production",
                   units="mmbbl/d",
                   history=FALSE,
                   hist_series="PET.MCRFPUS2.A",
                   hist_conversion=1,
                   hist_year=1950,
                   zero_y=TRUE
                   ){ #use oil as the default
  #testing
  #api_series<-".GEN_NA_ELEP_NA_SLR_PHTVL_NA_BLNKWH.A"
  #start_year=2015
  #end_year=2022
  #label="Oil Production"
  #units="mmbbl/d"
  #hist_series<-"PET.MCRFPUS2.A"
  #hist_conversion=1000
  series<-paste("AEO.",seq(start_year,end_year),".REF",seq(start_year,end_year),api_series,sep="")
  labels<-paste(seq(start_year,end_year)," AEO",sep="")
  elements=end_year-start_year+1
  data<-pd_fix(pdfetch_EIA(series,KEY),labels)%>%
    pivot_longer(-date,names_to = "variable")
  plot<-ggplot(data)+
  geom_line(aes(date,value,group=variable,color=variable,size=variable==paste(end_year,"AEO")),lty="31")+
  #geom_point(data=data %>% filter(variable==paste(end_year,"AEO")),aes(date,value,group=variable,color=variable),size=2.25)+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  scale_x_date(breaks=pretty_breaks(n=10),expand=c(0,0))+
  scale_color_viridis("",discrete = T,option="A",direction = -1,end = .9)+
  scale_size_manual("",values=c(1,1.5))+
  scale_linetype_manual("",values=c("solid"))+
  theme_minimal()+weekly_graphs()+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  guides(linetype=guide_legend(order = 1,keywidth = unit(1.6,"cm")),
         size="none",
          #shape = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         #linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         #colour = guide_legend(keywidth = unit(1.6,"cm"),override.aes = list(lty = "11")  ,nrow = 2),
         colour = guide_legend(keywidth = unit(1.6,"cm"),nrow = trunc(elements/6)+1,
                               override.aes = list(size=c(rep(1,elements-1),1.5))),
         #fill = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2)
         NULL)
  if(zero_y)
    plot<-plot+expand_limits(y=0)
  
  #get historical data
  if(history)
    {
    hist_data<-pd_fix(pdfetch_EIA(hist_series,KEY),"Historical data")%>%
      pivot_longer(-date,names_to = "variable")%>%
      mutate(value=value/hist_conversion)%>%
      filter(year(date)>=hist_year)
    plot<-plot+
    geom_line(data=hist_data,aes(date,value,lty="Historical Data"),size=1)+
    labs(y=paste(label," (",units,")",sep=""),x="",
         title=paste("Historical",label,"and EIA AEO Reference Case Projections"),
         caption="Source: Data via EIA AEO, graph by Andrew Leach.")
   }
  
  if(!history){
    plot<-plot+
    labs(y=paste(label," (",units,")",sep=""),x="",
         title=paste("EIA Annual Energy Outlook",label,"Forecasts"),
         caption="Source: Data via EIA AEO, graph by Andrew Leach.")
    }
plot
  
  }

#oil production
oil_prod<-eia_aeo_comp(history = TRUE,hist_conversion = 1000)
ggsave("oil_prod.png",oil_prod,width=14,height=7)

#oil imports and exports

#TRAD_NA_LFL_NA_NETIMP_NETIMP_USA_MILLBRLPDY.A
oil_imports<-eia_aeo_comp(start_year = 2014,api_series =".TRAD_NA_LFL_NA_GIM_NA_USA_MILLBRLPDY.A",label = "US Gross Crude Oil Imports",
                          units = "million barrels per day",
                          history = TRUE,
                          hist_series = "PET.MCRIMUS2.A",
                          hist_conversion = 1000,
                          hist_year=1950)
oil_imports
ggsave("oil_imports.png",oil_imports,width=14,height=7)

oil_exports<-eia_aeo_comp(start_year = 2014,api_series =".TRAD_NA_LFL_NA_EXP_NA_USA_MILLBRLPDY.A",label = "US Gross Crude Oil Exports",
                          units = "million barrels per day",
                          history = TRUE,
                          hist_series = "PET.MCREXUS2.A",
                          hist_conversion = 1000,
                          hist_year=2010)
oil_exports
ggsave("oil_exports.png",oil_exports,width=14,height=7)



#product imports and exports FIX!!

#TRAD_NA_LFL_NA_NETIMP_NETIMP_USA_MILLBRLPDY.A
oil_products_imports<-eia_aeo_comp(start_year = 2014,api_series =".TRAD_NA_LFL_NA_GIM_NA_USA_MILLBRLPDY.A",label = "US Gross Oil Products Imports",
                          units = "million barrels per day",
                          history = TRUE,
                          hist_series = "PET.MCRIMUS2.A",
                          hist_conversion = 1000,
                          hist_year=1950)
oil_products_imports
ggsave("oil_products_imports.png",oil_products_imports,width=14,height=7)

oil_products_exports<-eia_aeo_comp(start_year = 2014,api_series =".TRAD_NA_LFL_NA_EXP_NA_USA_MILLBRLPDY.A",label = "US Gross Oil Products Exports",
                          units = "million barrels per day",
                          history = TRUE,
                          hist_series = "PET.MCREXUS2.A",
                          hist_conversion = 1000,
                          hist_year=2010)
oil_products_exports
ggsave("oil_products_exports.png",oil_products_exports,width=14,height=7)



#gas trade


#TRAD_NA_LFL_NA_NETIMP_NETIMP_USA_MILLBRLPDY.A
gas_imports<-eia_aeo_comp(start_year = 2020,api_series =".SUP_IMP_NA_NA_NG_NA_NA_TRLCF.A",label = "US Gross Natural Gas Imports",
                          units = "TCF",
                          history = TRUE,
                          hist_series = "NG.N9100US2.M",
                          hist_conversion = 10^6/12,
                          hist_year=1950)
gas_imports
ggsave("gas_imports.png",gas_imports,width=14,height=7)

gas_exports<-eia_aeo_comp(start_year = 2014,api_series =".SUP_EXPT_NA_NA_NG_NA_NA_TRLCF.A",label = "US Gross Natural Gas Exports",
                          units = "TCF",
                          history = TRUE,
                          hist_series = "NG.N9130US2.M",
                          hist_conversion = 10^6/12,
                          hist_year=2010)
gas_exports
ggsave("gas_exports.png",gas_exports,width=14,height=7)



#hh spot price

gas_price<-eia_aeo_comp(start_year = 2014,api_series =".PRCE_HHP_NA_NA_NG_NA_USA_NDLRPMBTU.A",label = "Henry Hub Nominal Spot Price",
                          units = "$/MMBTU",
                          history = TRUE,
                          hist_series = "NG.RNGWHHD.M",
                          hist_conversion = 1,
                          hist_year=1990)
gas_price
ggsave("hh_gas_price.png",gas_price,width=14,height=7)


#wti spot price

wti_price<-eia_aeo_comp(start_year = 2015,api_series =".PRCE_NA_NA_NA_CR_WTI_USA_NDLRPBRL.A",label = "WTI (Cushing) Nominal Spot Price",
                        units = "$/bbl",
                        history = TRUE,
                        hist_series = "PET.RWTC.M",
                        hist_conversion = 1,
                        hist_year=1990)
wti_price
ggsave("wti_oil_price.png",wti_price,width=14,height=7)


#solar and coal generation

solar_graph<-eia_aeo_comp(start_year = 2014,api_series =".GEN_NA_ALLS_NA_SLR_NA_NA_BLNKWH.A",label = "Solar Electricity Generation",
                          units = "billion kWh",
                          history = TRUE,
                          hist_series = "ELEC.GEN.TSN-US-99.A"
                          )
solar_graph
ggsave("solar_graph.png",solar_graph,width=14,height=7)


coal_graph<-eia_aeo_comp(start_year = 2018,api_series =".GEN_NA_ELEP_POW_CL_NA_USA_BLNKWH.A",label = "Coal-fired Electricity Generation",
                          units = "billion kWh",
                         history = TRUE,
                         hist_conversion = 1000,
                         hist_series = "ELEC.GEN.COW-US-98.A")
ggsave("coal_graph.png",coal_graph,width=14,height=7)




#Emissions

ghg_graph<-eia_aeo_comp(start_year = 2015,api_series =".EMI_CO2_TEN_NA_NA_NA_NA_MILLMETNCO2.A",label = "Energy-related CO2 Emissions",
                         units = "Mt",
                         history = TRUE,
                         hist_series = "EMISS.CO2-TOTV-TT-TO-US.A",
                        zero_y = TRUE)
ghg_graph
ggsave("ghg_graph.png",ghg_graph,width=14,height=7)
