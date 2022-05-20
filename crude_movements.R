
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
library(lemon)

data_fetch<-function(key, cat){
  #key<-KEY
  #cat=476336
  ifelse(cat==999999999,
         url <- paste("https://api.eia.gov/category/?api_key=",
                      key, "&out=xml", sep="" ),
         url <- paste("https://api.eia.gov/category/?api_key=",
                      key, "&category_id=", cat, "&out=xml", sep="" )
  )
  
  #http://api.eia.gov/category/?api_key=YOUR_API_KEY_HERE&category_id=476336
  #url <- paste("https://api.eia.gov/category?api_key=",
  #             key, "&category_id=", cat, "&out=xml", sep="" )
  #https://api.eia.gov/category/?api_key=91b4dca0b858df64a2279d82f71af240&category_id=476336&out=xml
  #https://api.eia.gov/category?api_key=91b4dca0b858df64a2279d82f71af240&category_id=476336&out=xml
  
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


imports_proc_padd<-data.frame(do.call(rbind,data_store))



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


#all_data<-data.frame(all_data)
crude_data$date=ymd(rownames(crude_data))


movements<-crude_data %>% pivot_longer(-date,names_to="series_id",values_to = "quantity") #convert data to long form with series ids
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
  scale_fill_manual("",values = colors_tableau10()[-7],guide = "legend")+
  scale_colour_manual("",values=colors_tableau10()[-7],guide = "legend")+
  guides(fill=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 10),
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


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#grab the legend without a title
mylegend<-arrangeGrob(g_legend(moves_out + theme(legend.title = element_blank(),
                                                legend.spacing = unit(1.0, 'cm'),
                                                legend.text =  element_text(margin = margin(t = 0, r = 1, b = 0, l = 0)))))



png<-1
if(png==1)
  set_png(file_sent=paste("movements.png",sep=""),h_sent = 1200,w_sent=2000)
grid.arrange(arrangeGrob(moves_in +labs(caption="Note: Production denoted as supply from within the PADD, e.g. PADD 2 production appears as PADD 2 supply from PADD 2.")+
                           theme(legend.position="none",
                                           legend.margin=margin(c(0,0,0,0),unit="cm"),
                                           legend.text = element_text(colour="black", size = 14, face = "bold"),
                                            plot.caption = element_text(size = 10,face = "plain", colour="black"),          
                                            plot.title = element_text(size = 12,face = "bold", colour="black"),
                                          plot.subtitle = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           text = element_text(size = 10,face = "bold"),
                                           axis.text = element_text(size =12,face = "bold", colour="black"),
                                           axis.text.x = element_blank(),
                                          axis.title.x = element_blank(),
                                          strip.text = element_text(colour="black", size = 8, face = "bold"),
                                          
),
moves_out+ labs(caption = "Source: Data via EIA, graph by Andrew Leach")+
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
nrow=2,heights=c(10, 1),
bottom =text_grob(
  "Source: Data via EIA, graph by Andrew Leach",
  face = "italic", color = "black",size=10,hjust=0.5,vjust=0.5,lineheight = 2
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


#get canadian imports by PADD

library(stringr)

get_padd_imports<-function(commodity="Crude Oil",freq="M",unit_sent="Thousand Barrels per Day",from="Canada")
{
#function to grab and graph Canadian imports as a share of total imports by PADD
  products<-data_fetch(KEY,cat=376801)
  sub_cats<-products$Sub_Categories
  #find commodity
  sub_cats<-sub_cats %>% filter(name%in%c(commodity,str_to_lower(commodity),str_to_title(commodity)))
  imports<-data_fetch(KEY,cat=sub_cats$category_id)
  areas<-data_fetch(KEY,cat=imports$Sub_Categories$category_id)
  areas<-areas$Sub_Categories
  #build list from sub_cats
  cat_store <- list()
  cat_count<-1
  for (cat in areas$category_id) {
    #cat<-sub_cats$category_id[1]
    series<-data_fetch(KEY,cat=cat)
    cat_store[[cat_count]]<-series$Series_IDs
    cat_count<-cat_count+1
  }
  series<-tibble(do.call(rbind,cat_store))
  #filter for totals and Canada in monthly data
  commodity=case_when(
    str_to_lower(commodity)==str_to_lower("Total Crude Oil and Products")~"Crude Oil and Petroleum Products",
    str_to_lower(commodity)==str_to_lower("Products")~"Total Petroleum Products",
    TRUE~commodity
  )
  
  series<-series %>% filter(str_to_lower(f)==str_to_lower(freq))%>%
    filter(str_to_lower(units)==str_to_lower(unit_sent))%>%
    #find the totals
    filter(grepl(str_to_lower(paste("Imports by PADD of Processing of",commodity)),str_to_lower(name))
           |
          grepl(str_to_lower(paste("from",from)),str_to_lower(name)))

#get the data
  import_data<-EIA_to_DF(series)%>%
    pivot_longer(-date,values_to = "imports")%>%
    mutate(date=as_date(date),imports=na.fill(imports,fill = 0),
           name=gsub("Imports by PADD of Processing ","",name)
           )
  
  import_data<-import_data  %>%
  separate(name,c("name","freq"),sep=", ")%>%
  separate(name,c("name","commodity"),sep=" of ")%>%
  separate(name,c("name","from"),sep=" from ")%>%
    replace_na(list(from = "Total"))

#fix the factors
  import_data<-import_data%>%
    mutate(PADD=as_factor(name),
           PADD=fct_relevel(PADD,"Midwest (PADD 2)",after = 1),
           from=factor(from),
           from=fct_recode(from,c("Total Imports"="Total")),
           from=fct_recode(from,c("Imports from Russia"="Russia")),
           from=fct_recode(from,c("Imports from Canada"="Canada")),
           from=fct_relevel(from,"Total Imports"))
    
import_data  
}
  

cad_imports<-get_padd_imports()

set_png("crude_by_proc_padd.png")
ggplot(cad_imports%>%filter(year(date)>=1995),aes(date,imports,group = from,fill=from)) +
  geom_area(position = "identity",color=colors_ua10()[1],size=.05)+
  facet_wrap(~PADD,nrow = 1)+
  scale_fill_manual(name="",values=colors_ua10())+
  scale_colour_manual(name="",values=colors_ua10())+
  expand_limits(x=ymd("1994-12-31"))+
  guides(colour = guide_legend(show = FALSE),fill = guide_legend(nrow = (1),byrow=F)) +
  scale_x_date(breaks=pretty_breaks(),date_labels = "%b\n%Y",expand=c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  weekly_small()+    labs(y="US Processing of Canadian Crude (1000 bbl/d)",x="Date",
                          title="Crude Imports from Canada by US Processing Area",
                          caption="Source: EIA Data\nGraph by Andrew Leach")
  


rus_imports<-get_padd_imports(from = "Russia")%>%
  mutate(
    from=fct_recode(from,c("Imports from Russia"="Russia")),
    from=fct_relevel(from,"Total Imports"))

set_png("rus_crude_by_proc_padd.png")
ggplot(rus_imports%>%filter(year(date)>=2005),aes(date,imports,group = from,fill=from)) +
  geom_area(position = "identity",color=colors_ua10()[1],size=.05)+
  facet_wrap(~PADD,nrow = 1)+
  scale_fill_manual(name="",values=colors_ua10())+
  scale_colour_manual(name="",values=colors_ua10())+
  #expand_limits(x=ymd("1994-12-31"))+
  guides(colour = guide_legend(show = FALSE),fill = guide_legend(nrow = (1),byrow=F)) +
  scale_x_date(breaks=pretty_breaks(),date_labels = "%b\n%Y",expand=c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  weekly_small()+    labs(y="US Processing of Russian Crude (1000 bbl/d)",x="Date",
                          title="Crude Imports from Russia by US Processing Area",
                          caption="Source: EIA Data\nGraph by Andrew Leach")

dev.off()



rus_prods<-get_padd_imports(from = "Russia",commodity = "Products")%>%
  mutate(
    from=fct_recode(from,c("Imports from Russia"="Russia")),
    from=fct_relevel(from,"Total Imports"))

set_png("rus_prods_by_proc_padd.png")
ggplot(rus_prods%>%filter(year(date)>=2005),aes(date,imports,group = from,fill=from)) +
  geom_area(position = "identity",color=colors_ua10()[1],size=.05)+
  facet_wrap(~PADD,nrow = 1)+
  scale_fill_manual(name="",values=colors_ua10())+
  scale_colour_manual(name="",values=colors_ua10())+
  #expand_limits(x=ymd("1994-12-31"))+
  guides(colour = guide_legend(show = FALSE),fill = guide_legend(nrow = (1),byrow=F)) +
  scale_x_date(breaks=pretty_breaks(),date_labels = "%b\n%Y",expand=c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  weekly_small()+    labs(y="US Imports of Petroleum Products (1000 bbl/d)",x="Date",
                          title="Petroleum Product Imports from Russia by US Processing Area",
                          caption="Source: EIA Data\nGraph by Andrew Leach")

dev.off()



cad_prods<-get_padd_imports(from = "Canada",commodity = "Products")
  

set_png("cad_prods_by_proc_padd.png")
ggplot(cad_prods%>%filter(year(date)>=2005),aes(date,imports,group = from,fill=from)) +
  geom_area(position = "identity",color=colors_ua10()[1],size=.05)+
  facet_wrap(~PADD,nrow = 1)+
  scale_fill_manual(name="",values=colors_ua10())+
  scale_colour_manual(name="",values=colors_ua10())+
  #expand_limits(x=ymd("2004-12-31"))+
  guides(colour = guide_legend(show = FALSE),fill = guide_legend(nrow = (1),byrow=F)) +
  scale_x_date(breaks=pretty_breaks(),date_labels = "%b\n%Y",expand=c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  weekly_small()+    labs(y="US Imports of Petroleum Products (1000 bbl/d)",x="Date",
                          title="Petroleum Product Imports from Canada by US Processing Area",
                          caption="Source: EIA Data\nGraph by Andrew Leach")

dev.off()




  
  annotate("rect", fill = "black", alpha = 0.8, 
           xmin = as.Date("2014-08-01"), xmax =as.Date("2014-08-10"),
           ymin = 0, ymax = 4200) +
  annotate("text", x = as.Date("2013-01-01"), y = 3500, label = "Enbridge and\n TransCanada\nGulf Coast access\n pipelines begin to\ncome into service",size=2.4)+
  
  
  weekly_small()+    labs(y="US Processing of Canadian Crude (1000 bbl/d)",x="Date",
                          title="Crude Imports from Canada by US Processing Area",
                          caption="Source: EIA Data\nGraph by Andrew Leach")
dev.off()



#add crude exports to Canada imports graph

exports<-movements %>% filter(date>=ymd("2000-01-01") & Commodity=="Crude Oil",Mode=="Export")%>%
  clean_names()%>%
  mutate(to=factor(to),PADD=factor(from)) %>% mutate(PADD=fct_relevel(PADD,"Midwest (PADD 2)",after = 1))%>%
  select(date,PADD,from,imports=quantity)%>%
  mutate(imports=-imports,from="Total Exports")

imports<-cad_imports %>% select(date,PADD,from,imports)%>% bind_rows(exports)
                                                   



