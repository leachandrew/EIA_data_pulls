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
mylegend<-arrangeGrob(g_legend(moves_in +  guides(fill=guide_legend(nrow =1,byrow=FALSE))+
                                 theme(legend.title = element_blank(),
                                                legend.spacing = unit(1.0, 'cm'),
                                                legend.text =  element_text(margin = margin(t = 0, r = 1, b = 0, l = 0)))),nrow = 1)



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


#refined product movements

#refinery production by PADD

#http://api.eia.gov/category/?api_key=YOUR_API_KEY_HERE&category_id=296686


subs<-data_fetch(KEY,cat=302279)
#sub_cats<-subs$Sub_Categories

series <-subs$Series_IDs %>% filter(grepl("PADD",name),grepl("Monthly",name),units=="Thousand Barrels per Day")
series$name<-as.character(series$name)
series$To<-do.call("rbind",strsplit(series$name, " Refinery Net Production of Crude Oil and Petroleum Products"))[,1]
series$Mode<-"Refinery Net Production"
Rest<-do.call("rbind",strsplit(series$name, " Refinery Net Production of Crude Oil and Petroleum Products"))[,2]
series$From<-series$To
series$Commodity<-do.call("rbind",strsplit(Rest, ", Monthly"))[,1]
series$Commodity<-"Refinery Net Production of Crude Oil and Petroleum Products"
series$freq<-"Monthly"

production_padd<-series



#product supplied by PADD

#http://api.eia.gov/category/?api_key=YOUR_API_KEY_HERE&category_id=296686


subs<-data_fetch(KEY,cat=401685)
sub_cats<-subs$Sub_Categories

data_store <- list()
stack<-1
for (cat in sub_cats$category_id) {
  #for testing
  #cat<-sub_cats$category_id[2]
  subs<-data_fetch(KEY,cat=cat)
  series <-subs$Series_IDs %>% filter(grepl("Monthly",name),units=="Thousand Barrels")
  series$name<-as.character(series$name)
  series$To<-do.call("rbind",strsplit(series$name, " Product Supplied of "))[,1]
  series$Mode<-"Product Supplied"
  Rest<-do.call("rbind",strsplit(series$name, " Product Supplied of "))[,2]
  series$From<-series$To
  series$Commodity<-do.call("rbind",strsplit(Rest, ", Monthly"))[,1]
  series$freq<-"Monthly"
  data_store[[stack]]<-series
  stack<-stack+1
}

  
  
prod_supply_padd<-data.frame(do.call(rbind,data_store))%>%filter(To != "U.S.")


#all movements between PADDs
#http://api.eia.gov/category/?api_key=YOUR_API_KEY_HERE&category_id=314562

subs<-data_fetch(KEY,cat=330523)
sub_cats<-subs$Sub_Categories

data_store <- list()
stack<-1
for (cat in sub_cats$category_id) {
  #for testing
  #cat<-sub_cats$category_id[4]
  subs<-data_fetch(KEY,cat=cat)
  series <-subs$Series_IDs %>% filter(grepl("Monthly",name),units=="Thousand Barrels")
  series$name<-as.character(series$name)
  series$name<-gsub("From","from",series$name)
  series$To<-do.call("rbind",strsplit(series$name, " Receipts by "))[,1]
  Rest<-do.call("rbind",strsplit(series$name, " Receipts by "))[,2]
  series$Mode<-do.call("rbind",strsplit(Rest, " from "))[,1]
  Rest<-do.call("rbind",strsplit(Rest, " from "))[,2]
  series$From<-do.call("rbind",strsplit(Rest, " of "))[,1]
  Rest<-do.call("rbind",strsplit(Rest, " of "))[,2]
  series$Commodity<-do.call("rbind",strsplit(Rest, ", Monthly"))[,1]
  series$freq<-"Monthly"
  data_store[[stack]]<-series
  stack<-stack+1
}


movements_padd<-data.frame(do.call(rbind,data_store))%>%filter(To != "U.S.")







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


imports_padd<-data.frame(do.call(rbind,data_store))%>%filter(To != "U.S.")




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







padd_data<-rbind(imports_padd,exports_padd)%>% filter(grepl("Crude Oil",name))
padd_data<-rbind(padd_data,movements_padd%>% filter(grepl("Crude Oil",name)))
padd_data<-rbind(padd_data,prod_supply_padd%>% filter(grepl("Crude Oil",name)))


padd_data$series_id<-as.character(padd_data$series_id)
padd_data$series_id2<-gsub('\\-','.',padd_data$series_id)


#get all the movements and imports data

#all_data<-data.frame(pdfetch_EIA(padd_data$series_id,KEY),stringsAsFactors = F)
ref_prod_data<-EIA_to_DF(padd_data)

padds<-c("East Coast (PADD 1)","Midwest (PADD 2)","Gulf Coast (PADD 3)","Rocky Mountain (PADD 4)",
         "West Coast (PADD 5)")
series<-c("Imports","Exports","Receipts by Pipeline, Tanker, Barge and Rail","Product Supplied")
products<-c("Crude Oil and Petroleum Products","Crude Oil")



movements<-melt(ref_prod_data,id="date",variable.name = "series",value.name = "quantity") #convert data to long form with series ids
movements<-movements %>% left_join(padd_data,by=c("series"="name"))
movements$quantity[is.na(movements$quantity)]<-0
movements$quantity[movements$units=="Thousand Barrels"]<-movements$quantity[movements$units=="Thousand Barrels"]/
  days_in_month(movements$date[movements$units=="Thousand Barrels"])
movements$units[movements$units=="Thousand Barrels"]<-"Thousand Barrels per Day"
movements$units<-factor(movements$units)

#build products only series -> no PADD 1 to 5 crude movements

movements_test<-movements %>% #filter(To=="Gulf Coast (PADD 3)")%>% 
  group_by(date,Mode,To,From) %>%
  dcast(date+To+From+Mode ~ Commodity, value.var = "quantity") %>% replace(is.na(.), 0)%>%
  mutate(`Petroleum Products`=pmax(`Crude Oil and Petroleum Products`-`Crude Oil`,0))%>%
  melt(id=c("date","Mode","To","From"),value.name = "quantity",variable.name="Commodity")%>%
  filter(Commodity=="Petroleum Products")
  

ref_prod<-EIA_to_DF(production_padd) %>%
  melt(id="date",variable.name = "series",value.name = "quantity")%>% #convert data to long form with series id
left_join(production_padd,by=c("series"="name"))%>%
  select("date","Mode","To","From","Commodity","quantity")

movements_test<-movements_test%>%rbind(ref_prod)


#test some graphing

#keeps<-c("East Coast (PADD 1)","Midwest (PADD 2)","Gulf Coast (PADD 3)","Rocky Mountain (PADD 4)",
#         "West Coast (PADD 5)")

test<-movements_ %>% filter(date>=ymd("2005-01-01"),From=="Midwest (PADD 2)",(To=="East Coast (PADD 1)" | To=="Gulf Coast (PADD 3)"))
  ggplot(movements_test,aes(date,quantity,group = To,colour=To)) +
geom_line()




df1<-movements_test %>% filter(date>=ymd("2005-01-01")) %>%
  mutate(To=factor(To)) %>% mutate(To=fct_collapse(To,
                                                   "Trade" = c("Export")
  )
  ) %>%
  mutate(From=factor(From)) %>% mutate(From=fct_collapse(From,
                                                         "Trade" = c("Import")
                                                         )
  )
#df1$From<-fct_recode(df1$From, "Imports from Canada" = "Canada", "Other Imports" = "Other")
df1$From<- factor(df1$From, levels=c("East Coast (PADD 1)","Midwest (PADD 2)","Gulf Coast (PADD 3)","Rocky Mountain (PADD 4)",
                                     "West Coast (PADD 5)","Trade"))
df1$To<- factor(df1$To, levels=c("East Coast (PADD 1)","Midwest (PADD 2)","Gulf Coast (PADD 3)","Rocky Mountain (PADD 4)",
                                 "West Coast (PADD 5)","Trade"))




png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("padd_prod_imports.png")
prod_moves_in<-ggplot(filter(df1,To != "Trade",Mode!="Product Supplied"),aes(date,quantity,group = From,colour=From,fill=From)) +
  geom_area(position = "stack")+
  facet_grid(~To)+
  scale_x_date()+
  scale_fill_manual("Source of Crude",values = colors_tableau10()[-7],guide = "legend")+
  scale_y_continuous(breaks = seq(0,12000,3000))+
  scale_colour_manual("Source",values=colors_tableau10()[-7],guide = "legend")+
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
  labs(y="Total Petroleum Products (MMbbl/d)",x="Year",
       title=paste("US Refined Product Source By Region"),
       subtitle=paste("Production appears as sourced from within a PADD, so PADD 2 products production is shown as from PADD 2 in PADD 2"),
       caption="Source: EIA API, graph by Andrew Leach.")
print(prod_moves_in)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("padd_prod_exports.png")
prod_moves_out<-ggplot(filter(df1,Mode!="Refinery Net Production",From!="Trade"),aes(date,quantity,group = To,colour=To,fill=To)) +
  geom_area(position = "stack")+
  facet_grid(~From)+
  scale_x_date()+
  scale_y_continuous(breaks = seq(0,12000,3000))+
  scale_fill_manual("Destination",values = colors_tableau10()[-7],guide = "legend")+
  scale_colour_manual("Destination",values=colors_tableau10()[-7],guide = "legend")+
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
  labs(y="Total Petroelum Products (MMbbl/d)",x="Year",
       title=paste("US Refined Product Destination by Region"),
       subtitle=paste("Product supplied appears as, for example, the share of PADD 2 products destined for PADD 2"),
       caption="Source: EIA API, graph by Andrew Leach.")
print(prod_moves_out)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

png<-1
if(png==1)
  set_png(file=paste("prod_movements.png",sep=""),height = 1200,width=2000)
grid.arrange(arrangeGrob(prod_moves_in +labs(caption="Note: Production denoted as supply from within the PADD, i.e. PADD 2 production appears as PADD 2 supply from PADD 2.")+
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
                         prod_moves_out+labs(caption="Note: Product supplied within the PADD appears as such, i.e. PADD 2 product supplied appears as PADD 2 supply destined for PADD 2.")+
                           theme(legend.position="none",
                                 plot.title = element_text(size = 12,face = "bold", colour="black"),
                                 plot.subtitle = element_blank(),
                                 plot.caption = element_text(size = 12,face = "italic", colour="black"),          
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




#testing products data

subs<-data_fetch(KEY,cat=314640)
#sub_cats<-subs$Sub_Categories

series <-subs$Series_IDs %>% filter(grepl("PADD",name),grepl("Monthly",name),units=="Thousand Barrels per Day")
series$name<-as.character(series$name)
test_supply<-series %>% filter(grepl("Crude Oil",name))
test_supply<-rbind(test_supply,series %>% filter(grepl("Finished Petroleum Products",name)))
test_data<-EIA_to_DF(test_supply)
test_data$test<-test_data$`East Coast (PADD 1) Imports of Crude Oil and Petroleum Products, Monthly`-test_data$`East Coast (PADD 1) Imports of Finished Petroleum Products, Monthly`
test_data$test<-test_data$`East Coast (PADD 1) Imports of Crude Oil and Petroleum Products, Monthly`-test_data$`East Coast (PADD 1) Imports of Finished Petroleum Products, Monthly`-test_data$`East Coast (PADD 1) Imports of Crude Oil, Monthly`
ggplot(test_data,aes(date,test)) +
  geom_line()





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


