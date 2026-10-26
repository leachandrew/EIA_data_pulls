library(janitor)
library(tidyverse)
library(lubridate)
library(scales)
library(viridis)
library(cowplot)
library(eia)


eia_set_key(KEY) # set API key if not already set globally
#print(n=1000,eia_dir("petroleum/move/ptb"))

#eia_metadata("petroleum/move/impcp")


#print(n=50,eia_dir("petroleum/sum/crdsnd"))
eia_dir()

moves_in <- eia_data(
  dir = "petroleum/move/ptb",
  data = "value",
  facets = list(product="EPC0"),
  #facets = list(series=list( "MCRMXP1P21", "MCRMXP1P31", "MCRMXP1P51")),
  freq = "monthly",
  start = "2000",
  sort = list(cols = "period", order = "asc"),
)%>%clean_names()%>%
  mutate(series_description=gsub("Receipts by Pipeline, Tanker, Barge and Rail ","",series_description),
         series_description=gsub(" of Crude Oil \\(Thousand Barrels\\)","",series_description),
         value=as.numeric(value))%>%
  separate(series_description,sep=" from ",into = c("area","source"))%>%
  select(period,area,source,value)%>%
  mutate(source=paste("Movements from",source))
  

moves_in_collapse <- 
  moves_in %>% 
  mutate(source=gsub("Movements from ","",source),
         source=as_factor(source),
         area=as_factor(area),
         area=fct_recode(area,"Midwest and Rockies (PADDs 2 and 4)"="Midwest (PADD 2)"),
         area=fct_recode(area,"Midwest and Rockies (PADDs 2 and 4)"="Rocky Mountain (PADD 4)"),
         source=as_factor(source),
         source=fct_recode(source,"Midwest and Rockies (PADDs 2 and 4)"="Midwest (PADD 2)"),
         source=fct_recode(source,"Midwest and Rockies (PADDs 2 and 4)"="Rocky Mountain (PADD 4)"))%>%
  filter(area!=source)%>%
  mutate(source=as.character(source),area=as.character(area))%>%
  mutate(source=paste("Movements from",source))
         
         
moves_out<-moves_in%>%rename(area=source,source=area)%>%
  mutate(value=-1*value,
         area=gsub("Movements from ","",area),
         source=paste("Movements to",source))


moves_out_collapse<-moves_in_collapse%>%rename(area=source,source=area)%>%
  mutate(value=-1*value,
         area=gsub("Movements from ","",area),
         source=paste("Movements to",source))

Sys.sleep(1)

imports <- eia_data(
  dir = "petroleum/move/imp",
  data = "value",
  facets = list(product="EPC0"),
  #facets = list(series=list( "MCRMXP1P21", "MCRMXP1P31", "MCRMXP1P51")),
  freq = "monthly",
  start = "2000",
  sort = list(cols = "period", order = "asc"),
)%>%clean_names()%>%filter(area_name!="U.S.",units=="MBBL")%>%
  mutate(series_description=gsub(" Imports of Crude Oil \\(Thousand Barrels\\)","",series_description),
         source="Imports",area=series_description,
         value=as.numeric(value))%>%
  select(period,area,source,value)

Sys.sleep(1)

imports_collapse<-
  imports %>% 
  mutate(area=as_factor(area),
          area=fct_recode(area,"Midwest and Rockies (PADDs 2 and 4)"="Midwest (PADD 2)"),
          area=fct_recode(area,"Midwest and Rockies (PADDs 2 and 4)"="Rocky Mountain (PADD 4)"),
  )%>%
  group_by(period,area,source)%>%
  summarize(value=sum(value,na.rm = T))%>%
  ungroup()%>%
  mutate(area=as.character(area))


exports <- eia_data(
  dir = "petroleum/move/exp",
  data = "value",
  facets = list(product="EPC0"),
  #facets = list(series=list( "MCRMXP1P21", "MCRMXP1P31", "MCRMXP1P51")),
  freq = "monthly",
  start = "2000",
  sort = list(cols = "period", order = "asc"),
)%>%clean_names()%>%filter(area_name!="U.S.",units=="MBBL")%>%
  mutate(series_description=gsub(" Exports of Crude Oil \\(Thousand Barrels\\)","",series_description),
         area=series_description,source="Exports",
         value=as.numeric(value))%>%
  mutate(value=-1*value)%>%#exports are like negative production
  select(period,area,source,value)

exports_collapse<-
  exports %>% 
  mutate(area=as_factor(area),
         area=fct_recode(area,"Midwest and Rockies (PADDs 2 and 4)"="Midwest (PADD 2)"),
         area=fct_recode(area,"Midwest and Rockies (PADDs 2 and 4)"="Rocky Mountain (PADD 4)"),
  )%>%
  group_by(period,area,source)%>%
  summarize(value=sum(value,na.rm = T))%>%
  ungroup()%>%
  mutate(area=as.character(area))



Sys.sleep(1)
prod <- eia_data(
  dir = "petroleum/sum/snd",
  data = "value",
  #facets = list(product="EPC0"),
  facets = list(series=list("MCRFPP11","MCRFPP21","MCRFPP31","MCRFPP41","MCRFPP51")),
  freq = "monthly",
  start = "2000",
  sort = list(cols = "period", order = "asc"),
)%>%clean_names()%>% mutate(series_description=gsub(" Field Production of Crude Oil \\(Thousand Barrels\\)","",series_description),
                            area=series_description,source="Domestic Production",
                            value=as.numeric(value))%>%
  select(period,area,source,value)


prod_collapse<-
  prod %>% 
  mutate(area=as_factor(area),
         area=fct_recode(area,"Midwest and Rockies (PADDs 2 and 4)"="Midwest (PADD 2)"),
         area=fct_recode(area,"Midwest and Rockies (PADDs 2 and 4)"="Rocky Mountain (PADD 4)"),
  )%>%
  group_by(period,area,source)%>%
  summarize(value=sum(value,na.rm = T))%>%
  ungroup()%>%
  mutate(area=as.character(area))


movements<-bind_rows(prod,imports,exports,moves_in,moves_out)%>%
  mutate(period=ymd(paste(period,01,sep = "-")))%>%
  mutate(value=value/days_in_month(period),
  area=factor(area,
              levels=c("East Coast (PADD 1)","Midwest (PADD 2)","Gulf Coast (PADD 3)","Rocky Mountain (PADD 4)", "West Coast (PADD 5)")))

movements_plot<-
movements %>% 
  mutate(source=ifelse(grepl("Movements from",source),"Movements in from other PADDs",source))%>%
  mutate(source=ifelse(grepl("Movements to",source),"Movements out to other PADDs",source))%>%
  group_by(period,area,source)%>%
  summarize(value=sum(value),.groups = "drop")%>%
  group_by(period,area)%>%
  mutate(net_supply=sum(value),.groups = "drop")%>%
  mutate(source=as_factor(source),
         source=fct_relevel(source,"Exports",after = 4),
         source=fct_relevel(source,"Imports"))%>%
  ggplot()+
  geom_area(aes(period,value/1000,fill=source),position = "stack",color="black",linewidth=0.25)+
  geom_line(aes(period,net_supply/1000,colour="Net Supply"),linewidth=0.85)+
  facet_wrap(~area,nrow=1)+
  scale_x_date(date_breaks = "2 years",date_labels = "%Y",expand=c(0,0))+
  scale_fill_manual("",values = colors_tableau10()[-7],guide = "legend")+
  scale_colour_manual("",values = c("black"),guide = "legend")+
  guides(fill=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
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
  labs(y="Crude Oil Disposition (millon barrels per day)",x="",
       title=paste("US Crude Oil Movements by Region"),
       #subtitle=paste("Production appears as sourced from within a PADD, so PADD 2 production is oil from PADD 2 in PADD 2"),
       caption="Source: EIA API, graph by Andrew Leach.")


movements_plot+
  labs(title=NULL,
       caption=NULL
  )       +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    
    #plot.title = element_text(size = 10, color = "black"),
    #axis.title  = element_text(size = 9, color = "black"),
    
    # x-axis labels shifted slightly down from ticks
    text = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,
                               vjust = 0.5,  # centers horizontally
                               hjust = 1     # anchors to tick mark
    ),
    
    # y-axis labels unchanged
    axis.text.y = element_text(size = 12, color = "black"),
    
    #panel.grid = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(t = 5, r = 20, b = 5, l = 5)
  )



ggsave("movements_padd.png",dpi=300,width=12,height = 6)


# Combined PADD 2/4 and iso padd 2/4 and 3


movements_collapse<-bind_rows(prod_collapse,imports_collapse,exports_collapse,moves_in_collapse,moves_out_collapse)%>%
  filter(area %in% c("Midwest and Rockies (PADDs 2 and 4)","Gulf Coast (PADD 3)","West Coast (PADD 5)") )%>%
  mutate(period=ymd(paste(period,01,sep = "-")))%>%
  mutate(value=value/days_in_month(period),
         area=factor(area,
                     levels=c("Midwest and Rockies (PADDs 2 and 4)","Gulf Coast (PADD 3)","West Coast (PADD 5)")),
         #source=as_factor(source)
         )

movements_plot_small<-
  movements_collapse %>% 
  mutate(source=ifelse(grepl("Movements from",source),"Movements in from other PADDs",source))%>%
  mutate(source=ifelse(grepl("Movements to",source),"Movements out to other PADDs",source))%>%
  group_by(period,area,source)%>%
  summarize(value=sum(value),.groups = "drop")%>%
  group_by(period,area)%>%
  mutate(net_supply=sum(value),.groups = "drop")%>%
  mutate(source=as_factor(source),
         source=fct_relevel(source,"Exports",after = 4),
         source=fct_relevel(source,"Domestic Production",after = 2),
         source=fct_relevel(source,"Imports"))%>%
  ggplot()+
  geom_area(aes(period,value,fill=source),position = "stack",color="black",linewidth=0.25)+
  geom_line(aes(period,net_supply,colour="Net Supply"),linewidth=0.85)+
  facet_wrap(~area,nrow=1)+
  scale_x_date(date_breaks = "2 years",date_labels = "%Y",expand=c(0,0))+
  scale_fill_manual("",values = colors_tableau10()[-7],guide = "legend")+
  scale_colour_manual("",values = c("black"),guide = "legend")+
  guides(fill=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
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
  labs(y="Crude Oil Disposition (thousands of barrels per day)",x="",
       title=paste("US Crude Oil Movements by Region"),
       #subtitle=paste("Production appears as sourced from within a PADD, so PADD 2 production is oil from PADD 2 in PADD 2"),
       #caption="Source: EIA API, graph by Andrew Leach."
       caption = paste("Data via US Energy Information Administration, current to ",format(max(movements_collapse$period),"%B %Y"),". Graph by Andrew Leach.",sep="")
       )

movements_plot_small+
  #labs(#title=NULL,
       #caption=NULL
  #)       +
  theme_irpp() +
  guides(fill=guide_legend(nrow = 2),colour=guide_legend(order=1))+
  theme(
    axis.text.x = element_text(angle = 90,
                               vjust = 0.5,  # centers horizontally
                               hjust = 1     # anchors to tick mark
    ),
    #panel.grid = element_blank(),
    #legend.position = "bottom",
    #plot.margin = margin(t = 5, r = 20, b = 5, l = 5)
  )



ggsave("movements_padd_collapse.png",dpi=300,width=12,height = 6)
ggsave("movements_padd_collapse.eps",dpi=300,width=8,height = 5)
ggsave("movements_padd_collapse.svg",dpi=300,width=8,height = 5)







# Import by PADD and Grade

eia_dir("crude-oil-imports")

start_date<-ymd("2022-01-01")
end_date<-ymd("2025-12-31")

eia_clear_cache()

get_import_grade<-function(start_date="2022",end_date=as.character(year(Sys.Date()))){
Sys.sleep(2)
eia_data(
  dir = "crude-oil-imports",
  data = "quantity",
  facets = list(gradeId=list("HSO","HSW","LSO","LSW","MED"),
                originId=list("CTY_CA","WORLD"),
                destinationId=list("RP_1","RP_2","RP_3","RP_4","RP_5")
                ),
  freq = "monthly",
  start = as.character(start_date),
  end = as.character(end_date),
  sort = list(cols = "period", order = "asc"),
)%>%clean_names()%>%mutate(period=ymd(paste(period,01,sep = "-")),
                           quantity=as.numeric(quantity))%>%
  mutate(quantity_bpd=quantity/days_in_month(period))
}


import_data_old<-bind_rows(get_import_grade(start_date="2009-01-01",end_date = "2009-12-31"),
                           get_import_grade(start_date="2010-01-01",end_date = "2011-12-31"),
                           get_import_grade(start_date="2012-01-01",end_date = "2013-12-31"),
                           get_import_grade(start_date="2014-01-01",end_date = "2015-12-31"),
                           get_import_grade(start_date="2016-01-01",end_date = "2017-12-31"),
                           get_import_grade(start_date="2018-01-01",end_date = "2019-12-31"),
                           get_import_grade(start_date="2020-01-01",end_date = "2021-12-31"),                         
                           get_import_grade(start_date="2022-01-01",end_date = "2023-12-31"),                         
                           get_import_grade(start_date="2024-01-01",end_date = "2026-12-31"),
                           #get_import_grade(start_date="2025-01-01",end_date=as.character(Sys.Date())),                         
                           )%>%
  select(period,origin_name,destination_name,grade_name,quantity)%>%
  pivot_wider(names_from=c(origin_name),values_from = quantity)%>%
  mutate(ROW=World-Canada)%>%select(-World)%>%pivot_longer(c(Canada,ROW),names_to = "origin_name",values_to = "quantity")%>%
  mutate(grade_name=as_factor(grade_name),
         grade_name=fct_other(grade_name,keep=c("Heavy Sour"),other_level = "Other Crude Grades")
  )%>%
  group_by(period,grade_name,origin_name,destination_name)%>%
  summarize(quantity=sum(quantity,na.rm=T))
  
  
#import_plot_factors<-
  ggplot(
    import_data_old%>%
      mutate(origin_name=as_factor(origin_name))%>%
      mutate(origin_name=fct_relevel(origin_name,"ROW"))%>%
      mutate(origin_name=fct_recode(origin_name,"Rest of the World"="ROW"),
      grade_name=as_factor(grade_name),
      grade_name=fct_other(grade_name,keep=c("Light Sweet","Medium","Heavy Sour"),other_level = "Other Grades")
      )%>%
      group_by(period,origin_name,destination_name,grade_name)%>%
      summarize(quantity=sum(quantity,na.rm=T),.groups = "drop")%>%
      mutate(grade_name=fct_relevel(grade_name,c("Light Sweet","Medium","Heavy Sour"))))+
  geom_area(aes(period,quantity/days_in_month(period),group=origin_name,fill=origin_name),color="black",linewidth=0.25,position="stack")+
  scale_x_date(expand=c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  #look here, adding in the facet wrap and the labeler
  facet_grid(cols=vars(destination_name),rows=vars(grade_name))+
  scale_fill_manual("",values=c("darkblue","red"))+
  #scale_fill_manual("",values=c("red","darkblue"))+
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
        legend.position = "bottom",
        legend.box = "vertical",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  theme(text = element_text(size=16))+
  theme(legend.position = "bottom", legend.box = "vertical",
        legend.margin=margin(t = 0,b=0, unit='cm'))+
  #t, r, b, l (To remember order, think trouble)
  theme(plot.margin = unit(c(1,1,0.2,1), "cm"))+
  guides(color = guide_legend(keywidth = unit(1.6,"cm"),nrow = 1),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 1))+
  labs(y="Imports (thousands of barrels per day)",x="",
       title="US Crude Imports from Canada and the Rest of the World",
       subtitle="Imports by Refining PADD and Grade",
       caption = paste("Data via US Energy Information Administration, current to ",format(max(import_data_old$period),"%B %Y"),". Graph by Andrew Leach.",sep="")
  )+
  NULL

ggsave("C:/Users/aleach/Documents/Courses/econ_366_w25/slides/oil_transp/import_plot.png",dpi=400,width=14,height=7,bg="white")



#import_plot_irpp<-
ggplot(
  import_data_old%>%
    mutate(origin_name=as_factor(origin_name),
    destination_name=as_factor(destination_name),
    destination_name=fct_recode(destination_name,"Midwest and Rockies (PADDs 2 and 4)"="PADD2 (Midwest)"),
    destination_name=fct_recode(destination_name,"Midwest and Rockies (PADDs 2 and 4)"="PADD4 (Rocky Mountain)"),
    destination_name=fct_recode(destination_name,"Gulf Coast (PADD 3)"="PADD3 (Gulf Coast)"),
    destination_name=fct_recode(destination_name,"West Coast (PADD 5)"="PADD5 (West Coast)"))%>%
    filter(destination_name %in% c("Midwest and Rockies (PADDs 2 and 4)","Gulf Coast (PADD 3)","West Coast (PADD 5)") )%>%
    mutate(destination_name=fct_relevel(destination_name,c("Midwest and Rockies (PADDs 2 and 4)","Gulf Coast (PADD 3)","West Coast (PADD 5)")))%>%
    mutate(origin_name=fct_relevel(origin_name,"ROW"))%>%
    mutate(origin_name=fct_recode(origin_name,"Rest of the World"="ROW"),
           grade_name=as_factor(grade_name),
           grade_name=fct_other(grade_name,keep=c("Light Sweet","Medium","Heavy Sour"),other_level = "Other Grades")
           )%>%
    group_by(period,origin_name,destination_name,grade_name)%>%
    summarize(quantity=sum(quantity,na.rm=T),.groups = "drop")%>%
    mutate(grade_name=fct_relevel(grade_name,c("Light Sweet","Medium","Heavy Sour"))))+
  geom_area(aes(period,quantity/days_in_month(period),group=origin_name,fill=origin_name),color="black",linewidth=0.25,position="stack")+
  scale_x_date(date_breaks = "2 years",date_labels = "%Y",expand=c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  #look here, adding in the facet wrap and the labeler
  facet_grid(cols=vars(destination_name),rows=vars(grade_name))+
  scale_fill_manual("",values=c("darkblue","red"))+
  #scale_fill_manual("",values=c("red","darkblue"))+
  labs(y="US Crude Oil Imports (thousands of barrels per day)",x="",
       title="US Crude Imports by Refining PADD and Grade",
       #subtitle="Imports by Refining PADD and Grade",
       caption = paste("Data via US Energy Information Administration, current to ",format(max(import_data_old$period),"%B %Y"),". Graph by Andrew Leach.",sep="")
  )+
  theme_irpp() +
  theme(
    axis.text.x = element_text(angle = 90,
                               vjust = 0.5,  # centers horizontally
                               hjust = 1     # anchors to tick mark
    )
    
  )+
  guides(color = guide_legend(keywidth = unit(1.6,"cm"),nrow = 1),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 1))+
  labs(y="Imports (thousands of barrels per day)",x="")+
  NULL

ggsave("irpp_import_plot.png",dpi=400,width=10,height=6,bg="white")

ggsave("irpp_import_plot.svg",dpi=400,width=8,height=5,bg="white")






# Trade with Canada


eia_clear_cache()


#end_date<-"2024-02-01"
get_prods_trade<-function(start_date="2024-01-01",end_date=as.character(year(Sys.Date()))){
  padd_imports<-
    eia_data(
    dir = "petroleum/move/impcp",
    data = "value",
    facets = list(product=list("EPD0",
                               "EPM0F",
                               "EPOBG",
                               "EPP0","EPLLPA","EPJK"),
                  duoarea=list("NUS-NCA","R10-NCA","R20-NCA","R30-NCA","R40-NCA","R50-NCA",
                               "R10-Z00","R20-Z00","R30-Z00","R40-Z00","R50-Z00","NUS-Z00")
  
                  ),
    
    freq = "monthly",
    start = as.character(start_date),
    end = as.character(end_date),
    sort = list(cols = "period", order = "asc"),
  )  %>%clean_names()%>%mutate(period=ymd(paste(period,01,sep = "-")),
                             value=as.numeric(value))%>%
    filter(units=="MBBL/D")%>%
    filter(process%in%c("IM0"))%>%
    mutate(series_description=gsub("Imports of ","Imports from World of ",series_description),
    )%>%
    separate(series_description,sep=" Imports from ",into = c("dest","rest"))%>%
    separate(rest,sep=" of ",into = c("source","product_name"))%>%
    mutate(product_name=gsub(" \\(Thousand Barrels per Day\\)","",product_name))%>%
    select(period,product_name,dest,source,value)%>%
    pivot_wider(names_from=source,values_fill = list(value = 0))%>%
    mutate(ROW=World-Canada)%>%
    #select(-World)%>%
    pivot_longer(c(Canada,ROW,World),names_to = "source",values_to = "value")%>%
    mutate(trade="import")%>%
      rename("partner"="source",us_region=dest)
    
  Sys.sleep(2)
  padd_exports<-
    eia_data(
      dir = "petroleum/move/expcp",
      data = "value",
      facets = list(product=list("EPD0",
                                 "EPM0F",
                                 "EPOBG",
                                 "EPP0","EPLLPA","EPJK"),
                    duoarea=list("NUS-NCA","R10-NCA","R20-NCA","R30-NCA","R40-NCA","R50-NCA",
                                 "R10-Z00","R20-Z00","R30-Z00","R40-Z00","R50-Z00","NUS-Z00")
                    
      ),
      
      freq = "monthly",
      start = as.character(start_date),
      end = as.character(end_date),
      sort = list(cols = "period", order = "asc"),
    )  %>%clean_names()%>%mutate(period=ymd(paste(period,01,sep = "-")),
                                 value=as.numeric(value))%>%
    filter(units=="MBBL/D")%>%filter(process%in%c("EEX"))%>%
    mutate(series_description=gsub("Exports of ","Exports to World of ",series_description),
                                 )%>%
    separate(series_description,sep=" Exports to ",into = c("source","rest"))%>%
    separate(rest,sep=" of ",into = c("dest","product_name"))%>%
    mutate(product_name=gsub(" \\(Thousand Barrels per Day\\)","",product_name))%>%
    select(period,product_name,source,dest,value)%>%
    pivot_wider(names_from=dest,values_fill = list(value = 0))%>%
    mutate(ROW=World-Canada)%>%
    pivot_longer(c(Canada,ROW,World),names_to = "dest",values_to = "value")%>%
    mutate(trade="export")%>%
    rename("partner"=dest,us_region=source)
  
  
  all_exports<-
    eia_data(
      dir = "petroleum/move/expc",
      data = "value",
      facets = list(product=list("EPD0",
                                 "EPM0F",
                                 "EPOBG",
                                 "EPP0","EPLLPA","EPJK"),
                    duoarea=list("NUS-NCA","NUS-Z00")
                    
      ),
      
      freq = "monthly",
      start = as.character(start_date),
      end = as.character(end_date),
      sort = list(cols = "period", order = "asc"),
    )  %>%clean_names()%>%mutate(period=ymd(paste(period,01,sep = "-")),
                                 value=as.numeric(value))%>%
    filter(units=="MBBL/D")%>%filter(process%in%c("EEX"))%>%
    mutate(series_description=gsub("Exports of ","Exports to World of ",series_description),
    )%>%
    separate(series_description,sep=" Exports to ",into = c("source","rest"))%>%
    separate(rest,sep=" of ",into = c("dest","product_name"))%>%
    mutate(product_name=gsub(" \\(Thousand Barrels per Day\\)","",product_name))%>%
    select(period,product_name,source,dest,value)%>%
    pivot_wider(names_from=dest,values_fill = list(value = 0))%>%
    mutate(ROW=World-Canada)%>%
    pivot_longer(c(Canada,ROW,World),names_to = "dest",values_to = "value")%>%
    mutate(trade="export")%>%
    rename(partner=dest,us_region=source)
  
  
  all_imports<-
    eia_data(
      dir = "petroleum/move/impcus",
      data = "value",
      facets = list(product=list("EPD0",
                                 "EPM0F",
                                 "EPOBG",
                                 "EPP0","EPLLPA","EPJK"),
                    duoarea=list("NUS-NCA","NUS-Z00")
                    
      ),
      
      freq = "monthly",
      start = as.character(start_date),
      end = as.character(end_date),
      sort = list(cols = "period", order = "asc"),
    )  %>%clean_names()%>%mutate(period=ymd(paste(period,01,sep = "-")),
                                 value=as.numeric(value))%>%
    filter(units=="MBBL/D")%>%filter(process%in%c("IM0"))%>%
    mutate(series_description=gsub("Imports of ","Imports from World of ",series_description),
    )%>%
    separate(series_description,sep=" Imports from ",into = c("source","rest"))%>%
    separate(rest,sep=" of ",into = c("dest","product_name"))%>%
    mutate(product_name=gsub(" \\(Thousand Barrels per Day\\)","",product_name))%>%
    select(period,product_name,source,dest,value)%>%
    pivot_wider(names_from=dest,values_fill = list(value = 0))%>%
    mutate(ROW=World-Canada)%>%
    pivot_longer(c(Canada,ROW,World),names_to = "dest",values_to = "value")%>%
    mutate(trade="import")%>%
    rename(partner=dest,us_region=source)
  
  
  
bind_rows(padd_imports,padd_exports,all_exports,all_imports)%>%
  pivot_wider(names_from = "trade",values_fill = list(value = 0))%>%
  mutate(net=import-export)%>%
  mutate(net=import-export)%>%
  pivot_longer(c(import,export,net),names_to = "trade")


}

padd_trade<-bind_rows(
  get_prods_trade(start_date = "2021-01-01",end_date = as.character(Sys.Date())),
  get_prods_trade(start_date = "2018-01-01","2020-12-31"),
  get_prods_trade(start_date = "2015-01-01","2017-12-31"),
  get_prods_trade(start_date = "2011-01-01","2014-12-31"),
  get_prods_trade(start_date = "2007-01-01","2010-12-31"))
  
  padd_trade%>%filter(!grepl("Total",product_name),#us_region==region,
                    partner=="Canada",trade!="net")%>%
  mutate(us_region=as_factor(us_region),
         us_region=fct_relevel(us_region,c("U.S.","East Coast (PADD 1)",     "Midwest (PADD 2)"  ,"Gulf Coast (PADD 3)","Rocky Mountain (PADD 4)","West Coast (PADD 5)")))%>%
  mutate(trade=case_when(
      trade=="export" ~ paste("Exports to",partner),
      trade=="import" ~ paste("Imports from",partner),
      .default = "NA"
    ))%>%
  ggplot()+
  #geom_area(aes(period,value,group=partner,fill=partner),colour="black",linewidth=0.15,position="stack")+
  geom_line(aes(period,value,group=trade,colour=trade),linewidth=0.85)+
  facet_grid(cols=vars(us_region),rows=vars(product_name))+
  scale_x_date(name=NULL,breaks=pretty_breaks(),date_breaks = "24 months", date_labels =  "%b\n%Y",expand=c(0,0)) +
  expand_limits(x=Sys.Date()+months(3))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  scale_color_manual("",values = colors_ua10(),guide = guide_legend(nrow=1,order=1))+
  theme_minimal()+theme(legend.position = "bottom")+
    theme(panel.spacing.x = unit(.75, "cm")) +
  labs(y="Product Trade (1000 bbl/d)",x="",
       #title=paste("US Production of Tight Oil",sep=""),
       caption="Data via EIA, graph by Andrew Leach")

ggsave("products_trade_region.png",dpi=300,width = 16,height = 12,bg="white")


products_trade_region<-function(data_sent,region){

data_sent%>%filter(!grepl("Total",product_name), us_region==region,
                    partner=="Canada")%>%
  mutate(us_region=as_factor(us_region),
         us_region=fct_relevel(us_region,c("U.S.","East Coast (PADD 1)",     "Midwest (PADD 2)"  ,"Gulf Coast (PADD 3)","Rocky Mountain (PADD 4)","West Coast (PADD 5)")))%>%
  mutate(trade=case_when(
    trade=="export" ~ paste(us_region,"exports to",partner),
    trade=="import" ~ paste(us_region,"imports from",partner),
    trade=="net" ~ paste(us_region,"net imports from",partner),
    .default = "NA"
  ))%>%
  ggplot()+
  #geom_area(aes(period,value,group=partner,fill=partner),colour="black",linewidth=0.15,position="stack")+
  geom_line(aes(period,value,group=trade,colour=trade),linewidth=0.85)+
  facet_grid(cols=vars(us_region),rows=vars(product_name))+
  scale_x_date(name=NULL,breaks=pretty_breaks(),date_breaks = "12 months", date_labels =  "%b\n%Y",expand=c(0,0)) +
  expand_limits(x=Sys.Date()+months(3))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  scale_color_manual("",values = colors_ua10(),guide = guide_legend(nrow=1,order=1))+
  theme_minimal()+theme(legend.position = "bottom")+
  labs(y="Product Trade (1000 bbl/d)",x="",
       #title=paste("US Production of Tight Oil",sep=""),
       caption="Data via EIA, graph by Andrew Leach")

ggsave(paste("products_trade_",region,".png",sep=""),dpi=300,width = 12,height = 11,bg="white")

}
products_trade_region(padd_trade,"U.S.")

products_trade_region(padd_trade,"East Coast (PADD 1)")

products_trade_region(padd_trade,"West Coast (PADD 5)")

get_padd_prod_exports<-function(start_date="2022",end_date=as.character(year(Sys.Date()))){
  Sys.sleep(2)
  exports<-
    eia_data(
      dir = "petroleum/move/expc",
      data = "value",
      facets = list(product=list("EP00","EPC0","EPD0",
                                 "EPM0F",
                                 "EPOBG",
                                 "EPP0","EPLLPA","EPJK"),
                    duoarea=list("NUS-NCA","NUS-Z00")
                    
      ),
      
      freq = "monthly",
      start = as.character(start_date),
      end = as.character(end_date),
      sort = list(cols = "period", order = "asc"),
    )  %>%clean_names()%>%mutate(period=ymd(paste(period,01,sep = "-")),
                                 value=as.numeric(value))

  net_imports<-
    eia_data(
      dir = "petroleum/move/neti",
      data = "value",
      facets = list(product=list("EP00","EPC0","EPD0",
                                 "EPM0F",
                                 "EPOBG",
                                 "EPP0","EPLLPA","EPJK"),
                    duoarea=list("NUS-NCA","NUS-Z00")
                    
      ),
      
      freq = "monthly",
      start = as.character(start_date),
      end = as.character(end_date),
      sort = list(cols = "period", order = "asc"),
    )  %>%clean_names()%>%mutate(period=ymd(paste(period,01,sep = "-")),
                                 value=as.numeric(value))
  
  
  
  
}




start_date<-ymd("2022-01-01")
#end_date<-ymd("2023-12-31")

eia_clear_cache()
get_import_prods<-function(start_date="2022",end_date=as.character(year(Sys.Date()))){
  Sys.sleep(2)
  eia_data(
    dir = "petroleum/move/impcp",
    data = "quantity",
    facets = list(gradeId=list("HSO","HSW","LSO","LSW","MED"),
                  originId=list("CTY_CA","WORLD"),
                  destinationId=list("RP_1","RP_2","RP_3","RP_4","RP_5")
    ),
    freq = "monthly",
    start = as.character(start_date),
    end = as.character(end_date),
    sort = list(cols = "period", order = "asc"),
  )%>%clean_names()%>%mutate(period=ymd(paste(period,01,sep = "-")),
                             quantity=as.numeric(quantity))%>%
    mutate(quantity_bpd=quantity/days_in_month(period))
}


# Canadian products

library(cansim)

library(cowplot)

theme_ps <- function() {
  theme_cowplot() + 
    theme(
      text = element_text(size = 12, color = "black"), 
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11), 
      axis.line = element_line(color = "black", size = 0.8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom", 
      legend.title = element_blank(),
      legend.text = element_text(size = 11)
    ) + 
    theme(plot.margin = unit(c(1,1,1,1), "cm"))
}


petroleum_sd<-get_cansim("25-10-0081-01")%>%clean_names()%>%
  mutate(ref_date=ymd(paste(ref_date,"-01",sep="")))%>%
  filter(products %in% c("Propane","Finished petroleum products","Finished motor gasoline","Kerosene-type jet fuel","Distillate fuel oil",
                         "Motor gasoline blending components"))%>%filter(grepl("supply",supply_and_disposition))

exports<-get_cansim("25-10-0081-01")%>%clean_names()%>%
  mutate(ref_date=ymd(paste(ref_date,"-01",sep="")))%>%
  filter(products %in% c("Propane","Finished petroleum products","Finished motor gasoline","Kerosene-type jet fuel","Distillate fuel oil",
                         "Motor gasoline blending components"))%>%filter(grepl("disposition",supply_and_disposition))%>%
  mutate(supply_and_disposition=gsub(", disposition","",supply_and_disposition),
         value=-1*value)



options(scipen = 999)
petroleum_sd%>%
  bind_rows(exports)%>%filter(geo%in%c("Canada","Alberta","Ontario","Quebec","New Brunswick"),
                              supply_and_disposition!="Adjustments, supply")%>%
  mutate(supply_and_disposition=gsub(", supply","",supply_and_disposition),
         daily=value/days_in_month(ref_date),
         geo=as_factor(geo),geo=fct_rev(geo)
  )%>%
  ggplot()+
  geom_area(aes(ref_date,daily/1000,fill=supply_and_disposition),colour="black",linewidth=0.15,position="stack")+
  facet_grid(cols=vars(geo),rows=vars(products))+
  scale_x_date(name=NULL,breaks=pretty_breaks(),date_breaks = "24 months", date_labels =  "%b\n%Y",expand=c(0,0)) +
  expand_limits(x=Sys.Date()+months(3))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  scale_fill_manual("",values = colors_ua10(),guide = guide_legend(nrow=1,order=1))+
  theme_minimal()+theme(legend.position = "bottom")+
  theme(panel.spacing.y = unit(.75, "cm")) +
  labs(y="Supply (thousand cubic meters per day)",x="",
       #title=paste("US Production of Tight Oil",sep=""),
       caption="Data via StatCan, graph by Andrew Leach")

ggsave("products_reg.png",dpi=300,width = 12,height = 14,bg="white")


options(scipen = 999)
petroleum_sd%>%
  bind_rows(exports)%>%filter(geo%in%c("Canada"),supply_and_disposition!="Adjustments, supply",supply_and_disposition!="Stock change")%>%
  mutate(supply_and_disposition=gsub(", supply","",supply_and_disposition),
         daily=value/days_in_month(ref_date),
         geo=as_factor(geo),geo=fct_rev(geo),
         products=str_wrap(products,width=20),
         products=as_factor(products),
         products=fct_relevel(products,"Propane",after=5),
         products=fct_relevel(products,"Motor gasoline\nblending components",after=2),
         supply_and_disposition=as_factor(supply_and_disposition)
         
  )%>%
  ggplot()+
  geom_col(aes(ref_date,daily/1000,fill=supply_and_disposition),position="stack",width=32)+
  facet_grid(cols=vars(products))+
  scale_x_date(name=NULL,breaks=pretty_breaks(),date_breaks = "2 years", date_labels =  "%b\n%Y",expand=c(0,0)) +
  expand_limits(x=Sys.Date()+months(3),y=c(-400,400))+
  scale_y_continuous(breaks=pretty_breaks(n=7),expand=c(0,0))+
  scale_fill_manual("",values = colors_tableau10()[],guide = guide_legend(nrow=1,order=1))+
  #theme_minimal()+theme(legend.position = "bottom")+
  theme_ps()+
  theme(panel.spacing.y = unit(.75, "cm")) +
  labs(#y="Supply (positive) and Demand (negative) (thousand cubic meters per day)",x="",
        y = bquote("Supply (positive) and Demand (negative) (1000 m"^.(3)*"/d)"),
       x="",
       #title=paste("US Production of Tight Oil",sep=""),
       caption="Data via StatCan, graph by Andrew Leach")

ggsave("products_can.png",dpi=300,width = 13,height = 6,bg="white")

products_graph<-function(data,geo){
data%>%
  filter(geo%in%c(geo),supply_and_disposition!="Adjustments, supply",supply_and_disposition!="Stock change")%>%
  mutate(supply_and_disposition=gsub(", supply","",supply_and_disposition),
         daily=value/days_in_month(ref_date),
         geo=as_factor(geo),geo=fct_rev(geo),
         products=str_wrap(products,width=20),
         products=as_factor(products),
         products=fct_relevel(products,"Propane",after=5),
         products=fct_relevel(products,"Motor gasoline\nblending components",after=2),
         supply_and_disposition=as_factor(supply_and_disposition)
         
  )%>%
  ggplot()+
  geom_col(aes(ref_date,daily/1000,fill=supply_and_disposition),position="stack",width=32)+
  facet_grid(cols=vars(products))+
  scale_x_date(name=NULL,breaks=pretty_breaks(),date_breaks = "2 years", date_labels =  "%b\n%Y",expand=c(0,0)) +
  expand_limits(x=Sys.Date()+months(3),y=c(-400,400))+
  scale_y_continuous(breaks=pretty_breaks(n=7),expand=c(0,0))+
  scale_fill_manual("",values = colors_tableau10()[],guide = guide_legend(nrow=1,order=1))+
  #theme_minimal()+theme(legend.position = "bottom")+
  theme_ps()+
  theme(panel.spacing.y = unit(.75, "cm")) +
  labs(#y="Supply (positive) and Demand (negative) (thousand cubic meters per day)",x="",
    y = bquote("Supply (positive) and Demand (negative) (1000 m"^.(3)*"/d)"),
    x="",
    #title=paste("US Production of Tight Oil",sep=""),
    caption="Data via StatCan, graph by Andrew Leach")
}


petroleum_sd%>%bind_rows(exports)%>%products_graph("Canada")%>%
ggsave(file="products_can.png",dpi=300,width = 13,height = 6,bg="white")

petroleum_sd%>%bind_rows(exports)%>%products_graph("Alberta")%>%
  ggsave(file="products_ab.png",dpi=300,width = 13,height = 6,bg="white")

petroleum_sd%>%bind_rows(exports)%>%products_graph("Quebec")%>%
  ggsave(file="products_qc.png",dpi=300,width = 13,height = 6,bg="white")





petroleum_sd%>%
  bind_rows(exports)%>%filter(geo%in%c("Quebec"),supply_and_disposition!="Adjustments, supply",supply_and_disposition!="Stock change")%>%
  mutate(supply_and_disposition=gsub(", supply","",supply_and_disposition),
         daily=value/days_in_month(ref_date),
         geo=as_factor(geo),geo=fct_rev(geo),
         products=str_wrap(products,width=20),
         products=as_factor(products),
         products=fct_relevel(products,"Propane",after=5),
         products=fct_relevel(products,"Motor gasoline\nblending components",after=2),
         supply_and_disposition=as_factor(supply_and_disposition)
         
  )%>%
  ggplot()+
  geom_col(aes(ref_date,daily/1000,fill=supply_and_disposition),position="stack",width=32)+
  facet_grid(cols=vars(products))+
  scale_x_date(name=NULL,breaks=pretty_breaks(),date_breaks = "2 years", date_labels =  "%b\n%Y",expand=c(0,0)) +
  expand_limits(x=Sys.Date()+months(3),y=c(-400,400))+
  scale_y_continuous(breaks=pretty_breaks(n=7),expand=c(0,0))+
  scale_fill_manual("",values = colors_tableau10()[],guide = guide_legend(nrow=1,order=1))+
  #theme_minimal()+theme(legend.position = "bottom")+
  theme_ps()+
  theme(panel.spacing.y = unit(.75, "cm")) +
  labs(#y="Supply (positive) and Demand (negative) (thousand cubic meters per day)",x="",
    y = bquote("Supply (positive) and Demand (negative) (1000 m"^.(3)*"/d)"),
    x="",
    #title=paste("US Production of Tight Oil",sep=""),
    caption="Data via StatCan, graph by Andrew Leach")


#CIMT Imports and Exports

#imports
#https://www150.statcan.gc.ca/n1/pub/71-607-x/2021004/imp-eng.htm?r1=(12,13,24,35)&r2=0&r3=0&r4=270900&r5=1&r7=0&r8=1988-01-01&r9=2024-12-01


#download.file("https://www150.statcan.gc.ca/n1/pub/71-607-x/2021004/imp-eng.htm?r1=(12,13,24,35)&r2=0&r3=0&r4=270900&r5=1&r7=0&r8=1988-01-01&r9=2024-12-01","test.csv",mode="wb")
crude_imports<-read_csv("cimt_imports.csv",skip=1)%>%clean_names()%>%
  slice(1:(n() - 5))%>%
  mutate(period=ymd(period))
  
large_imp<-crude_imports %>% filter(year(period)>=2020)%>%
  group_by(country)%>%
  summarize(value=sum(value),na.rm=T,.groups="drop")%>%
  arrange(-value)%>%
  head(5)%>%pull(country)

crude_imports<-crude_imports %>%
  mutate(country=as_factor(country),
         country=fct_other(country,keep=large_imp))%>%
  group_by(period,province,country)%>%
  summarize(value=sum(value,na.rm = T),quantity=sum(quantity,na.rm = T))


crude_imports%>%filter(province!="Nova Scotia")%>%
ggplot()+
  geom_col(aes(period,value/10^9,fill=country),position="stack",width=28,linewidth=0.2,colour="black")+
  facet_grid(cols=vars(province))+
  scale_x_date(name=NULL,breaks=pretty_breaks(),date_breaks = "2 years", date_labels =  "%b\n%Y",expand=c(0,0)) +
  #expand_limits(x=Sys.Date()+months(3),y=1.5)+
  scale_y_continuous(breaks=pretty_breaks(n=7),expand=c(0,0))+
  scale_fill_manual("",values = colors_tableau10()[],guide = guide_legend(nrow=1,order=1))+
  #theme_minimal()+theme(legend.position = "bottom")+
  theme_ps()+
  theme(panel.spacing.y = unit(.75, "cm"),legend.position = "bottom",legend.justification = 0.5) +
  labs(#y="Supply (positive) and Demand (negative) (thousand cubic meters per day)",x="",
    y = bquote("Crude oil import values (billions of dollars per month)"),
    x="",
    #title=paste("US Production of Tight Oil",sep=""),
    caption="Data via CIMT database, graph by Andrew Leach")


ggsave(file="crude_imports.png",dpi=300,width = 13,height = 6,bg="white")

crude_imports%>%filter(province=="New Brunswick",year(period)>=2023)%>%
  group_by(province,period)%>%
  mutate(total=sum(quantity,na.rm = T),
         share=quantity/total,
         country=fct_relevel(country,c("Saudi Arabia","Nigeria"),after=Inf))%>%
  filter(country%in%c("Saudi Arabia","Nigeria","United States"))%>%
  
  ggplot()+
  geom_col(aes(period,share*100,fill=country),position="dodge",width=20,linewidth=0.2,colour="black")+
  facet_grid(cols=vars(province))+
  scale_x_date(name=NULL,breaks=pretty_breaks(),date_breaks = "2 months", date_labels =  "%b\n%Y",expand=c(0,0)) +
  #expand_limits(x=Sys.Date()+months(3),y=1.5)+
  scale_y_continuous(breaks=pretty_breaks(n=7),expand=c(0,0))+
  scale_fill_manual("",values = colors_tableau10()[],guide = guide_legend(nrow=1,order=1))+
  #theme_minimal()+theme(legend.position = "bottom")+
  theme_ps()+
  theme(panel.spacing.y = unit(.75, "cm"),legend.position = "bottom",legend.justification = 0.5) +
  labs(#y="Supply (positive) and Demand (negative) (thousand cubic meters per day)",x="",
    y = bquote("Share of crude oil import value (%)"),
    x="",
    #title=paste("US Production of Tight Oil",sep=""),
    caption="Data via CIMT database, graph by Andrew Leach")



# Table 2510006301
crude_disp<-get_cansim("2510006301")%>%
  clean_names()%>%
  filter(geo=="New Brunswick")



get_script_dir <- function() {
  # Case 1: running via source()
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  
  # Case 2: running in RStudio
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    return(dirname(rstudioapi::getSourceEditorContext()$path))
  }
  
  # Case 3: fallback
  return(normalizePath(getwd()))
}

script_dir <- get_script_dir()

aeo_crude_data <- read_excel(file.path(script_dir, "aeo_all_crude.xlsx"))%>%
  mutate(AEO=as_factor(AEO))%>%
  #mutate(aeo_year=gsub("AEO ","",AEO))%>%
  #select(-AEO)%>%
  pivot_longer(-AEO,values_to = "crude_price",names_to="year")%>%
  mutate(year=as.integer(year),
         date=ymd(paste(year,"01-01",sep="-"))
         )%>%
  filter(!is.na(crude_price))%>%
  mutate(fcast=as.numeric(gsub("AEO ","",AEO)))%>%
  filter(AEO!="Actual",year>=fcast)

steo_crude<-
  eia_data(
    dir = "steo",
    data = "value",
    facets = list(seriesId=list("BREPUUS")
                  
    ),
    freq = "monthly",
    #start = as.character(start_date),
    #end = as.character(end_date),
    sort = list(cols = "period", order = "asc"),
  )  %>%clean_names()%>%
  mutate(period=ym(period),
         value=as.numeric(value))


vintages <- c("AEO 2009","AEO 2014","AEO 2016","AEO 2019","AEO 2023","AEO 2025")

my_cols <- c(
  "Historical Data"       = "black",
  "Current STEO Forecast" = "black",
  setNames(viridis_pal(option = "mako", begin = 0.1, end = 0.7, direction = -1)(length(vintages)),
           vintages),
  "AEO 2026"              = "#D55E00"   # vermilion accent
)


p<-
ggplot(aeo_crude_data%>% filter(AEO!="Actual")%>%
         filter(AEO!="AEO 2020")%>%
         filter(AEO!="AEO 2010")%>%
         filter(AEO!="AEO 2011")%>%
         filter(AEO!="AEO 2012")%>%
         filter(AEO!="AEO 2013")%>%
         filter(AEO!="AEO 2015")%>%
         filter(AEO!="AEO 2017")%>%
         filter(AEO!="AEO 2018")%>%
         filter(AEO!="AEO 2021")%>%
         filter(AEO!="AEO 2022")
       )+
  geom_line(aes(date,crude_price,colour=AEO),linewidth=1.1,lty="22")+
  geom_line(dat=aeo_crude_data%>% filter(grepl("2026",AEO)),aes(date,crude_price,colour=AEO),linewidth=2,lty="solid")+
  geom_line(data=steo_crude %>% filter(year(period)>=2004,period<=Sys.Date()),aes(period,value,colour="Historical Data"),linewidth=1.1,lty="solid")+
  geom_line(data=steo_crude %>% filter(period>=Sys.Date()),aes(period,value,colour="Current STEO Forecast"),linewidth=1.1,lty="11")+
  expand_limits(y=0)+
  scale_y_continuous(breaks=pretty_breaks(n=10), expand=c(0,0))+
  scale_x_date(date_breaks = "2 year" , date_labels = "%Y", expand=c(0,0))+
  #scale_color_manual("",values=c("black","black",colors_tableau10()),breaks=c("Historical Data","Current STEO Forecast","AEO 2009","AEO 2014","AEO 2016","AEO 2019","AEO 2023","AEO 2025","AEO 2026"))+
  scale_colour_manual("", values = my_cols,,breaks=c("Historical Data","Current STEO Forecast","AEO 2009","AEO 2014","AEO 2016","AEO 2019","AEO 2023","AEO 2025","AEO 2026"))+
  #scale_color_viridis("",discrete=T,option="magma",direction = -1,begin=0,end = 0.8)+
  guides(color=guide_legend(nrow=2,keywidth = 2))+
  theme_irpp(base_size = 18)+ rotate_x()+
  theme(plot.margin = margin(t=5,r=25,l=5,b=5))+
  labs(y="Nominal Brent Crude Oil Price (US$/bbl)",x=NULL,
       title=paste("Brent Crude Oil Nominal Prices and EIA Projections"),
       caption="Source: EIA, graph by Andrew Leach")+
  NULL
  

p 
ggsave(plot=p,"eia_aeo.png",width=11,height=6,dpi=300,bg="white")
ggsave(plot=p,"eia_aeo.svg",width=8,height=6,dpi=300,bg="white")  


