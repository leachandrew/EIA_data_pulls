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



#code to access EIA demand by region

demand_series<-get_children(2122628)

local_time_demand<-demand_series %>% filter(f=="HL") %>%
  filter(name%in% c(
    #"Demand for PJM Interconnection, LLC (PJM), hourly - local time",
    "Demand for Midcontinent Independent System Operator, Inc. (MISO), hourly - local time",
    #"Demand for California Independent System Operator (CISO), hourly - local time",
    "Demand for Electric Reliability Council of Texas, Inc. (ERCO), hourly - local time",
    #"Demand for New York Independent System Operator (NYIS), hourly - local time",
    #"Demand for Bonneville Power Administration (BPAT), hourly - local time",
    NULL
  )
  )%>% mutate(f="H",
              name=gsub("Demand for ","",name),
              name=gsub(", hourly - local time","",name)
  )


data_list<-list()
for(i in seq(1,NROW(local_time_demand$series_id))){
  data_list[[i]]<-EIAdata::getEIA(ID=local_time_demand$series_id[i],key = KEY)
}

power_data<-do.call(cbind, data_list) %>% fortify.zoo %>% as_tibble 
names(power_data)<-c("local_time",local_time_demand$name)

power_data<-power_data %>% mutate(local_time=ymd_hms(local_time))

load("../alberta_power/forecast_data.Rdata")

#join AB data - we're tagging these all as UTC, so force AB to the same or it will adjust everything

power_data<-power_data %>% left_join(forecast_data%>%select(local_time=time,"Alberta Electric System Operator (AESO)"=actual_ail)
                                     %>%mutate(local_time=force_tz(local_time,tzone="UTC"))
                                     ,by=c("local_time"))


power_data<-power_data %>% pivot_longer(-local_time,values_to="load",names_to="ieso") %>% filter(!is.na(load))

index_vars<-power_data %>% filter(year(local_time)==2019) %>% group_by(ieso) %>% summarize(index=mean(load))

power_data<-power_data %>% left_join(index_vars)

power_data<-power_data %>% mutate(indexed_load=load/index*100)

power_data<-power_data %>%assign_date_time_days(time_var = local_time)%>% assign_peaks(time_var = local_time)


power_data<-power_data %>% mutate(ieso=gsub("ERCO","ERCOT",ieso))                                  

h_load<-power_data %>%group_by(ieso,year,hour)%>%summarize(load=mean(indexed_load),min=min(indexed_load),max=max(indexed_load))%>%
  mutate(time=ymd_hm(paste("2020-01-01 ",hour,":00",sep="")))


top_panel<-
  ggplot(filter(h_load,year==2019)) +
  geom_line(aes(time,load,color=ieso,group=ieso),size=1)+
  geom_line(data=filter(h_load,year==2019,ieso=="Alberta Electric System Operator (AESO)"),aes(time,load,color=ieso,group=ieso),size=1.25)+
  scale_colour_manual(NULL,values=c("black",colors_tableau10()))+
  scale_x_datetime(date_breaks = "2 hours", date_labels =  "%H:00",expand = c(0.01,0)) +
  scale_y_continuous(expand = c(0, 0),breaks = c(80,90,100,110,120)) +
  #expand_limits(y=c(80,120))+
  #guides(colour=guide_legend(nrow=1),fill=guide_legend(nrow=1))+
  labs(y="Hourly Average Load (Annual average = 100)",x="",
       title="Indexed Load by Hour (2019)",
       caption="Data via EIA and AESO")+
  ajl_line()+
NULL
top_panel

m_load<-power_data %>% group_by(ieso,year,month_fac,month) %>% summarize(load=mean(indexed_load,na.rm = T),min=min(indexed_load),max=max(indexed_load))%>%
  mutate(time=ymd_hm(paste("2020-",month,"-1 18:00",sep="")))

bottom_panel<-
  ggplot(filter(m_load,year==2019)) +
  geom_line(aes(time,load,color=ieso,group=ieso),size=1)+
  geom_line(data=filter(m_load,year==2019,ieso=="Alberta Electric System Operator (AESO)"),aes(time,load,color=ieso,group=ieso),size=1.25)+
  scale_colour_manual(NULL,values=c("black",colors_tableau10()))+
  scale_x_datetime(date_breaks = "1 months", date_labels =  "%b",expand=c(.01,0)) +
  #scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),breaks = c(80,90,100,110,120)) +
  guides(colour=guide_legend(nrow=2))+
  labs(y="Monthly Average Load (Annual average = 100)",x="",
       title="Indexed Load by Month (2019)",
       caption="Data via EIA and AESO")+
  ajl_line()+
NULL

bottom_panel

my_legend <- get_legend(
  bottom_panel + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 9,face = "bold"))
  )

p_grid<-plot_grid(
  top_panel+ ylim(80, 130)+
     theme(legend.position="none",
           plot.title = element_text(face = "bold"),
           plot.subtitle = element_text(size = 16, face = "italic"),
           plot.caption = element_blank(),
           panel.grid = element_blank(),
           text = element_text(size = 10,face = "bold"),
           axis.text = element_text(size = 10,face = "bold", colour="black"),
           #axis.title.x = element_blank()
           NULL
     )+
  NULL,
  bottom_panel+ ylim(80,130)+
    theme(legend.position="none",
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 16, face = "italic"),
          plot.caption = element_text(size = 8, face = "italic"),
          panel.grid = element_blank(),
          text = element_text(size = 10,face = "bold"),
          axis.text = element_text(size = 10,face = "bold", colour="black"),
          axis.title.x = element_blank(),
          NULL
    )+
    NULL,
  align = TRUE,axis="l", nrow = 1, rel_widths = c(1,1)
)

plot_grid(p_grid, my_legend, ncol = 1, rel_heights = c(1, .1))
ggsave("test.png",width = 14)





