
```{r, echo=FALSE}
# we're going to need to convert our start and end dates to date format using ymd from lubridate

library(httr)

get_imports_data <- function(start_date,end_date){
  
  GET(url=paste("https://api.eia.gov/v2/","natural-gas/move/poe1/data",sep=""),
      query = list(
        api_key=KEY,
        frequency="monthly",
        "data[0]"="value",
        "facets[process][]"="IRP",
        start=start_date,
        end=end_date,
        "sort[0][column]"="period",
        "sort[0][direction]"="asc",
        out="json"
      )
  )%>% 
    httr::content() %>%
    pluck("response", "data") %>%
    enframe() %>%
    unnest_auto(value) %>%
    clean_names()%>%filter(grepl("NCA",series))%>%
    separate(series_description, into = c("loc"), sep = " Natural Gas Pipeline Imports", extra =     "drop")%>%
    select(period,loc,value,units)%>%
    mutate(trade_flow="Exports from Canada to the US",
           period=ymd(paste(period,"-01",sep="")),
           value=as.numeric(value))
}

imports_data<-get_imports_data(as.character(Sys.Date()-years(15)),as.character(Sys.Date()))



get_exports_data<-function(start_date=as.character(Sys.Date()-years(15)),
                           end_date=as.character(Sys.Date())){
  
  #test<-
  GET(url=paste("https://api.eia.gov/v2/","natural-gas/move/poe2/data",sep=""),
      query = list(
        api_key=KEY,
        frequency="monthly",
        "data[0]"="value",
        "facets[process][]"="ENP",
        start=start_date,
        end=end_date,
        "sort[0][column]"="period",
        "sort[0][direction]"="asc",
        out="json"
      ))%>%
    httr::content() %>%
    pluck("response", "data") %>%
    enframe() %>%
    unnest_auto(value) %>%
    clean_names()%>%filter(grepl("NCA",series))%>%
    separate(series_description, into = c("loc", "unit"), sep = " \\(", extra = "merge")%>%
    separate(loc, into = c("loc"), sep = " Natural Gas Pipeline Exports to Canada", extra = "drop")%>%
    select(period,loc,value,units)%>%
    mutate(trade_flow="Imports from the US to Canada",
           period=ymd(paste(period,"-01",sep="")),
           value=as.numeric(value))
  
}
exports_data<-get_exports_data()

assignment_data<-bind_rows(imports_data,exports_data)%>%
  separate(loc,into=c("loc","state"),sep=", ")%>%
  group_by(period,trade_flow,state,units) %>% summarize(value=sum(value,na.rm=T),.groups = "drop")%>%
  mutate(pair=interaction(str_to_upper(state),str_to_title(trade_flow),sep = ", "),
         pair=gsub("Us","US",pair))

major_states<-
  assignment_data %>% group_by(pair,trade_flow)%>%
  summarize(mean_flow=mean(value,na.rm=T))%>%
  ungroup()%>%
  slice_max(mean_flow, n = 8, with_ties = FALSE)%>% # Select top 5 per trade_flow
  ungroup()


assignment_data <- assignment_data %>%
  mutate(state = ifelse(pair %in% major_states$pair,
                        state,"Other")) %>%
  mutate(state=as_factor(str_to_upper(state)))%>%
  group_by(period,state,trade_flow,units)%>%
  summarize(value=sum(value,na.rm=T),.groups = "drop")%>%
  mutate(state=fct_relevel(state,"OTHER",after = Inf),
         value=value/days_in_month(period))

#print(assignment_data)


write_excel_csv(assignment_data,"assignment_2_data_2.csv")
```

```{r, echo = F,eval=T,include=TRUE,out.width="95%",dpi=150,fig.align="center",echo=FALSE,fig.height=9,fig.width=14,cache=FALSE}

state_hex_colors <- c(
  "ID" = "#1F77B4",  # Blue (State seal background)
  "MN" = "#2CA02C",  # Green (Contrast from other blues)
  "MT" = "#FF7F0E",  # Orange (Closest to Montana's gold)
  "ND" = "#D62728",  # Red (Contrast against blues)
  "NY" = "#9467BD",  # Purple (Unique from others)
  "WA" = "#8C564B",  # Brown (Only non-blue U.S. flag)
  "ME" = "#E377C2",  # Pink (For strong distinction)
  "MI" = "#7F7F7F",  # Gray (Neutral, fits Michigan’s scheme)
  "OTHER" = "#BCBD22" # Yellow-Green (Clearly separate category)
)


assignment_data_gas<-read_csv("assignment_2_data_2.csv")%>%
  mutate(state=as_factor(state),
         state=fct_relevel(state,c("WA","ID","MT","ND","MN","MI","NY","ME")),
         state=fct_relevel(state,"OTHER",after = 10))



imports<-    ggplot(assignment_data_gas%>%filter(grepl("mports",trade_flow))%>%
                      I())+
  geom_area(aes(period,value/1000,group=state,fill=state),color="white",linewidth=0.25,position="stack")+
  facet_grid(rows=vars(trade_flow))+
  scale_x_date(expand=c(0,0),date_breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  scale_fill_manual("",values=c(state_hex_colors))+
  #scale_fill_manual("",values=rev(colors_tableau10()))+
  theme_classic() +
  expand_limits(y=10)+
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
  theme(legend.position = "right",
        legend.margin=margin(t = 0,b=0, unit='cm'))+
  #t, r, b, l (To remember order, think trouble)
  theme(plot.margin = unit(c(1,1,0.2,1), "cm"))+
  guides(color = guide_legend(keywidth = unit(1.6,"cm"),nrow = 1),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 1))+
  labs(y="Imports (billion cubic feet per day)",x="",
       #title="US Natural Gas Trade With Canada by State",
       caption = paste("Data via US Energy Information Administration, current to ",format(max(assignment_data$period),"%B %Y"),". Graph by Andrew Leach.",sep="")
  )+
  NULL

exports<-    
  ggplot(assignment_data_gas%>%filter(grepl("xports",trade_flow))%>%
           I())+
  geom_area(aes(period,value/1000,group=state,fill=state),color="black",linewidth=0.25,position="stack")+
  facet_grid(rows=vars(trade_flow))+
  scale_x_date(expand=c(0,0),date_breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(breaks=pretty_breaks(), expand=c(0,0))+
  scale_fill_manual("",values=c(state_hex_colors))+
  #scale_fill_manual("",values=colors_tableau10())+
  theme_classic() +
  expand_limits(y=10)+
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
  theme(legend.position = "right",
        legend.margin=margin(t = 0,b=0, unit='cm'))+
  #t, r, b, l (To remember order, think trouble)
  theme(plot.margin = unit(c(1,1,0.2,1), "cm"))+
  labs(y="Exports (billion cubic feet per day)",x="",
       title="US Natural Gas Trade With Canada by State",
       #         caption = paste("Data via US Energy Information Administration, current to #",format(max(assignment_data$period),"%B %Y"),". Graph by Andrew Leach.",sep="")
  )+
  NULL

cowplot::plot_grid(exports,imports,ncol = 1,rel_heights = c(1.4,1.2))
```
