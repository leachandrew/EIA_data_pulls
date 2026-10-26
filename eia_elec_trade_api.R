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

eia_clear_cache()
elec_trade_day <- function(date_sent){
  
  eia_data(
  dir = "electricity/rto/interchange-data",
  data = "value",
  facets = list(toba=list("AESO","BCHA","HQT","IESO","MHEB","NBSO","SPC")),
  freq = "hourly",
  start = date_sent,
  end= as.character(ymd(date_sent)+days(1)),
  sort = list(cols = "period", order = "asc"),
)%>%clean_names()%>%
  mutate(period=ymd_h(period))#%>%
  filter(date(period)==ymd(date_sent))
  }

days<-
  as.character(seq.Date(from = as_date("2025-03-01"),to=Sys.Date()-days(2),by=1))

elec_data<-NULL
for(day in days){
  print(as.character(day))
  elec_data<-bind_rows(elec_data,elec_trade_day(as.character(day)))
  }


elec_data%>% group_by(period,toba)%>% summarize(n=n())%>% distinct()

