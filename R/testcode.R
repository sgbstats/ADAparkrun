library(XML)
library(tidyverse)
library(RCurl)
library(readxl)
library(openxlsx)
library(beepr)
library(chron)
library(tibble)
library(chron)
library(googlesheets4)
adapat=tribble(~Name, ~Barcode,
               "Seb", 493595,
               "Rachel", 1539187,
               "Tom C", 1939393,
               "Frankie", 4458732,
               "Alex", 4087050,
               "Rob", 5349926,
               "Andy", 7402459,
               "Luke", 2407768,
               "Jonny", 593256,
               "Adam", 7232608,
               "Katie", 7526532,
               "Max", 582473,
               "Leah", 5700545,
               "Grace", 7554791,
               "Ella", 7470572,
               "Tom A", 5243446,
               "Phil", 7266141
)%>% arrange(Name) %>%  mutate(rm=paste("|", Name, "|[A",Barcode,"](https://www.parkrun.org.uk/parkrunner/",Barcode,"/all/)|",sep=""),
                               url=paste("https://www.parkrun.org.uk/parkrunner/",Barcode,"/all/",sep=""))



write_lines(c("|Name|Barcode|","|-|-|", adapat$rm), "README.md")


parkruns=tribble(~Name,~Event,~`Run Date`,~`Run Number`,~Pos,~Time,~AgeGrade,~`PB?`)

for(i in 1:nrow(adapat))
{
  url=adapat$url[i]
  link=getURL(url)         
  table=readHTMLTable(link)[[3]] %>% mutate(Name=adapat$Name[i])
  parkruns=parkruns %>% rbind.data.frame(table)
  Sys.sleep(10)
}
beepr::beep()

parkruns=parkruns %>% mutate(Time2=case_when(str_length(Time)==5~chron(times=paste("00:",Time, sep="")),
                                             T~chron(times=paste(Time))))
parkruns %>% count(Name, Event) %>% arrange(-n) %>% count(Name)

x=parkruns %>% count(Name, Event) %>% pivot_wider(names_from = "Name", values_from = n) %>% 
  merge(parkruns %>% count(Event))%>% arrange(-n) %>% select(-n)


sheet_write(x, ss="https://docs.google.com/spreadsheets/d/1Bv_LGrOK6leEFV76OlHPsLX8CvM17CmEhQ2_JzZg5YI/edit#gid=0",
            sheet="Parkruns")

y=parkruns %>% group_by(Name, Event) %>% slice_min(Time2, with_ties = F) %>% 
  mutate(Time=substr(as.character(Time2), 9-str_length(Time),9)) %>% 
  dplyr::select(Name, Event, Time) %>% 
  pivot_wider(names_from = "Name", values_from = Time) %>% 
  merge(parkruns %>% count(Event))%>% arrange(-n) %>% select(-n)

sheet_write(y, ss="https://docs.google.com/spreadsheets/d/1Bv_LGrOK6leEFV76OlHPsLX8CvM17CmEhQ2_JzZg5YI/edit#gid=0",
            sheet="PBs")
