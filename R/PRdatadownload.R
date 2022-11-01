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
# library(shinyjs)
options(gargle_oauth_cache = ".secrets")
gs4_auth(
  cache = ".secrets",
  email = "sebastiangbate@gmail.com"
)
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


parkruns=tribble(~Name,~Barcode,~Event,~`Run Date`,~`Run Number`,~Pos,~Time,~AgeGrade,~`PB?`)

for(i in 1:nrow(adapat))
{
  url=adapat$url[i]
  link=getURL(url)         
  table=readHTMLTable(link)[[3]] %>% mutate(Name=adapat$Name[i],
                                            Barcode=adapat$Barcode[i])
  parkruns=parkruns %>% rbind.data.frame(table)
  Sys.sleep(10)
}
beepr::beep()

parkruns=parkruns %>% mutate(Time2=case_when(str_length(Time)==5~chron(times=paste("00:",Time, sep="")),
                                             T~chron(times=paste(Time))))
parkruns %>% count(Event) %>% arrange(-n) 

x=parkruns %>% count(Name, Event) %>% pivot_wider(
  names_from = "Name", values_from = n) %>% 
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


parkruns2=parkruns %>% group_by(Event, Name, Barcode) %>% slice_min(`Run Date`, with_ties = F) %>% ungroup() %>% dplyr::select(Event, Name, Barcode)
sheet_write(parkruns2, ss="https://docs.google.com/spreadsheets/d/1Bv_LGrOK6leEFV76OlHPsLX8CvM17CmEhQ2_JzZg5YI/edit#gid=0",
            sheet="Parkrunlist")

save(adapat, file="ADAparkrun/adapat.Rda" )

save(parkruns, parkruns2, file="Data/Cache.Rda")

library(rvest)
library(httr)
i=15

# url=adapat$url[i]
# download.file(url, destfile = "junk/scrapedpage.html", quiet=TRUE)
rvest::read_html(url) %>% curl::curl(handle = curl::new_handle("useragent"="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/106.0.0.0 Safari/537.36"))
# 
# session(url)
con=curl::curl(url)
# 
# %>% read_html()
