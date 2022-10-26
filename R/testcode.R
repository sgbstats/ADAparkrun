library(XML)
library(tidyverse)
library(RCurl)
library(readxl)
library(openxlsx)
library(beepr)
library(chron)
library(tibble)

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
               "Tom A", 5243446
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

parkruns %>% count(Event) %>% arrange(-n)

x=parkruns %>% count(Name, Event) %>% pivot_wider(names_from = "Name", values_from = n) %>% 
  merge(parkruns %>% count(Event))%>% arrange(-n) %>% select(-n)

