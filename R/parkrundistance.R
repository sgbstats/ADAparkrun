library(XML)
library(tidyverse)
library(RCurl)

url="http://www.twentypenguins.co.uk/nerdy/index.php?id=28"

link=getURL(url)         
table=readHTMLTable(link)

events=table$parkrunlist
# names(events)=table$alphabet[1,]

events2=events %>% mutate(Index=as.numeric(Index)) %>% 
  filter(!is.na(Index)) %>% 
  dplyr::select(Index, Parkrun, `Driving distance`) %>% 
  separate(`Driving distance`, c("km", "miles"), sep="km") %>% 
  mutate(km=as.numeric(km),
         miles=km/1.6
         )

names=read.csv("Data/Parkruns.csv", stringsAsFactors = F)
# out=tribble(~Home, ~Parkrun, ~Index,~`Driving distance`, ~`Crow flies distance`)
out=data.frame("Home"=names$Name, "Parkrun"=names$Name, `Driving distance`="0km (0 miles)","Crow flies distance"="0km (0 miles)", Index=0) %>% 
  rename("Driving distance"= "Driving.distance",
         "Crow flies distance"="Crow.flies.distance")

tictoc::tic()
for(i in 1:nrow(names))
{
  url=paste("http://www.twentypenguins.co.uk/nerdy/index.php?id=",names$Num[i],sep="")
  link=getURL(url)         
  table=readHTMLTable(link)
  if(is.null(table$parkrunlist))
  {
    next
  }
  
  events=table$parkrunlist
  events2=events %>% mutate(Index=as.numeric(Index)) %>% 
    filter(!is.na(Index)) %>% 
    dplyr::select(Index, Parkrun, `Driving distance`, `Crow flies distance`) %>% 
    mutate(Home=names$Name[i])
  out=out %>% rbind.data.frame(events2)
  print(names$Name[i])
}
beepr::beep()
tictoc::toc()

Data=out   %>% 
  separate(`Driving distance`, c("km", "miles"), sep="km") %>% 
  mutate(km=as.numeric(km),
         miles=km/1.6
         
  )%>% merge(names %>% dplyr::select(Name, Short), by.x="Parkrun", by.y="Name")%>% 
  arrange(Home, Index)

save(Data,events, file="Data/Data.Rda")
save(Data,names, file="ADAparkrun/Data.Rda")
