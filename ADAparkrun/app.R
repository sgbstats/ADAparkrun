
library(shiny)
library(XML)
library(tidyverse)
library(RCurl)
library(shinycssloaders)
library(googlesheets4)
library(RJSONIO)
load("Data.Rda")
load("adapat.Rda")
`%notin%`=Negate(`%in%`)
# options(gargle_oauth_cache = ".secrets")
# gs4_auth(
#   cache = ".secrets",
#   email = "sebastiangbate@gmail.com"
# )
# parkruns=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Bv_LGrOK6leEFV76OlHPsLX8CvM17CmEhQ2_JzZg5YI/edit#gid=0",
#                                    sheet="Parkrunlist")




ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-size:20px;
        font-weight: bold;
      }
    "))
  ),
  tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
  # Application title
  titlePanel("Mutual NENDY"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("adapat", 
                  "ADAPAT members", 
                  choices = c("Adam"= 7232608,
                              "Alex"= 4087050,
                              "Andy"= 7402459,
                              "Ella"= 7470572,
                              "Frankie"= 4458732,
                              "Grace"=7554791,
                              "Jonny"= 593256,
                              "Katie"= 7526532,
                              "Leah"= 5700545,
                              "Luke"=2407768,
                              "Max"=582473,
                              "Phil"= 7266141,
                              "Rachel"= 1539187,
                              "Rob"= 5349926,
                              "Seb"= 493595,
                              "Tom A"= 5243446,
                              "Tom C"= 1939393
                  ),
                  multiple = T
                  
      ),
      selectInput("parkrun", "Home parkrun", choices = names$Short , selected = "South Manchester"),
      numericInput("distance", "Maximum Travel Distance (miles)",value=20,min = 0),
      actionButton("go", "Go")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # dataTableOutput("test")
      # textOutput("test2"),
      dataTableOutput("mnendy")%>% withSpinner(color="#0dc5c1")
    )
  )
)

parkruns0=fromJSON("https://adap.at/api/parkrun")
parkruns=tribble(~Event,~`Run Date`,~`Run Number`,~Pos,~Time,~AgeGrade,~`PB?`, ~Barcode)
for(i in 1:nrow(adapat))
{
  x=data.frame(matrix(unlist(parkruns0[[i]]$data$results), nrow=length(parkruns0[[i]]$data$results), byrow=TRUE),stringsAsFactors=FALSE) %>% 
    mutate(Barcode=as.numeric(parkruns0[[i]]$barcode)) %>% 
    rename("Event"="X1","Run Date"="X2","Run Number"="X3","Pos"="X4","Time"="X5","AgeGrade"="X6","PB?"="X7")
  parkruns=parkruns %>% rbind.data.frame(x)
}
server <- function(input, output) {
  
  prdata=eventReactive(input$go, {
    validate(need(length(input$adapat)>0, "Pick someone" ))
    
    #this currently needs to be commented out as the web scraper isn't working yet
    
    # adapat2=data.frame("Barcode"=input$adapat) %>% merge(adapat, by="Barcode")
    # parkruns=tribble(~Name,~Event,~`Run Date`,~`Run Number`,~Pos,~Time,~AgeGrade,~`PB?`)
    # 
    # for(i in 1:nrow(adapat2))
    # {
    #   url=adapat2$url[i]
    #   link=getURL(url)
    #   table=readHTMLTable(link)[[3]] %>% mutate(Name=adapat2$Name[i],
    #                                             Barcode=adapat2$Barcode[i])
    #   parkruns=parkruns %>% rbind.data.frame(table)
    #   # Sys.sleep(5)
    # }
    
    # adapat2
    # parkruns
    
    
    parkruns %>%
      filter(Barcode %in% input$adapat) %>%
      dplyr::select(Event)  %>% unlist()
  })
  
  output$mnendy=renderDataTable({
    `%notin%`=Negate(`%in%`)
    validate(need(length(input$adapat)>0, "" ))
    Data %>%
      merge(names %>% dplyr::select(Name, Short), by.x="Home", by.y="Name") %>%
      filter(
        Short.y==input$parkrun,
        Short.x %notin% prdata(),
        miles<=input$distance
      ) %>%
      # %>% 
      #   mutate(check=Short.x %notin% prdata())
      dplyr::select(Short.x, miles, km) %>% 
      rename("Parkrun"="Short.x") %>% 
      mutate(miles=round(miles,1),
             km=round(km,1))
    
  })
  
  output$test=renderDataTable({
    parkruns
  })
  
  output$test2=renderText({
    url=adapat$url[15]
    link=getURL(url)
    link
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
