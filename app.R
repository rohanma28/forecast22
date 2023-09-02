library(shiny)
library(leaflet)
library(rgdal)
library(ggplot2)

senateseats <- readOGR('/data.processed/22senateseats.shp', 
                       layer = "22senateseats")
probs <- read.csv('/data/processed/probs.csv')
wholesenprob <- read.csv('/data/processed/wholesenprob.csv')
hist <- read.csv('/data/processed/hist.csv')
seats <- hist$Var1
freq <- hist$Prop
tracker <- read.csv('/data/processed/tracker.csv')
dates <- seq(from = as.Date("2022-01-01"), by = "days", length.out = nrow(tracker))
DPCT <- tracker$DPCT
RPCT <- tracker$RPCT
if (tracker[nrow(tracker),"MARGIN"] < 0) {
  gbmargin <- paste("R+", substring(tracker[nrow(tracker),"MARGIN"],2,5), sep = "")
} else {
  gbmargin <- paste("D+", substring(tracker[nrow(tracker),"MARGIN"],1,4), sep = "")
}

senateseats <- merge(senateseats, probs, by.x = "STATE", by.y = "STATE")

color <- c("#aa0000", "#d44a4a", "#ff9696", "#aaaaaa", "#9696ff", "#4a4ad4", "#0000aa")
bins <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5)
pal <- colorBin(color, senateseats$COLOR, bins)

labels <- sprintf(
  "<strong>%s</strong>
  <br/> Democratic Probability: %f
  <br/> Republican Probability: %f
  <br/> Rating: %s
  <br/> Margin: %s",
  senateseats$NAME, senateseats$DPROB, senateseats$RPROB, senateseats$RATING, senateseats$MARGINLABEL
) %>% lapply(htmltools::HTML)

dem <- paste("Democrats have a ", wholesenprob[3,2] * 100, "% chance of holding the senate.", sep = "")
rep <- paste("Republicans have a ", wholesenprob[4,2] * 100, "% chance of flipping the senate.", sep = "")
genbalmargin <- paste("Predicted Generic Ballot Margin: ", gbmargin, sep = "")

ui <- fluidPage(
  tags$head(HTML("<title>Rohan Athreya's 2022 United States Election Forecast</title>")),
  titlePanel(h1("Rohan Athreya's 2022 United States Senate Elections Forecast", align = "center")),
  
  sidebarLayout( 
    
    sidebarPanel(
      h2(strong(dem)),
      br(),
      h2(strong(rep)),
      br(),
      
      plotOutput('BarChart'),
      br(),
      h4(strong("Last Updated November 8, 2022 at 12:00 AM"), align = "center"),
      br(),
      h4(strong("This model is final; no more updates will be made."), align = "center"),
      br(),
      h4(strong("Polls sourced from FiveThirtyEight database"), align = "center")
    ),
    
    mainPanel(
      h3(strong("Hover over states to see probabilities"), align = "center"),
      leafletOutput("mymap", width = 1000, height = 600),
      h2(strong(genbalmargin), align = "center"),
      plotOutput('Tracker')
    )
  )
)

server <- function(input, output) {
  output$mymap <- renderLeaflet({
    leaflet(senateseats) %>% 
      fitBounds(-171.5, 15.4, -34.9, 66.3) %>% 
      addTiles() %>% 
      addPolygons(color = "#000000",
                  weight = 1, 
                  opacity = 1, 
                  fill = TRUE, 
                  fillColor = ~pal(COLOR),
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(
                    weight = 3,
                    color = "#ff0000",
                    fillOpacity = 1),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
  })
  
  output$BarChart <- renderPlot({
    ggplot(hist, aes(seats, freq)) + 
           geom_col(fill = ifelse(seats < 50, '#ff0000', '#0000ff')) +
           labs(title = element_text("Probabilities of Senate Outcomes"), align = "center") +
           xlab("Democratic Senate Seats") +
           ylab("Percent Probability")
  })
  
  output$Tracker <- renderPlot({
    ggplot(tracker, aes(x=dates)) +
      geom_line(aes(y = DPCT), color = "#0000ff") + 
      geom_line(aes(y = RPCT), color = "#ff0000") +
      labs(title = element_text("Generic Ballot Polling Average"), align = "center") +
      xlab("Date") +
      ylab("Generic Ballot Numbers in Percentage") +
      ylim(c(30,60))
  })
}

shinyApp(ui = ui, server = server)
