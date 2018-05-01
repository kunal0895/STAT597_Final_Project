library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)

df_by_Continents <- read.csv("Continent_Level_Data.csv", header = TRUE, stringsAsFactors = FALSE)
df_public_debt_country <- read.csv("Country_Level_Data.csv", header = TRUE, stringsAsFactors = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Year", 
                  min = 1996, max = 2016, 
                  value = c(2000, 2005)
      ),
      radioButtons("plotType", "Level",  choices = c("Continent Level", "Country Level"), selected = "Continent Level"),
      selectInput(
        inputId = "region",
        label = "Region",
        choices = c(
          'All',
          'Africa',
          'Asia',
          'Central America/Caribbean',
          'Europe',
          'Middle East',
          'North America',
          'Oceania',
          'South America',
          'Southeast Asia'
        ),
        selected = "All"
      ),
      
      selectInput(
        inputId = "country",
        label = "Country",
        choices = c(
          'None',
          'Angola',
          'Armenia',
          'Australia',
          'Azerbaijan',
          'Bangladesh',
          'Belize',
          'Brazil',
          'Canada',
          'Chile',
          'Colombia',
          'Croatia',
          'Denmark',
          'Ecuador',
          'Equatorial Guinea',
          'Eritrea',
          'Estonia',
          'Fiji',
          'Finland',
          'France',
          'Germany',
          'Greece',
          'Honduras',
          'India',
          'Indonesia',
          'Iran',
          'Ireland',
          'Italy',
          'Japan',
          'Kazakhstan',
          'Kenya',
          'Kuwait',
          'Lithuania',
          'Malawi',
          'Malaysia',
          'Malta',
          'Mexico',
          'Mozambique',
          'New Zealand',
          'Nigeria',
          'Norway',
          'Oman',
          'Panama',
          'Papua New Guinea',
          'Philippines',
          'Russia',
          'Saudi Arabia',
          'Seychelles',
          'Sierra Leone',
          'South Africa',
          'Sri Lanka',
          'Sweden',
          'Tanzania',
          'Thailand',
          'Tunisia',
          'Turkey',
          'UK',
          'United Arab Emirates',
          'USA',
          'Venezuela',
          'Vietnam',
          'Yemen'
        ),
        selected = "None"
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("main_plot"),
      tableOutput("results"))
  ),
  titlePanel(" ")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #df_by_Continents <- read.csv("Continent_Level_Data.csv")
  #df_public_debt_country <- read.csv("Country_Level_Data.csv")
  #reduced_df <- reactive({
    #Continents <- readr::read_csv("~/Project/FinalProject/STAT597_Final_Project/Continent_Level_Data.csv")
    #country <- readr::read.csv("Country_Level_Data.csv")
  #  filter(
  #    df_public_debt_country, 
  #    Country == input$state, 
  #    year >= paste('X',input$yearInput[1], sep="") & year <= paste('X',input$yearInput[2], sep="")
  #  )
  #})
  
  output$main_plot <- renderPlot({
    
    #df_by_Continents <- reduced_df()$Continents
    
    sRegion <- input$region
    plot_type <- input$plotType
    if(plot_type == "Continent Level")
    {
      if(sRegion == "All")
      {
        ggplot(data = filter(
          df_by_Continents,
          year >= paste('X',input$yearInput[1], sep="") & year <= paste('X',input$yearInput[2], sep="")
        ), 
        aes(year, Total, 
            color = Region, 
            group = Region)) + geom_point() + geom_line()
      }
      else
      {
        ggplot(data = filter(
          df_by_Continents, 
          Region == input$region, 
          year >= paste('X',input$yearInput[1], sep="") & year <= paste('X',input$yearInput[2], sep="")
        ), 
        aes(year, Total, colour = Region, group = Region)) + geom_point() +
          geom_line() + ggtitle(input$nameInput)
      }
    }
    
    else if(plot_type == "Country Level") {
      
      ggplot(data = filter(
        df_public_debt_country, 
        Country == input$country, 
        year >= paste('X',input$yearInput[1], sep="") & year <= paste('X',input$yearInput[2], sep="")
      ), 
      aes(year, Debt_as_pct_of_GDP, colour = Country, group = Country)) + geom_point() +
        geom_line() + ggtitle(input$nameInput)
      
    }
    
    
  })
  output$results <- renderTable({
    plot_type = input$plotType
    Region = input$region
    Country = input$country
    if(plot_type == "Continent Level") 
    {
      if(Region != "All")
      {
        filter(
          df_by_Continents, 
          Region == input$region, 
          year >= paste('X',input$yearInput[1], sep="") & year <= paste('X',input$yearInput[2], sep="")
        )
      }
      
      else
      {
        filter(
          df_by_Continents,
          year >= paste('X',input$yearInput[1], sep="") & year <= paste('X',input$yearInput[2], sep="")
        )
      }
    }
    
    else if(plot_type == "Country Level")
    {
      if(Country != "None")
      {
        filter(
          df_public_debt_country,
          Country == input$country,
          year >= paste('X',input$yearInput[1], sep="") & year <= paste('X',input$yearInput[2], sep="")
        )
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)