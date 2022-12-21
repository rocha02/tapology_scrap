library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(htmlwidgets)
library(tidyverse)
library(reactable)
library(plotly)
library(wesanderson)

events <- readRDS("events.rds")
cards <- readRDS("cards.rds")

card_options <- names(cards[, 6:10])
  
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Tapology Champion Dashboard", titleWidth = 350),
                    dashboardSidebar(sidebarMenu(
                      menuItem("Score", tabName = "score", icon = icon("fas fa-trophy")),
                      menuItem("Card Info", tabName = "cards", icon = icon("fas fa-list-alt")),
                      menuItem("Source Code", icon = icon("fab fa-github-square"), 
                               href = "https://github.com/rocha02", newtab = TRUE))
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "score",
                              fluidRow(
                                valueBoxOutput("vbox1", width = 4),
                                valueBoxOutput("vbox2", width = 4),
                                valueBoxOutput("vbox3", width = 4)),
                              fluidRow(
                                box(width = 10, awesomeCheckboxGroup("month", "Month:", 
                                                           choices = unique(events$Month), 
                                                           selected = "December",
                                                           inline = TRUE)),
                                box(width = 2, awesomeCheckboxGroup("year", "Year:", 
                                                                    choices = unique(events$Year), 
                                                                    selected = "2022", 
                                                                    inline = TRUE))
                              ),
                                fluidRow(
                                  box(width = 12,
                                    title = "Tapology Scores", reactableOutput("ranking"))
                                  )
                              ),
                        tabItem(tabName = "cards",
                                fluidRow(
                                  box(width = 8, height = 104, awesomeCheckboxGroup("month_card", "Choose a range:", 
                                                                  choices = unique(events$Month), 
                                                                  selected = "July",
                                                                  inline = TRUE)),
                                  box(width = 2, height = 104, awesomeCheckboxGroup("year_card", "Year:", 
                                                                      choices = unique(events$Year), 
                                                                      selected = "2022", 
                                                                      inline = TRUE)),
                                  box(width = 2, selectInput("data", "Select Data:", choices = card_options))
                                  ),
                                  fluidRow(
                                    box(width = 12, height = 800,
                                      title = "Tapology Champion Cards", plotlyOutput("cards_plot", height = 700))
                                )
                        )
                      )
                    )
                    
    )

server <- function(input, output, session) {
  
  output$vbox1 <- renderValueBox({
    df1 <- events %>% 
      filter(Picks > 1) %>% 
      filter(Month == "November") %>% 
      filter(Year == "2022") %>% 
      #filter(Year %in% input$year) %>% 
      group_by(Member) %>% 
      summarise(Points= sum(Points)) %>% 
      arrange(desc(Points)) %>% 
      head(1)
    
    valueBox(
      paste0("Current Champ:", df1$Member), 
      subtitle = "Member with most points in the last month", 
      icon = icon("fas fa-trophy"),
      color = "teal"
    )
    
  })
  
  output$vbox2 <- renderValueBox({
    df2 <- events %>% 
      filter(Month %in% input$month) %>% 
      filter(Year %in% input$year) %>% 
      group_by(Member) %>% 
      summarise(Points= sum(Points), 
                Picks= sum(Picks),
                Correct= sum(Correct),
                median_points_per_pick = round((Points/Picks),1),
                Correct = round((Correct/Picks),3)) %>%
      arrange(desc(Correct)) %>% 
      filter(Picks > 9) %>% 
      head(1)
    
    valueBox(
      paste0(df2$Member, ": ", df2$Correct*100, "%"), 
      subtitle = "Best Correct % in selected period (with 10 picks or more)", 
      icon = icon("fas fa-check-circle"),
      color = "aqua"
    )
    
  })
    
    output$vbox3 <- renderValueBox({
      df3 <- events %>% 
        filter(Month %in% input$month) %>% 
        filter(Year %in% input$year) %>% 
        group_by(Member) %>% 
        summarise(Points= sum(Points), 
                  Picks= sum(Picks),
                  Perfect= sum(Perfect),
                  Correct= sum(Correct),
                  median_points_per_pick = round((Points/Picks),1),
                  Correct = round((Correct/Picks),3)) %>%
        arrange(desc(Perfect)) %>% 
        head(1)
      
      valueBox(
        paste0(df3$Member, ": ", df3$Perfect," Perfect Picks"), 
        subtitle = "Most Perfect Picks in selected period", 
        icon = icon("fas fa-bullseye"),
        color = "light-blue"
      )
    
  })
  
  
  output$ranking <- renderReactable({
    events %>% 
      dplyr::filter(Picks > 1) %>% 
      dplyr::filter(Month %in% input$month) %>% 
      dplyr::filter(Year %in% input$year) %>% 
      group_by(Member) %>% 
      summarise(Points= sum(Points), 
                Picks= sum(Picks),
                Correct= sum(Correct),
                median_points_per_pick = round((Points/Picks),1),
                Correct = round((Correct/Picks),3)) %>%
      arrange(desc(Points)) %>%
      mutate(Rank = rank(desc(Points), ties.method="min")) %>% 
      data.table::setcolorder(c("Rank", "Member", "Points", "Picks", "median_points_per_pick", "Correct")) %>% 
      reactable(
        striped = TRUE,
        highlight = TRUE,
        defaultPageSize = 15,
        defaultColDef = colDef(
          header = function(value) gsub(".", " ", value, fixed = TRUE),
          align = "center",
          headerStyle = list(background = "#f7f7f8")
        ),
        columns = list(
          Rank = colDef(name = "Rank", minWidth = 100),
          Member = colDef(name = "Member"),
          Correct = colDef(name = "Correct %", format = colFormat(percent = TRUE,  digits = 1)),
          median_points_per_pick = colDef(name = "Point average per pick")),
        theme = reactableTheme(
          borderColor = "#dfe2e5",
          stripedColor = "#f6f8fa",
          highlightColor = "#f0f5f9",
          cellPadding = "8px 12px",
          style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
          searchInputStyle = list(width = "100%")))
    
  })
  
output$cards_plot <- renderPlotly({
    
  p <- cards %>% 
    dplyr::filter(Month %in% input$month_card) %>% 
    dplyr::filter(Year %in% input$year_card) %>% 
    ggplot(aes_string(x="Event", y= input$data)) + 
    geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
    scale_x_discrete(labels = str_wrap(cards$Event, width = 14))+
    scale_fill_manual(values = wes_palette("Royal1"))+
    theme(axis.title.x=element_blank(),
          text = element_text(size = 12))+
    coord_flip()
    
ggplotly(p, tooltip = "y")
    
  })
  
}

shinyApp(ui, server)