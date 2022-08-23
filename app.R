library(shiny)
library(tidyverse)
library(plotly)
library(DT)


# Data sets
honey <- read.csv(here::here("data/honeyproduction.csv"))

price <- honey %>%
  group_by(state) %>%
  mutate(ave_price = mean(priceperlb))

value <- honey %>%
  group_by(state) %>%
  summarise(ave_value = mean(prodvalue)) %>%
  arrange(desc(ave_value)) %>%
  head() %>%
  mutate(order = fct_reorder(state, ave_value))

# UI
ui <- fluidPage(
  h2("Honey Production in USA"),
  h3("Honey production changing trend by year of USA"),
  fluidRow(
    column(8,
      offset = 1
    ),
  ),
  sidebarLayout(
    mainPanel(
      plotlyOutput("productionchart")
    ),
    sidebarPanel(
      sliderInput("year", "Year",
        min = 1998,
        max = 2012,
        value = c(1998, 2012),
        sep = ""
      )
    )
  ),
  h3("Average price of honey in every state"),
  fluidRow(
    sidebarLayout(
      mainPanel(
        plotlyOutput("pricechart")
      ),
      sidebarPanel(
        selectizeInput("meanprice",
          "state choice:",
          choices = c(
            "AL", "AZ", "AR", "CA", "CO", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY",
            "LA", "ME", "MD", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NJ", "NM", "NY", "NC",
            "ND", "OH", "OK", "OR", "PA", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
          ),
          multiple = TRUE,
          selected = c("AL", "AZ", "AR", "CA", "CO", "FL", "GA")
        )
      )
    ),
    h3("Top 6 states with highest value of honey"),
    mainPanel(plotlyOutput("valuechart"),
      width = "100%"
    ),
    h3("Original data table"),
    fluidRow(
      column(8, offset = 1),
    ),
    dataTableOutput("tabledata")
  ),
  fluidRow(
    column(
      10,
      div(
        class = "about",
        uiOutput("about")
      )
    )
  ),
  includeCSS("styles.css")
)


# Server
server <- function(input, output) {
  min <- reactive({
    input$year[1]
  })
  max <- reactive({
    input$year[2]
  })

  year_input <- reactive({
    honey %>% filter(
      year >= min(),
      year <= max()
    )
  })

  output$productionchart <- renderPlotly({
    production <- year_input() %>%
      ggplot(honey,
        mapping = aes(
          x = year,
          y = log10(totalprod),
          color = state
        )
      ) +
      geom_line() +
      theme_bw(base_size = 14) +
      scale_x_continuous(
        breaks = c(
          1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
          2006, 2007, 2008, 2009, 2010, 2011, 2012
        )
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
      labs(x = "Year", y = "log(Production)")
  })


  statename <- reactive({
    price %>% filter(state %in% input$meanprice)
  })

  output$pricechart <- renderPlotly({
    mean <- statename() %>%
      ggplot(price,
        mapping = aes(
          x = state,
          y = ave_price,
          fill = state
        )
      ) +
      geom_col() +
      geom_text(aes(label = round(ave_price, digits = 3))) +
      theme_bw(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
      labs(x = "State", y = "Average Price")
  })

  output$valuechart <- renderPlotly({
    topsix <- value %>%
      ggplot(aes(
        x = order,
        y = ave_value,
        fill = state
      )) +
      geom_col() +
      coord_flip() +
      theme_bw(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
      labs(x = "Average Value", y = "State")
  })

  output$tabledata <- renderDataTable({
    tabledata <- honey
    DT::datatable(tabledata)
  })


  output$about <- renderUI({
    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()
  })
}

shinyApp(ui = ui, server = server)
