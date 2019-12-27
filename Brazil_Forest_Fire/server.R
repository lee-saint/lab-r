#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(brazilmaps)
library(sf)
library(rgeos)
library(maptools)
library(anytime)
library(dplyr)
library(readxl)
library(ggvis)

# 주별 브라질 지도 데이터
brmap <- get_brmap(geo = "State", class = "data.frame")

# state id 데이터프레임 만들기
state_id <- data.frame(state = unique(fire_weather$State),
                       id = as.character(c(12, 27, 16, 13, 29, 23, 53, 32, 52, 21, 51, 50, 31, 15, 25, 41, 26, 22, 33, 24, 43, 11, 14, 42, 35, 28, 17)),
                       stringsAsFactors = F)

# brmap에 주별 산불 발생 중간값을 구해서 조인
fire_med <- fire_weather %>% group_by(State) %>% summarise(med = median(Number))
fire_med <- fire_med %>% left_join(state_id, by = c("State" = "state"))
brmap <- brmap %>% left_join(fire_med, by = "id")

# 연도/월별 산불 발생 중앙값
fire_year <- fire_weather %>% group_by(Year) %>% summarise(sum = sum(Number), mean = mean(Number), med = median(Number))
fire_month <- fire_weather %>% group_by(Month) %>% summarise(sum = sum(Number), mean = mean(Number), med = median(Number))
fire_region_year <- fire_weather %>% group_by(Year, Region) %>% summarise(sum = sum(Number), mean = mean(Number), med = median(Number))
fire_region_month <- fire_weather %>% group_by(Month, Region) %>% summarise(sum = sum(Number), mean = mean(Number), med = median(Number))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  fire_weather %>% ggvis(~Number) %>% layer_histograms() %>% add_axis("x", title = "Fire") %>% add_axis("y", title_offset = 50) %>% bind_shiny("fire_hist", "fire_hist_ui")


  plotdata_firetime <- reactive({
    if(input$x_axis == "Year" & input$region == F)
      df <- fire_year %>% select(x = Year, y = input$metric)
    else if(input$region == F)
      df <- fire_month %>% select(x = Month, y = input$metric)
    else if(input$x_axis == "Year")
      df <- fire_region_year %>% select(x = Year, y = input$metric, Region)
    else
      df <- fire_region_month %>% select(x = Month, y = input$metric, Region)
    # df <- ifelse(input$x_axis == "Year", select(fire_year, x = Year, y = input$metric), select(fire_month, x = Month, y = input$metric))
    df
  })
  # x_ax <- reactive(input$x_axis)
  # if(x_ax() == "Year")
  #   plotdata_firetime <- reactive({
  #     df <- fire_year[c("Year", input$metric)]
  #     names(df) <- c("x", "y")
  #     df
  #   })
  # else
  #   plotdata_firetime <- reactive({
  #     df <- fire_year[c("Year", input$metric)]
  #     names(df) <- c("x", "y")
  #     df
  #   })

    observe({
      if(input$region == F)
        plotdata_firetime() %>% ggvis(x = ~x, y = ~y) %>% layer_lines() %>% 
        add_axis("x", title = input$x_axis) %>% add_axis("y", title = input$metric, title_offset = 60) %>% bind_shiny("fire_timeplot", "fire_timeplot_ui")
      else
        plotdata_firetime() %>% ggvis(x = ~x, y = ~y, stroke = ~Region) %>% layer_lines() %>% 
        add_axis("x", title = input$x_axis) %>% add_axis("y", title = input$metric, title_offset = 55) %>% bind_shiny("fire_timeplot", "fire_timeplot_ui")
      # if (input$x_axis == "Year" & input$metric == "sum")
      #   fire_year %>% ggvis(~Year, ~sum) %>% layer_lines() %>% scale_numeric("x", nice = T) %>% bind_shiny("fire_timeplot", "fire_timeplot_ui")
      # else if (input$x_axis == "Year" & input$metric == "mean")
      #   fire_year %>% ggvis(~Year, ~mean) %>% layer_lines() %>% scale_numeric("x", nice = T) %>% bind_shiny("fire_timeplot", "fire_timeplot_ui")
      # else if (input$x_axis == "Year")
      #   fire_year %>% ggvis(~Year, ~med) %>% layer_lines() %>% scale_numeric("x", nice = T) %>% bind_shiny("fire_timeplot", "fire_timeplot_ui")
      # else if (input$x_axis == "Month" & input$metric == "sum")
      #   fire_month %>% ggvis(~Month, ~sum) %>% layer_lines() %>% bind_shiny("fire_timeplot", "fire_timeplot_ui")
      # else if (input$x_axis == "Month" & input$metric == "mean")
      #   fire_month %>% ggvis(~Month, ~mean) %>% layer_lines() %>% bind_shiny("fire_timeplot", "fire_timeplot_ui")
      # else if (input$x_axis == "Month")
      #   fire_month %>% ggvis(~Month, ~med) %>% layer_lines() %>% bind_shiny("fire_timeplot", "fire_timeplot_ui")
    })
    
    
    fire_weather %>% ggvis(~State, ~Number, fill = ~Region) %>% layer_boxplots() %>% 
      add_axis("x", title_offset = 100, properties = axis_props(labels = list(angle = 90, align = "left"))) %>% 
      add_axis("y", title = "Fire", title_offset = 55) %>% bind_shiny("fire_state", "fire_state_ui")
    observeEvent(input$bp, {
      fire_weather %>% ggvis(~State, ~Number, fill = ~Region) %>% layer_boxplots() %>% 
        add_axis("x", title_offset = 100, properties = axis_props(labels = list(angle = 90, align = "left"))) %>% 
        add_axis("y", title = "Fire", title_offset = 55) %>% bind_shiny("fire_state", "fire_state_ui")
    })
    
    fire_value <- function(x) {
      if(is.null(x)) return(NULL)
      df <- fire_med %>% filter(id == x$id) %>% select(1:2)
      paste0(as.character(df[1, 1]), ": ", df[1, 2], collapse="<br />")
    }
    
    observeEvent(input$map, {
      brmap %>% group_by(id, group) %>% ggvis(~long, ~lat) %>% layer_paths(fill = ~med, strokeOpacity:=0.5) %>% set_options(width = 600, height = 400, keep_aspect = T) %>% 
        scale_numeric("fill", range=c("white", "darkred")) %>% add_legend("fill", title = "Monthly Reported Forest Fire") %>% hide_axis("x") %>% hide_axis("y") %>% 
        add_tooltip(fire_value, "hover") %>% bind_shiny("fire_state", "fire_state_ui")
    })
    
    plotdata <- reactive({
      df <- fire_weather[input$hist_weather]
      names(df) <- "y"
      df
    })
    observe({
      plotdata() %>% ggvis(~y) %>% layer_histograms() %>% add_axis("y", title_offset = 40) %>% add_axis("x", title = input$hist_weather) %>% bind_shiny("weather_hist", "weather_hist_ui")
    })
    
    
    
    plotdata2 <- reactive({
      df <- fire_weather[c("Month", input$month_weather, "Region")]
      names(df) <- c("Month", "y", "Region")
      df
    })
    # weather_month <- reactive(fire_weather[c(3, 6:12)] %>% group_by(Month) %>% summarise(med = median(input$month_weather)))
    observe({
      if(input$weather_region == F)
        plotdata2() %>% group_by(Month) %>% summarise(med = median(y)) %>% ggvis(~Month, ~med) %>% layer_lines() %>% bind_shiny("weather_month", "weather_month_ui")
      else
        plotdata2() %>% group_by(Month, Region) %>% summarise(med = median(y)) %>% ggvis(~Month, ~med, stroke = ~Region) %>% layer_lines() %>% bind_shiny("weather_month", "weather_month_ui")
    })
})
