#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggvis)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Forest Fires in Brazil"),
    
    # navlist
    navlistPanel(
        "산불",
        tabPanel("히스토그램",
            ggvisOutput("fire_hist"),
            uiOutput("fire_hist_ui")
        ),
        tabPanel(
            "시간별 발생량",
            ggvisOutput("fire_timeplot"),
            uiOutput("fire_timeplot_ui"),
            hr(),
            fluidRow(
                column(5,
                       selectInput("x_axis", "시간 단위 선택", 
                                   choices = list("연도별" = "Year", "월별" = "Month"),
                                   selected = "Year")
                ),
                column(5,
                    checkboxInput("region", "지역별로 보기", value = F),
                    radioButtons("metric", "통계치 선택",
                                 choices = list("합계" = "sum", "평균" = "mean", "중앙값" = "med"), selected = "med")
                )
            )
        ),
        tabPanel("주(州)별 발생량",
            ggvisOutput("fire_state"),
            uiOutput("fire_state_ui"),
            hr(),
            actionButton("bp", "Boxplot"),
            actionButton("map", "Map")
        ),
        "날씨",
        tabPanel("히스토그램",
            ggvisOutput("weather_hist"),
            uiOutput("weather_hist_ui"),
            hr(),
            selectInput("hist_weather", "요소 선택",
                        choices = colnames(fire_weather[6:12]))
        ),
        tabPanel("월별 날씨",
            ggvisOutput("weather_month"),
            uiOutput("weather_month_ui"),
            hr(),
            fluidRow(
                column(5,
                    selectInput("month_weather", "요소 선택",
                                choices = colnames(fire_weather[6:12]),
                                selected = "temp")
                ),
                column(5,
                    checkboxInput("weather_region", "지역별로 보기", value = F)
                )
            )
        ),
        fluid = F, widths = c(3, 7)
    )
))