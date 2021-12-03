#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
# library(rvest)
# library(readxl)
library(dplyr)
library(maps)
# library(reshape2)
#install.packages("ggiraph")
library(ggiraph)
library(RColorBrewer)
 library(geojsonio)
#install.packages("plotly")
#library(plotly)
#install.packages("shinyWidgets")

#library(shinyWidgets)
#install.packages("shinydashboard")
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(reshape2)
library(ggplot2)

library(tidyverse)
#install.packages("vroom")
library(vroom)
# library(sp)
library(sf)
# library(tigris)
library(leaflet)
library(htmlwidgets)
library(shiny)
library(shinythemes)
library(htmltools)


# setwd("E:/ResearchWork2/Shinyapp/app")
covid_time_on <- read.csv("covid_time_on.csv",sep=",",header=TRUE)
covid_time_on$date_report <- as.Date(covid_time_on$date_report,format = "%Y-%m-%d")
covid_biweekly_cumgeo <- readRDS("covid_biweekly_cumgeo.RDS")
colnames(covid_biweekly_cumgeo)[12] <- "Age_Adjusted_Rate_Biweekly"
covid_cum_biweeklyb_geo <- readRDS("covid_cum_biweekly_geo.RDS")
temporal_cum <- read.csv("temporal_model9.csv",sep=",",header =TRUE)
temporal_biweekly <- read.csv("temporal_model_biweekly.csv",sep=",",header=TRUE)
temporal_cum$Date <- as.Date(temporal_cum$Date,format = "%Y-%m-%d")
temporal_biweekly$Date <- as.Date(temporal_biweekly$Date,format = "%Y-%m-%d")
atapoisson <- readRDS("alldata_final2.RDS")
atapoisson <- atapoisson[order(atapoisson$Date),]
atppoisson <- readRDS("alldata_pointfinal2.RDS")
atppoisson <- atppoisson[order(atppoisson$Date),]
# set mapping colour
covid_col = "#cc4c02"
covid_other_col = "#662506"




### MAP FUNCTIONS ###
# function to plot cumulative COVID cases by date
cumulative_plot = function(on_timeseries){
    g1 = ggplot(on_timeseries, aes(x = date_report, y = cumulative_cases)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
        ylab("Cumulative Cases") +  xlab("Date") + theme_bw() + 
        scale_colour_manual(values=c(covid_col)) +
        scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "T")}) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
              plot.margin = margin(5, 12, 5, 5))
    g1
}

# function to plot new COVID cases by date
new_cases_plot = function(on_timeseries){
    g2 = ggplot(on_timeseries, aes(x = date_report, y = cases_7day)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
        # geom_bar(position="stack", stat="identity") + 
        ylab("Daily New cases") + xlab("Date") + theme_bw() + 
        scale_colour_manual(values=c(covid_col)) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
              plot.margin = margin(5, 12, 5, 5))
    g2
}

#function to plot time effect
temporal_effect_plot = function(melt_temporal){
    g3 = ggplot(melt_temporal, aes(x=Date,y=value,group=type,color=type,shape=type)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
        # geom_bar(position="stack", stat="identity") + 
        ylab("Temporal Effect") + xlab("Date") + theme_bw() + 
        scale_colour_manual(values=c(covid_col,covid_other_col)) +
        theme(legend.title = element_blank(), legend.position = c(0.135,0.795), plot.title = element_text(size=10), 
              plot.margin = margin(5, 12, 5, 5))
    g3
    
}

ui <- bootstrapPage(
    tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">COVID-19 Tracking Map</a>'), id="nav",
               windowTitle = "COVID-19 Tracking Map",
               tabPanel("COVID-19 Map (Age-Adjusted Infection Rate)",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            
                            fluidRow(
                                #span(tags$i(h6("All data metrics are aggregated by month (categorized by biweeks ending in date).<br/> Age Adjusted Rate indicates the infected cases per 100,000 people <br/> which can make fairer comparions between groups with different age distributions.")), style="color:#045a8d"),
                                sidebarPanel(
                                    span(tags$i(h6("All data metrics are aggregated by public health units (computed by biweeks ending in date). Age Adjusted Rate indicates the infected cases per 100,000 people which can make fairer comparions between groups with different age distributions.")), style="color:#045a8d"),
                                             width = 3,
                                             selectInput("Date",
                                                         "Select a date in (Biweeks ending in):",
                                                          choices = unique(covid_biweekly_cumgeo$Date.x))
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Age-Adjusted Rate (Cumulative)",leafletOutput("Age_Adjusted_Rate_Cumulative",height="80vh")),
                                    tabPanel("Age-Adjusted Rate (Biweekly)", leafletOutput("Age_Adjusted_Rate_Biweekly",height="80vh"))
                                )),
                            ),
                            
                            
                            
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 420, left = 0, width = 250,height="auto",fixed=TRUE,
                                          draggable = TRUE,
                                          
                                          #span(tags$i(h6("Reported cases are subject to significant variation in testing policy")), style="color:#045a8d"),
                                          #h3(textOutput("reactive_case_count"), align = "right"),
                                          #h4(textOutput("reactive_death_count"), align = "right"),
                                          #h6(textOutput("clean_date_reactive"), align = "right"),
                                          #h6(textOutput("reactive_country_count"), align = "right"),
                                          plotOutput("epi_curve", height="130px", width="100%"),
                                          plotOutput("cumulative_plot", height="130px", width="100%"),
                                          
                                          # sliderTextInput("plot_date",
                                          #                 label = h5("Select mapping date"),
                                          #                 choices = format(unique(covid_time_on$date_report)),
                                          #                 #selected = format(current_date, "%d %b %y"),
                                          #                 #grid = FALSE,
                                          #                 #animate=animationOptions(interval = 3000, loop = FALSE)
                                          # )
                                          
                            ))),
               tabPanel("Estimaed Risk Map (Bayesian Spatial-temporal Model)",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            fluidRow(
                                sidebarPanel(
                                    span(tags$i(h6("All data metrics are aggregated by public health units (computed by biweeks ending in date). Bayesian Spatial-temporal is proposed to estimate infected risk and the time effect can represent the influence of time on the risk.")),style="color:#045a8d"),
                                    width = 3,
                                    selectInput("Date1",
                                                "Select a date in (Biweeks ending in):",
                                                choices = unique(covid_cum_biweeklyb_geo$Date.x))
                                ),
                                mainPanel(
                                    tabsetPanel(
                                        tabPanel("Bayesian Spatial-temporal Model (Cumulative)",leafletOutput("estimated_infected_risk_Cumulative",height="80vh")),
                                        tabPanel("Bayesian Spatial-temporal Model (Biweekly)", leafletOutput("estimated_infected_risk_Biweekly",height="80vh"))
                                    )),
                                    
                                ),
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 420, left = 0, width = 300,height="auto",fixed=TRUE,
                                          draggable = TRUE,
                                          
                                          #span(tags$i(h6("Reported cases are subject to significant variation in testing policy")), style="color:#045a8d"),
                                          #h3(textOutput("reactive_case_count"), align = "right"),
                                          #h4(textOutput("reactive_death_count"), align = "right"),
                                          #h6(textOutput("clean_date_reactive"), align = "right"),
                                          #h6(textOutput("reactive_country_count"), align = "right"),
                                          plotOutput("temporal_biweekly", height="130px", width="100%"),
                                          plotOutput("temporal_cumulative", height="130px", width="100%"),
                                          
                                          # sliderTextInput("plot_date",
                                          #                 label = h5("Select mapping date"),
                                          #                 choices = format(unique(covid_time_on$date_report)),
                                          #                 #selected = format(current_date, "%d %b %y"),
                                          #                 #grid = FALSE,
                                          #                 #animate=animationOptions(interval = 3000, loop = FALSE)
                                          # )
                                          
                            )
                            
                            )),
               tabPanel("Estimaed Risk Map (ATA Poisson Kriging Model)",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            fluidRow(
                                sidebarPanel(
                                    span(tags$i(h6("All data metrics are aggregated by public health units (computed by biweeks ending in date). Area-to-area Poisson Kriging Model is proposed to estimate infected risk for each unit in a specific period.")),style="color:#045a8d"),
                                    width = 3,
                                    selectInput("Date2",
                                                "Select a date in (Biweeks ending in):",
                                                choices = unique(atapoisson$Date))
                                    
                                ),
                                mainPanel(leafletOutput("reg.est",height="80vh"))
                            ))),
               tabPanel("Estimaed Risk Map (ATP Poisson Kriging Model)",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            fluidRow(
                                sidebarPanel(
                                    span(tags$i(h6("All data metrics are aggregated by public health units (computed by biweeks ending in date). Area-to-point Poisson Kriging Model is proposed to track how the virus spread in Ontario in a specific period.")),style="color:#045a8d"),
                                    width = 3,
                                    selectInput("Date3",
                                                "Select a date in (Biweeks ending in):",
                                                choices = unique(atppoisson$Date))
                                ),
                                mainPanel(leafletOutput("local_risk",height="80vh"))
                            ))),
               tabPanel("Statistical Analysis",
                        htmltools::includeMarkdown("statistical.md")),
               tabPanel("About this site",
                        tags$div(
                            tags$h4("Last update"), 
                            h6("2021-11-06"),
                            "There are several excellent COVID-19 data in Ontario available, including those run by", 
                            tags$a(href="https://data.ontario.ca/dataset", "COVID-19 Datasets publichsed by Government of Ontario,"),
                            tags$a(href="https://github.com/ccodwg/Covid19Canada", " COVID-19 Canada Open Data Working Group,"),"and other additional population datasets",
                            tags$a(href="https://cran.r-project.org/web/packages/cancensus/vignettes/cancensus.html", "2016 Canada Census."),
                            "Our aim is to ultilize these resources with several additional", tags$a(href="https://cran.r-project.org/web/packages/cancensus/vignettes/cancensus.html", "COVID-19 related factors."), "including the temporal effect to estimate the infected risk.",
                            
                            tags$br(),tags$br(),tags$h4("Background"), 
                            "In December 2019, a severe respiratory illness began to be reported across the city of Wuhan in China. 
                             It was caused by a new type of coronavirus, and the disease is now commonly referred to as COVID-19.
                             The number of COVID-19 cases started to escalate quickly in mid-January and the virus soon spread to other countries. 
                             The first case in Ontario was reported in 25th, Janurary 2020. And the virus started spreading quickly until now there are 602,087 cases reported",
                            tags$br(),tags$br(),
                            "The study begins by specifying a Bayesian Spatial-temporal model to take into account spatial and temporal effects on the spread of the virus in Ontario.
                             And then Use the Spatial-temporal model to assess policy decisions and test the significance of auxiliary variables.
                             Area-to-area(ATA) and Area-to-point(ATP) Poisson Kriging are implemented to provide another approach to create spatial maps which takes into account the sizes of the units used in aggregation of the data.",
                            tags$br(),tags$br(),tags$h4("Code"),
                            "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/Mujingrui", "Github."),
                            tags$br(),tags$br(),tags$h4("Sources"),
                            tags$b("Area-to-area Poisson Kriging:"), tags$a(href="https://ij-healthgeographics.biomedcentral.com/articles/10.1186/1476-072X-7-6", "How does Poisson kriging compare to the popular BYM model for mapping disease risks?,")," with additional information from the ",
                            tags$br(),
                            tags$b("Area-to-point Poisson Kriging:"), tags$a(href="https://ij-healthgeographics.biomedcentral.com/articles/10.1186/1476-072X-5-52", "Geostatistical analysis of disease data: accounting for spatial support and population density in the isopleth mapping of cancer mortality risk using area-to-point Poisson kriging,"),
                            tags$br(),
                            tags$b("Bayesian Spatial-temporal Models"), tags$a(href="https://link.springer.com/article/10.1007/BF00116466", "Bayesian image restoration, with two applications in spatial statistics"),tags$br(),
                            tags$a(href="https://www.researchgate.net/publication/309760682_Bayesian_spatial_and_spatiotemporal_modelling_with_R-INLA", "Bayesian spatial and spatiotemporal modelling with R-INLA"),
                            tags$br(),tags$br(),tags$h4("Author"),
                            "Jingrui Mu, Department of Mathematics and Statistics, University of Ottawa",
                            tags$br(),tags$br(),tags$h4("Supervisor"),
                            tags$a(href="https://mysite.science.uottawa.ca/malvo/","Mayer Alvo"),
                            tags$br(),tags$br(),tags$h4("Contact"),
                            "jmu063@uottawa.ca"
                            )
               )
               
                                      
))

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    month_PHU <- reactive({
        m <- covid_biweekly_cumgeo %>% filter(Date.x == input$Date)
        return(m)
    })
    
    output$Age_Adjusted_Rate_Cumulative <- renderLeaflet({
        labels <- sprintf(
            "<strong>%s</strong><br>%g cases per 100,000 people",
            month_PHU()$PHU_NAME_E,month_PHU()$Age_Adjusted_Rate) %>%
            lapply(htmltools::HTML)
        
        pal = colorBin("Spectral", domain = covid_biweekly_cumgeo$Age_Adjusted_Rate,bins = c(0,10,20,50,100,300,600,1000,2000,4000,7000),pretty = FALSE, reverse = TRUE)
        
        month_PHU() %>%
            leaflet() %>%
            #addProviderTiles(provider = "CartoDB.Positron")%>%
            addTiles() %>%
            addPolygons(color = "grey",
                        weight = 1,
                        label = labels,
                        #stroke = FALSE,
                        #smoothFactor = 0.5,
                        opacity = 1,
                        fillOpacity = 0.5,
                        fillColor = ~pal(month_PHU()$Age_Adjusted_Rate),
                        highlightOptions = highlightOptions(weight = 4,
                                                            fillOpacity = 0.5,
                                                            color = "black",
                                                            opacity = 0.5,
                                                            bringToFront = TRUE),
                        labelOptions = labelOptions(
                            style =
                                list(
                                    "font-weight" = "normal",
                                    padding = "3px 8px"
                                ),
                            textsize = "15px", direction = "auto"
                        )) %>%
            
            addLegend(position = "topleft",
                      pal = pal,
                      values = ~ Age_Adjusted_Rate,
                      title = "cases per 100,000 people",
                      opacity = 0.5)
        
        
    })
    
    output$Age_Adjusted_Rate_Biweekly <- renderLeaflet({
        labels <- sprintf(
            "<strong>%s</strong><br>%g cases per 100,000 people",
            month_PHU()$PHU_NAME_E,month_PHU()$Age_Adjusted_Rate_Biweekly) %>%
            lapply(htmltools::HTML)
        
        pal1 = colorBin("Spectral", domain = covid_biweekly_cumgeo$Age_Adjusted_Rate_Biweekly,bins = c(0,10,20,50,100,200,300,500,600,700,900),pretty = FALSE, reverse = TRUE)
        
        month_PHU() %>%
            leaflet() %>%
            #addProviderTiles(provider = "CartoDB.Positron")%>%
            addTiles() %>%
            addPolygons(color = "grey",
                        weight = 1,
                        label = labels,
                        #stroke = FALSE,
                        #smoothFactor = 0.5,
                        opacity = 1,
                        fillOpacity = 0.5,
                        fillColor = ~pal1(month_PHU()$Age_Adjusted_Rate_Biweekly),
                        highlightOptions = highlightOptions(weight = 4,
                                                            fillOpacity = 0.5,
                                                            color = "black",
                                                            opacity = 0.5,
                                                            bringToFront = TRUE),
                        labelOptions = labelOptions(
                            style =
                                list(
                                    "font-weight" = "normal",
                                    padding = "3px 8px"
                                ),
                            textsize = "15px", direction = "auto"
                        )) %>%
            
            addLegend(position = "topleft",
                      pal = pal1,
                      values = ~ Age_Adjusted_Rate_Biweekly,
                      title = "cases per 100,000 people",
                      opacity = 0.5)
        
        
    })
    
    output$cumulative_plot <- renderPlot({
        cumulative_plot(covid_time_on)
    })
    
    output$epi_curve <- renderPlot({
        new_cases_plot(covid_time_on)
    })
    
    month_PHU1 <- reactive({
        m1 <- covid_cum_biweeklyb_geo %>% filter(Date.x == input$Date1)
        return(m1)
    })
    
    output$estimated_infected_risk_Cumulative <- renderLeaflet({
        labels <- sprintf(
            "<strong>%s</strong><br>%g estimeted infected cases per 100,000 people",
            month_PHU1()$PHU_NAME_E,month_PHU1()$mean_cum) %>%
            lapply(htmltools::HTML)
        
        pal2 = colorBin("Spectral", domain = covid_cum_biweeklyb_geo$Age_Adjusted_Rate,bins = c(0,10,20,50,100,300,600,1000,2000,4000,7000),pretty = FALSE, reverse = TRUE)
        
        month_PHU1() %>%
            leaflet() %>%
            #addProviderTiles(provider = "CartoDB.Positron")%>%
            addTiles() %>%
            addPolygons(color = "grey",
                        weight = 1,
                        label = labels,
                        #stroke = FALSE,
                        #smoothFactor = 0.5,
                        opacity = 1,
                        fillOpacity = 0.5,
                        fillColor = ~pal2(month_PHU1()$mean_cum),
                        highlightOptions = highlightOptions(weight = 4,
                                                            fillOpacity = 0.5,
                                                            color = "black",
                                                            opacity = 0.5,
                                                            bringToFront = TRUE),
                        labelOptions = labelOptions(
                            style =
                                list(
                                    "font-weight" = "normal",
                                    padding = "3px 8px"
                                ),
                            textsize = "15px", direction = "auto"
                        )) %>%
            
            addLegend(position = "topleft",
                      pal = pal2,
                      values = ~ mean_cum,
                      title = "estimated infected cases <br> per 100,000 people",
                      opacity = 0.5)
        
        
    })
    
    output$estimated_infected_risk_Biweekly <- renderLeaflet({
        labels <- sprintf(
            "<strong>%s</strong><br>%g estimated infected cases per 100,000 people",
            month_PHU1()$PHU_NAME_E,month_PHU1()$mean_biweekly) %>%
            lapply(htmltools::HTML)
        
        pal3 = colorBin("Spectral", domain = covid_cum_biweeklyb_geo$mean_biweekly,bins = c(0,10,20,50,100,200,300,500,600,700,900),pretty = FALSE, reverse = TRUE)
        
        month_PHU1() %>%
            leaflet() %>%
            #addProviderTiles(provider = "CartoDB.Positron")%>%
            addTiles() %>%
            addPolygons(color = "grey",
                        weight = 1,
                        label = labels,
                        #stroke = FALSE,
                        #smoothFactor = 0.5,
                        opacity = 1,
                        fillOpacity = 0.5,
                        fillColor = ~pal3(month_PHU1()$mean_biweekly),
                        highlightOptions = highlightOptions(weight = 4,
                                                            fillOpacity = 0.5,
                                                            color = "black",
                                                            opacity = 0.5,
                                                            bringToFront = TRUE),
                        labelOptions = labelOptions(
                            style =
                                list(
                                    "font-weight" = "normal",
                                    padding = "3px 8px"
                                ),
                            textsize = "15px", direction = "auto"
                        )) %>%
            
            addLegend(position = "topleft",
                      pal = pal3,
                      values = ~ mean_biweekly,
                      title = "estimated infected cases <br> per 100,000 people",
                      opacity = 0.5)
        
        
    })
    
    output$temporal_biweekly <- renderPlot({
        temporal_effect_plot(temporal_biweekly)
    })
    
    output$temporal_cumulative <- renderPlot({
        temporal_effect_plot(temporal_cum)
    })
    
    month_PHU2 <- reactive({
        m2 <- atapoisson %>% filter(Date == input$Date2)
        return(m2)
    })
    
    output$reg.est <- renderLeaflet({
        labels <- sprintf(
            "<strong>%s</strong><br>%g estimeted infected cases per 100,000 people",
            month_PHU2()$PHU_NAME_E,month_PHU2()$reg.est) %>%
            lapply(htmltools::HTML)
        
        pal4 = colorBin("Spectral", domain = atapoisson$reg.est,bins = c(0,10,20,50,100,300,600,1000,2000,4000,6000),pretty = FALSE, reverse = TRUE)
        
        month_PHU2() %>%
            leaflet() %>%
            #addProviderTiles(provider = "CartoDB.Positron")%>%
            addTiles() %>%
            addPolygons(color = "grey",
                        weight = 1,
                        label = labels,
                        #stroke = FALSE,
                        #smoothFactor = 0.5,
                        opacity = 1,
                        fillOpacity = 0.5,
                        fillColor = ~pal4(month_PHU2()$reg.est),
                        highlightOptions = highlightOptions(weight = 4,
                                                            fillOpacity = 0.5,
                                                            color = "black",
                                                            opacity = 0.5,
                                                            bringToFront = TRUE),
                        labelOptions = labelOptions(
                            style =
                                list(
                                    "font-weight" = "normal",
                                    padding = "3px 8px"
                                ),
                            textsize = "15px", direction = "auto"
                        )) %>%
            
            addLegend(position = "topleft",
                      pal = pal4,
                      values = ~ reg.est,
                      title = "estimated infected cases <br> per 100,000 people",
                      opacity = 0.5)
        
        
    })
    
    month_PHU3 <- reactive({
        m3 <- atppoisson %>% filter(Date == input$Date3)
        return(m3)
    })
    
    output$local_risk <- renderLeaflet({
        labels <- sprintf(
            "<strong>%s</strong><br>%g estimeted infected cases per 100,000 people",
            month_PHU3()$PHU_NAME_E,month_PHU3()$local_risk) %>%
            lapply(htmltools::HTML)
        
        pal5 = colorBin("Spectral", domain = atapoisson$local_risk,bins = c(0,10,20,50,100,300,600,1000,2000,4000,7000),pretty = FALSE, reverse = TRUE)
        
        month_PHU3() %>%
            leaflet() %>%
            #addProviderTiles(provider = "CartoDB.Positron")%>%
            addTiles() %>%
            addPolygons(color = "grey",
                        weight = 1,
                        label = labels,
                        #stroke = FALSE,
                        #smoothFactor = 0.5,
                        opacity = 1,
                        fillOpacity = 0.5,
                        fillColor = ~pal5(month_PHU3()$local_risk),
                        highlightOptions = highlightOptions(weight = 4,
                                                            fillOpacity = 0.5,
                                                            color = "black",
                                                            opacity = 0.5,
                                                            bringToFront = TRUE),
                        labelOptions = labelOptions(
                            style =
                                list(
                                    "font-weight" = "normal",
                                    padding = "3px 8px"
                                ),
                            textsize = "15px", direction = "auto"
                        )) %>%
            
            addLegend(position = "topleft",
                      pal = pal5,
                      values = ~ local_risk,
                      title = "estimated infected cases <br> per 100,000 people",
                      opacity = 0.5)
        
        
    })
    
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
