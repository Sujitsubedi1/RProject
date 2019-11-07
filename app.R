#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.sni
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


pacman::p_load('ggplot2','shiny','tidyverse','readr', 'highcharter')

setwd("~/401WebApp/401Webapp/RProject")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Marks by Student"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tags$style('.well {background-color: #2A9FBC}'),
            fileInput("myData", label = h3("File input")),
            downloadButton("download_myData", "Download from web Browser"),
            actionButton("action", label = "save Data"),
            selectInput("Subject", "Select the subject:",
                        choices = c("CMPS401","Math201"),
                        selected = "CMPS401"),
            h4('User may load data, download data, save data and switch subgroups of data')
                    ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel('Plot histogram',
                         plotOutput('histogram')),
                tabPanel('Plot barchart',
                         plotOutput('barplot')),
                tabPanel('Mathematical Calculation',
                         tableOutput('math')),
                tabPanel('Data',
                         tableOutput('horsepower')),
                tabPanel('Global Data',
                         h4('*Data from global.R file'),
                         tableOutput('myUsers'))
            ) 
        )
    )
)
#access the global.R file
source("Global.R")

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    myData <- reactive({
        print(input$myData$name)
        read_csv(input$myData$datapath)
    })
    
    output$horsepower <- renderTable({
        if (is.null(input$myData)){
            return()
        } else if(!is.null(input$myData)){
            
            if(input$Subject == 'All'){
                myData()
            } else if ((input$Subject != '')){
                myData() %>% filter(Subject == input$Subject)
            }
        }
    })
    
    output$histogram <- renderPlot({
        if(is.null(input$myData)){
            return ()
        }else if (!is.null(input$myData)){
            myData() %>% filter(Subject == input$Subject)%>%
            ggplot(aes(marks))+ geom_histogram(bins = 10)+ labs(y = "Number Of Student")
        }
    })
    output$barplot <-renderPlot({
        if(is.null(input$myData)){
            return ()
        }else if (!is.null(input$myData)){
            myData() %>% filter(Subject == input$Subject)%>%
                ggplot(aes(Grade))+ geom_bar(bins = 10)+ labs(y = "Number Of Student")
        }
    })
    
    output$math <-renderTable({
        if(is.null(input$myData)){
            return ()
        }else if (!is.null(input$myData)){
            x<- mean(input$subject)
        }
    })

    output$download_myData <- downloadHandler(
        filename = function(){
            print(input$myData$name)
            paste(input$myData$name, ".csv", sep = "")
        },
        content = function(filename){
            print(filename)
            write_csv(myData(),filename)
        }
    )
    
    observeEvent(input$action,{
        write_csv(myData(),input$myData$name)
    })
    output$myUsers <- renderTable({
            return(myUsers)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

