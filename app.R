#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


pacman::p_load('ggplot2','shiny','tidyverse','readr', 'highcharter')

setwd("~/401WebApp/401Webapp")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Random Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("myData", label = h3("File input")),
            downloadButton("download_myData", "Download from web Browser"),
            actionButton("action", label = "save Data"),
            selectInput("cyl", "Select Cylinder Count:",
                        choices = c(4,6,8),
                        selected = 4),
            h4('User may load data, download data, save data and switch subgroups of data')
                    ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel('Plot',
                         plotOutput('histogram')),
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
            return(myData())
        }
    })
    
    output$histogram <- renderPlot({
        if(is.null(input$myData)){
            return ()
        }else if (!is.null(input$myData)){
            myData() %>% filter(cyl == input$cyl)%>%
            ggplot(aes(hp))+ geom_histogram(bins = 5)
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

