#Toolbox project
#Maier Wang
#mw3171
#shiny app


mypath="C:/Users/Maier.Wang/Desktop/documents/CU/toolbox/pumpkin_dataset" # Use specific directory to replace "my path".
setwd(mypath)
source("pumpkin_source code.R")
library(shiny)
library(gridExtra)

# User interface ----
ui <- fluidPage(
  titlePanel("pumpkin data analysis"),
  
  sidebarLayout(position = "left",
    sidebarPanel("sidebar panel",
                 checkboxInput("donum1", "Make #1 graph", value = T),
                 checkboxInput("donum2", "Make #2 graph", value = F),
                 checkboxInput("donum3", "Make #3 graph", value = F),
                 checkboxInput("donum4", "Make #4 graph", value = F),
                 checkboxInput("donum5", "Make #5 graph", value = F),
                 checkboxInput("donum6", "Make #6 graph", value = F),
                 checkboxInput("donum7", "Make #7 graph", value = F),
                 checkboxInput("donum8", "Make #8 graph", value = F),
                 
                 sliderInput("wt1","Weight 1",min=1,max=10,value=1),
                 sliderInput("wt2","Weight 2",min=1,max=10,value=1),
                 sliderInput("wt3","Weight 3",min=1,max=10,value=1),
                 sliderInput("wt4","Weight 4",min=1,max=10,value=1),
                 sliderInput("wt5","Weight 5",min=1,max=10,value=1),
                 sliderInput("wt6","Weight 6",min=1,max=10,value=1),
                 sliderInput("wt7","Weight 7",min=1,max=10,value=1),
                 sliderInput("wt8","Weight 8",min=1,max=10,value=1),
      
      helpText("Create graphs with 
        pumpkin data from 09/24/2016 to 09/30/2017.")),

    mainPanel("main panel",
              column(8,plotOutput(outputId="plotgraph", width="800px",height="500px")))
              )
  )


# Server logic ----
server <- function(input, output) 
  {
  
  set.seed(1:8)
  pt1 <- reactive({
    if (!input$donum1) return(NULL)
    pairs(pmk_num)
  })
  pt2 <- reactive({
    if (!input$donum2) return(NULL)
    g2
  })
  pt3 <- reactive({
    if (!input$donum3) return(NULL)
    g3
  })
  pt4 <- reactive({
    if (!input$donum4) return(NULL)
    g4
  })
  pt5 <- reactive({
    if (!input$donum5) return(NULL)
    g5
  })
  pt6 <- reactive({
    if (!input$donum6) return(NULL)
    g6
  })
  pt7 <- reactive({
    if (!input$donum7) return(NULL)
    g7
  })
  pt8 <- reactive({
    if (!input$donum8) return(NULL)
    plot(Date,Mean_Price)
  })
  output$plotgraph = renderPlot({
    ptlist <- list(pt1(),pt2(),pt3(),pt4(),pt5(),pt6(),pt7(),pt8())
    wtlist <- c(input$wt1,input$wt2,input$wt3,input$wt4,input$wt5,input$wt6,input$wt7,input$wt8)
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
  })
}

# Run app ----
shinyApp(ui, server)
