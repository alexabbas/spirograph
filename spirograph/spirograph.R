# Alex Abbas
# 200610
# create plots inspired by Spirograph(tm)

library(shiny)
library(tidyverse)
library(scales)

# read setup data
teethData = read.csv("teeth.csv")
teethOuter = teethData %>% filter(side=="outer")
teethLength = dim(teethOuter)[1]
teethHash = teethOuter$teeth
penColors = c("Red" = "red",
              "Green" = "forestgreen",
              "Blue" = "blue")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Spirograph"),

    # Sidebar
 
    fluidRow(
        
        column(4,
               wellPanel(
                   selectInput("fixed1",
                               "Fixed teeth:",
                               teethOuter$teeth,
                               selected = "84"),
                   selectInput("rotating1",
                               "Rotating teeth:",
                               teethOuter$teeth,
                               selected = "52"),
                   sliderInput("penRatio1",
                               "Pen radius:",
                               min = 0.01,
                               max = 1,
                               value = .9),
                   selectInput("penColor1", "Pen color:",
                               penColors,selected="red"),
                   checkboxInput("outside1","Outside")
               ),       
               wellPanel(
                   selectInput("fixed2",
                               "Fixed teeth:",
                               teethOuter$teeth,
                               selected = "84"),
                   selectInput("rotating2",
                               "Rotating teeth:",
                               teethOuter$teeth,
                               selected = "52"),
                   sliderInput("penRatio2",
                               "Pen radius:",
                               min = 0.01,
                               max = 1,
                               value = .7),
                   selectInput("penColor2", "Pen color:",
                               penColors,selected="forestgreen"),
                   checkboxInput("outside2","Outside")
               ),       
               wellPanel(
                   selectInput("fixed3",
                               "Fixed teeth:",
                               teethOuter$teeth,
                               selected = "84"),
                   selectInput("rotating3",
                               "Rotating teeth:",
                               teethOuter$teeth,
                               selected = "52"),
                   sliderInput("penRatio3",
                               "Pen radius:",
                               min = 0.01,
                               max = 1,
                               value = .5),
                   selectInput("penColor3", "Pen color:",
                               penColors,selected="blue"),
                   checkboxInput("outside3","Outside")
               )       
        ),
        
        column(8,
               plotOutput("spiroPlot",height="800px",width="800px")
        )
    )
)

server <- function(input, output) {

    # functions
    makeSpiroPlot <- function(
        fixed = 24,
        rotating = 38,
        penRatio = .8,
        col = "black",
        angularTravel = 2 * pi * timesBackToStart ( fixed, rotating ),
        startAngle = 0,
        resolution = 100,
        add = F,
        outside = fixed < rotating,
        plotx = 40
    ) {
        col = alpha(col,alpha=.8)
        step = pi / resolution
        rotRadius = rotating / pi / 2
        fixedRadius = fixed / pi / 2
        orbitRadius = fixedRadius + rotRadius * ifelse(outside,1,-1)
        penRadius = rotRadius * penRatio
        par(xpd=NA)
        if (!add) {
            i <- plotx #(orbitRadius + penRadius * ifelse(outside,1,-1))
            plot ( c( -i , i ), c( -i , i ), type = "n", xlab = "", ylab = "", axes = F )
        }
        circAngularOrient <- 0
        circAngularCoord <- startAngle
        steps <- angularTravel / step
        coord1 <- getPenCoords( circAngularOrient, circAngularCoord, orbitRadius, penRadius )
        for (i in 1:steps) {
            circAngularCoord <- circAngularCoord + step
            circAngularOrient <- ifelse(outside,
                                        circAngularOrient + (fixed / rotating + 1) * step,
                                        circAngularOrient - (fixed / rotating - 1) * step
                                        )
            coord2 <- getPenCoords( circAngularOrient, circAngularCoord, orbitRadius, penRadius )
            lines(c(coord1[1],coord2[1]),c(coord1[2],coord2[2]),col=col)
            coord1 <- coord2
        }
    }
    
    timesBackToStart <- function ( fixed, rotating ) {
        position <- 0
        end <- 10^2
        for ( i in 1:end ) {
            position <- position + rotating / fixed
            diff <- abs ( round( position ) - position )
            if ( diff < .00001 ) { return ( round(position) ) }
        }
        return ( end )
    }
    
    getPenCoords <- function( circAngularOrient, circAngularCoord, fixed, penRadius ) {
        circX <- sin( circAngularCoord ) * fixed
        circY <- cos( circAngularCoord ) * fixed
        penX <- circX + sin( circAngularOrient ) * penRadius
        penY <- circY + cos( circAngularOrient ) * penRadius
        c( penX , penY )
    }
    
    getPlotx = function(fix,rot,out,pen) {
        fix = as.numeric(fix)
        rot = as.numeric(rot)
        out = as.numeric(out)
        pen = as.numeric(pen)
        ifelse(out,
               fix + rot * pen,
               fix - rot * (1-pen)
        ) * .25
    }
    
    # make output plot
    output$spiroPlot <- renderPlot({
        plotx = max(c(
            getPlotx(input[["fixed1"]],input[["rotating1"]],input[["outside1"]],input[["penRatio1"]]),
            getPlotx(input[["fixed2"]],input[["rotating2"]],input[["outside2"]],input[["penRatio2"]]),
            getPlotx(input[["fixed3"]],input[["rotating3"]],input[["outside3"]],input[["penRatio3"]])
        ))
        makeSpiroPlot(
            fixed = as.numeric(input[["fixed1"]]),
            rotating = as.numeric(input[["rotating1"]]),
            penRatio = input[["penRatio1"]],
            col = input[["penColor1"]],
            outside = input[["outside1"]],
            plotx = plotx
        )
        makeSpiroPlot(
            fixed = as.numeric(input[["fixed2"]]),
            rotating = as.numeric(input[["rotating2"]]),
            penRatio = input[["penRatio2"]],
            col = input[["penColor2"]],
            outside = input[["outside2"]],
            add=T
        )
        makeSpiroPlot(
            fixed = as.numeric(input[["fixed3"]]),
            rotating = as.numeric(input[["rotating3"]]),
            penRatio = input[["penRatio3"]],
            col = input[["penColor3"]],
            outside = input[["outside3"]],
            add=T
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
