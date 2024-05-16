# Alex Abbas
# 121129
# creating plots inspired by Spirograph(tm)

# functions
spiroPlot <- function(
 majorRadius = 1,
 minorRadius = 1/3,
 penRadius = 1,
 col = "black",
 angularTravel = 2 * pi * timesBackToStart ( majorRadius, minorRadius ),
 startAngle = 0,
 step = pi / 100,
 add = FALSE
) {
 par(xpd=NA)
 if (!add) {
  i <- majorRadius + penRadius
  plot ( c( -i , i ), c( -i , i ) , type = "n", xlab = "", ylab = "", axes = F )
 }
 circAngularOrient <- 0
 circAngularCoord <- startAngle
 steps <- angularTravel / step
 coord1 <- getPenCoords( circAngularOrient, circAngularCoord, majorRadius, penRadius )
 for (i in 1:steps) {
  circAngularCoord <- circAngularCoord + step
  circAngularOrient <- circAngularOrient - majorRadius / minorRadius * step
  coord2 <- getPenCoords( circAngularOrient, circAngularCoord, majorRadius, penRadius )
  lines(c(coord1[1],coord2[1]),c(coord1[2],coord2[2]),col=col)
  coord1 <- coord2
 }
}

timesBackToStart <- function ( major, minor ) {
 laps <- 1
 position <- 0
 end <- 1000
 for ( i in 1:end ) {
  position <- position + minor / major
  diff <- abs ( round( position ) - position )
  if ( diff < .00001 ) { return ( laps ) }
  if ( position > 1 ) {
   laps <- laps + 1
   position <- position - 1
  }
 }
 return ( end )
}

getPenCoords <- function( circAngularOrient, circAngularCoord, majorRadius, penRadius ) {
 circX <- sin( circAngularCoord ) * majorRadius
 circY <- cos( circAngularCoord ) * majorRadius
 penX <- circX + sin( circAngularOrient ) * penRadius
 penY <- circY + cos( circAngularOrient ) * penRadius
 c( penX , penY )
}




### example 1 ###
for (i in 1:5) {
  spiroPlot(
    9/4,
    1/3,
    2,
    col=rainbow(5)[i],
    startAngle=.05*(i-1),
    add=i>1
  )
}  

### example 2 ###
for (i in 1:4) {
  spiroPlot(
    7/4,
    2/3,
    6/7,
    col=rainbow(10)[i+4],
    startAngle=.04*(i-1),
    add=i>1
  )
}  

### example 3 ###
for (i in 1:7) {
  spiroPlot(
    7/4*(1+i*.05),
    2/3*(1+i*.05),
    6/7,
    col=rainbow(10)[i+3],
    add=i>1
  )
}  

