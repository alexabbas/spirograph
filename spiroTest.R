
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
  outside = fixed < rotating
) {
  step = pi / resolution
  rotRadius = rotating / pi / 2
  fixedRadius = fixed / pi / 2
  orbitRadius = fixedRadius + rotRadius * ifelse(outside,1,-1)
  penRadius = rotRadius * penRatio
  par(xpd=NA)
  if (!add) {
    i <- orbitRadius + penRadius
    plot ( c( -i , i ), c( -i , i ) , type = "n", xlab = "", ylab = "", axes = F )
  }
  circAngularOrient <- 0
  circAngularCoord <- startAngle
  steps <- angularTravel / step
  coord1 <- getPenCoords( circAngularOrient, circAngularCoord, orbitRadius, penRadius )
  for (i in 1:steps) {
    circAngularCoord <- circAngularCoord + step
    circAngularOrient <- circAngularOrient + fixed / rotating * step * ifelse(outside,1,-1)
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


teethData = read.csv("~/Google Drive/etc/spirograph/spirograph/teeth.csv")
teethOuter = teethData %>% filter(side=="outer")
teethLength = dim(teethOuter)[1]
teethHash = c(1:max(teethOuter$teeth)) / max(teethOuter$teeth)

input = list(
  fixed = 96,
  rotating = 52,
  penRatio = .9,
  outside = F,
  penColor = "red"
)
makeSpiroPlot(
  fixed = input[["fixed"]], # teethHash[input["fixed"]],
  rotating = input[["rotating"]], # teethHash[input["rotating"]],
  penRatio = input[["penRatio"]],
  col = input[["penColor"]],
  outside = input[["outside"]]
)
