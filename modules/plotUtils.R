plotRet <- function(ret, accumulate, priceRef, FUN, mouseoverTickerLabel, xlab, ylab) UseMethod("plotRet")
plotRet.ret <- function(ret, accumulate=TRUE, priceRef=NULL, FUN=function(x) round(100*x, 2), mouseoverTickerLabel="precio", xlab="", ylab="Ret (%)"){
  if(accumulate) ret <- accumulate(ret)
  plotdat <- gather(data.frame(Fecha=index(ret), ret, check.names = FALSE), variable, value, -Fecha)
  if(!is.null(priceRef)){
    price <- data.frame(Fecha=index(ret), priceRef[index(ret)]) %>% gather(variable, value, -Fecha)
    plotdat$price <- paste0(mouseoverTickerLabel, ": ", round(price$value, 2))
    p <- plot_ly(data = plotdat, x=~Fecha, y=~FUN(value), text=~price, color=~ordered(variable), colors="Set1", line=list(size=3))  %>% add_lines()
  } else{
    p <- plot_ly(data = plotdat, x=~Fecha, y=~FUN(plotdat$value), color=~ordered(variable), colors="Set1", line=list(width=3))  %>% add_lines()
  }
  return(layout(p, yaxis=list(title=ylab), xaxis=list(title=xlab), paper_bgcolor='rgba(0,0,0,0)',  plot_bgcolor='rgba(0,0,0,0)'))
}

plotSeries <- function(series, ...) UseMethod("plotSeries")
plotSeries.xts <- function(series, FUN=function(x) x, xlab="", ylab="", ...){
  plotdat <- gather(data.frame(Fecha=index(series), series, check.names = FALSE), variable, value, -Fecha)
  p <- plot_ly(plotdat, x=~Fecha, y=~FUN(value), color=~ordered(variable), colors="Set1", line=list(width=3), ...)  %>% add_lines()
  return(layout(p, yaxis=list(title=ylab), xaxis=list(title=xlab), paper_bgcolor='rgba(0,0,0,0)',  plot_bgcolor='rgba(0,0,0,0)'))
}

contribBarChart <- function(vec1, vec2=NULL, firstBlue=TRUE){
  print(vec1)
  print(vec2)
  category <- names(vec1)
  if(is.null(category))  category <- 1:length(vec1)
  values <- as.numeric(vec1)
  scale <- character(length(values))
  scale[1] <- toRGB("blue", .5)
  for(i in 2:length(scale)) scale[i] <- ifelse(values[i]>0, toRGB("green", .5), toRGB("red", .5))
  p <- plot_ly(x=category, y=100*values, type="bar", marker=list(color=scale), name="Contrib") 
  if(!is.null(vec2)) if(length(vec2)==length(vec1)){
    p <- add_trace(p, x=category, y=100*as.numeric(vec2), name="Reporto",  type="marker+line", marker=list(size=4), line=list(size=3), name="Reporto", showlegend=TRUE)
  }
  return(layout(p, 
                xaxis=list(title=""), 
                yaxis=list(title="Ret (%)"), 
                paper_bgcolor='rgba(0,0,0,0)',  
                plot_bgcolor='rgba(0,0,0,0)',
                margin=list(b=150, t=100)
                ))
}

