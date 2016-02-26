
#ASB SEARCH + CHART BUILDER
ggSeriesData <- function(symbol.list, start, end){
  
  #Loop through symbols, fetch prices, and store in symbol.list
  x.list <- list()
  x.list <-lapply(symbol.list, function(x) {getSymbols(x, auto.assign=FALSE, options("getSymbols.warning4.0"=FALSE))})
  names(x.list) <- symbol.list
  
  #Create the sub-lists
  x.subset <- lapply(1:length(x.list), function(x){
    {date <- as.Date(time(x.list[[x]]))
    open <- as.vector(Op(x.list[[x]]))
    high <- as.vector(Hi(x.list[[x]]))
    low <- as.vector(Lo(x.list[[x]]))
    close <- as.vector(Cl(x.list[[x]]))
    
    #Build DF
    temp <- data.frame('date'=date,'open'=open,'high'= high,'low'=low,'close'=close)
    
    #Add Symbol
    temp$symbol <- as.character(names(x.list[x]))
    
    #Candlestick components
    temp$candleLower <- pmin(temp$open, temp$close)
    temp$candleMiddle <- (temp$open + temp$close)/2 #NA #Can't use NA 
    temp$candleUpper <- pmax(temp$open, temp$close)
    temp$fill <- ''
    temp$fill[temp$open < temp$close] = "Gain"
    temp$fill[temp$fill ==''] = "Loss"
    
    #Add Moving Averages
    temp$maLong <- SMA(temp$close, n=200)
    temp$maShort <- SMA(temp$close, n=50)}
    
    #Subset by date
    temp <-subset(temp, temp$date > start & temp$date < end)
    
    return(temp)
  })
  
  names(x.subset) <- symbol.list
  #Merge to single DF
  x.merged <- Reduce(function(...) merge(..., all=T), x.subset)
  return(x.merged)
}
