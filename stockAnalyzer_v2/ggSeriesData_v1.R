
#ASB SEARCH + CHART BUILDER
ggSeriesData <- function(symbol.list, start, end){
  
  #Loop through symbols, fetch prices, and store in symbol.list
  x.list <- list()
  x.list <-lapply(symbol.list, function(x) {getSymbols(x, auto.assign=FALSE, options("getSymbols.warning4.0"=FALSE))}) #Removed from=start, to=end,
  names(x.list) <- symbol.list
  
  #Create the sub-lists
  x.subset <- lapply(1:length(x.list), function(x){
    #First, strip OHLC data (need to vectorize)
    {date <- as.Date(time(x.list[[x]]))
    open <- as.vector(Op(x.list[[x]]))
    high <- as.vector(Hi(x.list[[x]]))
    low <- as.vector(Lo(x.list[[x]]))
    close <- as.vector(Cl(x.list[[x]]))
    
    #Then build the data frame
    temp <- data.frame('date'=date,'open'=open,'high'= high,'low'=low,'close'=close)
    
    #Add the symbol
    temp$symbol <- as.character(names(x.list[x]))
    
    #We want to construct our candlesticks  
    temp$candleLower <- pmin(temp$open, temp$close)
    temp$candleMiddle <- (temp$open + temp$close)/2 #NA #Can't use NA 
    temp$candleUpper <- pmax(temp$open, temp$close)
    temp$fill <- ''
    temp$fill[temp$open < temp$close] = "Gain" #'#31a354'
    temp$fill[temp$fill ==''] = "Loss" #'#de2d26'
    
    #Add Moving Averages
'    long <- as.numeric(end-start)
    short <- as.numeric(end-start)/4'
    temp$maLong <- SMA(temp$close, n=200)
    temp$maShort <- SMA(temp$close, n=50)}
    
    temp <-subset(temp, temp$date > start & temp$date < end)
    
    return(temp)
  })
  
  names(x.subset) <- symbol.list
  x.merged <- Reduce(function(...) merge(..., all=T), x.subset)
  return(x.merged)
}
