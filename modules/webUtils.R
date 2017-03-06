fetchYahoo <- function(symbols, from, to=Sys.Date(), adjusted=TRUE, check.names=TRUE, foundSymbolsIndex=FALSE){
  require(xts)
  # In:  un string con los simbolos a consultar, una fecha inicial de query.
  #      opcionalmente se puede dar una fecha final y un booleano indicando si se deben ajustar precios (splits, dividendos)
  # Out: un xts con la lista de precios en el periodo seleccionando.
  # si foundSymbols=TRUE regresa una lista cuyo primer elemnto es el xts y el segundo es un indice con los tickers encontrados de los tickers encontrados
  if(length(symbols)==0) stop("symbols no puede ser vac?o")
  localenv <- new.env()  
  prices.list <- list()
  symb.names <- character()
  found <- logical()
  options(warn=-1)
  j <- 1
  for(symb in symbols){
    success <- try({
      symb.names[j] <- getSymbols(symb, env = localenv, auto.assign = TRUE, src = "yahoo", from=from, to=to)
      if(adjusted) localenv[[symb]] <- adjustOHLC(localenv[[symb.names[j]]], use.Adjusted=adjusted) # Ajusta precios
      prices.list[[j]] <-  localenv[[symb]][ ,4] # Recupera la columnas de precios de cierre
      names(prices.list[[j]]) <- symb.names[j]
      j <- j +1
    }, silent=TRUE)
    found <- c(found, !inherits(success, "try-error"))
  }
  options(warn=0)
  if(sum(found)>0) warning(sprintf("Tickers %s not found in Yahoo", paste(symbols[which(!found)], collapse=", ")))
  names(found) <- symbols
  Prices <- do.call("cbind", prices.list) # une todas las columnas
  if(!check.names) names(Prices) <- symb.names[found] # Le pone los nombres a los indices
  if(foundSymbolsIndex) {return(list(prices=Prices, index=found))} else {return(Prices)}
} 