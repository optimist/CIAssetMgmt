rollProd <- function(object, roll) UseMethod("rollProd")
rollProd.xts <- function(x, roll=1) {
  require(RcppRoll)
  rollprod <- apply(x+1,2, RcppRoll::roll_prod,n=roll)-1
  date <- index(x)
  rollprod <- xts(rollprod, order.by=date[roll:length(date)])
  return(rollprod)
}

fillMissingDates <- function(db) UseMethod("fillMissingDates")
fillMissingDates.xts <- function(db){
  db.copy <- db
  for(j in 1:ncol(db.copy)){ 
    if(is.na(db.copy[1,j])){ # for first values, input the first observed
      it <- 2
      while(is.na(db.copy[it, j]) & it<=nrow(db.copy)) it <- it+1
      db.copy[1:(it-1), j] <- db.copy[it, j]
    }
    for(i in 2:nrow(db.copy)) if(is.na(db.copy[i, j])) db.copy[i, j] <- db.copy[i-1, j]   # for the next input the previous one
  }
  return(db.copy)
}

dlyRet <- function(object, roll) UseMethod("dlyRet", object)
dlyRet.xts <- function(prices, roll=1) {
  dlyRet <- prices/xts::lag.xts(prices)-1 # retornos
  dlyRet[is.na(dlyRet)] <- 0 # rellenar ceros
  dlyRet <- (rollProd(dlyRet, roll)+1)^(1/roll)-1
  attr(dlyRet, "class") <- c("ret", "xts", "zoo")
  return(dlyRet)
}

retInPeriod <- function(db, from=min(index(db)), to=max(index(db)), annualize=FALSE, fillMissing=TRUE) UseMethod("retInPeriod")
retInPeriod.xts <- function(db, from=min(index(db)), to=max(index(db)), annualize=FALSE, fillMissing=TRUE){
  ret <- dlyRet(fillMissingDates(db[paste(from, to, sep="/")]))
  res <- accumulate(ret, last=TRUE)
  if(annualize) res <- res*360/as.numeric(to-from)
  return(res)
}

# accumulateRet <- function(ret) UseMethod("accumulateRet")
# accumulateRet.xts <- function(ret) return(cumprod(ret+1)-1)

allRet <- function(db, roll=1, fillMissing=TRUE) UseMethod("allRet")
allRet.xts <- function(db, roll=1, fillMissing=TRUE){
  db.copy <- db
  if(fillMissing) db.copy <- fillMissingDates(db.copy)
  daily <- dlyRet(db.copy, roll)
  weekly <- do.call("cbind", lapply(names(db.copy), function(col) apply.weekly(daily[ ,col], function(x) prod(1+x)-1)))
  monthly <- do.call("cbind", lapply(names(db.copy), function(col) apply.monthly(daily[ ,col], function(x) prod(1+x)-1)))
  yearly <- do.call("cbind", lapply(names(db.copy), function(col) apply.yearly(daily[ ,col], function(x) prod(1+x)-1)))
  attr(weekly, "class") <- attr(monthly, "class")  <- attr(yearly, "class")  <- c("ret", "xts", "zoo")
  out <- list(daily=daily, weekly=weekly, monthly=monthly, yearly=yearly)
  attr(out, "class") <- "allRet"
  return(out)
}

accumulate <- function(ret, last=FALSE) UseMethod("accumulate")
accumulate.xts <- function(ret, last=FALSE){
  res <- cumprod(ret+1)-1
  attr(res, "class") <- c("ret", "xts", "zoo")
  if(last){
    res <- as.numeric(res[nrow(res), ])
    names(res) <- names(ret)
  }
  return(res)
}

annualize <- function(ret, ...) UseMethod("annualize")
annualize.ret <- function(ret){
  if(nrow(ret)>1) for(i in 2:nrow(ret)) ret[i, ] <- ret[i, ]*360/as.numeric(index(ret)[i]-index(ret)[i-1])
  return(ret)
}

annualize.allRet <- function(retlist, completeLastPeriod=FALSE){
  daily <- retlist$daily
  weekly <- retlist$weekly
  monthly <- retlist$monthly
  yearly <- retlist$yearly
  if(completeLastPeriod){
    lastweekdate <- index(weekly)[nrow(weekly)]
    i <- 0
    while(julian(ymd(lastweekdate))%%7 != 1)  lastweekdate <- lastweekdate + 1 # find next friday
    index(weekly)[nrow(weekly)] <- lastweekdate
    index(monthly)[nrow(monthly)] <- lastOfMonth(index(monthly)[nrow(monthly)])
  }
  out <- list(daily=annualize(daily), weekly=annualize(weekly), monthly=annualize(monthly), yearly=annualize(yearly))
  attr(out, "class") <- "allRet"
  return(out)
}
summary.allRet <- function(ret) {
  n <- nrow(ret$weekly)
  week <- ret$weekly[n, ]
  week.prev <- ret$weekly[n-1, ]
  n <- nrow(ret$monthly)
  month <- ret$monthly[n, ]
  month.prev <- ret$monthly[n-1, ]
  n <- nrow(ret$yearly)
  year <- ret$yearly[n, ]
  year.prev <- ret$yearly[n-1, ]
  summary <- data.frame(rbind(as.numeric(week.prev), as.numeric(week), as.numeric(month.prev), as.numeric(month), as.numeric(year.prev), as.numeric(year)))
  dimnames(summary) <- list(c("week.prev", "weektd", "month.prev", "mtd", "year.prev", "ytd"), names(ret[[1]]))
  return(summary)
}

# ESTA FUNCION ES DE UTILIDAD SOLO PARA LA APP DE SHINY
shinyAppSummaryTable <- function(db, key, annualize=FALSE, completeLastPeriod=FALSE) UseMethod("shinyAppSummaryTable")
shinyAppSummaryTable.xts <- function(db, key, annualize=FALSE, completeLastPeriod=FALSE){
  ret <- allRet(db[ ,key], fillMissing=TRUE)
  if(annualize) ret <- annualize(ret, completeLastPeriod)
  n <- nrow(ret$yearly)
  year.cur <- ret$yearly[n, ]
  year.prev <- ret$yearly[n-1, ]
  yeartab <- data.frame(rbind(year.prev, year.cur), row.names = c(year(year.prev), year(year.cur)))
  monthtab <- ret$monthly[year(index(ret$monthly))==year(year.cur), ]
  monthtab <- data.frame(monthtab, row.names=monthName(index(monthtab)))
  n <- nrow(ret$weekly)
  week.cur <- ret$weekly[n, ]
  week.prev <- ret$weekly[n-1, ]
  weektab <- data.frame(rbind(week.prev, week.cur), row.names = c("Semana anterior", "Semana"))
  table <- rbind(weektab, monthtab, yeartab)
  names(table) <- key
  return(table)
}



 
