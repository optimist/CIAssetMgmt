# for testing 1
# today <- as.Date("2016-06-16")
# maturityDate <- as.Date("2016-12-15")
# annualCouponRate <-  0.0725
# for testing 2
# today <- as.Date("2016-06-13")
# maturityDate <- as.Date("2019-12-11")
# annualCouponRate <-  .05

bondCashflow <- function(today, maturityDate, annualCouponRate=NULL, annualYield=NULL, faceValue=100, periodicity=182, yearLength=360, dayOfWeekPayment=4){
  # Important note: dayOfWeekForPayment means bonds are payed on thursday. this is true for Mexico
  # this is useful to know when a maturityDate has been moved because of holiday. Example MBONO 191211
  # which pays on wednesday
  output <- list()
  refDate <- maturityDate
  # queremos una condicion para checar si la fechad de vencimiento tambien es dia festivo.
  # lo mas facil es verificar si el dia de pago es jueves, si no hay que agregarle hastsa que sea jueves o el dia de pago del instrumento
  while((julian(refDate)-3)%%7 != dayOfWeekPayment) {
    refDate <- nextWorkingDay(refDate, n=1)
  }
  paymentDates <- maturityDate
  i <- 1
  repeat({
    newDate <- prevWorkingDay(refDate - i*periodicity, n=0)
    if(newDate > today){
      paymentDates <- c(newDate, paymentDates)
      i <- i + 1
    } else break
  })
  npayments <- length(paymentDates)
  totalDaysOfPeriod <- as.numeric(paymentDates - c(refDate - i*periodicity, paymentDates[-npayments]))
  daysOfCurrentCoupon <- as.numeric(today-(refDate - i*periodicity))%%182
  
  output$paymentDates <- paymentDates
  output$daysOfCurrentCoupon <- daysOfCurrentCoupon
  output$totalDaysOfPeriod <- totalDaysOfPeriod
  
  if (!is.null(annualCouponRate)) {
    couponValue <- faceValue*totalDaysOfPeriod*annualCouponRate/yearLength
    cashflow <- couponValue
    cashflow[npayments] <- 100 + cashflow[npayments]
    cashflow <- xts(cashflow, order.by=paymentDates)  
    accruedInterest <- faceValue*annualCouponRate*daysOfCurrentCoupon/360
    output$accruedInterest <- accruedInterest
    output$cashflow <- cashflow
    output$coupon_value <- annualCouponRate*periodicity/yearLength
    if (!is.null(annualYield)) {
      discountFactor <- 1/(1+totalDaysOfPeriod*annualYield/yearLength)^(1:npayments-daysOfCurrentCoupon/periodicity)
      output$discountFactor <- discountFactor
    } 
  }
  return(output)
}

bondPaymentDates <- function(today, maturityDate, periodicity=182, yearLength=360, dayOfWeekPayment=4){
  # Important note: dayOfWeekForPayment means bonds are payed on thursday. this is true for Mexico
  # this is useful to know when a maturityDate has been moved because of holiday. Example MBONO 191211
  # which pays on wednesday
  output <- list()
  refDate <- maturityDate
  if((julian(maturityDate)%%7-3)!=dayOfWeekPayment) refDate <- nextWorkingDay(maturityDate, n=1)
  paymentDates <- maturityDate
  i <- 1
  repeat({
    newDate <- prevWorkingDay(refDate - i*periodicity, n=0)
    if(newDate > today){
      paymentDates <- c(newDate, paymentDates)
      i <- i + 1
    } else break
  })
  return(paymentDates)
}

bondPrice <- function(annualYield, ...) UseMethod("bondPrice")
bondPrice.numeric <- function(annualYield, today, maturityDate, annualCouponRate, cleanPrice=FALSE, periodicity=182, faceValue=100,yearLength=360, dayOfWeekPayment=4){
    info <- bondCashflow(today, maturityDate, annualCouponRate, annualYield, faceValue , periodicity, yearLength, dayOfWeekPayment)
    price <- sum(as.numeric(info$cashflow)*info$discountFactor)
    if(cleanPrice) price <- price - info$accruedInterest
  return(price)
}
bondPrice.xts <- function(annualYield, maturityDate, annualCouponRate, cleanPrice=FALSE, periodicity=182, faceValue=100,yearLength=360, dayOfWeekPayment=4){
  date <- index(annualYield)
  res <- sapply(1:nrow(annualYield), function(i) {
    return(bondPrice(as.numeric(annualYield[i, 1]), date[i], maturityDate, annualCouponRate, cleanPrice, periodicity, faceValue, yearLength, dayOfWeekPayment))
  })
  return(xts(res, date))}

bondYield <- function(dirtyPrice, ...) UseMethod("bondYield")
bondYield.numeric <- function(dirtyPrice, today, maturityDate, annualCouponRate, periodicity=182, faceValue=100, yearLength=360, dayOfWeekPayment=4, tol=1e-5, maxit=1000, x0=.05, h=1e-6){
  fobj <- function(x) return((dirtyPrice - bondPrice(x, today, maturityDate, annualCouponRate, cleanPrice=FALSE, periodicity=periodicity, faceValue=faceValue, yearLength=yearLength, dayOfWeekPayment=dayOfWeekPayment))^2)
  x_i <- x0
  f_i <- fobj(x_i)
  i <- 1
  while(sqrt(f_i) > tol && i <= maxit){
    g_i <- (fobj(x_i+h) - fobj(x_i-h))/(2*h)
    x_i <- x_i-f_i/g_i
    f_i <- fobj(x_i)
    i <- i+1
  }
  if(i==maxit) stop("Algorithm did not converge: try changing maximum number of iterations.")
  return(x_i)
}
bondYield.xts <- function(dirtyPrice, maturityDate, annualCouponRate, periodicity=182, faceValue=100, yearLength=360, dayOfWeekPayment=4, tol=1e-5, maxit=1000, x0=rep(.05, nrow(dirtyPrice))) {
  fobj <- function(x) return((dirtyPrice - bondPrice(x, maturityDate, annualCouponRate, cleanPrice=FALSE, periodicity=periodicity, faceValue=faceValue, yearLength=yearLength, dayOfWeekPayment=dayOfWeekPayment))^2)
  x_i <- xts(x0, index(dirtyPrice))
  f_i <- fobj(x_i)
  i <- 1
  while(max(sqrt(f_i)) > tol && i <= maxit){
    g_i <- (fobj(x_i+h) - fobj(x_i-h))/(2*h)
    x_i <- x_i-f_i/g_i
    f_i <- fobj(x_i)
    i <- i+1
  }
  if(i==maxit) stop("Algorithm did not converge: try changing maximum number of iterations.")
  return(x_i)
}

bondMacaulayDuration <- function(today, maturityDate, annualYield, annualCouponRate, faceValue=100, periodicity=182, yearLength=360, dayOfWeekPayment=4) {
  res <- sapply(1:length(today), function(i) {
    info <- bondCashflow(today[i], maturityDate, annualYield, annualCouponRate, faceValue , periodicity, yearLength, dayOfWeekPayment)
    dur <- sum(as.numeric(info$cashflow)*info$discountFactor*as.numeric(info$paymentDates-today[i]))/bondPrice(today[i], maturityDate, annualYield, annualCouponRate, cleanPrice=FALSE, periodicity=periodicity, faceValue=faceValue, yearLength=yearLength, dayOfWeekPayment=dayOfWeekPayment)
    return(dur)
  })
  return(res)
}


