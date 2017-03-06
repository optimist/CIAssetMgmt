# DATE UTILITIES
# Vamos a tratar de tener un approach mas organizado a todo el asunto de las fechas
library(lubridate)

isHoliday <- function(object) UseMethod("isHoliday", object)
isHoliday.Date <- function(date){
  us <- character(0)
#   us <- c(
#     "2014-01-01", "2014-01-20", "2014-02-17", "2014-04-18", "2014-05-26", "2014-07-04", "2014-09-01",
#     "2014-11-27", "2014-12-25", "2015-01-01", "2015-01-19", "2015-02-16", "2015-04-03", "2015-05-25",
#     "2015-07-03", "2015-09-07", "2015-11-26", "2015-12-25", "2016-01-01",               "2016-02-15", # BORRE "2016-01-18"
#     "2016-03-25", "2016-05-30",               "2016-09-05", "2016-11-24", "2016-12-26", "2017-01-02", # BORRE "2016-07-04"
#     "2017-01-16", "2017-02-20", "2017-04-14", "2017-05-29", "2017-07-04", "2017-09-04", "2017-11-23",
#     "2017-12-25", "2018-01-01", "2018-01-15", "2018-02-19", "2018-03-30", "2018-05-28", "2018-07-04",
#     "2018-09-03", "2018-11-22", "2018-12-25", "2019-01-01", "2019-01-21", "2019-02-18", "2019-04-19",
#     "2019-05-27", "2019-07-04", "2019-09-02", "2019-11-28", "2019-12-25", "2020-01-01", "2020-01-20",
#     "2020-02-17", "2020-04-10", "2020-05-25", "2020-07-03", "2020-09-07", "2020-11-26", "2020-12-25")
  mex <- c(
    "2014-01-01", "2014-02-03", "2014-03-17", "2014-04-17", "2014-04-18", "2014-05-01","2014-05-05",
    "2014-09-16", "2014-11-17", "2014-12-25", "2015-01-01", "2015-02-02","2015-03-16", "2015-04-02",
    "2015-04-03", "2015-05-01", "2015-05-05", "2015-09-16", "2015-11-02", "2015-11-17", "2015-12-25",
    "2016-01-01", "2016-02-01", "2016-03-21", "2016-03-24", "2016-03-25", "2016-05-05", "2016-09-16",
    "2016-11-02", "2016-11-21", "2016-12-25", "2016-12-12", "2017-01-01", "2017-02-06", "2017-03-20", "2017-04-13", 
    "2017-04-14", "2017-05-01", "2017-05-05", "2017-09-16", "2017-11-02", "2017-11-20", "2017-11-25", "2017-12-12")
  return((as.character(date) %in% c(us, mex)))
}

isWeekday <- function(object) UseMethod("isWeekday", object)
isWeekday.Date <- function(date) return(as.numeric(julian(date)%%7) %in% c(0, 1, 4:6))

isWorkingDay <- function(object) UseMethod("isWorkingDay", object)
isWorkingDay.Date <- function(date) return(!isHoliday(date) & isWeekday(date))

nextWorkingDay <- function(object, n) UseMethod("nextWorkingDay", object)
nextWorkingDay.Date <- function(date, n){
  nextWorkingDay <- date
  for(i in 1:length(nextWorkingDay)){
    count <- 0
    # the nice functionality is: if n=0 then will return same day if it is workingday otherwise next working day
    if(n==0 && !isWorkingDay(nextWorkingDay[i])) count <- -1
    while(count < n){
      if(isWorkingDay(nextWorkingDay[i]+1))
        count <- count + 1
      nextWorkingDay[i] <- nextWorkingDay[i] + 1
    }
  }
  return(nextWorkingDay)
}

prevWorkingDay <- function(object, n) UseMethod("prevWorkingDay", object)
prevWorkingDay.Date <- function(date, n){
  prevWorkingDay <- date
  for(i in 1:length(prevWorkingDay)){
    count <- 0
    # the nice functionality is: if n=0 then will return same day if it is workingday otherwise prev working day
    if(n==0 && !isWorkingDay(prevWorkingDay[i])) count <- -1
    while(count < n){
      if(isWorkingDay(prevWorkingDay[i]-1)) 
        count <- count+1
      prevWorkingDay[i] <- prevWorkingDay[i] - 1
    } 
  }
  return(prevWorkingDay)
}

# -----------------------------------------------------------------------------------------------------------
# ESTAS UTILIZAN DIRECTAMENTE LUBRIDATE PARA CAMBIAR EL MES PERO SIEMPRE PERO NO REGRESA NA'S... (ES SUPER UTIL)
# LUBRIDATE REGRESA NA SI NO EXISTE LA FECHA CON EL MES CAMBIADO, RESUELE EL PROBLEMA DE LOS DIAS 30, 31 Y FEBRERO

monthShift <- function(date, shift) UseMethod("monthShift")
monthShift.Date <- function(date, shift=-1){
  res <- structure(NULL, class="Date")
  for(index in (1:length(date))){
    i <- 0
    repeat({
      newdate <- (date[index]-i)+sign(shift)*months(abs(shift))
      if(is.na(newdate)) {i <-i+1} else break
    })  
    res <- c(res, newdate)
  }
  return(res)
}

# ------------------------------------------------------------------------------------------------

firstOfMonth <- function(object) UseMethod("firstOfMonth")
firstOfMonth.Date <- function(date) return(as.Date(paste(format(date, "%Y-%m"), "01", sep="-")))

lastOfMonth <- function(date) UseMethod("lastOfMonth") # estas son las mas utiles para invertir
lastOfMonth.Date <- function(date) return(as.Date(paste0(format(monthShift(date, 1), "%Y-%m-"), "01"))-1)

monthPrevWorkingDay <- function(date) UseMethod("monthPrevWorkingDay", date) # estas son las mas utiles para invertir
monthPrevWorkingDay.Date <- function(date){
  day(date) <- 1
  return(prevWorkingDay(date, 1))
}

yearPrevWorkingDay <- function(date, n) UseMethod("yearPrevWorkingDay", date) # estas son las mas utiles para invertir
yearPrevWorkingDay.Date <- function(date){
  day(date) <- 1; month(date) <- 1
  return(prevWorkingDay(date, 1))
}

monthLastWorkingDay <- function(date) UseMethod("monthLastWorkingDay") # estas son las mas utiles para invertir
monthLastWorkingDay.Date <- function(date) return(monthPrevWorkingDay(monthShift(date, 1)))

# ESTA ESTA EN LUBRIDATE PERO SOLO EN INGLES
monthName <- function(object, ...) UseMethod("monthName")
monthName.numeric <- function(n, language="es", abbreviate=FALSE){
  if(!(language %in% c("es", "en"))) stop("Language must be 'en' or 'es'")
  n <- (n-1)%%12 + 1
  dicc.es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  dicc.es.abb <- c("Ene.", "Feb.", "Mar.", "Abr.", "May.", "Jun.", "Jul.", "Ago.", "Sep.", "Oct.", "Nov.", "Dic.")
  dicc.en <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  dicc.en.abb <- c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec.")
  if(abbreviate) {dicc <- switch(language, "es"=dicc.es.abb, "en"=dicc.en.abb)} else {dicc <- switch(language, "es"=dicc.es, "en"=dicc.en)}
  return(dicc[n])
}
monthName.Date <- function(date, language="es", abbreviate=FALSE) return(monthName(month(date), language, abbreviate))


# LA VERDAD ESTAS DE ABAJO YA ESTAN DE MAS, CON LO DE ARRIBA ES SUFICIENTE....

monthNameYear <- function(object, language, sep) UseMethod("monthNameYear", object)
monthNameYear.Date <- function(date, language="es", sep="-"){
  if(!(language %in% c("es", "en"))) stop("Language must be 'en' or 'es'")
  return(paste(monthName(date, language), year(date), sep=sep))
}

yearMonth <- function(object, sep) UseMethod("yearMonth", object)
yearMonth.Date <- function(date, sep="-"){
  month <- month(date)
  monthChar <- as.character(month)
  for(i in 1:length(month)) if(month[i]<10) monthChar[i] <- paste0("0", monthChar[i])
  return(paste(year(date), monthChar, sep=sep))
}

yearMonthLabel <- function(object, language, sepInput, sepOutput) UseMethod("yearMonthLabel", object)
yearMonthLabel.character <- function(yearMonth, language="es", sepInput="-", sepOutput=" "){
  if(!(language %in% c("es", "en"))) stop("Language must be 'en' or 'es'")
  sepInput.length <- nchar(sepInput)
  year <- substr(yearMonth, 1, 4)
  month <- substr(yearMonth, 5+sepInput.length, 6+sepInput.length)
  monthName <- monthName(as.numeric(month), language)
  return(paste(year(date), month(date), sep=sepOutput))
}
 

