dateInfoTableMonthly <- function(object, language, monthYearSep) UseMethod("dateInfoTableMonthly", object)
dateInfoTableMonthly.Date <- function(dateSeries, language="es", monthYearSep="-"){
  # El proposito de esta funcion es en el rango de fechas especificados decir cuantos dias laborales hubo, primer y ultimo dia laboral
  if(!(language %in% c("es", "en"))) stop("Language must be 'en' or 'es'")
  table <- dateInfoTable(dateSeries, language, monthYearSep)[ ,c("yearMonth", "monthPrevWD", "monthLastWD")]
  table <- apply(table[ ,-1], 2, FUN=function(col) tapply(col, INDEX=table$yearMonth, function(x) x[1]))
  return(table)
}