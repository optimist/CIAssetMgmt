
histComp <- function(date, group, percentage, group_label=group) {
  if(all(c(length(date), length(percentage), length(group), length(group_label))!=length(date))) stop("all lengths must be the same")
  if(!is.Date(date)|!is.character(group)|!is.numeric(percentage)) stop("wrong class of arguments")
  dt <- list(date=date, group=group, group_label=group_label, percentage=percentage)
  attr(dt, "class") <- "histComp"
  return(dt)
}

subset <- function(x, ...) UseMethod("subset") # ya existe esta funcion, por eso usamos este tipo de clases
subset.histComp <- function(comp, expr) {
  if(!is.logical(expr)) stop("expr must be a logical expression")
  subset <- histComp(date=comp$date[expr], group=comp$group[expr], percentage=comp$percentage[expr], group_label=comp$group_label[expr])
  return(subset)
}
tab <- function(comp, asDataTable=TRUE) UseMethod("tab")
tab.histComp <- function(comp, asDataTable=TRUE){
  data <- data.frame(date=comp$date, group=comp$group, group_label=comp$group_label, percentage=comp$percentage)
  if(asDataTable){
    data <- data.table(data)
    setkey(data, date)
  } 
  return(data)
}
tabWide <- function(comp) UseMethod("tabWide")
tabWide.histComp <- function(comp){
  data <- data.frame(date=comp$date, group=comp$group, group_label=comp$group_label, percentage=comp$percentage)
  data <- spread(data[ ,c("date", "group", "percentage")], key=group, value=percentage, convert = TRUE)
  data[is.na(data)] <- 0 
  data <- xts(data[ ,-1], order.by=data[ ,1])
  return(data)
}

compareNames <- function(comp, db) UseMethod("compareNames")
compareNames.histComp <- function(comp, db) {
  names <- unique(comp$group)
  id <- which(!(names %in% names(db)))
  varnames <- names[id]
  if(length(varnames)>0) {warning(sprintf("Variables %s not present in data", paste(varnames, collapse=", ")))} else(cat("All variables in data\n"))
  return(names[names %in% names(db)]) # regresa las variables si encontradas
}

fillPercentage <- function(comp, fillName) UseMethod("fillPercentage")
fillPercentage.histComp <- function(comp, fillName) {
  table <- tab(comp)
  allDates <- unique(table$date)
  fill <- table[ , 1-sum(percentage), by=date]
  fill <- data.frame(date=fill$date, group=fillName, group_label=fillName, percentage=fill$V1)
  table <- rbind(table, fill)
  return(histComp(date=table$date, group=as.character(table$group), group_label=as.character(table$group_label), percentage=table$percentage))
}

retContrib <- function(comp, ret, from=min(comp$date), to=max(comp$date), useLabel=TRUE, fixedReturnValue=NULL) UseMethod("retContrib")
retContrib <- function(comp, ret, from=min(comp$date), to=max(comp$date), useLabel=TRUE, fixedReturnValue=NULL) {
  uniqueDates <- index(ret)[index(ret) > from & index(ret)<=to]
  prevWD <- prevWorkingDay(uniqueDates, n=1)
  retTable <- data.frame()
  for(i in 1:length(uniqueDates)){
    groupvec <- comp$group[comp$date==prevWD[i]]
    compvec <- comp$percentage[comp$date==prevWD[i]]
    idx <- groupvec%in%names(ret)
    groupvec <- groupvec[idx]
    compvec <- compvec[idx]
    retvec <- ret[uniqueDates[i], groupvec]
    if(!is.null(fixedReturnValue)) retvec[1, ] <- fixedReturnValue
    contrib <- compvec*retvec
    if(length(groupvec)>0){
      if(useLabel){
        grouplabel <- comp$group_label[comp$date==prevWD[i]]
        grouplabel <- grouplabel[idx]
        names(contrib) <- grouplabel
      }
      retTable <- plyr::rbind.fill(retTable, data.frame(date=uniqueDates[i], contrib, check.names = FALSE))  
    } else{
      warning(sprintf("la fecha correspondiente al rendimiento %s y composiciOn %s no fue encontrada", uniqueDates[i], prevWD[i]))
    }
  }
  varid <- names(retTable)[-1]
  retTable[is.na(retTable)] <- 0
  retTable <- xts(retTable[ ,-1], retTable[ ,1])
  retTable <- rbind(xts(matrix(0, ncol=dim(retTable)[2]), order.by=from), retTable)
  names(retTable) <- varid
  attr(retTable, "class") <- c("ret", "xts", "zoo")
  return(retTable)
}

evalRet <- function(comp, ret, from=min(comp$group), to=max(comp$group)) UseMethod("evalRet")
evalRet <- function(comp, ret, from=min(comp$group), to=max(comp$group)) {
  contrib <- retContrib(comp, ret, from, to)
  evalRet <- apply(contrib, 1, sum, na.rm=TRUE)
  evalRet <- xts(evalRet, as.Date(index(contrib)))
  return(evalRet)
}


clavePrecio <- function(instrumento){
  res <- tolower(instrumento)
  res <- gsub("/", "", res)
  res <- gsub('\\*|\\+|\\-', "_", res)
  return(res)
}

createCompFromContrato <- function(dbposiciones, contrato){
  data <- dbposiciones[dbposiciones$contrato==contrato & dbposiciones$reporto=="D", ]
  if(nrow(data) > 0){
    date <- as.Date(data$fecha)
    data$tipo <- sapply(data$tipo, function(x) ifelse(nchar(x)<2, paste0(x, "_"), x))
    group <- clavePrecio(paste(data$tipo, data$emisora, data$serie, sep="/"))
    group_label <- paste(data$label_emisora, data$serie)
    percentage <- as.numeric(data$perc)
    comp <- histComp(date=date, group=group, group_label=group_label, percentage=percentage) %>% fillPercentage(fillName="Reporto")
  } else{
    dateIndex <- sort(unique(as.Date(dbposiciones$fecha)))
    comp <- histComp(date=dateIndex, group=rep("Reporto", length(dateIndex)), group_label=rep("Reporto", length(dateIndex)), percentage=rep(1,length(dateIndex)))
  }
  return(comp)
}

createCompFromCarteraModelo <- function(cm, group){
  data <- data.table(cm[cm$clave_grupo==group, ])[ ,porcentaje[1], by="fecha,subgrupo,clave_subgrupo,label_subgrupo"]
  data$label_subgrupo[is.na(data$clave_subgrupo)] <- "Otros"
  data$clave_subgrupo[is.na(data$clave_subgrupo)] <- "Otros"
  comp <- histComp(date=as.Date(data$fecha), group=data$clave_subgrupo, group_label=data$label_subgrupo, percentage=data$V1) %>% fillPercentage(fillName="Reporto")
  return(comp)
}

carteraModeloTable <- function(cm, date, groups){
  cm <- cm[which(cm$fecha==as.character(date)), ]
  table <- do.call("rbind.fill", lapply(groups, function(x){
    comp <- createCompFromCarteraModelo(cm, x) 
    vec <- comp$percentage
    names(vec) <- comp$group_label
    return(data.frame(t(vec), check.names = FALSE))
  }))
  table[is.na(table)] <- 0
  # if(fillReporto) table <- cbind(Reporto=apply(table, 1, function(x) 1-sum(x)) ,table)
  row.names(table) <- groups
  return(table)
}

# EJEMPLOS
# db <- readRDS("../../sinteticos/Data/db_full.RDS")
# ret <- dlyRet(db)
# test <- readRDS("../../sinteticos/Data/testigo_hist.RDS")
# cm <- readRDS("../../sinteticos/Data/carteramodelo_hist.RDS")
# test <- test[test$fecha!="0000-00-00", ]
# tail(test)
# comp <- createCompFromContrato(test, "25774")
# carteraModeloTable(cm_hist, "2016-07-12", c("Agr", "Mod", "Cons", "AgrRF", "ModRF", "ConsRF"))
# compareNames(comp, db)
# tab(comp)
# contrib <- retContrib(comp, ret, from=as.Date("2016-05-31"), to=as.Date("2016-06-30"))
# histRet <- evalRet(comp, ret, from=as.Date("2016-05-31"), to=as.Date("2016-06-30"))
# plot(accumulateRet(histRet))
# contribTotal <- accumulateRet(contrib)[nrow(contrib)]
