# OPTIMIST STATISTICAL FUNCTIONS

optimist_wt <- function(size, halflife){
  if(size < halflife){
    wt <- rep(1/size, size)
  } else{
    wt <- sapply(
      size:1, function(x) ifelse(
        x > (size-halflife), 
        (1/halflife)*((halflife-1)/halflife)^(size-halflife), 
        (1/halflife)*((halflife-1)/halflife)^(x-1)
      )
    )
  }
  return(wt)
}

optimist_mean <- function(object, halflife) UseMethod("optimist_mean", object)
optimist_mean.numeric <- function(object, halflife){
  if(missing(halflife)) halflife <- length(object)
  object <- na.omit(object)
  wt <- optimist_wt(length(object), halflife)
  return(sum(wt*object))
}
optimist_mean.matrix <- function(object, halflife){
  if(missing(halflife)) halflife <- nrow(object)
  return(sapply(1:(dim(object)[2]), function(j) optimist_mean(object[ ,j], halflife)))
}
optimist_mean.data.frame <- function(object, halflife){
  if(missing(halflife)) halflife <- nrow(object)
  res <- sapply(1:(dim(object)[2]), function(j) ifelse(is.numeric(object[ ,j]), optimist_mean(object[ ,j], halflife), NA))
  names(res) <- names(object)
  return(res)
}
optimist_mean.xts <- function(object, halflife){
  if(missing(halflife)) halflife <- nrow(object)
  return(optimist_mean(as.data.frame(object), halflife))
}

optimist_geomMean <- function(object, halflife) UseMethod("optimist_geomMean", object)
optimist_geomMean.numeric <- function(object, halflife){
  if(missing(halflife)) halflife <- length(object)
  object <- na.omit(object)
  wt <- optimist_wt(length(object), halflife)
  return(prod(object^wt))
}
optimist_geomMean.matrix <- function(object, halflife){
  if(missing(halflife)) halflife <- nrow(object)
  return(sapply(1:(dim(object)[2]), function(j) optimist_geomMean(object[ ,j], halflife)))
}
optimist_geomMean.data.frame <- function(object, halflife){
  if(missing(halflife)) halflife <- nrow(object)
  res <- sapply(1:(dim(object)[2]), function(j) ifelse(is.numeric(object[ ,j]), optimist_geomMean(object[ ,j], halflife), NA))
  names(res) <- names(object)
  return(res)
}
optimist_geomMean.xts <- function(object, halflife){
  if(missing(halflife)) halflife <- nrow(object)
  return(optimist_geomMean(as.data.frame(object), halflife))
}



optimist_var <- function(object, halflife) UseMethod("optimist_var", object)
optimist_var.numeric <- function(object, halflife){
  if(missing(halflife)) halflife <- length(object)
  mean <- optimist_mean(object, halflife)
  sq <- na.omit(object)^2
  wt <- optimist_wt(length(sq), halflife)
  var <- (sum(wt*sq)-mean^2)/(1-sum(wt^2)) # el coeficiente es para arreglar el sesgo
  return(var)
}
optimist_var.matrix <- function(object, halflife){
  if(missing(halflife)) halflife <- nrow(object)
  return(sapply(1:(dim(object)[2]), function(j) optimist_var(as.numeric(object[ ,j]), halflife)))
}
optimist_var.data.frame <- function(object, halflife){
  if(missing(halflife)) halflife <- nrow(object)
  res <- sapply(1:(dim(object)[2]), function(j) ifelse(is.numeric(object[ ,j]), optimist_var(object[ ,j], halflife), NA))
  names(res) <- names(object)
  return(res)
}
optimist_var.xts <- function(object, halflife){
  if(missing(halflife)) halflife <- nrow(object)
  return(optimist_var(as.data.frame(object), halflife))
}


optimist_cov <- function(object1, object2, halflife, cor) UseMethod("optimist_cov", object1)
optimist_cov.numeric <- function(object1, object2, halflife, cor=FALSE){
  if(missing(object2)) object2 <- object1
  if(missing(halflife)) halflife <- length(object)
  if(length(object1)!=length(object2)) stop("los vectores deben tener longitud igual")
  mean1 <- optimist_mean(object1, halflife)
  mean2 <- optimist_mean(object2, halflife)
  prod <- na.omit(object1*object2)
  wt <- optimist_wt(length(prod), halflife)
  cov <- (sum(wt*prod)-mean1*mean2)/(1-sum(wt^2)) # correccion de sesgo
  if(cor) cov <- cov/sqrt(optimist_var(object1, halflife)*optimist_var(object2, halflife))
  return(cov)
}
optimist_cov.matrix <- function(object, halflife, cor=FALSE){
  if(missing(halflife)) halflife <- nrow(object)
  nvar <- ncol(object)
  mat <- matrix(0, nrow=nvar, ncol=nvar)
  for(i in 1:nvar)
    for(j in i:nvar){
      mat[i, j] <- optimist_cov(as.numeric(object[ ,i]), as.numeric(object[ ,j]), halflife, cor)
      mat[j, i] <- mat[i, j]
    }
  dimnames(mat) <- list(dimnames(object)[[2]], dimnames(object)[[2]])
  return(mat)
}
optimist_cov.xts <- function(object, halflife, cor=FALSE){
  if(missing(halflife)) halflife <- nrow(object)
  return(optimist_cov(as.matrix(object), halflife, cor))
}
optimist_cov.data.frame <- function(object, halflife, cor=FALSE){
  numericCol <- sapply(names(object), function(x) is.numeric(object[ ,x]))
  object <- object[ ,numericCol]
  if(missing(halflife)) halflife <- nrow(object)
  return(optimist_cov(as.matrix(object), halflife, cor))
}

# # EJEMPLOS:
# #calcular medias, varianzas y covarianzas con o sin halflife de objetos xts y numericos
# db <- readRDS("../../sinteticos/Data/db_full.RDS")
# wt <- optimist_wt(100, 50)
# plot(wt, type="l")
# sum(wt)
# optimist_mean(db[ ,1:5])
# optimist_mean(iris) # puede recibir data frames y solo lo aplica a las columnas numericas
# optimist_mean(db[ ,1:5], halflife=10) # puede tener halflife
# optimist_var(db[ ,1:5], halflife=10)
# optimist_cov(db[ ,1:5], halflife=20, cor=TRUE)
# optimist_cov(db[ ,1:5], cor=TRUE)
# optimist_cov(iris, cor=TRUE) # cuando recibe un data frame busca automatico las columnas numericas 
# cor(iris[ ,-5]) # cuando no hay halflife, coincide con la correlacion normal, por eso era el coeficiente de correcion de sesgo
# optimist_cov(iris, cor=TRUE, halflife=10) 


