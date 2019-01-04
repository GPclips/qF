#'Prepares data for hypothesis 2
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'prepareDataH2()
prepareDataH2 <- function(){
	#create data frames for hypothesis
	print("Prepare data for hypothesis two ...")
	dataH2 <- merge(rename(bipDeutschlandBis2017, c("Jahr"="year", "bip_mrd_euro"="billion_eur")),rename(students_amount_y10, c("amount"="amountStudents")),by=c("year"))
	dataH2$billion_eur <- gsub("\\.", "", dataH2$billion_eur)
	dataH2$billion_eur <- gsub(",", "\\.", dataH2$billion_eur)
	dataH2 <- dataH2 %>% mutate(billion_eur = as.double(dataH2$billion_eur))
	return(dataH2)
}

#'Bootstrap method to calculate correlation coefficient
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'calcanalyseCorrelationCoefficient()
calcanalyseCorrelationCoefficient <- function(x, index){
  #choose bootstrap sub set
  mySubset <- x[index,]
  #return calculation
  return(cor(mySubset$billion_eur, mySubset$amountStudents))
}

#'Analyse correlation coefficient
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'analyseCorrelationCoefficient()
analyseCorrelationCoefficient <- function(d, i){
	set.seed(1911)
	cc <- boot(d, statistic=calcanalyseCorrelationCoefficient, R=i)
	print(cc)
	histogram(~ cc$t, xlab = "Correlation Coefficient", ylab = "Density", main="Histogram: COR(Amount Students/Year, BIP/Year [Billion Euros])")
}

#'Bootstrap method to calculate covariance
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'calcanalyseCovariance()
calcanalyseCovariance <- function(x, index){
  #choose bootstrap sub set
  mySubset <- x[index,]
  #return calculation
  return(cov(mySubset$billion_eur, mySubset$amountStudents))
}

#'Analyse covariance
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'analyzeCovariance()
analyzeCovariance<- function(d, i){
	set.seed(1911)
	c <- boot(d, statistic=calcanalyseCovariance, R=i)
	print(c)
	histogram(~ c$t, xlab = "Covariance", ylab = "Density", main="Histogram: COV(Amount Students/Year, BIP/Year [Billion Euros])")
}

#'Plot LM for hypothesis 2
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'plotLmH2()
plotLmH2 <- function(x){
    
    gg <- ggplot(x, aes(x=billion_eur, y=amountStudents)) + 
        geom_point() + 
        stat_smooth(method = "lm", col = "red") +
        labs(y="Amount Students/Year", 
             x="BIP (Billion Euros)", 
             title="LM: Amount Students/Year ~ BIP [Billion Euros]", 
             caption = "")
    
    plot(gg)
}
