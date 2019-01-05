#'Inits the project by loading the required resources
#'@param
#'@keywords init_projekt
#'@export
#'@examples
#'init_projekt()
init_projekt <- function() {
	
	print("load data resources")
	data(master_data_y3)
	data(students_amount_y10_GER)
	data(bipDeutschlandBis2017)
}

#'Plot LM using ggplot
#'@param xData, xLab, yLab, title
#'@keywords gpclips
#'@export
#'@examples
#'plotLm()
plotLm <- function(xData, xLab, yLab, title){
    
    gg <- ggplot(xData, aes(x=x, y=y)) + 
        geom_point() + 
        stat_smooth(method = "lm", col = "red") +
        labs(y=yLab, 
             x=xLab, 
             title=title)
    
    plot(gg)
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
  return(cov(mySubset$x, mySubset$y))
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
	return(c)
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
  return(cor(mySubset$x, mySubset$y))
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
	return(cc)
}