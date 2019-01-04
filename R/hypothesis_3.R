#'Prepares data for hypothesis 3
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'prepareDataH3()
prepareDataH3 <- function(){
	#create data frames for hypothesis
	print("Prepare data for hypothesis three ...")
	dataH3 <- master_data_y3 %>% select(gender, requestedCountry) %>% mutate(toJapan = ifelse(master_data_y3$requestedCountry == "Japan", "Yes", "No"))
	return(dataH3)
}

#'Bootstrap method to calculate percentage going to Japan
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'calcPercentJapan()
calcPercentJapan <- function(x, index){
  #print(index)
  mySubset <- x[index,]
  #print(mySubset)
  return(prop(~toJapan, success = "Yes", data = mySubset))
}

#'Analyse percent Japan
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'analyzePercentJapan()
analyzePercentJapan<- function(d, i){
	set.seed(1911)
	pJ <- boot(d, statistic=calcPercentJapan, R=i)
	print(pJ)
	histogram(~ pJ$t, xlab = "Percent Female", ylab = "Density", main="Histogram: Percent Female going to Japan")
}