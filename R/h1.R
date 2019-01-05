#'Prepares data for hypothesis 1
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'prepareDataH1()
prepareDataH1 <- function(){
	#create data frames for hypothesis
	cat("Prepare data for hypothesis one ...", fill=TRUE)
	dataH1 <- master_data_y3 %>% select(gender, requestedCountry) %>% mutate(toJapan = ifelse(master_data_y3$requestedCountry == "Japan", "Yes", "No"))
	return(dataH1)
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

#'Analyse percentage Japan
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'analyzePercentJapan()
analyzePercentJapan<- function(d, i){
	set.seed(1911)
	pJ <- boot(d, statistic=calcPercentJapan, R=i)
	print(pJ)
	return(pJ)
}