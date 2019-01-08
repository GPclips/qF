#'Prepares data for hypothesis 1
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'prepareDataH1()
prepareDataH1 <- function(){
	#create data frames for hypothesis
	cat("Prepare data for hypothesis one ...", fill=TRUE)
	dataH1 <- master_data_y3 %>% select(gender, requestedCountry) %>% removeMissings()
	dataH1 <- dataH1 %>% mutate(toJapan = ifelse(master_data_y3$requestedCountry == "Japan", "Yes", "No"))
	cat(nrow(dataH1), fill=TRUE, labels = "Using n entries for calculation: ")
	return(dataH1)
}

#'Bootstrap method to calculate percentage female going to Japan
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'calcPercentJapan()
calcPercentJapan <- function(x, index){
  #print(index)
  mySubset <- x[index,]
  
  tmpToJapan <- mySubset %>% filter(requestedCountry == "Japan")
  tmpFemaleToJapan <- tmpToJapan %>% filter(gender == "Female")
  
  #print(mySubset)
  return(nrow(tmpFemaleToJapan)/nrow(mySubset))
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

#'Bootstrap method to calculate abs female going to Japan
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'calcAbsJapan()
calcAbsJapan <- function(x, index){
  #print(index)
  mySubset <- x[index,]
  
  tmpToJapan <- mySubset %>% filter(requestedCountry == "Japan")
  tmpFemaleToJapan <- tmpToJapan %>% filter(gender == "Female")
  
  #print(mySubset)
  return(nrow(tmpFemaleToJapan))
}

#'Analyse percentage Japan
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'analyzePercentJapan()
analyzeAbsJapan<- function(d, i){
	set.seed(1911)
	pJ <- boot(d, statistic=calcAbsJapan, R=i)
	print(pJ)
	return(pJ)
}