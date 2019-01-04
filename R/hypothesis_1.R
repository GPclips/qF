
#'Runs hypothesis 1
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'run_hypothesis1()
run_hypothesis1 <- function(d) {
	print("running hypothesis 1 ...")
	View(d, "Data Hypothesis 1")
	anscombeH1(d)
}

#'Returns data prepared for hypothesis 1
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'prepareDataH1()
prepareDataH1 <- function(){
	#create data frames for hypothesis
	dataH1 <- baseData %>% select(birthday, gender, requestedCountry, createdOn) 
	dataH1 <- dataH1 %>% mutate(enrolment = as.Date(dataH1$createdOn, "%d.%m.%Y")) 
	dataH1 <- dataH1 %>% mutate(birthday = as.Date(dataH1$birthday, "%d.%m.%Y")) 
	dataH1 <- dataH1 %>% mutate(age = dataH1$enrolment-dataH1$birthday) 
	dataH1 <- dataH1 %>% mutate(ageInYears = dataH1$age/365) 
	dataH1 <- dataH1 %>% mutate(ageInYears = round(ageInYears, digits = 0))
	return(dataH1)
}

#'Shows data of hypothesis 1
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'anscombeH1()
anscombeH1 <- function(d){
	bargraph(~ ageInYears | gender, requestedCountry,  data=d)
}