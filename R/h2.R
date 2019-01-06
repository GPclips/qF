
#'Returns data prepared for hypothesis 1
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'prepareDataH2()
prepareDataH2 <- function(){
	cat("Prepare data for hypothesis two ...", fill=TRUE)
	#Filter for Deals
	#Filter for closed oppertunities = Deal
	dataH2 <- master_data_y3 %>% select(birthday, gender, createdOn, X1stProgramInvoiceSent) %>% removeMissings()
	dataH2 <- dataH2 %>% mutate(OpportunityDate = as.Date(dataH2$createdOn, "%d.%m.%Y"))
	dataH2 <- dataH2 %>% mutate(dealDate = as.Date(dataH2$X1stProgramInvoiceSent, "%d.%m.%Y"))
	#Calculate Age
	dataH2 <- dataH2 %>% mutate(birthday = as.Date(dataH2$birthday, "%d.%m.%Y")) 
	dataH2 <- dataH2 %>% mutate(ageInDays = dataH2$OpportunityDate-dataH2$birthday) 
	dataH2 <- dataH2 %>% mutate(ageInYears = dataH2$ageInDays/365) 
	dataH2 <- dataH2 %>% mutate(ageInYears = round(ageInYears, digits = 0))
	dataH2 <- dataH2 %>% mutate(ageInYears = as.numeric(ageInYears))

	#Calculate Delta
	dataH2 <- dataH2 %>% mutate(timeSpanOpportunityToDeal = dataH2$dealDate-dataH2$OpportunityDate)
	dataH2 <- dataH2 %>% mutate(timeSpanOpportunityToDeal = as.numeric(timeSpanOpportunityToDeal))
	#remove data with negative deal date
	dataH2 <- dataH2 %>% filter(timeSpanOpportunityToDeal >= 0)
	dataH2 <- dataH2 %>% mutate(x = ageInYears, y = timeSpanOpportunityToDeal)
	cat(nrow(dataH2), fill=TRUE, labels = "Using n entries for calculation: ")
	return(dataH2)
}