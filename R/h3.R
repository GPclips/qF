#'Prepares data for hypothesis 3
#'@param
#'@keywords gpclips
#'@export
#'@examples
#'prepareDataH3()
prepareDataH3 <- function(){
	#create data frames for hypothesis
	cat("Prepare data for hypothesis three ...", fill=TRUE)
	dataH3 <- merge(rename(bipDeutschlandBis2017, c("Jahr"="year", "bip_mrd_euro"="billion_eur")),rename(students_amount_y10_GER, c("amount"="amountStudents")),by=c("year"))
	dataH3$billion_eur <- gsub("\\.", "", dataH3$billion_eur)
	dataH3$billion_eur <- gsub(",", "\\.", dataH3$billion_eur)
	dataH3 <- dataH3 %>% mutate(billion_eur = as.double(dataH3$billion_eur))
	dataH3 <- dataH3 %>% mutate(x = billion_eur, y = amountStudents)
	return(dataH3)
}

