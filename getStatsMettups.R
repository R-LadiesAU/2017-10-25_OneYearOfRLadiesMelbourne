## extract names from meetups
# Adelaide, SA: does not exist
#install.packages("tidyverse")
#install.packages("genderizeR")
require("tidyverse")
require("genderizeR")
require("purrr")
require("rvest")
require("stringr")


get_gender <- function(event_url){

	event_page <- 
    event_url %>% read_html()

	member_nodes <- 
	    event_page %>%
	    html_nodes(".member-name")

	name_text_with_plusses <-
	    member_nodes %>% 
	    map(. %>% html_text() %>% str_trim() # str_extract()
	        ) %>%
	    unlist()

	plusses <- sum(!is.na(str_extract(name_text_with_plusses,"[1-9]")))

	givenNames = data.frame(findGivenNames(name_text_with_plusses, progress = FALSE,apikey="a902c2108f17334e5a2d596c676d4fb9"))
	givenNames$probability <- as.numeric(as.character(givenNames$probability))

	mean_prob <- data.frame(givenNames) %>% group_by(gender) %>% summarise(Npeople=length(gender),
		ProbEstFemale=mean(probability),
		Proportion=length(gender)/nrow(givenNames)) %>% mutate(TotalPart=sum(Npeople))
	mean_prob$unknown <- plusses
	
	list(TableResults=mean_prob,
		givenNames=givenNames)

}


meetups <- data.frame(Name=c("MoreJoyofText","Writing&managingRpackages","BuildingDashboardsInRwithFlexdashboard",
	"ExploratoryDataAnalysisPlotly","RunningPCAWhatWeCanLearnFromIt",
	"RShinyWebAppplatform","VisualAnalytics&GraphicsinR","AwesomeReproducibleScienceWithR",
	"DataWrangling1","LaunchMeet-up",
	"AnalystFirst","MiningtheInternetofEverything","RSmorgasbord","Real-timeOperationalAdvancedAnalytics",
	"BigDataIntegrationResearch",

	"HRUG Short Talks","WorkingwithRAnAdvancedWorkshop","ElementaryStatisticsfromanAdvancedRStandpoint",
	"ToolDevelopmentMulti-dimensionaldata",
	"GovHackHeathCareandR","ExploratoryDataAnalysisR","R&FeatureEngineering&transformation","LogisticModelTree",
	"5th Perth R meeting","BuildingEasy&InteractiveDashboardsR","VIPSpecialEventGrahamWilliams","EnergyForecastingwithSQLServer",
	"MachineLearning&VisualisationOKaggleData","AllaboutMicrosoftRServer",
	"2016AugRMeetUp","2016AprilRMeetUp","HadleyWickhamPurePredictablePipeable","2015SeptemberRMeetUp",
	"MachineLearningTake1","2ndMeetingWellingtonR-UsersGroup","LounchR-Ladies"),

	address=c("http://www.meetup.com/en-AU/MelbURN-Melbourne-Users-of-R-Network/events/232814925/",
		"http://www.meetup.com/en-AU/MelbURN-Melbourne-Users-of-R-Network/events/232814895/",
		"http://www.meetup.com/en-AU/MelbURN-Melbourne-Users-of-R-Network/events/232570843/",
		"http://www.meetup.com/en-AU/MelbURN-Melbourne-Users-of-R-Network/events/231012632/",
		"http://www.meetup.com/en-AU/MelbURN-Melbourne-Users-of-R-Network/events/229982137/",
		"http://www.meetup.com/it-IT/Brisbane-Users-of-R-Group-BURGr/events/234408874/",
		"http://www.meetup.com/it-IT/Brisbane-Users-of-R-Group-BURGr/events/233553749/",
		"http://www.meetup.com/it-IT/Brisbane-Users-of-R-Group-BURGr/events/232613455/",
		"http://www.meetup.com/it-IT/Brisbane-Users-of-R-Group-BURGr/events/231924394/",
		"http://www.meetup.com/it-IT/Brisbane-Users-of-R-Group-BURGr/events/231557839/",
		"http://www.meetup.com/it-IT/Canberra-R-Users-Group/events/232093239/",
		"http://www.meetup.com/it-IT/Canberra-R-Users-Group/events/222204742/",
		"http://www.meetup.com/it-IT/Canberra-R-Users-Group/events/221207111/",
		"http://www.meetup.com/it-IT/Canberra-R-Users-Group/events/220318789/",
		"http://www.meetup.com/it-IT/Canberra-R-Users-Group/events/220176795/",
		"http://www.meetup.com/it-IT/Hobart-R-Users-Group/events/228739114/",
		"http://www.meetup.com/it-IT/Hobart-R-Users-Group/events/192532412/",
		"http://www.meetup.com/it-IT/Hobart-R-Users-Group/events/194103962/",
		"http://www.meetup.com/it-IT/Hobart-R-Users-Group/events/173644602/",
		"http://www.meetup.com/it-IT/Western-Australia-R-Group-WARG/events/232586691/",
		"http://www.meetup.com/it-IT/Western-Australia-R-Group-WARG/events/230729785/",
		"http://www.meetup.com/it-IT/Western-Australia-R-Group-WARG/events/228916401/",
		"http://www.meetup.com/it-IT/Western-Australia-R-Group-WARG/events/228019349/",
		"http://www.meetup.com/it-IT/Western-Australia-R-Group-WARG/events/226094167/",
		"http://www.meetup.com/it-IT/R-Users-Sydney/events/233633497/",
		"http://www.meetup.com/it-IT/R-Users-Sydney/events/233626412/",
		"http://www.meetup.com/it-IT/R-Users-Sydney/events/233027554/",
		"http://www.meetup.com/it-IT/R-Users-Sydney/events/232215724/",
		"http://www.meetup.com/it-IT/R-Users-Sydney/events/230457972/",
		"http://www.meetup.com/it-IT/Auckland-R-Users-Group-AKLRUG/events/233225680/",
		"http://www.meetup.com/it-IT/Auckland-R-Users-Group-AKLRUG/events/230217616/",
		"http://www.meetup.com/it-IT/Auckland-R-Users-Group-AKLRUG/events/226683948/",
		"http://www.meetup.com/it-IT/Auckland-R-Users-Group-AKLRUG/events/224702972/",
		"http://www.meetup.com/it-IT/Auckland-R-Users-Group-AKLRUG/events/220878189/",
		"http://www.meetup.com/it-IT/Wellington-R-Users-Group-WRUG/events/141249592/?eventId=141249592&a=ea1_grp&rv=ea1&_af_eid=141249592&_af=event&rv=ea1",
		"https://www.meetup.com/it-IT/R-Ladies-Melbourne/events/234683544/"),

	group=c("Melbourne-Users-of-R-Network","Melbourne-Users-of-R-Network","Melbourne-Users-of-R-Network","Melbourne-Users-of-R-Network",
		"Melbourne-Users-of-R-Network","Brisbane-Users-of-R-Network","Brisbane-Users-of-R-Network","Brisbane-Users-of-R-Network",
		"Brisbane-Users-of-R-Network","Brisbane-Users-of-R-Network","Camberra-R-User-Group","Camberra-R-User-Group","Camberra-R-User-Group",
		"Camberra-R-User-Group","Camberra-R-User-Group","Hobart-R-User-Group","Hobart-R-User-Group",
		"Hobart-R-User-Group","Hobart-R-User-Group","Western-Australia-R-Group-WARG","Western-Australia-R-Group-WARG",
		"Western-Australia-R-Group-WARG","Western-Australia-R-Group-WARG","Western-Australia-R-Group-WARG",
		"R-Users-Sydney","R-Users-Sydney","R-Users-Sydney","R-Users-Sydney","R-Users-Sydney",
		"Auckland-R-Users-Group","Auckland-R-Users-Group","Auckland-R-Users-Group","Auckland-R-Users-Group","Auckland-R-Users-Group",
		"Wellington-R-Users-Group","R-Ladies-Melbourne"),
	Number=c(5,4,3,2,1,5,4,3,2,1, 5,4,3,2,1, 4,3,2,1, 5,4,3,2,1, 5,4,3,2,1, 5,4,3,2,1,1,1),
	City=c(rep(c("Melbourne","Brisbane","Camberra","Hobart","Perth","Sydney","Auckland","Wellington","Melbourne-Ladies"),times=c(5,5,5,4,5,5,5,1,1))))
meetups$key <- paste(meetups$City,meetups$Number,sep="_")

# Run function
urls <- as.character(meetups[,"address"])
# names(urls) <- meetups[,"key"]
# list_estimates <- urls %>% map(. %>% get_gender)
# saveRDS(list_estimates,"~/estimates_meetup_gender.rds")
event_url <- "https://www.meetup.com/it-IT/R-Ladies-Melbourne/events/234683544/"
event_url <- urls[1]

# Rladies <- get_gender(event_url)
# saveRDS(Rladies,"~/Rladies_stats.rds")
rladies <- readRDS("/Users/quaglieri.a/Documents/varie/Rladies/Rladies_stats.rds")
est <- readRDS("/Users/quaglieri.a/Documents/varie/Rladies/estimates_meetup_gender.rds")
## Get all the members of the group
# you need to get the offset of every group



