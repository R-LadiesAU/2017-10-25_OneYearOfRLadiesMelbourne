R-Ladies Melbourne - BioCAsia
================
Anna Quaglieri
15th May 2017

-   [Dowload tweets](#dowload-tweets)
    -   [Text cleaning](#text-cleaning)
-   [twitter timeline](#twitter-timeline)
-   [Dowload tweets with Rtweets package](#dowload-tweets-with-rtweets-package)
-   [Clean text](#clean-text)
    -   [Topic modeling](#topic-modeling)
-   [UseR! 2017](#user-2017)
    -   [After updating the 6 NAs](#after-updating-the-6-nas)
-   [abacbs](#abacbs)

RGB for R-Ladies Melbourne Flyer

``` r
knitr::opts_chunk$set(echo = TRUE,dev = "pdf")
```

``` r
library(twitteR)
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter():   dplyr, stats
    ## id():       dplyr, twitteR
    ## lag():      dplyr, stats
    ## location(): dplyr, twitteR

``` r
library(RCurl)
```

    ## Loading required package: bitops

    ## 
    ## Attaching package: 'RCurl'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     complete

``` r
library(ROAuth)
library(RJSONIO)
library(devtools)
library(rtweet)
```

    ## Welcome to rtweet v0.4.0!

    ## 
    ## Attaching package: 'rtweet'

    ## The following object is masked from 'package:twitteR':
    ## 
    ##     lookup_statuses

``` r
api_key<-'Tx6nu4td9a1L4Cy3WLYgtoxb9'
api_secret<- "VHr9jJIigl0Uvf55vf7K79uqwfiWKT5jJQJNPjstXXcj3wecne"
token <- "3108157034-Knm58WMZvymPlAAC4jhxTm2keWVHDx7XN9zCs2a"
token_secret <- "YWHNeceXflljBNjUCW99hsrY1OPV8e7euWIFVTsrmjTmP"

twitteR::setup_twitter_oauth(api_key, api_secret, token, token_secret)
```

Dowload tweets
==============

``` r
twittes_rladiesAU <- twitteR::searchTwitter("@RLadiesAU", n = 10000, lang= "en", since= "2016-10-01")
df <- twitteR::twListToDF(twittes_rladiesAU)


getUser_wrapper <- function(name_block){
  user_infos <- twitteR::lookupUsers(name_block, includeNA = FALSE)
  user_infosToDF <- twitteR::twListToDF(user_infos)
  return(user_infosToDF)
}

users <- getUser_wrapper(df$screenName)
```

Text cleaning
-------------

``` r
library(tm)
## Create a world clous from @atlsexyslim tweets
# Extract tweets 
some_txt <- df$text
some_txt <- iconv(some_txt, 'UTF-8', 'latin1', 'byte')
# Clean text
# remove punctuation
some_txt <- gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt <- gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt <- gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt <- gsub("[ \t]{2,}", "", some_txt)
some_txt <- gsub("^\\s+|\\s+$", "", some_txt)
some_txt <- gsub("\n", " ", some_txt)

# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(some_txt))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
"use", "see", "used", "via", "amp")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
# keep a copy for stem completion later
myCorpusCopy <- myCorpus
```

twitter timeline
================

``` r
tweets <- userTimeline("RLadiesAU", n = 3200)
df_tweets <- twitteR::twListToDF(tweets)
(n.tweet <- length(tweets))
writeLines(strwrap(df_tweets$text[10], 60))
```

Dowload tweets with Rtweets package
===================================

``` r
## name of app you created
appname <- "Rladies"

## api consumer
key <- 'Tx6nu4td9a1L4Cy3WLYgtoxb9'

## api comsumer secret 
secret <- "VHr9jJIigl0Uvf55vf7K79uqwfiWKT5jJQJNPjstXXcj3wecne"

## create a token, storing it as object 'twitter_token'
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret
)

##### Rladies AU
# Retrieve Tweets with Rladies in it
rla_tweet <- search_tweets("RLadiesAU OR #RLadiesAU OR @RLadiesAU", n = 3000,token = twitter_token)
sapply(rla_tweet,length)
rla_tweet$screen_name <- c(rla_tweet$screen_name,NA)
rla_tweet$user_id <- c(rla_tweet$user_id,NA)
rla_tweet$coordinates <- c(rla_tweet$coordinates,NA)
rla_tweet_df <- data.frame(screen_name=rla_tweet$screen_name,
                           user_id=rla_tweet$user_id,
                           created_at=rla_tweet$created_at,
                           status_id=rla_tweet$status_id,
                           text=rla_tweet$text,
                           retweet_count=rla_tweet$retweet_count,
                           favorite_count=rla_tweet$favorite_count,
                           is_quote_status=rla_tweet$is_quote_status,
                           quote_status_id=rla_tweet$quote_status_id,
                           do.call(cbind,rla_tweet[10:length(rla_tweet)]))

dir.create(file.path("twitter_data"),showWarnings = FALSE)
write.csv(rla_tweet_df,file.path("twitter_data",paste0("RLAU_search_tweets_",Sys.Date(),".csv")),row.names = FALSE)

## access and preview data on the users who posted the tweets
us <- users_data(rla_tweet) 
write.csv(us,file.path("twitter_data",paste0("RLAU_search_tweets_userData_",Sys.Date(),".csv")),row.names = FALSE)


## return 200 tweets from @KyloR3n's timeline
rladies_au_timeline <- get_timeline("RLadiesAU", n = 2000)
head(rladies_au_timeline)
write.csv(rladies_au_timeline,file.path("twitter_data",paste0("RLAU_timeline_",Sys.Date(),".csv")),row.names = FALSE)

## extract RladiesAu's user data
user_data_rldies_au <- users_data(rladies_au_timeline)
write.csv(user_data_rldies_au,file.path("twitter_data",paste0("RLAU_timeline_userData_",Sys.Date(),".csv")),row.names = FALSE)


####################################
##### All the mentions with R-Ladies
####################################
rladies <- search_users("RLadies", n = 10000)
rladies1 <- search_users("R-Ladies", n = 10000)
combine_rladies <- rbind(rladies,rladies1)[!duplicated(rbind(rladies,rladies1)),]
grep_rlad <- combine_rladies$screen_name[grep("RLadies",combine_rladies$screen_name)]
rlad_tweeters <- lookup_users(grep_rlad)
rlad <- tweets_data(rlad_tweeters)
# for everyone get latest 
```

``` r
rla_tweet_df <- read.csv(file.path("twitter_data",paste0("RLAU_search_tweets_","2017-11-03",".csv")))
us <- read.csv(file.path("twitter_data",paste0("RLAU_search_tweets_userData_","2017-11-03",".csv")))
rladies_au_timeline <- read.csv(file.path("twitter_data",paste0("RLAU_timeline_","2017-11-03",".csv")))
timeline_us <- read.csv(file.path("twitter_data",paste0("RLAU_timeline_userData_","2017-11-03",".csv")))

# combine tweet from timeline and tweet with R-Ladies AU
rla_tweet_df$source <- "tweetRLadiesAU"
rladies_au_timeline$source <- "timelineRLadiesAU"
colnames(rladies_au_timeline)[!(colnames(rladies_au_timeline) %in% colnames(rla_tweet_df))]
```

    ## character(0)

``` r
colnames(rla_tweet_df)[!(colnames(rla_tweet_df) %in% colnames(rladies_au_timeline))]
```

    ## character(0)

``` r
colnames(timeline_us)[!(colnames(timeline_us) %in% colnames(us))]
```

    ## character(0)

``` r
colnames(us)[!(colnames(us) %in% colnames(timeline_us))]
```

    ## character(0)

``` r
merge_tweet_time <- merge(rla_tweet_df,rladies_au_timeline,all=TRUE)
merge_tweet_time <- merge_tweet_time[!duplicated(merge_tweet_time),]
merge_tweet_time <- merge_tweet_time[!duplicated(merge_tweet_time[,colnames(merge_tweet_time) != "source"]),]
merge_users <- merge(us,timeline_us,all=TRUE)
```

    ## Warning in `[<-.factor`(`*tmp*`, ri, value = c(0L, 0L, 0L, 0L, 0L, 0L,
    ## 0L, : invalid factor level, NA generated

    ## Warning in `[<-.factor`(`*tmp*`, ri, value = c(0L, 0L, 0L, 0L, 0L, 0L,
    ## 0L, : invalid factor level, NA generated

    ## Warning in `[<-.factor`(`*tmp*`, ri, value = c(0L, 0L, 0L, 0L, 0L, 0L,
    ## 0L, : invalid factor level, NA generated

    ## Warning in `[<-.factor`(`*tmp*`, ri, value = c(0L, 0L, 0L, 0L, 0L, 0L,
    ## 0L, : invalid factor level, NA generated

    ## Warning in `[<-.factor`(`*tmp*`, ri, value = c(0L, 0L, 0L, 0L, 0L, 0L,
    ## 0L, : invalid factor level, NA generated

    ## Warning in `[<-.factor`(`*tmp*`, ri, value = c(0L, 0L, 0L, 0L, 0L, 0L,
    ## 0L, : invalid factor level, NA generated

    ## Warning in `[<-.factor`(`*tmp*`, ri, value = c(0L, 0L, 0L, 0L, 0L, 0L,
    ## 0L, : invalid factor level, NA generated

    ## Warning in `[<-.factor`(`*tmp*`, ri, value = c(0L, 0L, 0L, 0L, 0L, 0L,
    ## 0L, : invalid factor level, NA generated

``` r
merge_tweet_time_us <- merge(merge_tweet_time,merge_users[,c("user_id","name","screen_name","location","description","followers_count",
                                                             "friends_count","listed_count","created_at","time_zone")],all.x=TRUE)
```

The story that I wanna tell is that: - We are seeing an increasing in the number of followers both on twitter and participating to event - From meetup as well (Faces of one year of R-Ladies and their bios and introductions) - There is still a gap between people that participate and people that are followers - Who are our main followers/retweeters and how does that compares to other R-Ladies groups (R-Ladies London and San-Diego and a new one) - How do we get more people to come and how do we measure if we are making the difference? - Food and drinks! - survey to get to know better what they want (for example what we did for the intro to R) - mini hacks that will keep theme engaged - create a repository for women developers in R in Melbourne and tell their story and field - Is it possible to tell where are the packes made? and see how many come from here? - Has it changed the situation in useR! ? - Meetup bioconductor to start in Melbourne!

Clean text
==========

``` r
library(tm)
```

    ## Loading required package: NLP

    ## 
    ## Attaching package: 'NLP'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     annotate

``` r
## Create a world clous from @atlsexyslim tweets
# Extract tweets 
merge_tweet_time_us$text <- iconv(merge_tweet_time_us$text, 'UTF-8', 'latin1', 'byte')
# Clean text
# remove punctuation
merge_tweet_time_us$text <- gsub("[[:punct:]]", "", merge_tweet_time_us$text)
# remove numbers
merge_tweet_time_us$text <- gsub("[[:digit:]]", "", merge_tweet_time_us$text)
# remove html links
merge_tweet_time_us$text <- gsub("http\\w+", "", merge_tweet_time_us$text)
# remove unnecessary spaces
merge_tweet_time_us$text <- gsub("[ \t]{2,}", "", merge_tweet_time_us$text)
merge_tweet_time_us$text <- gsub("^\\s+|\\s+$", "", merge_tweet_time_us$text)
merge_tweet_time_us$text <- gsub("\n", " ", merge_tweet_time_us$text)
```

Topic modeling
--------------

UseR! 2017
==========

``` r
library(tidyverse)
user <- read.csv("useR2016/useR12017.csv")
user <- user %>% tidyr::separate(Speaker,into = c("Name","Surname"),sep=" ",remove=FALSE)
user <- user[!is.na(user$Name),]

library(genderizeR)
givenNames = data.frame(findGivenNames(user$Name, progress = FALSE))
givenNames$probability <- as.numeric(as.character(givenNames$probability))

write.csv(givenNames,"useR2016/genderised2017.csv")
sum(is.na(user$Name))

sum(user$Name == "")
user$Name <- tolower(user$Name)
user <- user[user$Name != "",]
dim(user)
length(unique(user$Name))

colnames(user)[which(colnames(user) == "Name")] <- "name"
user_genderised <- merge(user,givenNames,all.x = TRUE)

write.csv(user_genderised,"useR2016/genderised_users_initial.csv")
```

After updating the 6 NAs
------------------------

``` r
user <- read.csv("useR2016/genderised_users_NAupdated.csv")
user <- user[user$gender1 != "",]
table(as.character(user$gender1),useNA = "always")
```

    ## 
    ## female   male   <NA> 
    ##     68    188      1

``` r
table(user$Type.Track)
```

    ## 
    ##              Invited Talk           LT         Talk    Tutorial1 
    ##            4            5           64          141            2 
    ##   Tutorial10   Tutorial11   Tutorial12   Tutorial13   Tutorial14 
    ##            4            2            2            2            4 
    ##   Tutorial15   Tutorial16    Tutorial2    Tutorial3    Tutorial4 
    ##            2            4            2            4            2 
    ##    Tutorial5    Tutorial6    Tutorial7    Tutorial8    Tutorial9 
    ##            2            2            2            4            2

``` r
user$Talk.Type <- substr(as.character(user$Type.Track),1,5)
user$Talk.Type <- ifelse(user$Talk.Type == names(table(user$Talk.Type))[1],NA,user$Talk.Type)
table(user$Talk.Type)
```

    ## 
    ## Invit    LT  Talk Tutor 
    ##     5    64   141    42

``` r
user$Talk.Type <- factor(user$Talk.Type,levels=c("LT","Talk","Invit","Tutor"),
                         labels=c("Lightning","Talk","InvitedSpeaker","Tutorials"))

ggplot(user[!is.na(user$Talk.Type),],aes(x=as.character(gender1),fill=gender1)) + geom_bar(position="dodge") + facet_grid(~Talk.Type,scales = "free_x") + labs(x="Gender estimated with genderizeR") + theme_bw() + 
  ggtitle("useR! 2017") + scale_y_continuous(breaks=seq(0,130,25))
```

![](BioCAsia_files/figure-markdown_github-ascii_identifiers/genderised_userR2017-1.png)

``` r
library(ggrepel)
ggplot(user[!is.na(user$gender),],aes(x=count,y=probability,colour=gender1)) + geom_point() +
  geom_text_repel(data=user[user$probability < 0.8,],aes(x=count,y=probability,colour=gender1,label=name),hjust=0.5,vjust=-0.9,size=4) +
  theme_bw()
```

    ## Warning: Ignoring unknown parameters: hjust, vjust

    ## Warning: Removed 7 rows containing missing values (geom_point).

    ## Warning: Removed 8 rows containing missing values (geom_text_repel).

![](BioCAsia_files/figure-markdown_github-ascii_identifiers/genderised_userR2017-2.png)

``` r
table(user$probability,useNA = "always")
```

    ## 
    ##  0.5 0.51 0.52 0.59  0.6 0.63 0.69 0.79 0.81 0.84 0.85 0.87 0.88 0.89 0.92 
    ##    1    2    1    2    2    3    1    1    1    1    1    1    4    1    2 
    ## 0.93 0.94 0.95 0.96 0.97 0.98 0.99    1 <NA> 
    ##    1    1    5    3    4   11   37  163    8

abacbs
======

``` r
## name of app you created
appname <- "Rladies"

## api consumer
key <- 'Tx6nu4td9a1L4Cy3WLYgtoxb9'

## api comsumer secret 
secret <- "VHr9jJIigl0Uvf55vf7K79uqwfiWKT5jJQJNPjstXXcj3wecne"

## create a token, storing it as object 'twitter_token'
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret
)

##### Rladies AU
# Retrieve Tweets with Rladies in it
rla_tweet <- search_tweets("#abacbs17 OR #COMBINE17", n = 3000,token = twitter_token)
sapply(rla_tweet,length)
rla_tweet$screen_name <- c(rla_tweet$screen_name,NA)
rla_tweet$user_id <- c(rla_tweet$user_id,NA)
rla_tweet$coordinates <- c(rla_tweet$coordinates,NA)
rla_tweet_df <- data.frame(screen_name=rla_tweet$screen_name,
                           user_id=rla_tweet$user_id,
                           created_at=rla_tweet$created_at,
                           status_id=rla_tweet$status_id,
                           text=rla_tweet$text,
                           retweet_count=rla_tweet$retweet_count,
                           favorite_count=rla_tweet$favorite_count,
                           is_quote_status=rla_tweet$is_quote_status,
                           quote_status_id=rla_tweet$quote_status_id,
                           do.call(cbind,rla_tweet[10:length(rla_tweet)]))

dir.create(file.path("twitter_data"),showWarnings = FALSE)
write.csv(rla_tweet_df,file.path("twitter_data",paste0("abacbs2017_search_tweets_",Sys.Date(),".csv")),row.names = FALSE)

table(rla_tweet_df$screen_name)[order(table(rla_tweet_df$screen_name))]
```

``` r
library(tm)
library(RColorBrewer)

rla_tweet_df <- read.csv(file.path("twitter_data",paste0("abacbs2017_search_tweets_",Sys.Date(),".csv")))

# Extract tweets 
rla_tweet_df$text <- iconv(rla_tweet_df$text, 'UTF-8', 'latin1', 'byte')
# Clean text
# remove punctuation
rla_tweet_df$text <- gsub("[[:punct:]]", "", rla_tweet_df$text)
# remove numbers
rla_tweet_df$text <- gsub("[[:digit:]]", "", rla_tweet_df$text)
# remove html links
rla_tweet_df$text <- gsub("http\\w+", "", rla_tweet_df$text)
# remove unnecessary spaces
rla_tweet_df$text <- gsub("[ \t]{2,}", "", rla_tweet_df$text)
rla_tweet_df$text <- gsub("^\\s+|\\s+$", "", rla_tweet_df$text)
rla_tweet_df$text <- gsub("\n", " ", rla_tweet_df$text)
rla_tweet_df$text <- gsub("RT ", "", rla_tweet_df$text)

tweets <- unique(rla_tweet_df$text)

# Create objects for the worldcloud
corpus <- tm::Corpus(tm::VectorSource(tweets))

# Create termdocoumentmatrix
tdm <- tm::TermDocumentMatrix(corpus,
                    control = list(removePunctuation = TRUE,
        stopwords = c("machine", "learning", tm::stopwords("english")),
        removeNumbers = TRUE, tolower = TRUE))   
tdm
```

    ## <<TermDocumentMatrix (terms: 2766, documents: 931)>>
    ## Non-/sparse entries: 8218/2566928
    ## Sparsity           : 100%
    ## Maximal term length: 40
    ## Weighting          : term frequency (tf)

``` r
# define tdm as matrix
dm <- as.matrix(tdm)
# get word counts in decreasing order
word_freqs <- sort(rowSums(dm), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm <- data.frame(word = names(word_freqs), freq = word_freqs)

screen_names <- rla_tweet_df$screen_name

dm <- dm[-c(1,2),]
dm <- dm[!(rownames(dm) %in% c(as.character(screen_names),"higginsdes","lazappi","robbiebonelli","aliciaoshlack")), ]

dm <- dm[!(dm$word %in% c(as.character(screen_names),gsub("_","",as.character(screen_names)),"higginsdes","lazappi","robbiebonelli","aliciaoshlack")), ]

dm <- dm[!(dm$word %in% c("confbingo","talk","now")), ]


# Plot the word cloud
wordcloud::wordcloud(dm$word, dm$freq, min.freq=3,random.order=FALSE, max.words = 100, colors=brewer.pal(8, "Dark2"))
```

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : expression could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : joshuawkho could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : question could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : speaking could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : interesting could not be fit on page. It will not
    ## be plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : looking could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : much could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : andrew could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : tweeting could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : short could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : alignment could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : literature could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : winning could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : des could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : thank could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : sketchnotes could not be fit on page. It will not
    ## be plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : speed could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud::wordcloud(dm$word, dm$freq, min.freq = 3,
    ## random.order = FALSE, : different could not be fit on page. It will not be
    ## plotted.

![](BioCAsia_files/figure-markdown_github-ascii_identifiers/world_cloud-1.png)
