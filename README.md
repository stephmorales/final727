---
title: "Assessing the Mental Health Consequences of the 2021 Military Coup in Burma"
subtitle: "Fundamentals of Computing and Data Display"
author: "Htay-Wah Saw, Nicolas Rodriguez, Stephanie Morales"
date: "`r Sys.Date()`"
output:
  pdf_document:
    toc: yes
    df_print: kable
---
This project utilized the following libraries:

```{r, include = FALSE}
library(knitr)
library(tidyverse)
library(gtrendsR)
library(magrittr)
library(readxl)
library(sentimentr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
```

## Introduction

On February 1, 2021, the Burmese military perpetrated a coup and violently overthrew a civilian government that was overwhelmingly elected by the electorates in the 2020 November election. Following the coup, millions of civilians from all walks of life across the country took to the street and expressed their strong disapproval of the coup through peaceful protests. Hundreds of thousands of government employees have joined the Civil Disobedience Movement (CDM), refusing to continue to work under the military government. The military harshly responded to the nationwide rejection of the coup with excessive use of force, lethal weapons, and extreme violence against unarmed civilians. This has resulted in extreme violence against anti-coup protesters, gross violation of fundamental human rights, bombardment of residential areas, arbitrary killing of civilians and children, looting, burning, and destruction of private properties, using civilians as human shields, and arbitrary arrests of political prisoners by the Burmese military. The coup also triggered a plethora of humanitarian crises across the country, which include: (1) closure of banks, hospitals, clinics, schools; (2) limited access to basic healthcare; (3) soaring commodity prices; (4) hundreds of thousands of civilians who are internally displaced; (5) loss of unemployment and livelihoods; (6) disruption of communications due to internet cutoff and travel restriction.  

We recently fielded an online survey among the Burmese adult population aged 18 and above between October 7-14, which aimed to assess the socioeconomic, physical and mental health consequences of the coup. Prior studies reveal that the negative impact of violence, armed conflicts and political unrest on population mental health can be immediate and substantial. In this paper, we reported findings from the online survey focusing on the mental health consequences of the coup on the Burmese adult population. 

We also use data collected from the Google API to measure interest over time in the words "depression", "anxiety", and "military coup" in Myanmar. This was done to see whether there was an increase for these searches after the coup. 

This paper is an exploratory study that looks to identify anxiety and depression symptoms among the Burmese population. Our research contributes to the literature by addressing the need for studies on the consequences of armed conflicts and mental health. This study is the first, to the best of our knowledge, to offer timely data and analysis on this topic. This research also evaluates the use of online data, which is easy to collect, cheap, and widely available. We compare our online data collected from the Google API to our survey data to gauge the efficacy and accuracy of using online data. 

We address two main research questions in this paper:

  1. Is there a significant prevalence of anxiety/depression among the Burmese population as a result of the      coup?
  
  2. How does social media data compare to survey data?
  
  
## Gathering Survey Data

We fielded an online Qualtrics survey among Burmese adult population between October 7, 2021 - October 14, 2021 for a total of 8 consecutive days using Facebook platform for sampling. Specifically, we ran Facebook advertisements prepared in Burmese targeting 20 million Facebook users in Burma aged 18 and above. The Facebook advertisements appeared on top of a user’s Facebook page in their news feed. A mouse click on the advertisement led a Facebook user to our online survey website where respondents provided consent and completed the online survey in a secured manner. The Facebook sample was randomly chosen by Facebook’s built-in algorithms. A total of 1.33 million active Facebook users in Burma aged 18 and above saw our advertisements at least once between October 7, 2021 - October 14, 2021. Of those who saw the Facebook advertisements at least once during this time period, 31,015 clicked the link to our online survey. Of those who clicked the link, 7,720 completed the online survey for a completion rate of 25%. The Facebook platform has been recently used for collecting population data during public health emergencies like COVID-19 (Kreuter, Barkay 2020; Salomon, Reinhart, 2021). We found the use of Facebook platform to get access to the population to be extremely useful during political upheavals like those currently unfolding in Burma where timely population data are urgently needed for swift humanitarian responses and where in-person interviews are not possible due to security, operational, logistical, and cost reasons. Our online survey covered numerous topics including demographics, mental health, access to basic healthcare, COVID-19, employment, food insecurity, social safety net, trust in the banking system, sources of information used to learn about the crisis, and trust in those information sources. The online survey took about 16 minutes to complete. Respondents completed the online survey using their tablets, smartphones, laptops, or PCs, and were compensated MMK 2,000 each (about 1.12 US$) for their time participating in the study and completing the survey. 

We measured the mental health outcome using the Patient Health Questionnaire-4 (PHQ4). This brief four-question survey was developed by Kroenke (2009) and has been established by Lowe (2010) as a valid and reliable measure of anxiety (prevalence of feeling “nervous, anxious, or on edge” and not being able to “stop or control worrying”) and depression (prevalence of feeling “down, depressed, or hopeless” and “little interest or pleasure in doing things”) over the past fourteen days. The response scale for each of the items are “not at all”, “several days”, “more than half the days”, and “nearly every day”. The scores for each of the four items are totaled to analyze overall mental health outcome, while the two conditions (depression, anxiety) can be summed and scored separately to analyze the severity of each condition. We also asked respondents to self-evaluate their overall mental health on a 5-point scale (“Excellent”, “Very Good”, “Good”, “Fair”, “Poor”). We conducted all of our statistical analyses in Stata.

## Collecting data from the Google API 

We used data from the Google trends API to measure interest over time using R. The Google trends API is a public platform that allows users to measure interest over time for specific topics or search terms across Google search. For our analysis, we chose the terms "depression", "anxiety", "military coup", "sad", "democracy", and "dictatorship" in Myanmar. We wanted to see whether there was an increase in the search for these words after the coup occurred as compared to before the coup. Specifically, we looked at the months of January through November in 2021. The code for our analysis is found below.

```{r, include = FALSE}
res1 <- gtrends(c("depression", "anxiety", "sad", "military coup"), geo = "MM", 
               time = "2020-11-28 2021-11-28", low_search_volume = T)

res2 <- gtrends(c("military coup", "democracy", "dictatorship"), geo = "MM", 
               time = "2020-11-28 2021-11-28", low_search_volume = T)
plot(res1)
plot(res2)

str(res1)
str(res2)

#transforming data frame into tibble
library(tidyverse)
library(magrittr)
res_time1 <- as_tibble(res1$interest_over_time)
res_time2 <- as_tibble(res2$interest_over_time)
glimpse(res_time1)
glimpse(res_time2)

```

```{r, include = FALSE}

plot(res1)

```

```{r, include = FALSE}

plot(res2)

```

## Sentiment Analysis

As previously mentioned, our Qualtrics survey included the open ended question "if you have any comments or suggestions related to the current situation in Burma please enter them in the text box provided below". The comments on the aforementioned question provided textual data that was used to conduct a sentiment analysis to gauge whether respondents feelings were positive or negative and allowed us to visually establish which words were the found the most frequently among the comments. Our second source of textual data was derived from the Facebook advertisement that linked respondents to the survey on Qualtrics. The Facebook comments on the survey advertisement post were saved and analyzed using sentiment analysis to see which were negative and positive and which words were the most frequently used. All of our textual data was downloaded and saved into an excel file. The comments and open-ended responses were in Burmese, which were translated to English by qualified and experienced translators. The translated excel files were then imported into R, where they were cleaned, organized, and prepared for the sentiment analysis. The coding process and output is found below:

We first read in the excel files.

```{r, include=FALSE}
#Read excel files

c_facebook <- read_excel("facebook_comments.xlsx") #Facebook comments
oq_facebook <- read_excel("facebook_OQ.xlsx") #open-ended survey responses

```

Then, we conducted a sentiment analysis on the Facebook comments found on the Facebook post that was used to advertise the survey.

```{r, include=FALSE}
#Sentiment analysis Facebook comments

get_sentences(c_facebook$facebook_comments)
analysis_c <- extract_sentiment_terms(c_facebook$facebook_comments)
senti_commentsFB <- sentiment_by(c_facebook$facebook_comments)
c_facebook$ave_sentiment=senti_commentsFB$ave_sentiment

```

Sentiment analysis of the open-ended responses in the survey followed.

```{r, include=FALSE}

#Sentiment analysis open-ended responses

get_sentences(oq_facebook$oq_facebook)
analysis_oq <- extract_sentiment_terms(oq_facebook$oq_facebook)
senti_oq <- sentiment_by(oq_facebook$oq_facebook)
oq_facebook$ave_sentiment=senti_oq$ave_sentiment


# create a new variable from Facebook comments
SentimentAnalysis_c <- c_facebook %>% mutate(senti_commentsFB = case_when(ave_sentiment < 0 ~ 'Negative',
                           ave_sentiment == 0 ~ 'Neutral',
                           TRUE ~ 'Positive'))

SentimentAnalysis_c <- SentimentAnalysis_c %>% mutate(Color = case_when(ave_sentiment < 0 ~ 'Red',
                           ave_sentiment == 0 ~ 'Blue',
                           TRUE ~ 'Yellow'))
SentimentAnalysis_c$word_count=senti_commentsFB$word_count

# create a new variable from open-ended survey responses
SentimentAnalysis_oq <- oq_facebook %>% mutate(senti_oq = case_when(ave_sentiment < 0 ~ 'Negative',
                           ave_sentiment == 0 ~ 'Neutral',
                           TRUE ~ 'Positive'))

SentimentAnalysis_oq <- SentimentAnalysis_oq %>% mutate(Color = case_when(ave_sentiment < 0 ~ 'Red',
                           ave_sentiment == 0 ~ 'Blue',
                           TRUE ~ 'Yellow'))
SentimentAnalysis_oq$word_count=senti_oq$word_count
```


```{r, include=FALSE}
# Difference of means between Facebook comments and open-ended survey responses
summary(SentimentAnalysis_c$ave_sentiment)
summary(SentimentAnalysis_oq$ave_sentiment)
t.test(SentimentAnalysis_c$ave_sentiment,SentimentAnalysis_oq$ave_sentiment) 
```

Density plots were created to better visualize the results.

```{r, include=FALSE}

#Density plots

fc <- ggplot() + geom_density(
      mapping = aes(ave_sentiment),
      data = SentimentAnalysis_c,
      colour = "red",
      )

fc + ggtitle("Density Plot - Facebook comments") +
  xlab("Sentiment") + ylab("Density") +
  theme(
plot.title = element_text(color="red", size=14, face="bold.italic"),
axis.title.x = element_text(color="#993333", size=12, face="bold"),
axis.title.y = element_text(color="#993333", size=12, face="bold"))


oq <- ggplot() + geom_density(
      mapping = aes(ave_sentiment),
      data = SentimentAnalysis_oq,
      colour = "red",
      )

oq + ggtitle("Density Plot - open question") +
  xlab("Sentiment") + ylab("Density") +
  theme(
plot.title = element_text(color="red", size=14, face="bold.italic"),
axis.title.x = element_text(color="#993333", size=12, face="bold"),
axis.title.y = element_text(color="#993333", size=12, face="bold"))

```

Bar graphs were also created for visualization purposes.

```{r, include=FALSE}

fc2 <- ggplot(SentimentAnalysis_c) +
  geom_bar(mapping = aes(x = senti_commentsFB, fill = senti_commentsFB))

fc2 + ggtitle("Sentiment analysis - Facebook comments") +
  xlab("Sentiment") + ylab("Frequency") + labs(fill = "Sentiment") +
  theme(
plot.title = element_text(color="red", size=14, face="bold.italic"),
axis.title.x = element_text(color="#993333", size=12, face="bold"),
axis.title.y = element_text(color="#993333", size=12, face="bold"))


oq2 <- ggplot(SentimentAnalysis_oq) +
  geom_bar(mapping = aes(x = senti_oq, fill = senti_oq))

oq2 + ggtitle("Sentiment analysis - open question") +
  xlab("Sentiment") + ylab("Frequency") + labs(fill = "Sentiment") +
  theme(
plot.title = element_text(color="red", size=14, face="bold.italic"),
axis.title.x = element_text(color="#993333", size=12, face="bold"),
axis.title.y = element_text(color="#993333", size=12, face="bold"))

```

Furthermore, a word cloud was constructed.

```{r, include=FALSE}

### sentiment analysis from Facebook comments
#Create a vector containing only the text
text_n <- analysis_c$negative
text_p <- analysis_c$positive
# Create a corpus  
docs_n <- Corpus(VectorSource(text_n))
docs_p <- Corpus(VectorSource(text_p))

###open-ended survey responses
#Create a vector containing only the text
text_ne <- analysis_oq$negative
text_po <- analysis_oq$positive

# Create a corpus  
docs_ne <- Corpus(VectorSource(text_ne))
docs_po <- Corpus(VectorSource(text_po))


gsub("[[:punct:]]", "", analysis_c$negative)
gsub("character", "", analysis_c$negative)

gsub("[[:punct:]]", "", analysis_c$positive)
gsub("character", "", analysis_c$positive)

gsub("[[:punct:]]", "", analysis_oq$negative)
gsub("character", "", analysis_oq$negative)

gsub("[[:punct:]]", "", analysis_oq$positive)
gsub("character", "", analysis_oq$positive)

``` 

We cleaned our textual data to make our text clouds cleaner and more legible. 

```{r, include=FALSE}

### sentiment analysis from Facebook comments
#negative
docs_n <- docs_n %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs_n <- tm_map(docs_n, content_transformer(tolower))
docs_n <- tm_map(docs_n, removeWords, stopwords("english"))

#positive
docs_p <- docs_p %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs_p <- tm_map(docs_p, content_transformer(tolower))
docs_p <- tm_map(docs_p, removeWords, stopwords("english"))


###open-ended survey responses
#negative
docs_ne <- docs_ne %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs_ne <- tm_map(docs_ne, content_transformer(tolower))
docs_ne <- tm_map(docs_ne, removeWords, stopwords("english"))

#positive
docs_po <- docs_po %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs_po <- tm_map(docs_po, content_transformer(tolower))
docs_po <- tm_map(docs_po, removeWords, stopwords("english"))

```

```{r, include=FALSE}
# negative - facebook comments
dtm_n <- TermDocumentMatrix(docs_n) 
matrix_n <- as.matrix(dtm_n) 
words_n <- sort(rowSums(matrix_n),decreasing=TRUE) 
df_n <- data.frame(word_n = names(words_n),freq_n=words_n)
df_n <- df_n[-c(1), ]
rownames(df_n) <- NULL

# negative - open-ended survey responses
dtm_ne <- TermDocumentMatrix(docs_ne) 
matrix_ne <- as.matrix(dtm_ne) 
words_ne <- sort(rowSums(matrix_ne),decreasing=TRUE) 
df_ne <- data.frame(word_ne = names(words_ne),freq_ne=words_ne)
df_ne <- df_ne[-c(1), ]
rownames(df_ne) <- NULL

df_ne$word_ne<-recode(df_ne$word_ne,"cwrong"="wrong")
df_ne$word_ne<-recode(df_ne$word_ne,"cwould"="would")
df_ne$word_ne<-recode(df_ne$word_ne,"cworst"="worst")
df_ne$word_ne<-recode(df_ne$word_ne,"cworse"="worse")
df_ne$word_ne<-recode(df_ne$word_ne,"cworrying"="worrying")
df_ne$word_ne<-recode(df_ne$word_ne,"cworry"="worry")
df_ne$word_ne<-recode(df_ne$word_ne,"cworried"="worried")
df_ne$word_ne<-recode(df_ne$word_ne,"cwasted"="wasted")
df_ne$word_ne<-recode(df_ne$word_ne,"cwar"="war")
df_ne$word_ne<-recode(df_ne$word_ne,"cviolence"="violence")
df_ne$word_ne<-recode(df_ne$word_ne,"cviolation"="violation")
df_ne$word_ne<-recode(df_ne$word_ne,"cusurped"="usurped")
df_ne$word_ne<-recode(df_ne$word_ne,"curgent"="urgent")
df_ne$word_ne<-recode(df_ne$word_ne,"cupset"="upset")
df_ne$word_ne<-recode(df_ne$word_ne,"cuproot"="uproot")
df_ne$word_ne<-recode(df_ne$word_ne,"cuprising"="cuprising")
df_ne$word_ne<-recode(df_ne$word_ne,"cunwarranted"="unwarranted")
df_ne$word_ne<-recode(df_ne$word_ne,"cunjustly"="unjustly")
df_ne$word_ne<-recode(df_ne$word_ne,"cunjust"="unjust")
df_ne$word_ne<-recode(df_ne$word_ne,"cunhappily"="unhappily")
df_ne$word_ne<-recode(df_ne$word_ne,"cunfortunate"="unfortunate")
df_ne$word_ne<-recode(df_ne$word_ne,"cunemployment"="unemployment")
df_ne$word_ne<-recode(df_ne$word_ne,"cunemployed"="unemployed")
df_ne$word_ne<-recode(df_ne$word_ne,"cuncontrollably"="uncontrollably")
df_ne$word_ne<-recode(df_ne$word_ne,"cuncertainty"="uncertainty")
df_ne$word_ne<-recode(df_ne$word_ne,"cuncertain"="uncertain")
df_ne$word_ne<-recode(df_ne$word_ne,"cunauthorized"="unauthorized")
df_ne$word_ne<-recode(df_ne$word_ne,"ctroubles"="troubles")
df_ne$word_ne<-recode(df_ne$word_ne,"ctrouble"="trouble")
df_ne$word_ne<-recode(df_ne$word_ne,"ctreatment"="treatment")
df_ne$word_ne<-recode(df_ne$word_ne,"ctorturing"="torturing")
df_ne$word_ne<-recode(df_ne$word_ne,"ctortures"="tortures")
df_ne$word_ne<-recode(df_ne$word_ne,"ctorture"="torture")
df_ne$word_ne<-recode(df_ne$word_ne,"ctiring"="tiring")
df_ne$word_ne<-recode(df_ne$word_ne,"ctired"="tired")
df_ne$word_ne<-recode(df_ne$word_ne,"cthreatening"="threatening")
df_ne$word_ne<-recode(df_ne$word_ne,"cthreatened"="threatened")
df_ne$word_ne<-recode(df_ne$word_ne,"cterrorist"="terrorist")
df_ne$word_ne<-recode(df_ne$word_ne,"cterrible"="terrible")
df_ne$word_ne<-recode(df_ne$word_ne,"ctears"="tears")
df_ne$word_ne<-recode(df_ne$word_ne,"csuffering"="suffering")
df_ne$word_ne<-recode(df_ne$word_ne,"csuffer"="suffer")
df_ne$word_ne<-recode(df_ne$word_ne,"csue"="sue")
df_ne$word_ne<-recode(df_ne$word_ne,"csubjected"="subjected")
df_ne$word_ne<-recode(df_ne$word_ne,"cstruggling"="struggling")
df_ne$word_ne<-recode(df_ne$word_ne,"cstop"="stop")
df_ne$word_ne<-recode(df_ne$word_ne,"cstarving"="starving")
df_ne$word_ne<-recode(df_ne$word_ne,"cstarved"="starved")
df_ne$word_ne<-recode(df_ne$word_ne,"cstarve"="starve")
df_ne$word_ne<-recode(df_ne$word_ne,"csorry"="sorry")
df_ne$word_ne<-recode(df_ne$word_ne,"cslowly"="slowly")
df_ne$word_ne<-recode(df_ne$word_ne,"cslow"="slow")
df_ne$word_ne<-recode(df_ne$word_ne,"cshot"="shot")
df_ne$word_ne<-recode(df_ne$word_ne,"cshortages"="shortages")
df_ne$word_ne<-recode(df_ne$word_ne,"cshooting"="shooting")
df_ne$word_ne<-recode(df_ne$word_ne,"cshoot"="shoot")
df_ne$word_ne<-recode(df_ne$word_ne,"csentenced"="sentenced")
df_ne$word_ne<-recode(df_ne$word_ne,"cseized"="seized")
df_ne$word_ne<-recode(df_ne$word_ne,"cscared"="scared")
df_ne$word_ne<-recode(df_ne$word_ne,"cscarcity"="scarcity")
df_ne$word_ne<-recode(df_ne$word_ne,"cscarce"="scarce")
df_ne$word_ne<-recode(df_ne$word_ne,"csaddened"="saddened")
df_ne$word_ne<-recode(df_ne$word_ne,"csad"="sad")
df_ne$word_ne<-recode(df_ne$word_ne,"crushing"="rushing")
df_ne$word_ne<-recode(df_ne$word_ne,"crush"="rush")
df_ne$word_ne<-recode(df_ne$word_ne,"cruined"="ruined")
df_ne$word_ne<-recode(df_ne$word_ne,"cruin"="ruin")
df_ne$word_ne<-recode(df_ne$word_ne,"crobbery"="robbery")
df_ne$word_ne<-recode(df_ne$word_ne,"crevolt"="revolt")
df_ne$word_ne<-recode(df_ne$word_ne,"crestricted"="restricted")
df_ne$word_ne<-recode(df_ne$word_ne,"cresist"="resist")
df_ne$word_ne<-recode(df_ne$word_ne,"creject"="reject")
df_ne$word_ne<-recode(df_ne$word_ne,"crebellion"="rebellion")
df_ne$word_ne<-recode(df_ne$word_ne,"crebel"="rebel")
df_ne$word_ne<-recode(df_ne$word_ne,"cquit"="quit")
df_ne$word_ne<-recode(df_ne$word_ne,"cdictatorship"="dictatorship")
df_ne$word_ne<-recode(df_ne$word_ne,"cgovernment"="government")
df_ne$word_ne<-recode(df_ne$word_ne,"cdifficult"="difficult")
df_ne$word_ne<-recode(df_ne$word_ne,"cuprising"="uprising")
df_ne$word_ne<-recode(df_ne$word_ne,"cpunishment"="punishment")
df_ne$word_ne<-recode(df_ne$word_ne,"cprotests"="protests")
df_ne$word_ne<-recode(df_ne$word_ne,"cprotest"="protest")
df_ne$word_ne<-recode(df_ne$word_ne,"cprosecuted"="prosecuted")
df_ne$word_ne<-recode(df_ne$word_ne,"cproblems"="problems")
df_ne$word_ne<-recode(df_ne$word_ne,"cprisoners"="prisoners")
df_ne$word_ne<-recode(df_ne$word_ne,"cprevent"="prevent")
df_ne$word_ne<-recode(df_ne$word_ne,"cpressures"="pressures")
df_ne$word_ne<-recode(df_ne$word_ne,"cpressure"="pressure")
df_ne$word_ne<-recode(df_ne$word_ne,"cpoorer"="poorer")
df_ne$word_ne<-recode(df_ne$word_ne,"cpoor"="poor")
df_ne$word_ne<-recode(df_ne$word_ne,"cplight"="light")
df_ne$word_ne<-recode(df_ne$word_ne,"cperpetuation"="perpetuation")
df_ne$word_ne<-recode(df_ne$word_ne,"cpanic"="panic")
df_ne$word_ne<-recode(df_ne$word_ne,"cpandemic"="pandemic")
df_ne$word_ne<-recode(df_ne$word_ne,"coverthrow"="overthrow")
df_ne$word_ne<-recode(df_ne$word_ne,"corruption"="corruption")
df_ne$word_ne<-recode(df_ne$word_ne,"coppression"="oppression")
df_ne$word_ne<-recode(df_ne$word_ne,"coppressed"="oppressed")
df_ne$word_ne<-recode(df_ne$word_ne,"copposition"="opposition")
df_ne$word_ne<-recode(df_ne$word_ne,"coppose"="oppose")
df_ne$word_ne<-recode(df_ne$word_ne,"cons"="cons")
df_ne$word_ne<-recode(df_ne$word_ne,"coffensive"="offensive")
df_ne$word_ne<-recode(df_ne$word_ne,"cnegotiate"="negotiate")
df_ne$word_ne<-recode(df_ne$word_ne,"cnegative"="negative")
df_ne$word_ne<-recode(df_ne$word_ne,"cmurderous"="murderous")
df_ne$word_ne<-recode(df_ne$word_ne,"cmurder"="murder")
df_ne$word_ne<-recode(df_ne$word_ne,"cmotherfucker"="motherfucker")
df_ne$word_ne<-recode(df_ne$word_ne,"cmisrepresented"="misrepresented")
df_ne$word_ne<-recode(df_ne$word_ne,"clost"="lost")
df_ne$word_ne<-recode(df_ne$word_ne,"cloot"="loot")
df_ne$word_ne<-recode(df_ne$word_ne,"cloathe"="loathe")
df_ne$word_ne<-recode(df_ne$word_ne,"cleave"="leave")
df_ne$word_ne<-recode(df_ne$word_ne,"clawless"="lawless")
df_ne$word_ne<-recode(df_ne$word_ne,"claunch"="launch")
df_ne$word_ne<-recode(df_ne$word_ne,"clate"="late")
df_ne$word_ne<-recode(df_ne$word_ne,"clacking"="lacking")
df_ne$word_ne<-recode(df_ne$word_ne,"clack"="lack")
df_ne$word_ne<-recode(df_ne$word_ne,"ckilling"="killing")
df_ne$word_ne<-recode(df_ne$word_ne,"ckilled"="killed")
df_ne$word_ne<-recode(df_ne$word_ne,"ckill"="kill")
df_ne$word_ne<-recode(df_ne$word_ne,"cjunta"="junta")
df_ne$word_ne<-recode(df_ne$word_ne,"cirrelevant"="irrelevant")
df_ne$word_ne<-recode(df_ne$word_ne,"cirrational"="irrational")
df_ne$word_ne<-recode(df_ne$word_ne,"cintervention"="intervention")
df_ne$word_ne<-recode(df_ne$word_ne,"cinterrogated"="interrogated")
df_ne$word_ne<-recode(df_ne$word_ne,"cinsecurity"="insecurity")
df_ne$word_ne<-recode(df_ne$word_ne,"cinsecure"="insecure")
df_ne$word_ne<-recode(df_ne$word_ne,"cinjustices"="injustices")
df_ne$word_ne<-recode(df_ne$word_ne,"cinjustice"="injustice")
df_ne$word_ne<-recode(df_ne$word_ne,"cinflation"="inflation")
df_ne$word_ne<-recode(df_ne$word_ne,"cindiscriminate"="indiscriminate")
df_ne$word_ne<-recode(df_ne$word_ne,"cincident"="incident")
df_ne$word_ne<-recode(df_ne$word_ne,"cimposed"="imposed")
df_ne$word_ne<-recode(df_ne$word_ne,"cignore"="ignore")
df_ne$word_ne<-recode(df_ne$word_ne,"cignorance"="ignorance")
df_ne$word_ne<-recode(df_ne$word_ne,"chill"="hill")
df_ne$word_ne<-recode(df_ne$word_ne,"chelplessness"="helplessness")
df_ne$word_ne<-recode(df_ne$word_ne,"cheinous"="heinous")
df_ne$word_ne<-recode(df_ne$word_ne,"cheating"="cheating")
df_ne$word_ne<-recode(df_ne$word_ne,"chatred"="hatred")
df_ne$word_ne<-recode(df_ne$word_ne,"chate"="hate")
df_ne$word_ne<-recode(df_ne$word_ne,"chard"="hard")
df_ne$word_ne<-recode(df_ne$word_ne,"cgun"="gun")
df_ne$word_ne<-recode(df_ne$word_ne,"cgreed"="greed")
df_ne$word_ne<-recode(df_ne$word_ne,"cgrave"="grave")
df_ne$word_ne<-recode(df_ne$word_ne,"cgenocide"="genoide")
df_ne$word_ne<-recode(df_ne$word_ne,"cforeign"="foreign")
df_ne$word_ne<-recode(df_ne$word_ne,"cforced"="forced")
df_ne$word_ne<-recode(df_ne$word_ne,"cforce"="force")
df_ne$word_ne<-recode(df_ne$word_ne,"cflee"="flee")
df_ne$word_ne<-recode(df_ne$word_ne,"cfired"="fired")
df_ne$word_ne<-recode(df_ne$word_ne,"cfighting"="fighting")
df_ne$word_ne<-recode(df_ne$word_ne,"cfight"="fight")
df_ne$word_ne<-recode(df_ne$word_ne,"cfear"="fear")
df_ne$word_ne<-recode(df_ne$word_ne,"cfascist"="fascist")
df_ne$word_ne<-recode(df_ne$word_ne,"cfascism"="fascism")
df_ne$word_ne<-recode(df_ne$word_ne,"cfallen"="fallen")
df_ne$word_ne<-recode(df_ne$word_ne,"cfall"="fall")
df_ne$word_ne<-recode(df_ne$word_ne,"cfailure"="failure")
df_ne$word_ne<-recode(df_ne$word_ne,"cfail"="fail")
df_ne$word_ne<-recode(df_ne$word_ne,"cexpensive"="expensive")
df_ne$word_ne<-recode(df_ne$word_ne,"cexhaustion"="exhaustion")
df_ne$word_ne<-recode(df_ne$word_ne,"cescape"="escape")
df_ne$word_ne<-recode(df_ne$word_ne,"cerupted"="erupted")
df_ne$word_ne<-recode(df_ne$word_ne,"ceradication"="eradication")
df_ne$word_ne<-recode(df_ne$word_ne,"ceradicate"="eradicate")
df_ne$word_ne<-recode(df_ne$word_ne,"cepidemic"="epidemic")
df_ne$word_ne<-recode(df_ne$word_ne,"cenough"="enough")
df_ne$word_ne<-recode(df_ne$word_ne,"ceconomics"="economics")
df_ne$word_ne<-recode(df_ne$word_ne,"ceconomic"="economics")
df_ne$word_ne<-recode(df_ne$word_ne,"cdying"="dying")
df_ne$word_ne<-recode(df_ne$word_ne,"cdomestic"="domestic")
df_ne$word_ne<-recode(df_ne$word_ne,"cdislike"="dislike")
df_ne$word_ne<-recode(df_ne$word_ne,"cdisgusted"="disgusted")
df_ne$word_ne<-recode(df_ne$word_ne,"cdisappointed"="disappointed")
df_ne$word_ne<-recode(df_ne$word_ne,"cdire"="dire")
df_ne$word_ne<-recode(df_ne$word_ne,"cdifficulties"="difficulties")
df_ne$word_ne<-recode(df_ne$word_ne,"cdied"="died")
df_ne$word_ne<-recode(df_ne$word_ne,"cdie"="die")
df_ne$word_ne<-recode(df_ne$word_ne,"cdictatorial"="dictatorial")
df_ne$word_ne<-recode(df_ne$word_ne,"cdictator"="dictator")
df_ne$word_ne<-recode(df_ne$word_ne,"cdeterioration"="deterioration")
df_ne$word_ne<-recode(df_ne$word_ne,"cdeteriorating"="deteriorating")
df_ne$word_ne<-recode(df_ne$word_ne,"cdestruction"="destruction")
df_ne$word_ne<-recode(df_ne$word_ne,"cdestroyed"="destroyed")
df_ne$word_ne<-recode(df_ne$word_ne,"cdestroy"="destroy")
df_ne$word_ne<-recode(df_ne$word_ne,"cdesperately"="desperately")
df_ne$word_ne<-recode(df_ne$word_ne,"cdespair"="despair")
df_ne$word_ne<-recode(df_ne$word_ne,"cdepression"="depression")
df_ne$word_ne<-recode(df_ne$word_ne,"cdepressed"="depressed")
df_ne$word_ne<-recode(df_ne$word_ne,"cdepreciation"="depreciation")
df_ne$word_ne<-recode(df_ne$word_ne,"cdemolish"="demolish")
df_ne$word_ne<-recode(df_ne$word_ne,"cdefeat"="defeat")
df_ne$word_ne<-recode(df_ne$word_ne,"cdecreasing"="decreasing")
df_ne$word_ne<-recode(df_ne$word_ne,"cdecrease"="decrease")
df_ne$word_ne<-recode(df_ne$word_ne,"cdanger"="danger")
df_ne$word_ne<-recode(df_ne$word_ne,"ccuts"="cuts")
df_ne$word_ne<-recode(df_ne$word_ne,"ccut"="cut")
df_ne$word_ne<-recode(df_ne$word_ne,"cculprit"="culprit")
df_ne$word_ne<-recode(df_ne$word_ne,"ccruelly"="cruelly")
df_ne$word_ne<-recode(df_ne$word_ne,"ccrisis"="crisis")
df_ne$word_ne<-recode(df_ne$word_ne,"cconflicts"="conflicts")
df_ne$word_ne<-recode(df_ne$word_ne,"cconfiscation"="confiscation")
df_ne$word_ne<-recode(df_ne$word_ne,"cconcerns"="concerns")
df_ne$word_ne<-recode(df_ne$word_ne,"cconcerned"="concerned")
df_ne$word_ne<-recode(df_ne$word_ne,"ccommits"="commits")
df_ne$word_ne<-recode(df_ne$word_ne,"ccollapses"="collapses")
df_ne$word_ne<-recode(df_ne$word_ne,"ccollapse"="collapse")
df_ne$word_ne<-recode(df_ne$word_ne,"ccold"="cold")
df_ne$word_ne<-recode(df_ne$word_ne,"ccatastrophes"="catastrophes")
df_ne$word_ne<-recode(df_ne$word_ne,"cburning"="burning")
df_ne$word_ne<-recode(df_ne$word_ne,"cburn"="burn")
df_ne$word_ne<-recode(df_ne$word_ne,"cbrutally"="brutally")
df_ne$word_ne<-recode(df_ne$word_ne,"cbrutal"="brutal")
df_ne$word_ne<-recode(df_ne$word_ne,"cbitch"="bitch")
df_ne$word_ne<-recode(df_ne$word_ne,"cbad"="bad")
df_ne$word_ne<-recode(df_ne$word_ne,"cawful"="awful")
df_ne$word_ne<-recode(df_ne$word_ne,"catrocities"="atrocities")
df_ne$word_ne<-recode(df_ne$word_ne,"cashamed"="ashamed")
df_ne$word_ne<-recode(df_ne$word_ne,"cartillery"="artillery")
df_ne$word_ne<-recode(df_ne$word_ne,"carresting"="arresting")
df_ne$word_ne<-recode(df_ne$word_ne,"carrested"="arrested")
df_ne$word_ne<-recode(df_ne$word_ne,"carrest"="arrest")
df_ne$word_ne<-recode(df_ne$word_ne,"carbitrary"="arbitrary")
df_ne$word_ne<-recode(df_ne$word_ne,"captive"="captive")
df_ne$word_ne<-recode(df_ne$word_ne,"canxiety"="anxiety")
df_ne$word_ne<-recode(df_ne$word_ne,"canti"="anti")
df_ne$word_ne<-recode(df_ne$word_ne,"cambitions"="ambitions")
df_ne$word_ne<-recode(df_ne$word_ne,"cafraid"="afraid")
df_ne$word_ne<-recode(df_ne$word_ne,"caccused"="accused")
df_ne$word_ne<-recode(df_ne$word_ne,"cabuses"="abuses")
df_ne$word_ne<-recode(df_ne$word_ne,"cabandon"="abandon")


df_n$word_n<-recode(df_n$word_n,"cbad"="bad")
df_n$word_n<-recode(df_n$word_n,"cbreaking"="breaking")
df_n$word_n<-recode(df_n$word_n,"cdestroyed"="destroyed")
df_n$word_n<-recode(df_n$word_n,"cdevastation"="devastation")
df_n$word_n<-recode(df_n$word_n,"cdifficult"="difficult")
df_n$word_n<-recode(df_n$word_n,"ceconomic"="economic")
df_n$word_n<-recode(df_n$word_n,"cfight"="fight")
df_n$word_n<-recode(df_n$word_n,"cgovernment"="government")
df_n$word_n<-recode(df_n$word_n,"churt"="hurt")
df_n$word_n<-recode(df_n$word_n,"cimpossible"="impossible")
df_n$word_n<-recode(df_n$word_n,"creckless"="reckless")
df_n$word_n<-recode(df_n$word_n,"csad"="sad")
df_n$word_n<-recode(df_n$word_n,"cweak"="weak")
df_n$word_n<-recode(df_n$word_n,"cweakness"="weakness")
df_n$word_n<-recode(df_n$word_n,"cworried"="worried")
df_n$word_n<-recode(df_n$word_n,"cabsence"="absence")
df_n$word_n<-recode(df_n$word_n,"caftermath"="aftermath")
df_n$word_n<-recode(df_n$word_n,"cbother"="bother")
df_n$word_n<-recode(df_n$word_n,"cbreaking"="breaking")
df_n$word_n<-recode(df_n$word_n,"cbrutality"="brutality")
df_n$word_n<-recode(df_n$word_n,"cchallenge"="challenge")
df_n$word_n<-recode(df_n$word_n,"ccontrary"="contrary")
df_n$word_n<-recode(df_n$word_n,"cdevastation"="devastation")
df_n$word_n<-recode(df_n$word_n,"cego"="ego")
df_n$word_n<-recode(df_n$word_n,"cfatigue"="fatigue")
df_n$word_n<-recode(df_n$word_n,"cfire"="fire")
df_n$word_n<-recode(df_n$word_n,"cfraud"="fraud")
df_n$word_n<-recode(df_n$word_n,"churt"="hurt")
df_n$word_n<-recode(df_n$word_n,"cimpossible"="impossible")
df_n$word_n<-recode(df_n$word_n,"cliar"="liar")
df_n$word_n<-recode(df_n$word_n,"cmanipulation"="manipulation")
df_n$word_n<-recode(df_n$word_n,"cpain"="pain")
df_n$word_n<-recode(df_n$word_n,"creckless"="reckless")
df_n$word_n<-recode(df_n$word_n,"crecklessly"="recklessly")
df_n$word_n<-recode(df_n$word_n,"cset"="set")
df_n$word_n<-recode(df_n$word_n,"csuffered"="suffered")
df_n$word_n<-recode(df_n$word_n,"cthief"="thief")
df_n$word_n<-recode(df_n$word_n,"cunhappy"="unhappy")
df_n$word_n<-recode(df_n$word_n,"cunreasonable"="unreasonable")
df_n$word_n<-recode(df_n$word_n,"cwait"="wait")
df_n$word_n<-recode(df_n$word_n,"cweak"="weak")
df_n$word_n<-recode(df_n$word_n,"cweakness"="weakness")
df_n$word_n<-recode(df_n$word_n,"cdefeat"="defeat")
df_n$word_n<-recode(df_n$word_n,"cdestruction"="destruction")
df_n$word_n<-recode(df_n$word_n,"cfailure"="failure")
df_n$word_n<-recode(df_n$word_n,"chard"="hard")
df_n$word_n<-recode(df_n$word_n,"chate"="hate")
df_n$word_n<-recode(df_n$word_n,"cjunta"="junta")
df_n$word_n<-recode(df_n$word_n,"clawless"="lawless")
df_n$word_n<-recode(df_n$word_n,"cloot"="loot")
df_n$word_n<-recode(df_n$word_n,"clost"="lost")
df_n$word_n<-recode(df_n$word_n,"cshooting"="shooting")
df_n$word_n<-recode(df_n$word_n,"cwrong"="wrong")


df_n <- aggregate(df_n$freq_n, by=list(word_n=df_n$word_n), FUN=sum)
df_ne <- aggregate(df_ne$freq_ne, by=list(word_ne=df_ne$word_ne), FUN=sum)

```

```{r, include=FALSE}

# positive - facebook comments
dtm_p <- TermDocumentMatrix(docs_p) 
matrix_p <- as.matrix(dtm_p) 
words_p <- sort(rowSums(matrix_p),decreasing=TRUE) 
df_p <- data.frame(word_p = names(words_p),freq_p=words_p)
df_p <- df_p[-c(1), ]
rownames(df_p) <- NULL

# negative - open-ended survey responses
dtm_po <- TermDocumentMatrix(docs_po) 
matrix_po <- as.matrix(dtm_po) 
words_po <- sort(rowSums(matrix_po),decreasing=TRUE) 
df_po <- data.frame(word_po = names(words_po),freq_po=words_po)
df_po <- df_po[-c(1), ]
rownames(df_po) <- NULL


df_po$word_po<-recode(df_po$word_po,"cyouth"="youth")
df_po$word_po<-recode(df_po$word_po,"cyounger"="younger")
df_po$word_po<-recode(df_po$word_po,"cyoung"="young")
df_po$word_po<-recode(df_po$word_po,"cworking"="working")
df_po$word_po<-recode(df_po$word_po,"cwork"="work")
df_po$word_po<-recode(df_po$word_po,"cwon"="won")
df_po$word_po<-recode(df_po$word_po,"cwithstand"="withstand")
df_po$word_po<-recode(df_po$word_po,"cwishing"="wishing")
df_po$word_po<-recode(df_po$word_po,"cwish"="wish")
df_po$word_po<-recode(df_po$word_po,"cwinning"="winning")
df_po$word_po<-recode(df_po$word_po,"cwinner"="winner")
df_po$word_po<-recode(df_po$word_po,"cwin"="win")
df_po$word_po<-recode(df_po$word_po,"cwell"="well")
df_po$word_po<-recode(df_po$word_po,"cwelcome"="welcome")
df_po$word_po<-recode(df_po$word_po,"cwages"="wages")
df_po$word_po<-recode(df_po$word_po,"cvictorious"="victorious")
df_po$word_po<-recode(df_po$word_po,"cuniversity"="university")
df_po$word_po<-recode(df_po$word_po,"cuniversal"="universal")
df_po$word_po<-recode(df_po$word_po,"cunity"="unity")
df_po$word_po<-recode(df_po$word_po,"cunited"="united")
df_po$word_po<-recode(df_po$word_po,"cunite"="unite")
df_po$word_po<-recode(df_po$word_po,"cunderstand"="understand")
df_po$word_po<-recode(df_po$word_po,"ctruth"="truth")
df_po$word_po<-recode(df_po$word_po,"ctrust"="trust")
df_po$word_po<-recode(df_po$word_po,"cthrone"="throne")
df_po$word_po<-recode(df_po$word_po,"cthanks"="thanks")
df_po$word_po<-recode(df_po$word_po,"cthank"="thank")
df_po$word_po<-recode(df_po$word_po,"cteacher"="teaher")
df_po$word_po<-recode(df_po$word_po,"ctalk"="talk")
df_po$word_po<-recode(df_po$word_po,"csympathy"="sympathy")
df_po$word_po<-recode(df_po$word_po,"csurviving"="surviving")
df_po$word_po<-recode(df_po$word_po,"csurvive"="survive")
df_po$word_po<-recode(df_po$word_po,"csupporting"="supporting")
df_po$word_po<-recode(df_po$word_po,"csupport"="support")
df_po$word_po<-recode(df_po$word_po,"csucceed"="succeed")
df_po$word_po<-recode(df_po$word_po,"cstudy"="study")
df_po$word_po<-recode(df_po$word_po,"cstrong"="strong")
df_po$word_po<-recode(df_po$word_po,"cstriving"="striving")
df_po$word_po<-recode(df_po$word_po,"cstable"="stable")
df_po$word_po<-recode(df_po$word_po,"cstability"="stability")
df_po$word_po<-recode(df_po$word_po,"cspeedy"="speedy")
df_po$word_po<-recode(df_po$word_po,"cspecial"="speial")
df_po$word_po<-recode(df_po$word_po,"csolution"="solution")
df_po$word_po<-recode(df_po$word_po,"cskyrocketed"="skyroketed")
df_po$word_po<-recode(df_po$word_po,"csincerely"="sincerely")
df_po$word_po<-recode(df_po$word_po,"cshare"="share")
df_po$word_po<-recode(df_po$word_po,"cselfless"="selfless")
df_po$word_po<-recode(df_po$word_po,"csecured"="secured")
df_po$word_po<-recode(df_po$word_po,"csaving"="saving")
df_po$word_po<-recode(df_po$word_po,"csave"="save")
df_po$word_po<-recode(df_po$word_po,"csatisfied"="satisfied")
df_po$word_po<-recode(df_po$word_po,"csafe"="safe")
df_po$word_po<-recode(df_po$word_po,"cright"="right")
df_po$word_po<-recode(df_po$word_po,"crestore"="restore")
df_po$word_po<-recode(df_po$word_po,"crespected"="respeted")
df_po$word_po<-recode(df_po$word_po,"crelieved"="relieved")
df_po$word_po<-recode(df_po$word_po,"creliable"="reliable")
df_po$word_po<-recode(df_po$word_po,"crelevant"="relevant")
df_po$word_po<-recode(df_po$word_po,"credible"="credible")
df_po$word_po<-recode(df_po$word_po,"crecovery"="recovery")
df_po$word_po<-recode(df_po$word_po,"creceived"="received")
df_po$word_po<-recode(df_po$word_po,"create"="create")
df_po$word_po<-recode(df_po$word_po,"cready"="ready")
df_po$word_po<-recode(df_po$word_po,"cquickly"="quickly")
df_po$word_po<-recode(df_po$word_po,"cprovide"="provide")
df_po$word_po<-recode(df_po$word_po,"cprotect"="protect")
df_po$word_po<-recode(df_po$word_po,"cproper"="proper")
df_po$word_po<-recode(df_po$word_po,"cpro"="pro")
df_po$word_po<-recode(df_po$word_po,"cprime"="prime")
df_po$word_po<-recode(df_po$word_po,"cpresent"="present")
df_po$word_po<-recode(df_po$word_po,"cprecious"="precious")
df_po$word_po<-recode(df_po$word_po,"cpraying"="praying")
df_po$word_po<-recode(df_po$word_po,"cpray"="pray")
df_po$word_po<-recode(df_po$word_po,"cpractice"="practice")
df_po$word_po<-recode(df_po$word_po,"cpractical"="practical")
df_po$word_po<-recode(df_po$word_po,"cpleased"="pleased")
df_po$word_po<-recode(df_po$word_po,"cplease"="please")
df_po$word_po<-recode(df_po$word_po,"cpleasant"="pleasant")
df_po$word_po<-recode(df_po$word_po,"cplanning"="planning")
df_po$word_po<-recode(df_po$word_po,"cpeacefully"="peacefully")
df_po$word_po<-recode(df_po$word_po,"cpeaceful"="peaceful")
df_po$word_po<-recode(df_po$word_po,"cpeace"="peace")
df_po$word_po<-recode(df_po$word_po,"cpatient"="patient")
df_po$word_po<-recode(df_po$word_po,"covercome"="overome")
df_po$word_po<-recode(df_po$word_po,"council"="council")
df_po$word_po<-recode(df_po$word_po,"corganization"="organization")
df_po$word_po<-recode(df_po$word_po,"copportunity"="opportunity")
df_po$word_po<-recode(df_po$word_po,"copportunities"="opportunities")
df_po$word_po<-recode(df_po$word_po,"coordinate"="coordinate")
df_po$word_po<-recode(df_po$word_po,"convenient"="convenient")
df_po$word_po<-recode(df_po$word_po,"continuing"="continuing")
df_po$word_po<-recode(df_po$word_po,"continue"="continue")
df_po$word_po<-recode(df_po$word_po,"contact"="contact")


df_p$word_p<-recode(df_p$word_p,"cbenefits"="benefits")
df_p$word_p<-recode(df_p$word_p,"ceducated"="educated")
df_p$word_p<-recode(df_p$word_p,"cglad"="glad")
df_p$word_p<-recode(df_p$word_p,"cgrand"="grand")
df_p$word_p<-recode(df_p$word_p,"chappy"="happy")
df_p$word_p<-recode(df_p$word_p,"chope"="hope")
df_p$word_p<-recode(df_p$word_p,"cnew"="new")
df_p$word_p<-recode(df_p$word_p,"coath"="oath")
df_p$word_p<-recode(df_p$word_p,"cobjective"="objective")
df_p$word_p<-recode(df_p$word_p,"copportunities"="opportunities")
df_p$word_p<-recode(df_p$word_p,"cpresent"="present")
df_p$word_p<-recode(df_p$word_p,"crelieve"="relieve")
df_p$word_p<-recode(df_p$word_p,"cright"="right")
df_p$word_p<-recode(df_p$word_p,"csatisfied"="satisfied")
df_p$word_p<-recode(df_p$word_p,"cthank"="thank")
df_p$word_p<-recode(df_p$word_p,"cunited"="united")
df_p$word_p<-recode(df_p$word_p,"cadvantages"="advantages")
df_p$word_p<-recode(df_p$word_p,"cbuilding"="building")
df_p$word_p<-recode(df_p$word_p,"cchance"="chance")
df_p$word_p<-recode(df_p$word_p,"ccouncil"="council")
df_p$word_p<-recode(df_p$word_p,"cdeal"="deal")
df_p$word_p<-recode(df_p$word_p,"cdefense"="defense")
df_p$word_p<-recode(df_p$word_p,"cdemocracy"="democracy")
df_p$word_p<-recode(df_p$word_p,"cdeserve"="deserve")
df_p$word_p<-recode(df_p$word_p,"cfocus"="focus")
df_p$word_p<-recode(df_p$word_p,"cfound"="found")
df_p$word_p<-recode(df_p$word_p,"cfreedom"="freedom")
df_p$word_p<-recode(df_p$word_p,"cgeneral"="general")
df_p$word_p<-recode(df_p$word_p,"cgenuine"="genuine")
df_p$word_p<-recode(df_p$word_p,"cgood"="good")
df_p$word_p<-recode(df_p$word_p,"chumanity"="humanity")
df_p$word_p<-recode(df_p$word_p,"ckind"="kind")
df_p$word_p<-recode(df_p$word_p,"clike"="like")
df_p$word_p<-recode(df_p$word_p,"clove"="love")
df_p$word_p<-recode(df_p$word_p,"cmaintaining"="maintaining")
df_p$word_p<-recode(df_p$word_p,"cnatural"="natural")
df_p$word_p<-recode(df_p$word_p,"cpeace"="peace")
df_p$word_p<-recode(df_p$word_p,"cprotect"="protect")
df_p$word_p<-recode(df_p$word_p,"creflects"="reflects")
df_p$word_p<-recode(df_p$word_p,"crelief"="relief")
df_p$word_p<-recode(df_p$word_p,"cresponsible"="responsible")
df_p$word_p<-recode(df_p$word_p,"csave"="save")
df_p$word_p<-recode(df_p$word_p,"csecure"="secure")
df_p$word_p<-recode(df_p$word_p,"cskyrocketed"="skyrocketed")
df_p$word_p<-recode(df_p$word_p,"csupport"="support")
df_p$word_p<-recode(df_p$word_p,"csupporter"="supporter")
df_p$word_p<-recode(df_p$word_p,"ctalk"="talk")
df_p$word_p<-recode(df_p$word_p,"cthoughtfully"="thoughtfully")
df_p$word_p<-recode(df_p$word_p,"ctruth"="truth")
df_p$word_p<-recode(df_p$word_p,"cvoter"="voter")
df_p$word_p<-recode(df_p$word_p,"cwisdom"="wisdom")
df_p$word_p<-recode(df_p$word_p,"cwish"="wish")
df_p$word_p<-recode(df_p$word_p,"cyes"="yes")


df_p <- aggregate(df_p$freq_p, by=list(word_p=df_p$word_p), FUN=sum)
df_po <- aggregate(df_po$freq_po, by=list(word_po=df_po$word_po), FUN=sum)

```

We then get the output for the clean word cloud. 

```{r, include=FALSE}

# facebook comments wordcloud - positive
wordcloud(words = df_p$word_p, freq = df_p$x, min.freq = 2, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

```

```{r, include=FALSE}

# facebook comments wordcloud - negative
wordcloud(words = df_n$word_n, freq = df_n$x, min.freq = 2, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

```


```{r}
# open-ended survey responses wordcloud - positive
wordcloud(words = df_po$word_po, freq = df_po$x, min.freq = 2, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
```


```{r}
# open-ended survey responses wordcloud - negative
wordcloudneg <- wordcloud(words = df_ne$word_ne, freq = df_ne$x, min.freq = 2, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
```

## Results

### Survey Findings

In Table 1, we present the prevalence of reporting symptoms for anxiety and depression, depression, anxiety, as well as the percentage of respondents self-reporting  “Fair” or “Poor” mental health status. As Table 2 shows, a high percentage of the Burmese adult population show symptoms for anxiety and depression (60.71%), depression (61.39%), anxiety (58.02), fair or poor mental health status (69.15%). For comparison, using the same PHQ-4 measures and cutoff points, we find that the prevalence of anxiety and depression in the US during COVID-19 pandemic reached its highest level of 19% and 13%, respectively, in April 2020 (US data are from the Understanding America Study (UAS) Covid Tracking Survey: https://uasdata.usc.edu/index.php). A more detailed output is found in the appendix which includes the findings of the logistic regression in table 2. 


```{r, include=FALSE}

#creating table with output from Stata

table1 <- matrix(c(60.71, 61.39, 58.02, 69.15), ncol = 4, byrow = TRUE)
colnames(table1) <- c("Anxiety and Depression", "Depression", "Anxiety", "Fair or Poor")
rownames(table1) <- c("Overall, %")
table1 <- as.table(table1)

print(table1)

```

### Google Trends and Sentiment Analysis of Textual Data

The data we collected from the Google trends API established that the words military coup and sad saw the sharpest increase after the coup occurred in February 2021. For example, the word military coup did not appear to have search hits in January 2021 but this rate increased to 100 around February. The number of hits for the word sad varied across the months but remained relatively high throughout the analysis period. The words anxiety and depression were at their highest levels in the month of April with numbers fluctuating throughout the months that were examined. Our sentiment analysis revealed that the word democracy was the most commonly mentioned word for the positive sentiment while dictatorship was the most frequently mentioned word in the negative sentiment word cloud. Because of this, we decided to do a second Google trends API analysis to determine if we also saw an increase of searches for these two words. We found that there was an increase in searches for these two words, with the sharpest increase occurring at the time of the coup and immediately after and stabilizing as time went on.  

```{r, include = FALSE}

plot(res1)

```


```{r, include = FALSE}

plot(res2)

```
Our findings regarding the sentiment analysis of textual data revealed the words that were associated with a negative sentiment and a positive sentiment along with which words were mentioned most frequently. As figure negatives displays, the words dictatorship, junta, bad, worried, war, difficult, and fall were the most commonly mentioned negative sentiment words. These findings allowed us to look at the responses and comments that contained these words and analyze the context of these. The textual data revealed that many respondents wanted the dictatorship/war to end or fall and that they were worried about their future (i.e., want to end the military dictatorship quickly - depressed; truly worried with the military coup; it was very difficult, I had to live and eat anxiously) Meanwhile, the words peace, democracy, support, quickly, peaceful, and peace were the most commonly mentioned words with a positive sentiment. It is important to note that although the sentiment analysis coded certain words as positive, we can assess that this was not the case once we look at the comments left by respondents. For example, comments that mentioned the word peaceful were asking for peace and were not stating that they were in a peaceful place (i.e., "I am very [happy] to answer this kind of survey. Each and everyone [in] Myanmar feels sad and suffers from the impacts of Military coup. May the dictators die. We want our democracy back. May Myanmar be peaceful. May all the people in the world [be] relieved from Covid-19"; "want to see Myanmar as a peaceful country", "I am not pleased as people have been displaced, wishing to restore democracy and become peaceful country quickly"; "we all have been struggling living under the military rules; want a peaceful life and wish to release Aunty Suu quickly"). Finally, we observe no differences between Facebook comments sentiment scores and open-ended survey responses sentiment scores (p-value = 0.3265). However, open-ended survey responses scores have a higher variance (0.12)  than Facebook comments scores (0.04). In open-ended survey responses negative, for example, we see statements intensified such as "absolutely disgusting", "total disaster", and "very depressed".

```{r, include = FALSE}

wordcloud(words = df_po$word_po, freq = df_po$x, min.freq = 2, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

```
```{r}
# open-ended survey responses wordcloud - negative
wordcloudneg <- wordcloud(words = df_ne$word_ne, freq = df_ne$x, min.freq = 2, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
```




## Discussion

This project had two main goals. The first, was to determine if there was significant prevalence of anxiety and depression among the Burmese population after the military coup in February 2021. The second aim was to see how our survey data compared to Google trends data. Regarding our first research question, it was determined that a significant percentage of the population reported having depression, anxiety, a combination of both, and fair or poor mental health. The sentiment analysis conducted on the textual data collected from the open-ended response question and the comments on the Facebook advertisement showed that participants were apprehensive about their living situation and frequently mentioned the words dictatorship, worried, and difficult among others. It was also evident that although the sentiment analysis coded certain words as having a positive sentiment, that was not the case. After examining comments with words such as peace or democracy, it was noted that the context of these words was not positive as most of the comments were respondents asking for peace and democracy. 

We used the findings of the sentiment analysis to include words such as dictatorship and democracy along with sad, military coup, depression, and anxiety to see whether there was an increase of searches for these words after the coup in early 2021. We found that there were increases in searches, although these varied depending on the word and stabilized at different times. 

Our analysis led us to conclude that Google trends can reflect the findings of a survey to a limited extent, which is evident given the increase in searches for specific words that were measured in the survey and others that were mentioned in the textual data. This research has several limitations. First, our survey used a non-probability sample which makes it susceptible to measurement error, especially in underdeveloped countries such as Myanmar, where large percentages of the population do not have access to the internet. Second, a sentiment analysis was not able to gauge the context of certain words and therefore labeled words that are considered positive as having a positive sentiment even when they were found within a negative context. Researchers should be especially careful when reporting these sorts of results. Lastly, it is impossible to determine whether all of the Google searches were associated with the coup. Symptoms of depression and anxiety can also be associated with the pandemic or other life circumstances which are not reflected in our analysis. 

Still, this project allowed us to quickly collect data from a large sample. Fast data collection is necessary after tragic events such as the coup in Myanmar, and this project showed that doing so is an efficient way to collect data and inform policies to help those affected by these events. Our findings also revealed that using online data in conjunction with survey data can be done in a manner that leads to findings that are more holistic by taking different methods into account. Future studies should look to incorporate other sources of online data such as Twitter data or Reddit data. A non-probability survey should also be made available to establish how much non-probability data compares to probability data in times of crisis. We encourage scholars to take advantage of the multiple data sources available and to use these in concurrence with survey data to further strengthen their findings. 

## References

1. Kreuter, F., N. Barkay, A. Bilinski, A. Bradford, S. Chiu, R. Eliat, J. Fan, T. Galili, D. Haimovich and     B. Kim (2020). Partnering with a global platform to inform research and public policy making. Survey        Research Methods.

2. Kroenke, K., Spitzer, R. L., Williams, J. B., & Löwe, B. (2009). An ultra-brief screening scale for            anxiety and depression: the PHQ-4. Psychosomatics, 50(6), 613–621.         
      https://doi.org/10.1176/appi.psy.50.6.613

3. Löwe, B., Wahl, I., Rose, M., Spitzer, C., Glaesmer, H., Wingenfeld, K., Schneider, A., & Brähler, E.        (2010). A 4-item measure of depression and anxiety: validation and standardization of the Patient Health     Questionnaire-4 (PHQ-4) in the general population. Journal of affective disorders, 122(1-2), 86–95.         https://doi.org/10.1016/j.jad.2009.06.019

4. Salomon, J. A., A. Reinhart, A. Bilinski, E. J. Chua, W. La Motte-Kerr, M. M. Rönn, M. B. Reitsma, K. A.     Morris, S. LaRocca, T. H. Farag, F. Kreuter, R. Rosenfeld and R. J. Tibshirani (2021). "The US COVID-19     Trends and Impact Survey: Continuous real-time measurement of COVID-19 symptoms, risks, protective          behaviors, testing, and vaccination." Proceedings of the National Academy of Sciences 118(51):              e2111454118.
