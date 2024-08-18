#Data Cleaning Script for Surveys from Datacubed and Attaching Model Variables

library(ggplot2)
library(tidyverse)
library(dplyr)
library(tibble)
library(stringr)


# Set working directory
#Rootpath should be where all the .csv files are downloaded to. I recommend you desktop. 
rootpath = '/Users/keatonmackey/Desktop/'
setwd(rootpath)

#Read in CSV Files
dataResponse = read.csv('QuR-OUD Survey Responses.csv', sep = ',', header = T)
clinroResponse = read.csv('Clinro Responses.csv', sep = ',', header = T)

#Delete Practice Ids (These were used when we starting to practice clinro)
dataResponse <- dataResponse[dataResponse$subject_id <= 900, ]

# Get ID List and line up with cohort and clinro generated participant id (you can also just make a .csv file)
participant_id <- c(9182, 9198, 9205, 9266,9272, 9284, 9304, 9309, 9342)
subject_id <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
Cohort <- c(1, 1, 3, 3, 3, 3, 3, 2, 3)

idDF <- data.frame(participant_id,subject_id,Cohort)

#Separating out by Task and Id: This will seperate all the surveys so they can be properly scored and labeled. 

### Addiction Treatment and Relapse History Questionnaire (will need to mess with)
AddictionHistory <- dataResponse[dataResponse$survey_title == "Addiction Treatment and Relapse History Questionnaire", 
                                 c(2, 9, 10,11)]
AddictionHistory <- reshape(AddictionHistory, idvar = "subject_id", timevar = "question_number", direction = "wide")
colNames <- colnames(AddictionHistory)
colNames <- str_sort(colNames, numeric = TRUE)
AddictionHistory <- AddictionHistory[,colNames] 

AddictionHistory <- AddictionHistory %>%
  dplyr::select(subject_id, everything())

columns_to_remove <- grep("question", names(AddictionHistory))
AddictionHistory <- AddictionHistory[,-columns_to_remove]

names(AddictionHistory) <- c('subject_id', "AHQ1", "AHQ2", 'AHQ3', 'AHQ4', 'AHQ5', 'AHQ6', 
                   'AHQ7', 'AHQ8', 'AHQ9', 'AHQ10', 'AHQ11',
                   'AHQ12', 'AHQ13', 'AHQ14', 'AHQ15')



### BIS BAS
BisBasL <- dataResponse[dataResponse$survey_title == "BIS-BAS ", 
                                 c(2, 9, 10,11)]
BisBas <- reshape(BisBasL, idvar = "subject_id", timevar = "question_number", direction = "wide")

colNamesBis <- colnames(BisBas)
colNamesBis <- str_sort(colNamesBis, numeric = TRUE)
BisBas <- BisBas[,colNamesBis] 
BisBas <- BisBas %>%
  dplyr::select(subject_id, everything())

columns_to_remove <- grep("question", names(BisBas))
BisBas <- BisBas[,-columns_to_remove]

##### Need to do scoring


### BIS11
Bis11 <- dataResponse[dataResponse$survey_title == "BIS11", 
                       c(2, 9, 10,11)]
Bis11 <- reshape(Bis11, idvar = "subject_id", timevar = "question_number", direction = "wide")
colNamesBis11 <- colnames(Bis11)
colNamesBis11 <- str_sort(colNamesBis11, numeric = TRUE)
Bis11 <- Bis11[,colNamesBis11] 
Bis11 <- Bis11 %>%
  dplyr::select(subject_id, everything())
columns_to_remove <- grep("question", names(Bis11))
Bis11 <- Bis11[,-columns_to_remove]

### Brief Cope Scale
BriefCOPE <- dataResponse[dataResponse$survey_title == "Brief COPE Scale", 
                      c(2, 9, 10,11)]
BriefCOPE <- reshape(BriefCOPE, idvar = "subject_id", timevar = "question_number", direction = "wide")
colNamesBrief <- colnames(BriefCOPE)
colNamesBrief <- str_sort(colNamesBrief, numeric = TRUE)
BriefCOPE <- BriefCOPE[,colNamesBrief] 
BriefCOPE <- BriefCOPE %>%
  dplyr::select(subject_id, everything())
columns_to_remove <- grep("question", names(BriefCOPE))
BriefCOPE <- BriefCOPE[,-columns_to_remove]

### CAARS
CAARS <- dataResponse[dataResponse$survey_title == "CAARS", 
                      c(2, 9, 10,11)]
CAARS <- reshape(CAARS, idvar = "subject_id", timevar = "question_number", direction = "wide")
colNamesCAARS <- colnames(CAARS)
colNamesCAARS <- str_sort(colNamesCAARS, numeric = TRUE)
CAARS <- CAARS[,colNamesCAARS] 
CAARS <- CAARS %>%
  dplyr::select(subject_id, everything())
columns_to_remove <- grep("question", names(CAARS))
CAARS <- CAARS[,-columns_to_remove]

### Demographic Survey
DemoSurvey <- dataResponse[dataResponse$survey_title == "Demographic Survey", 
                      c(2, 9, 10,11)]
DemoSurvey <- reshape(DemoSurvey, idvar = "subject_id", timevar = "question_number", direction = "wide")
colNamesDemo <- colnames(DemoSurvey)
colNamesDemo <- str_sort(colNamesDemo, numeric = TRUE)
DemoSurvey <- DemoSurvey[,colNamesDemo] 
DemoSurvey <- DemoSurvey %>%
  dplyr::select(subject_id, everything())
columns_to_remove <- grep("question", names(DemoSurvey))
DemoSurvey <- DemoSurvey[,-columns_to_remove]

namesDemoSurvey <- c('subject_id')
for (i in 2:ncol(DemoSurvey)) {
  col = paste("DemoSurvey", i -1, sep = '.')
  namesDemoSurvey = c(namesDemoSurvey, col)
}
names(DemoSurvey) <- namesDemoSurvey

### ERQ
ERQ <- dataResponse[dataResponse$survey_title == "ERQ", 
                      c(2, 9, 10,11)]
ERQ <- reshape(ERQ, idvar = "subject_id", timevar = "question_number", direction = "wide")
colNamesERQ <<- colnames(ERQ)
colNamesERQ <- str_sort(colNamesERQ, numeric = TRUE)
ERQ <- ERQ[,colNamesERQ] 
ERQ <- ERQ %>%
  dplyr::select(subject_id, everything())
columns_to_remove <- grep("question", names(ERQ))
ERQ <- ERQ[,-columns_to_remove]

### First Experience Pleasure
FirstPleasure <- dataResponse[dataResponse$survey_title == "First Experience Pleasure (drug vs non drug experience)", 
                      c(2, 9, 10,11)]
FirstPleasure <- reshape(FirstPleasure, idvar = "subject_id", timevar = "question_number", direction = "wide")
colNamesFP <<- colnames(FirstPleasure)
colNamesFP <- str_sort(colNamesFP, numeric = TRUE)
FirstPleasure <- FirstPleasure[,colNamesFP] 
FirstPleasure <- FirstPleasure %>%
  dplyr::select(subject_id, everything())
columns_to_remove <- grep("question", names(FirstPleasure))
FirstPleasure <- FirstPleasure[,-columns_to_remove]

namesFirstPleasure <- c('subject_id')
for (i in 2:ncol(FirstPleasure)) {
  col = paste("FirstPleasure", i -1, sep = '.')
  namesFirstPleasure = c(namesFirstPleasure, col)
}
names(FirstPleasure) <- namesFirstPleasure

###HCQ-14-SF Survey (will need to mess with)
HCQ <- dataResponse[dataResponse$survey_title == "HCQ-14-SF Survey", 
                      c(2, 9, 10,11)]
HCQ <- reshape(HCQ, idvar = "subject_id", timevar = "question_number", direction = "wide")
colNamesHCQ <- colnames(HCQ)
colNamesHCQ <- str_sort(colNamesHCQ, numeric = TRUE)
HCQ <- HCQ[,colNamesHCQ] 
HCQ <- HCQ %>%
  dplyr::select(subject_id, everything())
columns_to_remove <- grep("question", names(HCQ))
HCQ <- HCQ[,-columns_to_remove]
###PHQ-8
PHQ <- dataResponse[dataResponse$survey_title == "Personal Health Questionnaire Depression Scale (PHQ-8)", 
                      c(2, 9, 10,11)]
PHQ <- reshape(PHQ, idvar = "subject_id", timevar = "question_number", direction = "wide")
colNamesPHQ <- colnames(PHQ)
colNamesPHQ <- str_sort(colNamesPHQ, numeric = TRUE)
PHQ <- PHQ[,colNamesPHQ] 
PHQ <- PHQ %>%
  dplyr::select(subject_id, everything())
columns_to_remove <- grep("question", names(PHQ))
PHQ <- PHQ[,-columns_to_remove]

###SHAPS Pleasure Scale
SHAPS <- dataResponse[dataResponse$survey_title == "SHAPS  Pleasure Scale", 
                      c(2, 9, 10,11)]
SHAPS <- reshape(SHAPS, idvar = "subject_id", timevar = "question_number", direction = "wide")
colNamesSHAPS <- colnames(SHAPS)
colNamesSHAPS <- str_sort(colNamesSHAPS, numeric = TRUE)
SHAPS <- SHAPS[,colNamesSHAPS] 
SHAPS <- SHAPS %>%
  dplyr::select(subject_id, everything())
columns_to_remove <- grep("question", names(SHAPS))
SHAPS <- SHAPS[,-columns_to_remove]
### STAI
STAI <- dataResponse[dataResponse$survey_title == "STAI", 
                      c(2, 9, 10,11)]
STAI <- reshape(STAI, idvar = "subject_id", timevar = "question_number", direction = "wide")
colNamesSTAI <- colnames(STAI)
colNamesSTAI <- str_sort(colNamesSTAI, numeric = TRUE)
STAI <- STAI[,colNamesSTAI] 
STAI <- STAI %>%
  dplyr::select(subject_id, everything())
columns_to_remove <- grep("question", names(STAI))
STAI <- STAI[,-columns_to_remove]
### SURPS
SURPS <- dataResponse[dataResponse$survey_title == "SURPS", 
                      c(2, 9, 10,11)]
SURPS <- reshape(SURPS, idvar = "subject_id", timevar = "question_number", direction = "wide")
colNamesSURPS <- colnames(SURPS)
colNamesSURPS <- str_sort(colNamesSURPS, numeric = TRUE)
SURPS <- SURPS[,colNamesSURPS] 
SURPS <- SURPS %>%
  dplyr::select(subject_id, everything())
columns_to_remove <- grep("question", names(SURPS))
SURPS <- SURPS[,-columns_to_remove]
### UPPS
UPPS <- dataResponse[dataResponse$survey_title == "UPPS", 
                      c(2, 9, 10,11)]
UPPS <- reshape(UPPS, idvar = "subject_id", timevar = "question_number", direction = "wide")
colNamesUPPS <- colnames(UPPS)
colNamesUPPS <- str_sort(colNamesUPPS, numeric = TRUE)
UPPS <- UPPS[,colNamesUPPS] 
UPPS <- UPPS %>%
  dplyr::select(subject_id, everything())
columns_to_remove <- grep("question", names(UPPS))
UPPS <- UPPS[,-columns_to_remove]
### WHOQOL
WHOQOL <- dataResponse[dataResponse$survey_title == "WHOQOL", 
                      c(2, 9, 10,11)]
WHOQOL <- reshape(WHOQOL, idvar = "subject_id", timevar = "question_number", direction = "wide")
colNamesWHOQOL <- colnames(WHOQOL)
colNamesWHOQOL <- str_sort(colNamesWHOQOL, numeric = TRUE)
WHOQOL <- WHOQOL[,colNamesWHOQOL] 
WHOQOL <- WHOQOL %>%
  dplyr::select(subject_id, everything())
columns_to_remove <- grep("question", names(WHOQOL))
WHOQOL <- WHOQOL[,-columns_to_remove]

#ASI
ASI <- clinroResponse[clinroResponse$clinro_title == "ASI (Lite)", 
                         c(1, 9, 10)]

ASI <- reshape(ASI, idvar = "participant_id", timevar = "question_number", direction = "wide")

colNames <- colnames(ASI)
colNames <- str_sort(colNames, numeric = TRUE)
ASI <- ASI[,colNames] 
ASI = dplyr::select(ASI, -1)
ASI <- ASI %>%
  dplyr::select(participant_id, everything())


namesASI <- c("participant_id")
for (i in 2:ncol(ASI)) {
  col = paste("ASI", i -1, sep = '.')
  namesASI = c(namesASI, col)
}
names(ASI) <- namesASI

#Fixing ASI route of admin issue -- this was because of lack of "N/A" option on Clinro input, so you have to manually do it when cleaning the data. 
for (i in 1:nrow(ASI)){
  heroinVal <- ASI[i, "ASI.55"]
  methadoneVal <- ASI[i, "ASI.56"]
  opiatesVal <- ASI[i, "ASI.57"]
  barbsVal <- ASI[i, "ASI.58"]
  sedsVal <- ASI[i, "ASI.59"]
  cocaineVal <- ASI[i, "ASI.60"]
  amphVal <- ASI[i, "ASI.61"]
  cannabisVal <- ASI[i, "ASI.62"]
  hallucVal <- ASI[i, "ASI.63"]
  inhalantsVal <- ASI[i, "ASI.64"]
  if (heroinVal == 'No'){
    ASI[i, 'ASI.65'] <- 'N/A'
  } 
  if (methadoneVal == 'No'){
    ASI[i, 'ASI.66'] <- 'N/A'
  } 
  if (opiatesVal == 'No'){
    ASI[i, 'ASI.67'] <- 'N/A'
  } 
  if (barbsVal == 'No'){
    ASI[i, 'ASI.68'] <- 'N/A'
  } 
  if (sedsVal == 'No'){
    ASI[i, 'ASI.69'] <- 'N/A'
  } 
  if (cocaineVal == 'No'){
    ASI[i, 'ASI.70'] <- 'N/A'
  } 
  if (amphVal == 'No'){
    ASI[i, 'ASI.71'] <- 'N/A'
  } 
  if (cannabisVal == 'No'){
    ASI[i, 'ASI.72'] <- 'N/A'
  } 
  if (hallucVal == 'No'){
    ASI[i, 'ASI.73'] <- 'N/A'
  } 
  if (inhalantsVal == 'No'){
    ASI[i, 'ASI.74'] <- 'N/A'
  } 
}


##RAVEN MATRICES
# Raven A
RavenA <- clinroResponse[clinroResponse$clinro_title == "Raven Matrices Test-A", 
                         c(1, 9, 10)]
RavenA <- reshape(RavenA, idvar = "participant_id", timevar = "question_number", direction = "wide")
colNames <- colnames(RavenA)
colNames <- str_sort(colNames, numeric = TRUE)
RavenA <- RavenA[,colNames] 
RavenA = dplyr::select(RavenA, -1)
names(RavenA) <- c("AQ1", "AQ2", 'AQ3', 'AQ4', 'AQ5', 'AQ6', 
                       'AQ7', 'AQ8', 'AQ9', 'AQ10', 'AQ11',
                       'AQ12', 'participant_id')
RavenA <- RavenA %>%
  dplyr::select(participant_id, everything())



# Raven B
RavenB <- clinroResponse[clinroResponse$clinro_title == "Raven Matrices Test-B", 
                         c(1, 9, 10)]
RavenB <- reshape(RavenB, idvar = "participant_id", timevar = "question_number", direction = "wide")
colNames <- colnames(RavenB)
colNames <- str_sort(colNames, numeric = TRUE)
RavenB <- RavenB[,colNames] 
RavenB = dplyr::select(RavenB, -1)
names(RavenB) <- c("BQ1", "BQ2", 'BQ3', 'BQ4', 'BQ5', 'BQ6', 
                   'BQ7', 'BQ8', 'BQ9', 'BQ10', 'BQ11',
                   'BQ12', 'participant_id')
RavenB <- RavenB %>%
  dplyr::select(participant_id, everything())



# Raven C
RavenC <- clinroResponse[clinroResponse$clinro_title == "Raven Matrices Test-C", 
                         c(1, 9, 10)]
RavenC <- reshape(RavenC, idvar = "participant_id", timevar = "question_number", direction = "wide")
colNames <- colnames(RavenC)
colNames <- str_sort(colNames, numeric = TRUE)
RavenC <- RavenC[,colNames] 
RavenC = dplyr::select(RavenC, -1)
names(RavenC) <- c("CQ1", "CQ2", 'CQ3', 'CQ4', 'CQ5', 'CQ6', 
                   'CQ7', 'CQ8', 'CQ9', 'CQ10', 'CQ11',
                   'CQ12', 'participant_id')
RavenC <- RavenC %>%
  dplyr::select(participant_id, everything())


# Raven D
RavenD <- clinroResponse[clinroResponse$clinro_title == "Raven Matrices Test-D", 
                         c(1, 9, 10)]
RavenD <- reshape(RavenD, idvar = "participant_id", timevar = "question_number", direction = "wide")
colNames <- colnames(RavenD)
colNames <- str_sort(colNames, numeric = TRUE)
RavenD <- RavenD[,colNames] 
RavenD = dplyr::select(RavenD, -1)
names(RavenD) <- c("DQ1", "DQ2", 'DQ3', 'DQ4', 'DQ5', 'DQ6', 
                   'DQ7', 'DQ8', 'DQ9', 'DQ10', 'DQ11',
                   'DQ12', 'participant_id')
RavenD <- RavenD %>%
  dplyr::select(participant_id, everything())

# Raven E
RavenE <- clinroResponse[clinroResponse$clinro_title == "Raven Matrices Test-E", 
                         c(1, 9, 10)]
RavenE <- reshape(RavenE, idvar = "participant_id", timevar = "question_number", direction = "wide")
colNames <- colnames(RavenE)
colNames <- str_sort(colNames, numeric = TRUE)
RavenE <- RavenE[,colNames] 
RavenE = dplyr::select(RavenE, -1)
names(RavenE) <- c("EQ1", "EQ2", 'EQ3', 'EQ4', 'EQ5', 'EQ6', 
                   'EQ7', 'EQ8', 'EQ9', 'EQ10', 'EQ11',
                   'EQ12', 'participant_id')
RavenE <- RavenE %>%
  dplyr::select(participant_id, everything())



### SCORING


###BIS BAS

answerColsBisBas <- grep("answer", colnames(BisBas))
BisBas <- BisBas %>% dplyr::mutate(
  across(
    .cols = answerColsBisBas,
    .fns = ~ case_when(
      .x == 'Very true for me'~ 4,
      .x == 'Somewhat true' ~ 3,
      .x == 'Somewhat false for me' ~ 2,
      .x == "Very false for me"  ~ 1,
    )
  )
)

namesBisBas <- c('subject_id')
for (i in 2:ncol(BisBas)) {
  col = paste("BisBas", i -1, sep = '.')
  namesBisBas = c(namesBisBas, col)
}
names(BisBas) <- namesBisBas

BisBas[is.na(BisBas)] = 0
BisBas$BASrscore = BisBas$BisBas.4 + BisBas$BisBas.7 + BisBas$BisBas.14 + BisBas$BisBas.18 + BisBas$BisBas.23
BisBas$BASfscore = BisBas$BisBas.5 + BisBas$BisBas.10 + BisBas$BisBas.15 + BisBas$BisBas.20
BisBas$BASdscore = BisBas$BisBas.3 + BisBas$BisBas.9 + BisBas$BisBas.12 + BisBas$BisBas.21
BisBas$BAStscore = BisBas$BASr + BisBas$BASf + BisBas$BASd

BisBas$BISscore = (5 - BisBas$BisBas.2) + BisBas$BisBas.8 + BisBas$BisBas.12 + BisBas$BisBas.16 + BisBas$BisBas.19 + 
  (5 -BisBas$BisBas.22) + BisBas$BisBas.24

###BIS11


answerColsBis11 <- grep("answer", colnames(Bis11))
Bis11 <- Bis11 %>% dplyr::mutate(
  across(
    .cols = answerColsBis11,
    .fns = ~ case_when(
      .x == 'Almost always/always'~ 4,
      .x == 'Often' ~ 3,
      .x == 'Occasionally' ~ 2,
      .x == "Rarely/Never"  ~ 1,
    )
  )
)
namesBis11 <- c('subject_id')
for (i in 2:ncol(Bis11)) {
  col = paste("Bis11", i -1, sep = '.')
  namesBis11 = c(namesBis11, col)
}
names(Bis11) <- namesBis11

Bis11$Bis11score = rowSums(Bis11[,answerColsBis11])

###Brief Cope Scale

answerColsBrief <- grep("answer", colnames(BriefCOPE))
BriefCOPE[] <- lapply(BriefCOPE, str_replace_all, "'","")
BriefCOPE[is.na(BriefCOPE)] = 0
BriefCOPE <- BriefCOPE %>% dplyr::mutate(
  across(
    .cols = answerColsBrief,
    .fns = ~ case_when(
      .x == "I havent been doing this at all" ~ 1,
      .x == "I have been doing this a little" ~ 2,
      .x == "I have been doing this a moderate amount" ~ 3,
      .x == "I have been doing this quite a lot" ~ 4,
    )
  )
)

namesBriefCOPE <- c('subject_id')
for (i in 2:ncol(BriefCOPE)) {
  col = paste("BriefCOPE", i -1, sep = '.')
  namesBriefCOPE = c(namesBriefCOPE, col)
}
names(BriefCOPE) <- namesBriefCOPE

avoidantCopeCols = c(2, 4, 5, 6, 9, 12, 15, 17, 20)
BriefCOPE$BriefCopeAvoidantCopescore = rowMeans(BriefCOPE[,avoidantCopeCols])
problemCopCols = c(3,8,11,13,18,24,26)
BriefCOPE$BriefCopeProblemFocusscore = rowMeans(BriefCOPE[,problemCopCols])
EmotionFocusCols = c(6,10)
BriefCOPE$BriefCopeEmotionFocusscore = rowMeans(BriefCOPE[,EmotionFocusCols])


#CAARS
answerColsCAARS <- grep("answer", colnames(CAARS))
CAARS <- CAARS %>% dplyr::mutate(
  across(
    .cols = answerColsCAARS,
    .fns = ~ case_when(
      .x == "Not at all" ~ 0,
      .x == "Just a little" ~ 1,
      .x == "Pretty much, often" ~ 2,
      .x == "Very much, very frequently" ~ 3,
    )
  )
)
namesCAARS <- c('subject_id')
for (i in 2:ncol(CAARS)) {
  col = paste("CAARS", i -1, sep = '.')
  namesCAARS = c(namesCAARS, col)
}
names(CAARS) <- namesCAARS

CAARS$CAARSscore = rowSums(CAARS[,answerColsCAARS])
#ERQ
answerColsERQ <- grep("answer", colnames(ERQ))
ERQ <- ERQ %>% dplyr::mutate(
  across(
    .cols = answerColsERQ,
    .fns = ~ case_when(
      .x == "Strongly disagree" ~ 1,
      .x == "Disagree" ~ 2,
      .x == "Somewhat disagree" ~ 3,
      .x == "Neutral" ~ 4,
      .x == "Somewhat agree" ~5,
      .x == "Agree" ~6,
      .x == "Strongly agree" ~7,
    )
  )
)

namesERQ <- c('subject_id')
for (i in 2:ncol(ERQ)) {
  col = paste("ERQ", i -1, sep = '.')
  namesERQ = c(namesERQ, col)
}
names(ERQ) <- namesERQ

cogRepraiseCols = c(2,4,6,9,11)
expressSuppression = c(3,5,7,10)
ERQ$ERQCognitiveReappraisalscore = rowMeans(ERQ[,cogRepraiseCols])
ERQ$ERQExpressionSuppressionscore = rowMeans(ERQ[,expressSuppression])

#HCQ - 14 - SF
#No Naive data because "answer no" so ended survey
answerColsHCQ <- grep("answer", colnames(HCQ))
HCQ <- HCQ %>% dplyr::mutate(
  across(
    .cols = answerColsHCQ,
    .fns = ~ case_when(
      .x == "1: Strongly disagree" ~ 1,
      .x == "2"~ 2,
      .x == "3"~ 3,
      .x == "4"~ 4,
      .x == "5"~ 5,
      .x == "6"~ 6,
      .x == "7: Strong agree" ~7,
    )
  )
)

namesHCQ <- c('subject_id')
for (i in 2:ncol(HCQ)) {
  col = paste("HCQ", i -1, sep = '.')
  namesHCQ = c(namesHCQ, col)
}
names(HCQ) <- namesHCQ

HCQ$HCQscore = (((8 - HCQ$HCQ.2) + (8 - HCQ$HCQ.6) + (8 - HCQ$HCQ.9) + (8 - HCQ$HCQ.10) + (8 - HCQ$HCQ.11) 
             + rowSums(HCQ[,c(4,5,6,8,9,13,14,15,16)])) / 14)

#PHQ-8
answerColsPHQ <- grep("answer", colnames(PHQ))
PHQ <- PHQ %>% dplyr::mutate(
  across(
    .cols = answerColsPHQ,
    .fns = ~ case_when(
      .x == "Not at all 
" ~ 0,
      .x == "Several days" ~ 1,
      .x == "More than half the days" ~ 2,
      .x == "Nearly every day" ~ 3,
    )
  )
)

namesPHQ <- c('subject_id')
for (i in 2:ncol(PHQ)) {
  col = paste("PHQ", i -1, sep = '.')
  namesPHQ = c(namesPHQ, col)
}
names(PHQ) <- namesPHQ

PHQ[is.na(PHQ)] = 0
PHQ$PHQscore = rowSums(PHQ[ ,answerColsPHQ])

#SHAPS Pleasure Scale
answerColsSHAPS <- grep("answer", colnames(SHAPS))
SHAPS <- SHAPS %>% dplyr::mutate(
  across(
    .cols = answerColsSHAPS,
    .fns = ~ case_when(
      .x == "Strongly disagree" ~ 0,
      .x == "Disagree" ~ 1,
      .x == "Agree" ~ 2,
      .x == "Strongly agree" ~ 3,
    )
  )
)

namesSHAPS <- c('subject_id')
for (i in 2:ncol(SHAPS)) {
  col = paste("SHAPS", i -1, sep = '.')
  namesSHAPS = c(namesSHAPS, col)
}
names(SHAPS) <- namesSHAPS

SHAPS$SHAPSscore = ((3 - SHAPS$SHAPS.2) + (3 - SHAPS$SHAPS.4) + (3 - SHAPS$SHAPS.5) + (3 - SHAPS$SHAPS.7) + (3 - SHAPS$SHAPS.9) 
              + rowSums(SHAPS[,c(2,4,7,9,11,12,13,14,15)]))


#STAI
answerColsSTAIS <- c(2:21)
answerColsSTAIT <- c(22:41)
STAITest <- STAI %>% dplyr::mutate(
  across(
    .cols = answerColsSTAIS,
    .fns = ~ case_when(
      .x == "Very much so" ~ 1,
      .x == "Moderately so" ~ 2,
      .x == "Somewhat" ~ 3,
      .x == "Not at all" ~ 4,
    )
  )
)
STAITest[is.na(STAITest)] = 1
STAI <- STAITest %>% dplyr::mutate(
  across(
    .cols = answerColsSTAIT,
    .fns = ~ case_when(
      .x == "Almost always" ~ 1,
      .x == "Often" ~ 2,
      .x == "Sometimes" ~ 3,
      .x == "Almost never" ~ 4,
    )
  )
)

namesSTAI <- c('subject_id')
for (i in 2:ncol(STAI)) {
  col = paste("STAI", i -1, sep = '.')
  namesSTAI = c(namesSTAI, col)
}
names(STAI) <- namesSTAI

STAI$STAISscore = rowSums(STAI[,answerColsSTAIS])
STAI$STAITscore = rowSums(STAI[,answerColsSTAIT])


#SURPS Scoring
answerColsSURPS <- c(3,4,6,8,9,10,11,12,13,15,16,17,18,19,20,22,23)
reverseAnswerColsSURPS <- c(2,5,7,14,21,24)
SURPS <- SURPS %>% dplyr::mutate(
  across(
    .cols = answerColsSURPS,
    .fns = ~ case_when(
      .x == "Strongly disagree" ~ 1,
      .x == "Somewhat disagree" ~ 2,
      .x == "Somewhat agree" ~ 3,
      .x == "Strongly agree" ~ 4,
    )
  )
)

SURPS <- SURPS %>% dplyr::mutate(
  across(
    .cols = reverseAnswerColsSURPS,
    .fns = ~ case_when(
      .x == "Strongly disagree" ~ 4,
      .x == "Somewhat disagree" ~ 3,
      .x == "Somewhat agree" ~ 2,
      .x == "Strongly agree" ~ 1,
    )
  )
)

namesSURPS <- c('subject_id')
for (i in 2:ncol(SURPS)) {
  col = paste("SURPS", i -1, sep = '.')
  namesSURPS = c(namesSURPS, col)
}
names(SURPS) <- namesSURPS

IntroversionCols <- c(2,5,7,14,18,21,24)
SURPS$SURPSIntroversionHopelessness_score = rowMeans(SURPS[, IntroversionCols])
AnxietySensitivityCols <- c(9,11,15,19,22)
SURPS$SURPSAnxietySensitivity_score = rowMeans(SURPS[,AnxietySensitivityCols])
ImpulsivityCols <- c(3,6,12,16,23)
SURPS$SURPSImpulsivity_score = rowMeans(SURPS[,ImpulsivityCols])
SensationSeekingCols = c(4,7,10,13,20)
SURPS$SURPSSensationSeeking_score = rowMeans(SURPS[,SensationSeekingCols])

#WHOQOL
answerColsWHOQOL <- c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)
WHOQOL <- WHOQOL %>% dplyr::mutate(
  across(
    .cols = answerColsWHOQOL,
    .fns = ~ case_when(
      .x == "Not at all" ~ 1,
      .x == "Very dissatisfied" ~ 1,
      .x == "Never" ~ 1,
      .x == "Very poor" ~ 1,
      .x == "A little" ~ 2,
      .x == "Dissatisfied" ~ 2,
      .x == "Seldom" ~ 2,
      .x == "Poor" ~ 2,
      .x == "A moderate amountV" ~ 3,
      .x == "A moderate amount" ~ 3,
      .x == "Neither satisfied nor dissatisfied" ~ 3,
      .x == "Moderately" ~ 3,
      .x == "Quite often" ~ 3,
      .x == "Quite oftenV" ~ 3,
      .x == "Neither poor nor good" ~ 3,
      .x == "Very much" ~ 4,
      .x == "Mostly" ~ 4,
      .x == "Satisfied" ~ 4,
      .x == "Very often" ~ 4,
      .x == "Good" ~ 4,
      .x == "An extreme amount" ~ 5,
      .x == "Extremely" ~ 5,
      .x == "Completely" ~ 5,
      .x == "Very satisfied" ~ 5,
      .x == "Always" ~ 5,
      .x == "Very good" ~ 5,
    )
  )
)

namesWHOQOL <- c('subject_id')
for (i in 2:ncol(WHOQOL)) {
  col = paste("WHOQOL", i -1, sep = '.')
  namesWHOQOL = c(namesWHOQOL, col)
}
names(WHOQOL) <- namesWHOQOL

WHOQOL$WHOQOLDomain1Raw_score = ((6 - WHOQOL$WHOQOL.3) + (6 - WHOQOL$WHOQOL.4) + WHOQOL$WHOQOL.10 
                     + WHOQOL$WHOQOL.15 + WHOQOL$WHOQOL.16 + WHOQOL$WHOQOL.17 + WHOQOL$WHOQOL.18) 
WHOQOL$WHOQOLDomain1Transformed_score = ((WHOQOL$WHOQOLDomain1Raw_score - 7) / 28) * 100

WHOQOL$WHOQOLDomain2Raw_score = ((6 - WHOQOL$WHOQOL.26) + WHOQOL$WHOQOL.5 + WHOQOL$WHOQOL.6 + 
                       WHOQOL$WHOQOL.7 + WHOQOL$WHOQOL.11 + WHOQOL$WHOQOL.19) 
WHOQOL$WHOQOLDomain2Transformed_score = ((WHOQOL$WHOQOLDomain2Raw_score - 6) / 24) * 100

WHOQOL$WHOQOLDomain3Raw_score = (WHOQOL$WHOQOL.20 + WHOQOL$WHOQOL.21 + WHOQOL$WHOQOL.22)
WHOQOL$WHOQOLDomain3Transformed_score = ((WHOQOL$WHOQOLDomain3Raw_score - 3) / 12) * 100

WHOQOL$WHOQOLDomain4Raw_score = (WHOQOL$WHOQOL.8 + WHOQOL$WHOQOL.9 + WHOQOL$WHOQOL.12 + WHOQOL$WHOQOL.13 
                     + WHOQOL$WHOQOL.14 + WHOQOL$WHOQOL.23 + WHOQOL$WHOQOL.24 + WHOQOL$WHOQOL.25)  
WHOQOL$WHOQOLDomain4Transformed_score = ((WHOQOL$WHOQOLDomain4Raw_score - 8) / 31) * 100



#UPPS Scoring
reverseAnswerColsUPPS <- c(3,4,6,8,9,10,11,13,14,16,18,19,21,23,24,26,27,30,31,32,35,36,37,40,41,42,45,46,47,
                           48,50,51,52,53,55,57,58,59,60)
answerColsUPPS <- c(2,54,5,15,20,25,28,33,38,43,7,12,17,22,29,34,39,44,49,56)

UPPS <- UPPS %>% dplyr::mutate(
  across(
    .cols = reverseAnswerColsUPPS,
    .fns = ~ case_when(
      .x == "Agree strongly" ~ 4,
      .x == "Agree Strongly" ~ 4,
      .x == "Agree somewhat" ~ 3,
      .x == "Agree Some" ~ 3,
      .x == "Disagree somewhat" ~ 2,
      .x == "Disagree Some" ~ 2,
      .x == "Disagree strongly" ~ 1,
      .x == "Disagree Stronglu" ~ 1,
    )
  )
)

UPPS <- UPPS %>% dplyr::mutate(
  across(
    .cols = answerColsUPPS,
    .fns = ~ case_when(
      .x == "Agree strongly" ~ 1,
      .x == "Agree somewhat" ~ 2,
      .x == "Disagree somewhat" ~ 3,
      .x == "Disagree strongly" ~ 4,
    )
  )
)

UPPS$UPPSNegativeUrgency_score = rowMeans(UPPS[,c(54,3,8,13,18,23,30,35,45,51,59)])
UPPS$UPPSLackOfPremed_score = rowMeans(UPPS[,c(2,7,12,17,22,29,34,39,44,49,56)])
UPPS$UPPSLackOfPreserv_score = rowMeans(UPPS[,c(5,15,20,25,28,33,38,43,10,48)])
UPPS$UPPSSensationSeeking_score = rowMeans(UPPS[,c(4,9,14,19,24,27,32,37,42,47,52,57)])
UPPS$UPPSPositiveUrgency_score = rowMeans(UPPS[,c(6,11,16,21,26,31,36,41,46,50,53,55,58,60)])

names(UPPS) <- c("subject_id", "UPPSQ1", "UPPSQ2", "UPPSQ3", "UPPSQ4", "UPPSQ5", "UPPSQ6", "UPPSQ7", "UPPSQ8", "UPPSQ9", "UPPSQ10", 
                 "UPPSQ11", "UPPSQ12", "UPPSQ13", "UPPSQ14", "UPPSQ15", "UPPSQ16","UPPSQ17", "UPPSQ18", "UPPSQ19", "UPPSQ20", "UPPSQ21", "UPPSQ12", "UPPSQ23", 
                 "UPPSQ24", "UPPSQ25", "UPPSQ26", "UPPSQ27", "UPPSQ28", "UPPSQ29", "UPPSQ30", "UPPSQ31", "UPPSQ32", "UPPSQ33", 
                 "UPPSQ34", "UPPSQ35", "UPPSQ36", "UPPSQ37", "UPPSQ38","UPPSQ39", "UPPSQ40", "UPPSQ41", "UPPSQ42","UPPSQ43", "UPPSQ44",
                 "UPPSQ45", "UPPSQ46", "UPPSQ47", "UPPSQ48", "UPPSQ49", "UPPSQ50", "UPPSQ51", "UPPSQ52", "UPPSQ53", "UPPSQ54", "UPPSQ55", "UPPSQ56",
                 "UPPSQ57", "UPPSQ58", "UPPSQ59", "UPPSNegativeUrgency_score", "UPPSLackOfPremed_score", "UPPSLackOfPreserv_score", "UPPSSensationSeeking_score",
                 "UPPSPositiveUrgency_score")


#Raven A

RavenA$RavenA_score <- NA
numberOfSids <- length(RavenA$participant_id)
scoreVec = numeric(numberOfSids)
for (row in 1:nrow(RavenA)) {
  Q1 <- RavenA[row, 2]
  Q2 <- RavenA[row, 3]
  Q3 <- RavenA[row, 4]
  Q4 <- RavenA[row, 5]
  Q5 <- RavenA[row, 6]
  Q6 <- RavenA[row, 7]
  Q7 <- RavenA[row, 8]
  Q8 <- RavenA[row, 9]
  Q9 <- RavenA[row, 10]
  Q10 <- RavenA[row, 11]
  Q11 <- RavenA[row, 12]
  Q12 <- RavenA[row, 13]
  if (Q1 == 4){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q2 == 5){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q3 == 1){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q4 == 2){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q5 == 6){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q6 == 3){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q7 == 6){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q8 == 2){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q9 == 1){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q10 == 3){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q11 == 5){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q12 == 4){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
}
RavenA$RavenA_score <- scoreVec

#Raven B

RavenB$RavenB_score <- NA
numberOfSids <- length(RavenB$participant_id)
scoreVec = numeric(numberOfSids)
for (row in 1:nrow(RavenB)) {
  Q1 <- RavenB[row, 2]
  Q2 <- RavenB[row, 3]
  Q3 <- RavenB[row, 4]
  Q4 <- RavenB[row, 5]
  Q5 <- RavenB[row, 6]
  Q6 <- RavenB[row, 7]
  Q7 <- RavenB[row, 8]
  Q8 <- RavenB[row, 9]
  Q9 <- RavenB[row, 10]
  Q10 <- RavenB[row, 11]
  Q11 <- RavenB[row, 12]
  Q12 <- RavenB[row, 13]
  if (Q1 == 2){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q2 == 6){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q3 == 1){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q4 == 2){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q5 == 1){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q6 == 3){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q7 == 5){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q8 == 6){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q9 == 4){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q10 == 3){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q11 == 4){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q12 == 5){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
}
RavenB$RavenB_score <- scoreVec

#Raven C

RavenC$RavenC_score <- NA
numberOfSids <- length(RavenC$participant_id)
scoreVec = numeric(numberOfSids)
for (row in 1:nrow(RavenC)) {
  Q1 <- RavenC[row, 2]
  Q2 <- RavenC[row, 3]
  Q3 <- RavenC[row, 4]
  Q4 <- RavenC[row, 5]
  Q5 <- RavenC[row, 6]
  Q6 <- RavenC[row, 7]
  Q7 <- RavenC[row, 8]
  Q8 <- RavenC[row, 9]
  Q9 <- RavenC[row, 10]
  Q10 <- RavenC[row, 11]
  Q11 <- RavenC[row, 12]
  Q12 <- RavenC[row, 13]
  if (Q1 == 8){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q2 == 2){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q3 == 3){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q4 == 8){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q5 == 7){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q6 == 4){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q7 == 5){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q8 == 1){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q9 == 7){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q10 == 6){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q11 == 1){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q12 == 2){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
}
RavenC$RavenC_score <- scoreVec

#Raven D

RavenD$RavenD_score <- NA
numberOfSids <- length(RavenD$participant_id)
scoreVec = numeric(numberOfSids)
for (row in 1:nrow(RavenD)) {
  Q1 <- RavenD[row, 2]
  Q2 <- RavenD[row, 3]
  Q3 <- RavenD[row, 4]
  Q4 <- RavenD[row, 5]
  Q5 <- RavenD[row, 6]
  Q6 <- RavenD[row, 7]
  Q7 <- RavenD[row, 8]
  Q8 <- RavenD[row, 9]
  Q9 <- RavenD[row, 10]
  Q10 <- RavenD[row, 11]
  Q11 <- RavenD[row, 12]
  Q12 <- RavenD[row, 13]
  if (Q1 == 3){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q2 == 4){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q3 == 3){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q4 == 7){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q5 == 8){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q6 == 6){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q7 == 5){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q8 == 4){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q9 == 1){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q10 == 2){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q11 == 5){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q12 == 6){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
}
RavenD$RavenD_score <- scoreVec

#Raven D

RavenE$RavenE_score <- NA
numberOfSids <- length(RavenE$participant_id)
scoreVec = numeric(numberOfSids)
for (row in 1:nrow(RavenE)) {
  Q1 <- RavenE[row, 2]
  Q2 <- RavenE[row, 3]
  Q3 <- RavenE[row, 4]
  Q4 <- RavenE[row, 5]
  Q5 <- RavenE[row, 6]
  Q6 <- RavenE[row, 7]
  Q7 <- RavenE[row, 8]
  Q8 <- RavenE[row, 9]
  Q9 <- RavenE[row, 10]
  Q10 <- RavenE[row, 11]
  Q11 <- RavenE[row, 12]
  Q12 <- RavenE[row, 13]
  if (Q1 == 7){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q2 == 6){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q3 == 8){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q4 == 2){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q5 == 1){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q6 == 5){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q7 == 2){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  } 
  if (Q8 == 4){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q9 == 1){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q10 == 5){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q11 == 3){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
  if (Q12 == 5){
    scoreVec[row] <- scoreVec[row] + 1
  } else {
    scoreVec[row] <- scoreVec[row]
  }
}
RavenE$RavenE_score <- scoreVec

#STRAIN
STRAIN = read.csv('STRAIN.csv', sep = ',', header = T)
STRAIN <- STRAIN[STRAIN$ID <= 900, ] #Get out practice IDs
STRAIN = select(STRAIN, -c(2,3)) #get rid of unneccessary columns
names(STRAIN)[names(STRAIN) == 'ID'] <- "subject_id"
oldNamesStrain <- colnames(STRAIN)
newnamesStrain <- c('subject_id')
for (i in 2:ncol(STRAIN)) {
  col = paste("STRAIN", oldNamesStrain[i], sep = '.')
  newnamesStrain = c(newnamesStrain, col)
}
names(STRAIN) <- newnamesStrain


#Merging Dataframes

ravens_list <- list(RavenA, RavenB, RavenC, RavenD, RavenE)

RavensDF <- ravens_list %>% reduce(full_join, by='participant_id')
RavensDF <- RavensDF[-c(1, 2), ]

surveyDFlistSubj <- list(Bis11, BisBas, BriefCOPE, CAARS, ERQ,
                     HCQ, PHQ, STAI, SURPS, UPPS, WHOQOL, SHAPS ,AddictionHistory,DemoSurvey, FirstPleasure, STRAIN)

surveyDFlistPart <- list(RavensDF, ASI)

SurveyDF <- idDF


for (i in 1:length(surveyDFlistSubj)) {
  newSurvey <- surveyDFlistSubj[[i]]
  SurveyDF <- merge(SurveyDF, newSurvey, by = 'subject_id', all = T)
}

for (i in 1:length(surveyDFlistPart)) {
  newSurvey <- surveyDFlistPart[[i]]
  SurveyDF <- merge(SurveyDF, newSurvey, by = 'participant_id', all = T)
}

#Fix HCQ

for (i in 1:nrow(SurveyDF)) {
  cohortNum <- SurveyDF[i, "Cohort"]
  if (cohortNum == 3) {
    SurveyDF[, 136:151][is.na(SurveyDF[, 136:151])] <- 0
  }
}


#Making just score data frames
Bis11ScoreCols <- grep("score", colnames(Bis11))
Bis11Score = Bis11[, c(1,Bis11ScoreCols)]

BisBasScoreCols <- grep("score", colnames(BisBas))
BisBasScore = BisBas[, c(1,BisBasScoreCols)]

BriefCopeScoreCols <- grep("score", colnames(BriefCOPE))
BriefCopeScore = BriefCOPE[, c(1,BriefCopeScoreCols)]

CAARSScoreCols <- grep("score", colnames(CAARS))
CAARSScore = CAARS[, c(1,CAARSScoreCols)]

ERQScoreCols <- grep("score", colnames(ERQ))
ERQScore = ERQ[, c(1,ERQScoreCols)]

HCQScoreCols <- grep("score", colnames(HCQ))
HCQScore = HCQ[, c(1,HCQScoreCols)]

PHQScoreCols <- grep("score", colnames(PHQ))
PHQScore = PHQ[, c(1,PHQScoreCols)]

STAIScoreCols <- grep("score", colnames(STAI))
STAIScore = STAI[, c(1,STAIScoreCols)]

SURPSScoreCols <- grep("score", colnames(SURPS))
SURPSScore = SURPS[, c(1,SURPSScoreCols)]

UPPSScoreCols <- grep("score", colnames(UPPS))
UPPSScore = UPPS[, c(1,UPPSScoreCols)]

WHOQOLScoreCols <- grep("score", colnames(WHOQOL))
WHOQOLScore = WHOQOL[, c(1,WHOQOLScoreCols)]

RAVENScoreCols <- grep("score", colnames(RavensDF))
RAVENScore = RavensDF[, c(1,RAVENScoreCols)]

SHAPSScoreCols <- grep("score", colnames(SHAPS))
SHAPSScore = SHAPS[, c(1,SHAPSScoreCols)]

STRAINScores = STRAIN[, -c(2:4)]

SurveyScores <- idDF

surveyDFScorelist <- list(Bis11Score, BisBasScore, BriefCopeScore, CAARSScore, ERQScore,
                          HCQScore, PHQScore, SHAPSScore,STAIScore, SURPSScore, UPPSScore, WHOQOLScore, STRAINScores)


#Putting scores together and fixing up
for (i in 1:length(surveyDFScorelist)) {
  newSurvey <- surveyDFScorelist[[i]]
  SurveyScores <- merge(SurveyScores, newSurvey, by = 'subject_id', all = T)
}


SurveyScores <- merge(SurveyScores, RAVENScore, by = 'participant_id', all = T)

for (i in 1:nrow(SurveyScores)) {
  cohortNum <- SurveyScores[i, "Cohort"]
  if (cohortNum == 3) {
    SurveyScores[, "HCQscore"][is.na(SurveyScores[, "HCQscore"])] <- 0
  }
}


#Writing out all the files. You will need to change accordingly
outpathDF = '/Users/keatonmackey/Desktop/fullsurvey.csv'
write.csv(SurveyScores, '/Users/keatonmackey/Desktop/fullsurveyscores.csv', row.names = FALSE)

write.csv(SurveyDF, outpathDF, row.names = FALSE)



#Adding variables from ITC and Lottery
ITCdata = read.csv('DelayedDiscountingData.csv', sep = ',', header = T)
ITCdata = select(ITCdata, -c(12)) #get rid of unnecessary column
names(ITCdata)[names(ITCdata) == 'Sub.Id'] <- "subject_id"

Lotterydata = read.csv('riskNAmbigParticipantData.csv', sep = ',', header = T)
Lotterydata = select(Lotterydata, -c(8)) #get rid of unnecessary column
names(Lotterydata)[names(Lotterydata) == 'Sub.Id'] <- "subject_id"

ScoresAllTaskDF <- merge(SurveyScores, ITCdata, by = 'subject_id', all = T)
ScoresAllTaskDF <- merge(ScoresAllTaskDF, Lotterydata, by = 'subject_id', all = T)


write.csv(ScoresAllTaskDF, '/Users/keatonmackey/Desktop/all_scores.csv', row.names = FALSE)



