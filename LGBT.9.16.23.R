

library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)


ls()
rm(list = ls())


setwd("C:/Users/acandia/OneDrive - Larkin Street Youth Services/Documents/Directories/onetime.reqs/LGBT.9.16.24")

###Load the Data
active <- read.csv("active.csv", header = TRUE)
cm2 <- read.csv("cma2.csv", header = TRUE)
cm1 <- read.csv("cm1.csv", header = TRUE)

dems <- read.csv("dems.csv", header = TRUE)
refs <- read.csv("ex.ref.csv", header = TRUE)

active$Program.End.Date <- as.Date(active$Program.End.Date, format = "%m/%d/%Y")
active$Program.Start.Date <- as.Date(active$Program.Start.Date, format = "%m/%d/%Y")
active <- active |> distinct(ParticipantID, .keep_all = TRUE)
#610
active <- active |> filter(Program.Name != "Lark Inn" & Program.Name != "Diamond Youth Shelter")
#441


active <- mutate(active, 
                 Dend = if_else(is.na(Program.End.Date), ymd("2023-06-30"), Program.End.Date),
                 Days = as.numeric(Dend - Program.Start.Date))

active <- left_join(active, dems, by = "ParticipantID")

active <- data.frame(active)
active <- active |> select(ParticipantID:Program.End.Date, Gender, SexualOrientation)

lb.1<- active |> filter(Gender %in% c("Gender Nonconforming/Non-Binary", "Gender Not Listed", "Genderqueer", "Transgender Man", "Transgender Woman", "Questioning", "Intersex") | SexualOrientation %in% c("Lesbian", "Queer", "Bisexual", "Sexual Orientation Not Listed", "Questioning"))
##136 LBTQ + 

lb.2 <- lb.1 |> filter(Gender == "Cisgender Man" & SexualOrientation == "Bisexual")
#18
##removed the cismale gay from the DF
lb.1 <- anti_join(lb.1, lb.2, by = "ParticipantID")
##117

##keep the elements of "exits" that do NOT appear in lb.1
active.1 <- anti_join(active, lb.1, by = "ParticipantID")

##bind in the cismale bi to df
active.1 <- rbind(active.1, lb.2)


c.gay <- active.1 |> filter(SexualOrientation %in% c("Gay", "Bisexual") & Gender == "Cisgender Man")
##54 cisgender gay

active.2 <- anti_join(active.1, c.gay, by = "ParticipantID")

active.2 |> count(Gender)
active.2 |> count(SexualOrientation)

c.het <- active.2 |> filter(SexualOrientation == "Straight/Heterosexual" & Gender %in% c("Cisgender Man", "Cisgender Woman"))
##262 cis.het

##removed #1 gay (must have been woman, 3 refused, 12, unknown from the dataset )
f.gay <- active.2 |> filter(SexualOrientation == "Gay")
##join with lb.1
lb.1 <- rbind(lb.1, f.gay)
#120

rm(lb.2, f.gay, active.1, active.2)

###########################################
#### Run Total numbers Exits/total housed
##########################################

### Total LBTQ +
### 120

## Total Cis Hetero 
### 262

## Total cis.male gay
### #54
rm(dems)
colnames(dems)

exits <- active |> filter(!is.na(Program.End.Date))

exits <- exits |> filter(Program.End.Date <= "2023-06-30")
#151
exits <- exits |> group_by(ParticipantID) |> slice_max(Program.End.Date) |> distinct(ParticipantID, .keep_all = TRUE)
##151 exits

exits <- left_join(exits, dems, by = "ParticipantID")
colnames(exits)
exits <- data.frame(exits)
exits <- exits |> select(ParticipantID:Days, Gender, SexualOrientation)
colnames(exits)
lb.ex <- exits |> filter(Gender %in% c("Gender Nonconforming/Non-Binary", "Gender Not Listed", "Genderqueer", "Transgender Man", "Transgender Woman", "Questioning", "Intersex") | SexualOrientation %in% c("Lesbian", "Queer", "Bisexual", "Sexual Orientation Not Listed", "Questioning"))
##43 LBTQ + 

lb.ex.2 <- lb.ex |> filter(Gender == "Cisgender Man" & SexualOrientation == "Bisexual")
#5

##removed the cismale gay from the DF
lb.ex <- anti_join(lb.ex, lb.ex.2, by = "ParticipantID")
##38

##keep the elements of "exits" that do NOT appear in lb.1
exits.1 <- anti_join(exits, lb.ex, by = "ParticipantID")

##bind in the cismale bi to df
exits.1 <- rbind(exits.1, lb.ex.2)


c.gay <- exits.1 |> filter(SexualOrientation %in% c("Gay", "Bisexual") & Gender == "Cisgender Man")
##18 cisgender gay

exits.2 <- anti_join(exits.1, c.gay, by = "ParticipantID")


c.het <- exits.2 |> filter(SexualOrientation == "Straight/Heterosexual" & Gender %in% c("Cisgender Man", "Cisgender Woman"))
##91 cis.het


##removed #1 gay (must have been woman, 3 refused, 12, unknown from the dataset )

f.gay <- exits.2 |> filter(SexualOrientation == "Gay")
##join with lb.1

lb.ex <- rbind(lb.ex, f.gay)
dim(lb.ex)
#39


rm(lb.ex.2, f.gay, exits.1, exits.2)

###############
### by quarter
## Q 2
### Q 2
lb.ex.2 <- lb.ex |> filter(Program.End.Date >= "2022-10-01" & Program.End.Date <= "2022-12-31")
#11
c.gay.2<- c.gay |>filter(Program.End.Date >= "2022-10-01" & Program.End.Date <= "2022-12-31")
#4
c.het.2 <- c.het|> filter(Program.End.Date >= "2022-10-01" & Program.End.Date <= "2022-12-31")
#31
31+4+11
### 46 exits, 

lb.ex.3<- lb.ex |> filter(Program.End.Date >= "2023-01-01" & Program.End.Date <= "2023-03-31")
##9
c.gay.3<- c.gay |> filter(Program.End.Date >= "2023-01-01" & Program.End.Date <= "2023-03-31")
#6
c.het.3<- c.het |> filter(Program.End.Date >= "2023-01-01" & Program.End.Date <= "2023-03-31")
#25
##33 exits


###########################################
#### Run these groups against exits cma's
##########################################

cm2$DateTaken <- as.Date(cm2$DateTaken, format = "%m/%d/%Y")
cm2 <- cm2 |> group_by(ParticipantID) |> slice_max(DateTaken) |> distinct(ParticipantID, .keep_all = TRUE)

cm2 <- cm2 |> mutate(DestinationAtExitWeight= ifelse(DestinationAtExit %in% c("Emergency Shelter, including Larkin Street shelters", 
                                                                              "Place not meant for human habitation", "Shelter",
                                                                              "Safe Haven (for DV situations)",
                                                                              "Jail, prison, or juvenile detention facility"),1,
                                                     ifelse(DestinationAtExit %in% c("Hotel or Motel paid for with an emergency shelter voucher", 
                                                                                     "Hospital or other residential non-psychiatric medical facility",
                                                                                     "Psychiatric hospital or other psychiatric facility", 
                                                                                     "Mental health treatment program or center", 
                                                                                     "Substance abuse treatment facility or detox center",
                                                                                     "Long-term care facility", "Other"), 2,
                                                            ifelse(DestinationAtExit %in% c("Client doesn't know", 
                                                                                            "Client prefers not to answer",
                                                                                            "Data was not collected by Case Manager",
                                                                                            "No exit interview was completed", ""), 0, 3))))

#cm2 |> select(DestinationAtExit, DestinationAtExitWeight)


lb.ex <- left_join(lb.ex, cm2, by = "ParticipantID", relationship = "many-to-many")

c.gay <- left_join(c.gay, cm2, by = "ParticipantID")
view(c.gay)

c.het <- left_join(c.het, cm2, by = "ParticipantID")
view(c.het)

colnames(lb.ex)
lb.ex<- data.frame(lb.ex)

colnames(lb.ex)

lb.ex |> count(DestinationAtExitWeight)
# DestinationAtExitWeight  n
# # 1                       0  9
# # 2                       1  2
# # 3                       2  3
# # 4                       3 25

##lb total = 39, rm 12 
## lb new = 25/25+2
# 25/27


lb.ex |> select(DestinationAtExit)
#0 removed

c.gay<- data.frame(c.gay)


c.gay |> count(DestinationAtExitWeight)
# DestinationAtExitWeight  n
# 1                       0  8
# 2                       3 10

##18 - 8 removed
## 10/10=10/10


c.gay |> select(DestinationAtExit)
##exclude 4 from total exits
c.het<- data.frame(c.het)
#c.het |> count(DestinationAtExit == "Unknown")
c.het |> count(DestinationAtExitWeight)

# DestinationAtExitWeight  n
# 1                       0 21
# 2                       1  1
# 3                       2  5
# 4                       3 64

##rm 26
## 91-26 = 65
# 64/64 + 1

#c.het |> select(DestinationAtExit, DestinationAtExitWeight)


### Total = 102 exits


###############
### by quarter
## Q 3

lb.ex.3<- lb.ex |> filter(Program.End.Date >= "2023-01-01" & Program.End.Date <= "2023-03-31")
##9
c.gay.3<- c.gay |> filter(Program.End.Date >= "2023-01-01" & Program.End.Date <= "2023-03-31")
#6
c.het.3<- c.het |> filter(Program.End.Date >= "2023-01-01" & Program.End.Date <= "2023-03-31")
#25

lb.ex.3 |> count(DestinationAtExitWeight)
# DestinationAtExitWeight n
# 1                       0 1
# 2                       2 1
# 3                       3 7

#rm 2, 
# 7/7

c.gay.3 |> count(DestinationAtExitWeight)
# DestinationAtExitWeight n
# 1                       0 2
# 2                       3 4

#rm 2 
# 4/4

c.het.3 |> count(DestinationAtExitWeight)
# DestinationAtExitWeight  n
# 1                       0  3
# 2                       1  1
# 3                       3 21
##rm 3

# 21/21 +1
#21/22

##33




### Q 2
lb.ex.2 <- lb.ex |> filter(Program.End.Date >= "2022-10-01" & Program.End.Date <= "2022-12-31")
#11
c.gay.2<- c.gay |>filter(Program.End.Date >= "2022-10-01" & Program.End.Date <= "2022-12-31")
#4
c.het.2 <- c.het|> filter(Program.End.Date >= "2022-10-01" & Program.End.Date <= "2022-12-31")
#31

##46 exits


lb.ex.2 |> count(DestinationAtExitWeight)
# DestinationAtExitWeight n
# 1                       0 3
# 2                       1 1
# 3                       3 7

##rm 3 dm = 8
## 7/ 7 +1 or 7/8

c.gay.2 |> count(DestinationAtExitWeight)
# 1                       0 2
# 2                       3 2

#rm 2, dm = 2
## 2/2

c.het.2 |> count(DestinationAtExitWeight)
# DestinationAtExitWeight  n
# 1                       0  9
# 2                       1  3
# 3                       2  1
# 4                       3 18

##rm 10, new den 21

## 18/18 +3 or 18/21

8+2 +21

###################################
### able to manage wellness at exit

well <- cm1 |> filter(Version %in% c("Final / At Exit", "Transfer to Other LS Housing Program"))
##153

#ph
well <- well |> mutate(PH = rowMeans(across(HealthBenSysNav:HealthLifeStage)),
                       MH = rowMeans(across(AccessMHResources:FewServices)),
                       EH = rowMeans(across(RangeEmo:SelfReflect)),
                       SH = rowMeans(across(SocialSupport:ConnectPeople)))


well <- well |> mutate(Well = ifelse(PH >= 3.5 & MH >= 3.5 & EH >= 3.5 & SH >= 3.5, 1, 0))

well$DateTaken<- as.Date(well$DateTaken, format = "%m/%d/%Y")
#well.q <- well |> filter(DateTaken>="2024-04-01" &DateTaken <= "2024-06-30")


gay.well<- left_join(c.gay, well, by = "ParticipantID")
view(gay.well)

gay.well <- gay.well |> filter(!is.na(FirstName))
##16 clients

##none had 0's so no exclusions
gay.well |> select(HealthBenSysNav, AccessMHResources, RangeEmo, SocialSupport)

gay.well |> count(Well)
##7 



het.well <- left_join(c.het, well, by = "ParticipantID")
het.well$DateTaken <- as.Date(het.well$DateTaken, format = "%m/%d/%Y")
het.well <- het.well |> group_by(ParticipantID) |> slice_max(DateTaken)|> distinct(ParticipantID, .keep_all = TRUE)
view(het.well)
##89 clients

het.well <- het.well |> filter(!is.na(FirstName))
het.well <- data.frame(het.well)
het.well |> count(Well)
#47 well 



lb.well <- left_join(lb.ex, well, by = "ParticipantID")
lb.well$DateTaken <- as.Date(lb.well$DateTaken, format = "%m/%d/%Y")
lb.well <- lb.well |> group_by(ParticipantID) |> slice_max(DateTaken) |> distinct(ParticipantID, .keep_all = TRUE)
lb.well <- distinct(ParticipantID, .keep_all = TRUE)

lb.well <- lb.well |> filter(!is.na(FirstName))
lb.well <- data.frame(lb.well)
lb.well |> count(Well)
##37 total, 19 well

37+16+89

### by quarter
## Q 2
lb.well.2<- lb.well |> filter(Program.End.Date >= "2022-10-01" & Program.End.Date <= "2022-12-31")
##10
gay.well.2<- gay.well |> filter(Program.End.Date >= "2022-10-01" & Program.End.Date <= "2022-12-31")
#4
het.well.2<- het.well |> filter(Program.End.Date >= "2022-10-01" & Program.End.Date <= "2022-12-31")
#29
29+10+5
### 44 exits, 

lb.well.2 |> count(Well)
#5

gay.well.2 |> count(Well)
#3

het.well.2 |> count(Well)
#14


### Q 3
lb.well.3 <- lb.well |> filter(Program.End.Date >= "2023-01-01" & Program.End.Date <= "2023-03-31")
#9
gay.well.3<- gay.well |>filter(Program.End.Date >= "2023-01-01" & Program.End.Date <= "2023-03-31")
#5
het.well.3 <- het.well|> filter(Program.End.Date >= "2023-01-01" & Program.End.Date <= "2023-03-31")
#25

#39 exits

lb.well.3 |> count(Well)
#5
gay.well.3 |> count(Well)
#2
het.well.3 |> count(Well)
#11


##########################
### external referrals
view(refs)

refs <- refs |> filter(Domain == "Health & Wellness")

lb.ex.ref <- left_join(lb.ex, refs, by = "ParticipantID", relationship == "many-to-many")
lb.ex.ref<- lb.ex.ref |> filter(!is.na(FirstName))
lb.ex.ref |> group_by(ParticipantID) |> count(n()!= 1)
#3

lb.ex |> count(Days <= 186)
##11

#3/28

gay.ref <-left_join(c.gay, refs, by = "ParticipantID", relationship = "many-to-many")
#18 exits
gay.ref <-data.frame(gay.ref)
gay.ref<- gay.ref |> filter(!is.na(FirstName))
gay.ref |> count(n()==1)

gay.ref<- gay.ref |> distinct(ParticipantID, .keep_all = TRUE) |> filter(!is.na(FirstName))
#6 clients with referrals
# 2 clients with 1+ /
c.gay |> count(Days <=186)
#7 removed
#2/11 FY 


het.ref <- left_join(c.het, refs, by = "ParticipantID", relationship = "many-to-many")
het.ref<- het.ref |>filter(!is.na(FirstName))
#11

het.ref |> group_by(ParticipantID) |> count(n()!=1)
##4 more than 1

c.het |> count(Days <=186)
#17

#4/74

##148 exits

## Q 2
lb.ref.2<- left_join(lb.ex.2, refs, by = "ParticipantID", relationship = "many-to-many") 
lb.ref.2<- lb.ref.2 |> filter(!is.na(FirstName))
lb.ref.2 |> group_by(ParticipantID) |> count(n()!=1)
#1/8


gay.ref.2<- left_join(c.gay.2, refs, by = "ParticipantID", relationship = "many-to-many") 
gay.ref.2<- gay.ref |> filter(!is.na(FirstName))
gay.ref.2 |> group_by(ParticipantID) |> count(n()!=1)
#1  1+ refs




het.ref.2<- left_join(c.het.2, refs, by = "ParticipantID", relationship = "many-to-many")
#31 exits
het.ref.2 <- het.ref.2 |> filter(!is.na(FirstName))
het.ref.2 |> group_by(ParticipantID) |> count(n()!=1)
#1

### Q 3
lb.ref.3 <- left_join(lb.ex.3, refs, by = "ParticipantID", relationship = "many-to-many")
lb.ref.3<- lb.ref.3 |> filter(!is.na(FirstName))
lb.ref.2 |> group_by(ParticipantID) |> count(n()!=1)
#1


gay.ref.3<- left_join(c.gay.3, refs, by = "ParticipantID", relationship = "many-to-many")

gay.ref.3<- gay.ref.3 |> filter(!is.na(FirstName))
gay.ref.3 |> group_by(ParticipantID) |> count(n()!=1)
#2

het.ref.3 <- left_join(c.het.3, refs, by = "ParticipantID", relationship= "many-to-many")
het.ref.3 <- het.ref.2 |> filter(!is.na(FirstName))
het.ref.2 |> group_by(ParticipantID) |> count(n()!=1)
#1

###Remove less than 6 months stays

16/30
6/13
34/43
31/72
19+14+31+8
31/72

21+30+23+12
30/86

lb.ex |> count(Days <= 186)
##11

c.gay |> count(Days <= 186)
#7

c.het |> count(Days <=186)
##17

148-17-7-11
##113

lb.ex.2 |> count(Days <= 186)
#4

c.gay.2 |> count(Days <= 186)
#2

c.het.2 |> count(Days <= 186)
#7

31-6-7
#18


lb.ex.3 |> count(Days <= 186)
#4

c.gay.2 |> count(Days <= 186)
#2

c.het.2 |> count(Days <= 186)
#7

#33-13 = 20

#####################
## client feedback survey results
#####################

feed <- read.csv("feedback.csv", header = TRUE)

feed$DateofSurvey <- as.Date(feed$DateofSurvey, format = "%m/%d/%Y")

feed <- feed |> filter(DateofSurvey >= "2022-07-01" &DateofSurvey <= "2023-06-30")
##533 responses

lb.1 <- feed |> filter(Gender %in% c("Gender Nonconforming/Non-Binary", "Gender Not Listed", "gender Unknown", "Genderqueer", "Transgender Male", "Transgender Female", "Questioning", "Intersex") | SexualOrientation %in% c("Lesbian", "Queer", "Bisexual", "Gender Not Listed", "Questioning"))
##158 LBTQ + 

lb.2 <- lb.1 |> filter(Gender %in% c("Cisgender Male", "Male") & SexualOrientation == "Bisexual")
#17

##removed the cismale gay from the DF
lb.1<- anti_join(lb.1, lb.2, by = "SurveyID")
##141

##keep the elements of "feed" that do NOT appear in lb.1
feed.1 <- anti_join(feed, lb.1, by = "SurveyID")

##bind in the cismale bi to df
feed.1 <- rbind(feed.1, lb.2)

c.gay <- feed.1 |> filter(Gender %in% c("Male", "Cisgender Male"))
c.gay <- c.gay |> filter(SexualOrientation %in% c("Bisexual", "Gay"))
##58 cisgender gay

feed.2 <- anti_join(feed.1, c.gay, by = "SurveyID")

write.csv(feed.2, "het.csv")

c.het <-read.csv("het.2.csv", header = TRUE)
##234



234+58+141
##433 total

lb.1 |> count(StaffRespectMe)
# StaffRespectMe  n
# 1              1 84
# 2              2 30
# 3              3 18
# 4              4  2
# 5              5  1
# 6            999  5
# 7             NA  1

##132
# rm: 6
141-6
##135


c.gay |> count(StaffRespectMe)
# StaffRespectMe  n
# 1              1 39
# 2              2  9
# 3              3  5
# 4              4  1
# 5              5  1
# 6            999  3

## 53
# rm 3

#53/55

c.het |> count(StaffRespectMe)
# StaffRespectMe   n
# 1              1 173
# 2              2  41
# 3              3   8
# 4              4   2
# 5            999  10

#214
#224
#rm 10

135+55+224

##414

#################
## overall satisfaction

##433

lb.1 |> count(FeelSafe)
# FeelSafe  n
# 1        1 57
# 2        2 47
# 3        3 17
# 4        4  4
# 5        5  2
# 6      999 14

#104
#rm 14
##151-14 = 137

c.gay |> count(FeelSafe)
# FeelSafe  n
# 1        1 24
# 2        2 20
# 3        3  6
# 4        4  1
# 5        5  2
# 6      999  5

#44
#rm 5
#58-5 = 53

c.het |> count(FeelSafe)
# FeelSafe   n
# 1        1 114
# 2        2  59
# 3        3  13
# 4        4   4
# 5      999  44

#173
#rm 44
234-44
##190
433-14-5-44
#370
################
### Overall Satisfaction
lb.1 |> count(OverallSatisfaction)
# OverallSatisfaction  n
# 1                   1 53
# 2                   2 68
# 3                   3 16
# 4                   4  3
# 5                   5  1

#121
#141

c.gay |> count(OverallSatisfaction)
# OverallSatisfaction  n
# 1                   1 23
# 2                   2 29
# 3                   3  5
# 4                   4  1

#52/58

c.het |> count(OverallSatisfaction)
# OverallSatisfaction   n
# 1                   1 114
# 2                   2 101
# 3                   3  17
# 4                   4   1
# 5                   5   1

#215

#######################
## Would recommend LSYS to a friend

lb.1 |> count(RecommendtoFriend)

# 1                 1 76
# 2                 2 47
# 3                 3  8
# 4                 4  3
# 5                 5  2
# 6               999  5

##rm 5
##123/136

c.gay |> count(RecommendtoFriend)

# RecommendtoFriend  n
# 1                 1 26
# 2                 2 24
# 3                 3  1
# 4                 4  2
# 5                 5  2
# 6               999  3
## rm 3
#50/55


c.het |> count(RecommendtoFriend)
# RecommendtoFriend   n
# 1                 1 147
# 2                 2  60
# 3                 3  13
# 4                 4   2
# 5                 5   1
# 6               999  11

#rm 11

#207 -223


## Q 2
lb.2<- lb.1 |> filter(DateofSurvey >= "2022-10-01" & DateofSurvey <= "2022-12-31")
##20
gay.2<- c.gay |> filter(DateofSurvey >= "2022-10-01" & DateofSurvey <= "2022-12-31")
#4
c.het$DateofSurvey <- as.Date(c.het$DateofSurvey, format = "%m/%d/%Y")
het.2<- c.het |> filter(DateofSurvey >= "2022-10-01" & DateofSurvey <= "2022-12-31")
#29

##53 q2

### Q 3
lb.3 <- lb.1 |> filter(DateofSurvey >= "2023-01-01" & DateofSurvey <= "2023-03-31")
#54
gay.3<- c.gay |>filter(DateofSurvey >= "2023-01-01" & DateofSurvey <= "2023-03-31")
##32
het.3 <- c.het|> filter(DateofSurvey >= "2023-01-01" & DateofSurvey <= "2023-03-31")
#73

#159 q 3


lb.2 |> count(OverallSatisfaction)
##14
#no rm

gay.2 |> count(OverallSatisfaction)
#3

het.2 |> count(OverallSatisfaction)
#28

###
lb.2 |> count(StaffRespectMe)
#19/19
gay.2 |> count(StaffRespectMe)
#4/4
het.2 |> count(StaffRespectMe)
#28/28

###
lb.2 |> count(FeelSafe)
#12/19

gay.2 |> count(FeelSafe)
#4/4
het.2 |> count(FeelSafe)
#25/28


###
#would recommend
lb.2 |> count(RecommendtoFriend)
#rm 1
#16/19

gay.2 |> count(RecommendtoFriend)
#4/4

het.2 |> count(RecommendtoFriend)
#rm 1
#24/28



##Q3
##159

lb.3 |> count(OverallSatisfaction)
##49/54

gay.3 |> count(OverallSatisfaction)
#29/32

het.3 |> count(OverallSatisfaction)
#71/73

###
lb.3 |> count(StaffRespectMe <= 3)
#49/53

gay.3 |> count(StaffRespectMe <= 3)
#29/32
het.3 |> count(StaffRespectMe <= 3)
#71/73

###
lb.3 |> count(FeelSafe <= 2)
#44/54

gay.3 |> count(FeelSafe <= 2)
#23/32
het.3 |> count(FeelSafe <= 2)
#66/73


##
#would recommendd

lb.3 |> count(RecommendtoFriend)
#47/52

gay.3 |> count(RecommendtoFriend)
#26/30

het.3 |> count(RecommendtoFriend)
#69/72
