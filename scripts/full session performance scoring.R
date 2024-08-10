# Survival Problem-Solving Scoring Code
# Created by Lindsay Larson

###########FULL CLEANING DATA############
#read in the cleaned individual (pre-session) data
preperf <- read.csv("/Users/lindsaylarson/Library/CloudStorage/GoogleDrive-lindsaylarson2020@u.northwestern.edu/.shortcut-targets-by-id/1vhGzWCkmp0v5UoGZkeC7aiP6d9_4vMan/STRONG Phase 1 Data Collection/Performance Scoring/Problem-Solving Scoring/spring pre-survey.csv")

#script below removes columns 17,33, and columns 49 through 373 (old creativity data)
preperf <- preperf[-c(67:95),-c(1:4,21,37,53:379)] #[rows,columns]

colnames(preperf)

#read in the cleaned team (in session) data
perf <- read.csv("/Users/lindsaylarson/Library/CloudStorage/GoogleDrive-lindsaylarson2020@u.northwestern.edu/.shortcut-targets-by-id/1vhGzWCkmp0v5UoGZkeC7aiP6d9_4vMan/STRONG Phase 1 Data Collection/Performance Scoring/Problem-Solving Scoring/spring ps perf.csv")

perf <- perf[,-c(4:5)] #[rows,columns]

#sort both data sets by PID
preperf <- preperf[order(preperf$PID),]
perf <- perf[order(perf$PID),]

#Add first 3 columns of perf to preperf (double check that you have same number of columns for pre-session data and in session data, or else the code immediately below will not work properly because it is merging based on you sorting the PIDs and assumes that you have perfect matching for each person - one row of pre-survey and one row of in session data per person)
preperf <- cbind(perf[1:3], preperf)





#####Scoring Individual Data######
#below code assumes that initial spreadsheets match the row setups in the original documents - that is, the answer for round 1, object 1 is located in column 5, the answer for round 1, object 2 is located in column 6, and so on

#Round 1 answers are 15, 4, 6, 8, 13, 11, 12, 1, 3, 9, 14, 2, 10, 7, 5
#Q6_x

colnames(preperf)

preperf$round1 <- abs(preperf[,5]-15)+abs(preperf[,6]-4)+abs(preperf[,7]-6)+abs(preperf[,8]-8)+abs(preperf[,9]-13)+abs(preperf[,10]-11)+abs(preperf[,11]-12)+abs(preperf[,12]-1)+abs(preperf[,13]-3)+abs(preperf[,14]-9)+abs(preperf[,15]-14)+abs(preperf[,16]-2)+abs(preperf[,17]-10)+abs(preperf[,18]-7)+abs(preperf[,19]-5)





#Round 2 answers are 15, 1, 14, 3, 4, 13, 9, 2, 12, 5, 10, 11, 8, 6, 7
#Q29_x

preperf$round2 <- abs(preperf[,20]-15)+abs(preperf[,21]-1)+abs(preperf[,22]-14)+abs(preperf[,23]-3)+abs(preperf[,24]-4)+abs(preperf[,25]-13)+abs(preperf[,26]-9)+abs(preperf[,27]-2)+abs(preperf[,28]-12)+abs(preperf[,29]-5)+abs(preperf[,30]-10)+abs(preperf[,31]-11)+abs(preperf[,32]-8)+abs(preperf[,33]-6)+abs(preperf[,34]-7)

#Round 3 answers are 4, 6, 12, 7, 11, 10, 8, 5, 15, 3, 13, 9, 14, 2, 1
#Q48_x
#note round 3 (Q48_x is missing Q48_12, this is ok, just a weird Qualtrics output)

preperf$round3 <- abs(preperf[,35]-4)+abs(preperf[,36]-6)+abs(preperf[,37]-12)+abs(preperf[,38]-7)+abs(preperf[,39]-11)+abs(preperf[,40]-10)+abs(preperf[,41]-8)+abs(preperf[,42]-5)+abs(preperf[,43]-15)+abs(preperf[,44]-3)+abs(preperf[,45]-13)+abs(preperf[,46]-9)+abs(preperf[,47]-14)+abs(preperf[,48]-2)+abs(preperf[,49]-1)

###Standardize the performance scores for each round###
#low is better, means less of a difference between team and expert scores
preperf$z1 <- scale(preperf$round1, center = TRUE, scale = TRUE)
preperf$z2 <- scale(preperf$round2, center = TRUE, scale = TRUE)
preperf$z3 <- scale(preperf$round3, center = TRUE, scale = TRUE)
preperf$zall <- (preperf$z1 + preperf$z2 + preperf$z3)/3


#####Scoring Team Data######
#notice, now you are working with the "perf" dataset which is the cleaned in-session (team) dataset
#Round 1 answers are 15, 4, 6, 8, 13, 11, 12, 1, 3, 9, 14, 2, 10, 7, 5
#Q6_x

perf$round1 <- abs(perf[,5]-15)+abs(perf[,6]-4)+abs(perf[,7]-6)+abs(perf[,8]-8)+abs(perf[,9]-13)+abs(perf[,10]-11)+abs(perf[,11]-12)+abs(perf[,12]-1)+abs(perf[,13]-3)+abs(perf[,14]-9)+abs(perf[,15]-14)+abs(perf[,16]-2)+abs(perf[,17]-10)+abs(perf[,18]-7)+abs(perf[,19]-5)





#Round 2 answers are 15, 1, 14, 3, 4, 13, 9, 2, 12, 5, 10, 11, 8, 6, 7
#Q29_x

perf$round2 <- abs(perf[,20]-15)+abs(perf[,21]-1)+abs(perf[,22]-14)+abs(perf[,23]-3)+abs(perf[,24]-4)+abs(perf[,25]-13)+abs(perf[,26]-9)+abs(perf[,27]-2)+abs(perf[,28]-12)+abs(perf[,29]-5)+abs(perf[,30]-10)+abs(perf[,31]-11)+abs(perf[,32]-8)+abs(perf[,33]-6)+abs(perf[,34]-7)

#Round 3 answers are 4, 6, 12, 7, 11, 10, 8, 5, 15, 3, 13, 9, 14, 2, 1
#Q48_x
#note round 3 (Q48_x is missing Q48_12, this is ok, just a weird Qualtrics output)

perf$round3 <- abs(perf[,35]-4)+abs(perf[,36]-6)+abs(perf[,37]-12)+abs(perf[,38]-7)+abs(perf[,39]-11)+abs(perf[,40]-10)+abs(perf[,41]-8)+abs(perf[,42]-5)+abs(perf[,43]-15)+abs(perf[,44]-3)+abs(perf[,45]-13)+abs(perf[,46]-9)+abs(perf[,47]-14)+abs(perf[,48]-2)+abs(perf[,49]-1)



###Standardize the performance scores for each round###
#low is better, means less of a difference between team and expert scores
perf$z1 <- scale(perf$round1, center = TRUE, scale = TRUE)
perf$z2 <- scale(perf$round2, center = TRUE, scale = TRUE)
perf$z3 <- scale(perf$round3, center = TRUE, scale = TRUE)
perf$zall <- (perf$z1 + perf$z2 + perf$z3)/3

#####Some data description code here - optional for cleaning purposes ####
library(psych)
colnames(perf)
colnames(preperf)
#describe the raw scores by condition
describeBy(perf[,52:54], perf$Condition)
describeBy(preperf[,50:52], preperf$Condition)

#describe the z scores by condition
describeBy(perf[,55:57], perf$Condition)
describeBy(preperf[,53:56], preperf$Condition)


#########Create the final doc of data and clean########
#combine the individual and team data
perfall <- cbind(preperf[,c(3:4,50:56)], perf[50:56])

#rename columns so they are distinct
names(perfall) <- c("uniteam","PID","ind1","ind2","ind3","indz1","indz2","indz3","indzall","team1","team2","team3","teamz1","teamz2","teamz3","teamzall")


#Remove participants who didn't pass attention check (based on Dissertation R script cleaning)
#badlist <- c("999", "848", "110", "174", "941") #updated with those from spring who did not pass in-survey attention checks
#perfall <- perfall[ ! perfall$PID %in% badlist, ]

#edit the below line to save to your own R directory
write.csv(perfall,"/Users/lindsaylarson/Library/CloudStorage/GoogleDrive-lindsaylarson2020@u.northwestern.edu/.shortcut-targets-by-id/1vhGzWCkmp0v5UoGZkeC7aiP6d9_4vMan/STRONG Phase 1 Data Collection/Performance Scoring/Problem-Solving Scoring/Spring_Survival_Performance.csv")


######################################################################################################################################################################################################################################################### BELOW HERE IS BASIC DESCRIPTIVE ANALYSIS - NOT NECESSARY FOR CLEANING PURPOSES RIGHT NOW, BUT USEFUL FOR LATER ANALYSIS OF DATA #########
#####GRaphing the results - these are updated#######
detach("package:psych", unload = TRUE)
library(tidyverse)
#library("ggpubr")
#library(dplyr)
#library(tidyr)


####### Get difference scores at individual level ##########
perfall$diff1 <- (perfall$ind1 - perfall$team1)
perfall$diff2 <- (perfall$ind2 - perfall$team2)
perfall$diff3 <- (perfall$ind3 - perfall$team3)


perfall$diffz1 <- (perfall$indz1 - perfall$teamz1)
perfall$diffz2 <- (perfall$indz2 - perfall$teamz2)
perfall$diffz3 <- (perfall$indz3 - perfall$teamz3)

########Performance Difference ANOVAs at individual level#########
#this code is less helpful for just cleaning up the spring data since it will be all the same condition
perfall$Condition <- factor(perfall$Condition)
time1.lm <- lm(diff1 ~ Condition, data = perfall)
time1.av <- aov(time1.lm)
summary(time1.av)
time1tukey <- TukeyHSD(time1.av)
time1tukey
plot(time1tukey)

perfall$Condition <- factor(perfall$Condition)
time2.lm <- lm(diff2 ~ Condition, data = perfall)
time2.av <- aov(time2.lm)
summary(time2.av)
time2tukey <- TukeyHSD(time2.av)
time2tukey
plot(time2tukey)

perfall$Condition <- factor(perfall$Condition)
time3.lm <- lm(diff3 ~ Condition, data = perfall)
time3.av <- aov(time3.lm)
summary(time3.av)
time3tukey <- TukeyHSD(time3.av)
time3tukey
plot(time3tukey)




####### Aggregate Performance at team level and get difference scores ##########
teamscores <- aggregate(perfall, list(perfall$uniteam), mean)
teamscores <- teamscores[,-c(3)]
teamscores$diff1 <- (teamscores$ind1 - teamscores$team1)
teamscores$diff2 <- (teamscores$ind2 - teamscores$team2)
teamscores$diff3 <- (teamscores$ind3 - teamscores$team3)


teamscores$diffz1 <- (teamscores$indz1 - teamscores$teamz1)
teamscores$diffz2 <- (teamscores$indz2 - teamscores$teamz2)
teamscores$diffz3 <- (teamscores$indz3 - teamscores$teamz3)




#####Survival Raw######
#Pivot Data at individual level
perfall_long = perfall %>%
  pivot_longer(
    cols = -c("Condition","PID","uniteam","indzall","teamzall"),
    names_to = c("level", "time"),
    names_pattern = "([A-Za-z]+)(\\d+)",
    values_to = "score"
  )
#Pivot Data at team level

teamscores_long = teamscores %>%
  pivot_longer(
    cols = -c("Group.1","Condition","uniteam","indzall","teamzall"),
    names_to = c("level", "time"),
    names_pattern = "([A-Za-z]+)(\\d+)",
    values_to = "score"
  )



####Make Difference figures by condition####

teamscores_long$Condition <- as.factor(teamscores_long$Condition)
#Raw Difference perf scores#
teamscores_long %>%
  subset(level %in% c("diff")) %>%
  group_by(time, Condition) %>%
  summarise(m = mean(score)) %>%
  ggplot(aes(x = time, y = m, group = Condition, color = Condition)) + geom_point() + geom_line() + labs(title = "Average Team Member Difference between \n Individual and Team Survival Scores", x = "Time", y = "Difference Scores (lower is better)") + scale_fill_discrete(name = "Condition", labels = c("1" = "Taskwork only","2" = "Teamwork only", "3" = "Taskwork + Teamwork")) + scale_x_discrete(labels=c("1" = "Round 1","2"="Round 2","3"="Round 3"))

#standardizes Difference perf scores#

teamscores_long %>%
  subset(level %in% c("diffz")) %>%
  group_by(time, Condition) %>%
  summarise(m = mean(score)) %>%
  ggplot(aes(x = time, y = m, group = Condition, color = Condition)) + geom_point() + geom_line() + labs(title = "Average Team Member Difference between \n Individual and Team Survival Scores (standardized)", x = "Time", y = "Difference Scores (lower is better)") + scale_fill_discrete(name = "Condition", labels = c("1" = "Taskwork only","2" = "Teamwork only", "3" = "Taskwork + Teamwork")) + scale_x_discrete(labels=c("1" = "Round 1","2"="Round 2","3"="Round 3"))

#raw perf scores#

teamscores_long %>%
  subset(level %in% c("team")) %>%
  group_by(time, Condition) %>%
  summarise(m = mean(score)) %>%
  ggplot(aes(x = time, y = m, group = Condition, color = Condition)) + geom_point() + geom_line() + labs(title = "Team Performance Survival Scores by Condition", x = "Time", y = "Scores (lower is better)") + scale_fill_discrete(name = "Condition", labels = c("1" = "Taskwork only","2" = "Teamwork only", "3" = "Taskwork + Teamwork")) + scale_x_discrete(labels=c("1" = "Round 1","2"="Round 2","3"="Round 3"))

#standardized perf scores#

teamscores_long %>%
  subset(level %in% c("teamz")) %>%
  group_by(time, Condition) %>%
  summarise(m = mean(score)) %>%
  ggplot(aes(x = time, y = m, group = Condition, color = Condition)) + geom_point() + geom_line() + labs(title = "Standardized Team Performance Survivsl Scores by Condition", x = "Time", y = "Scores (lower is better)") + scale_fill_discrete(name = "Condition", labels = c("1" = "Taskwork only","2" = "Teamwork only", "3" = "Taskwork + Teamwork")) + scale_x_discrete(labels=c("1" = "Round 1","2"="Round 2","3"="Round 3"))


####Boxplots of survival perf to see how it shakes out by condition a little easier######
teamscores_long$Condition <- as.factor(teamscores_long$Condition)

teamscores_long %>%
  subset(level %in% "diff") %>%
  ggplot(aes(x = Condition, y = score)) +
  geom_boxplot(aes(fill = Condition)) + 
  facet_grid(. ~ time, , labeller=label_both) + 
  labs(title = "Difference in Survival (raw)", x = "Condition", y = "Difference Score") + scale_x_discrete(labels = c("Taskwork only","Teamwork only","Taskwork + Teamwork")) + theme(legend.position = "none")

teamscores_long %>%
  subset(level %in% "diffz") %>%
  ggplot(aes(x = Condition, y = score)) +
  geom_boxplot(aes(fill = Condition)) + 
  facet_grid(. ~ time, , labeller=label_both) + 
  labs(title = "Difference in Survival (z scores)", x = "Condition", y = "Difference Score") + scale_x_discrete(labels = c("Taskwork only","Teamwork only","Taskwork + Teamwork")) + theme(legend.position = "none")

teamscores_long %>%
  subset(level %in% "team") %>%
  ggplot(aes(x = Condition, y = score)) +
  geom_boxplot(aes(fill = Condition)) + 
  facet_grid(. ~ time, , labeller=label_both) + 
  labs(title = "Team Survival Scores by Condition (raw)", x = "Condition", y = "Score") + scale_x_discrete(labels = c("Taskwork only","Teamwork only","Taskwork + Teamwork")) + theme(legend.position = "none")

teamscores_long %>%
  subset(level %in% "teamz") %>%
  ggplot(aes(x = Condition, y = score)) +
  geom_boxplot(aes(fill = Condition)) + 
  facet_grid(. ~ time, , labeller=label_both) + 
  labs(title = "Team Survival Scores by Condition (z scores)", x = "Condition", y = "Score") + scale_x_discrete(labels = c("Taskwork only","Teamwork only","Taskwork + Teamwork")) + theme(legend.position = "none")


###########Randos that were missed in initial data cleaning scoring them and adding in############
#read in the cleaned individual (pre-session) data

preperf <- read.csv("~/Library/CloudStorage/GoogleDrive-lindsaylarson2020@u.northwestern.edu/.shortcut-targets-by-id/0B2Rzx1_pTDVjZnd3NzI2TjhGV0U/~ Research/Lindsay Dissertation/Full Study Data Collection/Dissertation Data Analysis/all sessions pre-survey survival performance missings.csv")

#alist <- c(139,241,249,262,372,493,773,824,906,938)

preperf <- preperf %>% filter(PID %in% c(139,241,249,262,372,493,773,824,906,938))


#script below removes columns 17,33, and columns 49 through 373 (old creativity data)
#preperf <- preperf[-c(67:95),-c(1:4,21,37,53:379)] #[rows,columns]

preperf <- preperf[,-c(17,33,49:374)] #[rows,columns]

colnames(preperf)

#read in the cleaned team (in session) data
perf <- read.csv("~/Library/CloudStorage/GoogleDrive-lindsaylarson2020@u.northwestern.edu/.shortcut-targets-by-id/0B2Rzx1_pTDVjZnd3NzI2TjhGV0U/~ Research/Lindsay Dissertation/Full Study Data Collection/Dissertation Data Analysis/all sessions survival performance missings.csv")

perf <- perf %>% filter(PID %in% c(139,241,249,262,372,493,773,824,906,938))



#perf <- perf[,-c(4:5)] #[rows,columns]

#sort both data sets by PID
preperf <- preperf[order(preperf$PID),]
perf <- perf[order(perf$PID),]

#Add first 3 columns of perf to preperf (double check that you have same number of columns for pre-session data and in session data, or else the code immediately below will not work properly because it is merging based on you sorting the PIDs and assumes that you have perfect matching for each person - one row of pre-survey and one row of in session data per person)
preperf <- cbind(perf[1:3], preperf)





#####Scoring Individual Data######
#below code assumes that initial spreadsheets match the row setups in the original documents - that is, the answer for round 1, object 1 is located in column 5, the answer for round 1, object 2 is located in column 6, and so on

#Round 1 answers are 15, 4, 6, 8, 13, 11, 12, 1, 3, 9, 14, 2, 10, 7, 5
#Q6_x

colnames(preperf)

preperf$round1 <- abs(preperf[,5]-15)+abs(preperf[,6]-4)+abs(preperf[,7]-6)+abs(preperf[,8]-8)+abs(preperf[,9]-13)+abs(preperf[,10]-11)+abs(preperf[,11]-12)+abs(preperf[,12]-1)+abs(preperf[,13]-3)+abs(preperf[,14]-9)+abs(preperf[,15]-14)+abs(preperf[,16]-2)+abs(preperf[,17]-10)+abs(preperf[,18]-7)+abs(preperf[,19]-5)





#Round 2 answers are 15, 1, 14, 3, 4, 13, 9, 2, 12, 5, 10, 11, 8, 6, 7
#Q29_x

preperf$round2 <- abs(preperf[,20]-15)+abs(preperf[,21]-1)+abs(preperf[,22]-14)+abs(preperf[,23]-3)+abs(preperf[,24]-4)+abs(preperf[,25]-13)+abs(preperf[,26]-9)+abs(preperf[,27]-2)+abs(preperf[,28]-12)+abs(preperf[,29]-5)+abs(preperf[,30]-10)+abs(preperf[,31]-11)+abs(preperf[,32]-8)+abs(preperf[,33]-6)+abs(preperf[,34]-7)

#Round 3 answers are 4, 6, 12, 7, 11, 10, 8, 5, 15, 3, 13, 9, 14, 2, 1
#Q48_x
#note round 3 (Q48_x is missing Q48_12, this is ok, just a weird Qualtrics output)

preperf$round3 <- abs(preperf[,35]-4)+abs(preperf[,36]-6)+abs(preperf[,37]-12)+abs(preperf[,38]-7)+abs(preperf[,39]-11)+abs(preperf[,40]-10)+abs(preperf[,41]-8)+abs(preperf[,42]-5)+abs(preperf[,43]-15)+abs(preperf[,44]-3)+abs(preperf[,45]-13)+abs(preperf[,46]-9)+abs(preperf[,47]-14)+abs(preperf[,48]-2)+abs(preperf[,49]-1)

###Standardize the performance scores for each round###
#low is better, means less of a difference between team and expert scores
preperf$z1 <- scale(preperf$round1, center = TRUE, scale = TRUE)
preperf$z2 <- scale(preperf$round2, center = TRUE, scale = TRUE)
preperf$z3 <- scale(preperf$round3, center = TRUE, scale = TRUE)
preperf$zall <- (preperf$z1 + preperf$z2 + preperf$z3)/3

preperf <- preperf %>% rename(ind1 = round1, ind2 = round2, ind3 = round3)

#####Scoring Team Data######
#notice, now you are working with the "perf" dataset which is the cleaned in-session (team) dataset
#Round 1 answers are 15, 4, 6, 8, 13, 11, 12, 1, 3, 9, 14, 2, 10, 7, 5
#Q6_x

perf$round1 <- abs(perf[,5]-15)+abs(perf[,6]-4)+abs(perf[,7]-6)+abs(perf[,8]-8)+abs(perf[,9]-13)+abs(perf[,10]-11)+abs(perf[,11]-12)+abs(perf[,12]-1)+abs(perf[,13]-3)+abs(perf[,14]-9)+abs(perf[,15]-14)+abs(perf[,16]-2)+abs(perf[,17]-10)+abs(perf[,18]-7)+abs(perf[,19]-5)





#Round 2 answers are 15, 1, 14, 3, 4, 13, 9, 2, 12, 5, 10, 11, 8, 6, 7
#Q29_x

perf$round2 <- abs(perf[,20]-15)+abs(perf[,21]-1)+abs(perf[,22]-14)+abs(perf[,23]-3)+abs(perf[,24]-4)+abs(perf[,25]-13)+abs(perf[,26]-9)+abs(perf[,27]-2)+abs(perf[,28]-12)+abs(perf[,29]-5)+abs(perf[,30]-10)+abs(perf[,31]-11)+abs(perf[,32]-8)+abs(perf[,33]-6)+abs(perf[,34]-7)

#Round 3 answers are 4, 6, 12, 7, 11, 10, 8, 5, 15, 3, 13, 9, 14, 2, 1
#Q48_x
#note round 3 (Q48_x is missing Q48_12, this is ok, just a weird Qualtrics output)

perf$round3 <- abs(perf[,35]-4)+abs(perf[,36]-6)+abs(perf[,37]-12)+abs(perf[,38]-7)+abs(perf[,39]-11)+abs(perf[,40]-10)+abs(perf[,41]-8)+abs(perf[,42]-5)+abs(perf[,43]-15)+abs(perf[,44]-3)+abs(perf[,45]-13)+abs(perf[,46]-9)+abs(perf[,47]-14)+abs(perf[,48]-2)+abs(perf[,49]-1)



###Standardize the performance scores for each round###
#low is better, means less of a difference between team and expert scores
perf$z1 <- scale(perf$round1, center = TRUE, scale = TRUE)
perf$z2 <- scale(perf$round2, center = TRUE, scale = TRUE)
perf$z3 <- scale(perf$round3, center = TRUE, scale = TRUE)
perf$zall <- (perf$z1 + perf$z2 + perf$z3)/3

perf <- perf %>% rename(team1 = round1, team2 = round2, team3 = round3)

perfall <- cbind(preperf[,c(3:4,50:52)], perf[52:54])

write.csv(perfall, "~/Library/CloudStorage/GoogleDrive-lindsaylarson2020@u.northwestern.edu/.shortcut-targets-by-id/0B2Rzx1_pTDVjZnd3NzI2TjhGV0U/~ Research/Lindsay Dissertation/Full Study Data Collection/Dissertation Data Analysis/survival perf missings.csv")
