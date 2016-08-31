library(stringr)
library(plyr)
library(tidyr)
library(dplyr)

#data
AonJobLvl <- rep(c("M2","P1"), times=2)
AonFunc <- c("HR","FA","PD","IT")
LazJobTitle <- tolower(c("Compensation & Benefits","Legal Counsel","UI/UX Design","Infrastructure"))
AonJobCode <- rep(0, times=4)
data <- data.frame(cbind(AonJobLvl, AonFunc,LazJobTitle,AonJobCode))
data <- separate(data = data, col = LazJobTitle, into = c("LJT1", "LJT2","LJT3","LJT4"), sep = " ")

#index
AonJobLvl <- c("M2","P1","M2","P1","P2","M1")
AonFunc <-   c("HR","FA","PD","IT","HR","IT")
AonJobTitle2 <- tolower(c("Compensation/Benefits Mgmt","Legal Counsel 5","UI/Human Factors Engineer  2",
                          "Systems Infrastructure 1","Compensation Analyst","Systems Infrastructure 2"))
AonJobCode2 <- c(1002,1011,1022,1031,1001,1032)
index <- data.frame(cbind(AonJobLvl, AonFunc, AonJobTitle2, AonJobCode2))

#function
jobmatch <- function(){
  for i in nrow(data) {
    for j in nrow(index) {
      str_subset(index$AonJobTitle2, data$LJT1[i])
      
      
    }
    
    where match()
  grep() == TRUE
  
  
  }

} 



mergeddata <- merge(data, index, by=c("AonJobLvl","AonFunc"))


grep("UI/UX Design", index$AonJobTitle2, value = TRUE, ignore.case = TRUE) 

str_subset(index$AonJobTitle2, "[UI/UX Design]")

str_subset(index$AonJobTitle2, data$LJT1[2])

str_subset(index$AonJobTitle2, "compensation")

