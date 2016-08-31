library(ggplot2)
library(scales)
library(GGally)
library(dplyr)
library(broom)
library(splines)
library(mgcv)
library(tibble)
library(readr)
library(tidyr)

getwd()
dev.off()

df <- read_csv("2016_New Salary Penetration Data.csv")

#prep data
df$`LOCAL / REGIONAL` <- tolower(df$`LOCAL / REGIONAL`)
df <- df %>% 
  filter(`CAREER BAND` != "EX" & `CAREER BAND` != "N/A") %>%
  filter(`COUNTRY CURRENCY` == "TH") %>%
  filter(`CC1` == "Support" | `CC1` == "Technology")
df$`JOB GRADE` <- as.factor(df$`JOB GRADE`)
df$`RANGE PENETRATION` <- as.numeric(sub("%","",df$`RANGE PENETRATION`))
df$`MIN` <- as.numeric(levels(df$`MIN`))[df$`MIN`]
df$`MID` <- as.numeric(levels(df$`MID`))[df$`MID`]
df$`MAX` <- as.numeric(levels(df$`MAX`))[df$`MAX`]

#plot
ggplot(df, aes(x = `RANGE PENETRATION`, fill = `CC2`)) + geom_density(alpha = 0.5)

##THA
ggplot(df, aes(x = `CC2`, y = `ANNUAL BASE SALARY - LOCAL`, fill =`CC2`)) + geom_boxplot() + 
  ggtitle("SG Salary with Internal Salary Bands") + 
  scale_y_continuous(labels = dollar) + 
  
ggplot(df, aes(x = `Age`, y = `RANGE PENETRATION`, color =`CC2`)) + geom_jitter(size=3)
  
  scale_x_discrete(limits=c("21","22","31","32","41","42","51","52")) + 
  geom_smooth(aes(x = JOB.GRADE, y = MIN, group = 1), colour='red') +
  geom_smooth(aes(x = JOB.GRADE, y = MID, group = 1), colour="black") +
  geom_smooth(aes(x = JOB.GRADE, y = MAX, group = 1), colour="blue")

ggplot(df, aes(x = JOB.GRADE, y = ANNUAL.BASE.SALARY...LOCAL)) + geom_jitter(size=4, aes(color=JOB.GRADE)) + 
  ggtitle("SG Salary with Internal Salary Bands") + 
  scale_y_continuous(labels = dollar) + 
  scale_x_discrete(limits=c("21","22","31","32","41","42","51","52")) + 
  geom_smooth(aes(x = JOB.GRADE, y = MIN, group = 1), colour='red') +
  geom_smooth(aes(x = JOB.GRADE, y = MID, group = 1), colour="black") +
  geom_smooth(aes(x = JOB.GRADE, y = MAX, group = 1), colour="blue")

#####
#OPS#
#####

df <- read.csv("2016_New Salary Penetration Data.csv")
#prep data
df$LOCAL...REGIONAL <- tolower(df$LOCAL...REGIONAL)
df$JOB.GRADE <- as.factor(df$JOB.GRADE)
df$RANGE.PENETRATION <- as.numeric(sub("%","",df$RANGE.PENETRATION))
df <- df %>% 
  filter(CAREER.BAND != "EX" & CAREER.BAND != "N/A") %>%
  filter(COUNTRY.CURRENCY == "TH") %>% 
  #filter(JOB.GRADE == "21" & JOB.GRADE == "22") %>%
  filter(CC1 == "Operations", CC2 == "Customer Service")
df$MIN <- as.numeric(levels(df$MIN))[df$MIN]
df$MID <- as.numeric(levels(df$MID))[df$MID]
df$MAX <- as.numeric(levels(df$MAX))[df$MAX]
df$MIN_SS <- as.numeric(levels(df$MIN_SS))[df$MIN_SS]
df$MID_SS <- as.numeric(levels(df$MID_SS))[df$MID_SS]
df$MAX_SS <- as.numeric(levels(df$MAX_SS))[df$MAX_SS]
df <- df %>% 
  mutate(max_mid = MAX/MID) %>% 
  mutate(RS = (MAX/MIN)-1) %>%
  mutate(MAX2 = MIN) %>%
  mutate(MID2 = MAX2/max_mid) %>%
  mutate(MIN2 = MAX2/(1+RS))

#plot
ggplot(df, aes(x = RANGE.PENETRATION, fill = COUNTRY.CURRENCY)) + geom_density(alpha = 0.5) + 
  facet_grid(COUNTRY.CURRENCY ~ .) + coord_cartesian(xlim=c(-80,150))

ggplot(df) + geom_boxplot( aes(x = JOB.GRADE, y = ANNUAL.BASE.SALARY...LOCAL, fill = JOB.GRADE)) + 
  ggtitle("TH") + guides(fill=FALSE) + 
  scale_y_continuous(labels = dollar, limits=c(0,1400000)) + 
  scale_x_discrete(limits=c("21","22","31","32")) + 
  geom_line(aes(x = JOB.GRADE, y = MIN, group = 1), colour="red", size=1) +
  geom_line(aes(x = JOB.GRADE, y = MID, group = 1), colour="black", size=1) +
  geom_line(aes(x = JOB.GRADE, y = MAX, group = 1), colour="blue", size=1) + 
  geom_line(aes(x = JOB.GRADE, y = MIN_SS, group = 1), colour="red", size=1, linetype="dashed") + 
  geom_line(aes(x = JOB.GRADE, y = MID_SS, group = 1), colour="black", size=1, linetype="dashed") +
  geom_line(aes(x = JOB.GRADE, y = MAX_SS, group = 1), colour="blue", size=1, linetype="dashed") 


  + facet_grid(COUNTRY.CURRENCY ~ .)

ggpairs(df[,c("Length.of.Service","RANGE.PENETRATION","JOB.GRADE","ACTIVE.STATUS")], colour="ACTIVE.STATUS", diag=list(continuous="density", discrete="bar"), axisLabels="show")


#####

ggplot(df, aes(x = FULL.JOB.GRADE, y = ANNUAL.BASE.SALARY...LOCAL, fill = FULL.JOB.GRADE)) + geom_jitter() + 
  ggtitle(" ") + theme_light() + guides(fill=FALSE) + 
  scale_y_continuous(labels = dollar) + 
  scale_x_discrete(limits=c("SS21","SS22","SS31","SS32","P21","P22","P31","P32","P41","P42","M41","M42","P51","P52","M51","M52")) + 
  geom_smooth(aes(x = FULL.JOB.GRADE, y = MIN, group = 1), colour='red') +
  geom_smooth(aes(x = FULL.JOB.GRADE, y = MID, group = 1), colour="black") +
  geom_smooth(aes(x = FULL.JOB.GRADE, y = MAX, group = 1), colour="blue") + 
  ylab("Annual Base Salary (Local Currency)") + 
  theme(axis.title.y=element_text(size=16), axis.title.x=element_text(size=16), axis.text.y  = element_text(size=16), axis.text.x  = element_text(size=16))



ggplot(df, aes(x = RANGE.PENETRATION, fill = COUNTRY.CURRENCY)) + geom_density(alpha = 0.5) + 
  facet_grid(COUNTRY.CURRENCY ~ .)

+ coord_cartesian(xlim=c(-150,150))

ggplot(data=df, aes(x= COUNTRY.CURRENCY, y=JOB.GRADE)) + geom_tile(aes(fill=RANGE.PENETRATION), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + 
  theme_minimal()






#prep data##############################################################################

df$RANGE.PENETRATION..30SEP2015. <- as.numeric(sub("%","",df$RANGE.PENETRATION..30SEP2015.))/100
df$RANGE.PENETRATION..1JAN2016. <- as.numeric(sub("%","",df$RANGE.PENETRATION..1JAN2016.))/100
df$ANNUAL.BASE.SALARY...USD..30SEP2015. <-gsub("[[:punct:]]", "", df$ANNUAL.BASE.SALARY...USD..30SEP2015.)
df$ANNUAL.BASE.SALARY...USD..30SEP2015. <- as.numeric(sub("USD ","",df$ANNUAL.BASE.SALARY...USD..30SEP2015.))
df$ANNUAL.BASE.SALARY...USD...1JAN2016. <-gsub("[[:punct:]]", "", df$ANNUAL.BASE.SALARY...USD...1JAN2016.)
df$ANNUAL.BASE.SALARY...USD...1JAN2016. <- as.numeric(sub("USD ","",df$ANNUAL.BASE.SALARY...USD...1JAN2016.))
df$JOB.GRADE..30SEP2015. <- as.factor(df$JOB.GRADE..30SEP2015.)
df$JOB.GRADE..1JAN2016. <-  as.factor(df$JOB.GRADE..1JAN2016.)
df$FX.Rate <- df$ANNUAL.BASE.SALARY...LOCAL..30SEP2015./df$ANNUAL.BASE.SALARY...USD..30SEP2015.
df$MIN2 <- gsub("[[:punct:]]", "", df$MIN2)
df$MIN2 <- as.numeric(sub(" ","",df$MIN2))
df$MAX2 <- gsub("[[:punct:]]", "", df$MAX2)
df$MAX2 <- as.numeric(sub(" ","",df$MAX2))
df$MIN2.USD <- df$MIN2/df$FX.Rate
df$MAX2.USD <- df$MAX2/df$FX.Rate

#ggplots
ggplot(df, aes(x = RANGE.PENETRATION..30SEP2015.)) + geom_histogram(alpha = 0.5)


ggpairs(df[,c("COUNTRY","RANGE.PENETRATION..30SEP2015.","JOB.GRADE..30SEP2015.")], diag=list(continuous="density", discrete="bar"), axisLabels="show")

ggplot(df, aes(y = RANGE.PENETRATION..30SEP2015., x = JOB.GRADE..30SEP2015.)) + geom_jitter(aes(colour = CAREER.BAND..30SEP2015.))

ggplot(df, aes(x = JOB.GRADE..30SEP2015., y = ANNUAL.BASE.SALARY...USD...1JAN2016., fill = JOB.GRADE..30SEP2015.)) + geom_boxplot() + 
  scale_y_continuous(labels = dollar) + 
  scale_fill_brewer(palette="Blues") + 
  geom_smooth(aes(x = JOB.GRADE..30SEP2015., y = MIN2.USD, group = 1), colour='red') +
  geom_smooth(aes(x = JOB.GRADE..30SEP2015., y = MAX2.USD, group = 1), colour="black")


ggplot(data=df, aes(x=PL.RATING..2015., y=JOB.GRADE..1JAN2016.)) + geom_tile(aes(fill=RANGE.PENETRATION..30SEP2015.), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
