
RangePenetration <- function(XX, LR) {
    
    df <- read.csv("2016_New Salary Penetration Data.csv")
  
    #prep data
    df$LOCAL...REGIONAL <- tolower(df$LOCAL...REGIONAL)
    df$JOB.GRADE <- as.factor(df$JOB.GRADE)
    df <- df %>% 
      filter(CAREER.BAND != "EX" & CAREER.BAND != "N/A") %>%
      filter(COUNTRY.CURRENCY == XX) %>%
      filter(LOCAL...REGIONAL == LR)
    df$RANGE.PENETRATION <- as.numeric(sub("%","",df$RANGE.PENETRATION))
    df$MIN <- as.numeric(levels(df$MIN))[df$MIN]
    df$MID <- as.numeric(levels(df$MID))[df$MID]
    df$MAX <- as.numeric(levels(df$MAX))[df$MAX]
    
    #plot
    pplot <- ggplot(df, aes(x = FULL.JOB.GRADE, y = ANNUAL.BASE.SALARY...LOCAL, fill = FULL.JOB.GRADE)) + geom_boxplot() + 
              ggtitle(" ") + theme_light() + guides(fill=FALSE) + 
              scale_y_continuous(labels = dollar) + 
              scale_x_discrete(limits=c("SS21","P21","SS22","P22","SS31","P31","SS32","P32","P41","M41","P42","M42","P51","M51","P52","M52")) + 
              geom_smooth(aes(x = FULL.JOB.GRADE, y = MIN, group=1), method='loess', colour='red') +
              geom_smooth(aes(x = FULL.JOB.GRADE, y = MID, group=1), method='loess', colour="black") +
              geom_smooth(aes(x = FULL.JOB.GRADE, y = MAX, group=1), method='loess', colour="blue") + 
              ylab("Annual Base Salary (Local Currency)") + 
              theme(axis.title.y=element_text(size=16), axis.title.x=element_text(size=16), axis.text.y  = element_text(size=16), axis.text.x  = element_text(size=16))
    
    ggsave("penetration_boxplot.jpeg", width = 12, height = 10)
    
  return(pplot)
}

