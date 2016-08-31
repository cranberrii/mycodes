library(dplyr)
library(ggplot2)

hr <- read.csv("~/eugeneyan/hr/FULL TOM 2015 - Copy.csv")

# Recode leaver as factor
hr$attrition <- as.factor(hr$Leaver)

# Change values to lowercase
hr <- data.frame(lapply(hr, function(v) {
	if (is.factor(v)) return(tolower(v)) 
	else return(v) 
	}))

# Examine distribution of Leaver
# Looks like 1 = attrition, 2 & 3 = transfer
hr %>% 
	group_by(attrition) %>%
	count(attrition) %>%
	mutate(proportion = n/sum(n))

# Examine distribution of contract
hr %>% 
	group_by(Type.of.Contract) %>%
	count(Type.of.Contract) %>%
	mutate(proportion = n/sum(n))

# Filter dataset to only include attrition = 1 or 0
# Filter out if contract is internship
hr <- hr %>%
	filter(attrition %in% c(0, 1)) %>%
	filter(Type.of.Contract == 'localcontract')

hr_attrit <- hr %>% filter(attrition == '1')

# Length of service by attrition
# It seems that attrition often occurs at the 8 - 15 month period, 
# before dropping at the 26 month period
ggplot(data = hr, aes(x = Length.of.Service, fill = attrition)) + 
	geom_density(alpha=0.33)

ggplot(data = hr_attrit, aes(x = Length.of.Service, fill = attrition)) + 
	geom_histogram(alpha=0.5, binwidth=1)

# Job grade by attrition
hr$job_grade <- substring(text = as.character(hr$JOB.GRADE), 
													first = 0, last = 1)

# Job grade by attrition
# Examine distribution of job_grade
hr %>% 
	group_by(job_grade, attrition) %>%
	count(job_grade) %>%
	mutate(proportion = n/sum(n))

# Looks like attrition likelihood is consistent across job_grades
ggplot(data = hr, aes(x = job_grade, ..count..)) + 
	geom_bar(aes(fill = attrition), position = 'dodge')

# Attrition looks to be about 30% for job_grade between 2 - 5
hr %>% 
	group_by(job_grade, attrition) %>%
	summarise(n = n()) %>%
	mutate(freq = n/sum(n))

# Does promotion reduce attrition?
ggplot(data = hr, aes(x = PROMO.1.YEAR, ..count..)) +
	geom_bar(aes(fill = attrition), position = 'dodge', binwidth = 1)

# Those with promo values that are higher have fewer attrition
hr %>% 
	group_by(PROMO.1.YEAR, attrition) %>%
	summarise(n = n()) %>%
	mutate(freq = n/sum(n))

# Those with salary increment 2 seems to have higher attrition
hr %>% 
	group_by(SalaryIncrementin1Year, attrition) %>%
	summarise(n = n()) %>%
	mutate(freq = n/sum(n))

# Is there any relationship with age? Doesn't look like it.
ggplot(data = hr, aes(x = EmployeeAge, fill = attrition)) + 
	geom_density(alpha=0.33)


# Any relationship with education level? Clearly non-degree have higher
# attrition. It could be due to how well the job pays as well, and seniority 
# of the role.
hr %>% 
	group_by(EducationLevel, attrition) %>%
	summarise(n = n()) %>%
	mutate(freq = n/sum(n))

ggplot(data = hr, aes(x = EducationLevel, ..count..)) +
	geom_bar(aes(fill = attrition), position = 'dodge', binwidth = 1)

# Is there any relationship with MaritalStatus?
hr %>% 
	group_by(MaritalStatus, attrition) %>%
	summarise(n = n()) %>%
	mutate(freq = n/sum(n))

# Logistic Regression
log_mod <- glm(attrition ~ EmployeeAge + Length.of.Service + EducationLevel + PROMO.1.YEAR, family=binomial(link='logit'), data=hr)
summary(log_mod)

hr %>% 
	group_by(MainLoc, attrition) %>%
	summarise(n = n()) %>%
	mutate(freq = n/sum(n))
