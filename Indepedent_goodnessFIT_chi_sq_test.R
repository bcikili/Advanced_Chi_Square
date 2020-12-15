install.packages('dplyr')
library('dplyr')
install.packages('gmodels')
library("gmodels")
install.packages('tidyr')
library("tidyr")
install.packages('IDPmisc')
library('IDPmisc')



loan <- read.csv('loans.csv')
 
#independent
loan_dict <- read.csv('loans_dictionary.csv')
CrossTable(loan$term, loan$loan_status, chisq = TRUE, expected =TRUE, sresid = TRUE, format= 'SPSS')


#goodness fit chi-square
loan %>% group_by(term) %>% summarise(count =n())

observed = c(29096, 10621)
expected = c(0.67, 0.33)
chisq.test(x =observed, p= expected)

# p value is significant for both tests. Expected values are higher than 5. longer term more charge off, lesser term more people fully paid.

str(loan$issue_d)
loan$Issue_dateR <- as.Date(loan$issue_d, format="%b-%d")
loan1 <- separate(loan, Issue_dateR, c("Ignore", "Issue_Month", "Issue_Year"), sep="-")



CrossTable(loan1$home_ownership, loan1$Issue_Year, fisher = TRUE, chisq = TRUE, mcnemar = TRUE, expected =TRUE, sresid = TRUE, format= 'SPSS')
#Only the year 2011, mortgage status is higher than expected. Ownership status fell after 2009, but not at significant level. Rent status is getting lesser since 2009.

################################

loan1 %>% group_by(loan_status) %>% summarize(count=n())

observed = c(32950, 5627)
expected = c(0.60, 0.40)
chisq.test(x = observed, p = expected)
#expected values differ, that means it does not come from a larger population. 






