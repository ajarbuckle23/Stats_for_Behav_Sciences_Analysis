library(tidyverse)

JobTitles <- c("VP+", "Director", "Manager", "Below Manager")
REStatus <- c("R/E", "Non-R/E")

# Manual Value Entry 
DiversityHC <- matrix(rep(2, times = 8), ncol = 4, byrow = TRUE)
colnames(DiversityHC) <- JobTitles
rownames(DiversityHC) <- REStatus
DiversityHC[1,1] <- 17
DiversityHC[1,2] <- 153
DiversityHC[1,3] <- 475
DiversityHC[1,4] <- 317
DiversityHC[2,1] <- 80
DiversityHC[2,2] <- 429
DiversityHC[2,3] <- 845
DiversityHC[2,4] <- 510
DiversityHC <- as.table(DiversityHC)
chisq.test(DiversityHC)

# More streamlined population method
values <- c(5, 29, 90, 95, 8, 44, 114, 108)
DiversityStarts <- matrix(values, ncol = 4, byrow = TRUE)
colnames(DiversityStarts) <- JobTitles
rownames(DiversityStarts) <- REStatus
DiversityStarts <- as.table(DiversityStarts)
chisq.test(DiversityStarts)

