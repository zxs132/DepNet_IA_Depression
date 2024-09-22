########## uploading packages ##########

library(dplyr)
library(tidyverse)
library(sjlabelled)
library(moonBook)
library(rrtable)


########## uploading data sets ##########

setwd("~/Desktop/Yonsei/Semester_2/Probabilistic Graphical Model/Final")

# Wave 1
dw1 <- read.csv("RawData/w1_2008_data_220324.csv", fileEncoding = "euc-kr", 
               na.strings = c("99999999", "NA"))
# Wave 10
dw10 <- read.csv("RawData/w10_2017_data_221013.csv", fileEncoding = "euc-kr", 
                na.strings = c("99999999", "NA"))
# Wave 12
dw12 <- read.csv("RawData/w12_2019_data_230126.csv", fileEncoding = "euc-kr", 
                na.strings = c("99999999", "NA"))


########## Creating (time-invariant) variables from dw1 ##########

pskc1 <- data.frame(N_ID = dw1$N_ID)


## peripartum depression
dpr <- dw1 %>% 
 
  transmute(pre = BMt08dpr001 + BMt08dpr002 + BMt08dpr003 + BMt08dpr004 + BMt08dpr005 + BMt08dpr006, # prenatal depression
            post = CMt08dpr007 + CMt08dpr008 + CMt08dpr009 + CMt08dpr010 + CMt08dpr011 + CMt08dpr012) # postnatal depression

peridep <- ifelse(dpr$pre < 14 & dpr$post < 14, 1, ifelse(!is.na(dpr$pre + dpr$post), 2, NA))

pskc1$peridep <- peridep # (1=none, 2=depressive)

## children's gender
pskc1$gender <- dw1$BCh08dmg001 # (1=boy, 2=girl)



########## Creating (time-dependent) variables from dw10 ##########

pskc10 <- data.frame(N_ID = dw10$N_ID)


## year
pskc10$year <- 2017


## mother's depression (current)
dpr <- dw10 %>% 
  
  select(EMt17dpr013, EMt17dpr014, EMt17dpr015, EMt17dpr016, EMt17dpr017, EMt17dpr018) %>%
  
  rowSums

mdep <- ifelse(dpr >= 14, 2, ifelse(!is.na(dpr), 1, NA))

pskc10$mdep <- mdep # # (1=none, 2=depressive)


## Internet addiction
IA_scores <- dw10 %>% 
  
  select(ECh17mid018:ECh17mid032) %>%
  
  mutate(ECh17mid027_r = 5 - ECh17mid027, ECh17mid028_r = 5 - ECh17mid028) # reverse coding

IA_df <- IA_scores %>% 
  
  select(-c(ECh17mid027, ECh17mid028)) %>%
  
  transmute(total = rowSums(.),
            f1 = ECh17mid018 + ECh17mid022 + ECh17mid026 + ECh17mid029 + ECh17mid032,
            f3 = ECh17mid020 + ECh17mid024 + ECh17mid027_r + ECh17mid030,
            f4 = ECh17mid021 + ECh17mid025 + ECh17mid028_r + ECh17mid031)

IA <- ifelse(IA_df$total >= 30 | (IA_df$f1 >= 14 & IA_df$f3 >= 12 & IA_df$f4 >= 11), 2, 
             
             ifelse(!is.na(IA_df$total), 1, NA))

pskc10$IA <- IA # (1=normal, 2=Internet addiction)


## having a smartphone
smart <- ifelse(dw10$DCh17mid012 == 1, 2, ifelse(!is.na(dw10$DCh17mid012), 1, NA))

pskc10$smart <- smart # (1=no, 2=yes)


## overall happiness
happy <- rep(NA, dim(dw10)[1])
happy[which(dw10$JCh17shs011 == 1 | dw10$JCh17shs011 == 2)] <- 2
happy[which(dw10$JCh17shs011 == 3 | dw10$JCh17shs011 == 4)] <- 1

pskc10$happy <- happy # (1=happy, 2=unhappy)


## sleep duration
sleepm <- dw10$DCh17slp022e * 5/7 + dw10$DCh17slp023e * 2/7

sleep <- cut(sleepm, breaks = c(0, 9, Inf), right = F, labels = c(1, 2))

pskc10$sleep <- as.numeric(sleep) # (1=less than 9-hours, 2=more)


## self-report health status
health <- rep(NA, dim(dw10)[1])
health[which(dw10$DCh17hlt049 == 1 | dw10$DCh17hlt049 == 2)] <- 1
health[which(dw10$DCh17hlt049 == 3)] <- 2
health[which(dw10$DCh17hlt049 == 4 | dw10$DCh17hlt049 == 5)] <- 3

pskc10$health <- health # (1=low, 2=middle, 3=high)


## CBCL - caregiver rating behavior problem 
CBCL <- ifelse(dw10$ICh17cbt001 >= 64 | dw10$ICh17cbt002 >= 64 | dw10$ICh17cbt003 >= 64, 2, 
               
               ifelse(!is.na(dw10$ICh17cbt001 + dw10$ICh17cbt002 + dw10$ICh17cbt003), 1, NA))

pskc10$CBCL <- CBCL # (1=no, 2=yes)


## family composition
family <- ifelse(dw10$DMt17dmg029 == 1 & dw10$DFt17dmg029 == 1, 1, 
                 
                 ifelse(!is.na(dw10$DMt17dmg029 + dw10$DFt17dmg029), 2, NA))

pskc10$family <- family # (1=living with both parents, 2=the others)


## household income
income <- cut(dw10$DHu17ses006, breaks = c(0, 286.1395, 507.9435, Inf), 
              
              right = F, labels = c(1, 2, 3))

pskc10$income <- as.numeric(income) # (1=low, 2=middle, 3=high)


## employment status of parents
empstatus <- rep(2, dim(dw10)[1])
empstatus[which(dw10$DMt17jcg003 < 4 & dw10$FFt17jcg023 < 4)] <- 3
empstatus[which(dw10$DMt17jcg003 == 4 & dw10$FFt17jcg023 == 4)] <- 1
empstatus[which(is.na(dw10$DMt17jcg003 + dw10$FFt17jcg023))] <- NA

pskc10$empstatus <- empstatus # (1=none, 2=either, 3=both)



########## Creating (time-dependent) variables from dw12 ##########

pskc12 <- data.frame(N_ID = dw12$N_ID)


## year
pskc12$year <- 2019


## mother's depression (current)
dpr <- dw12 %>% 
  
  select(EMt19dpr013, EMt19dpr014, EMt19dpr015, EMt19dpr016, EMt19dpr017, EMt19dpr018) %>%
  
  rowSums

mdep <- ifelse(dpr >= 14, 2, ifelse(!is.na(dpr), 1, NA))

pskc12$mdep <- mdep # (1=none, 2=depressive)


## Internet addiction
IA_scores <- dw12 %>% 
  
  select(ECh19mid018:ECh19mid032) %>%
  
  mutate(ECh19mid027_r = 5 - ECh19mid027, ECh19mid028_r = 5 - ECh19mid028) # reverse coding

IA_df <- IA_scores %>%
  
  select(-c(ECh19mid027, ECh19mid028)) %>%
  
  transmute(total = rowSums(.),
            f1 = ECh19mid018 + ECh19mid022 + ECh19mid026 + ECh19mid029 + ECh19mid032,
            f3 = ECh19mid020 + ECh19mid024 + ECh19mid027_r + ECh19mid030,
            f4 = ECh19mid021 + ECh19mid025 + ECh19mid028_r + ECh19mid031)

IA <- ifelse(IA_df$total >= 30 | (IA_df$f1 >= 14 & IA_df$f3 >= 12 & IA_df$f4 >= 11), 2, 
             
             ifelse(!is.na(IA_df$total), 1, NA))

pskc12$IA <- IA # (1=normal, 2=Internet addiction)


## having smartphone
smart <- ifelse(dw12$DCh19mid012 == 1, 2, ifelse(!is.na(dw12$DCh19mid012), 1, NA))

pskc12$smart <- smart # (1=no, 2=yes)


## overall happiness
happy <- rep(NA, dim(dw12)[1])
happy[which(dw12$JCh19shs011 == 1 | dw12$JCh19shs011 == 2)] <- 2
happy[which(dw12$JCh19shs011 == 3 | dw12$JCh19shs011 == 4)] <- 1

pskc12$happy <- happy # (1=happy, 2=unhappy)


## sleep duration
sleepm <- dw12$DCh19slp024a * 5/7 + dw12$DCh19slp024c * 2/7

sleep <- cut(sleepm, breaks = c(0, 9, Inf), right = F, labels = c(1, 2))

pskc12$sleep <- as.numeric(sleep) # (1=less than 9-hours, 2=more)


## self-report health status
health <- rep(NA, dim(dw12)[1])
health[which(dw12$DCh19hlt049 == 1 | dw12$DCh19hlt049 == 2)] <- 1
health[which(dw12$DCh19hlt049 == 3)] <- 2
health[which(dw12$DCh19hlt049 == 4 | dw12$DCh19hlt049 == 5)] <- 3

pskc12$health <- health # (1=low, 2=middle, 3=high)


## CBCL - caregiver rating dehavior problem 
CBCL <- ifelse(dw12$ICh19cbt001 >= 64 | dw12$ICh19cbt002 >= 64 | dw12$ICh19cbt003 >= 64, 2, 
               
               ifelse(!is.na(dw12$ICh19cbt001 + dw12$ICh19cbt002 + dw12$ICh19cbt003), 1, NA))

pskc12$CBCL <- CBCL # (1=no, 2=yes)


## family composition
family <- ifelse(dw12$DMt19dmg029 == 1 & dw12$DFt19dmg029 == 1, 1, 
                 
                 ifelse(!is.na(dw12$DMt19dmg029 + dw12$DFt19dmg029), 2, NA))

pskc12$family <- family # (1=living with both parents, 2=the others)


## household income
income <- cut(dw12$DHu19ses006, breaks = c(0, 291.1000, 566, Inf), 
              
              right = F, labels = c(1, 2, 3))

pskc12$income <- as.numeric(income) # (1=low, 2=middle, 3=high)


## employment status of parents
empstatus <- rep(2, dim(dw12)[1])
empstatus[which(dw12$DMt19jcg003 < 4 & dw12$FFt19jcg003 < 4)] <- 3
empstatus[which(dw12$DMt19jcg003 == 4 & dw12$FFt19jcg003 == 4)] <- 1
empstatus[which(is.na(dw12$DMt19jcg003 + dw12$FFt19jcg003))] <- NA

pskc12$empstatus <- empstatus # (1=none, 2=either, 3=both)



########## Combining data sets ##########

df1 <- right_join(pskc1, pskc10, by = "N_ID") %>% na.omit
df2 <- right_join(pskc1, pskc12, by = "N_ID") %>% na.omit

id <- intersect(df1$N_ID, df2$N_ID)

iadata_all <- rbind(df1[df1$N_ID %in% id,], df2[df2$N_ID %in% id,])

iadata <- iadata_all %>% arrange(N_ID, year)

write.csv(iadata, "iadata.csv", row.names = F)


########## Setting labels ##########

d <- iadata %>%
  
  var_labels(peridep = "Peripartum depression",
             gender = "Gender", 
             year = "Year",
             mdep = "Mother's depression (current)",
             IA = "IA", 
             smart = "Having Smartphone",
             happy = "Happiness", 
             sleep = "Sleep duration", 
             health = "Health status", 
             CBCL = "Behavior problem (CBCL)",
             family = "Family composition", 
             income = "Household income",
             empstatus = "Employment status of parents")


d$peridep <- set_labels(d$peridep, labels = c("no"=1, "yes"=2))

d$gender <- set_labels(d$gender, labels = c("boy"=1, "girl"=2))

d$year <- set_labels(d$year, labels = c("Wave10 (2017)"=2017, "Wave12 (2019)"=2019))

d$mdep <- set_labels(d$mdep, labels = c("no"=1, "yes"=2))

d$IA <- set_labels(d$IA, labels = c("normal"=1, "Internet addiction"=2))

d$smart <- set_labels(d$smart, labels = c("no"=1, "yes"=2))

d$happy <- set_labels(d$happy, labels = c("happy"=1, "unhappy"=2))

d$sleep <- set_labels(d$sleep, labels = c("<9 hours"=1, "≥9 hours"=2))

d$health <- set_labels(d$health, labels = c("low"=1, "middle"=2, "high"=3))

d$CBCL <- set_labels(d$CBCL, labels = c("no"=1, "yes"=2))

d$family <- set_labels(d$family, labels = c("living with both parents"=1, "the others"=2))

d$income <- set_labels(d$income, labels = c("low"=1, "middle"=2, "high"=3))

d$empstatus <- set_labels(d$empstatus, labels = c("none"=1, "either"=2, "both"=3))

library(DescTools)
for(i in 1:ncol(d)){
  temp <- d[d$year == 2017,]
  if(i %in% c(2,3,5,7,8,9,10,11,12,13,14)){
    print(colnames(d)[i])
    print(PercTable(table(temp[,i], temp[["IA"]]), rfrq = "010"))
  }
}

for(i in 1:ncol(d)){
  temp <- d[d$year == 2019,]
  if(i %in% c(2,3,5,7,8,9,10,11,12,13,14)){
    print(colnames(d)[i])
    print(PercTable(table(temp[,i], temp[["IA"]]), rfrq = "010"))
  }
}

########## Description of variables ##########

chisqtest <- moonBook::mytable(year + IA ~ peridep + mdep + gender + smart + happy + sleep + 
                                 health + CBCL + family + income + empstatus, data = d)

x <- rrtable::mytable2flextable(chisqtest); x
