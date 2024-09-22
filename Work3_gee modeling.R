########## loading packages ##########

library(dplyr)
library(sjlabelled)
library(geepack)
library(MuMIn) 
library(broom)
library(moonBook)
library(flextable)
library(texreg)
# remotes::install_github("sarbearschwartz/texreghelpr")
library(texreghelpr)

setwd("~/Desktop/Yonsei/Semester_2/Probabilistic Graphical Model/Final")

iadata <- read.csv("iadata.csv")

iadata2 <- iadata %>% arrange(N_ID, year)

id <- iadata2$N_ID

y <- iadata2$IA - 1

X <- iadata2 %>% select(-c(N_ID, IA)) %>% transmute_all(., as.factor)

d <- cbind(id, y, X)

colnames(d)[1:2] <- c("N_ID", "IA")


## fitting GEEs with different correlation structures
M1 <- geeglm(IA ~ year + peridep + mdep + gender + smart + 
               happy + sleep + health + CBCL + family + income + empstatus, 
             id = N_ID, data = d, family = binomial(link=logit), 
             corstr = 'independence')

M2 <- geeglm(IA ~ year + peridep + mdep + gender + smart + 
               happy + sleep + health + CBCL + family + income + empstatus, 
             id = N_ID, data = d, family = binomial(link=logit), 
             corstr = 'exchangeable')


MuMIn::QIC(M1, M2, typeR = TRUE)

## Fitting GEEs with important predictors only 
M3 <- geeglm(IA ~ peridep + mdep + smart + CBCL + income + empstatus, 
             id = N_ID, data = d, family = binomial(link=logit), 
             corstr = 'exchangeable')
summary(M3)

########## Making some edits to the gee_stepper function in the "pstools" package ##########

gee_stepper <- function(fit, upper) {
  
  preds     <- all.vars(as.list(upper)[[3]])
  preds_out <- preds
  preds_in  <- character(0)
  
  fit0 <- stats::update(fit, formula = . ~ 1)
  
  while(TRUE) {
    fits <-
      c(list(fit0),
        lapply(preds_out, function(p) {stats::update(fit0, formula = stats::as.formula(paste(". ~ . +", p))) }),
        lapply(preds_in,  function(p) {stats::update(fit0, formula = stats::as.formula(paste(". ~ . -", p))) })
      )
    
    minqic <- which.min(sapply(fits, function(x) {MuMIn::QIC(x, typeR = TRUE)}))
    fit0 <- fits[[minqic]]
    
    status <-
      dplyr::data_frame(preds = sapply(fits, function(f) {as.character(stats::formula(f))[3]}),
                        qic   = sapply(fits, function(x) {MuMIn::QIC(x, typeR = TRUE)}),
                        pick  = seq_along(fits) == minqic)
    
    preds_in  <- all.vars(as.list(stats::formula(fit0))[[3]])
    preds_out <- dplyr::setdiff(preds, preds_in)
    
    print(status)
    
    if (minqic == 1) {
      break
    }
  }
  fit0
}


sapply(fits, function(f) {as.character(stats::formula(f))[3]})


########## Variable selection ##########

# Option 1) Stepwise selection
stepwise <- gee_stepper(M2, upper = formula(M2))
stepwise$coefficients

# Option 2) Comparing all the subsets of variables (accurate, but computaionally intensive)
# M2 <- update(M2, na.action = 'na.fail')
# subsets <- dredge(M2, rank = "QIC", typeR = TRUE)

optM <- geeglm(IA ~ year + peridep + mdep + gender + sleep + CBCL + empstatus, 
               id = N_ID, data = d, 
               family = binomial, corstr = 'exchangeable')


########## Adjusted odds ratios for the full and the optimal models ##########

summary(M2) # full model

tb_full <- broom::tidy(M2)

tb_full_or <- moonBook::extractOR(M2); tb_full_or

result <- cbind(tb_full, tb_full_or)

f <- function(x) {paste0(x["OR"], " [", x["lcl"], ",",  x["ucl"], "]")}

result["OR [95% CI]"] <- apply(tb_full_or, 1, f)

tb <- result[c("term", "estimate", "std.error", "statistic", "p.value", "OR [95% CI]")]
names(tb) <- c("Variable", "Estimate", "Std. error", "Wald", "p-value", "OR [95% CI]")

x <- tb %>% flextable; x



summary(optM) # optimal model

tb_reduced <- broom::tidy(optM)

tb_reduced_or <- moonBook::extractOR(optM); tb_reduced_or

result <- cbind(tb_reduced, tb_reduced_or)

f <- function(x) {paste0(x["OR"], " [", x["lcl"], ",",  x["ucl"], "]")}

result["OR [95% CI]"] <- apply(tb_reduced_or, 1, f)

tb <- result[c("term", "estimate", "std.error", "statistic", "p.value", "OR [95% CI]")]
names(tb) <- c("Variable", "Estimate", "Std. error", "Wald", "p-value", "OR [95% CI]")

x <- tb %>% flextable; x
