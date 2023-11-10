###############################################################################
###                                                                         ###
###                        Simulation of data                               ###
###                                                                         ###
###############################################################################

# @author Katharina Stark
# 04.12.2020


rm(list = ls())

path <- ""
setwd(path)

###############################################################################
### Initial population for simulation study                                 ###
###############################################################################

### Definition of variables ---------------------------------------------------
psex <- c(1,2)                   # 1=male, 2=female
pedu <- c(1,2,3,4,5)             # 1=no, 2=special, 3=HS, 4=RS, 5=Abi
pclass <- c(1,2,3)               # 1=low, 2=middle, 3=upper
psmoker <- c(1,2,3)              # 1=yes, 2=no, 3=no answer
psmodev <- c(1,2,3)              # 1=cigars, 2=cigarettes, 3=ecig/vap
psmofreq <- c(1,2,3)             # 1=irregular, 2=once/several, 3=daily
psmostop <- c(1,2)               # 1=yes, 2=no
psmoever <- c(1,2)               # 1=yes, 2=no 
psmostopr <- c(1,2,3)            # 1=health, 2=costs, 3=other
psubhealth <- c(1,2)             # 1=good, 2=poor


##  (0) - (3) ID, SEX, EDU, CLASS
initPop <- expand.grid(ID = NA, sex = psex, edu = pedu, 
                       class = pclass)
N <- dim(initPop)[1]

##  Seed for random number generator
RNGkind(sample.kind = "Rounding")
set.seed(21214)

##  (4) SMOKER
#   Respondents with "middle" or "high" (_he) education have a lower probability of smoking
#   Sample the probability of a respondent not giving an answer
#   -> Assumption: There are minimal 10% an maximal 15% respondents not giving an answer
(psmoker_na <- runif(1, 0.10, 0.20))
#   Sample the probability of a respondent being a smoker
(psmoker_yes_he <- runif(1, psmoker_na, 0.3))
#   Resulting probability of being a non-smoker
(psmoker_no_he  <- 1-psmoker_na-psmoker_yes_he)
sum(psmoker_na, psmoker_yes_he, psmoker_no_he)

#   Respondents with "no", "special" or "low" (_le) education
# Resulting probability of being a non-smoker
(psmoker_no_le <- runif(1, psmoker_na, 0.3))
#   Sample the probability of a respondent being a smoker
(psmoker_yes_le <- 1-psmoker_na-psmoker_no_le)
sum(psmoker_na, psmoker_yes_le, psmoker_no_le)

initPop$smoker <- factor(ifelse(initPop$edu %in% c(4:5), 
                                sample(psmoker, N, T, c(psmoker_yes_he, psmoker_no_he, psmoker_na)),
                                sample(psmoker, N, T, c(psmoker_yes_le, psmoker_no_le, psmoker_na))),
                         levels=1:3)


##  (5) SMOKING DEVICE
initPop$smodev <- factor(ifelse(initPop$smoker==1, 
                                round(runif(N, 1, 3), 0),
                                NA), 
                         levels=1:3)


##  (6) SMOKING FREQUENCY
initPop$smofreq <- factor(ifelse(initPop$smoker==1, 
                                  round(runif(N, 1, 3), 0), 
                                  NA), 
                           levels=1:3)


##  (7) EVER TRIED STOP SMOKING
#   Respondents with "middle" or "high" education have a higher probability
#   of ever tried to stop smoking
#   Sample the probability of a respondent to ever tried stop smoking
psmostop_yes_he <- runif(1, 0.6, 1)
#   Resulting probability of a respondent to never tried stop smoking
psmostop_no_he  <- 1-psmostop_yes_he
#   Respondents with "no", "special" or "low" education
psmostop_yes_le <- runif(1, 0, 0.6)
#   Resulting probability of a respondent to never have tried stop smoking
psmostop_no_le  <- 1-psmostop_yes_le

initPop$smostop <- factor(ifelse(initPop$smoker==1 & initPop$edu %in% c(4:5), 
                                  sample(psmostop, N, T, c(psmostop_yes_he, psmostop_no_he)),
                                  ifelse(initPop$smoker==1 & initPop$edu %in% c(1:3), 
                                         sample(psmostop, N, T, c(psmostop_yes_le, psmostop_no_le)), 
                                         NA)),
                           levels=1:2)


##  (8) EVER SMOKED
#   Respondents with "middle" or "high" education have a lower probability of having ever smoked
#   Sample the probability of a respondent of having ever smoked
psmoever_yes_he <- runif(1, 0, 0.5)
#   Resulting probability of a respondent of having never smoked
psmoever_no_he  <- 1-psmoever_yes_he
#   Respondents with "no", "special" or "low" education
#   Sample the probability of a respondent of having ever smoked
psmoever_yes_le <- runif(1, 0.5, 1)
#   Resulting probability of a respondent of having never smoked
psmoever_no_le  <- 1-psmoever_yes_le

initPop$smoever <- factor(ifelse(initPop$smoker==2 & initPop$edu %in% c(4:5), 
                                    sample(psmoever, N, T, c(psmoever_yes_he, psmoever_no_he)),
                                    ifelse(initPop$smoker==2 & initPop$edu %in% c(1:3), 
                                           sample(psmoever, N, T, c(psmoever_yes_le, psmoever_no_le)), 
                                           NA)),
                             levels=1:2)


##  (9) REASON TRIED TO STOP/STOPPED SMOKING
initPop$smostopr <- factor(ifelse((initPop$smoker==1 & initPop$smostop == 1) |
                                       (initPop$smoker==2 & initPop$smoever==1),
                                     round(runif(N, 1, 3), 0), NA), 
                              levels=1:3)


##  (10) SUBJECTIVE HEALTH
initPop$subhealth <- factor(ifelse(initPop$smoker %in% c(1:2), 
                                     round(runif(N, 1, 2), 0), NA), 
                              levels=1:2)

# View(initPop)
save(initPop, file = "initPop.RData")


### Sample population from initial population ---------------------------------

N <- 200                # Number of persons
M <- 500                # Number of replications
I <-   5                # Number of interviewer

##  Set seed for random number generation
RNGkind(sample.kind = "Rounding")
set.seed(1308)

simPop <- initPop[sample(1:30, N, replace = TRUE),]
simPop$ID <- 1:N
# View(simPop)


### Marginal distributions ----------------------------------------------------

apply(simPop[, 2:length(simPop[1,])], 2, 
      function(x) cbind(addmargins(table(x, useNA = "ifany")),
                        round(addmargins(prop.table(table(x, useNA = "ifany"))), 3)))

#   Convert factors to strings
simPop[] <- lapply(simPop, as.character)

save(simPop, file = "simPop.RData")



###############################################################################
### Data for Baseline Scenario                                              ###
###############################################################################
# Description: No errors occur

### Load data -----------------------------------------------------------------
rm(list = ls())
load("simPop.RData")
data <- simPop
rm(simPop)

#   Save data of baseline scenario
save(data, file = "baseline_data.RData")



###############################################################################
### Data for erroneous Scenario 1                                           ###
###############################################################################
# Description: Wrong filter programming
#              We assume that question 5 (smodev) has erroneously been omitted,
#              leading to an missing edge (smoker->smodev) and to an additional
#              wrong edge (smoker->smofreq).
#              To simulated the erroneous data, we assign NA's to question 5.


### Load data -----------------------------------------------------------------
rm(list = ls())
load("simPop.RData")
data <- simPop
rm(simPop)
data$smodev <- NA 

### Marginal distributions ----------------------------------------------------
apply(data[, 2:length(data[1,])], 2,
      function(x) cbind((table(x,useNA = "ifany")),
                        round((prop.table(table(x, useNA = "ifany"))), 3)))

#   Save data of scenario 1
save(data, file = "s1_data.RData")



###############################################################################
### Data for erroneous Scenario 2                                           ###
###############################################################################
# Description: Path redundancy
#              Our sample contains only very few smokers and thus the paths 1
#              and 2 are used very little.
#              To simulate this scenario, we sample only few smoker from the
#              initial population


### Load data -----------------------------------------------------------------
rm(list = ls())
load("initPop.RData")
initPop$ID <- 1:length(initPop[,1])

##  Sample from initial population --------------------------------------------
N <- 200                # Number of persons

##   Seed for random number generator
RNGkind(sample.kind = "Rounding")
set.seed(1308)

##   Cases being sampled
sample_idx <- c(sample(initPop$ID[initPop$smoker == 1], N*0.095, T),
                sample(initPop$ID[initPop$smoker == 2], N*0.605, T),
                sample(initPop$ID[initPop$smoker == 3], N*0.300, T))

data <- initPop[sample_idx,]
data$ID <- 1:N
rm(initPop)

# View(data)

### Marginal distributions ----------------------------------------------------
apply(data[, 2:length(data[1,])], 2,
      function(x) cbind((table(x,useNA = "ifany")),
                        round((prop.table(table(x, useNA = "ifany"))), 3)))

#   Convert factors to strings
data[] <- lapply(data, as.character)
#   Save data of scenario 2
save(data, file = "s2_data.RData")
