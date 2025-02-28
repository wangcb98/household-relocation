# Set the directory to the folder this code belongs to
setwd("~/Dropbox/Tomorrow's Cities Risk Work - UCL Team/Chenbo Work/relocation inclination study/code/R/IRM data")
require(tidyverse)
require(dplyr)
require(haven)
require(glmnet)
require(ggplot2)
require(ggpubr)
require(scales)
require(labelled)
require(caret)
require(pROC)
require(ROCR)
require(tree)
require(randomForest)
require(smotefamily)
require(splitTools)
require(boot)
require(readxl)
require(writexl)
require(PRROC)
require(MLmetrics)
require(doParallel)
require(tictoc)

## (1) Prepare the data for input to random forest model
# "IRM4.sav" contains the survey data provided by The Asia Foundation
full_dataset.IRM4 <- read_sav("IRM4.sav")
table(full_dataset.IRM4$R4_B2)
# R4_B2. Where are you living now?
# R4_F9. How was your house classified in the most recent official damage assessment?
# R4_E2_1_ELECTRICITY, R4_E2_2_DRINKING_WATER, R4_E2_3_MEDICAL_FACILITY, R4_E2_4_SCHOOL, R4_E2_5_MOTORABLE_ROAD
table(full_dataset.IRM4$R4_E2_1_ELECTRICITY)
# 1  Very satisfied
# 2  Somewhat satisfied
# 3  Neither satisfied not dissatisfied
# 4  Somewhat dissatisfied
# 5  Very dissatisfied
# 98 Refused
# 99 DK/CS
## low place attachment: households who answered 4 or 5 to any of above

## Housing reconstruction grant
# How much of the NPR 300,000 grant have you received at this point?
table(full_dataset.IRM4$R4_F13)
table(full_dataset.IRM4$R4_F13>99)

## R4_C2_1 to R4_C2_16 except R4_C2_8: To what extent was your xxx (income source) affected by the earthquake?
# value                label
# 1  Completely affected
# 2    Somewhat affected
# 3         Not affected
# 98              Refused
# 99 Don't know/Can't say
table(full_dataset.IRM4$R4_C2_1)
table(full_dataset.IRM4$R4_C2_2)
table(full_dataset.IRM4$R4_C2_3)

## positive unemployment status: households who answered 5 in any of their responses to R4_F14_1 to R4_F14_6

# R4_D22. Do you or anyone else in your household plan to migrate (leave for at least 3 months) in the next 12 months?
table(full_dataset.IRM4$R4_D22)
# 1  Yes (184)
# 2  No (4615)
# 98 Refused
# 99 Don’t know / Can’t say (55)

dataset.IRM4 <- full_dataset.IRM4 %>%
  filter(
    R4_F9 <= 3 & R4_B6 < 98 & R4_A9 < 98 #filter to include only households within known damage state, current housing condition, and income group
  ) %>%
  mutate(
    relocation = if_else( # Do you or anyone else in your household plan to migrat?
      # relocation: households who answered "1" to question D22
      R4_D22 == 1,
      1,0),
    low_PS = if_else(
      R4_E2_1_ELECTRICITY == 4 | R4_E2_1_ELECTRICITY == 5 | 
        R4_E2_2_DRINKING_WATER == 4 | R4_E2_2_DRINKING_WATER == 5 | 
        R4_E2_3_MEDICAL_FACILITY ==4 | R4_E2_3_MEDICAL_FACILITY ==5 | 
        R4_E2_4_SCHOOL ==4 | R4_E2_4_SCHOOL ==5 | 
        R4_E2_5_MOTORABLE_ROAD == 4| R4_E2_5_MOTORABLE_ROAD == 5,
      1, 0),
    gov_fund = if_else( # How much of the NPR 300,000 grant (NRA: national reconstruction authority) have you received at this point?
      R4_F13>99,
      1,0),
    # this definition of unemployment leads to 36.03% of unemployed households
    unemployment = if_else(
      R4_F14_1 == 5 | R4_F14_2 == 5 | R4_F14_3 == 5 | 
        R4_F14_4 == 5 | R4_F14_5 == 5 | R4_F14_6 == 5 |
        R4_F23A == 3 | R4_D7_1 == 4 | R4_D7_2 == 4 |
        R4_D7_3 == 4 | R4_D7_4 == 4,
      1,0),
    # earthquake-induced impacts on livelihood
    # partially + completelyaffected = 45.5% (2208)
    # completely affected = 14.5% (702)
    livelihood_impact = ifelse(
      R4_C2_1 < 3 | R4_C2_2 < 3 | R4_C2_3 < 3 | 
        R4_C2_4 < 3 | R4_C2_5 < 3 | R4_C2_6 < 3 |
        R4_C2_7 < 3 |R4_C2_9 < 3 | R4_C2_10 < 3 | 
        R4_C2_11 < 3 | R4_C2_12 < 3 | R4_C2_13 < 3 | 
        R4_C2_14 < 3 | R4_C2_15 < 3 | R4_C2_16 < 3,
      1,0
    ),
    living_condition =NA,
    income_group = NA,
    damage_level = R4_F9
  )

# Age: R4_A3 (age in integer)
dataset.IRM4$relocation[is.na(dataset.IRM4$relocation)] = 0
dataset.IRM4$livelihood_impact[is.na(dataset.IRM4$livelihood_impact)] = 0
dataset.IRM4$low_PS[is.na(dataset.IRM4$low_PS)] = 0
dataset.IRM4$gov_fund[is.na(dataset.IRM4$gov_fund)] = 0
dataset.IRM4$unemployment[is.na(dataset.IRM4$unemployment)] = 0
dataset.IRM4$income_group[dataset.IRM4$R4_A9 %in% c(1,2,3)] = 1 # low-income < 20,000 Rs/month
dataset.IRM4$income_group[dataset.IRM4$R4_A9 == 4] = 2          # 20,000<= mid-income < 40,000 Rs/month
dataset.IRM4$income_group[dataset.IRM4$R4_A9 == 5] = 3          # high-income >= 40,000 Rs/month
dataset.IRM4$relocation <- as.factor(dataset.IRM4$relocation) # relocation decision
dataset.IRM4$livelihood_impact <- as.factor(dataset.IRM4$livelihood_impact) # livelihood impact
dataset.IRM4$damage_level <- as.factor(dataset.IRM4$damage_level) # residential building damage
dataset.IRM4$R4_HH_Size_Gr <- as.factor(dataset.IRM4$R4_HH_Size_Gr) # household size group
dataset.IRM4$low_PS <- as.factor(dataset.IRM4$low_PS) # place satisfaction
dataset.IRM4$R4_A9 <- as.factor(dataset.IRM4$R4_A9) # household income
dataset.IRM4$income_group <- as.factor(dataset.IRM4$income_group) # converted household income
dataset.IRM4$gov_fund <- as.factor(dataset.IRM4$gov_fund) # government fund
dataset.IRM4$unemployment <- as.factor(dataset.IRM4$unemployment)
dataset.IRM4$R4_AgeGr<- as.factor(dataset.IRM4$R4_AgeGr) # age group
dataset.IRM4$R4_A2<- as.factor(dataset.IRM4$R4_A2) # gender
table(dataset.IRM4$relocation)
table(dataset.IRM4$livelihood_impact)
table(dataset.IRM4$damage_level)
table(dataset.IRM4$R4_A3)
table(dataset.IRM4$R4_HH_Size_Gr)
table(dataset.IRM4$low_PS)
table(dataset.IRM4$gov_fund)
table(dataset.IRM4$R4_A9)
table(dataset.IRM4$income_group)
table(dataset.IRM4$R4_A2)

saveRDS(dataset.IRM4, file = "dataset_IRM4.Rds")

# (2) Predictor Selection

# split data
## 85% for model development 
# (bootstrapped sampling 70% train, 15% validation)
# 15% test (set aside for reporting the model performance)
new_split_data <- F
seed_split <- 444740
if (new_split_data){
  set.seed(seed_split)
  dt <- readRDS(file = "dataset_IRM4.Rds") #3300 samples: 0 -- 3146  1 -- 154
  idx_reloc <- which(dt$relocation == 1, arr.ind = TRUE)
  idx_noreloc <- which(dt$relocation == 0, arr.ind = TRUE)
  imbalance_ratio <- length(idx_noreloc)/length(idx_reloc)
  idx_test <- sample(idx_reloc,0.15*length(idx_reloc),replace = FALSE)
  idx_test <- append(idx_test,sample(idx_noreloc,0.15*length(idx_noreloc),replace = FALSE))
  dt.test <- dt[idx_test,]
  dt.train_val <- dt[-idx_test,]
  idx_reloc_tv <- which(dt.train_val$relocation == 1, arr.ind = TRUE) # indices w.r.t. the 85% train-validation sets
  idx_noreloc_tv <- which(dt.train_val$relocation == 0, arr.ind = TRUE) # indices w.r.t. the 85% train-validation sets
  save(idx_reloc,idx_noreloc,idx_test,
       dt.test,dt.train_val,
       idx_reloc_tv,idx_noreloc_tv,imbalance_ratio,
       file = paste("data_split_",seed_split,".RData",sep = ""))
}
if(!new_split_data){
  load(paste("data_split_",seed_split,".RData",sep = ""))
  paste0("splitted data are loaded!")
  paste0("sanity check, number of overlapped samples between test and train+val (should be zero): ",
         length(intersect(dt.test$Res_ID,dt.train_val$Res_ID)))
}

## loosely tune the hyperparams (only Ntree and Mtry) 
Ntree <- c(100,200,300,400,500,600,700,800,900,1000)
Mtry <- 1:8
Sampsize <- Sampsize <- c(10,50,seq(100,1000,100))
mean.auc.val = array(0,dim=c(length(Mtry),length(Ntree),length(Sampsize)))
mean.auc.train = array(0,dim=c(length(Mtry),length(Ntree),length(Sampsize)))
set.seed(1989)
for(idx_mtry in 1:length(Mtry)){
  for(idx_tree in 1:length(Ntree)){
    for(idx_sampsize in 1:length(Sampsize)){
      n_sim <- 10
      auc.train = rep(0,n_sim) # auc and f1 score (best possible) in 100 simulations
      auc.val = rep(0,n_sim)
      f1.train = rep(0,n_sim)
      f1.val = rep(0,n_sim)
      for (idx in 1:n_sim){
        # use stratified sampling to divide the train-validation set into 70:15 (14:3)
        idx_val <- sample(idx_reloc_tv,(3/14)*length(idx_reloc_tv),replace = FALSE)
        idx_val <- append(idx_val,sample(idx_noreloc_tv,(3/14)*length(idx_noreloc_tv),replace = FALSE))
        dt.val <- dt.train_val[idx_val,]
        dt.train <- dt.train_val[-idx_val,]
        # use bootstrapping to obtain a balanced train set
        boot.rato <- imbalance_ratio-1
        idx_reloc_train <- which(dt.train$relocation == 1, arr.ind = TRUE) # indices w.r.t. the 70% train set
        idx_boot <- sample(idx_reloc_train,length(idx_reloc_train)*boot.rato,replace = TRUE)
        dt.train.boot <- rbind(dt.train,dt.train[idx_boot,])
        # use bootstrapping to obtain a balanced validation set
        idx_reloc_val <- which(dt.val$relocation == 1, arr.ind = TRUE)
        idx_val_boot <- sample(idx_reloc_val,length(idx_reloc_val)*boot.rato,replace = TRUE)
        dt.val.boot <- rbind(dt.val,dt.val[idx_val_boot,])
        # generate random noises (binary and integer)
        dt.train.boot$noise <- as.factor(if_else(runif(nrow(dt.train.boot))>0.5,1,0)) # binary
        dt.train.boot$noise2 <- sample(1:100,nrow(dt.train.boot), replace=TRUE) # integer from 1 to 100
        dt.val.boot$noise <- as.factor(if_else(runif(nrow(dt.val.boot))>0.5,1,0))
        dt.val.boot$noise2 <- sample(1:100,nrow(dt.val.boot), replace=TRUE)
        mod.rf <- randomForest(
          relocation~livelihood_impact+damage_level+R4_A3+R4_HOUSEHOLD_NO+low_PS+gov_fund+income_group+R4_A2+noise+noise2,
          data=dt.train.boot,
          mtry = Mtry[idx_mtry],
          ntree = Ntree[idx_tree],
          sampsize = Sampsize[idx_sampsize],
          importance=F, 
          replace=TRUE)
        pred.train = predict(mod.rf, dt.train.boot, type = "prob")[,2]
        auc.train[idx] = auc(dt.train.boot$relocation, pred.train, levels = c(0,1), direction = "<")
        dt.train.boot$pred.train=pred.train
        rf.roc.train <- roc(relocation ~ pred.train, data = dt.train.boot, levels = c(0,1), direction = "<")
        pred.val=predict(mod.rf, dt.val.boot, type = "prob")[,2]
        auc.val[idx] = auc(dt.val.boot$relocation, pred.val, levels = c(0,1), direction = "<")
        dt.val.boot$pred.val=pred.val
        rf.roc.val <- roc(relocation ~ pred.val, data = dt.val.boot, levels = c(0,1), direction = "<")
      }
      mean.auc.val[idx_mtry,idx_tree,idx_sampsize] <- mean(auc.val)
      mean.auc.train[idx_mtry,idx_tree,idx_sampsize] <- mean(auc.train)
    }}}
paste("For optimal AUC: mtry = ",Mtry[which(mean.auc.val == max(mean.auc.val),arr.ind = TRUE)[1]],
      ", ntree = ",Ntree[which(mean.auc.val == max(mean.auc.val),arr.ind = TRUE)[2]],
      ", sampsize = ",Sampsize[which(mean.auc.val == max(mean.auc.val),arr.ind = TRUE)[3]])


# compute modified gini importance index
dt <- readRDS(file = "dataset_IRM4.Rds") #3300 samples: 0 -- 3146  1 -- 154
dt$damage <-if_else(dt$R4_B1==4,0,1) # not damaged = 0, damaged = 1
dt$damage <- as.factor(dt$damage)
# run the calculations in parallel
tic()
parallel::detectCores()
n.cores <- parallel::detectCores() - 1 # leave one/two cores available for other tasks
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK")
#check cluster definition (optional)
print(my.cluster)
## socket cluster with 8 nodes on host 'localhost'
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered
foreach::getDoParRegistered()
#check how many workers are available
foreach::getDoParWorkers()

set.seed(1989)
n_sim <- 1000
Result_Each_Parfor<-foreach(idx = 1:n_sim,.combine='c', .packages=c('haven','glmnet','labelled','tidyverse','randomForest','pROC','ROCR','PRROC','caret','boot','dplyr')) %dopar% {
  # use stratified sampling to divide the train-validation set into 70:15 (14:3)
  idx_val <- sample(idx_reloc_tv,(3/14)*length(idx_reloc_tv),replace = FALSE)
  idx_val <- append(idx_val,sample(idx_noreloc_tv,(3/14)*length(idx_noreloc_tv),replace = FALSE))
  dt.val <- dt.train_val[idx_val,]
  dt.train <- dt.train_val[-idx_val,]
  # use upsampling to obtain more samples with "no damage"
  dt.train$damage <- if_else(dt.train$R4_B1==4,0,1) # not damaged = 0, damaged = 1
  dt.train$damage <- as.factor(dt.train$damage)
  up.ratio <- 20
  idx_dl_no_damage <- which(dt.train$damage == 0, arr.ind = TRUE)
  idx_dl_up <- sample(idx_dl_no_damage,length(idx_dl_no_damage)*up.ratio,replace = TRUE)
  dt.train <- rbind(dt.train,dt.train[idx_dl_up,])
  # use bootstrapping to obtain a balanced train set
  imbalance_ratio <- table(dt.train$relocation)[1]/table(dt.train$relocation)[2]
  boot.rato <- imbalance_ratio-1
  idx_reloc_train <- which(dt.train$relocation == 1, arr.ind = TRUE) # indices w.r.t. the 70% train set
  idx_boot <- sample(idx_reloc_train,length(idx_reloc_train)*boot.rato,replace = TRUE)
  dt.train.boot <- rbind(dt.train,dt.train[idx_boot,])
  # use bootstrapping to obtain a balanced train and validation set
  idx_tv_boot <- sample(idx_reloc_tv,length(idx_reloc_tv)*boot.rato,replace = TRUE)
  dt.tv.boot <- rbind(dt.train_val,dt.train_val[idx_tv_boot,])
  # generate random noises (binary and integer)
  dt.train.boot$noise <- as.factor(if_else(runif(nrow(dt.train.boot))>0.5,1,0)) # binary
  dt.train.boot$noise_uniform <- runif(nrow(dt.train.boot)) # unifrom dist. 0~1
  dt.train.boot$R4_A3 <- as.integer(dt.train.boot$R4_A3)
  dt.train.boot$R4_HOUSEHOLD_NO <- as.integer(dt.train.boot$R4_HOUSEHOLD_NO)
  mod.rf <- randomForest(
    relocation~livelihood_impact+damage+R4_A3+R4_HOUSEHOLD_NO+low_PS+gov_fund+income_group+R4_A2+noise_uniform,
    data=dt.train.boot,
    mtry=1,
    ntree=200, # Pre-tuned hyperparams
    replace=TRUE,
    importance=TRUE)
  list(data.frame(importance(mod.rf))$MeanDecreaseGini)
}

#stop the cluster when finished
parallel::stopCluster(cl = my.cluster)
toc()

gini_importance <- data.frame(
  matrix(unlist(Result_Each_Parfor), nrow = n_sim, ncol = 9,byrow=T))
lapply(gini_importance,mean)
lapply(gini_importance-gini_importance$X9,mean)

write_xlsx(gini_importance,'gini_importance_tuned_uniform_noise_1000sims.xlsx')

# Plot gini importance indices for each predictor
gini_importance <- read_xlsx('gini_importance_tuned_uniform_noise_1000sims.xlsx')
oldnames = c("X1","X2","X3","X4","X5","X6","X7","X8","X9")
newnames = c("livelihood impact","damage level","age","household size","low PS","government fund","income group","gender","uniform noise")

dfplot <- gini_importance %>% rename_at(vars(oldnames), ~ newnames)
dfplot_bias_corrected <- dfplot %>% mutate(
  `livelihood impact` = `livelihood impact`-`uniform noise`,
  `damage level` = `damage level`-`uniform noise`,
  `age` = `age`-`uniform noise`,
  `household size` = `household size`-`uniform noise`,
  `place satisfaction` = `low PS`-`uniform noise`,
  `government fund` = `government fund`-`uniform noise`,
  `income group` = `income group`-`uniform noise`,
  `gender` = `gender`-`uniform noise`,
)

plot1<-ggplot(dfplot_bias_corrected, aes(x=`livelihood impact`)) +
  geom_histogram(
    color="darkblue",
    fill="lightblue",
    binwidth = 5,
    center = 100,
    linetype="solid") +
  scale_x_continuous(breaks=c(-20,0,50,100,150,200),limits=c(-20,200)) +
  ylim(0,320) +
  geom_vline(
    aes(xintercept=mean(`livelihood impact`)),
    color="darkred", linetype="dashed", linewidth=1) +
  theme(axis.text.x = element_text(face=NULL, color="black",
                                   size = 14, angle=0),
        axis.text.y = element_text(face=NULL, color="black",
                                   size = 14, angle=0),
        axis.title = element_text(size = 14)) +
  labs(x = 'bias-corrected GI')

plot1 <- annotate_figure(
  plot1,top = text_grob(
    "livelihood impact", color = "darkred", face = "bold", size = 14))

plot2<-ggplot(dfplot_bias_corrected, aes(x=`damage level`)) +
  geom_histogram(
    color="darkblue", 
    fill="lightblue",
    binwidth = 5,
    center = 100,
    linetype="solid") +
  scale_x_continuous(breaks=c(-20,0,50,100,150,200),limits=c(-20,200)) +
  ylim(0,320) + 
  geom_vline(
    aes(xintercept=mean(`damage level`)),
    color="darkred", linetype="dashed", linewidth=1)+
  theme(axis.text.x = element_text(face=NULL, color="black", 
                                   size = 14, angle=0),
        axis.text.y = element_text(face=NULL, color="black", 
                                   size = 14, angle=0),
        axis.title = element_text(size = 14)) + 
  labs(x = 'bias-corrected GI')

plot2 <- annotate_figure(
  plot2,top = text_grob(
    "residential damage", color = "darkred", face = "bold", size = 14))

plot3<-ggplot(dfplot_bias_corrected, aes(x=age)) +
  geom_histogram(
    color="darkblue", 
    fill="lightblue",
    binwidth = 5,
    center = 100,
    linetype="solid") +
  scale_x_continuous(breaks=c(-20,0,50,100,150,200),limits=c(-20,200)) +
  ylim(0,320) + 
  geom_vline(
    aes(xintercept=mean(age)),
    color="darkred", linetype="dashed", linewidth=1)+
  theme(axis.text.x = element_text(face=NULL, color="black", 
                                   size = 14, angle=0),
        axis.text.y = element_text(face=NULL, color="black", 
                                   size = 14, angle=0),
        axis.title = element_text(size = 14)) + 
  labs(x = 'bias-corrected GI')

plot3 <- annotate_figure(
  plot3,top = text_grob(
    "age of the household head", color = "darkred", face = "bold", size = 14))

plot4<-ggplot(dfplot_bias_corrected, aes(x=`household size`)) +
  geom_histogram(
    color="darkblue", 
    fill="lightblue",
    binwidth = 5,
    center = 100,
    linetype="solid") +
  scale_x_continuous(breaks=c(-20,0,50,100,150,200),limits=c(-20,200)) +
  ylim(0,320) + 
  geom_vline(
    aes(xintercept=mean(`household size`)),
    color="darkred", linetype="dashed", linewidth=1)+
  theme(axis.text.x = element_text(face=NULL, color="black", 
                                   size = 14, angle=0),
        axis.text.y = element_text(face=NULL, color="black", 
                                   size = 14, angle=0),
        axis.title = element_text(size = 14)) + 
  labs(x = 'bias-corrected GI') 
plot4 <- annotate_figure(
  plot4,top = text_grob(
    "household size", color = "darkred", face = "bold", size = 14))

plot5<-ggplot(dfplot_bias_corrected, aes(x=`place satisfaction`)) +
  geom_histogram(
    color="darkblue", 
    fill="lightblue",
    binwidth = 5,
    center = 100,
    linetype="solid") +
  scale_x_continuous(breaks=c(-20,0,50,100,150,200),limits=c(-20,200)) +
  ylim(0,320) + 
  geom_vline(
    aes(xintercept=mean(`place satisfaction`)),
    color="darkred", linetype="dashed", linewidth=1)+
  theme(axis.text.x = element_text(face=NULL, color="black", 
                                   size = 14, angle=0),
        axis.text.y = element_text(face=NULL, color="black", 
                                   size = 14, angle=0),
        axis.title = element_text(size = 14)) + 
  labs(x = 'bias-corrected GI')
plot5 <- annotate_figure(
  plot5,top = text_grob(
    "place satisfaction", color = "darkred", face = "bold", size = 14))

plot6<-ggplot(dfplot_bias_corrected, aes(x=`government fund`)) +
  geom_histogram(
    color="darkblue", 
    fill="lightblue",
    binwidth = 5,
    center = 100,
    linetype="solid") +
  scale_x_continuous(breaks=c(-20,0,50,100,150,200),limits=c(-20,200)) +
  ylim(0,320) + 
  geom_vline(
    aes(xintercept=mean(`government fund`)),
    color="darkred", linetype="dashed", linewidth=1)+
  theme(axis.text.x = element_text(face=NULL, color="black", 
                                   size = 14, angle=0),
        axis.text.y = element_text(face=NULL, color="black", 
                                   size = 14, angle=0),
        axis.title = element_text(size = 14)) + 
  labs(x = 'bias-corrected GI')
plot6 <- annotate_figure(
  plot6,top = text_grob(
    "access to government fund", color = "darkred", face = "bold", size = 14))

plot7<-ggplot(dfplot_bias_corrected, aes(x=`income group`)) +
  geom_histogram(
    color="darkblue", 
    fill="lightblue",
    binwidth = 5,
    center = 100,
    linetype="solid") +
  scale_x_continuous(breaks=c(-20,0,50,100,150,200),limits=c(-20,200)) +
  ylim(0,320) + 
  geom_vline(
    aes(xintercept=mean(`income group`)),
    color="darkred", linetype="dashed", linewidth=1)+
  theme(axis.text.x = element_text(face=NULL, color="black", 
                                   size = 14, angle=0),
        axis.text.y = element_text(face=NULL, color="black", 
                                   size = 14, angle=0),
        axis.title = element_text(size = 14)) + 
  labs(x = 'bias-corrected GI')
plot7 <- annotate_figure(
  plot7,top = text_grob(
    "household income group", color = "darkred", face = "bold", size = 14))

plot8<-ggplot(dfplot_bias_corrected, aes(x=gender)) +
  geom_histogram(
    color="darkblue", 
    fill="lightblue",
    binwidth = 5,
    center = 100,
    linetype="solid") +
  scale_x_continuous(breaks=c(-20,0,50,100,150,200),limits=c(-20,200))+
  ylim(0,320) + 
  geom_vline(
    aes(xintercept=mean(gender)),
    color="darkred", linetype="dashed", linewidth=1)+
  theme(axis.text.x = element_text(face=NULL, color="black", 
                                   size = 14, angle=0),
        axis.text.y = element_text(face=NULL, color="black", 
                                   size = 14, angle=0),
        axis.title = element_text(size = 14)) + 
  labs(x = 'bias-corrected GI')
plot8 <- annotate_figure(
  plot8,top = text_grob(
    "gender of the household head", color = "darkred", face = "bold", size = 14))

ggarrange(plot2, plot6, plot1, plot5, plot7, plot8, plot3, plot4,
          ncol = 2, nrow = 4)


# Test the model on the test data using the tuned hyperparameters
nsim <- 1000
auc.test <- rep(0,nsim)
auc.test.imbal <- rep(0,nsim)
auc.tv <- rep(0,nsim)
set.seed(1989)
# Gilles Marcou and Didier Rognan (2007) consider AUC of 0.6 to 0.7 as fair
# https://pubs.acs.org/doi/full/10.1021/ci600342e
dt.test$damage <- if_else(dt.test$R4_B1==4,0,1) # not damaged = 0, damaged = 1
dt.test$damage <- as.factor(dt.test$damage)
dt.train_val$damage <- if_else(dt.train_val$R4_B1==4,0,1) # not damaged = 0, damaged = 1
dt.train_val$damage <- as.factor(dt.train_val$damage)
# upsample to increase the samples associated with "no damage" in train_val and test sets
up.ratio <- 20
idx_dl_no_damage <- which(dt.test$damage == 0, arr.ind = TRUE)
idx_dl_up <- sample(idx_dl_no_damage,length(idx_dl_no_damage)*up.ratio,replace = TRUE)
dt.test <- rbind(dt.test,dt.test[idx_dl_up,])
idx_dl_no_damage.tv <- which(dt.train_val$damage == 0, arr.ind = TRUE)
idx_dl_up.tv <- sample(idx_dl_no_damage.tv,length(idx_dl_no_damage.tv)*up.ratio,replace = TRUE)
dt.train_val <- rbind(dt.train_val,dt.train_val[idx_dl_up.tv,])
# use bootstrapping to obtain balanced test set
boot.rato<-table(dt.test$relocation)[1]/table(dt.test$relocation)[2]-1
idx_reloc_test <- which(dt.test$relocation == 1, arr.ind = TRUE)
idx_test_boot <- sample(idx_reloc_test,length(idx_reloc_test)*boot.rato,replace = TRUE)
dt.test.boot <- rbind(dt.test,dt.test[idx_test_boot,])
# use bootstrapping to obtain balanced train_val set
boot.rato<-table(dt.train_val$relocation)[1]/table(dt.train_val$relocation)[2]-1
idx_reloc_tv <- which(dt.train_val$relocation == 1, arr.ind = TRUE)
idx_tv_boot <- sample(idx_reloc_tv,length(idx_reloc_tv)*boot.rato,replace = TRUE)
dt.tv.boot <- rbind(dt.train_val,dt.train_val[idx_tv_boot,])
# append noise (not really need it?)
dt.test.boot$noise <- as.factor(if_else(runif(nrow(dt.test.boot))>0.5,1,0))
dt.test.boot$noise_uniform <- runif(nrow(dt.test.boot)) # unifrom dist. 0~1
dt.tv.boot$noise <- as.factor(if_else(runif(nrow(dt.tv.boot))>0.5,1,0))
dt.tv.boot$noise_uniform <- runif(nrow(dt.tv.boot)) # unifrom dist. 0~1
dt.test$R4_A3 <- as.integer(dt.test$R4_A3)
dt.test$R4_HOUSEHOLD_NO <- as.integer(dt.test$R4_HOUSEHOLD_NO)
dt.test.boot$R4_A3 <- as.integer(dt.test.boot$R4_A3)
dt.test.boot$R4_HOUSEHOLD_NO <- as.integer(dt.test.boot$R4_HOUSEHOLD_NO)
dt.tv.boot$R4_A3 <- as.integer(dt.tv.boot$R4_A3)
dt.tv.boot$R4_HOUSEHOLD_NO <- as.integer(dt.tv.boot$R4_HOUSEHOLD_NO)
# sanity check: are you sure the test and train_val data are not overlapped?
intersect(dt.tv.boot$Res_ID,dt.test.boot$Res_ID)

seeds <- sample(1000000,nsim,replace = F) # seeds for fitting RF models
# whether to run in parallel
tic()
parallel::detectCores()
n.cores <- parallel::detectCores() - 1 # leave one/two cores available for other tasks
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK")
#check cluster definition (optional)
print(my.cluster)
## socket cluster with 8 nodes on host 'localhost'
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered
foreach::getDoParRegistered()
#check how many workers are available
foreach::getDoParWorkers()
CW.AUC.RESULTS<-foreach(id_seed = 1:nsim,
                        .combine='c', 
                        .packages=c('haven','glmnet','labelled','tidyverse',
                                    'randomForest','pROC','ROCR','PRROC',
                                    'caret','boot','dplyr')) %dopar% {
                                      set.seed(seeds[id_seed])
                                      model.fit <- randomForest(
                                        # relocation~livelihood_impact+damage+R4_A3+R4_HOUSEHOLD_NO+gov_fund+income_group, #123467
                                        # relocation~damage+R4_A3+R4_HOUSEHOLD_NO+gov_fund+income_group+R4_A2, #234678
                                        # relocation~damage+R4_A3+R4_HOUSEHOLD_NO+gov_fund+income_group, #23467
                                        relocation~livelihood_impact+damage+R4_A3+R4_HOUSEHOLD_NO+low_PS+gov_fund+income_group+R4_A2, #12345678
                                        data=dt.tv.boot,
                                        mtry=1, ntree=200,
                                        importance=F,
                                        replace=TRUE)
                                      pred.test=predict(model.fit, dt.test.boot, type = "prob")[,2] # probability predicted by the model of each example being 1
                                      pred.test.imbal=predict(model.fit, dt.test, type = "prob")[,2] # probability predicted by the model of each example being 1
                                      pred.tv=predict(model.fit, dt.tv.boot, type = "prob")[,2] # probability predicted by the model of each example being 1
                                      list(auc(dt.test.boot$relocation, pred.test, levels = c(0,1), direction = "<")[1],
                                           auc(dt.test$relocation, pred.test.imbal, levels = c(0,1), direction = "<")[1],
                                           auc(dt.tv.boot$relocation, pred.tv, levels = c(0,1), direction = "<")[1])
                                    }
auc.test<-unlist(CW.AUC.RESULTS[seq(1,3*nsim,by=3)])
auc.test.imbal<-unlist(CW.AUC.RESULTS[seq(2,3*nsim,by=3)])
auc.tv<-unlist(CW.AUC.RESULTS[seq(3,3*nsim,by=3)])
mean(auc.test)
mean(auc.test.imbal)

data.frame(auc.test.imbal) %>% 
  ggplot(aes(x=auc.test.imbal)) +
  geom_histogram(
    color="darkblue", 
    fill="lightblue",
    alpha = 0.5,
    linewidth = 0.5,
    binwidth = 0.001,
    center = 0.770,
    linetype="solid") +
  xlim(0.69,0.74) +
  # ylim(0,300) + 
  # scale_x_continuous(breaks=c(0.74,0.75,0.76,0.77,0.78),limits=c(0.74,0.78)) +
  scale_x_continuous(breaks=c(0.69,0.70,0.71,0.72,0.73,0.74),limits=c(0.69,0.74)) +
  geom_vline(
    aes(xintercept=mean(auc.test.imbal)),
    color="darkred", linetype="dashed", linewidth=1) +
  theme(
    axis.text.x = element_text(face=NULL, color="black", 
                               size = 14, angle=0),
    axis.text.y = element_text(face=NULL, color="black", 
                               size = 14, angle=0),
    axis.title = element_text(size = 14)) +
  labs(x = 'AUC of ROC curve (on test data)')

#stop the cluster when finished
parallel::stopCluster(cl = my.cluster)
toc()
save.image("use_rf_model_1000sims_12345678.RData")

# rebalance the entire dataset for fitting the final model
set.seed(1989)
dt <- readRDS(file = "dataset_IRM4.Rds")
dt$damage <- if_else(dt$R4_B1==4,0,1) # not damaged = 0, damaged = 1
dt$damage <- as.factor(dt$damage)
# upsample to increase the samples associated with "no damage" in train_val and test sets
up.ratio <- 20
idx_dl_no_damage <- which(dt$damage == 0, arr.ind = TRUE)
idx_dl_up <- sample(idx_dl_no_damage,length(idx_dl_no_damage)*up.ratio,replace = TRUE)
dt <- rbind(dt,dt[idx_dl_up,])
boot.rato<-table(dt$relocation)[1]/table(dt$relocation)[2]-1
idx_reloc <- which(dt$relocation == 1, arr.ind = TRUE)
idx_boot <- sample(idx_reloc,length(idx_reloc)*boot.rato,replace = TRUE)
dt.boot <- rbind(dt,dt[idx_boot,])
dt.boot$R4_A3 <- as.integer(dt.boot$R4_A3)
dt.boot$R4_HOUSEHOLD_NO <- as.integer(dt.boot$R4_HOUSEHOLD_NO)

# fit four models
mod.1 <- randomForest(
  relocation~livelihood_impact+damage+R4_A3+R4_HOUSEHOLD_NO+low_PS+gov_fund+income_group+R4_A2,
  data=dt.boot,
  mtry = 1,
  ntree = 200,
  importance=F, 
  replace=TRUE)
# OOB estimate of  error rate: 25.35%
# Confusion matrix:
#   0    1 class.error
# 0 2801 1866   0.3998286
# 1  500 4167   0.1071352

mod.2 <- randomForest(
  relocation~livelihood_impact+damage+R4_A3+R4_HOUSEHOLD_NO+gov_fund+income_group,
  data=dt.boot,
  mtry = 1,
  ntree = 600,
  importance=F, 
  replace=TRUE)
# Confusion matrix:
#   0    1 class.error
# 0 2657 2010   0.4306835
# 1  511 4156   0.1094922
# OOB estimate of  error rate: 27.01%

mod.3 <- randomForest(
  relocation~damage+R4_A3+R4_HOUSEHOLD_NO+gov_fund+income_group+R4_A2,
  data=dt.boot,
  mtry = 1,
  ntree = 700,
  importance=F, 
  replace=TRUE)
# Confusion matrix:
#   0    1 class.error
# 0 2505 2162  0.46325262
# 1  329 4338  0.07049496
#OOB estimate of  error rate: 26.69%

mod.4 <- randomForest(
  relocation~damage+R4_A3+R4_HOUSEHOLD_NO+gov_fund+income_group,
  data=dt.boot,
  mtry = 1,
  ntree = 300,
  importance=F, 
  replace=TRUE)
# Confusion matrix:
#   0    1 class.error
# 0 2302 2365  0.50674952
# 1  235 4432  0.05035355
# OOB estimate of  error rate: 27.86%