# %% [code]


# %% [code]
library(plyr)
library(tidyverse)
library(lightgbm)
library(MLmetrics)
library(lubridate)
#block scientific notation
options(scipen = 99)

# %% [code]
train_iden <- read_csv("../input/train_identity.csv")
train_trans <- read_csv("../input/train_transaction.csv")
test_iden <- read_csv("../input/test_identity.csv")
test_trans <- read_csv("../input/test_transaction.csv")

# %% [code]
y <- train_trans$isFraud 
train_trans$isFraud <- NULL
train <- train_trans %>% left_join(train_iden)
test <- test_trans %>% left_join(test_iden)

rm(train_iden,train_trans,test_iden,test_trans) ; invisible(gc())

# %% [code]
#EDA, remove coorelated features (I think this came from PCA in a python module, forgot which one)
drop_col <- c('V300','V309','V111','C3','V124','V106','V125','V315','V134','V102','V123','V316','V113',
              'V136','V305','V110','V299','V289','V286','V318','V103','V304','V116','V29','V284','V293',
              'V137','V295','V301','V104','V311','V115','V109','V119','V321','V114','V133','V122','V319',
              'V105','V112','V118','V117','V121','V108','V135','V320','V303','V297','V120')

# %% [code]
# using single hold-out validation (20%)
tr_idx <- which(train$TransactionDT < quantile(train$TransactionDT,0.8))
train[,drop_col] <- NULL
test[,drop_col] <- NULL

# %% [code]
#############################################################################################################
# create tem dataframe for both train and test data and engineer time of day feature
tem <- train %>% bind_rows(test) %>%
  mutate(hr = floor( (TransactionDT / 3600) %% 24 ),
         weekday = floor( (TransactionDT / 3600 / 24) %% 7)
        ) %>%
  select(-TransactionID,-TransactionDT)

# %% [code]


# %% [code]
##############################################################################################################
# FE part1: # Latest browser
fe_part1 <- tem[, "id_31"]
fe_part1$latest_browser = 0

new_browsers <- c("samsung browser 7.0", "opera 53.0", "mobile safari 10.0", "google search application 49.0",
                 "firefox 60.0", "edge 17.0", "chrome 69.0", "chrome 67.0", "chrome 63.0", "chrome 63.0", 
                 "chrome 64.0", "chrome 64.0 for android", "chrome 64.0 for ios", "chrome 65.0", "chrome 65.0 for android",
                 "chrome 65.0 for ios", "chrome 66.0", "chrome 66.0 for android", "chrome 66.0 for ios")

fe_part1[fe_part1$id_31 %in% new_browsers,] <- 1

paste0(nrow(fe_part1[fe_part1$latest_browser == 1 ,])*100/nrow(fe_part1), " % total rows with latest browsers")
paste0(nrow(fe_part1[fe_part1$latest_browser == 0 ,])*100/nrow(fe_part1), " % total rows with old browsers")

fe_part1 <- fe_part1[, -c(1)]

# %% [code]
################################################################################################
#FE part2: mean and sd transaction amount to card for card1, card4, id_02, D15
fe_part2 <- tem[, c("card1", "card4", "TransactionAmt", "id_02", "D15")]

fe_part2 <- fe_part2 %>% left_join(ddply(fe_part2, ~card1, summarise, mean_card1_Trans= mean(TransactionAmt), sd_card1_Trans = sd(TransactionAmt)))
fe_part2 <- fe_part2 %>% left_join(ddply(fe_part2, ~card4, summarise, mean_card4_Trans = mean(TransactionAmt), sd_card4_Trans = sd(TransactionAmt)))

fe_part2 <- fe_part2 %>% left_join(ddply(fe_part2, ~card1, summarise, mean_card1_id02= mean(id_02), sd_card1_id02 = sd(id_02)))
fe_part2 <- fe_part2 %>% left_join(ddply(fe_part2, ~card4, summarise, mean_card4_id02 = mean(id_02), sd_card4_id02 = sd(id_02)))

fe_part2 <- fe_part2 %>% left_join(ddply(fe_part2, ~card1, summarise, mean_card1_D15= mean(D15), sd_card1_D15 = sd(D15)))
fe_part2 <- fe_part2 %>% left_join(ddply(fe_part2, ~card4, summarise, mean_card4_D15 = mean(D15), sd_card4_D15 = sd(D15)))

# %% [code]
head(fe_part2)
head(unique(fe_part2$mean_card1_D15))
head(unique(fe_part2$mean_card1_id02))
fe_part2 <- fe_part2[, -c(1,2,3,4,5)]

# %% [code]
##############################################################################################################
# FE part3: email binning for purchaser and Recipiant

#proton mail is sketch
fe_part3 <- tem[, c("P_emaildomain", "R_emaildomain")]

#email bin function
bin_email <- function(df, grouped, P_colname, R_colname){
    
    typeof(df)
    df$P_placeholder <- 0
    df$R_placeholder <- 0
    
    names(df)[names(df) == "P_placeholder"] <- P_colname
    names(df)[names(df) == "R_placeholder"] <- R_colname
    
    df[df$P_emaildomain %in% grouped, P_colname] <- 1
    df[df$R_emaildomain %in% grouped, R_colname] <- 1
    
    print(paste0(nrow(df[df[, P_colname] == 1,])*100/nrow(df), " % total transactions are ", P_colname, " for Purchaser"))
    print(paste0(nrow(df[df[, R_colname] == 1,])*100/nrow(df), " % total transactions are ", R_colname, " for Recipiant"))
    
    return(df)
}

#is Yahoo
a<- c("yahoo.fr", "yahoo.de", "yahoo.es", "yahoo.co.uk", "yahoo.com", "yahoo.com.mx", "ymail.com", "rocketmail.com", "frontiernet.net")
fe_part3 <- bin_email(fe_part3,a, "P_isyahoo", "R_isyahoo")

#is Microsoft
b<- c("hotmail.com", "live.com.mx", "live.com", "msn.com", "hotmail.es", "outlook.es", "hotmail.fr", "hotmail.de", "hotmail.co.uk")
fe_part3 <- bin_email(fe_part3,b, "P_ismfst", "R_ismfst")

#is apple icloud / mac / me -> apple
c<- c("icloud.com", "mac.com", "me.com")
fe_part3 <- bin_email(fe_part3,c, "P_ismac", "R_ismac")

#is att
d <- c("prodigy.net.mx", "att.net", "sbxglobal.net")
fe_part3 <- bin_email(fe_part3,d, "P_isatt", "R_isatt")

#iscenturylink
e <- c("centurylink.net", "embarqmail.com", "q.com")
fe_part3 <- bin_email(fe_part3,e, "P_iscenturylink", "R_iscenturylink")

#isaol
f <- c("aim.com", "aol.com")
fe_part3 <- bin_email(fe_part3,f, "P_isaol", "R_isaol")

#isspectrum
g <- c("twc.com", "charter.com")
fe_part3 <- bin_email(fe_part3,g, "P_isspectrum", "R_isspectrum")

#isproton
h <- c("protonmail.com")
fe_part3 <- bin_email(fe_part3,h, "P_isproton", "R_isproton")

#iscomcast
i <- c("comcast.net")
fe_part3 <- bin_email(fe_part3,i, "P_iscomcast", "R_iscomcast")

#isgoogle
j <- c("gmail.com")
fe_part3 <- bin_email(fe_part3,j, "P_isgoogle", "R_isgoogle")

#isanonynous
k <- c("anonymous.com")
fe_part3 <- bin_email(fe_part3,k, "P_isanon", "R_isanon")

#isNA
l <- NA
fe_part3 <- bin_email(fe_part3,l, "P_isNA", "R_isNA")

#-c(a,b,c,d,e,f,g,h,i, j, k, l) remaining bins

fe_part3 <- fe_part3[, -c(1,2)]

# %% [code]
##############################################################################################################
#FE part4: count encoding of base features
char_features <- tem[,colnames(tem) %in% 
                     c("ProductCD","card1","card2","card3","card4","card5","card6","addr1","addr2","P_emaildomain",
                   "R_emaildomain","M1","M2","M3","M4","M5","M6","M7","M8","M9","DeviceType","DeviceInfo","id_12",
                   "id_13","id_14","id_15","id_16","id_17","id_18","id_19","id_20","id_21","id_22","id_23","id_24",
                   "id_25","id_26","id_27","id_28","id_29","id_30","id_31","id_32","id_33","id_34","id_35","id_36",
                   "id_37","id_38")]

fe_part4 <- data.frame(0)
for(a in colnames(char_features) ){
  tem1 <- char_features %>% group_by(.dots = a) %>% mutate(count = length(card4)) %>% ungroup() %>% select(count)
  colnames(tem1) <- paste(a,"__count_encoding",sep="")
  fe_part4 <- data.frame(fe_part4,tem1)
}


fe_part4 <- fe_part4[,-1]
rm(char_features,tem1) ; invisible(gc())
cat("fe_part4 ncol :" , ncol(fe_part4) ,"\n" )

# %% [code]
tem <- data.frame(tem,fe_part1, fe_part2, fe_part3, fe_part4)
#############################################################################################################
# label 
char_features <- colnames(tem[, sapply(tem, class) %in% c('character', 'factor')])
for (f in char_features){
  levels <- unique(tem[[f]])
  tem[[f]] <- as.integer(factor(tem[[f]], levels=levels))
}

# %% [code]
#log of transaction amount 
#https://scikit-learn.org/stable/auto_examples/compose/plot_transformed_target.html
#idea from gb-2-make-amount-useful-again
tem$TransactionAmt <- log(tem$TransactionAmt)

# %% [code]
train <- tem[1:nrow(train),]
test <- tem[-c(1:nrow(train)),]
y_train <- y
rm(tem) ; invisible(gc())

# %% [code]
length(y_train)

# %% [code]
############################################################################################################
# model
cat("train_col :" , ncol(train), "test_col :", ncol(test) ,"\n" )


d0 <- lgb.Dataset(data.matrix( train[tr_idx,] ), label = y[tr_idx] )
dval <- lgb.Dataset(data.matrix( train[-tr_idx,] ), label = y[-tr_idx] ) 

# not tuned
lgb_param <- list(boosting_type = 'gbdt',
                  objective = "binary" ,
                  metric = "AUC",
                  boost_from_average = "false",
                  learning_rate = 0.005,
                  num_leaves = 192,
                  min_gain_to_split = 0,
                  feature_fraction = 0.3,
                 # feature_fraction_seed = 666666,
                  bagging_freq = 1,
                  bagging_fraction = 0.7,
                 # min_sum_hessian_in_leaf = 0,
                  min_data_in_leaf = 100,
                  lambda_l1 = 0,
                  lambda_l2 = 0
                 )
  
# exploratory training
valids <- list(valid = dval)
lgb <- lgb.train(params = lgb_param,  data = d0, nrounds = 15000, 
                 eval_freq = 200, valids = valids, early_stopping_rounds = 200, verbose = 1)


oof_pred <- predict(lgb, data.matrix(train[-tr_idx,]))
cat("best iter :" , lgb$best_iter, "best score :", AUC(oof_pred, y[-tr_idx]) ,"\n" )
num_rounds <- lgb$best_iter

#get nrounds with exploratory training
rm(lgb,d0,dval) ; invisible(gc())



# %% [code]
EPOCHS <- 5L
kf <- rsample::vfold_cv(train, v = EPOCHS)
kf <- rsample::rsample2caret(kf)
y_oof <- rep(0, nrow(train))
y_preds <- 0
cv_auc <- rep(NA_real_, EPOCHS)

for (i in seq(kf$index)) {
    cat("Training fold", i, fill = TRUE)
    # Get splits and create dMatrices
    trn_idx <- kf$index[[i]]
    val_idx <- kf$indexOut[[i]]
    
    dtrain <- lgb.Dataset(as.matrix(train[trn_idx, ]), label = y_train[trn_idx])
    dvalid <- lgb.Dataset(as.matrix(train[val_idx, ]), label = y_train[val_idx])
    
    # Model training
    clf <- lgb.train(
        lgb_param, 
        data = dtrain, 
        nrounds = num_rounds, 
        valids = list(train = dtrain, valid = dvalid),
        eval_freq = 200, 
        early_stopping_rounds = 200,
        reset_data = TRUE,
        verbose = 1
    )
    # OOF validation predictions
    y_oof[val_idx] <- 
        predict(clf, as.matrix(train[val_idx, ]), num_iteration = clf$best_iter)
    cv_auc[i] <- yardstick::roc_auc_vec(factor(y_train[val_idx]), y_oof[val_idx])
    cat("ROC AUC", cv_auc[i], fill = TRUE)
    
    # Submission predictions
    y_preds <- y_preds + (predict(clf, as.matrix(test), 
                                  num_iteration = clf$best_iter) / EPOCHS)
    invisible(gc())
}
cat("Mean CV ROC AUC", mean(cv_auc), fill = TRUE)

# %% [code]
#submission
imp <- lgb.importance(clf)
submission <- as_tibble(data.table::fread("../input/sample_submission.csv"))
submission$isFraud <- y_preds
data.table::fwrite(submission, "submission-baseline.csv" )
data.table::fwrite(imp, "imp.csv" )
head(submission)