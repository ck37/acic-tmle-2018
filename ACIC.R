library(reshape2)
library(dplyr)
library(tmle)
library(randomForest)
library(ctmle)

# Set the working directory to the current path
# setwd(path)

# Function to get point estimation and CI for ATE
getATE <- function(dat){
      
      Y <- dat$y
      A <- dat$z
      Delta <- !is.na(Y)
      
      W <- dat[,2:178]
      
      # Fit Q model
      
      Q_fit <- SuperLearner(X = (cbind(A, W))[Delta,], 
                            Y=Y[Delta],
                            SL.library= c(
                                  'SL.gam',
                                  'SL.gbm'),
                            cvControl=list(V = 5L), family = 'gaussian'
      )
      Qbar1 <- predict(Q_fit, newx=(cbind(A=1, W)))$pred
      Qbar0 <- predict(Q_fit, newx=(cbind(A=0, W)))$pred
      Q <- cbind(Qbar0, Qbar1)
      
      # Fit g model
      g_fit <- SuperLearner(X =W, 
                            Y=A,
                            SL.library= c(
                                  'SL.gam',
                                  'SL.glmnet'),
                            cvControl=list(V = 5L),  family = 'binomial'
      )
      g1W <- predict(g_fit, newx=W)$pred
      
      # Fit Delta model if there is missing value
      if(sum(Delta) != length(Y)){
            delta_fit <- SuperLearner(X = (cbind(A, W)), 
                                  Y = Delta,
                                  SL.library= c(
                                        'SL.gam',
                                        'SL.glmnet'),
                                  cvControl=list(V = 5L),  family = 'binomial'
            )
            pDelta1 <- predict(delta_fit, newx=(cbind(A=1, W)))$pred
            pDelta0 <- predict(delta_fit, newx=(cbind(A=0, W)))$pred
            pDelta <- cbind(pDelta0, pDelta1)
            
      }else{
            pDelta = NULL
            Delta <- rep(1,length(Y))
      }
      
      
      tmle_fit <- tmle(Y=Y, A=A, W=W, 
                       Delta = Delta,
                       verbose = TRUE,
                       Q = Q,
                       g1W = g1W,
                       pDelta1 = pDelta)
      
      res <- list(est = tmle_fit$estimates$ATE$psi,
                  CI = tmle_fit$estimates$ATE$CI)
      
      return(res)
}


X <- read.csv('../x.csv')
files <- list.files()

train_files <- files[!grepl("_cf", files)]

mse_all <- c()
covered_all <- c()
res_all <- c()

for(file in train_files){
      
      # file <- train_files[2]
      print(file)
      name = strsplit(x = file, split = "[.]")[[1]][1]
      
      tmp <- read.csv(file)
      
      dat <- merge(X, tmp, by = 'sample_id')
      
      res <- getATE(dat)
      
      # saving results
      res_tmp <- data.frame(ufid = name, 
                            effect_size = res$est, 
                            li = res$CI[1], 
                            ri = res$CI[2])
      res_all <- rbind(res_all, res_tmp)
      
      # Evaluation
      truth <- paste0(name, '_cf.csv')
      truth <- read.csv(truth)
      ATE_true <- mean(truth$y1 - truth$y0)
      
      mse_tmp <- (ATE_true - res$est)^2
      covered_tmp <-  (res$CI[1] < ATE_true) &  (res$CI[2] > ATE_true) 
      
      mse_all <- c(mse_all, mse_tmp)
      covered_all <- c(covered_all, covered_tmp)
}

# Save the results for submission
write.csv(res_all, file = 'res.csv', row.names = FALSE)
