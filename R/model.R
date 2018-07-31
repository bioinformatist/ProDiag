# library(e1071)
# tune.model <- tune(svm, Status~., data = training.data[, -1], range = list(cost = 10^(-1:2), gamma = seq(0.1,10,0.1)))
# https://rpubs.com/skydome20/R-Note14-SVM-SVR
# https://rpubs.com/skydome20/R-Note8-ANN
# require(neuralnet) # for neuralnet(), nn model
# require(caret)     # for train(), tune parameters
# names(fuck)[2] <- "SP_B"
# names(fuck)[9] <- "ITG_b1"
# names(fuck)[12] <- "TSP_1"
# fuck <- fuck[complete.cases(fuck),]
# n <- names(fuck)
# formula.ANN <- as.formula(paste("Status ~", paste(n[!n %in% "Status"], collapse = " + ")))
# https://stackoverflow.com/questions/17794575/error-in-terms-formulaformula-in-formula-and-no-data-argument
# bpn <- neuralnet(formula = formula.bpn, data = fuck,hidden = c(1, 2), learningrate = 0.01, threshold = 0.01, stepmax = 5e5)
# plot(bpn)
#
# model <- train(form=formula.bpn, data=fuck, method="neuralnet", tuneGrid = expand.grid(.layer1=c(1:4), .layer2=c(0:4), .layer3=c(0)), learningrate = 0.01, threshold = 0.01, stepmax = 5e5)

test.ann <- function(formula, data, model = "neuralnet", seed = 920304, train.ratio = 0.8) {
  smp.size <- floor(train.ratio*nrow(data))
  set.seed(seed)
  train.ind <- sample(seq_len(nrow(data)), smp.size)
  train <- data[train.ind, ]
  test <- data[-train.ind, ]
  bpn <- train(form=formula, data=train, method="neuralnet", tuneGrid = expand.grid(.layer1=c(1:4), .layer2=c(0:4), .layer3=c(0)), learningrate = 0.01, threshold = 0.01, stepmax = 5e5)$finalModel
  prob.result <- compute(bpn, test[, 1:16])$net.result
  nn.pred = ROCR::prediction(prob.result, test$Status)
  pref <- ROCR::performance(nn.pred, "tpr", "fpr")
  ROCR::plot(pref)
}
