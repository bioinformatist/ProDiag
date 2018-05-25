get.svm.model <- function(training.df, formula, cost.range = 10^(-1:2), gamma.range = seq(0.1,10,0.1)) {
  tune.model <- tune(svm, Status~., data = training.data, range = list(cost = 10^(-1:2), gamma = seq(0.1,10,0.1)))

}
