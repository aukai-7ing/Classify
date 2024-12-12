#'支持向量机
#'
#'@param X_train 训练集
#'@param y_train 训练集的类
#'@param X_test 测试集
#'@return return y_pred 预测集
#'@example
#'svm(X_train,y_train,X_test)
# 梯度函数
svm_gradient<- function(x,y,alpha=0.001,num_iter=10000){
  X<- cbind(1,x)#make design matrix
  n <- nrow(X)  #number of sample
  p <- ncol(X) #number of feature+1 (bias)
  w_intial <- rep(0,p)
  W <- matrix(w_intial ,nrow = num_iter+1,ncol = p,byrow = T) #matrix put intial guess and the procedure to do gradient descent
  for(i in 1:num_iter){
    for(j in 1:p)
    {
      W[i+1,j]<- W[i,j]+alpha*sum(((y*(X%*%W[i,]))<1)*1 * y * X[,j] )
    }
  }
  return(W[nrow(W),])
}

# 测试函数
svm <- function(X_train, y_train, X_test) {
  y_train <-as.matrix(y_train)
  y_train <-matrix(y_train,ncol=1)
  X_test <-cbind(1,X_test)
  classes <- sort(unique(y_train))
  t <- list()           # 用于存储每个类别的W
  votes <- matrix(0, nrow = nrow(X_test), ncol = length(classes)) # 投票矩阵

  # 为每一对类别训练 SVM
  for (i in seq_along(classes)) {
    for (j in seq(i + 1, length(classes))) {
      class1 <- classes[i]
      class2 <- classes[j]

      # 创建二元标签向量
      y_k <- ifelse(y_train == class1, 1, ifelse(y_train == class2, -1, 0))

      # 提取对应的训练数据
      X_k <- X_train[y_k != 0, ]
      y_k <- y_k[y_k != 0]
      y_k <-as.matrix(y_k)
      W <- svm_gradient(X_k, y_k)

      # 对测试集进行预测
      pred_test <- X_test %*% W

      # 投票机制: 将结果存入投票矩阵
      predicted_classes <- ifelse(pred_test > 0, class1, class2)
      for (k in seq_along(predicted_classes)) {
        votes[k, which(classes == predicted_classes[k])] <- votes[k, which(classes == predicted_classes[k])] + 1
      }
    }
  }

  # 根据投票结果选择最终类别
  prediction_test <- apply(votes, 1, which.max) - 1
  return(prediction_test)
}
