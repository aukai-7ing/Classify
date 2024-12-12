#'逻辑回归函数
#'
#'@param X_train 训练集
#'@param y_train 训练集的类
#'@param X_test 测试集
#'@param lmd 截距项
#'@param alpha 迭代速度
#'@param num_iter 迭代最高次数
#'@return return y_pred 预测集
#'@examples
#'logistic(X_train,y_train,X_test)
# Sigmoid Function
sigmoid <- function(z) {
  return(1 / (1 + exp(-z)))
}

# Gradient Descent Function
gradient_descent <- function(X, y, lmd, alpha, num_iter) {

  # 初始化theta为零
  theta <- rep(0, ncol(X))

  for (i in 1:num_iter) {
    z <- X %*% theta
    h <- sigmoid(z)
    m <- length(y)
    # 添加正则化项
    reg <- lmd / m * theta
    reg[1] <- 0  # 第一个theta（截距）不正则化

    gradient <- (t(X) %*% (h - y)) / m + reg  # 矩阵乘法和平均值

    theta <- theta - alpha * gradient
  }

  return(theta)
}


# Predict Function
predict <- function(X_test, theta) {
  z <- X_test %*% theta
  return(sigmoid(z))
}

# Main Logistic Function
logistic <- function(X_train, y_train, X_test, lmd = 0.1, alpha = 0.1, num_iter = 30000) {

  # 添加截距项
  intercept_train <- matrix(1, nrow = nrow(X_train), ncol = 1)
  X_train <- cbind(intercept_train, X_train)  # 合并截距项和特征

  intercept_test <- matrix(1, nrow = nrow(X_test), ncol = 1)
  X_test <- cbind(intercept_test, X_test)      # 合并截距项和特征

  # 一对多分类
  u <- sort(unique(y_train))  # 获取所有类别
  t <- list()           # 用于存储每个类别的theta

  for (c in u) {
    # 将标签设置为0和1
    ynew <- as.integer(y_train == c)

    # 调用梯度下降函数
    theta <- gradient_descent(X_train, ynew, lmd, alpha, num_iter)

    t[[as.character(c)]] <- theta
  }

  # 计算测试集概率
  pred_test <- matrix(0, nrow = length(u), ncol = nrow(X_test))

  for (i in u) {
    pred_test[which(u == i), ] <- predict(X_test, t[[as.character(i)]])
  }

  # 选择最大概率的类别
  prediction_test <- apply(pred_test, 2, which.max) - 1

  return(prediction_test)
}
