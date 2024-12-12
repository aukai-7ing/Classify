#'K近邻算法
#'
#'@param X_train 训练集
#'@param y_train 训练集的类
#'@param X_test 测试集
#'@param k 参数
#'@return return y_pred 预测集
#'@example
#'KNN(X_train,y_train,X_test)


# kNN Function
kNN <- function(X_train, y_train, X_test, k) {
  pred <- c()

  # 调整代码类型
  X_test <- as.matrix(X_test)
  X_train <- as.matrix(X_train)

  for (i in 1:nrow(X_test)) {
    # 计算距离
    newdist <- numeric(length(y_train))

    for (j in 1:length(y_train)) {
      newdist[j] <- euclidian(X_train[j, ], X_test[i, ])
    }

    newdist <- rbind(newdist, y_train)
    idx <- order(newdist[1, ])

    # 从小到大排序
    newdist <- newdist[, idx]

    # 标签
    count_list <- list('0' = 0, '1' = 0, '2' = 0)
    # 计数
    for (j in 1:k) {
      count_list[[as.character(newdist[2, j])]] <- count_list[[as.character(newdist[2, j])]] + 1
    }

    key_max <- names(which.max(unlist(count_list)))
    pred <- c(pred, as.integer(key_max)) }

  return(pred)
}
# Distances

euclidian <- function(p1, p2) {
  dist <- 0
  for (i in seq_along(p1)) {
    dist <- dist + (p1[i] - p2[i])^2
  }
  dist <- sqrt(dist)
  return(dist)
}
