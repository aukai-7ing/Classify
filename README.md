
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Classify

<!-- badges: start -->
<!-- badges: end -->

The goal of Classify is to 给出三个分类算法函数

## Installation

You can install the development version of Classify from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("aukai-7ing/Classify")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Classify)
#> 
#> 载入程序包：'Classify'
#> The following object is masked from 'package:stats':
#> 
#>     predict
library(ggplot2)
library(dplyr)
#> 
#> 载入程序包：'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(caret)
#> 载入需要的程序包：lattice
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
#取120个作训练集，30个作测试集
selected <- sample(1:150, replace = FALSE, size = 120)
X_train <- as.matrix(iris[selected, c("Petal.Length", "Petal.Width")])
X_test <- as.matrix(iris[-selected, c("Petal.Length", "Petal.Width")])
y_train <- as.integer(iris[selected, "Species"])-1
y_test <- as.integer(iris[-selected, "Species"])-1

rownames(X_train) <- NULL
rownames(X_test) <- NULL
rownames(y_train) <- NULL
rownames(y_test) <- NULL

# 准确率计算函数
accuracy <- function(y_true, y_pred) {
  total_samples <- length(y_true)
  
  # 计算正确预测的数量
  correct_predictions <- sum(y_true == y_pred)
  
  return(correct_predictions / total_samples)
}

# 执行逻辑回归模型
logistic_pred <- logistic(X_train, y_train, X_test)
accuracy_logistic <- accuracy(y_test, logistic_pred)

# 执行k-NN模型，假设k=5
knn_predictions <- kNN(X_train, y_train, X_test, k = 5)
accuracy_knn <- accuracy(y_test, knn_predictions)

# 执行SVM模型
predictions <- svm(X_train, y_train, X_test)
accuracy_svm <- accuracy(y_test, predictions)

cat("\n Logistic Regression Accuracy:", accuracy_logistic,
    "\n k-NN Accuracy:", accuracy_knn,
    "\n SVM Accuracy:", accuracy_svm, "\n")
#> 
#>  Logistic Regression Accuracy: 0.9666667 
#>  k-NN Accuracy: 0.9333333 
#>  SVM Accuracy: 0.9333333

cat("\n Logistic Regression prediction:\n", logistic_pred,
    "\n k-NN prediction:\n", knn_predictions,
    "\n SVM prediction:\n", predictions,
    "\n true data:\n", y_test, "\n")
#> 
#>  Logistic Regression prediction:
#>  0 0 0 0 0 0 0 0 1 1 1 2 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 
#>  k-NN prediction:
#>  0 0 0 0 0 0 0 0 1 1 1 2 1 1 1 1 1 2 2 1 2 2 2 2 2 2 2 2 2 2 
#>  SVM prediction:
#>  0 0 0 0 0 0 0 0 1 1 1 2 1 1 1 1 1 2 2 1 2 2 2 2 2 2 2 2 2 2 
#>  true data:
#>  0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

``` r
# 创建一个网格以绘制决策边界
x_min <- min(X_train[, 1]) - 1
x_max <- max(X_train[, 1]) + 1
y_min <- min(X_train[, 2]) - 1
y_max <- max(X_train[, 2]) + 1
grid_x1 <- seq(x_min, x_max, length.out = 100)
grid_x2 <- seq(y_min, y_max, length.out = 100)
grid_points <- expand.grid(grid_x1, grid_x2)

# 使用逻辑回归模型进行预测
logistic_grid_predictions <- logistic(X_train, y_train, as.matrix(grid_points))

# 将预测结果添加到网格数据框中
grid_df_logistic <- cbind(grid_points, class = logistic_grid_predictions)

# 绘制逻辑回归的决策边界
ggplot() +
  geom_point(data = as.data.frame(X_train), aes(x = Petal.Length, y = Petal.Width, col = factor(y_train)), size = 2) +
  geom_point(data = grid_df_logistic, aes(x = Var1, y = Var2, col = factor(class)), alpha = 0.3) +
  labs(title = "Logistic Regression", x = "Petal Length", y = "Petal Width", color = "Species") +
  theme_bw()
```

<img src="man/figures/README-plot-1.png" width="100%" />

``` r

# 使用k-NN对网格点进行预测
kNN_grid_predictions <- kNN(X_train, y_train, as.matrix(grid_points), k = 5)

# 将预测结果添加到网格数据框中
grid_df_KNN <- cbind(grid_points, class = kNN_grid_predictions)

# 绘制散点图和决策边界
ggplot() +
  geom_point(data = as.data.frame(X_train), aes(x = Petal.Length, y = Petal.Width, col = factor(y_train)), size = 2) +
  geom_point(data = grid_df_KNN, aes(x = Var1, y = Var2, col = factor(class)), alpha = 0.3) +
  labs(title = "k-NN", x = "Petal Length", y = "Petal Width", color = "Species") +
  theme_bw()
```

<img src="man/figures/README-plot-2.png" width="100%" />

``` r


# 使用SVM对网格点进行预测
svm_predictions_grid <- svm(X_train, y_train, as.matrix(grid_points))

# 将预测结果添加到网格数据框中
grid_df_svm <- cbind(grid_points, class = svm_predictions_grid)

# 绘制SVM的决策边界
ggplot() +
  geom_point(data = as.data.frame(X_train), aes(x = Petal.Length, y = Petal.Width, col = factor(y_train)), size = 2) +
  geom_point(data = grid_df_svm, aes(x = Var1, y = Var2, col = factor(class)), alpha = 0.3) +
  labs(title = "SVM Decision Boundary", x = "Petal Length", y = "Petal Width", color = "Species") +
  theme_bw()
```

<img src="man/figures/README-plot-3.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
