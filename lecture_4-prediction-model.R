### 110年衛生福利部統計處R軟體推廣課程 ###
### 進階篇：預測模型  @ 線上課程 ###



### 初始化 ----



# ~ 使用套件 ----

# 下載
pack_need <- c("data.table", "ggplot2", "fastDummies", "pROC", "class")
install.packages(pack_need, dependencies = TRUE)
rm(pack_need)

# library
library(data.table) # 資料操作套件
library(fastDummies) # 欄位轉譯套件
library(pROC) # ROC分析
library(class) # knn
library(ggplot2) # 視覺化

# 路徑物件
file <- "C:/Users/liu/Downloads/data"



# ~ 課程資料 ----

# 讀取資料
setwd(file)
dt <- fread("pneumonia.csv")

# 整體摘要性統計
summary(dt)

# 前20筆資料
head(dt, 20)

# 數值型資料描述性統計分析
summary(dt$age) # 連續變數 age

summary(dt$cci) # 連續變數 cci

summary(dt$bed_day) # 連續變數 bed_day

# 類別型資料描述性統計分析
table(dt$sex) # 類別變數 sex，1 = 男性；2 = 女性

table(dt$agegp) # 類別變數 agegp，0 = 65歲以下；1 = 65歲(含)以上

table(dt$incgp) # 類別變數 incgp，依據承保檔投保金額區分為0 = 15,840以下；1 = 15,840-30,000；2 = 30,000以上

table(dt$med_center) # 類別變數 med_center，0 = 在非醫學中心接受治療；1 = 在醫學中心接受治療

table(dt$pneumonia_pre) # 類別變數 pneumonia_pre，0 = 過去半年沒有曾經因為肺炎住院；1 = 過去半年曾經因為肺炎住院

table(dt$htn) # 類別變數 htn ，0 = 過去半年沒有曾經因為HTN就診；1 = 過去半年曾經因為HTN就診

table(dt$dm) # 類別變數 dm ，0 = 過去半年沒有曾經因為DM就診；1 = 過去半年曾經因為DM就診

table(dt$hyperlipidemia) # 類別變數 hyperlipidemia，0 = 過去半年沒有曾經因為hyperlipidemia就診；1 = 過去半年曾經因為hyperlipidemia就診



# ~ 設計dummy variables ----

# 有幾個欄位非以0/1這類one-hot-code表示
table(dt$sex)
table(dt$incgp)

# 使用fastDummies::dummy_cols進行轉碼
dt <- dummy_cols(dt, select_columns = c("sex", "incgp"))

# 確認incgp的編碼轉譯
with(dt, table(incgp_0, incgp))
with(dt, table(incgp_1, incgp))
with(dt, table(incgp_2, incgp))

# 確認sex的編碼轉譯
with(dt, table(sex_1, sex))
with(dt, table(sex_2, sex))





### 迴歸模型預測(linear) ----

# 用於預測結果為連續變數時

# 命題：建構模型來預測病患年齡

# ~ 瞭解資料 ----

# 新物件
dt_1 <- dt

# outcome
summary(dt_1$bed_day)



# ~ 分割資料 ----

# 原始樣本總數
totn <- nrow(dt_1)

# 設定亂數種子
set.seed(2188)

# 使用70%作為train dataset，另外30%作為valid dataset
prop <- 0.7
gplist <- sample(x = c("train", "valid"), size = totn, replace = T, prob = c(prop, 1 - prop))
table(gplist)

# 切割出訓練資料集
dt_1_train <- dt_1[gplist == "train"]

# 切割出驗證資料集
dt_1_valid <- dt_1[gplist == "valid"]



# ~ 建立模型 ----

# 模型架構可以獨立成為一個物件
fm <- bed_day ~ age + sex_2 + incgp_1 + incgp_2 + cci + pneumonia_pre + htn + dm + hyperlipidemia


# ~ 訓練模型 ----

# glm
model <- glm(fm, data = dt_1_train)

# 檢視
summary(model)

# 把預測值加入data中
dt_1_train$bed_day_pred <- predict(model, dt_1_train)

# 計算殘差
dt_1_train <- dt_1_train[, bed_day_resd := bed_day_pred - bed_day]

# 訓練資料集產生模型之解釋力(R-square)，越接近1越好
print(with(summary(model), 1 - deviance/null.deviance))

# 計算RMSE，越接近0越好
sqrt(mean(dt_1_train$bed_day_resd^2))

# 計算相關係數 Pearson's r，預測值與實際值的相關係數，越接近1越好
with(dt_1_train, cor(bed_day_pred, bed_day))

# 繪圖視覺化
ggplot(dt_1_train, aes(x = bed_day, y = bed_day_pred)) + geom_point(color = "red", size = 3) + geom_abline()

# 模型診斷
ggplot(dt_1_train, aes(x = bed_day_pred, y = bed_day_resd)) + geom_point(color = "black", size = 3) + geom_hline(yintercept = 0)



# ~ 驗證模型 ----

# 套用模型，建立預測結果的欄位
dt_1_valid$bed_day_pred <- predict(model, newdata = dt_1_valid)

# 計算殘差
dt_1_valid <- dt_1_valid[, bed_day_resd := bed_day_pred - bed_day]

# 計算RMSE，越接近0越好
sqrt(mean(dt_1_valid$bed_day_resd^2))

# 計算相關係數 Pearson's r，預測值與實際值的相關係數，越接近1越好
with(dt_1_valid, cor(bed_day_pred, bed_day))

# 繪圖視覺化
ggplot(dt_1_valid, aes(x = bed_day, y = bed_day_pred)) + geom_point(color = "blue", size = 3) + geom_abline()

# 模型診斷
ggplot(dt_1_valid, aes(x = bed_day_pred, y = bed_day_resd)) + geom_point(color = "black", size = 3) + geom_hline(yintercept = 0)



# 整理記憶體
rm(dt_1, dt_1_train, dt_1_valid)
rm(totn, prop, gplist, fm, model)





### 迴歸模型預測(logistic) ----

# 預測結果為二元變數

# 命題：建構模型來預測病患6個月內的存活狀況

# ~ 瞭解資料 ----

# 新物件
dt_2 <- dt

# outcome
table(dt_2$death)



# ~ 分割資料 ----

# 原始樣本總數
totn <- nrow(dt_2)

# 設定亂數種子
set.seed(5050)

# 使用60%作為train dataset，另外40%作為valid dataset
prop <- 0.6
gplist <- sample(x = c("train", "valid"), size = totn, replace = T, prob = c(prop, 1 - prop))
table(gplist)

# 切割出訓練資料集
dt_2_train <- dt_2[gplist == "train"]

# 切割出驗證資料集
dt_2_valid <- dt_2[gplist == "valid"]



# ~ 建立模型 ----

# 模型架構可以獨立成為一個物件
fm <- death ~ age + sex_2 + incgp_1 + incgp_2 + med_center + cci + pneumonia_pre + htn + dm + hyperlipidemia


# ~ 訓練模型 ----

# glm
model <- glm(fm, data = dt_2_train, family = "binomial")

# 檢視
summary(model)

# 把預測值加入data中
dt_2_train$death_pred <- predict(model, dt_2_train, type = "response")

# 建構ROC物件
dt_2_train_roc <- roc(dt_2_train$death, dt_2_train$death_pred)

# 統計ROC曲線下面積，越接近1越好
auc(dt_2_train_roc)

# 繪製ROC曲線
plot(dt_2_train_roc, col = "red", lwd = 3)
text(x = 0.2, y = 0.2, paste0("AUROC = ", round(auc(dt_2_train_roc), 4)))



# ~ 驗證模型 ----

# 套用模型，建立預測結果的欄位
dt_2_valid$death_prob <- predict(model, dt_2_valid, type = "response")

# 建構ROC物件
dt_2_valid_roc <- roc(dt_2_valid$death, dt_2_valid$death_prob)

# 統計ROC曲線下面積，越接近1越好
auc(dt_2_valid_roc)

# 繪製ROC曲線
plot(dt_2_valid_roc, col = "blue", lwd = 3)
text(x = 0.2, y = 0.2, paste0("AUROC = ", round(auc(dt_2_valid_roc), 4)))

# 利用預測機率找出切點
cutoff <- coords(dt_2_valid_roc, "best", ret = "threshold")

# 若預測機率大於cutoff則預測事件會發生
dt_2_valid <- dt_2_valid[, death_pred := 0][death_prob >= cutoff$threshold, death_pred := 1]

# 交叉表
with(dt_2_valid, table(death, death_pred))




# 整理記憶體
rm(dt_2, dt_2_train, dt_2_valid)
rm(dt_2_train_roc, dt_2_valid_roc, cutoff)
rm(totn, prop, gplist, fm, model)





### 分類預測KNN ----

# 用於outcome為多分類

# 命題：建構模型來預測病患住院天數會落在哪一個段落

# ~ 瞭解資料 ----

# 新物件
dt_3 <- dt

# outcome
summary(dt_3$bed_day)
table(dt_3$bed_day)

# 分組
dt_3 <- dt_3[, daygp := cut(x = bed_day, breaks = c(1, 5, 11, 31), labels = c(1:3), include.lowest = T)]
table(dt_3$daygp)



# ~ 分割資料 ----

# 原始樣本總數
totn <- nrow(dt_3)

# 設定亂數種子
set.seed(8764)

# 使用60%作為train dataset，另外40%作為valid dataset
prop <- 0.6
gplist <- sample(x = c("train", "valid"), size = totn, replace = T, prob = c(prop, 1 - prop))
table(gplist)

# 切割出訓練資料集
dt_3_train <- dt_3[gplist == "train"]

# 切割出驗證資料集
dt_3_valid <- dt_3[gplist == "valid"]



# ~ 建立模型 ----

# 擷取變數，KNN比較特別，把資料為進去之前要用as.matrix轉換為矩陣物件
# *_iv代表independent variable，模型的因子，自變項
# *_dv代表dependent variable，模型的結果，依變項
dt_3_train_iv <- as.matrix(dt_3_train[, c("agegp", "sex_2", "incgp_1", "incgp_2", "med_center", "cci", "pneumonia_pre", "htn", "dm", "hyperlipidemia")])
dt_3_train_dv <- as.matrix(dt_3_train[, c("daygp")])
dt_3_valid_iv <- as.matrix(dt_3_valid[, c("agegp", "sex_2", "incgp_1", "incgp_2", "med_center", "cci", "pneumonia_pre", "htn", "dm", "hyperlipidemia")])
dt_3_valid_dv <- as.matrix(dt_3_valid[, c("daygp")])



# ~ 訓練模型 ----

# knn
dt_3_valid$daygp_pred <- knn(
  train = dt_3_train_iv,
  test = dt_3_valid_iv,
  cl = dt_3_train_dv,
  k = 3
)



# ~ 驗證模型 ----

# 確認
with(dt_3_valid, table(daygp, daygp_pred))

# 相同的比率
with(dt_3_valid, mean(daygp_pred == daygp))




# 整理記憶體
rm(dt_3, dt_3_train, dt_3_valid)
rm(dt_3_train_iv, dt_3_train_dv, dt_3_valid_iv, dt_3_valid_dv)
rm(totn, prop, gplist)





### 如何調整模型 ----



# ~ 模型因子的變化(1) ----

# 新物件
dt_4 <- dt

# age
summary(dt_4$age)
table(dt_4$agegp)

# 資料分割
totn <- nrow(dt_4)
set.seed(5050)
prop <- 0.6
gplist <- sample(x = c("train", "valid"), size = totn, replace = T, prob = c(prop, 1 - prop))
table(gplist)
dt_4_train <- dt_4[gplist == "train"]
dt_4_valid <- dt_4[gplist == "valid"]

# 模型(1)age以連續數值放入
fm_1 <- death ~ age + sex_2 + incgp_1 + incgp_2 + med_center + cci + pneumonia_pre + htn + dm + hyperlipidemia

model_1 <- glm(fm_1, data = dt_4_train, family = "binomial")
dt_4_valid$death_prob_1 <- predict(model_1, dt_4_valid, type = "response")
dt_4_valid_roc <- roc(dt_4_valid$death, dt_4_valid$death_prob_1)
cutoff <- coords(dt_4_valid_roc, "best", ret = "threshold")
dt_4_valid <- dt_4_valid[, death_pred_1 := 0][death_prob_1 >= cutoff$threshold, death_pred_1 := 1]

# 模型(2)age以類別dummies放入
fm_2 <- death ~ agegp + sex_2 + incgp_1 + incgp_2 + med_center + cci + pneumonia_pre + htn + dm + hyperlipidemia

model_2 <- glm(fm_2, data = dt_4_train, family = "binomial")
dt_4_valid$death_prob_2 <- predict(model_2, dt_4_valid, type = "response")
dt_4_valid_roc <- roc(dt_4_valid$death, dt_4_valid$death_prob_2)
cutoff <- coords(dt_4_valid_roc, "best", ret = "threshold")
dt_4_valid <- dt_4_valid[, death_pred_2 := 0][death_prob_2 >= cutoff$threshold, death_pred_2 := 1]

# 交叉表
with(dt_4_valid, table(death, death_pred_1))
with(dt_4_valid, table(death, death_pred_2))

# 整理記憶體
rm(dt_4, dt_4_train, dt_4_valid)
rm(fm_1, fm_2)
rm(model_1, model_2)
rm(cutoff)
rm(dt_4_valid_roc)
rm(totn, prop, gplist)



# ~ 模型因子的變化(2) ----

# 新物件
dt_5 <- dt

# age
summary(dt_5$age)

# 新的因子
dt_5 <- dt_5[, age_p2 := age**2]
dt_5 <- dt_5[, age_sqrt := sqrt(age)]

head(dt_5[, .(id, age, age_p2, age_sqrt)])

# 資料分割
totn <- nrow(dt_5)
set.seed(5050)
prop <- 0.6
gplist <- sample(x = c("train", "valid"), size = totn, replace = T, prob = c(prop, 1 - prop))
table(gplist)
dt_5_train <- dt_5[gplist == "train"]
dt_5_valid <- dt_5[gplist == "valid"]

# 模型(1)age以連續數值放入
fm_1 <- bed_day ~ age + sex_2 + incgp_1 + incgp_2 + med_center + cci + pneumonia_pre + htn + dm + hyperlipidemia

model_1 <- glm(fm_1, data = dt_5_train)
dt_5_valid <- dt_5_valid[, bed_day_pred_1 := predict(model_1, dt_5_valid)]
dt_5_valid <- dt_5_valid[, bed_day_resd_1 := bed_day_pred_1 - bed_day]

# 模型(2)age以1次方、2次方、1/2次方放入
fm_2 <- bed_day ~ age + age_p2 + age_sqrt + sex_2 + incgp_1 + incgp_2 + med_center + cci + pneumonia_pre + htn + dm + hyperlipidemia

model_2 <- glm(fm_2, data = dt_5_train)
dt_5_valid <- dt_5_valid[, bed_day_pred_2 := predict(model_2, dt_5_valid)]
dt_5_valid <- dt_5_valid[, bed_day_resd_2 := bed_day_pred_2 - bed_day]

# RMSE
sqrt(mean(dt_5_valid$bed_day_resd_1^2))
sqrt(mean(dt_5_valid$bed_day_resd_2^2))

# 整理記憶體
rm(dt_5, dt_5_train, dt_5_valid)
rm(fm_1, fm_2)
rm(model_1, model_2)
rm(totn, prop, gplist)



# ~ 模型因子的變化(3) ----

# 新物件
dt_6 <- dt

# age
table(dt_6$re_adm)

# 資料分割
totn <- nrow(dt_6)
set.seed(5050)
prop <- 0.6
gplist <- sample(x = c("train", "valid"), size = totn, replace = T, prob = c(prop, 1 - prop))
table(gplist)
dt_6_train <- dt_6[gplist == "train"]
dt_6_valid <- dt_6[gplist == "valid"]

# 模型(1)一般模型
fm_1 <- re_adm ~ age + sex_2 + incgp_1 + incgp_2 + med_center + cci + pneumonia_pre + htn + dm + hyperlipidemia

model_1 <- glm(fm_1, data = dt_6_train, family = "binomial")
dt_6_valid$re_adm_prob_1 <- predict(model_1, dt_6_valid, type = "response")
dt_6_valid_roc <- roc(dt_6_valid$re_adm, dt_6_valid$re_adm_prob_1)
cutoff <- coords(dt_6_valid_roc, "best", ret = "threshold")
dt_6_valid <- dt_6_valid[, re_adm_pred_1 := 0][re_adm_prob_1 >= cutoff$threshold, re_adm_pred_1 := 1]

# 模型(2)讓DM和hyperlipidemia產生交互作用項
fm_2 <- re_adm ~ age + sex_2 + incgp_1 + incgp_2 + med_center + cci + pneumonia_pre + htn + dm * hyperlipidemia
model_2 <- glm(fm_2, data = dt_6_train, family = "binomial")

summary(model_2)

dt_6_valid$re_adm_prob_2 <- predict(model_2, dt_6_valid, type = "response")
dt_6_valid_roc <- roc(dt_6_valid$re_adm, dt_6_valid$re_adm_prob_2)
cutoff <- coords(dt_6_valid_roc, "best", ret = "threshold")
dt_6_valid <- dt_6_valid[, re_adm_pred_2 := 0][re_adm_prob_2 >= cutoff$threshold, re_adm_pred_2 := 1]

# 交叉表
with(dt_6_valid, table(re_adm, re_adm_pred_1))
with(dt_6_valid, table(re_adm, re_adm_pred_2))

# 整理記憶體
rm(dt_6, dt_6_train, dt_6_valid)
rm(fm_1, fm_2)
rm(model_1, model_2)
rm(cutoff)
rm(dt_6_valid_roc)
rm(totn, prop, gplist)



# ~ 模型因子的選擇 ----

# 新物件
dt_7 <- dt

# death
table(dt_7$death)

# 資料分割
totn <- nrow(dt_7)
set.seed(5050)
prop <- 0.6
gplist <- sample(x = c("train", "valid"), size = totn, replace = T, prob = c(prop, 1 - prop))
table(gplist)
dt_7_train <- dt_7[gplist == "train"]
dt_7_valid <- dt_7[gplist == "valid"]

# null model formula, not practical
fm_null <- death ~ 1

# base model formula
fm_base <- death ~ age + sex_2
model_base <- glm(fm_base, data = dt_7_train, family = "binomial")
summary(model_base)
print(model_base$aic)

# full model formula
fm_full <- death ~ age + sex_2 + incgp_1 + incgp_2 + med_center + cci + pneumonia_pre + htn + dm + hyperlipidemia
model_full <- glm(fm_full, data = dt_7_train, family = "binomial")
summary(model_full)
print(model_full$aic)

# forward method
# death ~ age + sex_2 + pneumonia_pre + cci
# AIC=40.74
model_forward <- step(model_base, scope = list(lower = model_base, upper = model_full), direction = "forward")

# backward method
# death ~ age + sex_2 + pneumonia_pre + htn + dm
# AIC=40.68
model_backward <- step(model_full, scope = list(lower = model_base, upper = model_full), direction = "backward")

# statistical significant
potential_cov <- c("incgp_1", "incgp_2", "med_center", "cci", "pneumonia_pre", "htn", "dm", "hyperlipidemia")

test_uni <- function(x){
  fm <- paste0("death ~ age + sex_2 + ", x)
  md <- glm(fm, data = dt_7_train, family = "binomial")
  st <- summary(md)
  pv <- st$coefficients[x, 4]
  rs <- paste(x, round(pv, 4))
  if (pv < 0.1) {
    return(rs)
  }
}

# only pneumonia hostory achieve criteria
unlist(lapply(potential_cov, test_uni))



# prediction result
potential_model <-c(
  base = "death ~ age + sex_2",
  full = "death ~ age + sex_2 + incgp_1 + incgp_2 + med_center + cci + pneumonia_pre + htn + dm + hyperlipidemia",
  forward = "death ~ age + sex_2 + pneumonia_pre + cci",
  backward = "death ~ age + sex_2 + pneumonia_pre + htn + dm",
  statistical = "death ~ age + sex_2 + pneumonia_pre"
)

# test accuracy
test_accuracy <- function(x){
  
  train <- dt_7_train
  valid <- dt_7_valid
  
  model <- glm(x, data = train, family = "binomial")
  valid$death_prob <- predict(model, valid, type = "response")
  valid_roc <- roc(valid$death, valid$death_prob)
  cutoff <- coords(valid_roc, "best", ret = "threshold")
  valid <- valid[, death_pred := 0][death_prob >= cutoff$threshold, death_pred := 1]
  accuracy <- with(valid, mean(death == death_pred))
  accuracy <- round(accuracy, 4)*100
  return(accuracy)
}

# lapply
unlist(lapply(potential_model, test_accuracy))

# 清理記憶體
rm(list = ls(pattern = "^fm"))
rm(list = ls(pattern = "^model"))
rm(list = ls(pattern = "^dt_7"))
rm(list = ls(pattern = "^potential"))
rm(list = ls(pattern = "^test"))
rm(totn, prop, gplist)





### 想知道更多 ----

#  Supervised learning
#    Continuous outcome
#      Linear regression
#    Count outcome
#      Poisson regression
#    Binary outcome
#      Logistic regression
#    Categorical outcome
#      kNN
#      Bayes
#      Classification Trees
#    Enhance Algorithm
#      Random forest
#      SVM, Support Vector Machine
#      GBM, Gradient Boosting Machines
#      GAM, Generalized Additive Model
#      xgboost
# Unsupervised learning
#    k-means
#    Hierarchical clustering
#    PCA, Principle Component Analysis





### END ###
### Author: Peter Liu 劉品崧 ###
### Contact: psliu520@gmail.com ###