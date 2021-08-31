 ### 110年衛生福利部統計處R軟體推廣課程 ###
### 進階篇：共病指數CCI的資料處理及應用 @ 線上課程 ###



### 初始化 ----



# ~ 資料匯入與套件載入 ----

# 下載
pack_need <- c("data.table", "fastDummies")
install.packages(pack_need, dependencies = TRUE)

rm(pack_need)

# library
library(data.table) # 資料操作套件
library(fastDummies) # 創造虛擬變項



# ~ 路徑物件 ----

# 建立好路徑就可以重複使用
# 開新專案也可以模仿套用
file <- "C:/Users/liu/Downloads/data"
full <- "C:/Users/liu/Downloads/data/full"
demo <- "C:/Users/liu/Downloads/data/demo"





### 研究資料 ----



# ~ 匯入資料 ----

# pneumonia_cut.csv
setwd(file)
dt <- fread("pneumonia_cut.csv")

# 資料結構
head(dt)

# 整體摘要
summary(dt)

# 變數名稱
colnames(dt)



# ~ 描述性統計 ----

# 數值型資料描述性統計分析
summary(dt$age) # 連續變數 age 年齡

summary(dt$bed_day) # 連續變數 bed_day 住院天數

# 類別型資料描述性統計分析
table(dt$sex) # 類別變數 sex，1 = 男性；2 = 女性

table(dt$agegp) # 類別變數 agegp，0 = 65歲以下；1 = 65歲(含)以上

table(dt$incgp) # 類別變數 incgp，依據承保檔投保金額區分為0 = 15,840以下；1 = 15,840-30,000；2 = 30,000以上

table(dt$med_center) # 類別變數 med_center，0 = 在非醫學中心接受治療；1 = 在醫學中心接受治療

table(dt$pneumonia_pre) # 類別變數 pneumonia_pre，0 = 過去半年沒有曾經因為肺炎住院；1 = 過去半年曾經因為肺炎住院

table(dt$htn) # 類別變數 htn，0 = 過去半年沒有曾經因為HTN就診；1 = 過去半年曾經因為HTN就診

table(dt$dm) # 類別變數 dm，0 = 過去半年沒有曾經因為DM就診；1 = 過去半年曾經因為DM就診

table(dt$hyperlipidemia) # 類別變數 hyperlipidemia，0 = 過去半年沒有曾經因為hyperlipidemia就診；1 = 過去半年曾經因為hyperlipidemia就診

# 結果變數描述性統計
table(dt$death) # 類別變數 death，0 = 180天內存活；1 = 180天內死亡



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



# ~ logistic regression ----

# formula
fm_1 <- death ~ age + sex_1 + incgp_1 + incgp_2 + med_center + pneumonia_pre + htn + dm + hyperlipidemia

# fitting
model_1 <- glm(fm_1, data = dt, family = "binomial")

# summary of model
summary(model_1)

# odds ratio & ci & p value
cbind(
  OR = round(exp(coef(model_1)), 2),
  round(exp(confint(model_1)), 2),
  p.value = round(summary(model_1)$coefficients[, 4], 4)
)

# WARNING
# Reviewer提醒我們要加入CCI作為covariate
# CCI, charlson comorbidity index





# 衛福資料簡介 -----



# ~ 門診檔 ----

# 檔案路徑
setwd(demo)

# 費用檔
cd <- fread("cd.csv", colClasses = "character")
View(cd)

# 醫令檔
oo <- fread("oo.csv", colClasses = "character")
View(oo)

# 合併結果
m1 <- merge.data.table(cd, oo, by = c("hosp_id", "fee_ym", "appl_date", "appl_type", "case_type", "seq_no"))
View(m1)

# 整理記憶體
rm(cd, oo, m1)



# ~ 住院檔 ----

# 檔案路徑
setwd(demo)

# 住院費用檔
dd <- fread("dd.csv", colClasses = "character")
View(dd)

# 住院醫令檔
do <- fread("do.csv", colClasses = "character")
View(do)

# 合併結果
m1 <- merge.data.table(dd, do, by = c("hosp_id", "fee_ym", "appl_date", "appl_type", "case_type", "seq_no"))
View(m1)

# 整理記憶體
rm(dd, do, m1)





### 以衛福資料庫建構CCI ----



# ~ 住院檔 ----

# 讀取住院費用檔
setwd(full)
temp <- fread("h_nhi_ipdte103.csv", colClasses = "character")
head(temp)
colnames(temp) <- tolower(colnames(temp))
temp <- temp[, .(id, in_date, fee_ym, icd9cm_1, icd9cm_2, icd9cm_3, icd9cm_4, icd9cm_5)]
head(temp, 20)

# 取研究樣本相關的資料
nrow(temp)
temp <- temp[dt$id, on = .(id), nomatch = 0]
nrow(temp)

# 在1-5月的申報紀錄
table(temp$fee_ym)
temp <- temp[grepl("20140[1-5]", fee_ym)]
table(temp$fee_ym)

# 將診斷碼轉置方便後續處理
head(temp[, .(icd9cm_1, icd9cm_2, icd9cm_3, icd9cm_4, icd9cm_5)], 20)
temp <- melt(
  data = temp, 
  id.vars = c("id", "in_date"),
  measure.vars = c("icd9cm_1", "icd9cm_2", "icd9cm_3", "icd9cm_4", "icd9cm_5"),
  value.name = "icd")
head(temp, 20)

# 將同一次住院的診斷去重複
nrow(temp)
temp <- unique(temp[, .(id, in_date, icd)])
nrow(temp)
head(temp, 20)

# 找尋CCI相關的診斷
temp <- temp[grepl("^41[0-2]", icd), mi := 1] # Myocardial infarction
temp <- temp[grepl("^39891|^40201|^40211|^40291|^40401|^40403|^40411|^40413|^40491|^40493|^425[4-9]|^428", icd), chf := 1] # Congestive heart failure
temp <- temp[grepl("^0930|^4373|^440|^441|^443[1-9]|^4471|^5571|^5579|^V434", icd), pvd := 1] # Peripheral vascular disease
temp <- temp[grepl("^36234|^43[0-8]", icd), cevd := 1] # Cerebrovascular disease
temp <- temp[grepl("^290|^2941|^3312", icd), dementia := 1] # Dementia
temp <- temp[grepl("^4168|^4169|^49[0-9]|^50[1-5]|^5064|^5081|^5088", icd), copd := 1] # Chronic pulmonary disease
temp <- temp[grepl("^4465|^710[0-4]|^714[0-2]|^7148|^725", icd), rheumd := 1] # Rheumatic disease
temp <- temp[grepl("^53[1-4]", icd), pud := 1] # Peptic ulcer disease
temp <- temp[grepl("^07022|^07023|^07032|^07033|^07044|^07054|^0706|^0709|^570|^571|^5733|^5734|^5738|^5739|^V427", icd), mld := 1] # Mild liver disease
temp <- temp[grepl("^250[0-3]|^2508|^2509", icd), diab := 1] # Diabetes without chronic complication
temp <- temp[grepl("^250[4-7]", icd), diabwc := 1] # Diabetes with chronic complication
temp <- temp[grepl("^3341|^342|^343|^344[0-69]|^3449", icd), hemip := 1] # Hemiplegia or paraplegia
temp <- temp[grepl("^40301|^40311|^40391|^40402|^40403|^40412|^40413|^40492|^40493|^582|^583[0-7]|^585|^586|^5880|^V420|^V451|^V56", icd), rend := 1] # Renal disease
temp <- temp[grepl("^14[0-9]|^15[0-9]|^16[0-9]|^17[0-2]|^17[4-9]|^18[0-9]|^19[0-4]|^195[0-8]|^20[0-8]|^2386", icd), canc := 1] # malignancy
temp <- temp[grepl("^456[0-2]|^572[2-8]", icd), msld := 1] # Moderate or severe liver disease
temp <- temp[grepl("^19[6-9]", icd), metacanc := 1] # Metastatic solid tumor
temp <- temp[grepl("^04[2-4]", icd), aids := 1] # AIDS/HIV

# 確認有抓資料
sapply(temp[, .(mi, chf, pvd, cevd, dementia, copd, rheumd, pud, mld, diab, diabwc, hemip, rend, canc, msld, metacanc, aids)], sum, na.rm = T)

# 歸人
temp <- temp[, lapply(.SD, sum, na.rm = T), .SDcols = c("mi", "chf", "pvd", "cevd", "dementia", "copd", "rheumd", "pud", "mld", "diab", "diabwc", "hemip", "rend", "canc", "msld", "metacanc", "aids"), by = .(id)]

# 另存
dt_cci_ipd <- temp

# 整理
rm(temp)
gc()



# ~ 門診檔 ----

# 讀取檔案之檔名清單
setwd(full)
dt_opd <- list.files(pattern = "^h_nhi_opdte1030[1-5]_10.csv$")
print(dt_opd)

# 儲存輸出之容器
dt_cci_opd <- vector(mode = "list", length(dt_opd))

# 以迴圈的方式逐一讀取及處理
for (i in 1:length(dt_opd)) {

  # 讀取住院費用檔
  temp <- fread(dt_opd[[i]], colClasses = "character")
  colnames(temp) <- tolower(colnames(temp))
  temp <- temp[, .(id, func_date, fee_ym, icd9cm_1, icd9cm_2, icd9cm_3)]
  
  # 取研究樣本相關的資料
  temp <- temp[dt$id, on = .(id), nomatch = 0]
  
  # 在1-5月的申報紀錄
  temp <- temp[grepl("20140[1-5]", fee_ym)]
  
  # 將診斷碼轉置方便後續處理
  temp <- melt(
    data = temp, 
    id.vars = c("id", "func_date"),
    measure.vars = c("icd9cm_1", "icd9cm_2", "icd9cm_3"),
    value.name = "icd")
  
  # 將同一次門診的診斷去重複
  temp <- unique(temp[, .(id, func_date, icd)])
  
  # 找尋CCI相關的診斷
  temp <- temp[grepl("^41[0-2]", icd), mi := 1] # Myocardial infarction
  temp <- temp[grepl("^39891|^40201|^40211|^40291|^40401|^40403|^40411|^40413|^40491|^40493|^425[4-9]|^428", icd), chf := 1] # Congestive heart failure
  temp <- temp[grepl("^0930|^4373|^440|^441|^443[1-9]|^4471|^5571|^5579|^V434", icd), pvd := 1] # Peripheral vascular disease
  temp <- temp[grepl("^36234|^43[0-8]", icd), cevd := 1] # Cerebrovascular disease
  temp <- temp[grepl("^290|^2941|^3312", icd), dementia := 1] # Dementia
  temp <- temp[grepl("^4168|^4169|^49[0-9]|^50[1-5]|^5064|^5081|^5088", icd), copd := 1] # Chronic pulmonary disease
  temp <- temp[grepl("^4465|^710[0-4]|^714[0-2]|^7148|^725", icd), rheumd := 1] # Rheumatic disease
  temp <- temp[grepl("^53[1-4]", icd), pud := 1] # Peptic ulcer disease
  temp <- temp[grepl("^07022|^07023|^07032|^07033|^07044|^07054|^0706|^0709|^570|^571|^5733|^5734|^5738|^5739|^V427", icd), mld := 1] # Mild liver disease
  temp <- temp[grepl("^250[0-3]|^2508|^2509", icd), diab := 1] # Diabetes without chronic complication
  temp <- temp[grepl("^250[4-7]", icd), diabwc := 1] # Diabetes with chronic complication
  temp <- temp[grepl("^3341|^342|^343|^344[0-69]|^3449", icd), hemip := 1] # Hemiplegia or paraplegia
  temp <- temp[grepl("^40301|^40311|^40391|^40402|^40403|^40412|^40413|^40492|^40493|^582|^583[0-7]|^585|^586|^5880|^V420|^V451|^V56", icd), rend := 1] # Renal disease
  temp <- temp[grepl("^14[0-9]|^15[0-9]|^16[0-9]|^17[0-2]|^17[4-9]|^18[0-9]|^19[0-4]|^195[0-8]|^20[0-8]|^2386", icd), canc := 1] # malignancy
  temp <- temp[grepl("^456[0-2]|^572[2-8]", icd), msld := 1] # Moderate or severe liver disease
  temp <- temp[grepl("^19[6-9]", icd), metacanc := 1] # Metastatic solid tumor
  temp <- temp[grepl("^04[2-4]", icd), aids := 1] # AIDS/HIV
  
  # 歸人
  temp <- temp[, lapply(.SD, sum, na.rm = T), .SDcols = c("mi", "chf", "pvd", "cevd", "dementia", "copd", "rheumd", "pud", "mld", "diab", "diabwc", "hemip", "rend", "canc", "msld", "metacanc", "aids"), by = .(id)]
  
  # 另存
  dt_cci_opd[[i]] <- temp
  
  # 整理
  rm(temp)
  
  # log
  print(paste("complete", i, Sys.time()))
}

# 垂直疊加
dt_cci_opd <- rbindlist(dt_cci_opd)

# 歸人
dt_cci_opd <- dt_cci_opd[, lapply(.SD, sum, na.rm = T), .SDcols = c("mi", "chf", "pvd", "cevd", "dementia", "copd", "rheumd", "pud", "mld", "diab", "diabwc", "hemip", "rend", "canc", "msld", "metacanc", "aids"), by = .(id)]

# 確認有抓資料
sapply(dt_cci_opd[, .(mi, chf, pvd, cevd, dementia, copd, rheumd, pud, mld, diab, diabwc, hemip, rend, canc, msld, metacanc, aids)], sum, na.rm = T)

# 整理
rm(dt_opd)
rm(i)
gc()



# ~ 整合門診及住院 ----

# 垂直整合
dt_cci <- rbind(dt_cci_opd, dt_cci_ipd)

# 歸人
dt_cci <- dt_cci[, lapply(.SD, sum, na.rm = T), .SDcols = c("mi", "chf", "pvd", "cevd", "dementia", "copd", "rheumd", "pud", "mld", "diab", "diabwc", "hemip", "rend", "canc", "msld", "metacanc", "aids"), by = .(id)]

# 把全體研究樣本的ID合併過來，即使沒有抓到CCI因子的，也要在variable裡面補上0
dt_cci <- merge(dt[, .(id)], dt_cci, by = c("id"), all.x = T)

# 至少有一次就診的，標記為1，代表有此病史，其餘標記為0
dt_cci <- dt_cci[, lapply(.SD, function(x){ifelse(is.na(x), 0, ifelse(x >= 1, 1, 0))}), .SDcols = c("mi", "chf", "pvd", "cevd", "dementia", "copd", "rheumd", "pud", "mld", "diab", "diabwc", "hemip", "rend", "canc", "msld", "metacanc", "aids"), by = .(id)]

# 確認資料筆數
sapply(dt_cci[, .(mi, chf, pvd, cevd, dementia, copd, rheumd, pud, mld, diab, diabwc, hemip, rend, canc, msld, metacanc, aids)], sum, na.rm = T)

# 計算CCI指標
dt_cci <- dt_cci[, cci := ((mi + chf + pvd + cevd + dementia + copd + rheumd + pud + mld + diab) * 1 + (diabwc + hemip + rend + canc) * 2 + msld * 3 + (metacanc + aids) * 6)]

# CCI的分布
table(dt_cci$cci)

# histogram
hist(dt_cci$cci)



# ~ 合併原始資料 ----

# 另存
dt_2 <- merge(dt, dt_cci, by = c("id"), all.x = T)



### 重新分析 ----



# ~ logistic regression + cci score ----

# formula
fm_2 <- death ~ age + sex_1 + incgp_1 + incgp_2 + med_center + pneumonia_pre + htn + dm + hyperlipidemia + cci

# fitting
model_2 <- glm(fm_2, data = dt_2, family = "binomial")

# summary of model
summary(model_2)

# odds ratio & ci & p value
cbind(
  OR = round(exp(coef(model_2)), 2),
  round(exp(confint(model_2)), 2),
  p.value = round(summary(model_2)$coefficients[, 4], 4)
)





### END ###
### Author: Peter Liu 劉品崧 ###
### Contact: psliu520@gmail.com ###