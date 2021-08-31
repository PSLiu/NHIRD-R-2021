### 110年衛生福利部統計處R軟體推廣課程 ###
### 進階篇：Survival analysis 2021.05.10(Mon) @ 中山醫學大學 ###

### 初始化 ----



# ~ 資料匯入與套件載入 ----

# 下載
pack_need <- c("fst", "data.table", "fastDummies", "lubridate", "survival", "survminer", "cmprsk", "jtools")

for (i in 1:length(pack_need)) {
  install.packages(pack_need[i], dependencies = TRUE)
}
rm(i)
rm(pack_need)

# library
library(fst) # 讀取範例資料.fst格式檔案
library(data.table) # 資料操作套件
library(fastDummies) # 快速建立dummies
library(lubridate) # 計算時間
library(survival) # 存活分析主套件
library(survminer) # 搭配ggplot2進行存活分析繪圖的套件
library(cmprsk) # 競爭風險模型
library(jtools) # 輔助視覺化套件


# 資料來源路徑
setwd("C:/Users/liu/Downloads/data")

# 讀取範例資料
pneumonia <- fread("pneumonia.csv")



# ~ 資料檢視 ----

# 整體摘要性統計
summary(pneumonia)

# 前20筆資料
head(pneumonia, 20)

# 直接檢視(不建議使用於大型資料)
View(pneumonia)

# 數值型資料描述性統計分析
summary(pneumonia$age) # 連續變數 age

summary(pneumonia$cci)# 連續變數 cci

summary(pneumonia$bed_day)# 連續變數 bed_day

# 類別型資料描述性統計分析
table(pneumonia$sex) # 類別變數 sex，1 = 男性；2 = 女性

table(pneumonia$agegp) # 類別變數 agegp，0 = 65歲以下；1 = 65歲(含)以上

table(pneumonia$incgp) # 類別變數 incgp，依據承保檔投保金額區分為0 = 15,840以下；1 = 15,840-30,000；2 = 30,000以上

table(pneumonia$med_center) # 類別變數 med_center，0 = 在非醫學中心接受治療；1 = 在醫學中心接受治療

table(pneumonia$pneumonia_pre) # 類別變數 pneumonia_pre，0 = 過去半年沒有曾經因為肺炎住院；1 = 過去半年曾經因為肺炎住院



# ~ 設計dummy variables ----

# 使用fastDummies::dummy_cols
pneumonia <- dummy_cols(pneumonia, select_columns = c("agegp", "sex", "incgp", "med_center"))

# 確認
table(pneumonia$incgp_0, pneumonia$incgp)
table(pneumonia$incgp_1, pneumonia$incgp)
table(pneumonia$incgp_2, pneumonia$incgp)




### 存活分析的資料結構 ----



# ~ 時間 ----

# outcome為因肺炎再次入院，計算時間為出院後到發生再次肺炎或死亡事件(censor)
pneumonia <- pneumonia[, re_adm_ft := as.numeric(difftime(min(re_adm_date, death_date), discharge_date, units = "days")), by = .(id)]

# summary
summary(pneumonia$re_adm_ft)

# outcome為死亡，時間為出院後到發生死亡事件
pneumonia <- pneumonia[, death_ft := as.numeric(difftime(death_date, discharge_date, units = "days")), by = .(id)]

# summary
summary(pneumonia$death_ft)



# ~ 事件 ----

table(pneumonia$death) # 0為6個月內存活，1為出院後6個月內發生死亡

table(pneumonia$re_adm) # 0為6個月內存活，1為出院後6個月內發生肺炎再入院

table(pneumonia$re_adm, pneumonia$death) # 交叉分析



### 無母數模型 ----



# ~ 模型配適(fit) ----

# 建立物件fit_1a為觀察期間(re_adm_ft)與再次住院事件(re_adm)之模型，解釋變數為年齡分組(agegp_1)
fit_1a<- survfit(Surv(re_adm_ft, re_adm) ~ agegp_1, data = pneumonia)

# 建立物件fit_1b為觀察期間(death_ft)與死亡事件(death)之模型，解釋變數為年齡分組(agegp_1)
fit_1b<- survfit(Surv(death_ft, death) ~ agegp_1, data = pneumonia)



#~ 生命表life table ----

# 以fit_1a使用summary指令即可觀察存活狀況
summary(fit_1a)

# 以fit_1a呈現指定區間的數值
summary(fit_1a, times = seq(from = 0, to = 210, by = 30))

# 以fit_1b使用summary指令即可觀察存活狀況
summary(fit_1b)

# 以fit_1b呈現指定區間的數值
summary(fit_1b, times = seq(from = 0, to = 210, by = 30))



# ~ 存活分析圖 ----

# 以fit_1a使用survminer::ggsurvplot繪製Kaplan-Meier estimator
ggsurvplot(fit_1a)

# 以fit_1a使用survminer::ggsurvplot繪製Kaplan-Meier estimator
ggsurvplot(fit_1a, fun = "event")

# 以fit_1a使用survminer::ggsurvplot繪製Nelson-Aalen estimator
ggsurvplot(fit_1a, fun = "cumhaz")

# 以fit_1b使用survminer::ggsurvplot繪製Kaplan-Meier estimator
ggsurvplot(fit_1b)

# 以fit_1b使用survminer::ggsurvplot繪製Kaplan-Meier estimator
ggsurvplot(fit_1b, fun = "event")

# 以fit_1b使用survminer::ggsurvplot繪製Nelson-Aalen estimator
ggsurvplot(fit_1b, fun = "cumhaz")



# ~ 比較存活函數的差異 ----

# 進行log rank test
survdiff(Surv(re_adm_ft, re_adm) ~ agegp_1, data = pneumonia)

# 進行log rank test
survdiff(Surv(death_ft, death) ~ agegp_1, data = pneumonia)





### KM plot繪圖 ----



# ~ KM plot相關指令 ----

# 增加p value並指定位置
ggsurvplot(fit_1a, pval = TRUE, pval.coord = c(20, 0.2))

# 增加信賴區間，並指定信賴區間的顏色透明度
ggsurvplot(fit_1a, conf.int = T, conf.int.alpha = 0.3)

# 指定線條的顏色
ggsurvplot(fit_1a, palette = c("blue", "red"))

# 指定線條的類型
ggsurvplot(fit_1a, linetype = c("solid", "dashed"))

# 指定線條的粗細
ggsurvplot(fit_1a, size = 3)

# 指定x軸與y軸區間
ggsurvplot(fit_1a, ylim = c(0.50, 1))

# 顯示number at risk，並指定table佔畫面的比例，在此設定為40%
ggsurvplot(fit_1a, risk.table = T, risk.table.height = 0.4)

# 調整圖例(legend)的位置，指定方向
ggsurvplot(fit_1a, pneumonia, legend = "right")

# 調整圖例(legend)的位置，指定座標
ggsurvplot(fit_1a, pneumonia, legend = c(0.8, 0.3))

# 調整圖例(legend)的標籤
ggsurvplot(fit_1a, pneumonia, legend = c(0.8, 0.3), legend.title = "Age group", legend.labs = c("<65", ">=65"))

# 依照變數分開畫圖
ggsurvplot_group_by(fit_1a, pneumonia, group = "htn")

# 依照變數分開畫圖且併在同一版面呈現
ggsurvplot_facet(fit_1a, pneumonia, facet.by = "htn")

# 依照變數分開畫圖且併在同一版面呈現
ggsurvplot_facet(fit_1a, pneumonia, facet.by = c("htn", "copd"))

# 累積發生事件
ggsurvplot(fit_1a, fun = "event")



# ~ 儲存圖片 ----

# 將圖片儲存為物件
my_surv_plot <- ggsurvplot(fit_1a, fun = "event")

# 指定輸出的框架
plot_output <- arrange_ggsurvplots(list(my_surv_plot), ncol = 1, nrow = 1, print = FALSE)

# 指定大小以及尺寸單位
ggsave("my_surv_plot.pdf", plot_output, width = 20, height = 12, units = "cm", dpi = 300)





### Cox PH regression model ----



# ~ 單變項模型分析 ----

# 建立物件fit_2a為觀察期間(re_adm_ft)與再次住院事件(re_adm)之模型，解釋變數為過去半年肺炎病史(pneumonia_pre)
fit_2a <- coxph(Surv(re_adm_ft, re_adm) ~ pneumonia_pre, data = pneumonia)

# 檢視模型摘要
summary(fit_2a)

# 建立物件fit_2b為觀察期間(death_ft)與死亡事件(death)之模型，解釋變數為過去半年肺炎病史(pneumonia_pre)
fit_2b <- coxph(Surv(death_ft, death) ~ pneumonia_pre, data = pneumonia)

# 檢視模型摘要
summary(fit_2b)



# ~ proportional-hazards assumption ----

# 使用Schoenfeld residuals作為檢定參考

# 以fit_2a進行PH assumption檢定
cox.zph(fit_2a)

# 以fit_2b進行PH assumption檢定
cox.zph(fit_2b)

# 使用survminer::ggcoxzph將結果視覺化

# 以fit_2a進行PH assumption檢定結果進行視覺化
ggcoxzph(cox.zph(fit_2a))

# 以fit_2b進行PH assumption檢定結果進行視覺化
ggcoxzph(cox.zph(fit_2b))



# ~ 模型診斷 ----

# survminer::ggcoxdiagnostics的dfbeta進行診斷
# 模擬每抽掉一個觀察值，對迴歸係數的影響

# 以fit_2a進行模型診斷
ggcoxdiagnostics(fit_2a, type = "dfbeta")

# 以fit_2b進行模型診斷
ggcoxdiagnostics(fit_2b, type = "dfbeta")



# ~ 多變項模型分析 ----

# 建立物件fit_3a為觀察期間(re_adm_ft)與再次住院事件(re_adm)之模型
# 解釋變數為年齡分組、性別、投保金額分組、醫院等級、住院天數、CCI、過去病史(pneumonia、HTN、DM、高血脂)
fit_3a <- coxph(Surv(re_adm_ft, re_adm) ~ agegp_1 + sex_1 + incgp_1 + incgp_2 + med_center_1 + bed_day + cci + pneumonia_pre + htn + dm + hyperlipidemia, data = pneumonia)

# 檢視模型摘要
summary(fit_3a)

# 建立物件fit_3b為觀察期間(death_ft)與死亡事件(death)之模型
# 解釋變數為年齡分組、性別、投保金額分組、醫院等級、住院天數、CCI、過去病史(pneumonia、HTN、DM、高血脂)
fit_3b <- coxph(Surv(death_ft, death) ~ agegp_1 + sex_1 + incgp_1 + incgp_2 + med_center_1 + bed_day + cci + pneumonia_pre + htn + dm + hyperlipidemia, data = pneumonia)

# 檢視模型摘要
summary(fit_3b)



# ~ 模型診斷 ----

# survminer::ggcoxdiagnostics的dfbeta進行診斷
# 模擬每抽掉一個觀察值，對迴歸係數的影響

# 以fit_3a進行模型診斷
ggcoxdiagnostics(fit_3a, type = "dfbeta")

# 以fit_3b進行模型診斷
ggcoxdiagnostics(fit_3b, type = "dfbeta")



# ~ 兩種模型的比較 ----

# 使用jtools::plot_summs進行迴歸係數的視覺化
# omit.coefs是可以指定忽略不呈現的變項名稱
plot_summs(
  fit_3a, fit_3b, 
  exp = TRUE,
  model.names = c("Model for re-admission", "Model for death"),
  omit.coefs = c("(Intercept)", "agegp_1", "sex_1", "incgp_1", "incgp_2"))

# model.names可以自己給定名稱，只會比較這些指定的變數
plot_summs(
  fit_3a, fit_3b, 
  exp = TRUE,
  coefs = c(
    "Treating in medical cneter" = "med_center_1",
    "Length of stay" = "bed_day",
    "CCI" = "cci",
    "Pneumonia history" = "pneumonia_pre",
    "HTN history" = "htn",
    "DM history" = "dm",
    "Hyperlipidemia history" = "hyperlipidemia"
    ),
  model.names = c("Model for re-admission", "Model for death"))





### 衍伸模型 ----



# ~ 分層模型 ----

# 如果考量治療院所(醫學中心 vs 非醫中)具有不同的基線特質(baseline)
fit_4a <- coxph(Surv(re_adm_ft, re_adm) ~ agegp_1 + sex_1 + incgp_1 + incgp_2 + strata(med_center_1) + bed_day + cci + pneumonia_pre + htn + dm + hyperlipidemia, data = pneumonia)

# 未分層
summary(fit_3a)

# 有分層
summary(fit_4a)



# ~ 競爭風險模型 ----

# 肺炎再次入院之前，若病患已經死亡，則不可能發生研究有興趣的觀察終點

# 設計competing event
pneumonia <- pneumonia[, crr_re_adm := re_adm]
pneumonia <- pneumonia[death == 1 & re_adm == 0, crr_re_adm := 2]

# 結果
table(pneumonia$crr_re_adm)

# 以年齡層來看發生不同觀察終點的事件數
table(pneumonia$crr_re_adm, pneumonia$agegp)

# 使用cmprsk::crr進行分析
fit_4b <- crr(pneumonia$re_adm_ft, pneumonia$crr_re_adm, pneumonia[, .(agegp_1, sex_1, incgp_1, incgp_2, med_center_1, bed_day, cci, pneumonia_pre, htn, dm, hyperlipidemia)])

summary(fit_4b)





### END ###
### Author: Peter Liu 劉品崧 ###
### Contact: psliu520@gmail.com ###