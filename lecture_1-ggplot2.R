### 110年衛生福利部統計處R軟體推廣課程 ###
### 進階篇：GGPLOT2 2021.05.07(Fri) @ 慈濟大學 ###

### 初始化 ----



# ~ 資料匯入與套件載入 ----

# 下載
pack_need <- c("ggplot2", "fst", "data.table", "dplyr")

for (i in 1:length(pack_need)) {
  install.packages(pack_need[i], dependencies = TRUE)
}
rm(i)
rm(pack_need)

# library
library(ggplot2) # 今天的主角
library(fst) # 讀取範例資料.fst格式檔案
library(data.table) # 資料操作套件
library(dplyr) # 時常與ggplot2進行chaining使用

# 資料來源路徑
setwd("C:/Users/liu/Downloads/data")

# 讀取範例資料
dmmpr <- fread("dmmpr.csv")



# ~ 資料檢視 ----

# 整體摘要性統計
summary(dmmpr)

# 前20筆資料
head(dmmpr, 20)

# 直接檢視(不建議使用於大型資料)
View(dmmpr)

# 數值型資料描述性統計分析
summary(dmmpr$age)

summary(dmmpr$cci)

summary(dmmpr$mpr)

# 類別型資料描述性統計分析
table(dmmpr$sex) # 1為男性，2為女性

table(dmmpr$agegp) # 0為65歲以下，1為65歲(含)以上

table(dmmpr$mprgp) # 0為mpr<80%，1為mpr>80%





### 圖層概念 ----



# ~ 底圖與外型 ----
ggplot(data = dmmpr)



# ~ 資料內容 ----

# 可以將基礎架構儲存為物件，之後用"+"連接其他功能與外觀
plot_base <- ggplot(data = dmmpr, aes(age, mpr))



# ~ 圖形外觀 ----

# 形狀
plot_base +
  geom_point()

# 顏色
plot_base +
  geom_point(aes(color = factor(sex)))

# 大小，直接指定
plot_base +
  geom_point(aes(size = 10))

# 大小，依照其他變數(呈現第三維度)
plot_base +
  geom_point(aes(size = cci))

# 背景主題
plot_base +
  geom_point() +
  theme_classic()



# ~ 輔助線 ----

# 水平線，請填入y軸截距為80
plot_base +
  geom_point() + 
  geom_hline(aes(yintercept = 80))

# 垂直線，請填入x軸截距為50
plot_base +
  geom_point() + 
  geom_vline(aes(xintercept = 50))

# 斜直線，請填入y軸截距為-3，斜率為0.1
plot_base +
  geom_point() + 
  geom_abline(
    aes(intercept = -3, slope = 0.1))

# 搭配回歸線
fit_result <- coef(lm(mpr ~ age, data = dmmpr))
plot_base +
  geom_point() + 
  geom_abline(
    aes(
      intercept = fit_result["(Intercept)"], 
      slope = fit_result["age"])
  )



# ~ 儲存圖片 ----

# 決定好最後要儲存的圖片，建議先指定為物件
fit_result <- coef(lm(mpr ~ age, data = dmmpr))
my_paper_plot <- plot_base +
  geom_point() + 
  geom_abline(
    aes(
      intercept = fit_result["(Intercept)"], 
      slope = fit_result["age"])) +
  theme_classic()

# 先show出來看看
my_paper_plot

# ggsave()指令預設是儲存為.png檔
ggsave("myplot_format_1.png", my_paper_plot)

# 改為tiff檔案
ggsave("myplot_format_2.tiff", my_paper_plot)

# 指定大小以及尺寸單位，請填入寬度20，高度12，單位為"cm"(文字要用雙引號包含)
ggsave("myplot_format_3.tiff", my_paper_plot, width = 20, height = 12, units = "cm", dpi = 300)





### 常用統計圖形 ----



# ~ 單一數值變數：直方圖(histogram) ----

# 用於統計連續變數的分組計數
dmmpr %>% 
  ggplot(aes(age)) + 
  geom_histogram()

# binwidth: 每組的區間要多大，請填入15
dmmpr %>% 
  ggplot(aes(age)) + 
  geom_histogram(binwidth = 15)

# bins: 總共要分幾組，請填入12
dmmpr %>% 
  ggplot(aes(age)) + 
  geom_histogram(bins = 12)

# breaks: 自訂分組區間
dmmpr %>% 
  ggplot(aes(age)) +
  stat_bin(breaks = seq(0, 100, by = 10))

# 稍微變化一下
dmmpr %>% 
  ggplot(aes(age)) +
  stat_bin(breaks = seq(0, 100, by = 10), fill = "navy", color = "black") +
  stat_bin(
    aes(label = ..count..), breaks = seq(0, 100, by = 10), 
    geom = "text", color = "red", vjust = -1.5, size = 5) +
  ylim(0, 40) +
  theme_classic()



# ~ 單一類別變數：長條圖(bar) ----

# 用於統計類別變數的計數
dmmpr %>% 
  ggplot(aes(incgp)) + 
  geom_bar()

# 分組長條圖
dmmpr %>% 
  ggplot(aes(incgp, fill = factor(sex))) + 
  geom_bar(position = "dodge")

# 堆疊長條圖(次數)
dmmpr %>% 
  ggplot(aes(incgp, fill = factor(sex))) + 
  geom_bar(position = "stack")

# 堆疊長條圖(百分比)
dmmpr %>% 
  ggplot(aes(incgp, fill = factor(sex))) + 
  geom_bar(position = "fill")

# 補充：圓餅圖，用於觀察分布狀況
as.data.table(table(dmmpr$incgp)) %>%
  ggplot(aes(x = "", y = N, fill = V1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)


# ~ 兩個數值變數：散佈圖 ----

# 用於觀察兩個連續變數之間的分布狀況與相關性
dmmpr %>%
  ggplot(aes(age, cci)) +
  geom_point()

# 可以加上顏色看出集中趨勢
dmmpr %>%
  ggplot(aes(age, cci, color = factor(htn))) +
  geom_point()



# ~ y數值x類別：盒鬚圖 ----

# 描繪連續變數在各組當中的分布情形
dmmpr %>%
  ggplot(aes(factor(incgp), age)) +
  geom_boxplot()

# 加上顏色方便區分
dmmpr %>%
  ggplot(aes(factor(incgp), age, fill = factor(incgp))) +
  geom_boxplot()

# 或是想要比較分組
dmmpr %>%
  ggplot(aes(factor(incgp), age, fill = factor(sex))) +
  geom_boxplot()

# 加上明顯區別的圖例(legend)
dmmpr %>%
  ggplot(aes(factor(incgp), age, fill = factor(incgp))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Insurance amount", labels = c("< NTD 15840", "NTD 15840 - 30000", "> NTD 30000"))





### 自訂圖形外觀 ----



# ~ 基礎圖形 ----

# 糖尿病患的
plot_base_1 <- dmmpr %>% ggplot(aes(age, cci)) + geom_point()

# show
plot_base_1



# ~ x軸與y軸的單位調整 ----

# 限制(limits)，請填入c(20, 40)
plot_base_1 + scale_x_continuous(limits = c(20, 40))

# 展延(expand)，請填入c(-2, 20)
plot_base_1 + scale_y_continuous(expand = c(-2, 20))

# 反向
plot_base_1 + scale_x_reverse()

# 自訂間隔區間
plot_base_1 + scale_x_continuous(breaks = seq(from = 0, to = 100, by = 25))



# ~ label ----

# 主標題、次標題、註解
plot_base_1 + 
  labs(
    title = "Plot of Age vs CCI", 
    subtitle = "DM sample from NHIRD", 
    caption = "my first ggplot"
  ) +
  annotate(geom = "text", x = 80, y = 6, label = "Peter Liu")

# x軸與Y軸
plot_base_1 + 
  labs(
    x = "Age in index month",
    y = "CCI score"
  )



# ~ 多圖並排 ----

# 橫向
plot_base_1 + facet_grid(cols = vars(incgp))

# 縱向
plot_base_1 + facet_grid(rows = vars(mprgp))

# 矩形
plot_base_1 + facet_grid(cols = vars(incgp), rows = vars(mprgp))



# ~ 佈景主題(theme)文字外觀 ----

# 基礎圖形
plot_base_2 <- dmmpr %>% 
  ggplot(aes(cci, mpr)) +
  geom_point()

# show
plot_base_2

# 文字大小
plot_base_2 + 
  facet_grid(cols = vars(incgp), rows = vars(sex)) +
  labs(
    title = "Plot of Age vs CCI", 
    subtitle = "DM sample from NHIRD", 
    caption = "my first ggplot",
    x = "Age in index month",
    y = "CCI score"
  ) +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 6),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    strip.text.y = element_text(size = 10),
    strip.text.x = element_text(size = 10)
  )

# 文字顏色
plot_base_2 + 
  facet_grid(cols = vars(incgp), rows = vars(sex)) +
  labs(
    title = "Plot of Age vs CCI", 
    subtitle = "DM sample from NHIRD", 
    caption = "my first ggplot",
    x = "Age in index month",
    y = "CCI score"
  ) +
  theme(
    plot.title = element_text(color = "blue"),
    plot.subtitle = element_text(color = "lightcoral"),
    plot.caption = element_text(color = "maroon"),
    axis.title.y = element_text(color = "brown"),
    axis.title.x = element_text(color = "darkviolet"),
    axis.text.y = element_text(color = "orange"),
    axis.text.x = element_text(color = "navy"),
    strip.text.y = element_text(color = "red"),
    strip.text.x = element_text(color = "blue")
  )

# 字體
plot_base_2 + 
  facet_grid(cols = vars(incgp), rows = vars(sex)) +
  labs(
    title = "Plot of Age vs CCI", 
    subtitle = "DM sample from NHIRD", 
    caption = "my first ggplot",
    x = "Age in index month",
    y = "CCI score"
  ) +
  theme(
    plot.title = element_text(family = "serif"),
    plot.subtitle = element_text(family = "serif"),
    plot.caption = element_text(family = "serif"),
    axis.title.y = element_text(family = "sans"),
    axis.title.x = element_text(family = "sans"),
    axis.text.y = element_text(family = "sans"),
    axis.text.x = element_text(family = "sans"),
    strip.text.y = element_text(family = "mono"),
    strip.text.x = element_text(family = "mono")
  )

# 字型
plot_base_2 + 
  facet_grid(cols = vars(incgp), rows = vars(sex)) +
  labs(
    title = "Plot of Age vs CCI", 
    subtitle = "DM sample from NHIRD", 
    caption = "my first ggplot",
    x = "Age in index month",
    y = "CCI score"
  ) +
  theme(
    plot.title = element_text(face = "italic"),
    plot.subtitle = element_text(face = "italic"),
    plot.caption = element_text(face = "italic"),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "plain"),
    axis.text.x = element_text(face = "plain"),
    strip.text.y = element_text(face = "bold.italic"),
    strip.text.x = element_text(face = "bold.italic")
  )

# 位置
plot_base_2 + 
  facet_grid(cols = vars(incgp), rows = vars(sex)) +
  labs(
    title = "Plot of Age vs CCI", 
    subtitle = "DM sample from NHIRD", 
    caption = "my first ggplot",
    x = "Age in index month",
    y = "CCI score"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle  = element_text(hjust = 0.5),
    axis.title.y = element_text(vjust = 2),
    axis.title.x = element_text(vjust = -2),
    axis.text.y = element_text(angle = 30),
    axis.text.x = element_text(angle = 30),
    strip.text.y = element_text(angle = 0),
    strip.text.x = element_text()
  )





### END ###
### Author: Peter Liu 劉品崧 ###
### Contact: psliu520@gmail.com ###