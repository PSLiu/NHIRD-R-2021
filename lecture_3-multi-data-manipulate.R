### 110年衛生福利部統計處R軟體推廣課程 ###
### 進階篇：多重資料操作 @ 線上課程 ###



### 初始化 ----



# ~ 資料匯入與套件載入 ----

# 下載
pack_need <- c("data.table", "ggplot2")
install.packages(pack_need, dependencies = TRUE)

# library
library(data.table) # 資料操作套件
library(ggplot2) # 繪圖套件

# 路徑物件
file <- "C:/Users/liu/Downloads/data"
full <- "C:/Users/liu/Downloads/data/full"
demo <- "C:/Users/liu/Downloads/data/demo"





### 使用for迴圈 ----



# ~ 基礎架構 ----

# for迴圈可以把向量物件元素逐一的代入迴圈當中進行執行

# 先建立一個文字向量，從英文大寫字母中抽取10個所組成
# 並且希望逐一列印內容
a1 <- sample(x = LETTERS, size = 10)

# 手工方法
print(a1[1])
print(a1[2])
print(a1[3])
print(a1[4])
print(a1[5])
print(a1[6])
print(a1[7])
print(a1[8])
print(a1[9])
print(a1[10])

# 觀念：手工方法是一個必要的過程
# 你要先知道核心的任務/動作是甚麼
# 核心對了，迴圈的產出才會是有用的
# 先血一個知道對就好
# 接下來教各位如何寫迴圈

# 迴圈方法
# for迴圈的基本架構
# for (variable in vector) {
#   some action
# }
# vector的內容會逐一的被放入迴圈當中
# 在迴圈當中，這個放入內容的代號就是variable
# 在下面這個例子，variable的代號叫做i
# 放入的vector是剛剛所建立的a1物件
# 所有的動作會被包含在大括號{}裡面
for (i in a1) {
  print(i)
}

# 迴圈方法 - 另外一種形式
# 把索引值當作vector而逐一代入迴圈當中取a1的index也是一種方法
for (i in 1:10) {
  print(a1[i])
}

# 迴圈方法 - 另外一種形式加強版
# 如果這個迴圈常常會用到
# 但是vector的長度有時會增減
# 可以寫length(a1)去取a1的長度
# 就不用一直手動修改
for (i in 1:length(a1)) {
  print(a1[i])
}



# ~ 複雜動作 ----

# 迴圈當中不是只有print這樣簡單的指令可以執行
# 任何事情都可以做

# i可以重複被使用
for (i in 1:length(a1)) {
  print(paste0("第", i, "個抽出來的字母是", a1[i]))
}

# 指令也可以用在多行指令
for (i in 1:length(a1)) {
  print(paste0("第", i, "個抽出來的字母是"))
  print(a1[i])
}

# 當指令一複雜的時候，我們可能需要看到運算時間
# 進行50次迴圈
# 每次迴圈都從英文大寫字母中抽取300000個
# 看看數量最大的字母是誰
for (i in c(1:50)) {
  
  # time on，將時間紀錄為物件
  time_on <- Sys.time()
  
  # 抽樣
  temp <- sample(x = LETTERS, size = 3E6, replace = T)
  
  # 誰最大
  result <- names(sort(table(temp))[1])
  
  # time off，將時間紀錄為物件
  time_off <- Sys.time()
  
  # 計算時間差距
  time_diff <- time_off - time_on
  
  # 列印結果
  print(paste("Iteration", i, "Use", time_diff, "Most char is", result))
}

# 過程產物也會變成物件
# 適時可以清除
rm(time_on, time_off, time_diff)
rm(temp)
rm(result)
rm(a1)
rm(i)



# ~ 多層迴圈 ----

# 迴圈可以用多層次的方式執行
for (i in c(1:3)) {
  for (j in c("A", "B", "C")) {
    print(paste(i, j))
  }
}

# 迴圈可以用多層次的方式執行
for (i in c(2015:2018)) {
  for (j in c(1:12)) {
    print(paste("讀取", i, "年", j, "月西醫門診費用檔"))
  }
}

# 迴圈也可以多個迴圈包在同一個迴圈當中
for (i in c(1:3)) {
  # 第一個迴圈
  for (j in c("A", "B", "C")) {
    print(paste(i, j))
  }
  # 第二個迴圈
  for (j in c("x", "y", "z")) {
    print(paste(i, j))
  }
}

rm(i)
rm(j)



# ~ 練習時間 10 min ----

# 使用迴圈的寫法列印出以下文字
# 今年農曆正月1日是2021-2-12，星期五
# 今年農曆正月2日是2021-2-13，星期六
# 今年農曆正月3日是2021-2-14，星期日
# ...
# 今年農曆正月15日是2021-2-26，星期五

# 練習看看

# 老師範例
library(lubridate)

for (i in c(1:15)) {
  eve <- ymd(20210211)
  showdate <- eve + i
  showweek <- weekdays(showdate)
  print(paste0("今年農曆正月", i, "日是", showdate, "，", showweek))
}


# 清理物件
rm(i)
rm(eve, showdate, showweek)





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



# ~ 承保檔 ----

# 檔案路徑
setwd(demo)

# 住院費用檔
id <- fread("id.csv", colClasses = "character")
View(id)

# 整理記憶體
rm(id)





### 清單 ----

# 前面的範例比較單純
# 最終只是把結果print出來
# 但是這樣無法讓未來繼續使用
# 清單就是一個解決方案



# ~ 清單元素的特性 ----

# 可以容納多種不同屬性的物件
teacher <- list(
  last_name = "Liu",
  lecture_date = ymd(20210819),
  student_number = 40
)

# 元素個數
length(teacher)

# 不同元素可以有不同屬性
class(teacher$last_name)
class(teacher$lecture_date)
class(teacher$student_number)

# 也可以用[[數字]]做為索引
class(teacher[[1]])
class(teacher[[2]])
class(teacher[[3]])

# 如果清單裡面全部都是data.table
draw <- list(
  data.table(pick = sample(letters, 50, replace = T)),
  data.table(pick = sample(letters, 50, replace = T)),
  data.table(pick = sample(letters, 50, replace = T)),
  data.table(pick = sample(letters, 50, replace = T)),
  data.table(pick = sample(letters, 50, replace = T))
)

# 可以使用rbindlist()將元素一起堆疊起來
draw_rbind <- rbindlist(draw)

# 整理物件
rm(draw)
rm(draw_rbind)
rm(teacher)



# ~ 基礎應用 ----

# 找出衛福資料庫裡面的承保檔
# 作為input的元素來源
setwd(full)
insfile <- list.files(pattern = "^h_nhi_enrol")

# 創造list作為output物件的置放空間
insnumber <- vector(mode = "list", length = length(insfile))

# 使用迴圈執行，查詢每個月份的承保人數
for (i in 1:length(insfile)) {
  
  # 讀取檔案
  temp <- fread(insfile[[i]])
  
  # 中間處理
  result <- nrow(temp)
  
  # 將資料筆數回傳
  insnumber[[i]] <- result
  
  # 整理記憶體，避免卡住下一個檔案的讀取
  rm(temp, result)
}

# 將output做點處理
insnumber_out <- data.table(
  month = c(1:12),
  people = unlist(insnumber)
)

# 畫圖
ggplot(data = insnumber_out, aes(x = month, y = people)) + geom_line() + scale_y_continuous(limits = c(95000, 100000))

# 整理記憶體
rm(i)
rm(insfile)
rm(insnumber)
rm(insnumber_out)




# ~ 進階應用 ----

# 首先使用setwd將路徑設定在我們原始檔案的位置
setwd(full)

# 使用list.files函數抓取資料夾的檔案名稱成為清單
# 加上full.names = T是因為需要完整路徑
datalist_all <- list.files(full.names = T)

print(datalist_all)

# 擷取部分，檔名符合IPDTE檔案命名的西醫門診檔
datalist_cd <- grep("opdte[0-9]+_10.csv", datalist_all, value = T)

# 手動建立清單，長度與input相同
# 為了儲存output
outlist_cd <- vector("list", length(datalist_cd))

# 迴圈執行
for (i in c(1:length(datalist_cd))){
  # 讀取資料
  temp <- fread(datalist_cd[[i]])
  
  # 擷取第一診斷碼為糖尿病的人
  temp <- temp[grep("^250", ICD9CM_1)]
  
  # 依照ID歸人
  result <- unique(temp[, .(ID)])
  
  # 輸出資訊
  outlist_cd[[i]] <- nrow(result)
  
  # 整理記憶體
  rm(temp, result)
}

# 將output做點處理
dmtrend <- data.table(
  month = c(1:12),
  people = unlist(outlist_cd)
)

# 糖尿病人數在各月份的趨勢
ggplot(data = dmtrend, aes(x = month, y = people)) + geom_line() + scale_y_continuous(limits = c(2500, 3500))

# 整理
rm(i)
rm(datalist_all)
rm(datalist_cd)
rm(outlist_cd)
rm(dmtrend)



# ~ 練習時間 10min ----

# 分析每月的DM病人性別比例
# 繪製堆疊長條圖表示

# 練習看看

# 首先使用setwd將路徑設定在我們原始檔案的位置
setwd(full)

# 使用list.files函數抓取資料夾的檔案名稱成為清單
datalist_all <- list.files()

# 擷取部分，檔名符合IPDTE檔案命名的西醫門診檔
datalist_cd <- grep("opdte[0-9]+_10.csv", datalist_all, value = T)

# 手動建立清單，長度與input相同
# 為了儲存output
outlist_cd <- vector("list", length(datalist_cd))

# 迴圈執行
for (i in c(1:length(datalist_cd))){
  # 讀取資料
  temp <- fread(datalist_cd[[i]])
  
  # 擷取第一診斷碼為糖尿病的人
  temp <- temp[grep("^250", ICD9CM_1)]
  
  # 保留ID和SEX欄位
  temp <- temp[, .(ID, ID_S, FEE_YM)][ID_S %in% c(1, 2)]
  
  # 依照ID歸人
  result <- unique(temp[, .(ID, ID_S, month = as.character(FEE_YM))])

  # 輸出資訊
  outlist_cd[[i]] <- result
  
  # 整理記憶體
  rm(temp, result)
}

# 預檢
head(outlist_cd[[1]])
head(outlist_cd[[12]])

# 堆疊
outdata <- rbindlist(outlist_cd)

# 糖尿病人數在各月份的趨勢
ggplot(data = outdata, aes(x = month, fill = factor(ID_S))) + 
  geom_bar(position = "stack") +
  labs(x = "Year month", y = "No. of DM patients") +
  scale_fill_discrete(name = "Sex", labels = c("Male", "Female"))

# 整理
rm(i)
rm(datalist_all)
rm(datalist_cd)
rm(outlist_cd)
rm(outdata)





### 函數 ----



# ~ 自訂函數 ----

# 既有函數不敷使用的時候
# 自訂函數是發揮自己創造力的時候
# 可以建立較複雜流程

# name <- function(variables) {
#   
# }

# 制定一個函數，計算input的3次方後再開跟號的結果
my_func_1 <- function(x){
  y <- sqrt(x**3)
  return(y)
}



# ~ 使用函數 ----

# 給數值元素
my_func_1(3)

# 給數值向量
my_input_1 <- c(10:27)
my_func_1(my_input_1)



# 整理
rm(my_input_1)
rm(my_func_1)





### 函數搭配lapply ----

# 我們來把剛才找糖尿病人性別的迴圈修改看看



# ~ 建立清單 ----

# 工作目錄
setwd(full)

# 西醫門診清單
datalist_all <- list.files()
datalist_cd <- grep("opdte[0-9]+_10.csv", datalist_all, value = T)



# ~ 定義函數 ----

# 函數名稱可以自行定義
find_dm <- function(m){
  
  # 讀取資料
  temp <- fread(m)
  
  # 擷取第一診斷碼為糖尿病的人
  temp <- temp[grep("^250", ICD9CM_1)]
  
  # 保留ID和SEX欄位
  temp <- temp[, .(ID, ID_S, FEE_YM)][ID_S %in% c(1, 2)]
  
  # 依照ID歸人
  result <- unique(temp[, .(ID, ID_S, month = as.character(FEE_YM))])
  
  # 輸出資訊
  return(result)
}

# 如果只放入一個INPUT，可以直接放進function裡面
out_dm <- find_dm(datalist_cd[[1]])

table(out_dm$ID_S)



# ~ lapply ----

# 用於把整個list的每一個元素都應用同一個function
# 回傳成list的形式

# 在此，原始的list就是我們的檔案路徑
# 輸入function當中去應用

# lapply(list, function)

outlist_cd <- lapply(datalist_cd, find_dm)



# ~ 整理結果 ----

# 堆疊
outdata <- rbindlist(outlist_cd)

# 糖尿病人數在各月份的趨勢
ggplot(data = outdata, aes(x = month, fill = factor(ID_S))) + 
  geom_bar(position = "stack") +
  labs(x = "Year month", y = "No. of DM patients") +
  scale_fill_discrete(name = "Sex", labels = c("Male", "Female"))

# 整理
rm(datalist_all)
rm(datalist_cd)
rm(outlist_cd)
rm(outdata)
rm(out_dm)
rm(find_dm)





### END ###
### Author: Peter Liu 劉品崧 ###
### Contact: psliu520@gmail.com ###