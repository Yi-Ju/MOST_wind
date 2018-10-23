# this script is for data clean of MOST 離岸風機 projects, because on the end of Oct. the data should be clean and put into database.
# start from Oct. 23
# mostly modify date, time, lat, long.

# 日期格式 yyyy-MM-dd，MM大寫是月份與小寫mm分鐘區隔，HH:mm:ss ，HH大寫表示24時制

library(data.table)
library(dplyr)
library(lubridate)
library(stringr)


# 1.6魚卵及仔稚魚生物量及群集組成 Oct. 23 ==========
# 07 MOST 離岸風機\資料整理-工作區\  子計畫1.6 新增一欄日期，格式 yyyy-MM-dd

data_1.6 <- fread('/Users/yiju/GoogleDrive/MOST_wind/1.6魚卵及仔稚魚生物量及群集組成_1810W.csv')
summary(data_1.6) # 採樣日期 NA's:934
which(is.na(data_1.6$採樣日期)) 

data_1.6_2 <- data_1.6[complete.cases(data_1.6[ , '採樣日期']),]
dim(data_1.6_2)
# V26, V27, V28 are whole blank/empty, so remove them
data_1.6_2 <- data_1.6_2[, -c(26, 27, 28)]

# data_1.6_2 is ready for cleaning.
# 新增一欄日期，格式 yyyy-MM-dd

plot(data_1.6_2$採樣日期)
data_1.6_2$採樣日期 <- as.character(data_1.6_2$採樣日期)

y <- substr(data_1.6_2$採樣日期, 1, 4)
M <- substr(data_1.6_2$採樣日期, 5, 6)
d <- substr(data_1.6_2$採樣日期, 7, 8)

data_1.6_2$Date <- paste(y, M, d, sep = '-')
# data_1.6_2$Date <- as.POSIXct.Date(data_1.6_2$Date)

fwrite(data_1.6_2, '/Users/yiju/GoogleDrive/MOST_wind/1.6魚卵及仔稚魚生物量及群集組成_1810W_2.csv')


write.table(data_1.6_2, '/Users/yiju/GoogleDrive/MOST_wind/1.6魚卵及仔稚魚生物量及群集組成_1810W_2.txt', sep="\t", row.names = F, col.names = T)


# 2.1白海豚努力量表_2017_W  Oct. 23 =========
# 2.1 各新增Latitude和Longitude欄位，改為小數點下十進位；新增Time欄位合併小時與分鐘為 HH:mm:ss


## 2.1 read in the data from 2017 ====
data_2.1_2017_0717 <- fread('/Users/yiju/GoogleDrive/MOST_wind/2.1白海豚努力量表_2017_W_0717.csv')
data_2.1_2017_0718 <- fread('/Users/yiju/GoogleDrive/MOST_wind/2.1白海豚努力量表_2017_W_0718.csv')
data_2.1_2017_0728 <- fread('/Users/yiju/GoogleDrive/MOST_wind/2.1白海豚努力量表_2017_W_0728.csv')
data_2.1_2017_0824 <- fread('/Users/yiju/GoogleDrive/MOST_wind/2.1白海豚努力量表_2017_W_0824.csv')
data_2.1_2017_0922 <- fread('/Users/yiju/GoogleDrive/MOST_wind/2.1白海豚努力量表_2017_W_0922.csv')
data_2.1_2017_0923 <- fread('/Users/yiju/GoogleDrive/MOST_wind/2.1白海豚努力量表_2017_W_0923.csv')
data_2.1_2017_0924 <- fread('/Users/yiju/GoogleDrive/MOST_wind/2.1白海豚努力量表_2017_W_0924.csv')

# data_2.1_2017_0718 has one more column than others, two EC columns(column2 and 3), decide to remove one of the ECs
# remove column 2
data_2.1_2017_0718 <- fread('/Users/yiju/GoogleDrive/MOST_wind/2.1白海豚努力量表_2017_W_0718.csv', drop = 2, col.names = colnames(data_2.1_2017_0717))
colnames(data_2.1_2017_0717) == colnames(data_2.1_2017_0718)

# remove useless/redundant column
data_2.1_2017_0717 <- data_2.1_2017_0717[complete.cases(data_2.1_2017_0717[ , 'id']),]
data_2.1_2017_0718 <- data_2.1_2017_0718[complete.cases(data_2.1_2017_0718[ , 'id']),]
data_2.1_2017_0728 <- data_2.1_2017_0728[complete.cases(data_2.1_2017_0728[ , 'id']),]
data_2.1_2017_0824 <- data_2.1_2017_0824[complete.cases(data_2.1_2017_0824[ , 'id']),]
data_2.1_2017_0922 <- data_2.1_2017_0922[complete.cases(data_2.1_2017_0922[ , 'id']),]
data_2.1_2017_0923 <- data_2.1_2017_0923[complete.cases(data_2.1_2017_0923[ , 'id']),]
data_2.1_2017_0924 <- data_2.1_2017_0924[complete.cases(data_2.1_2017_0924[ , 'id']),]


# combine above 7 dataframe into 1 big dataframe
data_2.1_2017_all <- rbind(data_2.1_2017_0717, data_2.1_2017_0718, data_2.1_2017_0728, data_2.1_2017_0824, data_2.1_2017_0922, data_2.1_2017_0923, data_2.1_2017_0924)


# plot(data_2.1_2017_all$id)
fwrite(data_2.1_2017_all, '/Users/yiju/GoogleDrive/MOST_wind/2.1白海豚努力量表_2017_W_all.csv')

# aa <- fread('/Users/yiju/GoogleDrive/MOST_wind/2.1白海豚努力量表_2017_W_all.csv')
# sum(aa == data_2.1_2017_all) #7316 = 236*31, they are totally the same



# clean Time "%b %d %H %M %S"
# 新增Time欄位合併小時與分鐘為 HH:mm:ss
data_2.1_2017_all$Time <- str_pad(data_2.1_2017_all$Time, width = 2, side = 'left', pad = '0')
data_2.1_2017_all$V6 <- str_pad(data_2.1_2017_all$V6, width = 2, side = 'left', pad = '0')

data_2.1_2017_all$Timing <- paste(data_2.1_2017_all$Time, data_2.1_2017_all$V6, '00', sep = ':')

# clean long, lat
# 各新增Latitude和Longitude欄位，改為小數點下十進位
# 24度 05.136分 (單位：度+分)
# 24+05.136/60 = 24.0856度

# =====================
# work flow :
# paste V8, V9 sep=.
# as.numeric
#/60
# +度 lat
# =====================
    
# Lat
mins <- as.numeric(paste(data_2.1_2017_all$V8, data_2.1_2017_all$V9, sep = '.'))
which(is.na(mins)) # 75th row (id:165) without Lat data
data_2.1_2017_all$Lat <- as.numeric(data_2.1_2017_all$`Latitude(N)`) + mins/60
summary(data_2.1_2017_all$Lat) # 1 NA


# Long
minss <- as.numeric(paste(data_2.1_2017_all$V11, data_2.1_2017_all$V12, sep = '.'))
which(is.na(minss)) # 75th row (id:165) without Long data
data_2.1_2017_all$Long <- as.numeric(data_2.1_2017_all$`Longitude(E)`) + minss/60
summary(data_2.1_2017_all$Long) # 1 NA


# output the result
fwrite(data_2.1_2017_all, '/Users/yiju/GoogleDrive/MOST_wind/2.1白海豚努力量表_2017_W_all_2.csv')








## 2.1 the data from 2018 =====
# 2.1 MOST_努力量表_1810W，有三個sheets，各新增Latitude和Longitude欄位，改為小數點下十進位；新增Time欄位合併小時與分鐘為 HH:mm:ss

# clean Time

# clean long, lat




