# this script is for data clean of MOST 離岸風機 projects, because on the end of Oct. the data should be clean and put into database.
# start from Oct. 23
# mostly modify date, time, lat, long.

# 日期格式 yyyy-MM-dd，MM大寫是月份與小寫mm分鐘區隔，HH:mm:ss ，HH大寫表示24時制

library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(measurements)

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

# ===================== #
# work flow :
# paste V8, V9 sep=.
# as.numeric
#/60
# +度 lat
# ===================== #
    
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


# finished at Oct. 23 17:45





## 2.1 the data from 2018 =====
# 各新增Latitude和Longitude欄位，改為小數點下十進位；新增Time欄位合併小時與分鐘為 HH:mm:ss

data_2.1_2018_0726 <- fread('/Users/yiju/GoogleDrive/MOST_wind/2.1 MOST_努力量表_1810W_0726.csv')
data_2.1_2018_0727 <- fread('/Users/yiju/GoogleDrive/MOST_wind/2.1 MOST_努力量表_1810W_0727.csv')
data_2.1_2018_0921 <- fread('/Users/yiju/GoogleDrive/MOST_wind/2.1 MOST_努力量表_1810W_0921.csv')

colnames(data_2.1_2018_0726) == colnames(data_2.1_2018_0727)

# remove useless/redundant column
data_2.1_2018_0726 <- data_2.1_2018_0726[complete.cases(data_2.1_2018_0726[ , 'id']),]
data_2.1_2018_0727 <- data_2.1_2018_0727[complete.cases(data_2.1_2018_0727[ , 'id']),]
data_2.1_2018_0921 <- data_2.1_2018_0921[complete.cases(data_2.1_2018_0921[ , 'id']),]


# combine above 3 dataframe into 1 big dataframe
data_2.1_2018_all <- rbind(data_2.1_2018_0726, data_2.1_2018_0727, data_2.1_2018_0921)


# plot(data_2.1_2018_all$id)
fwrite(data_2.1_2018_all, '/Users/yiju/GoogleDrive/MOST_wind/2.1 MOST_努力量表_1810W_all.csv')


# clean Time "%b %d %H %M %S"
# 新增Time欄位合併小時與分鐘為 HH:mm:ss
data_2.1_2018_all$Time <- str_pad(data_2.1_2018_all$Time, width = 2, side = 'left', pad = '0')
data_2.1_2018_all$V6 <- str_pad(data_2.1_2018_all$V6, width = 2, side = 'left', pad = '0')

data_2.1_2018_all$Timing <- paste(data_2.1_2018_all$Time, data_2.1_2018_all$V6, '00', sep = ':')

# clean long, lat
# 各新增Latitude和Longitude欄位，改為小數點下十進位
# 24度 05.149分 (單位：度+分)
# 24+05.149/60 = 24.08581667度

# ===================== #
# work flow :
# paste V8, V9 sep=.
# as.numeric
#/60
# +度 lat
# ===================== #

# Lat
mins_2018 <- as.numeric(paste(data_2.1_2018_all$V8, data_2.1_2018_all$V9, sep = '.'))
which(is.na(mins_2018)) # no NA
data_2.1_2018_all$Lat <- as.numeric(data_2.1_2018_all$`Latitude(N,84)`) + mins_2018/60
summary(data_2.1_2018_all$Lat) # no NA


# Long
minss_2018 <- as.numeric(paste(data_2.1_2018_all$V11, data_2.1_2018_all$V12, sep = '.'))
which(is.na(minss_2018)) # no NA
data_2.1_2018_all$Long <- as.numeric(data_2.1_2018_all$`Longitude(E,84)`) + minss_2018/60
summary(data_2.1_2018_all$Long) # no NA


# output the result
fwrite(data_2.1_2018_all, '/Users/yiju/GoogleDrive/MOST_wind/2.1 MOST_努力量表_1810W_all_2.csv')


# finished at Oct. 24 9:40AM



# 2.6離岸風電對鳥類生態的衝擊_1810_W ========
# 2.6先處理經緯度，退滿潮時間還在等回覆
# 這樣的日期和時間，資料庫可以讀，不用改
# Lat, Long
data_2.6 <- fread('/Users/yiju/GoogleDrive/MOST_wind/2.6離岸風電對鳥類生態的衝擊_1810_W.csv')
plot(data_2.6$航點編號) # 非連續


# clean Long, Lat

## 2.6 Lat =====

# 2405590代表北緯24度05.590分
# 24+5.59/60 = 24.09316667度
summary(data_2.6$`緯度(DDMMMMM)`)
plot(data_2.6$`緯度(DDMMMMM)`/10000)

## change the Lat formate to xx xx.xxx
# extract first 2 element of strings, 度
# insert 小數點 xx.xxx分

data_2.6$`緯度(DDMMMMM)` <- as.character(data_2.6$`緯度(DDMMMMM)`)

DD <- substr(data_2.6$`緯度(DDMMMMM)`, 1, 2)
MM <- substr(data_2.6$`緯度(DDMMMMM)`, 3, 4)
MMM <- substr(data_2.6$`緯度(DDMMMMM)`, 5, 7)

MM.MMM <- paste(MM, MMM, sep = '.')
Deg_dec_Min <- paste(DD, MM.MMM, sep = ' ')

data_2.6$Lat <- conv_unit(Deg_dec_Min, from = 'deg_dec_min', to = 'dec_deg')

# 2.6 Long =====

# 12018590代表東經120度18.590分
# 120+18.59/60 = 120.3098333度
summary(data_2.6$`經度(DDMMMMM)`)
plot(data_2.6$`經度(DDMMMMM)`/10000)

# one column mistyping
which.min(data_2.6$`經度(DDMMMMM)`) # 113: 1201507
data_2.6$New經度 <- data_2.6$`經度(DDMMMMM)`
data_2.6[113, 'New經度'] <- 12015070

# use this new column(New經度) to deal with Long
summary(data_2.6$New經度)
plot(data_2.6$New經度/10000)



## change the Long formate to xxx xx.xxx
# extract first 3 element of strings, 度
# insert 小數點 xx.xxx分

data_2.6$New經度 <- as.character(data_2.6$New經度)

DDD2 <- substr(data_2.6$New經度, 1, 3)
MM2 <- substr(data_2.6$New經度, 4, 5)
MMM2 <- substr(data_2.6$New經度, 6, 8)

MM.MMM2 <- paste(MM2, MMM2, sep = '.')
Deg_dec_Min2 <- paste(DDD2, MM.MMM2, sep = ' ')

data_2.6$Long <- conv_unit(Deg_dec_Min2, from = 'deg_dec_min', to = 'dec_deg')

fwrite(data_2.6, '/Users/yiju/GoogleDrive/MOST_wind/2.6離岸風電對鳥類生態的衝擊_1810_W_2.csv')


# 3.2底棲魚類多樣性及其時空變化調查研究_1810_W ======
# 四個經緯度欄位，日期合併，單欄小時轉HH:mm
data_3.2 <- fread('/Users/yiju/GoogleDrive/MOST_wind/3.2底棲魚類多樣性及其時空變化調查研究_1810_W.csv')
summary(data_3.2)

# 日期合併為yyyy-MM-dd 
data_3.2$月 <- str_pad(data_3.2$月, width = 2, side = 'left', pad = '0')
data_3.2$日 <- str_pad(data_3.2$日, width = 2, side = 'left', pad = '0')

data_3.2$Date <- paste(data_3.2$年, data_3.2$月, data_3.2$日, sep = '-')

# 單欄小時轉HH:mm:ss
data_3.2$時 <- str_pad(data_3.2$時, width = 2, side = 'left', pad = '0')

data_3.2$Time <- paste(data_3.2$時,'00', '00' ,sep = ':')

# 四個經緯度欄位
# 下網點Lat
DD3 <- substr(data_3.2$下網點緯度, 2, 3)
MM.MMM3 <- substr(data_3.2$下網點緯度, 5, 10)

Deg_dec_Min3 <- paste(DD3, MM.MMM3, sep = ' ')

data_3.2$下網點Lat <- conv_unit(Deg_dec_Min3, from = 'deg_dec_min', to = 'dec_deg')

# 下網點Long
DD4 <- substr(data_3.2$下網點經度, 2, 4)
MM.MMM4 <- substr(data_3.2$下網點經度, 6, 11)

Deg_dec_Min4 <- paste(DD4, MM.MMM4, sep = ' ')

data_3.2$下網點Long <- conv_unit(Deg_dec_Min4, from = 'deg_dec_min', to = 'dec_deg')



# 起網點Lat
DD5 <- substr(data_3.2$起網點緯度, 2, 3)
MM.MMM5 <- substr(data_3.2$起網點緯度, 5, 10)

Deg_dec_Min5 <- paste(DD5, MM.MMM5, sep = ' ')

data_3.2$起網點Lat <- conv_unit(Deg_dec_Min5, from = 'deg_dec_min', to = 'dec_deg')

# 起網點Long
DD6 <- substr(data_3.2$起網點經度, 2, 4)
MM.MMM6 <- substr(data_3.2$起網點經度, 6, 11)

Deg_dec_Min6 <- paste(DD6, MM.MMM6, sep = ' ')

data_3.2$起網點Long <- conv_unit(Deg_dec_Min6, from = 'deg_dec_min', to = 'dec_deg')



fwrite(data_3.2, '/Users/yiju/GoogleDrive/MOST_wind/3.2底棲魚類多樣性及其時空變化調查研究_1810_W_2.csv')


# finished on Oct. 24 14:20


# 3.3 工作魚礁列表_1810_W ============
# 已經先手動處理<公式、計算過程> 和 <計算結果> 這兩欄
# (1)調查時間→yyyy-MM-dd；(2)GPS→插兩欄，度以下小數點十進位


data_3.3 <- fread('/Users/yiju/GoogleDrive/MOST_wind/3.3 工作魚礁列表_1810_W_1.csv')

summary(data_3.3)

## clean Date (1)調查時間→yyyy-MM-dd

# capture the first 4 characters as a group ((.{4}), year) , then following 2 characters as a group ((.{2}), month), finally followed by one or more characters in another capture group ((.*), day) and then replace with the backreference of first group (\\1) followed by a - followed by second backreference (\\2).


data_3.3$Date <- sub("(.{4})(.{2})(.*)", "\\1-\\2-\\3", data_3.3$調查時間)


# (2)GPS→插兩欄，度以下小數點十進位


Lat_Long <- str_split_fixed(data_3.3$GPS, ' ; ', 2)

data_3.3_2 <- cbind(data_3.3, Lat_Long)
colnames(data_3.3_2)[18] <- 'Lat'
colnames(data_3.3_2)[19] <- 'Long'

# Lat
data_3.3_2$Lat <- str_replace(data_3.3_2$Lat, "°", " ")

data_3.3_2$Lat <- sub("'N$", "", data_3.3_2$Lat)
# If you have a string vector and want to replace the last 'N character from it you can use sub. $ here ensures that the 'N is the last character in your vector.

data_3.3_2$Lat <- conv_unit(data_3.3_2$Lat, from = 'deg_dec_min', to = 'dec_deg')



# Long
data_3.3_2$Long <- str_replace(data_3.3_2$Long, "°", " ")

data_3.3_2$Long <- sub("'E$", "", data_3.3_2$Long)
# If you have a string vector and want to replace the last 'E character from it you can use sub. $ here ensures that the 'E is the last character in your vector.

data_3.3_2$Long <- conv_unit(data_3.3_2$Long, from = 'deg_dec_min', to = 'dec_deg')



fwrite(data_3.3_2, '/Users/yiju/GoogleDrive/MOST_wind/3.3 工作魚礁列表_1810_W_2.csv')



### 3.1 底棲無脊椎生物17-18_1810_W ============
# 經緯度和時間 

data_3.1 <- fread('/Users/yiju/GoogleDrive/MOST_wind/3.1底棲無脊椎生物17-18_1810_W.csv', na.strings = c(''))

summary(data_3.1)



# Date and Time
data_3.1$年 <- as.character(data_3.1$年)
data_3.1$月 <- as.character(data_3.1$月)
data_3.1$日 <- as.character(data_3.1$日)
data_3.1$時 <- as.character(data_3.1$時)

data_3.1$月 <- str_pad(data_3.1$月, width = 2, side = 'left', pad = '0')
data_3.1$日 <- str_pad(data_3.1$日, width = 2, side = 'left', pad = '0')
data_3.1$時 <- str_pad(data_3.1$時, width = 2, side = 'left', pad = '0')


data_3.1$Date <- paste(data_3.1$年, data_3.1$月, data_3.1$日, sep = '-')
data_3.1$Time <- paste(data_3.1$時,'00', '00' ,sep = ':')


# 4 lat and long
data_3.1$下網點緯度 <- as.character(data_3.1$下網點緯度)
data_3.1$下網點經度 <- as.character(data_3.1$下網點經度)
data_3.1$起網點緯度 <- as.character(data_3.1$起網點緯度)
data_3.1$起網點經度 <- as.character(data_3.1$起網點經度)



# 下網點Lat
DD7 <- substr(data_3.1$下網點緯度, 2, 3)
MM.MMM7 <- substr(data_3.1$下網點緯度, 5, 10)

Deg_dec_Min7 <- paste(DD7, MM.MMM7, sep = ' ')

data_3.1$下網點Lat <- conv_unit(Deg_dec_Min7, from = 'deg_dec_min', to = 'dec_deg')


# 下網點Long
DD8 <- substr(data_3.1$下網點經度, 2, 4)
MM.MMM8 <- substr(data_3.1$下網點經度, 6, 11)

Deg_dec_Min8 <- paste(DD8, MM.MMM8, sep = ' ')

data_3.1$下網點Long <- conv_unit(Deg_dec_Min8, from = 'deg_dec_min', to = 'dec_deg')



# 起網點Lat
DD9 <- substr(data_3.1$起網點緯度, 2, 3)
MM.MMM9 <- substr(data_3.1$起網點緯度, 5, 10)

Deg_dec_Min9 <- paste(DD9, MM.MMM9, sep = ' ')

data_3.1$起網點Lat <- conv_unit(Deg_dec_Min9, from = 'deg_dec_min', to = 'dec_deg')

# 起網點Long
DD10 <- substr(data_3.1$起網點經度, 2, 4)
MM.MMM10 <- substr(data_3.1$起網點經度, 6, 11)

Deg_dec_Min10 <- paste(DD10, MM.MMM10, sep = ' ')

data_3.1$起網點Long <- conv_unit(Deg_dec_Min10, from = 'deg_dec_min', to = 'dec_deg')



fwrite(data_3.1, '/Users/yiju/GoogleDrive/MOST_wind/3.1底棲無脊椎生物17-18_1810_W_2.csv')


# finished on Oct. 30 16:18













