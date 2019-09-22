###################################################################
# 地価公示データの基礎Rデータに、同一標準地に関するid列を追加する
# 地価公示データの基礎Rデータに、都道府県名、市町村名の列をつける
# 2019/9/22 syunsuke.fukuda@gmail.com
###################################################################

library(tidyverse)

###################################################################
# 同一標準地に関するid列の追加
###################################################################

# ルーチンスクリプトの読み込み
source("code/add_std_id_tool.R", encoding = "UTF-8")

# 地価公示基礎データの準備（kouji_data）
load("rdata/kouji_data.rda")

# データ内のもっとも古いものと新しいものの年次を確認
oldest_year <- min(kouji_data$`年次`)
newest_year <- max(kouji_data$`年次`)

# 最古の年度のデータ集合
oldest_data <- kouji_data %>% filter(`年次` == oldest_year)

# 最古の年度のデータに一意のstd_idを付加する
oldest_data$std_id <- 1:nrow(oldest_data)

# 最古のデータ集合に、経年のデータを順次付け加える
ans_db <- oldest_data
for(i in (oldest_year + 1):newest_year){
  ans_db <- add_std_id(ans_db, kouji_data %>% filter(`年次` == i))
}


###################################################################
# 都道府県、市町村列の追加
# 総務省の全国地方公共団体コードを利用
###################################################################

# 全国地方公共団体コードのRデータを準備（cities_db）
load("rdata/cities_db.rda")

improved_kouji_db <- ans_db %>% left_join(cities_db, by = c("行政" = "団体コード"))

save(improved_kouji_db, file = "improved_kouji_db.rda")
