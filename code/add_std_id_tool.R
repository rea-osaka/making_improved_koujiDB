library(tidyverse)
#####################################################################################
# 同一標準地を表すstd_idを作成するルーチン
# 2019/9/21 syunsuke.fukuda@gmail.com
#
# 地価公示データフレームに
# 同一標準地に一意のidを割りふるstd_id列を付け加える。
# 経年のstd_id付のデータフレームに、新年度のデータを付け加える形の
# ルーチンを作成する。
# 新しいデータが発表される毎に、全データをつくりなおすことなく、
# 地価公示がされ他時に、既存データに新しいデータを追加する事が出来る。
#
# https://github.com/rea-osaka/kouji_db_base
# 地価公示の基礎データベースに作成することが出来る
#
# 関数に渡す引数について
# past_df 既存のstd_id付公示データフレーム
# add_df  付け加える新しい年の地価公示データ
#         どのようなデータを渡しても過去データの中の最新年の
#         翌年のデータのみを扱う

add_std_id <- function(past_df, add_df){
  
  start_t <- proc.time()
  
  # 同一標準地を示すid番号の管理
  cnt_num <- max(past_df$std_id) + 1
  
  # 過去データの中のもっとも新しい年のデータ（つまり去年のデータ）
  l_year_data <- past_df %>% filter(`年次` == max(past_df$`年次`))
  
  # 新たに付け加える今年のデータ(過去データ最新年の次の年に限定)
  n_year_data <- 
    add_df %>% 
    filter(`年次` == max(past_df$`年次`) + 1) %>% 
    mutate(std_id = 0)
  
  # 状況表示
  print(paste("now writing is", max(past_df$`年次`) + 1 , "year data."))
  print(paste("last year data count is", nrow(l_year_data)))
  print(paste("new year data count is", nrow(n_year_data)))
  
  # n_year_dataを一行ずつ処理
  for(i in 1:nrow(n_year_data)){
    
    #ターゲットの前年データを探す
    ans <- 
      l_year_data %>%
      filter(`所在地コード` == n_year_data[i,]$`前年所在地コード`,
             `用途` == n_year_data[i,]$`前年用途`,
             `連番` == n_year_data[i,]$`前年連番`)
    
    # 検索結果毎の処理
    if(nrow(ans) == 1){
      n_year_data[i,]$std_id <- ans$std_id
    }else{
      n_year_data[i,]$std_id <- cnt_num
      cnt_num <- cnt_num + 1
    }
    
  }# end of for loop
  
  ans_db <- bind_rows(past_df,n_year_data)
  
  # 経過時間
  print(proc.time() - start_t)
  
  return(ans_db)
  
}
