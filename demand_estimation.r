library(tidyverse)

# データ読み込み
data <- read_csv("Hackathon_Ideal_Data.csv")

# ビスケットのデータを取り出す
data_biscuits <- data %>% filter(GRP == "BISCUITS - CORE & NON CORE")

## 前処理

# ブランドをメーカーブランドにまとめる(ブランドにフレーバー含まれることあるため)
data_biscuits_agg <- data_biscuits %>% 
  group_by(MONTH, STORECODE, SGRP, CMP, MBRD) %>% 
  summarise(across(c(QTY, VALUE), sum), .groups="drop") %>% 
  filter(QTY > 0) # 数量が1以上のレコードだけ残す

# 同じメーカーブランドでも企業名やジャンルが違うレコードあるか確認
data_biscuits_agg %>% distinct(SGRP, CMP, MBRD) %>% group_by(MBRD) %>% filter(n() > 1)

# メーカーの表記ゆれと同一ブランドでも違うジャンルのものがあることがわかった
# 前処理してからもう一度メーカブランドにまとめる処理する
data_biscuits <- data_biscuits %>% 
  # メーカーの表記ゆれ直す
  mutate(CMP = case_when(
    CMP == "KAIRA DISTRICT CO-OP MILK" ~ "G C M M F",
    CMP == "CHAMPION" ~ "SUKHDATA FOODS",
    CMP == "PATANJALI AYURVED LTD" ~ "PATANJALI BISCUITS PVT LTD",
    CMP == "PICKWICK HYGIENIC PRODUCTS" ~ "PRIMLAKS",
    TRUE ~ CMP
  )) %>% 
  # メーカブランド名にジャンルも含める
  mutate(MBRD2 = paste0(MBRD, " (", SGRP, ")")) %>% 
  # 集計
  group_by(MONTH, STORECODE, SGRP, CMP, MBRD2) %>% 
  summarise(across(c(QTY, VALUE), sum), .groups="drop") %>% 
  filter(QTY > 0) # 数量が1以上

# いずれかの月において1店舗でしか販売されていない商品
biscuits_only1shop <- data_biscuits %>% 
  distinct(MONTH, STORECODE, MBRD2) %>% 
  group_by(MONTH, MBRD2) %>% 
  filter(n() == 1) %>% 
  ungroup() %>% 
  distinct(MBRD2) %>% 
  pull()

# いずれかの月において1店舗でしか販売されていない商品をデータから除く
data_biscuits <- data_biscuits %>% 
  filter(!(MBRD2 %in% biscuits_only1shop))

## データ構築

# 潜在的市場規模
data_biscuits <- data_biscuits %>% 
  # 月別店舗別最大売上数量1600くらいなのでとりあえず2000と設定
  mutate(market_size = 2000) # 本当は店舗ごとに市場規模異なる設定にすべき?

# シェアの列作る
data_biscuits <- data_biscuits %>% 
  mutate(share = QTY / market_size) %>% # シェア
  group_by(MONTH, STORECODE) %>% 
  mutate(outshare = 1 - sum(share)) %>% # 「何も買わない」のシェア
  ungroup()

# 価格の列作る
data_biscuits <- data_biscuits %>% 
  mutate(price = VALUE / QTY)

# 価格の操作変数として各商品の同月別店舗平均価格
data_biscuits <- data_biscuits %>% 
  group_by(MONTH, MBRD2) %>% 
  mutate(price_instruments = (sum(price) - price) / (n() - 1)) %>% 
  ungroup()

# グループ内シェア
# ジャンルをグループにする
data_biscuits <- data_biscuits %>% 
  group_by(MONTH, STORECODE, SGRP) %>% 
  mutate(within_share = share / sum(share)) %>% 
  ungroup()

# グループ内シェアの操作変数として各商品の同月他店舗平均グループ内シェア
data_biscuits <- data_biscuits %>% 
  group_by(MONTH, MBRD2) %>% 
  mutate(within_share_instruments = (sum(within_share) - within_share) / (n() - 1)) %>% 
  ungroup()

## plain logit推定

## nested logit推定

