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
  filter(QTY > 0, VALUE > 0) # 数量および金額が0より大きい

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

# 価格の操作変数として各商品の同月他店舗平均価格
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

plain_logit <- estimatr::iv_robust(
  formula = log(share) - log(outshare) ~ log(price) | log(price_instruments),
  data = data_biscuits,
  fixed_effects = MBRD2,
  clusters = MBRD2,
  se_type = 'stata'
)

summary(plain_logit) # 推定結果

## nested logit推定

nested_logit <- estimatr::iv_robust(
  formula = log(share) - log(outshare) ~ log(price) + log(within_share) | log(price_instruments) + log(within_share_instruments),
  data = data_biscuits,
  fixed_effects = MBRD2,
  clusters = MBRD2,
  se_type = 'stata')

summary(nested_logit) # 推定結果

## nested logit 反実仮想

# 予測値を出すために商品ダミーを明示的に説明変数に入れる
nested_logit_prediction <- estimatr::iv_robust(
  formula = log(share) - log(outshare) ~ log(price) + log(within_share) + factor(MBRD2) | log(price_instruments) + log(within_share_instruments) + factor(MBRD2),
  data = data_biscuits
)

# オレオの価格を10%安くしてみる
data_biscuits_counterfactual_nested <- data_biscuits %>% 
  mutate(price = if_else(stringr::str_detect(MBRD2, "OREO"), price * 0.9, price))

# 平均効用deltaを計算
# モデルの予測値にはrho*log(within_share)の部分も含まれているのでそれを除く
delta_nested <- 
  predict(nested_logit_prediction, newdata = data_biscuits_counterfactual_nested) -
  coef(nested_logit)['log(within_share)'] * log(data_biscuits$within_share)

# deltaとrhoの列を加える
data_biscuits_counterfactual_nested <- data_biscuits_counterfactual_nested %>% 
  mutate(delta = delta_nested, rho = coef(nested_logit)['log(within_share)'])

# 各グループの包括的価値(inclusive value)を計算する
data_biscuits_counterfactual_nested_inclusive <- data_biscuits_counterfactual_nested %>% 
  group_by(MONTH, STORECODE, SGRP) %>% 
  summarise(inclusive = sum(exp(delta / (1 - rho))), .groups="drop") %>% # 各グループの包括的価値
  mutate(rho = coef(nested_logit)['log(within_share)']) %>% 
  group_by(MONTH, STORECODE) %>% 
  mutate(inclusive_sum = 1 + sum(inclusive^(1-rho))) %>% # 包摂的価値の市場内集計
  ungroup() %>% 
  select(!rho)

# 元のデータフレームに包摂的価値を引っ付ける
data_biscuits_counterfactual_nested <- data_biscuits_counterfactual_nested %>% 
  left_join(data_biscuits_counterfactual_nested_inclusive, by = c("MONTH", "STORECODE", "SGRP"))

# 予測シェアと数量と売上金額を計算
data_biscuits_counterfactual_nested <- data_biscuits_counterfactual_nested %>% 
  mutate(share_predicted = exp(delta / (1 - rho)) / (inclusive^rho * inclusive_sum),
         quantity_predicted = share_predicted * market_size,
         value_predicted = price * quantity_predicted)

# オレオの実際の売上と値下げ後の予測売上を比較
data_biscuits_counterfactual_nested %>% 
  filter(stringr::str_detect(MBRD2, "OREO")) %>% 
  summarise(across(c(QTY, quantity_predicted, VALUE, value_predicted), sum), .groups="drop")
