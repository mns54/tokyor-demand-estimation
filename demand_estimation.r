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
  group_by(MONTH, STORECODE) %>% 
  mutate(quantity_total = sum(QTY)) %>% # 各市場の総売上数量
  group_by(STORECODE) %>% 
  mutate(market_size = max(quantity_total) * 2) %>%  #各店舗の最大総売上数量の2倍を潜在的市場規模とする
  ungroup()

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
  fixed_effects = ~ MBRD2,
  clusters = MBRD2,
  se_type = 'stata'
)

summary(plain_logit) # 推定結果

## plain logit 反実仮想

# 予測値を出すために商品ダミーを明示的に説明変数に入れる
plain_logit_prediction <- estimatr::iv_robust(
  formula = log(share) - log(outshare) ~ log(price) + factor(MBRD2) | log(price_instruments) + factor(MBRD2),
  data = data_biscuits
)

# オレオの価格を20%安くしてみる
data_biscuits_counterfactual_plain <- data_biscuits %>% 
  mutate(price_original = price, # 元の価格を残す
         price = if_else(MBRD2 == "OREO (CREAM)", 0.8 * price, price))

# 平均効用deltaを計算
delta_plain <- predict(plain_logit_prediction,
                       newdata = data_biscuits_counterfactual_plain)

# 反実仮想シェアを計算
data_biscuits_counterfactual_plain <- 
  data_biscuits_counterfactual_plain %>% 
  mutate(delta_counterfactual = delta_plain, # 反実仮想の平均効用
         delta_fitted = fitted(plain_logit) # 元の価格での平均効用のフィット
         ) %>%
  group_by(MONTH, STORECODE) %>% # 市場でグループ化
  # シェア計算
  mutate(share_counterfactual = exp(delta_counterfactual) / (1 + sum(exp(delta_counterfactual))),
         share_fitted = exp(delta_fitted) / (1 + sum(exp(delta_fitted)))) %>%
  ungroup() %>% 
  # 数量と金額を計算
  mutate(quantity_counterfactual = share_counterfactual * market_size,
         quantity_fitted = share_fitted * market_size,
         value_counterfactual = price * quantity_counterfactual,
         value_fitted = price_original * quantity_fitted)

# MONDELEZの実際の売上と反実仮想売上を比較
data_biscuits_counterfactual_plain %>% 
  filter(CMP == "MONDELEZ INTERNATIONAL") %>% 
  group_by(MBRD2) %>% 
  summarise(across(c(QTY, quantity_fitted, quantity_counterfactual,
                     VALUE, value_fitted, value_counterfactual),
                   sum),
            .groups="drop")

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

# オレオの価格を20%安くしてみる
data_biscuits_counterfactual_nested <- data_biscuits %>% 
  mutate(price_original = price,
         price = if_else(MBRD2 == "OREO (CREAM)", 0.8 * price, price))

# 平均効用deltaを計算
# モデルの予測値にはrho*log(within_share)の部分も含まれているのでそれを除く
rho <- coef(nested_logit)['log(within_share)'] # rho
delta_nested <- 
  predict(nested_logit_prediction, newdata = data_biscuits_counterfactual_nested) -
  rho * log(data_biscuits$within_share)

# deltaとrhoの列を加える
data_biscuits_counterfactual_nested <- data_biscuits_counterfactual_nested %>% 
  mutate(rho = rho,
         delta_counterfactual = delta_nested,
         delta_fitted = fitted(nested_logit) - rho * log(within_share))

# 各グループの包括的価値(inclusive value)を計算する
data_biscuits_counterfactual_nested_inclusive <- data_biscuits_counterfactual_nested %>% 
  group_by(MONTH, STORECODE, SGRP) %>% 
  # 各グループの包括的価値
  summarise(inclusive_counterfactual = sum(exp(delta_counterfactual / (1 - rho))),
            inclusive_fitted = sum(exp(delta_fitted / (1 - rho))),
            .groups="drop") %>%
  mutate(rho = rho) %>% 
  group_by(MONTH, STORECODE) %>% 
  # 包摂的価値の市場内集計
  mutate(inclusive_counterfactual_sum = 1 + sum(inclusive_counterfactual^(1-rho)),
         inclusive_fitted_sum = 1 + sum(inclusive_fitted^(1-rho))) %>%
  ungroup() %>% 
  select(!rho)

# 元のデータフレームに包摂的価値を引っ付ける
data_biscuits_counterfactual_nested <- data_biscuits_counterfactual_nested %>% 
  left_join(data_biscuits_counterfactual_nested_inclusive, by = c("MONTH", "STORECODE", "SGRP"))

# 予測シェアと数量と売上金額を計算
data_biscuits_counterfactual_nested <- data_biscuits_counterfactual_nested %>% 
  mutate(share_counterfactual = exp(delta_counterfactual / (1 - rho)) / (inclusive_counterfactual^rho * inclusive_counterfactual_sum),
         share_fitted = exp(delta_fitted / (1 - rho)) / (inclusive_fitted^rho * inclusive_fitted_sum),
         quantity_counterfactual = share_counterfactual * market_size,
         quantity_fitted = share_fitted * market_size,
         value_counterfactual = price * quantity_counterfactual,
         value_fitted = price_original * quantity_fitted)

# MONDELEZの実際の売上と反実仮想売上を比較
data_biscuits_counterfactual_nested %>% 
  filter(CMP == "MONDELEZ INTERNATIONAL") %>% 
  group_by(MBRD2) %>% 
  summarise(across(c(QTY, quantity_fitted, quantity_counterfactual,
                     VALUE, value_fitted, value_counterfactual),
                   sum),
            .groups="drop")
