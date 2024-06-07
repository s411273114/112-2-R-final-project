# 保留特定資料 ----- -----

library(dplyr)
library(stringr)

# 資料1：X15歲以上吸菸者每天平均吸菸支數
# 讀取資料
data1 <- get("X15歲以上吸菸者每天平均吸菸支數")

# 篩選出"教育程度別1="開頭的列
data1_filtered <- data1 %>% filter(str_detect(分析項目, "^教育程度別1=")| str_detect(分析項目, "^整體"))

# 查看過濾後的資料
glimpse(data1_filtered[1:3, ])

# 資料2：X15歲以上人口每日吸菸率
# 讀取資料
data2 <- get("X15歲以上人口每日吸菸率")

# 保留第1行及第3至第8行的資料
data2_filtered <- data2[c(1, 3:8), ]

# 刪除包含"102年"的資料欄位
data2_filtered <- data2_filtered %>% select(-contains("102年"))

# 查看過濾後的資料
glimpse(data2_filtered[1:3, ])


# 去除年度開頭的"民國" ----- \
data1_filtered$年度 <- gsub("民國", "", data1_filtered$年度)

# 替換"教育程度別1=" ----- 
library(dplyr)

# 替換開頭
data1_filtered$分析項目 <- gsub("^教育程度別1=", "教育別=", data1_filtered$分析項目)
# 修改欄位名稱
data2_filtered <- data2_filtered %>%
  rename(
    "103年整體每日吸菸率(%)" = "103年每日吸菸率(%)",
    "104年整體每日吸菸率(%)" = "104年每日吸菸率(%)",
    "105年整體每日吸菸率(%)" = "105年每日吸菸率(%)"
  )
# 轉換檔案二的資料呈現方式 -----
library(dplyr)
library(tidyr)

# 將資料轉換為長格式
data2_long <- data2_filtered %>%
  pivot_longer(
    cols = starts_with("103年整體每日吸菸率(%)"):starts_with("105年女性每日吸菸率(%)"),
    names_to = c("年度", "指標"),
    names_pattern = "^(\\d+年)(.*)$",
    values_to = "值"
  ) %>%
  pivot_wider(
    names_from = "指標",
    values_from = "值"
  ) %>%
  relocate(年度, .before = everything())

# 查看結果的前幾行
glimpse(data2_long)

# 進行替換
data2_long <- data2_long %>%
  mutate(across(everything(), ~ gsub("教育別=小學或以下", "教育別=小學及以下", .)))

# 水平合併 data1_filtered 與 data2_long -----
library(dplyr)

# 確認共同欄位
common_columns <- intersect(names(data1_filtered), names(data2_long))

# 合併數據框
merged_data <- data1_filtered %>%
  full_join(data2_long, by = common_columns)

# 查看合併結果的前幾行
glimpse(merged_data)
