#"""
#longで持ったときのテスト
#"""

library(magrittr)

### construct.Rからデータ読み込み

path_merge = file.path("code", "merge.R")
source(path_merge)

dataset_list_all_year %>% tidyr::pivot_longer(!hhid, names_to = "var", values_to = "value")
