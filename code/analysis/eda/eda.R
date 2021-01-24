#"""
#EDA: 探索的データ分析
#"""

library(magrittr)
library(ggplot2)

### construct.Rからデータ読み込み

path_merge = file.path("code", "cleaning", "merge.R")
source(path_merge)

### 欠損地処理のため
make_hist = function(data, x){
  df = dataset_list[[data]]
  histgram = ggplot(data = df, aes(x = eval(parse(text = x))), y = ..density.., xlab = x, ylab = "density") +
    geom_histogram() 
  plt = plot(histgram)
  filename = file.path("fig", paste0(df, "_", x, ".png"))
  print(filename)
  ggsave(filename = filename, plot = plt, width = 1000, height = 1000)
}

for (i in names(dataset_list)) {
  make_hist(dataset_2015_all, "hhsize")
}
make_hist("dataset_2015_all", "hhsize")




### 可視化・相関
#cor_list <-c("in_error", "elite_gov_use", "elite_con_use", "elite_lc_gov_use","elite_public_use", "lpercapcons", "hhhead_female_use", "nd_drought_use","nd_flood_use",               
#              "nd_pest_use","nd_livestock_use","nd_epi_use", "ltrnsfr_cash_use", "ltrnsfr_food_use", "ltrnsfr_inkind_use", "trnsfr_food_dummy_use.x", "trnsfr_cash_dummy_use.x", "trnsfr_inkind_dummy_use.x")
#corrplot::corrplot(cor(dataset_list[[3]][,cor_list], use="pairwise.complete.obs"), method="shade", shade.col=NA, tl.col="black", tl.srt=20, addCoef.col="black")
#
#cor_list = c("r1_sect_11_trnsfr_food_dummy_use", "r1_sect_11_trnsfr_cash_dummy_use","ltrnsfr_cash_use", "ltrnsfr_food_use", "ltrnsfr_inkind_use", 
#             "trnsfr_cash_dummy_use.x","trnsfr_food_dummy_use.x", "trnsfr_inkind_dummy_use.x","trnsfr_any_dummy_use.x"  
#            )


### 可視化年度毎の推移



### error_rate
