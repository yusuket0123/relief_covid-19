#"""
#EDA: 探索的データ分析
#"""

library(magrittr)
library(ggplot2)

### construct.Rからデータ読み込み

path_merge = file.path("code", "merge.R")
source(path_merge)

### 欠損地処理のため
make_hist = function(data, x, color = NULL){
  df = dataset_list[[data]]
  if(is.null(color) == FALSE) {
    histgram = ggplot(data = subset(df, !is.na(df[[color]])), aes(x = eval(parse(text = x)), y = ..density.., fill = as.factor(eval(parse(text = color)))))  +
      scale_fill_hue(name = color, labels = c("1" = "yes", "0" = "no")) +
      geom_density(position = "identity", alpha = 0.7)
  } else {
    histgram = ggplot(data = df, aes(x = eval(parse(text = x)), y = ..density..)) +
      geom_histogram()
  }
  histgram = histgram +
    labs(x = x)
  plt = plot(histgram)
  filename = file.path("fig", paste0(x, "_", data, ".png"))
  print(filename)
  ggsave(filename = filename, plot = plt)
}

plt_list = list("dataset_2015_all", "dataset_2018_all")
var_noncolor_list = c("hhsize", "rooms")
for (i in plt_list) {
  for (j in var_noncolor_list) {
    make_hist(i, j, color = NULL)
  }
}

# 受益者かどうかでpercapcons異なるか
for (i in plt_list) {
  for (j in var_color_list) {
    make_hist(i, "lpercapcons", color = "trnsfr_any_dummy")
  }
}

# eliteとnoneliteの比較
plot_elite = function(data, dummy_from){
  df = dataset_list[[data]] %>%
    dplyr::mutate(elite_dummy = as.factor(dplyr::case_when(eval(parse(text = dummy_from)) > 0 ~ 1, 
                                                           TRUE ~ eval(parse(text = dummy_from)))
    ))
  histgram = ggplot(data = subset(df, !is.na(df$elite_dummy)), 
                    aes(x = lpercapcons, y = ..density.., fill =  elite_dummy)) +
    scale_fill_hue(name = dummy_from, labels = c("1" = "yes", "0" = "no")) +
    geom_density(position = "identity", alpha = 0.7)
  plt = plot(histgram)
  filename = file.path("fig", paste0(dummy_from, "_", data, ".png"))
  ggsave(filename = filename, plot = plt)
  print(filename)
}

var_elite_list = list("elite_lc_gov", "elite_gov", "elite_public", "elite_con")
for (i in plt_list) {
  for (j in var_elite_list) {
    plot_elite(data = i, dummy_from = j)
  }
}


### imputaion前のpercapconsの比較
#for (i in plt_list) {
#  filename = file.path("fig", paste0("percapcons_before_imp_", i, ".png"))
#  g = ggplot2::ggplot(eval(parse(text = i)), aes(x = percapcons_use)) +
#    geom_boxplot() +
#    xlim(0, 2000)
#  g = plot(g)
#  ggsave(filename = filename, g)
#}

quantile(dataset_2018_all$percapcons_imp_use, c(0.1174), na.rm = TRUE)#137430
quantile(dataset_2018_all$percapcons_imp_use, c(0.12), na.rm = TRUE)

g = ggplot2::ggplot(dataset_2018_all %>% dplyr::filter(!is.na(trnsfr_any_dummy_use)), aes(x = lpercapcons_use, y = as.factor(trnsfr_any_dummy_use))) +
  geom_detnsity() + 
  geom_vline(xintercept = round(quantile(dataset_2018_all$lpercapcons_use, c(0.1174), na.rm = TRUE), 2), linetype="dashed") +
  scale_color_hue(name = "beneficiary", labels = c("1" = "yes", "0" = "no")) +
  scale_x_discrete(limits = c(round(quantile(dataset_2018_all$lpercapcons_use, c(0.1174), na.rm = TRUE), 2))) +
  labs(x = "log per capita consumption")
plot(g)

g = ggplot2::ggplot(dataset_2018_all, aes(x = lpercapcons_use)) +
  geom_histogram() + 
  labs(x = "log per capita consumption")
plot(g)
dataset_list_all_year$trnsfr_food_dummy

df = dataset_list_all_year %>% dplyr::group_by(year) %>%
  dplyr::summarise(prob_benefit_all = mean(trnsfr_any_dummy, na.rm = TRUE),
                   prob_benefit_cash = mean(trnsfr_cash_dummy, na.rm = TRUE),
                   prob_benefit_food = mean(trnsfr_food_dummy, na.rm = TRUE),
                   prob_benefit_inkind = mean(trnsfr_inkind_dummy, na.rm = TRUE),
                   prob_povline = mean(povline, na.rm = TRUE)
                   ) %>%
  tidyr::pivot_longer(., cols = c("prob_benefit_all", "prob_benefit_cash", "prob_benefit_food", "prob_benefit_inkind", "prob_povline"), names_to = "type", values_to = "prob_beneficiary")
  
p<- ggplot(df, aes(x=year, y=prob_beneficiary, group = type,color=type)) + 
  geom_line() +
  geom_point() + 
  labs(y = "prop. of beneficiaries") +
  scale_color_hue(name = "aid", labels = c("prob_benefit_all" = "any", "prob_benefit_cash" = "cash", "prob_benefit_food" = "food", "prob_benefit_inkind" = "inkind", "prob_povline" = "poverty_line")) +
  geom_text(aes(label=round(prob_beneficiary, 3)),hjust=0.2, vjust=-0.7, size = 7)
plot(p)
make_hist(data = "dataset_2015_all", "percapcons")

quantile(dataset_list$dataset_2018_all$percapcons_imp, c(0.1),na.rm = TRUE)

### 可視化・相関
cor_list <-c("in_error", "elite_gov_use", "elite_con_use", "elite_lc_gov_use","elite_public_use", "lpercapcons", "hhhead_female_use", "nd_drought_use","nd_flood_use",               
              "nd_pest_use","nd_livestock_use","nd_epi_use", "ltrnsfr_cash_use", "ltrnsfr_food_use", "ltrnsfr_inkind_use", "trnsfr_food_dummy_use.x", "trnsfr_cash_dummy_use.x", "trnsfr_inkind_dummy_use.x")
#corrplot::corrplot(cor(dataset_list[[3]][,cor_list], use="pairwise.complete.obs"), method="shade", shade.col=NA, tl.col="black", tl.srt=20, addCoef.col="black")
#
#cor_list = c("r1_sect_11_trnsfr_food_dummy_use", "r1_sect_11_trnsfr_cash_dummy_use","ltrnsfr_cash_use", "ltrnsfr_food_use", "ltrnsfr_inkind_use", 
#             "trnsfr_cash_dummy_use.x","trnsfr_food_dummy_use.x", "trnsfr_inkind_dummy_use.x","trnsfr_any_dummy_use.x"  
#            )


### 可視化年度毎の推移



### error_rate
