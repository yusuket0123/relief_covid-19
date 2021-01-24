#"""
# hh levelの記述統計とバランステスト
#"""


library(magrittr)

### setup

path_merge = file.path("code", "cleaning", "merge.R")
source(path_merge)
path_desc_func = file.path("code", "eda", "desc_func.R")
source(path_desc_func)


# 記述統計年度毎
list_desc = purrr::map(names(dataset_list), ~ psych::describe(dataset_list[[.x]]))
names(list_desc) = c(names(dataset_list))

# 全体の記述統計年度毎吐き出し
list(data = list_desc, names = names(list_desc)) %>% purrr::pmap(output_csv) 

### 年度毎のバランステスト
var_list_201518 = list("capital", "hhhead_female", "hhhead_age","hh_head_literacy","hh_head_educ_pri","hh_head_educ_sec","hh_head_educ_high","hh_head_educ_vocation","hh_head_educ_col",
                       "lhhsize", "hh_work_employee", "hh_work_farm", "hh_work_business",
                       "elite_public", "elite_lc_gov", "elite_gov", "elite_con", 
                       "lpercapcons", "nd_drought", "nd_flood", "nd_livestock","nd_epi")
var_list_201820 = list("capital", "hhhead_female", "hhhead_age","hh_head_literacy","hh_head_educ_pri","hh_head_educ_sec","hh_head_educ_high","hh_head_educ_vocation","hh_head_educ_col",
                       "lhhsize", "hh_work_employee", "hh_work_farm", "hh_work_business",
                       "elite_public", "elite_lc_gov", "elite_gov", "elite_con", 
                       "lpercapcons_imp")


tab = test_b(dataset_list_all_year, "year", "2015", "2018", var_list = var_list_201518)
write.csv(tab, file.path("desc", "balance_test", "t.test_by_year_15vs18.csv"))
tab = test_b(dataset_list_all_year, "year", "2018", "2020", var_list = var_list_201820)
write.csv(tab, file.path("desc", "balance_test", "t.test_by_year_18vs20.csv"))



### error = 1,0のバランステスト
list_i = list("dataset_2015_all", "dataset_2018_all")
list_j = list("in_error_popbnfcry", "ex_error_popbnfcry", "in_error_povline","ex_error_povline")
for (i in list_i) {
  for (j in list_j) {
    tab = test_b(dataset_list[[i]], j, "1", "0", var_list = var_list_201518) # balance test table
    filename = paste("t.test", "by", j, "in", i, sep = "_")
    print(filename)
    print(tab)
    write.csv(tab, file.path("desc", "balance_test",paste0(filename, ".csv"))) #csv書き出し
  }
}
var_list_2020 = list("capital", "hhhead_female", "hhhead_age", "hhsize", "hh_head_literacy","hh_head_educ_pri","hh_head_educ_sec","hh_head_educ_high","hh_head_educ_vocation","hh_head_educ_col",
                     "hh_work_employee", "hh_work_farm", "hh_work_business","elite_public", "elite_lc_gov", "elite_gov", "elite_con", "lpercapcons", "shock_sickness","shock_jobloss")
tab = test_b(dataset_list$dataset_2020_all, "in_error_popbnfcry", "1", "0", var_list = var_list_2020) # balance test tables
write.csv(tab, file.path("desc", "balance_test", "t.test_by_in_error_bottom10_in_dataset_2020_all.csv"))
tab = test_b(dataset_list$dataset_2020_all, "ex_error_popbnfcry", "1", "0", var_list = var_list_2020) # balance test tables
write.csv(tab, file.path("desc", "balance_test", "t.test_by_ex_error_bottom10_in_dataset_2020_all.csv"))

# 年度*eliteごとに分けてみる
var_list_elite = list("capital", "hhhead_female", "hhhead_age", "hhsize", "hh_head_literacy","hh_head_educ_pri","hh_head_educ_sec","hh_head_educ_high","hh_head_educ_vocation","hh_head_educ_col",
                      "hhsize", "hh_work_employee", "hh_work_farm", "hh_work_business","lpercapcons", "nd_drought", "nd_flood", "nd_livestock","nd_epi")
list_i = list("dataset_2015_all", "dataset_2018_all")
list_j = list("elite_public", "elite_lc_gov", "elite_gov", "elite_con")
for (i in list_i) {
  for (j in list_j) {
    tab = test_b(dataset_list[[i]], j, "1", "0", var_list = var_list_elite) # balance test tables
    filename = paste("t.test", "by", j, "in", i, sep = "_")
    print(filename)
    print(tab)
    write.csv(tab, file.path("desc", "balance_test",paste0(filename, ".csv"))) #csv書き出し
  }
}

# 2020*capitalごとに分けてみる
var_list_2020 = list("trnsfr_any_dummy", "trnsfr_food_dummy", "trnsfr_cash_dummy"), "hhhead_female", "hhhead_age", "hhsize", "hh_head_literacy","hh_head_educ_pri","hh_head_educ_sec","hh_head_educ_high","hh_head_educ_vocation","hh_head_educ_col",
                     "hh_work_employee", "hh_work_farm", "hh_work_business","elite_public", "elite_lc_gov", "elite_gov", "elite_con", "lpercapcons", "shock_sickness","shock_jobloss")
tab = test_b(dataset_list$dataset_2020_all, "capital", "1", "0", var_list = "shock_jobloss") # balance test tables
write.csv(tab, file.path("desc", "balance_test", "t.test_by_capital_in_dataset_2020_all.csv"))


