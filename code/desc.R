#"""
#記述統計とバランステスト
#"""


library(magrittr)

### construct.Rからデータ読み込み

path_merge = file.path("code", "merge.R")
source(path_merge)



# 記述統計年度毎
list_desc = purrr::map(names(dataset_list), ~ psych::describe(dataset_list[[.x]]))
names(list_desc) = c(names(dataset_list))


# scv吐き出し
output_csv <- function(data, names){ 
  folder_path <- "desc/summary_stats"
  write.csv(data, file.path(folder_path, paste("desc_", names, ".csv", sep = "")))
}

# 全体の記述統計年度毎吐き出し
list(data = list_desc, names = names(list_desc)) %>% purrr::pmap(output_csv) 

### バランステスト

test_b = function(data, devide_by, pattern1, pattern2, var_list){
  list_t = list()
  for (i in var_list) {
    t <- t.test(dplyr::filter(data, eval(parse(text = devide_by)) == pattern1)[[i]], 
                dplyr::filter(data, eval(parse(text = devide_by)) == pattern2)[[i]])
    list_t[[i]] = t
    print(i)
  }
  name_estimate1 = paste(devide_by, "yes",sep = "_")
  name_estimate2 = paste(devide_by, "no",sep = "_")
  tab = purrr::map_df(list_t, broom::tidy) %>% 
    dplyr::mutate(var = names(list_t)) %>%
    dplyr::select(c("var", "estimate1", "estimate2", "p.value")) %>%
    dplyr::rename(!!name_estimate1 := estimate1,
                  !!name_estimate2 := estimate2,
    )
  return(tab)
}


### 年度毎のバランステスト
var_list_201518 = list("urban", "hhhead_female", "hhhead_age","hh_head_literacy","hh_head_educ_pri","hh_head_educ_sec","hh_head_educ_high","hh_head_educ_vocation","hh_head_educ_col",
                       "lhhsize", "hh_work_employee", "hh_work_farm", "hh_work_business",
                       "elite_public", "elite_lc_gov", "elite_gov", "elite_con", 
                       "lpercapcons", "nd_drought", "nd_flood", "nd_livestock","nd_epi")
var_list_201820 = list("urban", "hhhead_female", "hhhead_age","hh_head_literacy","hh_head_educ_pri","hh_head_educ_sec","hh_head_educ_high","hh_head_educ_vocation","hh_head_educ_col",
                       "lhhsize", "hh_work_employee", "hh_work_farm", "hh_work_business",
                       "elite_public", "elite_lc_gov", "elite_gov", "elite_con", 
                       "lpercapcons")


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
var_list_2020 = list("urban", "hhhead_female", "hhhead_age", "hhsize", "hh_head_literacy","hh_head_educ_pri","hh_head_educ_sec","hh_head_educ_high","hh_head_educ_vocation","hh_head_educ_col",
                     "hh_work_employee", "hh_work_farm", "hh_work_business","elite_public", "elite_lc_gov", "elite_gov", "elite_con", "lpercapcons", "shock_sickness","shock_jobloss")
tab = test_b(dataset_list$dataset_2020_all, "in_error_popbnfcry", "1", "0", var_list = var_list_2020) # balance test tables
write.csv(tab, file.path("desc", "balance_test", "t.test_by_in_error_bottom10_in_dataset_2020_all.csv"))
tab = test_b(dataset_list$dataset_2020_all, "ex_error_popbnfcry", "1", "0", var_list = var_list_2020) # balance test tables
write.csv(tab, file.path("desc", "balance_test", "t.test_by_ex_error_bottom10_in_dataset_2020_all.csv"))
dataset_list$dataset_2020_all$in_error_popbnfcry_imp
# 年度*eliteごとに分けてみる
var_list_elite = list("urban", "hhhead_female", "hhhead_age", "hhsize", "hh_head_literacy","hh_head_educ_pri","hh_head_educ_sec","hh_head_educ_high","hh_head_educ_vocation","hh_head_educ_col",
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

