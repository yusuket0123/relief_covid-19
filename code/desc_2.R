#"""
# community levelのバランステスト
#"""

library(magrittr)

### setup
path_merge_community = file.path("code", "merge_community.R")
path_desc_func = file.path("code", "desc_func.R")

source(path_merge_community)
source(path_desc_func)

var_list = c("hhhead_female", "trnsfr_any_dummy", "hhhead_age","hh_head_literacy","hh_head_educ_pri",
                "hh_head_educ_sec","hh_head_educ_high","hh_head_educ_vocation","hh_head_educ_col",
                "lhhsize", "hh_work_employee", "hh_work_farm", "hh_work_business",
                "elite_public", "elite_lc_gov", "elite_gov", "elite_con", "lpercapcons_imp",
                "ex_error_povline", "in_error_povline", "ex_error_popbnfcry", "in_error_popbnfcry"
                )
var_list = purrr::map(var_list, ~ paste0("com_", .x))

### バランステスト
test_b(dataset_community_level$df_community_level_2015, devide_by = "com_capital", pattern1 = "1", pattern2 = "0", var_list = var_list)
dataset_community_level$df_community_level_2020 %>% dplyr::group_by(com_capital)%>%
  dplyr::summarise(mean(com_trnsfr_any_dummy, na.rm = TRUE))

dataset_list$dataset_2020_all %>% dplyr::group_by(capital)%>%
  dplyr::summarise(mean(trnsfr_any_dummy, na.rm = TRUE))
test_b(dataset_list$dataset_2020_all, devide_by = "capital", pattern1 = "1", pattern2 = "0", var_list = var_list)
