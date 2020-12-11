#"""
#imputation
#"""

library(miceadds)

#path_clean = file.path("code", "clean.R")
#source(path_clean)



### imputation
get_hhid = function(data){
  df = data %>%
    dplyr::filter(s7bq1 == 1|s10bq1 == 1) %>%
    dplyr::select("hhid_use")
  return(df)
}
get_df_use = function(data){
  df = data %>%
    dplyr::filter(s7bq1 == 1|s10bq1 == 1) %>%
    dplyr::select("hhid_use", "item_cd", "p0","p1","q0", "q1","zone", "category") #%>%
   
  return(df)
}
make_df_imp = function(df_use){
  V <- ncol(df_use)
  # create initial predictor matrix and imputation methods
  predictorMatrix <- matrix( 1, nrow=V, ncol=V)
  diag(predictorMatrix) <- 0
  rownames(predictorMatrix) <- colnames(predictorMatrix) <- colnames(df_use)
  predictorMatrix[, c("hhid_use", "item_cd", "zone", "category")] <- 0
  method <- rep("norm", V)
  post = rep( "imp[[j]][, i] = squeeze(imp[[j]][, i], c(0, max(imp[[j]], na.rm = TRUE)))", V)
  names(method) <- colnames(df_use)
  #** groupwise imputation of variable books
  method[c("p0", "p1", "q0", "q1")] <- "bygroup"
  # specify name of the grouping variable ('idschool') and imputation method ('norm')
  group <- list( "p0"="category", "p1"="category", "q0"="category", "q1" ="category")
  imputationFunction <- list("p0"="norm","p1"="norm", "q0"="norm", "q1"="norm" )
  
  #** conduct multiple imputation in mice
  imp <- mice::mice( df_use, method=method, predictorMatrix=predictorMatrix, post = post,
                     m=3, maxit=5, group=group, imputationFunction=imputationFunction, seed = 12345 )
  
  return(imp)
}


make_cons_food = function(data){
  df = data %>%
    dplyr::mutate(p0_imp = dplyr::case_when(s7bq1 == 1 ~ p0_imp, TRUE ~ NA_real_),
                  p1_imp = dplyr::case_when(s10bq1 == 1 ~ p1_imp, TRUE ~ NA_real_),
                  q0_imp = dplyr::case_when(s7bq1 == 1 ~ q0_imp, TRUE ~ NA_real_),
                  q1_imp = dplyr::case_when(s10bq1 == 1 ~ q1_imp, TRUE ~ NA_real_)
                  ) %>%
    dplyr::group_by(hhid_use) %>%
    dplyr::mutate(h_idx = sum(p1_imp * q0_imp, na.rm = TRUE) / sum(p0_imp * q0_imp, na.rm = TRUE),
                  cons_food_imp_p_use = sum(p0_imp*q0_imp, na.rm = TRUE) / 7,
                  cons_food_h = sum(p1_imp*q1_imp, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(h_idx_imp_use = dplyr::case_when((h_idx > 0) & (!is.na(h_idx)) & (!is.infinite(h_idx)) & (!is.nan(h_idx)) ~ h_idx,
                                               TRUE ~ NA_real_
    )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cons_food_imp_h_use = cons_food_h / h_idx_imp_use / 7) 
  return(df)
}


### 2015年
#MARの確認
a = dataset_2015$sect7b_plantingw3 %>% dplyr::left_join(., dataset_2015$sect1_harvestw3 %>% 
                                                          dplyr::select(c("hhid_use", "hhhead_female_use","hhhead_age_use","hhsize_use",
                                                                          "hh_head_literacy_use","hh_head_educ_pri_use","hh_head_educ_sec_use",
                                                                          "hh_head_educ_high_use","hh_head_educ_vocation_use","hh_head_educ_col_use"))) %>%
  dplyr::mutate(p1_attr = ifelse(is.na(p1), 1, 0),
                p0_attr = ifelse(is.na(p0), 1, 0),
                q1_attr = ifelse(is.na(q1), 1, 0),
                q0_attr = ifelse(is.na(q0), 1, 0)
                )
cor_list <-c("p1", "p0" ,"q1" ,"q0","p1_attr","p0_attr", "q1_attr", "q0_attr", "hhhead_female_use","hhhead_age_use","hhsize_use",
             "hh_head_literacy_use","hh_head_educ_pri_use","hh_head_educ_sec_use",
             "hh_head_educ_high_use","hh_head_educ_vocation_use","hh_head_educ_col_use")
corrplot::corrplot(cor(a[,cor_list], use="pairwise.complete.obs"), method="shade", shade.col=NA, tl.col="black", tl.srt=20, addCoef.col="black")

# imputation
dataset_2015$sect7b_plantingw3 %<>%
  dplyr::mutate(zone = stringi::stri_sub(hhid_use, from = 1, to = 1)) 
plt_hist_2015_bfr = VIM::aggr(get_df_use(dataset_2015$sect7b_plantingw3), prop=TRUE)

imp_2015 = make_df_imp(get_df_use(dataset_2015$sect7b_plantingw3))
plt_dens_2015 = mice::densityplot(imp_2015)

df_imp_2015 = mice::complete(imp_2015, 1) %>% dplyr::rename(p0_imp = p0, p1_imp = p1, q0_imp = q0, q1_imp = q1)
#df_imp_2015 %<>% dplyr::mutate(p0_imp = ifelse(p0_imp == 0, NA, p0_imp),
#                          p1_imp = ifelse(p1_imp == 0, NA, p1_imp),
 #                         q0_imp = ifelse(q0_imp == 0, NA, q0_imp),
  #                        q1_imp = ifelse(q1_imp == 0, NA, q1_imp)
#)
plt_hist_2015_aft = VIM::aggr(df_imp_2015, prop=TRUE)
dataset_2015$sect7b_plantingw3 %<>% dplyr::left_join(., df_imp_2015, by = c("hhid_use", "item_cd"))
write.csv(df_imp_2015, file.path("datas", "processed", "imp_pfood_dataset_2015.csv"))
dataset_2015$sect7b_plantingw3 %<>% make_cons_food(.) %>% dplyr::distinct(hhid_use, .keep_all = TRUE)



### 2018
#MARの確認
a = dataset_2018$sect7b_plantingw4 %>% dplyr::left_join(., dataset_2018$sect1_harvestw4 %>% 
                                                          dplyr::select(c("hhid_use", "hhhead_female_use","hhhead_age_use","hhsize_use",
                                                                          "hh_head_literacy_use","hh_head_educ_pri_use","hh_head_educ_sec_use",
                                                                          "hh_head_educ_high_use","hh_head_educ_vocation_use","hh_head_educ_col_use"))) %>%
  dplyr::mutate(p1_attr = ifelse(is.na(p1), 1, 0),
                p0_attr = ifelse(is.na(p0), 1, 0),
                q1_attr = ifelse(is.na(q1), 1, 0),
                q0_attr = ifelse(is.na(q0), 1, 0)
  )
cor_list <-c("p1", "p0" ,"q1" ,"q0","p1_attr","p0_attr", "q1_attr", "q0_attr", "hhhead_female_use","hhhead_age_use","hhsize_use",
             "hh_head_literacy_use","hh_head_educ_pri_use","hh_head_educ_sec_use",
             "hh_head_educ_high_use","hh_head_educ_vocation_use","hh_head_educ_col_use")
corrplot::corrplot(cor(a[,cor_list], use="pairwise.complete.obs"), method="shade", shade.col=NA, tl.col="black", tl.srt=20, addCoef.col="black")


# imputation
plt_hist_2018_bfr = VIM::aggr(get_df_use(dataset_2018$sect7b_plantingw4), prop=TRUE)
imp = make_df_imp(get_df_use(dataset_2018$sect7b_plantingw4))
plt_dens_2018 = mice::densityplot(imp)
df_imp = mice::complete(imp, 1) %>% dplyr::rename(p0_imp = p0, p1_imp = p1, q0_imp = q0, q1_imp = q1)
plt_hist_2018_aft =  VIM::aggr(df_imp, prop=TRUE)
dataset_2018$sect7b_plantingw4 %<>% dplyr::left_join(., df_imp, by = c("hhid_use", "item_cd"))
write.csv(df_imp, file.path("datas", "processed", "imp_pfood_dataset_2015.csv"))
dataset_2018$sect7b_plantingw4 %<>% make_cons_food(.) %>% dplyr::distinct(hhid_use, .keep_all = TRUE)

a = read.csv(file.path("datas", "processed", "imp_pfood_dataset_2015.csv"))
