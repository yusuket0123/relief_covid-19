#"""
#synthetic did
#"""

library(magrittr)


### construct.Rからデータ読み込み

path_merge = file.path("code", "cleaning", "merge.R")
source(path_merge)

make_df_use_ssid = function(name, cols, outcome){
  if(stringi::stri_detect_regex(name, ".*community.*") == TRUE){
    df_list = dataset_community_level
  }else{
    df_list = dataset_list
  }
  df_list[[name]] %>%
    dplyr::select(cols) %>%
    #dplyr::filter(!is.na(.[[outcome]])) %>%
    dplyr::rename_at(dplyr::vars(!contains("id")), 
                     ~paste0(., "_", 
                             dplyr::if_else(stringi::stri_detect_regex(name, ".*community.*") == TRUE,
                                    gsub(pattern="df_community_level_", replacement="", x = name),
                                    gsub(pattern="(dataset_)|(_all)", replacement="", x = name)
                                    )
                       )
    )
}

make_df_imp = function(data){
  V <- ncol(data)
  # create initial predictor matrix and imputation methods
  predictorMatrix <- matrix( 1, nrow=V, ncol=V)
  diag(predictorMatrix) <- 0
  rownames(predictorMatrix) <- colnames(predictorMatrix) <- colnames(data)
  predictorMatrix[, c("hhid", "capital_2015", "capital_2018", "capital_2020")] <- 0
  method <- rep("norm", V)
  #post = rep( "imp[[j]][, i] = squeeze(imp[[j]][, i], c(0, max(imp[[j]], na.rm = TRUE)))", V)
  #names(method) <- colnames(a)
  #** groupwise imputation of variable books
  #method[c("p0", "p1", "q0", "q1")] <- "bygroup"
  # specify name of the grouping variable ('idschool') and imputation method ('norm')
  #group <- list( "p0"="category", "p1"="category", "q0"="category", "q1" ="category")
  #imputationFunction <- list("p0"="norm","p1"="norm", "q0"="norm", "q1"="norm" )
  
  #** conduct multiple imputation in mice
  imp <- mice::mice( data, method=method, predictorMatrix=predictorMatrix,  #post = post, #
                     m=10, maxit=10, seed = 12345 )
  data2 = mice::complete(imp, 1)
  return(data2)
}

output.df_use = function(data, id, outcome, cols){
  write.csv(
    purrr::reduce(
      purrr::map(names(data),
                 ~ make_df_use_ssid(name = as.character(.x), cols = cols, outcome = outcome)
      ), 
      dplyr::full_join, by = id),  
    file.path("datas", "processed", "sdid", paste0("df_use_sdid_", outcome, ".csv")
    )
    )
  print("save succeed")
}

cols = c("hhid", "iner_elite_gov_povln", "capital")
df =purrr::reduce(
  purrr::map(names(dataset_list),
             ~ make_df_use_ssid(name = as.character(.x), cols = cols, outcome = "iner_elite_gov_povln")
  ), 
  dplyr::full_join, by = "hhid")
hhid_use = dplyr::filter(df, !is.na(capital_2018)&!is.na(capital_2020))$hhid
df = df %>%
  dplyr::filter( !is.na(capital_2018)&!is.na(capital_2020)) %>%
  dplyr::mutate(capital_2015 = ifelse(!is.na(capital_2015), capital_2015, 
                                              ifelse(!is.na(capital_2018), capital_2018, capital_2020)),
                        capital_2018 = ifelse(!is.na(capital_2018), capital_2018, 
                                              ifelse(!is.na(capital_2015), capital_2015, capital_2020)),
                        capital_2020 = ifelse(!is.na(capital_2020), capital_2020, 
                                              ifelse(!is.na(capital_2018), capital_2018, capital_2015))
                        ) 
df %>% dplyr::group_by(capital_2015) %>% dplyr::summarise(mean(iner_elite_gov_povln_2020, na.rm = TRUE), mean(iner_elite_gov_povln_2018, na.rm = TRUE))

df_imp = make_df_imp(df)
write.csv(df_imp, file.path("datas", "processed", "sdid", "df_imp_use_sdid_iner_elite_gov_povln.csv"))


debug = function(){
  outcomes = c("iner_elite_gov_popbn", "exer_elite_gov_popbn", "iner_elite_gov_povln", "exer_elite_gov_povln")
  outcomes = c(outcomes, paste("com", outcomes, sep = "_"))　
  
  for (o in outcomes) {
    if(stringi::stri_detect_regex(o, "^com_.*") == TRUE){
      data = dataset_community_level
      id = "comid"
      cols = c(id, o, "com_capital")
    }else{
      data = dataset_list
      id = "hhid"
      cols = c(id, o, "capital")
    }
    ####
    output.df_use(data = data, id = id, outcome = o, cols = cols)
    print(o)
  }
}
