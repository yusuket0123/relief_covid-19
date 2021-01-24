#"""
#バランステストの関数
#"""


library(magrittr)

### csv吐き出し
output_csv <- function(data, names){ 
  folder_path <- "desc/summary_stats"
  write.csv(data, file.path(folder_path, paste("desc_", names, ".csv", sep = "")))
}

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
