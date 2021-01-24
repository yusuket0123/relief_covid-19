#"""
#データの読み込み
#"""

### importing liberary
library(magrittr)

list_filenames = list()
names_readdatas = list()
path_data = list.files(file.path("datas", "original","nga")) # /ngaまでpath指定

for (i in path_data) {
  path_wave = file.path("datas", "original", "nga", i) # nga/waveごとのデータまでpath指定
  filenames = list.files(path_wave) # waveごとのデータフォルダ内のファイル名を取得
  for(j in filenames){
    path = file.path("datas", "original", "nga", i, j)
    list_filenames = append(unlist(list_filenames), path) # それぞれのファイルのpathを取得
    names_readdatas = append(unlist(names_readdatas), j) # path name
  }
}

readdatas = list_filenames %>% purrr::set_names(names_readdatas) %>% purrr::map(., readr::read_csv) # 全てのファイルを格納
