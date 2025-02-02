read.csv("datasets/csv/bolsistas_incts.csv",h=T,fileEncoding = "latin1",sep = ";")->bolsa
library(stringr)
library(RSelenium)
data.frame(Nome=unique(word(bolsa$X..Nome.Beneficiário))[order(unique(word(bolsa$X..Nome.Beneficiário)))],Sexo=NA)->sex_data

library(tidyverse)
library(RSelenium)

rD <- rsDriver(browser = "firefox",
               chromever = NULL,
               port = 5555L)
remDr <- rD$client

sex_data[is.na(sex_data$Sexo),]->sex_nas

for(x in 1:nrow(sex_nas)){
  
  remDr$navigate(paste0("https://www.dicionariodenomesproprios.com.br/",sex_nas$Nome[x]))
  # search_res<-1
  # while(!is.na(word(search_res)[1])){
  #   try(remDr$findElement("class","search-box"),silent = T)->search
  #   try(search$sendKeysToElement(list(sex_data$Nome[x])))->search_res
  # }
  # rm(search_res)
  # 
  # remDr$findElement("class","search-button")->elem1
  # elem1$clickElement()
  # res_try<-1
  # 
  # #Sys.sleep(2)
  # 
  # while(!is.na(word(res_try)[1])){
  #   try(remDr$findElements("class","full-w"),silent = T)->results
  #   try(results[[1]]$clickElement())->res_try
  # }
  # 
  remDr$findElements("class","origem-related")->res2
  try(res2[[1]],silent = T)->res2_try
  if(is(res2_try)[1]=="try-error"){
    next
  } else {
    str_extract(res2[[1]]$getElementText(),"masculino|feminino")->sex_nas$Sexo[x] 
  }
}
remDr$quit()

rbind(sex_nas,sex_data[!is.na(sex_data$Sexo),])->neo_sex
neo_sex[order(neo_sex$Nome),]->neo_sex
nrow(neo_sex[is.na(neo_sex$Sexo),])

write.csv(neo_sex,"neo_sex.csv",fileEncoding = "latin1")
