# Extração de liminares monocráticas em ADI e ADPF no Supremo Tribunal Federal
# Adriano Belisário - Agência Pública - setembro de 2018

#Lista, instala e carrega pacotes
pacotes = c("readxl","tidyr", "dplyr","plyr","stringr","janitor") 
install.packages(pacotes)
lapply(pacotes, library, character.only = TRUE)

#Define período, url base e baixa arquivos, totalizando 547.656 registros extraídos em 16/09
anos <- 2017:2018
url <- "http://www.stf.jus.br/arquivo/cms/publicacaoBOInternet/anexo/decisoes/monocraticas/"
lapply(paste0(url,"decisoes_monocraticas_lista_", anos,".xlsx"), function(x) download.file(x, basename(x)))
baixados <- list.files(pattern = '.xlsx') # cria lista dos arquivos baixados
decisoes_gerais <- do.call("rbind",lapply(baixados,FUN=function(files){read_excel(files,skip=5)})) #importa
#decisoes_gerais$`Data Andamento` <- as.Date(decisoes_gerais$`Data Andamento`,'%YYYY-MM-DD') #configura data 

#Filtra ADI e ADPF liminares fora do recesso em 2018 e 2017
#Calendario do STF: http://www.stf.jus.br/portal/cms/verTexto.asp?servico=processoCalendarioStf&pagina=calendarioStf
#Calendario de 2017 http://www.stf.jus.br/arquivo/cms/processoCalendarioStf/anexo/CalendrioSTFOficial2017.pdf
#Aprox. 27 semanas ou 129 dias de exercício ate 11/09/2018 ---  2017, 192 (57+66+69) dias, total 329 dias
#77 liminares (não casos únicos!) entre 2017 e 18
liminar_adi_adpf <- decisoes_gerais %>% 
  filter(str_detect(`Andamento`, "Liminar")) %>%
  filter(Classe %in% c("ADI","ADPF"))

#Depois de 2017
liminar_adi_adpf <- subset(liminar_adi_adpf, `Data Andamento` >= as.POSIXct('2017-02-01'))
#Recesso de 2017,1caso
recesso <- subset(liminar_adi_adpf, `Data Andamento` > as.POSIXct('2018-07-02') & `Data Andamento` < as.POSIXct('2018-07-30'))
liminar_adi_adpf <- subset(liminar_adi_adpf, `Data Andamento` < as.POSIXct('2018-07-02') | `Data Andamento` > as.POSIXct('2018-07-30'))
#Final de ano, sem casos
liminar_adi_adpf <- subset(liminar_adi_adpf, `Data Andamento` > as.POSIXct('2018-02-01') | `Data Andamento` < as.POSIXct('2017-12-20'))
#Recesso de 2018,sem caso
liminar_adi_adpf <- subset(liminar_adi_adpf, `Data Andamento` > as.POSIXct('2018-07-31') | `Data Andamento` < as.POSIXct('2018-07-02'))

#Total de casos 73
length(unique(liminar_adi_adpf$Link))

#Ranking por andamento
ddply(liminar_adi_adpf, .(`Andamento`), summarise, casos_unicos = length(unique(`Link`)))

#Ranking por ministro
ddply(liminar_adi_adpf, .(`Orgão Julgador`), summarise, casos_unicos = length(unique(`Link`)))

#Ranking ADI/ADPF - Gilmar
gilmar <- liminar_adi_adpf %>% 
  filter(str_detect(`Orgão Julgador`, "GILMAR MENDES"))

ddply(gilmar, .(`Classe`), summarise, casos_unicos = length(unique(`Link`)))

#Ranking por classe
ddply(liminar_adi_adpf, .(`Classe`), summarise, casos_unicos = length(unique(`Link`)))

#Exporta casos 
write.csv(file="liminares_2017_18.csv",liminar_adi_adpf)
