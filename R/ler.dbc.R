
#' ler.dbc: Funcao para ler arquivos .dbc
#'
#' @description  Esta funcao recebe o caminho da pasta contendo os dados(arquivos) que foram baixados em formato .dbc. Ela ler esses arquivos, realiza um recorte nessas bases de dados e  salva todas em um unico arquivo .Rdata
#'
#'
#' @param UF Uma String, Nome do estado
#' @param caminho Uma String, caminho ate o diretorio onde foi baixado os dados dbc
#'
#' @export
#'
#' @examples
#' ler.dbc("CE","C:/Users/gustaavobraga/Documents")
#'
ler.dbc <- function(UF,caminho){

  ## Liste todos os arquivos dentro da raiz da pasta.
  files <- list.files(caminho, full.names = TRUE, recursive = TRUE)

  # Salvando cada caminho do vetor files em uma variável
  filePA=files[grep(paste0('PA',UF), files)]
  fileRD=files[grep(paste0('RD',UF), files)]
  fileRJ=files[grep(paste0('RJ',UF), files)]
  fileER=files[grep(paste0('ER',UF), files)]
  fileSP=files[grep(paste0('SP',UF), files)]
  rm(files)

  if(UF=='CE'){
    CNES_HOSP = c('2611686')
  } else if(UF=='SC'){
    CNES_HOSP = c('3157245')
  } else {
    CNES_HOSP = c('3157245')
  }

  # Para ver qual é o CNES_HOSP em comum para todas as bases, utilize:
  #Reduce(intersect, list(y1$PA_CODUNI,y2$CNES,y3$CNES,y4$CNES,y5$SP_CNES))[1]

  #----AMBULATORIAL-------------------------


  PA = data.frame()
  for(i in filePA){
    # Ler a base de dados dbc
    y = read.dbc::read.dbc(i,as.is=T)
    # Filtra só as linhas que tem PA_CODUNI == CNES_HOSP
    y = subset(y,PA_CODUNI %in% CNES_HOSP)
    # ADD essa nova base de dados "y" ao final do dataframe "PA"
    PA = plyr::rbind.fill(PA,y)
  }


  #----HOSPITALAR -------------------------


  RD = data.frame()
  for(i in fileRD){
    y = read.dbc::read.dbc(i,as.is=T)
    y = subset(y,CNES %in% CNES_HOSP)
    RD = plyr::rbind.fill(RD,y)
  }


  RJ = data.frame()
  for(i in fileRJ){
    y = read.dbc::read.dbc(i,as.is=T)
    y = subset(y,CNES %in% CNES_HOSP)
    RJ = plyr::rbind.fill(RJ,y)
  }


  ER = data.frame()
  for(i in fileER){
    y = read.dbc::read.dbc(i,as.is=T)
    y = subset(y,CNES %in% CNES_HOSP)
    ER = plyr::rbind.fill(ER,y)
  }


  SP = data.frame()
  for(i in fileSP){
    y = read.dbc::read.dbc(i,as.is=T)
    y = subset(y,SP_CNES %in% CNES_HOSP)
    SP = plyr::rbind.fill(SP,y)
  }

  dataER = ER
  dataPA = PA
  dataRD = RD
  dataRJ = RJ
  dataSP = SP

  dataframes <- list(
    dataER = dataER,
    dataPA = dataPA,
    dataRD = dataRD,
    dataRJ = dataRJ,
    dataSP = dataSP
  )

  # Atribuindo o ambiente global do R à variável env
  env <- globalenv()

  # Atribuir os dataframes ao environment global
  list2env(dataframes, envir = env)

  objetos_para_salvar <- c("dataER", "dataPA", "dataRD", "dataRJ", "dataSP")
  save(list = objetos_para_salvar, file = "Data_SIA_SIH.Rdata")

  #Remove todos os arquivos dbc que foram baixados
  arquivos_dbc <- list.files(path = caminho, pattern = "\\.dbc$", full.names = TRUE)
  for (arquivo in arquivos_dbc) {
    file.remove(arquivo)
    #print(paste("Arquivo", basename(arquivo), "removido com sucesso."))
  }

}
