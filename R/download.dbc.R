
#' download.dbc: Baixa dados.dbc do site DataSUS
#'
#' @description A funcao download.dbc acessa o site do DataSUS para baixar os dados de Producao Hospitalar (SIH) e Producao Ambulatorial (SIA) de um estado especifico dentro de uma faixa de tempo determinada.
#'
#' @param UF uma strign, nome do estado
#' @param inicio um number de 6 digitos, os 4 primeiro sao do ano e os 2 ultimos sao do mes
#' @param final um number de 6 digitos, os 4 primeiro sao do ano e os 2 ultimos sao do mes
#'
#' @export
#'
#' @examples
#' download.dbc("CE",202301,202302)
#'
download.dbc = function(UF,inicio, final){
  periodo = inicio:final
  #Verifica se o mes informado é maior que 0 e menor igual que 12
  periodo = periodo[as.numeric(substr(periodo,5,6)) > 0 & as.numeric(substr(periodo,5,6)) <= 12]
  #Remove os dois primeiro digitos de cada ano
  periodo = substr(periodo,3,6)

  #Faz o download dos dados dbc
  download.dbc2FTP <- function(UF,ANOMES,TIPO){
    arquivo <- paste0(TIPO,UF,ANOMES,".dbc")
    if( TIPO == "PA" ) {
      url <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/",arquivo)

    } else if( TIPO == "ER" | TIPO == "RD" | TIPO == "RJ" | TIPO == "SP") {
      url <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/",arquivo)
    }
    download.file(url, destfile = arquivo, mode = "wb")
    #"wd"(write binary) especifica o modo de abertura dos dados.
  }

  #Para cada mes existe 5 bases de dados (arquivos) dbc
  #Executando o download para cada mes

  #Produção Ambulatorial (SIA)
  sapply(1:length(periodo),function(i)download.dbc2FTP(UF,periodo[i],"PA"))

  #Produção Hospitalar (SIH)
  sapply(1:length(periodo),function(i)download.dbc2FTP(UF,periodo[i],"RD"))
  sapply(1:length(periodo),function(i)download.dbc2FTP(UF,periodo[i],"RJ"))
  sapply(1:length(periodo),function(i)download.dbc2FTP(UF,periodo[i],"ER"))
  sapply(1:length(periodo),function(i)download.dbc2FTP(UF,periodo[i],"SP"))
}


