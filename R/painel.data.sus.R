
#' painel.data.sus: Funcao principal
#'
#' @description Esta funcao permite executar todas as outras funcaes do pacote "PainelDataSUSGustavo" de uma vez so.
#'
#' @param UF uma strign, nome do estado
#' @param inicio um number de 6 digitos, os 4 primeiro sao do ano e os 2 ultimos sao do mes
#' @param final um number de 6 digitos, os 4 primeiro sao do ano e os 2 ultimos sao do mes
#' @param dir Uma string indicando o diretório de saída onde os arquivos serão armazenados (o padrão é o diretório criado dentro da pasta Documentos do Usuário).
#'
#' @export
#'
#' @examples
#' painel.data.sus('CE',202301,202302,".")
#'
painel.data.sus = function(UF,inicio, final, dir = "."){

  #Verifica se o pacote "pacman"esta instalado, se nao estiver ele é instaldo
  if (!require(pacman)) {
    install.packages("pacman")
    library(pacman)
  }
  #Verifica se o pacote "read.dbc"esta instalado, se nao estiver ele é instaldo
  if (!require(read.dbc)) {
    cat('O pacote \"Read.dbc\" precisar ser instalodo antes de chamar a funcao \"painel.data.sus\"')
    break
    #install.packages("read.dbc")
    #library(read.dbc)
  }

  pacman::p_load(read.dbc, stringr, dplyr, openxlsx)
  dirAtual = getwd()

  # Defina o diretório de destino para baixar arquivos
  dir_destino <- file.path(dir, "SIM")

  #Verifique a existência do diretório de destino, caso contrário, crie-o
  if (!dir.exists(dir_destino)) {
    dir.create(dir_destino, recursive = TRUE)
    cat(paste0("O diretório ", dir_destino, " foi criado.\n"))
  }

  # Informe o diretório
  cat(paste0("Os arquivos serão salvos em: ", dir_destino, "\n"))
  setwd(dir_destino)

  download.dbc(UF,inicio, final)

  cat(paste0("Lendo os arquivos DBC que foram baixados.\n"))
  ler.dbc(UF,getwd())

  cat(paste0("Criando dataset do painel.\n"))
  script.painel(UF)

  # Full path of the file "Painel_SIM.pbix"
  caminho_pasta <- system.file("extdata", package = "PainelDataSUSGustavo")
  caminho_completo <- file.path(caminho_pasta, "Painel_DATASUS_HU_UFSC.pbix")

  print(dir_destino)

  # Check if the file exists
  if (file.exists(caminho_completo)) {
    # Move the file to the destination directory
    setwd('..')
    novo_caminho_completo <- file.path(dir_destino, "Painel_DATASUS_HU_UFSC.pbix")
    file.copy(caminho_completo, novo_caminho_completo)
    cat("Arquivo movido com sucesso para:", novo_caminho_completo, "\n")
  } else {cat("O arquivo 'Painel_DATASUS_HU_UFSC.pbix' não foi encontrado no diretório:", caminho_pasta, "\n")}

  setwd(dirAtual)

}



