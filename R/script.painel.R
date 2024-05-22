
#' script.painel: Script do Painel do Power BI
#'
#' @description Essa funcao faz um tratamento na base de dados que vai ser usada no painel do Power BI. Essa base de dados foi criada pela funcao ler.dbc.R.
#'
#' @param UF Uma String, Nome do estado
#'
#' @export
#'
#' @examples
#' script.painel("CE")
#'
script.painel = function(UF){
  options(xlsx.datetime.format="dd/mm/yyyy")

  load(file="Data_SIA_SIH.Rdata")


  if(UF=='CE'){
    CNES_HOSP = c('2611686')
  } else if(UF=='SC'){
    CNES_HOSP = c('3157245')
  } else {
    CNES_HOSP = c('3157245')
  }

  HOSPITAL = c("HU_UFSC")


  #$ Load Produção Ambulatorial

  dataPA = dataPA %>%
    select(PA_CODUNI,
           PA_MVM,
           PA_CMP,
           PA_PROC_ID,
           PA_TPFIN,
           PA_SUBFIN,
           PA_NIVCPL,
           PA_QTDPRO,
           PA_QTDAPR,
           PA_VALPRO,
           PA_VALAPR,
           PA_DOCORIG,
           PA_CBOCOD,
           PA_CNPJCPF)

  #$ __________________________________________________

  #$ Ambulatorial Data

  #$ Padronização dos campos: Produção Ambulatorial

  dataPA <- rename(dataPA,
                   CNES = 'PA_CODUNI',
                   ANOMES_MVM = 'PA_MVM',
                   ANOMES_CMP = 'PA_CMP',
                   PROC_COD = 'PA_PROC_ID',
                   PROC_TIPO_FINANC_COD = 'PA_TPFIN',
                   PROC_SUB_FINANC_COD = 'PA_SUBFIN',
                   PROC_COMPLEX_COD = 'PA_NIVCPL',
                   QTD = 'PA_QTDPRO',
                   QTD_APR = 'PA_QTDAPR',
                   VAL = 'PA_VALPRO',
                   VAL_APR = 'PA_VALAPR',
                   CBO = 'PA_CBOCOD')

  dataPA$QTD_ATO_PROF = dataPA$QTD

  dataPA$DATA_CMP = as.Date(str_trim(paste('01',dataPA$ANOMES_CMP, sep='',collapse = NULL)),format = "%d%Y%m")

  dataPA$DATA_MVM = as.Date(str_trim(paste('01',dataPA$ANOMES_MVM, sep='',collapse = NULL)),format = "%d%Y%m")

  dataPA$BASE = "Ambulatorial"

  #$ Assign define registro
  dataPA = dataPA %>% mutate(
    PROC_REGISTRO = case_when(
      PA_DOCORIG == "C" ~ "BPA - Consolidado",
      PA_DOCORIG == "I" ~ "BPA - Individualizado",
      PA_DOCORIG == "P" ~ "APAC - Procedimento Principal",
      PA_DOCORIG == "S" ~ "APAC - Procedimento Secundário"
    ))

  # SubFinanciamento
  dataPA = dataPA %>%
    mutate(
      PROC_SUB_FINANC_COD = if_else(PROC_SUB_FINANC_COD == "0000" | is.na(PROC_SUB_FINANC_COD), NA_character_,
                                    paste0(PROC_TIPO_FINANC_COD, PROC_SUB_FINANC_COD))
    )

  #$ Union both datasets

  producao = dataPA

  rm(dataPA)

  producao = producao %>%
    mutate(
      HOSPITAL = case_when(
        CNES == CNES_HOSP ~ HOSPITAL
      )
    )

  producao$ANO_CMP = format(producao$DATA_CMP, "%Y")

  producao$ANO_MVM = format(producao$DATA_MVM, "%Y")

  producao$MES_CMP = as.integer(format(producao$DATA_CMP, "%m"))

  producao$MES_MVM = as.integer(format(producao$DATA_MVM, "%m"))

  # Separar grupos, subgrupos e forma de organização

  producao$PROC_GRUPO_COD = strtrim(producao$PROC_COD, 2)
  producao$PROC_SUB_GRUPO_COD = strtrim(producao$PROC_COD, 4)
  producao$PROC_FORMA_ORG_COD = strtrim(producao$PROC_COD, 6)

  #tabela =
  #  producao  %>% dplyr::filter(PROC_FORMA_ORG_COD %in% c("040601") ) %>%
  #  dplyr::group_by(BASE, PROC_FORMA_ORG_COD, Ano) %>%
  #  dplyr::summarise(PRO = sum(QTD, na.rm = T)) %>%
  #  as.data.frame()

  #$$$$$$$$$$$$$$$$$$$$$$$$$$$

  arquivo_rdata = system.file("data", "tab_procedures_SIGTAP_SUS.RData", package = "PainelDataSUSGustavo")

  load(arquivo_rdata)


  rm(tb_procedimento_all_dupli)

  # Match with procedimento
  producao$PROC_NM = tb_procedimento_all_primary[match(producao$PROC_COD,
                                                       tb_procedimento_all_primary$PROC_COD),"PROC_NM"]

  # Match with groups
  producao$PROC_GRUPO_NM = tb_grupo[match(producao$PROC_GRUPO_COD,
                                          tb_grupo$PROC_GRUPO_COD),"PROC_GRUPO_NM"]

  # Match with sub-groups
  producao$PROC_SUB_GRUPO_NM = tb_sub_grupo[match(producao$PROC_SUB_GRUPO_COD,
                                                  tb_sub_grupo$PROC_SUB_GRUPO_COD),"PROC_SUB_GRUPO_NM"]

  # Match with Forma-Organizacao
  producao$PROC_FORMA_ORG_NM = tb_forma_organizacao[match(producao$PROC_FORMA_ORG_COD,
                                                          tb_forma_organizacao$PROC_FORMA_ORG_COD),"PROC_FORMA_ORG_NM"]

  # Match with Complexidade
  producao$PROC_COMPLEX_NM = tb_complexidade[match(producao$PROC_COMPLEX_COD,
                                                   tb_complexidade$PROC_COMPLEX_COD),"PROC_COMPLEX_NM"]

  # Match with Financiamento
  producao$PROC_TIPO_FINANC_NM = tb_financiamento[match(producao$PROC_TIPO_FINANC_COD,
                                                        tb_financiamento$PROC_TIPO_FINANC_COD),"PROC_TIPO_FINANC_NM"]

  # Match with Rubrica and SubFinanciamento
  producao$PROC_SUB_FINANC_NM = tb_rubrica[match(producao$PROC_SUB_FINANC_COD,tb_rubrica$PROC_SUB_FINANC_COD),"PROC_SUB_FINANC_NM"]

  # Match with Ocupacao
  producao$CBO_PROF = tb_ocupacao[match(producao$CBO, tb_ocupacao$CO_OCUPACAO),"NO_OCUPACAO"]


  #$ Incluir COmplexidade e Financiamento pelo SigTAP

  # Match with procedimento
  producao$PROC_COMPLEX_COD_SIGTAP = tb_procedimento_all_primary[match(producao$PROC_COD,
                                                                       tb_procedimento_all_primary$PROC_COD),"PROC_COMPLEX_COD"]

  # Match with procedimento
  producao$PROC_TIPO_FINANC_COD_SIGTAP = tb_procedimento_all_primary[match(producao$PROC_COD,
                                                                           tb_procedimento_all_primary$PROC_COD),"PROC_TIPO_FINANC_COD"]

  #$ Subfinanciamento
  producao$PROC_SUB_FINANC_COD_SIGTAP = tb_procedimento_all_primary[match(producao$PROC_COD,
                                                                          tb_procedimento_all_primary$PROC_COD),"PROC_SUB_FINANC_COD"]

  # Match with procedimento
  producao$PROC_COMPLEX_NM_SIGTAP = tb_procedimento_all_primary[match(producao$PROC_COD,
                                                                      tb_procedimento_all_primary$PROC_COD),"PROC_COMPLEX_NM"]

  # Match with procedimento
  producao$PROC_TIPO_FINANC_NM_SIGTAP = tb_procedimento_all_primary[match(producao$PROC_COD,
                                                                          tb_procedimento_all_primary$PROC_COD),"PROC_TIPO_FINANC_NM"]

  #$ Subfinanciamento
  producao$PROC_SUB_FINANC_NM_SIGTAP = tb_procedimento_all_primary[match(producao$PROC_COD,
                                                                         tb_procedimento_all_primary$PROC_COD),"PROC_SUB_FINANC_NM"]

  #$ Subfinanciamento
  producao$PROC_TIPO_PROGRAMACAO_SIGTAP = tb_procedimento_all_primary[match(producao$PROC_COD,
                                                                            tb_procedimento_all_primary$PROC_COD),"PROC_TIPO_PROGRAMACAO_SIGTAP"]

  ##$$ Match SIGtap
  setmatchSIGtap = tb_procedimento_all_primary[match(producao$PROC_COD, tb_procedimento_all_primary$PROC_COD),
                                               c("VL_SA_num","VL_SH_num","VL_SP_num","VL_TO_num","VL_TOTAL")]

  producao = cbind(producao, setmatchSIGtap)
  rm(setmatchSIGtap)

  # removes whitespace from start and end of string

  producao = producao %>%
    mutate(
      PROC_NM = str_trim(PROC_NM),
      PROC_GRUPO_NM = str_trim(PROC_GRUPO_NM),
      PROC_SUB_GRUPO_NM = str_trim(PROC_SUB_GRUPO_NM),
      PROC_FORMA_ORG_NM = str_trim(PROC_FORMA_ORG_NM),
      PROC_COMPLEX_NM = str_trim(PROC_COMPLEX_NM),
      PROC_TIPO_FINANC_NM = str_trim(PROC_TIPO_FINANC_NM),
      PROC_SUB_FINANC_NM = str_trim(PROC_SUB_FINANC_NM),
      CBO_PROF = str_trim(CBO_PROF),

      PROC_COMPLEX_NM_SIGTAP = str_trim(PROC_COMPLEX_NM_SIGTAP),
      PROC_TIPO_FINANC_NM_SIGTAP = str_trim(PROC_TIPO_FINANC_NM_SIGTAP),
      PROC_SUB_FINANC_NM_SIGTAP = str_trim(PROC_SUB_FINANC_NM_SIGTAP))


  #$ Selecionar

  outputSIA = producao %>%
    select(
      HOSPITAL,
      DATA_CMP,
      ANOMES_CMP,
      ANO_CMP,
      MES_CMP,
      DATA_MVM,
      ANOMES_MVM,
      ANO_MVM,
      MES_MVM,

      BASE,

      QTD,
      VAL,
      QTD_APR,
      VAL_APR,
      QTD_ATO_PROF,

      PROC_NM,
      PROC_GRUPO_NM,
      PROC_SUB_GRUPO_NM,
      PROC_FORMA_ORG_NM,
      PROC_TIPO_FINANC_NM,
      PROC_SUB_FINANC_NM,
      PROC_COMPLEX_NM,

      CBO_PROF,
      PROC_REGISTRO
    )

  rm(producao)

  # Rename

  outputSIA =
    outputSIA %>%
    rename(
      Hospital = HOSPITAL,
      `Mês/Ano de realiz.` = DATA_CMP,
      `AnoMês_CMP` = ANOMES_CMP,
      `Ano de realiz.` = ANO_CMP,
      `Mês de realiz.` = MES_CMP,
      `Mês/Ano de proces.` = DATA_MVM,
      `AnoMês_MVM` = ANOMES_MVM,
      `Ano de proces` = ANO_MVM,
      `Mês de proces.` = MES_MVM,
      `Produção` = BASE,
      `Qtde. produzida` = QTD,
      `Financeiro produzido` = VAL,
      `Qtde. aprovada` = QTD_APR,
      `Financeiro aprovado` = VAL_APR,
      `Qtde. atos prof` = QTD_ATO_PROF,

      Procedimentos = PROC_NM,
      Grupo = PROC_GRUPO_NM,
      `Sub-grupo` = PROC_SUB_GRUPO_NM,
      `Forma de organização` = PROC_FORMA_ORG_NM,
      `Tipo de financiamento` = PROC_TIPO_FINANC_NM,
      `Tipo de sub-financiamento` = PROC_SUB_FINANC_NM,
      Complexidade = PROC_COMPLEX_NM,

      `CBO do profissional` = CBO_PROF,
      `Tipo de registro` = PROC_REGISTRO

    )

  outputSIA =
    outputSIA %>%
    filter(`Produção` == "Ambulatorial") %>%
    select(
      Hospital,
      Procedimentos,
      Grupo,
      `Sub-grupo`,
      `Forma de organização`,
      `Tipo de financiamento`,
      `Tipo de sub-financiamento`,
      Complexidade,
      `Qtde. produzida`,
      `Financeiro produzido`,
      `Qtde. aprovada`,
      `Financeiro aprovado`,
      `Mês/Ano de realiz.`,
      `Ano de realiz.`,
      `Mês de realiz.`,
      `Mês/Ano de proces.`,
      `Ano de proces`,
      `Mês de proces.`,
      `CBO do profissional`,
      `Tipo de registro`)

  #$ ________________________________
  #$ ________________________________
  #$ ________________________________

  #$ Work with Aproved and Rejected data.

  #$ Trabalhar com as bases de AIH's

  dataRD$TIPO = "Aprovada"
  dataRJ$TIPO = "Rejeitada"

  RD_RJ = bind_rows(dataRD, dataRJ)
  # rm(dataRD, dataRJ)

  #$ Selecionar principais variaveis de trabalho

  RD_RJ = RD_RJ %>%
    select(c(CNES,
             N_AIH, SEQUENCIA, REMESSA,
             TIPO, ANO_CMPT, MES_CMPT, DT_INTER, DT_SAIDA, IDENT, SEQ_AIH5, DIAS_PERM,
             FINANC, FAEC_TP, COMPLEX,
             PROC_REA, DIAG_PRINC, ESPEC,
             VAL_SH,VAL_SP,VAL_TOT))

  # rm(CNES)

  # Criar variável hospital
  RD_RJ$HOSPITAL[RD_RJ$CNES == CNES_HOSP[1]] = HOSPITAL[1]
  #RD_RJ$HOSPITAL[RD_RJ$CNES == CNES_HOSP[2]] = HOSPITAL[2]

  # Cod procedimento
  RD_RJ$PROC_COD = RD_RJ$PROC_REA

  # Separar grupos, subgrupos e forma de organização
  RD_RJ$PROC_GRUPO_COD=strtrim(RD_RJ$PROC_REA,2)
  RD_RJ$PROC_SUB_GRUPO_COD=strtrim(RD_RJ$PROC_REA,4)
  RD_RJ$PROC_FORMA_ORG_COD=strtrim(RD_RJ$PROC_REA,6)

  # Complexidade e Financiamento
  RD_RJ$PROC_TIPO_FINANC_COD = RD_RJ$FINANC
  RD_RJ$PROC_COMPLEX_COD = substr(RD_RJ$COMPLEX, start = 2, stop = 2)

  # CID
  RD_RJ$PROC_CID10 = RD_RJ$DIAG_PRINC

  # SubFinanciamento
  RD_RJ$PROC_SUB_FINANC_COD = RD_RJ$FAEC_TP

  # Match with procedimento
  RD_RJ$PROC_NM = tb_procedimento_all_primary[match(RD_RJ$PROC_COD,tb_procedimento_all_primary$PROC_COD),"PROC_NM"]

  # Match with groups
  RD_RJ$PROC_GRUPO_NM=tb_grupo[match(RD_RJ$PROC_GRUPO_COD,tb_grupo$PROC_GRUPO_COD),"PROC_GRUPO_NM"]

  # Match with sub-groups
  RD_RJ$PROC_SUB_GRUPO_NM=tb_sub_grupo[match(RD_RJ$PROC_SUB_GRUPO_COD,tb_sub_grupo$PROC_SUB_GRUPO_COD),"PROC_SUB_GRUPO_NM"]

  # Match with Forma de Organização
  RD_RJ$PROC_FORMA_ORG_NM=tb_forma_organizacao[match(RD_RJ$PROC_FORMA_ORG_COD,tb_forma_organizacao$PROC_FORMA_ORG_COD),"PROC_FORMA_ORG_NM"]

  # Match with Financiamento
  RD_RJ$PROC_TIPO_FINANC_NM = tb_financiamento[match(RD_RJ$PROC_TIPO_FINANC_COD,tb_financiamento$PROC_TIPO_FINANC_COD),"PROC_TIPO_FINANC_NM"]

  # Match with Rubrica and SubFinanciamento
  RD_RJ$PROC_SUB_FINANC_NM = tb_rubrica[match(RD_RJ$PROC_SUB_FINANC_COD,tb_rubrica$PROC_SUB_FINANC_COD),"PROC_SUB_FINANC_NM"]

  # Match with Complexidade
  RD_RJ$PROC_COMPLEX_NM = tb_complexidade[match(RD_RJ$PROC_COMPLEX_COD,tb_complexidade$PROC_COMPLEX_COD),"PROC_COMPLEX_NM"]

  # Match with CID
  RD_RJ$PROC_CID10_NM = tb_cid[match(RD_RJ$PROC_CID10,tb_cid$PROC_CID10),"PROC_CID10_NM"]

  # Match with ESPEC / LEITO
  RD_RJ$TIPO_LEITO = tb_tipo_leito[match(RD_RJ$ESPEC, tb_tipo_leito$CO_TIPO_LEITO),"TIPO_LEITO"]

  # removes whitespace from start and end of string
  RD_RJ = RD_RJ %>%
    mutate(
      PROC_NM = str_trim(PROC_NM),
      PROC_GRUPO_NM = str_trim(PROC_GRUPO_NM),
      PROC_SUB_GRUPO_NM = str_trim(PROC_SUB_GRUPO_NM),
      PROC_FORMA_ORG_NM = str_trim(PROC_FORMA_ORG_NM),
      PROC_COMPLEX_NM = str_trim(PROC_COMPLEX_NM),
      PROC_TIPO_FINANC_NM = str_trim(PROC_TIPO_FINANC_NM),
      PROC_SUB_FINANC_NM = str_trim(PROC_SUB_FINANC_NM),
      PROC_CID10_NM = str_trim(PROC_CID10_NM),
      TIPO_LEITO = str_trim(TIPO_LEITO)
    )

  rm(tb_complexidade,tb_financiamento,tb_forma_organizacao,tb_grupo,tb_sub_grupo,tb_procedimento_all_primary,tb_rubrica, tb_tipo_leito,
     tb_ocupacao, tb_cid,
     rl_procedimento_habilitacao, rl_procedimento_servico, tb_habilitacao, tb_servico, tb_servico_classificacao)

  # Adicionar a mesma data CMPT para ANO_PROC, MES_PROC e DATA_PROC

  RD_RJ = RD_RJ %>% rename(ANO_CMP = ANO_CMPT, MES_CMP = MES_CMPT)

  RD_RJ$DATA_CMP = as.Date(str_trim(paste('01',RD_RJ$ANO_CMP,RD_RJ$MES_CMP,sep='',collapse = NULL)) ,format = "%d%Y%m")

  # Adicionar data formato AAAAMM
  RD_RJ$ANOMES_CMP = paste0(RD_RJ$ANO_CMP, RD_RJ$MES_CMP)

  #$ ________________________________

  # Criar variável de contagem do número de AIHs
  RD_RJ$countAIH = 1

  RD_RJ$id = 1:nrow(RD_RJ)

  #$ ________________________________

  outputSIH = RD_RJ

  outputSIH = as.data.frame(outputSIH)

  #rm(RD_RJ)

  #$ Selecionar

  outputSIH = outputSIH %>%
    select(
      HOSPITAL,
      CNES,

      PROC_NM,
      PROC_GRUPO_NM,
      PROC_SUB_GRUPO_NM,
      PROC_FORMA_ORG_NM,
      PROC_TIPO_FINANC_NM,
      PROC_SUB_FINANC_NM,
      PROC_COMPLEX_NM,
      PROC_CID10_NM,
      TIPO,
      N_AIH,

      DATA_CMP ,
      ANO_CMP ,
      MES_CMP,

      VAL_TOT,
      countAIH
    )

  #$ Rename

  outputSIH = outputSIH %>%
    rename(
      Hospital = HOSPITAL,

      Procedimentos = PROC_NM,
      Grupo = PROC_GRUPO_NM,
      `Sub-grupo` = PROC_SUB_GRUPO_NM,
      `Forma de organização` = PROC_FORMA_ORG_NM,
      `Tipo de financiamento` = PROC_TIPO_FINANC_NM,
      `Tipo de sub-financiamento` = PROC_SUB_FINANC_NM,
      Complexidade = PROC_COMPLEX_NM,
      `CID-10` = PROC_CID10_NM,
      `Situação da AIH` = TIPO,

      `Nº da AIH` = N_AIH,

      `Financeiro`  = VAL_TOT,
      `Quantidade` = countAIH,

      `Mês/Ano de processamento` = DATA_CMP ,
      `Ano de processamento` = ANO_CMP ,
      `Mês de processamento` = MES_CMP)


  #$ Data do relatório

  data_relatorio = data.frame(DATA = rep(format(Sys.time(),format="%d/%m/%Y %H:%M:%S"),1))

  rm(dataER, dataRD, dataRJ, dataSP, RD_RJ)

  rm(CNES_HOSP, HOSPITAL)


  dataframes <- list(data_relatorio = data_relatorio, outputSIA = outputSIA, outputSIH = outputSIH)

  env <- globalenv()

  # Atribuir os dataframes ao environment global
  list2env(dataframes, envir = env)

  #$ save
  objetos_para_salvar <- c("data_relatorio", "outputSIA", "outputSIH")
  save(list = objetos_para_salvar, file = "dataset_painel.Rdata")
}
