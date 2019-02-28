##======================================================================
## Criando pacote com devtools e roxygen2
##======================================================================

##----------------------------------------------------------------------
## Pacotes necessarios
library(devtools)
library(roxygen2)

##----------------------------------------------------------------------
## 1) Criando a estrutura (esqueleto) do pacote
create("DataDriven", rstudio = FALSE)
# irá criar o diretório 'meupacote' com man/, R/ e DESCRIPTION
# Edite o arquivo DESCRIPTION com as informações do seu pacote

##----------------------------------------------------------------------
## 2) Crie funções e carregue com
load_all()
# cada vez que criar ou alterar uma função, use esse comando para
# carregá-las no seu workspace e testá-las. Crie funções em arquivos com
# extensão .R dentro do diretório R/

##----------------------------------------------------------------------
## 3) Documente as funções usando as tags do roxygen2 e use a função
document()
# para gerar os arquivos com extensão .Rd correspondentes no diretório
# man/. Use também a função
check_man()
# para conferir a documentação

##----------------------------------------------------------------------
## 4) Certifique-se de que o arquivo NAMESPACE esteja contendo todas as
## funções do seu pacote a serem exportadas, e todas as funções (ou
## pacotes inteiros) a serem importadas para o seu pacote. Use as tags
## @export para exportar e @import ou @importFrom para importar (na
## documentação roxygen2 de cada função). Rode
document()
# sempre que exportar ou importar uma função, para que o arquivo
# NAMESPACE seja atualizado.

##----------------------------------------------------------------------
## 5) Confira o pacote com
check()
# e construa o pacote final com
build()

