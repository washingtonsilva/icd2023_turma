

# Descricao ---------------------------------------------------------------

##########################################################################
## Disciplina: Introcucao a Ciencia dos Dados                            #
## Descricao: Aula 1 em 09/03/2022                                       #
## Autor: Seu Nome                                                       #
## Ultima alteracao: 09/03/2022                                          #
##########################################################################

# Pacotes utilizados ------------------------------------------------------

## instalar pacotes - apenas uma vez

install.packages("readr", "readxl", "dplyr", "writexl", "xts", "ggplot2", 
                 "ggthemes", "plotly", "dygraphs")

## Ativando pacotes/blbliotecas

library(readr)  
library(dplyr)
library(readxl)
library(tidyr)
library(writexl)    
library(xts)
library(ggplot2)
library(ggthemes)   
library(plotly)     
library(dygraphs)   

# Importando arquivo csv --------------------------------------------------

## endereco (path) do arquivo de dados
file_csv <- "/Users/wss/github/icd2023_mestrado/scripts/aula01/dados_brutos/ipeadata[06-04-2021-04-16].csv"

## importando com readr::csv2()
cambio_csv <- read_csv2(file_csv,
                    na = c(" "),
                    skip = 1,
                    locale = locale(decimal_mark = ","),
                    col_names = c("dia", "real_dolar"),
                    col_types = cols(
                    dia = col_date(format = "%d/%m/%Y"),
                    real_dolar = col_double())
                    )

## verificando estrutura
cambio_csv
View(cambio_csv)
dplyr::glimpse(cambio_csv)
summary(cambio_csv)

## retirando NA - missing data = dados faltantes
cambio_csv <- cambio_csv %>% select(dia, real_dolar) %>% drop_na()
cambio_csv
summary(cambio_csv)

## salvando o objeto como arquivo binario R
path1 <- "dados_analise/cambio.rds"
write_rds(cambio_csv, path1)

## Importando o arquivo binario R 
cambio <- read_rds(path3)
cambio


# Importando planilhas MS Excel ------------------------------------------

## endereco (path) do arquivo de dados
file_xls <- "dados_brutos/ipeadata[06-04-2021-09-33].xls"

## importando com readxl::read_xls()
cambio_xls <- readxl::read_xls(file_xls,
                               skip = 1,
                               sheet = "Séries",
                               col_names = c("dia", "real_dolar"),
                               col_types = "text"
                               )
cambio_xls
View(cambio_xls)
class(cambio_xls)
dplyr::glimpse(cambio_xls)

## ajustando a classe das variaveis dia e real_dolar
cambio_serie <- cambio_xls %>%
  transmute(
         dia = as.Date(dia, format = "%d/%m/%Y"),
         real_dolar = as.numeric(gsub(',','.', real_dolar))
            )
cambio_serie
dplyr::glimpse(cambio_serie)
summary(cambio_serie)

## Tentem retirar os dados faltantes!

## salvando o objeto como arquivo binario R
path2 <- "dados_analise/cambio_serie.rds"
write_rds(cambio_serie, path2)

## Importando o arquivo binario R 
cambio <- read_rds(path3)
cambio

# Exploratory Data Analysis -----------------------------------------------

## grafico de linha basico

cambio_csv %>%
  ggplot(aes(x = dia, y = real_dolar)) +
  geom_line(color = "#69b3a2") +
  labs(
    title = "Taxa de Câmbio R$/US$",
    subtitle = "comercial - compra - média",
    caption = "Fonte: ipeadata - Bacen/SGS",
    x = "dia",
    y = "Taxa de Câmbio (R$/US$)"
  ) +
  theme_minimal()

## grafico de linha + area

p1 <- cambio_csv %>%
  ggplot(aes(x = dia, y = real_dolar)) +
  geom_area(fill = "#69b3a2", alpha = 0.5) +
  geom_line(color = "#69b3a2") +
  labs(title = "Taxa de Câmbio R$/US$",
       subtitle = "comercial - compra - média",
       caption = "Fonte: ipeadata - Bacen/SGS",
       x = "dia",
       y = "Taxa de Câmbio (R$/US$)") +
  theme_minimal()
p1

## grafico interativo com o pacote ggplotly

p2 <- cambio_csv %>%
  ggplot(aes(x = dia, y = real_dolar)) +
  geom_area(fill = "#69b3a2", alpha = 0.5) +
  geom_line(color = "#69b3a2") +
  xlab("dia") +
  ylab("Taxa de Câmbio (R$/US$)") +
  theme_minimal()
p2

p2 <- ggplotly(p2)
p2

## grafico interativo com o pacote dygraph

## convertendo a serie para xts
cambio_st <- xts(x = cambio_csv$real_dolar, order.by = cambio_csv$dia)
class(cambio_st)
plot(cambio_st)

p3 <- dygraph(data,
              main = "Taxa de Câmbio R$/US$ - comercial - compra - média",
              xlab = "dia",
              ylab = "Taxa de Câmbio R$/US$") %>%
  dySeries("V1", label = "Taxa de Câmbio") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyOptions(fillGraph = TRUE ) %>%
  dyRangeSelector()
p3

## Exercicio: Sua Vez
## Importe o arquivo UKHP.xls e faca um grafico de linha





