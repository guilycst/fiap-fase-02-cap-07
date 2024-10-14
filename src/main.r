# GuilhermeCastro_RM560054_fase2_cap7

if(!require("readr")) install.packages("readr", dependencies=TRUE)
if(!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
if(!require("dplyr")) install.packages("dplyr", dependencies=TRUE)

library(readr)
library(ggplot2)
library(dplyr)

dados <- read_delim("serie_historica_cana.csv", delim = ";", locale = locale(decimal_mark = ","))

dados$area_plantada_mil_ha <- as.numeric(gsub(",", ".", dados$area_plantada_mil_ha))

area_plantada <- dados$area_plantada_mil_ha
media_area <- mean(area_plantada, na.rm = TRUE)
mediana_area <- median(area_plantada, na.rm = TRUE)
moda_area <- as.numeric(names(sort(table(area_plantada), decreasing = TRUE)[1]))
desvio_padrao_area <- sd(area_plantada, na.rm = TRUE)
variancia_area <- var(area_plantada, na.rm = TRUE)
quartis_area <- quantile(area_plantada, na.rm = TRUE)

cat("Média da Área Plantada:", media_area, "\n")
cat("Mediana da Área Plantada:", mediana_area, "\n")
cat("Moda da Área Plantada:", moda_area, "\n")
cat("Desvio Padrão da Área Plantada:", desvio_padrao_area, "\n")
cat("Variância da Área Plantada:", variancia_area, "\n")
cat("Quartis da Área Plantada:\n", quartis_area, "\n")

if (!"escala_producao" %in% colnames(dados)) {
  dados$escala_producao <- cut(dados$producao_mil_t,
                               breaks = quantile(dados$producao_mil_t, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                               labels = c("Pequena Producao", "Media Producao", "Grande Producao"),
                               include_lowest = TRUE)
}

dados_contagem <- dados %>%
  group_by(ano_agricola, escala_producao) %>%
  summarise(contagem = n())

ggplot(dados_contagem, aes(x = ano_agricola, y = contagem, color = escala_producao, group = escala_producao)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Quantidade de Escala de Produção por Ano Agrícola", x = "Ano Agrícola", y = "Quantidade") +
  scale_color_manual(values = c("Pequena Producao" = "lightblue", "Media Producao" = "lightgreen", "Grande Producao" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
