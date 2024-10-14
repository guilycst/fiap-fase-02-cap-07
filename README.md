# FIAP - Faculdade de Informática e Administração Paulista

<p align="center">
<a href= "https://www.fiap.com.br/"><img src="assets/logo-fiap.png" alt="FIAP - Faculdade de Informática e Admnistração Paulista" border="0" width=40% height=40%></a>
</p>

<br>

# Nome do projeto

## Nome do grupo

## 👨‍🎓 Integrantes: 
- <a href="https://www.linkedin.com/company/inova-fusca">Guilherme de Castro</a>

## 📜 Descrição

### ANÁLISE ESTATÍSTICA DE DADOS DO AGRO
Atenção Atividade Avaliativa:

- Verifique se o arquivo do upload está correto, não é possível enviar um outro arquivo após fechamento da entrega na plataforma ou correção do professor.

- Não deixe para realizar a entrega da atividade nos últimos minutos do prazo, você pode ter algum problema e perder a entrega. As entregas são realizadas apenas pela plataforma.

- Não disponibilize a resposta da sua atividade em grupos de WhatsApp, Discord, Microsoft Teams, pois pode gerar plágio e zerar a atividade para todos.

- Você tem um período máximo de 15 dias após a publicação da nota para solicitar a revisão da correção.

Contextualização: Agronegócio

O agronegócio é um setor econômico que engloba todas as atividades relacionadas à produção, comercialização e distribuição de produtos agrícolas. É uma área vital para a economia global, especialmente em países onde a agricultura desempenha um papel significativo na geração de riqueza e na alimentação da população.

### Componentes do Agronegócio:

Produção Agrícola: engloba todas as atividades relacionadas ao cultivo de plantas (agrícolas, frutíferas etc.) e à criação de animais para produção de alimentos, fibras e outros produtos.
Agroindústria: processamento, transformação e industrialização de produtos agrícolas em alimentos processados, rações, bioenergia, entre outros.
Distribuição e Logística: inclui o transporte, armazenamento e distribuição dos produtos agrícolas, garantindo que cheguem aos mercados consumidores de forma eficiente.
Comercialização: envolve a venda e compra de produtos agrícolas, tanto no mercado interno quanto no internacional, abrangendo operações de importação e exportação.
Importância do Agronegócio:

Segurança alimentar: o agronegócio é fundamental para garantir o abastecimento de alimentos para a população mundial, desempenhando um papel crucial na segurança alimentar global.
Geração de empregos: é um dos maiores empregadores em muitas regiões, oferecendo oportunidades de trabalho tanto no campo quanto na agroindústria e nos serviços relacionados.
Contribuição econômica: contribui significativamente para o Produto Interno Bruto (PIB) de muitos países, especialmente aqueles onde a agricultura é uma parte essencial da economia.
Desenvolvimento regional: o agronegócio muitas vezes é um motor para o desenvolvimento rural, ajudando a melhorar a infraestrutura e o padrão de vida nas áreas agrícolas.
Inovação e tecnologia: está cada vez mais integrado com avanços tecnológicos, como a agricultura de precisão, biotecnologia e novas técnicas de manejo, impulsionando a eficiência e sustentabilidade.
Desafios e Tendências:

Sustentabilidade: a pressão por práticas agrícolas sustentáveis, conservação de recursos naturais e mitigação das mudanças climáticas são desafios importantes.
Globalização: o aumento do comércio internacional e a necessidade de conformidade com padrões globais de qualidade e segurança alimentar.
Digitalização: a adoção de tecnologias digitais para monitoramento, gestão de culturas, automação e análise de dados está transformando o agronegócio.

1. Pesquise nas seguintes fontes de dados públicos, dados relacionados ao agronegócio:

CONAB (Companhia Nacional de Abastecimento)
https://www.conab.gov.br/

IBGE (Instituto Brasileiro de Geografia e Estatística)
https://www.ibge.gov.br/

MAPA (Ministério da Agricultura, Pecuária e Abastecimento)
https://www.gov.br/agricultura/pt-br

Embrapa (Empresa Brasileira de Pesquisa Agropecuária)
https://www.embrapa.br/

INPE (Instituto Nacional de Pesquisas Espaciais)
https://www.gov.br/inpe/pt-br

CNA BRASIL (Confederação da Agricultura e Pecuária do Brasil)
https://www.cnabrasil.org.br/

 

2. Crie uma base de dados em Excel contendo, pelo menos, 30 linhas e 4 colunas, sendo as colunas:

Uma variável quantitativa discreta;
Uma variável quantitativa contínua;
Uma variável qualitativa nominal;
Uma variável qualitativa ordinal.
 

3. Escolha uma variável quantitativa e faça uma análise exploratória dela em R contendo:

As Medidas de Tendência Central;
As Medidas de Dispersão;
As Medidas Separatrizes;
Uma análise gráfica.
 

4. Escolha uma variável qualitativa e faça uma análise gráfica dela em R.

### Entregáveis:
O arquivo Excel com a base de dados;

O arquivo R com os códigos utilizados, contendo na primeira linha do código a # com o seu nome completo, RM, fase e capítulo, exemplo:

# JoaoSantos_RM76332_fase2_cap9


## Análise do gráfica [visualização](./Rplots.pdf)

O gráfico gerado mostra a quantidade de registros por escala de produção (Pequena, Média e Grande Produção) ao longo dos anos agrícolas por estado brasileiro. A análise do gráfico revela algumas tendências:

**Crescimento da Grande Produção:**

Podemos observar que, em alguns anos, há um aumento de Grande Produção, indicando uma concentração de maiores produtores.
Esse aumento pode estar relacionado ao crescimento de grandes operações agrícolas ao longo do tempo.

**Flutuação da Média Produção:**

A Média Produção parece ter uma variação ao longo dos anos, com alguns períodos apresentando um aumento ou diminuição nos registros.
Isso pode indicar uma transição de produtores médios para escalas maiores ou menores.

**Pequena Produção:**

A Pequena Produção também flutua, com anos de maior e menor presença nos registros, sugerindo que pequenos produtores podem estar enfrentando desafios ao longo do tempo ou sendo substituídos por operações maiores.

**Conclusão:**
A evolução das escalas de produção ao longo do tempo indica uma dinâmica interessante no setor agrícola, com possíveis transições de pequenos e médios produtores para grandes operações. As flutuações podem ser influenciadas por políticas agrícolas, avanços tecnológicos e fatores econômicos.




## 📁 Estrutura de pastas

Dentre os arquivos e pastas presentes na raiz do projeto, definem-se:
```
.
├── ANALISE_GRAFICA.MD          --Analise do gráfico
├── assets
│   └── logo-fiap.png
├── README.md
├── Rplots.pdf                  --Gráfico
├── serie_historica_cana.csv    --dataset
├── serie_historica_cana.xlsx   --dataset
└── src
    └── main.r                  --script r
```

## 🔧 Como executar o código

Executar do diretório raiz no terminal:

```
Rscript main.R
```

O arquivo Rplots.pdf vai ser gerado, demais métricas são impressas no terminal.

## 📋 Licença

<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1"><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1"><p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><a property="dct:title" rel="cc:attributionURL" href="https://github.com/agodoi/template">MODELO GIT FIAP</a> por <a rel="cc:attributionURL dct:creator" property="cc:attributionName" href="https://fiap.com.br">Fiap</a> está licenciado sobre <a href="http://creativecommons.org/licenses/by/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">Attribution 4.0 International</a>.</p>


