#install.packages("shiny")
#install.packages("fipe")
#install.packages("knitr")
#install.packages("ggplot2")
#install.packages("checkr")
#install.packages("xml2")
#install.packages("httr")
#install.packages("lubridate")
#install.packages("DT")
#install.packages("shinybusy")
Sys.setlocale(locale = "Portuguese")

library(shiny)
library(fipe)
library(knitr)
library(dplyr)
library(ggplot2)
library(checkr)
library(xml2)
library(httr)
library(lubridate)
library(DT)
library(shinybusy)

# civic_lxl <- fipe_vehicle(model = "Civic Sed. LXL/ LXL SE 1.8 Flex 16V Aut.", make = "Honda", 
#                           year = 2012, dat;e = seq.Date(as.Date("2001-01-01"), as.Date("2021-11-01"), 
#                                           ;.             by = "1 months"))
# knitr::kable(civic_lxl)
# 
# civic_lxl %>% ggplot(aes(date, price, color = year, group = year)) + 
#   geom_line(color = "grey30") + 
#   geom_point(size = 3) + facet_wrap(~model) + 
#   labs(x = "Mês de refêrencia", y = "Valor (R$)", color = "Ano do \nmodelo") + 
#   scale_y_continuous(breaks = seq(0, 1e+05, 1000), 
#                      labels = scales::dollar_format(prefix = NULL, big.mark = ".")) + 
#   scale_x_date(date_breaks = "1 year", date_labels = "%b/%y") + 
#   scale_color_viridis_d() + 
#   theme_bw() + 
#   theme(legend.position = "top")

get_reference <-  function(date) {
  
  date_month <- lubridate::floor_date(check_date(date), "month")
  
  httr::POST(
    "http://veiculos.fipe.org.br/api/veiculos/ConsultarTabelaDeReferencia",
    httr::add_headers(Referer = "http://veiculos.fipe.org.br/")
  ) %>%
    httr::content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    dplyr::mutate(date = lubridate::dmy(paste0("01/", Mes))) %>%
    dplyr::select(date, reference_code = Codigo) %>%
    dplyr::filter(date %in% date_month) %>%
    dplyr::pull(reference_code)
}

# retira caracteres especiais e simflifica a grafica para
# facilitar o match dos nomes
#
clean_name <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT')
}

# consulta as marcas disponiveis para um determinado mes
# de referencia e retorna o codigo e o nome correspondente
#
get_make <- function(reference_code) {
  
  table_make <- httr::POST(
    "http://veiculos.fipe.org.br/api/veiculos/ConsultarMarcas",
    httr::add_headers(Referer = "http://veiculos.fipe.org.br/"),
    body = list(
      codigoTabelaReferencia = reference_code,
      codigoTipoVeiculo = 1
    )
  ) %>%
    httr::content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    dplyr::rename(make_name = Label, make_code = Value) %>%
    dplyr::mutate(make_code = as.integer(make_code))
  
  return(table_make)
}

# consulta os modelos disponiveis para um determinado mes
# de referencia e marca e retorna o codigo correspondente
#
get_model <- function(make = NULL, reference_code) {
  
  table_model <- httr::POST(
    "http://veiculos.fipe.org.br/api/veiculos/ConsultarModelos",
    httr::add_headers(Referer = "http://veiculos.fipe.org.br/"),
    body = list(
      codigoTipoVeiculo = 1,
      codigoTabelaReferencia = reference_code,
      codigoMarca = make
    )
  ) %>%
    httr::content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>% 
    '[['(1) %>% 
    tibble::rownames_to_column() %>%
    dplyr::select(model_name = Label, model_code = Value)
  
  return(table_model)
}

# extrai tabela com os anos disponíveis de cada modelo
#
get_table_year <- function(reference_code, model_code, make_code) {
  
  content <- httr::POST(
    "http://veiculos.fipe.org.br/api/veiculos/ConsultarAnoModelo",
    httr::add_headers(Referer = "http://veiculos.fipe.org.br/"),
    body = list(
      codigoTipoVeiculo = 1,
      codigoTabelaReferencia = reference_code,
      codigoModelo = model_code,
      codigoMarca = make_code
    )
  ) %>%
    httr::content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()
  
  if (content[[2]][[1]] == "nadaencontrado") return(NULL)
  
  content %>%
    tidyr::separate(Label, c("ano", "combustivel")) %>%
    dplyr::mutate(
      ano = ifelse(ano == "32000", 0L, as.integer(ano))
    ) %>%
    dplyr::select(year = ano, year_code = Value) %>%
    tibble::as_tibble()
}

ui <- fluidPage(
  
  titlePanel(" Histórico de valores de veículos FIPE"),
  
  sidebarLayout(
    #Painel lateral
    sidebarPanel(
      selectInput("data_referencia", 
                  "Selecione a Data de Referência", 
                  choices = format(as.Date(Sys.Date()),format="%b/%Y")),
      uiOutput("secondSelection"),
      uiOutput("terceiroSelection"),
      uiOutput("quartoSelection"),
      # selectInput("marca", "Selecione a Marca do Veículo", choices = c("")),
      # selectInput("modelo", "Selecione a Modelo do Veículo", choices = c("")),
      # selectInput("ano", "Selecione o Ano do Modelo do Veículo", choices = c("")),
      # dateRangeInput('dateRange',
      #                label = "Pédiode d'analyse : ",
      #                format = "mm/yyyy",
      #                language="pt-BR",
      #                start = as.Date("2001-01-01"), 
      #                end= Sys.Date(),
      #                startview = "year",
      #                separator = " - "),
      sliderInput(
        "range",
        "Período de Análise:",
        min = as.Date("2001-01-01"),
        max = as.Date(cut(Sys.Date(), "month")),
        value =  c(as.Date("2001-01-01"), as.Date(cut(Sys.Date(), "month"))),
        timeFormat="%b/%Y"
      ),
      actionButton("atualizar", "Atualizar")
    ),
    #Painel central
    mainPanel(
      plotOutput("plot_veiculo"),
      # DTOutput("table_veiculo")
      # tableOutput("table_veiculo")
    )
  )
  
)

server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  observeEvent(input$data_referencia,{
    data_ref = get_reference(lubridate::dmy(paste0("01/", input$data_referencia)))
    marcas_referencia = get_make(reference_code = data_ref)
    
    output$secondSelection <- renderUI({
      selectInput("marca", "Selecione a Marca do Veículo", choices = c(marcas_referencia$make_name))
    })
  })
  
  observeEvent(input$marca,{
    data_ref = get_reference(lubridate::dmy(paste0("01/", input$data_referencia)))
    marcas_referencia = get_make(reference_code = data_ref)
    modelos_referencia = get_model(marcas_referencia$make_code[marcas_referencia$make_name == input$marca],data_ref)
    
    output$terceiroSelection <- renderUI({
      selectInput("modelo", "Selecione a Modelo do Veículo", choices = c(modelos_referencia$model_name))
    })
  })
  
  observeEvent(input$modelo,{
    data_ref = get_reference(lubridate::dmy(paste0("01/", input$data_referencia)))
    marcas_referencia = get_make(reference_code = data_ref)
    modelos_referencia = get_model(marcas_referencia$make_code[marcas_referencia$make_name == input$marca],data_ref)
    anos_referencia = get_table_year(data_ref,modelos_referencia$model_code[modelos_referencia$model_name == input$modelo],marcas_referencia$make_code[marcas_referencia$make_name == input$marca])
    
    output$quartoSelection <- renderUI({
      selectInput("ano", "Selecione a Ano do Modelo do Veículo", choices = c(anos_referencia$year))
    })
  })
  
  
  observeEvent(input$atualizar,{
    
    show_modal_spinner(spin = "orbit",
                       color = "firebrick",
                       text = "Por Favor, Aguarde! Carregando os dados...")
    
    rv$df_car = fipe_vehicle(model = isolate(input$modelo),
                             make = isolate(input$marca),
                             year = isolate(input$ano),
                             date = seq.Date(as.Date(isolate(input$range[1])), as.Date(isolate(input$range[2])),
                                             by = "1 months"))
    
    # df_car = reactive({
    #   
    #   # fipe_veiculo = fipe_vehicle(model = "Civic Sed. LXL/ LXL SE 1.8 Flex 16V Aut.", make = "Honda", 
    #   #                             year = 2012, date = as.Date("2017-01-01"))
    #   
    #   # fipe_vehicle(model = isolate(input$modelo),
    #   #              make = isolate(input$marca),
    #   #              year = isolate(input$ano),
    #   #              date = as.Date("2017-01-01"))
    #   
    #   fipe_vehicle(model = isolate(input$modelo),
    #                make = isolate(input$marca),
    #                year = isolate(input$ano),
    #                date = seq.Date(as.Date(isolate(input$range[1])), as.Date(isolate(input$range[2])),
    #                                by = "1 months"))
    # })
    
    output$plot_veiculo = renderPlot({
      rv$df_car %>% ggplot(aes(date, price, color = year, group = year)) +
        geom_line(color = "grey30") +
        geom_point(size = 3) + facet_wrap(~model) +
        labs(x = "Mês de refêrencia", y = "Valor (R$)", color = "Ano do \nmodelo") +
        scale_y_continuous(breaks = seq(0, 1e+05, 1000),
                           labels = scales::dollar_format(prefix = NULL, big.mark = ".")) +
        scale_x_date(date_breaks = "1 year", date_labels = "%b/%y") +
        scale_color_viridis_d() +
        theme_bw() +
        theme(legend.position = "top")
    })
    
    remove_modal_spinner() # remove it when done
    # output$table_veiculo <- renderTable({
    #   df_car()
    # })
  })
  
  # output$table_veiculo = renderDT({
  #   df_car()
  # })
  
  # observeEvent(input$range,{
  #   
  #   plot_car = fipe_vehicle(model = input$modelo, 
  #                           make = input$marca, 
  #                           year = input$ano, 
  #                           date = seq.Date(as.Date(input$range[1]), as.Date(input$range[2]), 
  #                                           by = "1 months"))
  #   
  #   #### Criacao do grafico historico
  #   
  #   # output$plot_veiculo = renderPlot({
  #   #   plot_car %>% ggplot(aes(date, price, color = year, group = year)) + 
  #   #     geom_line(color = "grey30") + 
  #   #     geom_point(size = 3) + facet_wrap(~model) + 
  #   #     labs(x = "Mês de refêrencia", y = "Valor (R$)", color = "Ano do \nmodelo") + 
  #   #     scale_y_continuous(breaks = seq(0, 1e+05, 1000), 
  #   #                        labels = scales::dollar_format(prefix = NULL, big.mark = ".")) + 
  #   #     scale_x_date(date_breaks = "1 year", date_labels = "%b/%y") + 
  #   #     scale_color_viridis_d() + 
  #   #     theme_bw() + 
  #   #     theme(legend.position = "top")
  #   # })
  #   
  #   output$table_veiculo = renderDT({
  #     plot_car
  #   })
  #   
  # })
  
}

shinyApp(ui, server)