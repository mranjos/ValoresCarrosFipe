# install.packages("shiny")
# install.packages("fipe")
# install.packages("knitr")
# install.packages("ggplot2")
# install.packages("checkr")
# install.packages("xml2")
# install.packages("httr")
# install.packages("lubridate")
# install.packages("DT")
# install.packages("shinybusy")
# install.packages("shinymaterial")
# install.packages("shinydashboard")
# install.packages("ggiraph")

Sys.setlocale(locale = "English")
library(shiny)
library(knitr)
library(dplyr)
library(ggplot2)
library(xml2)
library(httr)
library(lubridate)
library(DT)
library(shinybusy)
library(shinydashboard)
library(ggiraph)

###############################################.#
#### Funcoes adaptadas para inputs do shiny ###.#
###############################################.#

check_date <- function(x) {
  
  if (!lubridate::is.Date(x)) {
    
    test1 <- tryCatch(lubridate::dmy(x), warning=function(w) w)
    
    if (!any((class(test1) == "warning") == TRUE)) {
      
      return(test1)
      
    } else {
      
      test2 <- tryCatch(lubridate::ymd(x), warning=function(w) w)
      
      if (lubridate::is.Date(test2)) {
        
        return(test2)
        
      } else {
        
        stop("All formats failed to parse to date. No formats found.")
        
      }
    }
  }
  
  x
}

get_reference <-  function(date) {
  
  date_month <- lubridate::floor_date(check_date(date), "month")
  
  httr::POST(
    "http://veiculos.fipe.org.br/api/veiculos/ConsultarTabelaDeReferencia",
    httr::add_headers(Referer = "http://veiculos.fipe.org.br/")
  ) %>%
    httr::content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    dplyr::mutate(Mes = gsub("janeiro","january",Mes)) %>% 
    dplyr::mutate(Mes = gsub("fevereiro","february",Mes)) %>% 
    dplyr::mutate(Mes = gsub("março","march",Mes)) %>% 
    dplyr::mutate(Mes = gsub("abril","april",Mes)) %>% 
    dplyr::mutate(Mes = gsub("maio","may",Mes)) %>% 
    dplyr::mutate(Mes = gsub("junho","june",Mes)) %>% 
    dplyr::mutate(Mes = gsub("julho","july",Mes)) %>% 
    dplyr::mutate(Mes = gsub("agosto","august",Mes)) %>% 
    dplyr::mutate(Mes = gsub("setembro","september",Mes)) %>% 
    dplyr::mutate(Mes = gsub("outubro","october",Mes)) %>% 
    dplyr::mutate(Mes = gsub("novembro","november",Mes)) %>% 
    dplyr::mutate(Mes = gsub("dezembro","december",Mes)) %>%  
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
get_make_aux <- function(reference_code, reference_veiculo) {
  
  table_make <- httr::POST(
    "http://veiculos.fipe.org.br/api/veiculos/ConsultarMarcas",
    httr::add_headers(Referer = "http://veiculos.fipe.org.br/"),
    body = list(
      codigoTabelaReferencia = reference_code,
      codigoTipoVeiculo = reference_veiculo
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
get_model_aux <- function(make = NULL, reference_code, reference_veiculo) {
  
  table_model <- httr::POST(
    "http://veiculos.fipe.org.br/api/veiculos/ConsultarModelos",
    httr::add_headers(Referer = "http://veiculos.fipe.org.br/"),
    body = list(
      codigoTipoVeiculo = reference_veiculo,
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
get_table_year <- function(reference_code, model_code, make_code, reference_veiculo) {
  
  content <- httr::POST(
    "http://veiculos.fipe.org.br/api/veiculos/ConsultarAnoModelo",
    httr::add_headers(Referer = "http://veiculos.fipe.org.br/"),
    body = list(
      codigoTipoVeiculo = reference_veiculo,
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

#### Fim das funcoes adaptadas para o input ####

########################################################.#
#### Funcoes adaptadas para hospedar no Shinyapps.io ###.#
########################################################.#

# consulta as marcas disponiveis para um determinado mes
# de referencia e retorna o codigo correspondente
#
get_make <- function(make = NULL, reference_code, reference_veiculo) {
  
  table_make <- httr::POST(
    "http://veiculos.fipe.org.br/api/veiculos/ConsultarMarcas",
    httr::add_headers(Referer = "http://veiculos.fipe.org.br/"),
    body = list(
      codigoTabelaReferencia = reference_code,
      codigoTipoVeiculo = reference_veiculo
    )
  ) %>%
    httr::content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    dplyr::rename(make_name = Label, make_code = Value) %>%
    dplyr::mutate(make_code = as.integer(make_code))
  
  if (is.null(make)) {
    
    table_make %>%
      dplyr::pull(make_code) %>%
      return()
    
  } else {
    
    table_make %>%
      dplyr::filter(clean_name(make_name) %in% clean_name(make)) %>%
      dplyr::pull(make_code) %>%
      return()
  }
}

# consulta os modelos disponiveis para um determinado mes
# de referencia e marca e retorna o codigo correspondente
#
get_model <- function(model, make = NULL, reference_code, reference_veiculo) {
  
  reference_code_max <- reference_code[which.max(reference_code)]
  
  make_code <- get_make(make, reference_code_max,reference_veiculo)
  
  models <- paste0(stringr::str_to_lower(model), collapse = "|")
  
  purrr::map_dfr(
    make_code,
    ~httr::POST(
      "http://veiculos.fipe.org.br/api/veiculos/ConsultarModelos",
      httr::add_headers(Referer = "http://veiculos.fipe.org.br/"),
      body = list(
        codigoTipoVeiculo = reference_veiculo,
        codigoTabelaReferencia = reference_code_max,
        codigoMarca = .x
      )
    ) %>%
      httr::content("text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      '[['(1) %>%
      tibble::rownames_to_column() %>%
      dplyr::select(model_name = Label, model_code = Value) %>%
      dplyr::mutate(
        make_code = .x,
        model_name = clean_name(model_name)
      ) %>%
      tibble::as_tibble()
  ) %>%
    dplyr::filter(stringr::str_detect(model_name, models))
}

# consulta o ano do modelo disponivel para um determinado mes
# de referencia, make e modelo
#
get_year <- function(model, make = NULL, year_filter = NULL, reference_code,reference_veiculo) {
  
  #reference_code_max <- reference_code[which.max(reference_code)]
  
  model_code <- get_model(model, make, reference_code,reference_veiculo)
  
  if (nrow(model_code) == 0) stop("Model not found", call. = FALSE)
  
  table_year <- model_code %>%
    tidyr::crossing(., reference_code_range = range(reference_code)) %>%
    dplyr::mutate(
      year_code = purrr::pmap(
        list(reference_code_range, model_code, make_code,reference_veiculo),
        get_table_year
      )
    ) %>%
    dplyr::filter(!purrr::map_lgl(year_code, is.null)) %>%
    tidyr::unnest() %>%
    dplyr::distinct(model_code, make_code, year_code, year)
  
  if (is.null(year_filter)) {
    
    return(table_year)
    
  } else {
    
    table_year %>%
      dplyr::filter(year %in% year_filter) %>%
      return()
    
  }
}

# consulta o valor do modelo
#
get_price <- function(reference_code, make_code, model_code, year_code, reference_veiculo) {
  
  ano <- as.character(
    stringr::str_split(year_code, "-", simplify = TRUE)[1, 1]
  )
  combustivel <- as.integer(
    stringr::str_split(year_code, "-", simplify = TRUE)[1, 2]
  )
  
  content <- httr::POST(
    "http://veiculos.fipe.org.br/api/veiculos/ConsultarValorComTodosParametros",
    httr::add_headers(Referer = "http://veiculos.fipe.org.br/"),
    body = list(
      codigoTabelaReferencia = reference_code,
      codigoMarca = make_code,
      codigoModelo = model_code,
      codigoTipoVeiculo = reference_veiculo,
      anoModelo = ano,
      codigoTipoCombustivel = combustivel,
      tipoVeiculo = "carro",
      modeloCodigoExterno = "",
      tipoConsulta = "tradicional"
    )
  ) %>%
    httr::content() %>%
    tibble::as_tibble()
  
  if (content[[2]] == "nadaencontrado") return(NULL)
  
  content %>%
    dplyr::mutate(MesReferencia = gsub("janeiro","january",MesReferencia)) %>% 
    dplyr::mutate(MesReferencia = gsub("fevereiro","february",MesReferencia)) %>% 
    dplyr::mutate(MesReferencia = gsub("março","march",MesReferencia)) %>% 
    dplyr::mutate(MesReferencia = gsub("abril","april",MesReferencia)) %>% 
    dplyr::mutate(MesReferencia = gsub("maio","may",MesReferencia)) %>% 
    dplyr::mutate(MesReferencia = gsub("junho","june",MesReferencia)) %>% 
    dplyr::mutate(MesReferencia = gsub("julho","july",MesReferencia)) %>% 
    dplyr::mutate(MesReferencia = gsub("agosto","august",MesReferencia)) %>% 
    dplyr::mutate(MesReferencia = gsub("setembro","september",MesReferencia)) %>% 
    dplyr::mutate(MesReferencia = gsub("outubro","october",MesReferencia)) %>% 
    dplyr::mutate(MesReferencia = gsub("novembro","november",MesReferencia)) %>% 
    dplyr::mutate(MesReferencia = gsub("dezembro","december",MesReferencia)) %>% 
    dplyr::mutate(
      MesReferencia = lubridate::dmy(paste0("01 ", MesReferencia)),
      AnoModelo = ifelse(AnoModelo == "32000", 0L, as.integer(AnoModelo)),
      Valor = readr::parse_number(
        Valor, locale = readr::locale(decimal_mark = ",")
      )
    ) %>%
    dplyr::select(
      fipe_code = CodigoFipe,
      date = MesReferencia,
      make = Marca,
      model = Modelo,
      year = AnoModelo,
      #gas = Combustivel,
      price = Valor
    )
}

fipe_vehicle <- function(model, make = NULL, year = NULL,date = Sys.Date(), progress = FALSE, parallel = FALSE, reference_veiculo) {
  
  reference_code <- get_reference(date)
  
  base_cod_ano <- get_year(model, make, year, reference_code, reference_veiculo)
  
  if (parallel) {
    future::plan(future::multiprocess)
  } else {
    future::plan(future::sequential)
  }
  
  base_cod_ano %>%
    tidyr::crossing(., reference_code) %>%
    dplyr::mutate(
      price = furrr::future_pmap(
        list(reference_code, make_code, model_code, year_code,reference_veiculo),
        get_price, .progress = progress
      )
    ) %>%
    dplyr::filter(!purrr::map_lgl(price, is.null)) %>%
    dplyr::select(price) %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      year = ifelse(year == 0L, "0 km", as.character(year)),
      year = suppressWarnings(forcats::fct_relevel(year, "0 km", after = Inf))
    ) %>%
    dplyr::select(
      model, make, year, date, price
    )
}

#####################################.#
#### Inicio da construcao do Dash ###.#
#####################################.#

ui <- dashboardPage(
  dashboardHeader(title = "Veículos FIPE"),
  dashboardSidebar(
    selectInput("data_referencia",
                "Data de Referência",
                choices = format(as.Date(Sys.Date()),format="%b/%Y")),
    selectInput("cod_veiculo",
                "Tipo de Veículo",
                choices = c("Carros","Caminhões e Micro-Ônibus","Motos")),
    uiOutput("secondSelection"),
    uiOutput("terceiroSelection"),
    uiOutput("quartoSelection")
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Selecione o período para acompanhamento",
        status = "primary", 
        width = "12",
        solidHeader = TRUE,
        collapsible = TRUE,
        sliderInput(
          "range",
          "",
          min = as.Date("2001-01-01"),
          max = as.Date(cut(Sys.Date(), "month")),
          value =  c(as.Date("2001-01-01"), as.Date(cut(Sys.Date(), "month"))),
          timeFormat="%b/%Y",
          width = "100%"
        ),
        tags$br(),
        actionButton("atualizar", "Analisar")
      )
    ),
    fluidRow(
      box(
        title = "Acompahamento do Preço (R$) ao longo dos anos",
        status = "primary", 
        width = "12",
        solidHeader = TRUE,
        ggiraphOutput("plot_veiculo")
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  observeEvent(input$cod_veiculo,{
    data_ref = get_reference(lubridate::dmy(paste0("01/", input$data_referencia)))
    reference_veiculo = ifelse(input$cod_veiculo == "Carros e Utilitários Pequenos", 1,
                               ifelse(input$cod_veiculo == "Caminhões e Micro-Ônibus",3,
                                      ifelse(input$cod_veiculo == "Motos",2,1)))
    marcas_referencia = get_make_aux(reference_code = data_ref,reference_veiculo)
    
    output$secondSelection <- renderUI({
      selectInput("marca", "Marca do Veículo", choices = c(marcas_referencia$make_name))
    })
  })
  
  observeEvent(input$marca,{
    data_ref = get_reference(lubridate::dmy(paste0("01/", input$data_referencia)))
    reference_veiculo = ifelse(input$cod_veiculo == "Carros e Utilitários Pequenos", 1,
                               ifelse(input$cod_veiculo == "Caminhões e Micro-Ônibus",3,
                                      ifelse(input$cod_veiculo == "Motos",2,1)))
    marcas_referencia = get_make_aux(reference_code = data_ref,reference_veiculo)
    modelos_referencia = get_model_aux(marcas_referencia$make_code[marcas_referencia$make_name == input$marca],data_ref,reference_veiculo)
    
    output$terceiroSelection <- renderUI({
      selectInput("modelo", "Modelo do Veículo", choices = c(modelos_referencia$model_name))
    })
  })
  
  observeEvent(input$modelo,{
    data_ref = get_reference(lubridate::dmy(paste0("01/", input$data_referencia)))
    reference_veiculo = ifelse(input$cod_veiculo == "Carros", 1,
                               ifelse(input$cod_veiculo == "Caminhões e Micro-Ônibus",3,
                                      ifelse(input$cod_veiculo == "Motos",2,1)))

    marcas_referencia = get_make_aux(reference_code = data_ref,reference_veiculo)
    modelos_referencia = get_model_aux(marcas_referencia$make_code[marcas_referencia$make_name == input$marca],data_ref,reference_veiculo)
    anos_referencia = get_table_year(data_ref,modelos_referencia$model_code[modelos_referencia$model_name == input$modelo],marcas_referencia$make_code[marcas_referencia$make_name == input$marca],reference_veiculo)
    
    output$quartoSelection <- renderUI({
      selectInput("ano", "Ano do Modelo do Veículo", choices = c(anos_referencia$year))
    })
  })
  
  
  observeEvent(input$atualizar,{
    
    shinybusy::show_modal_spinner(spin = "orbit",
                                  color = "firebrick",
                                  text = "Por Favor, Aguarde! Carregando os dados...")
    
    reference_veiculo = ifelse(input$cod_veiculo == "Carros", 1,
                               ifelse(input$cod_veiculo == "Caminhões e Micro-Ônibus",3,
                                      ifelse(input$cod_veiculo == "Motos",2,1)))
    
    rv$df_car = fipe_vehicle(model = isolate(input$modelo),
                             make = isolate(input$marca),
                             year = isolate(input$ano),
                             date = seq.Date(as.Date(isolate(input$range[1])), as.Date(isolate(input$range[2])),
                                             by = "1 months"),
                             reference_veiculo = reference_veiculo)
    
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
    
    output$plot_veiculo = renderGirafe({
      
      rv$df_car$legenda = c(paste0("Valor = R$ ", rv$df_car$price, "\n Data = ", format(rv$df_car$date, "%b/%Y")))
      
      gg_scatter = rv$df_car %>% ggplot(aes(x = date, y = price, color = year, group = year)) +
        geom_line(color = "#3C33FF") +
        geom_point_interactive(aes(x = date, y = price, color = year, group = year, tooltip = legenda),
                               color = "#3C33FF", 
                               size = 2) + 
        facet_wrap(~model) +
        labs(x = "Mês de refêrencia", y = "Valor (R$)", color = "Ano do \nmodelo") +
        scale_y_continuous(n.breaks = 10,
                           labels = scales::dollar_format(prefix = NULL, big.mark = ".")) +
        scale_x_date(date_breaks = "1 year", date_labels = "%b/%y") +
        scale_color_viridis_d() +
        theme_bw() +
        theme(legend.position = "top",
              axis.text.x = element_text(angle = 45,hjust=1, size=10),
              axis.text.y = element_text(size=10),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12))
      
      girafe(
        ggobj = gg_scatter, width_svg = 9
      )
      
      # gg_scatter = rv$df_car %>% ggplot(aes(date, price, color = year, group = year)) +
      #   geom_line(color = "grey30", linestyle = "dashed") +
      #   geom_point_interactive(size=3) + 
      #   facet_wrap(~model) +
      #   labs(x = "Mês de refêrencia", y = "Valor (R$)", color = "Ano do \nmodelo") +
      #   scale_y_continuous(n.breaks = 10,
      #                      labels = scales::dollar_format(prefix = NULL, big.mark = ".")) +
      #   scale_x_date(date_breaks = "1 year", date_labels = "%b/%y") +
      #   scale_color_viridis_d() +
      #   theme_bw() +
      #   theme(legend.position = "top")
      # 
      # ggiraph(ggobj = gg_scatter, 
      #        options = list(opts_selection(type = "single", only_shiny = FALSE)) )
      
    })
    
    shinybusy::remove_modal_spinner() # remove it when done
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
