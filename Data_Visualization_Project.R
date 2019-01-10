library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(lubridate)
library(shinyjs)
library(billboarder)
library(purrr)
library(devtools)

categorias = read.csv("C:\\Users\\hppor\\Desktop\\Faculdade\\Projeto\\Category_Name.csv")
clientes = read.csv("C:\\Users\\hppor\\Desktop\\Faculdade\\Projeto\\Customer.csv")
geolocalização = read.csv("C:\\Users\\hppor\\Desktop\\Faculdade\\Projeto\\Geolocalization.csv")
orders = read.csv("C:\\Users\\hppor\\Desktop\\Faculdade\\Projeto\\Order.csv")
order_items = read.csv("C:\\Users\\hppor\\Desktop\\Faculdade\\Projeto\\Order_Items.csv")
order_payment = read.csv("C:\\Users\\hppor\\Desktop\\Faculdade\\Projeto\\Order_Payment.csv")
order_review = read.csv("C:\\Users\\hppor\\Desktop\\Faculdade\\Projeto\\order_Review.csv")
produtos = read.csv("C:\\Users\\hppor\\Desktop\\Faculdade\\Projeto\\Products.csv")
vendedores = read.csv("C:\\Users\\hppor\\Desktop\\Faculdade\\Projeto\\Sellers.csv")

categorias = data.frame(categorias)
clientes = data.frame(clientes)
geolocalização = data.frame(geolocalização)
orders = data.frame(orders)
order_items = data.frame(order_items)
order_payment = data.frame(order_payment)
order_review = data.frame(order_review)
produtos = data.frame(produtos)
vendedores = data.frame(vendedores)

dataframe = merge(x = order_items, y = orders, by="order_id")
dataframe = merge(x = dataframe, y = produtos, by="product_id")
dataframe = merge(x = dataframe, y = vendedores, by="seller_id")
dataframe = merge(x = dataframe, y = order_payment, by="order_id")
dataframe = merge(x = dataframe, y = clientes, by="customer_id")
dataframe = merge(x = dataframe, y = order_review, by="order_id")

#add 4 empty columns to dataframe. The goal is add a latitude and longitude for the seller and the customer
new_columns = c("cus_lat", "cus_lng", "sel_lat", "sel_lng")
dataframe[, new_columns] = NA

#Populate the latitude and longitude
dataframe$sel_lat = geolocalização$geolocation_lat[match(dataframe$seller_zip_code_prefix, geolocalização$geolocation_zip_code_prefix)]
dataframe$sel_lng = geolocalização$geolocation_lng[match(dataframe$seller_zip_code_prefix, geolocalização$geolocation_zip_code_prefix)]
dataframe$cus_lat = geolocalização$geolocation_lat[match(dataframe$customer_zip_code_prefix, geolocalização$geolocation_zip_code_prefix)]
dataframe$cus_lng = geolocalização$geolocation_lng[match(dataframe$customer_zip_code_prefix, geolocalização$geolocation_zip_code_prefix)]

#Add a new Column with the right names of the states. 
#https://en.wikipedia.org/wiki/States_of_Brazil
siglas = c("SP", "MG", "PR", "SC", "DF", "RS", "RJ", "GO", "MA", "ES", "BA", "PI", "RO", "MT", "CE", "RN", "PE", "SE", "MS", "PB", "PA", "AM", "AC")
states = c("São Paulo", "Minas Gerais", "Paraná", "Santa Catarina", "Distrito Federal", "Rio Grande do Sul", "Rio de Janeiro", "Goiás", "Maranhão", "Espírito Santo", "Bahia", "Piauí", "Rondônia", "Mato Grosso", "Ceará", "Rio Grande do Norte", "Pernambuco", "Sergipe", "Mato Grosso do Sul", "Paraíba", "Pará", "Amazonas", "Acre")
region = c("Sudeste", "Sudeste", "Noroeste", "Sul", "Centro-Oeste", "Sul", "Sudeste", "Centro-Oeste", "Noroeste", "Sudeste", "Noroeste", "Noroeste", "Norte", "Centro-Oeste", "Noroeste", "Noroeste", "Noroeste", "Noroeste", "Centro-Oeste", "Noroeste", "Norte", "Norte", "Norte")

#Create a dataframe to match the geography of the 
geography_dataframe = data.frame(siglas, states, region) 

#Populate the state and the region
columns_geography = c("customer_state_name", "customer_region", "seller_state_name", "seller_region")
dataframe[, columns_geography] = NA
dataframe$seller_state_name = geography_dataframe$states[match(dataframe$seller_state, geography_dataframe$siglas)]
dataframe$seller_region = geography_dataframe$region[match(dataframe$seller_state, geography_dataframe$siglas)]
dataframe$customer_state_name = geography_dataframe$states[match(dataframe$customer_state, geography_dataframe$siglas)]
dataframe$customer_region = geography_dataframe$region[match(dataframe$customer_state, geography_dataframe$siglas)]


dataframe = subset(dataframe, select=-c(seller_id, product_id, order_id, customer_id, review_id))
#Drop not delivered products 
dataframe = dataframe[dataframe$order_status == 'delivered',] #lost 3420 rows

#Try discovery some null values
categorias_name_vector = as.vector(dataframe['product_category_name']) # 0 records
dataframe = dataframe[dataframe$product_category_name != '',] #Lost 1638

dataframe = dataframe[!is.na(dataframe$order_purchase_timestamp),]

#Prepare Categories

#Unique Values | Count
categorias_unique = data.frame(Categoria = dataframe$product_category_name)
categorias_unique = data.frame(table(categorias_unique$Categoria))

#Livros
column_main_category = c("Categoria")
dataframe[, column_main_category] = NA
dataframe$Categoria[(dataframe$product_category_name == 'livros_tecnicos')] = "Livros"
dataframe$Categoria[(dataframe$product_category_name == 'livros_importados')] = "Livros" 
dataframe$Categoria[(dataframe$product_category_name == 'livros_interesse_geral')] = "Livros"

#Outros
dataframe$Categoria[(dataframe$product_category_name == 'cool_stuff')] = "Outros"
dataframe$Categoria[(dataframe$product_category_name == 'market_place')] = "Outros"
dataframe$Categoria[(dataframe$product_category_name == 'papelaria')] = "Outros"
dataframe$Categoria[(dataframe$product_category_name == 'industria_comercio_e_negocios')] = "Outros"
dataframe$Categoria[(dataframe$product_category_name == 'relogios_presentes')] = "Outros"
dataframe$Categoria[(dataframe$product_category_name == 'artes')] = "Outros"
dataframe$Categoria[(dataframe$product_category_name == 'flores')] = "Outros"
dataframe$Categoria[(dataframe$product_category_name == 'artigos_de_natal')] = "Outros"
dataframe$Categoria[(dataframe$product_category_name == 'artes_e_artesanato')] = "Outros"
dataframe$Categoria[(dataframe$product_category_name == 'la_cuisine')] = "Outros"
dataframe$Categoria[(dataframe$product_category_name == 'seguros_e_servicos')] = "Outros"

#Alimentação
dataframe$Categoria[(dataframe$product_category_name == 'alimentos_bebidas')] = "Alimentação"
dataframe$Categoria[(dataframe$product_category_name == 'agro_industria_e_comercio')] = "Alimentação"
dataframe$Categoria[(dataframe$product_category_name == 'alimentos')] = "Alimentação"
dataframe$Categoria[(dataframe$product_category_name == 'bebidas')] = "Alimentação"

#Animais
dataframe$Categoria[(dataframe$product_category_name == 'pet_shop')] = "Animais"

#Casa
dataframe$Categoria[(dataframe$product_category_name == 'moveis_decoracao')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'utilidades_domesticas')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'cama_mesa_banho')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'moveis_escritorio')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'eletronicos')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'eletrodomesticos')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'moveis_cozinha_area_de_servico_jantar_e_jardim')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'climatizacao')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'casa_conforto')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'portateis_casa_forno_e_cafe')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'eletroportateis')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'moveis_sala')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'eletrodomesticos_2')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'moveis_quarto')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'casa_conforto_2')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'portateis_cozinha_e_preparadores_de_alimentos')] = 'Casa'
dataframe$Categoria[(dataframe$product_category_name == 'moveis_colchao_e_estofado')] = 'Casa'


#Fashion & Higiene
dataframe$Categoria[(dataframe$product_category_name == 'perfumaria')] = 'Fashion & Higiene'
dataframe$Categoria[(dataframe$product_category_name == 'beleza_saude')] = 'Fashion & Higiene'
dataframe$Categoria[(dataframe$product_category_name == 'fashion_bolsas_e_acessorios')] = 'Fashion & Higiene'
dataframe$Categoria[(dataframe$product_category_name == 'fashion_underwear_e_moda_praia')] = 'Fashion & Higiene'
dataframe$Categoria[(dataframe$product_category_name == 'fashion_roupa_masculina')] = 'Fashion & Higiene'
dataframe$Categoria[(dataframe$product_category_name == 'malas_acessorios')] = "Fashion & Higiene"
dataframe$Categoria[(dataframe$product_category_name == 'fashion_calcados')] = "Fashion & Higiene"
dataframe$Categoria[(dataframe$product_category_name == 'fashion_roupa_feminina')] = "Fashion & Higiene"


#Telemóveis
dataframe$Categoria[(dataframe$product_category_name == 'telefonia')] = 'Telemóveis'
dataframe$Categoria[(dataframe$product_category_name == 'telefonia_fixa')] = 'Telemóveis'

#Desporto
dataframe$Categoria[(dataframe$product_category_name == 'esporte_lazer')] = 'esporte_lazer'
dataframe$Categoria[(dataframe$product_category_name == 'fashion_esporte')] = 'esporte_lazer'

#Musica
dataframe$Categoria[(dataframe$product_category_name == 'audio')] = 'Audio'
dataframe$Categoria[(dataframe$product_category_name == 'instrumentos_musicais')] = 'Audio'
dataframe$Categoria[(dataframe$product_category_name == 'cine_foto')] = 'Audio'
dataframe$Categoria[(dataframe$product_category_name == 'musica')] = 'Audio'
dataframe$Categoria[(dataframe$product_category_name == 'dvds_blu_ray')] = 'Audio'
dataframe$Categoria[(dataframe$product_category_name == 'cds_dvds_musicais')] = 'Audio'


#Informática
dataframe$Categoria[(dataframe$product_category_name == 'consoles_games')] = 'Infomática'
dataframe$Categoria[(dataframe$product_category_name == 'informatica_acessorios')] = 'Infomática'
dataframe$Categoria[(dataframe$product_category_name == 'pcs')] = 'Infomática'
dataframe$Categoria[(dataframe$product_category_name == 'tablets_impressao_imagem')] = 'Infomática'
dataframe$Categoria[(dataframe$product_category_name == 'pc_gamer')] = 'Infomática'


#Construção
dataframe$Categoria[(dataframe$product_category_name == 'ferramentas_jardim')] = "Construção"
dataframe$Categoria[(dataframe$product_category_name == 'construcao_ferramentas_construcao')] = "Construção"
dataframe$Categoria[(dataframe$product_category_name == 'construcao_ferramentas_construcao')] = "Construção"
dataframe$Categoria[(dataframe$product_category_name == 'construcao_ferramentas_iluminacao')] = "Construção"
dataframe$Categoria[(dataframe$product_category_name == 'construcao_ferramentas_jardim')] = "Construção"
dataframe$Categoria[(dataframe$product_category_name == 'casa_construcao')] = "Construção"
dataframe$Categoria[(dataframe$product_category_name == 'construcao_ferramentas_seguranca')] = "Construção"
dataframe$Categoria[(dataframe$product_category_name == 'construcao_ferramentas_ferramentas')] = "Construção"

#Infantil
dataframe$Categoria[(dataframe$product_category_name == 'bebes')] = "Infantil"
dataframe$Categoria[(dataframe$product_category_name == 'brinquedos')] = "Infantil"
dataframe$Categoria[(dataframe$product_category_name == 'fraldas_higiene')] = "Infantil"
dataframe$Categoria[(dataframe$product_category_name == 'artigos_de_festas')] = "Infantil"
dataframe$Categoria[(dataframe$product_category_name == 'fashion_roupa_infanto_juvenil')] = "Infantil"

#Automotivo
dataframe$Categoria[(dataframe$product_category_name == 'automotivo')] = "Automotivo"
dataframe$Categoria[(dataframe$product_category_name == 'sinalizacao_e_seguranca')] = "Automotivo"



#Fill the columns
sell_history = data.frame(Date = dataframe$order_purchase_timestamp)
sell_history$Date_ = as.Date(as.character(sell_history$Date))
sell_history$Hour = hour(sell_history$Date)
sell_history$Weekday = weekdays(as.Date(as.character(sell_history$Date)))
sell_history$Day = day(sell_history$Date)
sell_history$Month = month(sell_history$Date)
sell_history$Year = year(sell_history$Date)
sell_history$price = dataframe$price


#Year Sells
year_sell = data.frame(Data = sell_history$Date_, Price = sell_history$price, Year=sell_history$Year)
year_sell$Date = as.Date(as.character(sell_history$Date, format="%m/%B/%Y"))
year_sell = group_by(year_sell, Year)
year_sell = summarise(year_sell, Price=sum(Price))
year_sell = data.frame(Year=year_sell$Year, Price = year_sell$Price)

#Test for month sells
month_sell = data.frame(Date = sell_history$Date_, Price = sell_history$price, Month=sell_history$Month)
month_sell$Date = as.Date(as.character(sell_history$Date, format="%m/%B/%Y"))
month_sell = group_by(month_sell, Month)
month_sell = summarise(month_sell, Price=sum(Price))
month_sell = data.frame(Month=month_sell$Month, Price = year_sell$Price)

#Dates
date_column_purchase = c("Date_Purchase")
dataframe[date_column_purchase,]=NA
dataframe$Date_Purchase = as.Date(as.character(dataframe$order_purchase_timestamp, format = "%m/%B/%Y"))

#First Plot
group_date = data.frame(Date=dataframe$Date_Purchase, Price = dataframe$price)
group_date$Date = as.Date(as.character(group_date$Date, format="%Y-%m-%d"))
group_date = group_by(group_date, Date)
group_date = summarise(group_date, Price=sum(Price))
group_date = group_date[!is.na(group_date$Date),]
group_date$Date = as.Date(as.character(group_date$Date, format="%Y-%m-%d"))
group_date = as.data.frame.table(group_date)
group_date_columns = c("Month", "Year")
group_date[,group_date_columns]=NA
group_date = data.frame(Date = group_date$Freq.Date, Price = group_date$Freq.Price, Month = group_date$Month, Year=group_date$Year)
group_date$Month = month(as.Date(as.character(group_date$Date, format="%Y-%m-%d")))
group_date$Year = year(as.Date(as.character(group_date$Date, format="%Y-%m-%d")))


#plot1 to data
year = data_frame(names=c(unique(year_sell$Year)), y=c(year_sell[1,2] ,year_sell[2,2], year_sell[3,2]), drilldown=tolower(names))
year_list = list_parse(year)

year_2016 = sell_history[sell_history$Year=="2016",]
year_2016 = subset(sell_history, select=-c(Date, Date_, Hour, Weekday, Day, Year))
year_2016 = group_by(year_2016, Month)
year_2016 = summarise(year_2016, Price=sum(price))
year_2016 = data.frame(Month=year_2016$Month, Price=year_2016)
year_2016 = subset(year_2016, select=-c(Price.Month))

year_2017 = sell_history[sell_history$Year=="2017",]
year_2017 = subset(sell_history, select=-c(Date, Date_, Hour, Weekday, Day, Year))
year_2017 = group_by(year_2017, Month)
year_2017 = summarise(year_2017, Price=sum(price))
year_2017 = data.frame(Month=year_2017$Month, Price=year_2017)
year_2017 = subset(year_2017, select=-c(Price.Month))

year_2018 = sell_history[sell_history$Year=="2018",]
year_2018 = subset(sell_history, select=-c(Date, Date_, Hour, Weekday, Day, Year))
year_2018 = group_by(year_2018, Month)
year_2018 = summarise(year_2018, Price=sum(price))
year_2018 = data.frame(Month=year_2018$Month, Price=year_2018)
year_2018 = subset(year_2018, select=-c(Price.Month))


#plot2 

main_category = 
  
  year_sell = data.frame(Data = sell_history$Date_, Price = sell_history$price, Year=sell_history$Year)
year_sell$Date = as.Date(as.character(sell_history$Date, format="%m/%B/%Y"))
year_sell = group_by(year_sell, Year)
year_sell = summarise(year_sell, Price=sum(Price))
year_sell = data.frame(Year=year_sell$Year, Price = year_sell$Price)

#Lets try shinny
header = dashboardHeader(title = "OLIST")
#sidebar
sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "Overview", icon=icon("globe-americas")),
    menuItem("Segundo", tabName = "Segundo", icon=icon("sellsy"),
             br())
  )
)
body = dashboardBody(
  fluidRow(
    #sidebarPanel(
    #  selectInput("primeiro", "segundo", unique(group_date$Year))
    #),
    box(
      title = "Histórico Total Vendas - Lucro",
      highchartOutput("plot")
    ),
    box(title = "Categorias Bubbles",
        br(),
        tags$script(src="javascrip.js"))
  ),
  fluidRow(
    box(title = "Transacções"
    ),
    box(title = "Vendas Total Mapa")
  )
)

ui = dashboardPage(
  header, sidebar, body
)

server = function(input, output) {
  
  # A reactive function for tree map
  dataInput = reactive({
    #from = which(group_date$Month  == input$date[1])
    #to = which(group_date$Date == input$date[2])
    #out = group_date[from:to, c("Date", "Price")]
    #out
    #out = group_by(group_date$Year)
    condition = which(group_date$Year ==input$primeiro)
    out = data.frame("Year" = c(2016, 2017, 2018), "Price"=condition)
    #out
  })
  #output$plot = renderBillboarder({
  #  billboarder() %>%
  #    bb_linechart(data = dataInput(), type = "spline")
  output$plot = renderHighchart({
    
    
    hc = highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Basic drilldown") %>%
      hc_xAxis(type = "category") %>%
      hc_legend(enabled = FALSE) %>%
      hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE)
        )
      ) %>%
      hc_add_series(
        name = "animals",
        data = year
      )
    hc %>%
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = list(
          list(
            id = "2016",
            data = list_parse2(year_2016)
          ),
          list(
            id="2017",
            data = list_parse2(year_2017)
          ),
          list(
            id="2018",
            data=list_parse2(year_2018)
          ))
      )
  })
}
shinyApp(ui, server)

