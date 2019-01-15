library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(lubridate)
library(shinyjs)
library(billboarder)
library(purrr)
library(viridisLite)
library(devtools)
library(purrr)
library(geojsonio)
library(httr)
library(stringr)


categories = read.csv("Category_Name.csv")
clients = read.csv("Customer.csv")
geolocation = read.csv("Geolocalization.csv")
orders = read.csv("Order.csv")
order_items = read.csv("Order_Items.csv")
order_payment = read.csv("Order_Payment.csv")
order_review = read.csv("order_Review.csv")
products = read.csv("Products.csv")
retailer = read.csv("Sellers.csv")

categories = data.frame(categories)
clients = data.frame(clients)
geolocation = data.frame(geolocation)
orders = data.frame(orders)
order_items = data.frame(order_items)
order_payment = data.frame(order_payment)
order_review = data.frame(order_review)
products = data.frame(products)
retailer = data.frame(retailer)

dataframe = merge(x = order_items, y = orders, by = "order_id")
dataframe = merge(x = dataframe, y = products, by = "product_id")
dataframe = merge(x = dataframe, y = retailer, by = "seller_id")
dataframe = merge(x = dataframe, y = order_payment, by = "order_id")
dataframe = merge(x = dataframe, y = clients, by = "customer_id")
dataframe = merge(x = dataframe, y = order_review, by = "order_id")

#add 4 empty columns to dataframe. The goal is add a latitude and longitude for the seller and the customer
new_columns = c("cus_lat", "cus_lng", "sel_lat", "sel_lng")
dataframe[, new_columns] = NA

#Populate the latitude and longitude
dataframe$sel_lat = geolocation$geolocation_lat[match(dataframe$seller_zip_code_prefix,
                                                      geolocation$geolocation_zip_code_prefix)]
dataframe$sel_lng = geolocation$geolocation_lng[match(dataframe$seller_zip_code_prefix,
                                                      geolocation$geolocation_zip_code_prefix)]
dataframe$cus_lat = geolocation$geolocation_lat[match(dataframe$customer_zip_code_prefix,
                                                      geolocation$geolocation_zip_code_prefix)]
dataframe$cus_lng = geolocation$geolocation_lng[match(dataframe$customer_zip_code_prefix,
                                                      geolocation$geolocation_zip_code_prefix)]

#Add a new Column with the right names of the states.
#https://en.wikipedia.org/wiki/States_of_Brazil
initials = c(
  "SP",
  "MG",
  "PR",
  "SC",
  "DF",
  "RS",
  "RJ",
  "GO",
  "MA",
  "ES",
  "BA",
  "PI",
  "RO",
  "MT",
  "CE",
  "RN",
  "PE",
  "SE",
  "MS",
  "PB",
  "PA",
  "AM",
  "AC"
)
states = c(
  "São Paulo",
  "Minas Gerais",
  "Paraná",
  "Santa Catarina",
  "Distrito Federal",
  "Rio Grande do Sul",
  "Rio de Janeiro",
  "Goiás",
  "Maranhão",
  "Espirito Santo",
  "Bahia",
  "Piauí",
  "Rondônia",
  "Mato Grosso",
  "Ceará",
  "Rio Grande do Norte",
  "Pernambuco",
  "Sergipe",
  "Mato Grosso do Sul",
  "Paraíba",
  "Pará",
  "Amazonas",
  "Acre"
)
region = c(
  "Sudeste",
  "Sudeste",
  "Nordeste",
  "Sul",
  "Centro-Oeste",
  "Sul",
  "Sudeste",
  "Centro-Oeste",
  "Nordeste",
  "Sudeste",
  "Nordeste",
  "Nordeste",
  "Norte",
  "Centro-Oeste",
  "Nordeste",
  "Nordeste",
  "Nordeste",
  "Nordeste",
  "Centro-Oeste",
  "Nordeste",
  "Norte",
  "Norte",
  "Norte"
)

#Create a dataframe to match the geography the states, regions and initials
geography_dataframe = data.frame(initials , states, region)
geography_dataframe[, c("Region_Initials")] = NA
geography_dataframe$Region_Initials[(geography_dataframe$region == "Sudeste")] = "SE"
geography_dataframe$Region_Initials[(geography_dataframe$region == "Nordeste")] = "NE"
geography_dataframe$Region_Initials[(geography_dataframe$region == "Sul")] = "S"
geography_dataframe$Region_Initials[(geography_dataframe$region == "Centro-Oeste")] = "CO"
geography_dataframe$Region_Initials[(geography_dataframe$region == "Norte")] = "N"

#Populate the state and the region
columns_geography = c(
  "customer_state_name",
  "customer_region",
  "seller_state_name",
  "seller_region",
  "cus_region_initials",
  "sel_region_initials"
)
dataframe[, columns_geography] = NA
dataframe$seller_state_name = geography_dataframe$states[match(dataframe$seller_state, geography_dataframe$initials)]
dataframe$seller_region = geography_dataframe$region[match(dataframe$seller_state, geography_dataframe$initials)]
dataframe$customer_state_name = geography_dataframe$states[match(dataframe$customer_state, geography_dataframe$initials)]
dataframe$customer_region = geography_dataframe$region[match(dataframe$customer_state, geography_dataframe$initials)]
dataframe$cus_region_initials = geography_dataframe$Region_Initials[match(dataframe$customer_state, geography_dataframe$initials)]
dataframe$sel_region_initials = geography_dataframe$Region_Initials[match(dataframe$seller_state, geography_dataframe$initials)]

#drop the unknown states from customer
dataframe = dataframe[!is.na(dataframe$customer_state), ]

#drop the unknown states from seller
dataframe = dataframe[!is.na(dataframe$seller_state), ]
dataframe = subset(dataframe,
                   select = -c(seller_id, product_id, order_id, customer_id, review_id))
#Drop not delivered products
dataframe = dataframe[dataframe$order_status == 'delivered', ] #lost 3420 rows
dataframe = dataframe[year(as.Date(
  as.character(dataframe$order_purchase_timestamp, format = "%Y-%m-%d")
)) != 2016, ]

#Try discovery some null values
category_name_vector = as.vector(dataframe['product_category_name']) # 0 records
dataframe = dataframe[dataframe$product_category_name != '', ] #Lost 1638 rows

dataframe = dataframe[!is.na(dataframe$order_purchase_timestamp), ]

#Prepare Categories

#Unique Values | Count
categories_unique = data.frame(Category = dataframe$product_category_name)
categories_unique = data.frame(table(categories_unique$Category))

#Books
column_main_category = c("Category")
dataframe[, column_main_category] = NA
dataframe$Category[(dataframe$product_category_name == 'livros_tecnicos')] = "Books"
dataframe$Category[(dataframe$product_category_name == 'livros_importados')] = "Books"
dataframe$Category[(dataframe$product_category_name == 'livros_interesse_geral')] = "Books"
dataframe$Category[(dataframe$product_category_name == 'papelaria')] = "Books"

#Others
dataframe$Category[(dataframe$product_category_name == 'cool_stuff')] = "Others"
dataframe$Category[(dataframe$product_category_name == 'market_place')] = "Others"
dataframe$Category[(dataframe$product_category_name == 'industria_comercio_e_negocios')] = "Others"
dataframe$Category[(dataframe$product_category_name == 'flores')] = "Others"
dataframe$Category[(dataframe$product_category_name == 'artigos_de_natal')] = "Others"
dataframe$Category[(dataframe$product_category_name == 'seguros_e_servicos')] = "Others"
dataframe$Category[(dataframe$product_category_name == 'pet_shop')] = "Others"

#Art
dataframe$Category[(dataframe$product_category_name == 'artes_e_artesanato')] = "Arts"
dataframe$Category[(dataframe$product_category_name == 'artes')] = "Arts"

#Food
dataframe$Category[(dataframe$product_category_name == 'alimentos_bebidas')] = "Grocery"
dataframe$Category[(dataframe$product_category_name == 'agro_industria_e_comercio')] = "Grocery"
dataframe$Category[(dataframe$product_category_name == 'alimentos')] = "Grocery"
dataframe$Category[(dataframe$product_category_name == 'bebidas')] = "Grocery"

#Home
dataframe$Category[(dataframe$product_category_name == 'moveis_decoracao')] = 'Home'
dataframe$Category[(dataframe$product_category_name == 'utilidades_domesticas')] = 'Home'
dataframe$Category[(dataframe$product_category_name == 'cama_mesa_banho')] = 'Home'
dataframe$Category[(dataframe$product_category_name == 'moveis_escritorio')] = 'Home'
dataframe$Category[(dataframe$product_category_name == 'eletronicos')] = 'Home'
dataframe$Category[(dataframe$product_category_name == 'eletrodomesticos')] = 'Home'
dataframe$Category[(
  dataframe$product_category_name == 'moveis_cozinha_area_de_servico_jantar_e_jardim'
)] = 'Home'
dataframe$Category[(dataframe$product_category_name == 'climatizacao')] = 'Home'
dataframe$Category[(dataframe$product_category_name == 'casa_conforto')] = 'Home'
dataframe$Category[(dataframe$product_category_name == 'portateis_casa_forno_e_cafe')] = 'Home'
dataframe$product_category_name[(dataframe$product_category_name == 'portateis_casa_forno_e_cafe')] = 'portateis_cozinha_e_preparadores_de_alimentos'
dataframe$Category[(dataframe$product_category_name == 'eletroportateis')] = 'Home'
dataframe$Category[(dataframe$product_category_name == 'moveis_sala')] = 'Home'
dataframe$Category[(dataframe$product_category_name == 'eletrodomesticos_2')] = 'Home'
dataframe$product_category_name[(dataframe$product_category_name == 'eletrodomesticos_2')] = 'eletrodomesticos'
dataframe$Category[(dataframe$product_category_name == 'moveis_quarto')] = 'Home'
dataframe$Category[(dataframe$product_category_name == 'casa_conforto_2')] = 'Home'
dataframe$product_category_name[(dataframe$product_category_name == 'casa_conforto_2')] = 'casa_conforto'
dataframe$Category[(
  dataframe$product_category_name == 'portateis_cozinha_e_preparadores_de_alimentos'
)] = 'Home'
dataframe$Category[(dataframe$product_category_name == 'moveis_colchao_e_estofado')] = 'Home'
dataframe$Category[(dataframe$product_category_name == 'la_cuisine')] = "Home"

#Fashion
dataframe$Category[(dataframe$product_category_name == 'perfumaria')] = 'Fashion'
dataframe$Category[(dataframe$product_category_name == 'beleza_saude')] = 'Fashion'
dataframe$Category[(dataframe$product_category_name == 'fashion_bolsas_e_acessorios')] = 'Fashion'
dataframe$Category[(dataframe$product_category_name == 'fashion_underwear_e_moda_praia')] = 'Fashion'
dataframe$Category[(dataframe$product_category_name == 'fashion_roupa_masculina')] = 'Fashion'
dataframe$Category[(dataframe$product_category_name == 'malas_acessorios')] = "Fashion"
dataframe$product_category_name[(dataframe$product_category_name == 'malas_acessorios')] = 'fashion_bolsas_e_acessorios'
dataframe$Category[(dataframe$product_category_name == 'fashion_calcados')] = "Fashion"
dataframe$Category[(dataframe$product_category_name == 'fashion_roupa_feminina')] = "Fashion"
dataframe$Category[(dataframe$product_category_name == 'relogios_presentes')] = "Fashion"

#Mobiles
dataframe$Category[(dataframe$product_category_name == 'telefonia')] = 'Telephones'
dataframe$Category[(dataframe$product_category_name == 'telefonia_fixa')] = 'Telephones'

#Sports
dataframe$Category[(dataframe$product_category_name == 'esporte_lazer')] = 'Sports'
dataframe$Category[(dataframe$product_category_name == 'fashion_esporte')] = 'Sports'

#Music
dataframe$Category[(dataframe$product_category_name == 'cds_dvds_musicais')] = 'Media'
dataframe$Category[(dataframe$product_category_name == 'audio')] = 'Media'
dataframe$product_category_name[(dataframe$product_category_name == 'audio')] = 'cds_dvds_musicais'
dataframe$Category[(dataframe$product_category_name == 'instrumentos_musicais')] = 'Media'
dataframe$Category[(dataframe$product_category_name == 'cine_foto')] = 'Media'
dataframe$Category[(dataframe$product_category_name == 'musica')] = 'Media'
dataframe$product_category_name[(dataframe$product_category_name == 'musica')] = 'cds_dvds_musicais'
dataframe$Category[(dataframe$product_category_name == 'dvds_blu_ray')] = 'Media'

#Computing
dataframe$Category[(dataframe$product_category_name == 'consoles_games')] = 'Computing'
dataframe$Category[(dataframe$product_category_name == 'informatica_acessorios')] = 'Computing'
dataframe$Category[(dataframe$product_category_name == 'pcs')] = 'Computing'
dataframe$Category[(dataframe$product_category_name == 'tablets_impressao_imagem')] = 'Computing'
dataframe$Category[(dataframe$product_category_name == 'pc_gamer')] = 'Computing'


#Construction
dataframe$Category[(dataframe$product_category_name == 'ferramentas_jardim')] = "Construction"
dataframe$Category[(dataframe$product_category_name == 'construcao_ferramentas_construcao')] = "Construction"
dataframe$Category[(dataframe$product_category_name == 'construcao_ferramentas_iluminacao')] = "Construction"
dataframe$Category[(dataframe$product_category_name == 'construcao_ferramentas_jardim')] = "Construction"
dataframe$product_category_name[(dataframe$product_category_name == 'construcao_ferramentas_jardim')] = 'ferramentas_jardim'
dataframe$Category[(dataframe$product_category_name == 'casa_construcao')] = "Construction"
dataframe$Category[(dataframe$product_category_name == 'construcao_ferramentas_seguranca')] = "Construction"
dataframe$Category[(dataframe$product_category_name == 'construcao_ferramentas_ferramentas')] = "Construction"

#Child
dataframe$Category[(dataframe$product_category_name == 'bebes')] = "Child"
dataframe$Category[(dataframe$product_category_name == 'brinquedos')] = "Child"
dataframe$Category[(dataframe$product_category_name == 'fraldas_higiene')] = "Child"
dataframe$Category[(dataframe$product_category_name == 'artigos_de_festas')] = "Child"
dataframe$Category[(dataframe$product_category_name == 'fashion_roupa_infanto_juvenil')] = "Child"

#Car
dataframe$Category[(dataframe$product_category_name == 'automotivo')] = "Car"
dataframe$Category[(dataframe$product_category_name == 'sinalizacao_e_seguranca')] = "Car"


#Fill the columns
sell_history = data.frame(Date = dataframe$order_purchase_timestamp)
sell_history$Date_ = as.Date(as.character(sell_history$Date))
sell_history$Hour = hour(sell_history$Date)
sell_history$Weekday = weekdays(as.Date(as.character(sell_history$Date)))
sell_history$Day = day(sell_history$Date)
sell_history$Month = month(sell_history$Date)
sell_history$Year = year(sell_history$Date)
sell_history$price = dataframe$price


#Yearly Sales
year_sell = data.frame(Data = sell_history$Date_,
                       Price = sell_history$price,
                       Year = sell_history$Year)
year_sell$Date = as.Date(as.character(sell_history$Date, format = "%m/%B/%Y"))
year_sell = group_by(year_sell, Year)
year_sell = summarise(year_sell, Price = sum(Price))
year_sell = data.frame(Year = year_sell$Year, Price = year_sell$Price)

#monthly sales
month_sell = data.frame(Date = sell_history$Date_,
                        Price = sell_history$price,
                        Month = sell_history$Month)
month_sell$Date = as.Date(as.character(sell_history$Date, format = "%m/%B/%Y"))
month_sell = group_by(month_sell, Month)
month_sell = summarise(month_sell, Price = sum(Price))
month_sell = data.frame(Month = month_sell$Month, Price = year_sell$Price)

#Dates
date_column_purchase = c("Date_Purchase")
dataframe[date_column_purchase, ] = NA
dataframe$Date_Purchase = as.Date(as.character(dataframe$order_purchase_timestamp, format = "%m/%B/%Y"))

#First Plot
group_date = data.frame(Date = dataframe$Date_Purchase, Price = dataframe$price)
group_date$Date = as.Date(as.character(group_date$Date, format = "%Y-%m-%d"))
group_date = group_by(group_date, Date)
group_date = summarise(group_date, Price = sum(Price))
group_date = group_date[!is.na(group_date$Date), ]
group_date$Date = as.Date(as.character(group_date$Date, format = "%Y-%m-%d"))
group_date = as.data.frame.table(group_date)
group_date_columns = c("Month", "Year")
group_date[, group_date_columns] = NA
group_date = data.frame(
  Date = group_date$Freq.Date,
  Price = group_date$Freq.Price,
  Month = group_date$Month,
  Year = group_date$Year
)
group_date$Month = month(as.Date(as.character(group_date$Date, format =
                                                "%Y-%m-%d")))
group_date$Year = year(as.Date(as.character(group_date$Date, format = "%Y-%m-%d")))


#plot1 to data
year_sell = data.frame(Data = sell_history$Date_,
                       Price = sell_history$price,
                       Year = sell_history$Year)
year_sell$Date = as.Date(as.character(sell_history$Date, format = "%Y-%m-%d"))
year_sell = group_by(year_sell, Year)
year_sell = summarise(year_sell, Price = sum(Price))
year_sell = data.frame(Year = year_sell$Year, Price = year_sell$Price)

year = data_frame(
  name = c('2017', '2018'),
  y = c(year_sell[1, 2], year_sell[2, 2]),
  drilldown = tolower(name)
)
year_list = list_parse(year)

year_2017 = sell_history[sell_history$Year == "2017", ]
year_2017 = subset(year_2017, select = -c(Date, Date_, Hour, Weekday, Day, Year))
year_2017 = group_by(year_2017, Month)
year_2017 = summarise(year_2017, Price = sum(price))
year_2017 = data.frame(
  name = c(
    'January',
    'Februrary',
    'March',
    'April',
    'May',
    'June',
    'July',
    'August',
    'September',
    'October',
    'November',
    'December'
  ),
  Price = year_2017
)
year_2017 = subset(year_2017, select = -c(Price.Month))

year_2018 = sell_history[sell_history$Year == "2018", ]
year_2018 = subset(year_2018, select = -c(Date, Date_, Hour, Weekday, Day, Year))
year_2018 = group_by(year_2018, Month)
year_2018 = summarise(year_2018, Price = sum(price))
year_2018 = data.frame(
  name = c(
    'January',
    'Februrary',
    'March',
    'April',
    'May',
    'June',
    'July',
    'August'
  ),
  Price = year_2018
)
year_2018 = subset(year_2018, select = -c(Price.Month))


category_name = data.frame(
  Category = dataframe$Category,
  Price = dataframe$price,
  Year = year(as.Date(
    as.character(dataframe$order_purchase_timestamp, format = "%Y-%m-%d")
  )),
  Month = month(as.Date(
    as.character(dataframe$order_purchase_timestamp, format = "%Y-%m-%d")
  ))
)



subcat = data.frame(
  Category = dataframe$Category,
  Subcat = dataframe$product_category_name,
  Price = dataframe$price,
  Year = year(as.Date(as.character(dataframe$order_purchase_timestamp, format = "%Y-%m-%d"))),
  Month = month(as.Date(as.character(dataframe$order_purchase_timestamp, format = "%Y-%m-%d"))),
  Siglas = dataframe$cus_region_initials,
  Date = dataframe$order_purchase_timestamp,
  customer_state = dataframe$customer_state
)
#dataframe$Category[(dataframe$product_category_name == 'climatizacao')] = 'Home'
subcat$Subcat = as.character(subcat$Subcat)
subcat$Subcat[subcat$Subcat == 'alimentos_bebidas'] = "Snacks"
subcat$Subcat[subcat$Subcat == 'bebidas'] = 'Drinks'
subcat$Subcat[subcat$Subcat == 'alimentos'] = 'Food'
subcat$Subcat[subcat$Subcat == 'agro_industria_e_comercio'] = 'Resales'
subcat$Subcat[subcat$Subcat == 'seguros_e_servicos'] = 'Insurance'
subcat$Subcat[subcat$Subcat == 'flores'] = 'Flowers'
subcat$Subcat[subcat$Subcat == 'artigos_de_natal'] = 'Christmas'
subcat$Subcat[subcat$Subcat == 'market_place'] = 'Resale'
subcat$Subcat[subcat$Subcat == 'industria_comercio_e_negocios'] = 'Business'
subcat$Subcat[subcat$Subcat == 'pet_shop'] = 'Pets'
subcat$Subcat[subcat$Subcat == 'cool_stuff'] = "Cool Stuff"
subcat$Subcat[subcat$Subcat == 'la_cuisine'] = 'Kitchen'
subcat$Subcat[subcat$Subcat == 'moveis_colchao_e_estofado'] = 'Mattress'
subcat$Subcat[subcat$Subcat == 'moveis_quarto'] = 'Bedroom Furniture'
subcat$Subcat[subcat$Subcat == 'moveis_cozinha_area_de_servico_jantar_e_jardim'] = 'Outdoor Furniture'
subcat$Subcat[subcat$Subcat == 'portateis_cozinha_e_preparadores_de_alimentos'] = 'Portable Kitchen Appliances'
subcat$Subcat[subcat$Subcat == 'climatizacao'] = 'Air Conditioning'
subcat$Subcat[subcat$Subcat == 'casa_conforto'] = 'Comfort'
subcat$Subcat[subcat$Subcat == 'moveis_sala'] = 'Living Room Furniture'
subcat$Subcat[subcat$Subcat == 'eletronicos'] = 'Eletronics'
subcat$Subcat[subcat$Subcat == 'eletroportateis'] = 'Portable Eletronics'
subcat$Subcat[subcat$Subcat == 'eletrodomesticos'] = 'Appliances'
subcat$Subcat[subcat$Subcat == 'moveis_escritorio'] = 'Office Furniture'
subcat$Subcat[subcat$Subcat == 'utilidades_domesticas'] = 'Utils'
subcat$Subcat[subcat$Subcat == 'moveis_decoracao'] = 'Decorations'
subcat$Subcat[subcat$Subcat == 'cama_mesa_banho'] = 'Bed & Bath'
subcat$Subcat[subcat$Subcat == 'dvds_blu_ray'] = 'DVDs'
subcat$Subcat[subcat$Subcat == 'cine_foto'] = 'Movie Posters'
subcat$Subcat[subcat$Subcat == 'cds_dvds_musicais'] = 'CDs'
subcat$Subcat[subcat$Subcat == 'instrumentos_musicais'] = 'Instruments'
subcat$Subcat[subcat$Subcat == 'sinalizacao_e_seguranca'] = 'Safety'
subcat$Subcat[subcat$Subcat == 'automotivo'] = 'Motor'
subcat$Subcat[subcat$Subcat == 'artes_e_artesanato'] = 'Crafts'
subcat$Subcat[subcat$Subcat == 'artes'] = 'Art'
subcat$Subcat[subcat$Subcat == 'construcao_ferramentas_ferramentas'] = 'Tools'
subcat$Subcat[subcat$Subcat == 'construcao_ferramentas_seguranca'] = 'Safety tools'
subcat$Subcat[subcat$Subcat == 'construcao_ferramentas_iluminacao'] = 'Lighting'
subcat$Subcat[subcat$Subcat == 'casa_construcao'] = 'Home'
subcat$Subcat[subcat$Subcat == 'construcao_ferramentas_construcao'] = 'Constuction Tools'
subcat$Subcat[subcat$Subcat == 'ferramentas_jardim'] = 'Garden Tools'
subcat$Subcat[subcat$Subcat == 'pc_gamer'] = 'Gamer PCs'
subcat$Subcat[subcat$Subcat == 'tablets_impressao_imagem'] = 'Tablets'
subcat$Subcat[subcat$Subcat == 'consoles_games'] = 'Games'
subcat$Subcat[subcat$Subcat == 'pcs'] = 'PCs'
subcat$Subcat[subcat$Subcat == 'informatica_acessorios'] = 'Accessories'
subcat$Subcat[subcat$Subcat == 'fashion_roupa_infanto_juvenil'] = 'Clothes'
subcat$Subcat[subcat$Subcat == 'fraldas_higiene'] = 'Diapers'
subcat$Subcat[subcat$Subcat == 'artigos_de_festas'] = 'Party'
subcat$Subcat[subcat$Subcat == 'bebes'] = 'Baby'
subcat$Subcat[subcat$Subcat == 'brinquedos'] = 'Toys'
subcat$Subcat[subcat$Subcat == 'fashion_roupa_feminina'] = 'Woman'
subcat$Subcat[subcat$Subcat == 'fashion_underwear_e_moda_praia'] = 'Underwear'
subcat$Subcat[subcat$Subcat == 'fashion_roupa_masculina'] = 'Men'
subcat$Subcat[subcat$Subcat == 'fashion_calcados'] = 'Shoes'
subcat$Subcat[subcat$Subcat == 'fashion_bolsas_e_acessorios'] = 'Fashion Accessories'
subcat$Subcat[subcat$Subcat == 'perfumaria'] = 'Perfume'
subcat$Subcat[subcat$Subcat == 'relogios_presentes'] = 'Watches'
subcat$Subcat[subcat$Subcat == 'beleza_saude'] = 'Beauty'
subcat$Subcat[subcat$Subcat == 'livros_importados'] = 'Forgein'
subcat$Subcat[subcat$Subcat == 'livros_tecnicos'] = 'Tecnical'
subcat$Subcat[subcat$Subcat == 'livros_interesse_geral'] = 'Others'
subcat$Subcat[subcat$Subcat == 'papelaria'] = 'Stationary'
subcat$Subcat[subcat$Subcat == 'fashion_esporte'] = 'Clothing'
subcat$Subcat[subcat$Subcat == 'esporte_lazer'] = 'Equipment'
subcat$Subcat[subcat$Subcat == 'telefonia_fixa'] = 'Landline'
subcat$Subcat[subcat$Subcat == 'telefonia'] = 'Mobile'

#Region Map
region_map_df = data.frame(Initials = dataframe$cus_region_initials, Price = dataframe$price)
region_map_df = group_by(region_map_df, Initials)
region_map_df = summarise(region_map_df, Price=sum(Price))
region_map_df = data.frame(Initials = region_map_df$Initials, Price=region_map_df$Price)
region_map_df = region_map_df[!is.na(region_map_df$Initials),]
#id column

#If we want, we can use also https://raw.githubusercontent.com/GeoKing88/visualization/master/regioes_2010.geojson, but takes more time to load

brasil_region_map = "https://raw.githubusercontent.com/GeoKing88/visualization/master/regioes_2010_3.geojson" %>% 
  GET() %>% 
  content() %>% 
  jsonlite::fromJSON(simplifyVector = FALSE)

id_column = c("id")
region_map_df[, id_column] = NA
region_map_df$id[region_map_df$Initials == "S"] = 1
region_map_df$id[region_map_df$Initials == "SE"] = 2
region_map_df$id[region_map_df$Initials == "N"] = 3
region_map_df$id[region_map_df$Initials == "NE"] = 4
region_map_df$id[region_map_df$Initials == "CO"] = 5

region_map_df_2018 = data.frame(Date = dataframe$order_purchase_timestamp, Initials = dataframe$cus_region_initials, Price=dataframe$price)
region_map_df_2018 = region_map_df_2018[year(as.Date(as.character(region_map_df_2018$Date, format = "%Y-%m-%d"))) == "2018",]
region_map_df_2018 = subset(region_map_df_2018, select = -c(Date))
region_map_df_2018 = group_by(region_map_df_2018, Initials)
region_map_df_2018 = summarise(region_map_df_2018, Price = sum(Price))
region_map_df_2018 = data.frame(Initials = region_map_df_2018$Initials, Price=region_map_df_2018$Price)
region_map_df_2018 = region_map_df_2018[!is.na(region_map_df_2018$Initials),]


id_column = c("id")
region_map_df_2018[, id_column] = NA
region_map_df_2018$id[region_map_df_2018$Initials == "S"] = 1
region_map_df_2018$id[region_map_df_2018$Initials == "SE"] = 2
region_map_df_2018$id[region_map_df_2018$Initials == "N"] = 3
region_map_df_2018$id[region_map_df_2018$Initials == "NE"] = 4
region_map_df_2018$id[region_map_df_2018$Initials == "CO"] = 5

#State Map
state_map_df = data.frame(Initials = paste("BR.",dataframe$customer_state), Price = dataframe$price)
state_map_df = group_by(state_map_df, Initials)
state_map_df = summarise(state_map_df, Price = sum(Price))
state_map_df = data.frame(Initials = state_map_df$Initials, Price = state_map_df$Price)
state_map_df = data.frame(Initials = str_replace_all(state_map_df$Initials, fixed(" "), replacement = ""), Price = state_map_df$Price)
#state_map_df$Initials = dQuote(state_map_df$Initials)
state_map_df = state_map_df[!is.na(state_map_df$Price),]
colnames(state_map_df) = c("hasc", "Price")

url = "http://code.highcharts.com/mapdata/countries/br/br-all.geo.json"
tmpfile = tempfile(fileext = ".json")
download.file(url, tmpfile)
brasil_states_map = readLines(tmpfile)
brasil_states_map = gsub(".* = ", "", brasil_states_map)
brasil_states_map = jsonlite::fromJSON(brasil_states_map, simplifyVector = FALSE)
highchart(type = "map") %>% 
  hc_add_series_map(map = brasil_states_map, df = state_map_df, joinBy = "hasc", value = "Price")

region_map_df2 = data.frame(Date = dataframe$order_purchase_timestamp, sigla = dataframe$cus_region_initials, Price=dataframe$price)
region_map_df2 = region_map_df2[year(as.Date(as.character(region_map_df2$Date, format = "%Y-%m-%d"))) == "2018",]
region_map_df2 = subset(region_map_df2, select = -c(Date))
region_map_df2 = group_by(region_map_df2, sigla)
region_map_df2 = summarise(region_map_df2, Price = sum(Price))
region_map_df2 = data.frame(sigla = region_map_df2$sigla, Price=region_map_df2$Price)
region_map_df2 = region_map_df2[!is.na(region_map_df2$sigla),]



create_map = function(administration_division = "Region", display_map = brasil_region_map,
                      year_condition=NULL,month_condition = NULL, category=NULL, subcategory=NULL){
  join_keys = 'sigla'
  if (administration_division == "State"){
    dataframe2 = data.frame(Date = subcat$Date, 
                            sigla = paste("BR.",subcat$customer_state), Price=subcat$Price, 
                            Category = subcat$Category, SubCategory = subcat$Subcat)
    display_map = brasil_states_map
    join_keys = 'hasc'
  }else if (administration_division=="Region"){
    dataframe2 = data.frame(Date = subcat$Date, 
                            sigla = subcat$Siglas, Price=subcat$Price, 
                            Category = subcat$Category, SubCategory = subcat$Subcat)

  }else{
    stop("Choose between State or Region Administration Division")
  }
  if (!is.null(year_condition)){
    print("É Aqui")
    dataframe2 = dataframe2[as.numeric(year(as.Date(as.character(dataframe2$Date, format = "%Y-%m-%d")))) == year_condition,]
  }
  if (!is.null(month_condition)){
    dataframe2 = dataframe2[as.numeric(month(as.Date(as.character(dataframe2$Date, format = "%Y-%m-%d")))) == month_condition,]
  }
  if (!is.null(category)){
    print("categoria")
    dataframe2 = dataframe2[dataframe2$Category %in% category,]
  }
  if (!is.null(subcategory)){
    print("subcategoria")
    dataframe2 = dataframe2[dataframe2$SubCategory %in% subcategory,]
  }
  print('aqui?')
  dataframe2 = subset(dataframe2, select = c(sigla, Price))
  dataframe2 = group_by(dataframe2, sigla)
  dataframe2 = summarise(dataframe2, Price = sum(Price))
  if(administration_division=="State"){
    dataframe2 = data.frame(hasc = str_replace_all(dataframe2$sigla, fixed(" "), replacement = ""), Price = dataframe2$Price)
  }else{
    dataframe2 = data.frame(sigla = dataframe2$sigla, Price = dataframe2$Price)
  }
  print("antes de returnar")
  hc =highchart(type = "map") %>% 
    hc_add_series_map(map = display_map , df = dataframe2 , joinBy = join_keys, value = "Price") %>%
    hc_title(text="Review")%>%
    hc_subtitle(text="Brazil - Region")%>%
    hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
    hc_mapNavigation(enabled = TRUE)
  
  

  
  return(hc)
}
#create_map(administration_division = "State", display_map = brasil_states_map)


#Lets try shinny

#Lets try shinny
header = dashboardHeader(title = "Olist Sales Visualization", titleWidth = 300,
                         includeCSS("www/style.css"),
                         includeCSS("www/bootstrap.css"),
                         includeScript("www/boostrap.js"))
#sidebar
sidebar = dashboardSidebar( 
  title = "Sales Mapping",
  radioButtons(
    inputId = "map_radio_button",
    label = "Divisão Administrativa",
    choices =
      c("Region", "State"),
    selected = c("State")
  )
)
#selectInput ("segundo", "Cities", choices = list(unique(geolocation[,"geolocation_city"]))) )
body = dashboardBody(tags$head(tags$style(
  HTML(
    '
    .content-wrapper {
    background-color: #aaaaaa;
    }
    .skin-green .main-header .logo{
    background-color: #53917e;
    }
    .skin-green .main-header .navbar{
    background-color: #53917e;
    }
    .skin-green .left-side, .skin-green .main-sidebar, .skin-green .wrapper{
    background-color: #53917e;
    }
    .skin-green .sidebar-menu>li.active>a, .skin-green .sidebar-menu>li:hover>a{
    background: #53917e;
    border-left-color: #53917e;
    }
    '
  )
  )),
  fluidRow(
    #sidebarPanel(
    #  selectInput("primeiro", "segundo", unique(group_date$Year))
    #),
    box(title = "Sales over Time",
        highchartOutput("plot")),
    box(title = "Sales Mapping", highchartOutput("plot3"))
    
  ),
  fluidRow(box(title = "Sales per Category", highchartOutput("plot2"), width = "12")))

ui = dashboardPage(title = 'Olist Sales Visualization', skin = 'green',
                   header, sidebar, body)

  
  #AQUI!!!!
  

#shinyApp(ui = ui, server = server)

server = function(input, output) {


  map88 = reactive({
    year_c = NULL
    month_c = NULL
    category_c=NULL
    subcategory_c=NULL
    if (NROW(na.omit(category_name[category_name$Year == as.numeric(input$canvasClicked2[1]),])) >
        0) {
      year_c = as.numeric(input$canvasClicked2[1])
    }
    else if (NROW(na.omit(category_name[category_name$Month == as.numeric(input$canvasClicked2[1]),])) >
             0) {
      if (all(input$canvasClicked2[2] == "Sales per Month 2018")) {
        year_c = 2018
        month_c = as.numeric(input$canvasClicked2[1])
      } else{
        year_c = 2017
        month_c = as.numeric(input$canvasClicked2[1])
      }
    }
    if(NROW(na.omit(subcat[subcat$Category %in% input$canvasClicked,]))>0){
      category_c = input$canvasClicked
    }else if (NROW(na.omit(subcat[subcat$Subcat %in% input$canvasClicked,]))>0){
      subcategory_c = input$canvasClicked
    }
    
  
      if (input$map_radio_button == "State"){
        print("Entrei bababababb")
        create_map(administration_division = "State", display_map = brasil_states_map,  year_condition=year_c, month_condition = month_c, category=category_c, subcategory=subcategory_c)
      }else{
        create_map(year_condition=year_c,month_condition = month_c, category=category_c, subcategory=subcategory_c)
      }
    
    
  })
  cats = reactive({
    #plot2
    print(input$canvasClicked2[1])
    print(input$canvasClicked2[2]) 
    if (NROW(na.omit(category_name[category_name$Year == as.numeric(input$canvasClicked2[1]),])) >
        0) {
      category_namex = category_name[category_name$Year == as.numeric(input$canvasClicked2[1]),]
    }
    else if (NROW(na.omit(category_name[category_name$Month == as.numeric(input$canvasClicked2[1]),])) >
             0) {
      if (all(input$canvasClicked2[2] == "Sales per Month 2018")) {
        print('mes de 2018')
        category_namex = category_name[category_name$Year == '2018',]
        category_namex = category_namex[category_namex$Month == as.numeric(input$canvasClicked2[1]),]
      } else{
        print('mes de 2017')
        category_namex = category_name[category_name$Year == '2017',]
        category_namex = category_namex[category_namex$Month == as.numeric(input$canvasClicked2[1]),]
      }
    } else{
      years2 = as.numeric(unique(year$name))
      
      category_namex = category_name[category_name$Year %in% years2,]
    }
    
    print(input$canvasClicked2)
    category_price = group_by(category_namex, Category)
    category_price = summarise(category_price, Price = sum(Price))
    category_price = data.frame(Category = category_price$Category , Price = category_price$Price)
    category_price = category_price[!is.na(category_price$Category), ]
    
    
    category = data_frame(
      name = category_price$Category ,
      y = c(category_price$Price),
      drilldown = name
    )
    category = category[order(category$name), ]
    category
  })
  
  
  
  # A reactive function for tree map
  dataInput = reactive({
    if (NROW(na.omit(subcat[subcat$Year == input$canvasClicked2[1],])) >
        0) {
      subcats = subcat[subcat$Year == as.numeric(input$canvasClicked2[1]),]
    }
    else if (NROW(na.omit(subcat[subcat$Month ==  as.numeric(input$canvasClicked2[1]),])) >
             0) {
      if (all(input$canvasClicked2[2] == "Sales per Month 2018")) {
        print('mes de 2018')
        subcats = subcat[subcat$Year == '2018',]
        subcats = subcats[subcats$Month ==  as.numeric(input$canvasClicked2[1]),]
      } else{
        print('mes de 2017')
        subcats = subcat[subcat$Year == '2017',]
        subcats = subcats[subcats$Month == as.numeric(input$canvasClicked2[1]),]
      }
    } else{
      years2 = as.numeric(unique(year$name))
      
      subcats = subcat[subcat$Year %in% years2,]
    }
    
    
    subcats = subcats %>%
      group_by(Category, Subcat) %>%
      summarise(Price = sum(Price))
    subcats = data.frame(subcats[order(subcats$Subcat), ])
    subcats = subcats[complete.cases(subcats),]
    serie = list()
    for (i in cats()$drilldown) {
      serie = append(serie, list(list(
        id = i,
        name = 'Sales per Subcategory',
        data = list_parse2(subset(subcats[subcats$Category == i, ], select = -c(Category)))
      )))
    }
    serie
  })
  
  
  
  dataInput2 = reactive({
    year_cat = data.frame(Category = subcat$Category, Price = subcat$Price, Year = subcat$Year, Subcat = subcat$Subcat) 
    if(NROW(na.omit(year_cat[year_cat$Category %in% input$canvasClicked,]))>0){
      year_cat = year_cat[year_cat$Category %in% input$canvasClicked,]
      year_cat = year_cat[!is.na(year_cat$Category), ]
    }else if (NROW(na.omit(year_cat[year_cat$Subcat %in% input$canvasClicked,]))>0){
      year_cat = year_cat[year_cat$Subcat %in% input$canvasClicked,]
      year_cat = year_cat[!is.na(year_cat$Subcat), ]
    }else {
      year_cat = year_cat[!is.na(year_cat$Subcat), ]
      year_cat = year_cat[!is.na(year_cat$Category), ]
    }
    
    
    year_cat_final = subset(year_cat, select=c( Year, Price))
    year_cat_final = group_by(year_cat_final, Year)
    year_cat_final = summarise(year_cat_final, Price= sum(Price))
    year_cat_final = data.frame(Year = year_cat_final$Year , Price = year_cat_final$Price)
    year_cat_final$Year = as.character(year_cat_final$Year)
    #year_cat_final = year_cat_final[!is.na(year_final_cat$Year),]
    final = data_frame(name = year_cat_final$Year, y= year_cat_final$Price, drilldown = year_cat_final$Year)
    final = final[order(final$name),]
    final
  })
  
  dataInput2018 = reactive({
    
    year_cat = data.frame(Category = subcat$Category, Price = subcat$Price, Year = subcat$Year, Month = subcat$Month, Subcat = subcat$Subcat) 
    if(NROW(na.omit(year_cat[year_cat$Category %in% input$canvasClicked,]))>0){
      year_cat = year_cat[year_cat$Category %in% input$canvasClicked,]
      year_cat = year_cat[!is.na(year_cat$Category), ]
    }else if (NROW(na.omit(year_cat[year_cat$Subcat %in% input$canvasClicked,]))>0){
      year_cat = year_cat[year_cat$Subcat %in% input$canvasClicked,]
      year_cat = year_cat[!is.na(year_cat$Subcat), ]
    }else {
      year_cat = year_cat[!is.na(year_cat$Subcat), ]
      year_cat = year_cat[!is.na(year_cat$Category), ]
    }
    
    year_cat = year_cat[year_cat$Year =='2018',]
    year_cat_final = subset(year_cat, select=c( Year, Price, Month))
    year_cat_final = group_by(year_cat_final, Year, Month)
    year_cat_final = summarise(year_cat_final, Price= sum(Price))
    year_cat_final = data.frame(Month = year_cat_final$Month , Price = year_cat_final$Price, Year = year_cat_final$Year)
    year_cat_final$Month = as.character(year_cat_final$Month)
    year_cat_final$Year = as.character(year_cat_final$Year)
    #year_cat_final = year_cat_final[!is.na(year_final_cat$Year),]
    final = data_frame(Month = year_cat_final$Month, y= c(year_cat_final$Price), Year= year_cat_final$Year, drilldown = Year)
    final = subset(final, select= -c(Year))
    final = final[order(as.numeric(final$Month)),]
    
    final
  })
  
  dataInput2017 = reactive({
    year_cat = data.frame(Category = subcat$Category, Price = subcat$Price, Year = subcat$Year, Month = subcat$Month, Subcat = subcat$Subcat) 
    if(NROW(na.omit(year_cat[year_cat$Category %in% input$canvasClicked,]))>0){
      year_cat = year_cat[year_cat$Category %in% input$canvasClicked,]
      year_cat = year_cat[!is.na(year_cat$Category), ]
    }else if (NROW(na.omit(year_cat[year_cat$Subcat %in% input$canvasClicked,]))>0){
      year_cat = year_cat[year_cat$Subcat %in% input$canvasClicked,]
      year_cat = year_cat[!is.na(year_cat$Subcat), ]
    }else {
      year_cat = year_cat[!is.na(year_cat$Subcat), ]
      year_cat = year_cat[!is.na(year_cat$Category), ]
    }
    
    year_cat = year_cat[year_cat$Year =='2017',]
    year_cat_final = subset(year_cat, select=c( Year, Price, Month))
    year_cat_final = group_by(year_cat_final, Year, Month)
    year_cat_final = summarise(year_cat_final, Price= sum(Price))
    year_cat_final = data.frame(Month = year_cat_final$Month , Price = year_cat_final$Price, Year = year_cat_final$Year)
    year_cat_final$Month = as.character(year_cat_final$Month)
    year_cat_final$Year = as.character(year_cat_final$Year)
    #year_cat_final = year_cat_final[!is.na(year_final_cat$Year),]
    final = data_frame(Month = year_cat_final$Month, y= c(year_cat_final$Price), Year= year_cat_final$Year, drilldown = Year)
    final = subset(final, select= -c(Year))
    final = final[order(as.numeric(final$Month)),]
    final
  })
  
  #output$plot = renderBillboarder({
  #  billboarder() %>%
  #    bb_linechart(data = dataInput(), type = "spline")
  output$plot = renderHighchart({
    canvasClickFunction2 <- JS("function(event) {Shiny.onInputChange('canvasClicked2',[ event.point.name, this.name]);}")
    hc = highchart() %>%
      hc_chart(type = "column",
               events = list(
                 load = JS("function() {console.log(this)}"),
                 drilldown = JS(
                   "function(e) {
                   console.log(e.seriesOptions); this.xAxis[0].setTitle({text: 'Month'})}"
                 ),
                 drillup = JS(
                   "function(e) {
                   console.log(e.seriesOptions); this.xAxis[0].setTitle({text: 'name'})}"
                 )
                 )) %>%
      hc_title(text = ' ') %>%
      hc_xAxis(title = list(text = 'Year')) %>%
      hc_yAxis(title = list(text = 'Sales in $R')) %>%
      hc_colors('#53917e') %>%
      hc_xAxis(type='category') %>%
      hc_legend(enabled = FALSE) %>%
      hc_plotOptions(series = list(
        boderWidth = 0,
        dataLabels = list(enabled = FALSE),
        events = list(stacking = FALSE,click = canvasClickFunction2)
      )) %>%
      hc_add_series(name = "Sales per Year",
                    data = dataInput2())
    hc %>%
      hc_drilldown(allowPointDrilldown = TRUE,
                   series = list(
                     list(
                       id = "2017",
                       name = 'Sales per Month 2017',
                       data = list_parse2(dataInput2017())
                     ),
                     list(
                       id = "2018",
                       name = 'Sales per Month 2018',
                       data = list_parse2(dataInput2018())
                     )
                   ))
  })
  output$plot2 = renderHighchart({
    canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', event.point.name);}")
    hc = highchart() %>%
      
      hc_chart(type = "column",
               events = list(
                 load = JS("function() {console.log(this)}"),
                 drilldown = JS(
                   "function(e) {
                   console.log(e.seriesOptions); this.xAxis[0].setTitle({text: 'Subcategory'})}"
                 ),
                 drillup = JS(
                   "function(e) {
                   console.log(e.seriesOptions); this.xAxis[0].setTitle({text: 'Category'})}"
                 )
               )) %>%
      hc_title(text = '') %>%
      hc_colors('#53917e') %>%
      hc_xAxis(title = list(text = 'Category')) %>%
      hc_yAxis(title = list(text = 'Sales in $R')) %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(type='category') %>%
      hc_plotOptions(series = list(
        boderWidth = 0,
        dataLabels = list(enabled = FALSE),
        events = list(stacking = FALSE,click = canvasClickFunction)
      )) %>%
      hc_add_series(name = 'Sales per Category', data = cats())
    hc %>%
      hc_drilldown(allowPointDrilldown = TRUE,
                   series = dataInput())
  })
  output$plot3 = renderHighchart({
      map88()
    })
}
shinyApp(ui = ui, server = server)
