    library(shiny)
    library(shinydashboard)
    library(shinythemes)
    library(leaflet)
    library(leaflet.extras)
    library(RColorBrewer)

    library(openair)
    library(openairmaps)
    library(vctrs)
    library(tidyverse)

    library(ggplot2)
    library(lubridate)
    library(worldmet)

    library(owmr) # OpenWeatherMap package
    library(seacarb)

    library(fpp2)
    library(xts)
    library(lubridate)
    library(forecast)

    library(Rcapture)
    library(secr)

    library(worldmet)

    # Define user interface
    ui <- dashboardPage(

        # Define dashboard header
        dashboardHeader(),

        # Define dashboard sidebar
        dashboardSidebar(

            # Define sidebar menu
            sidebarMenu(

                # Define menu items
                menuItem(
                    "Common Maps",
                    tabName = "maps",
                    icon = icon("globe"),
                    menuSubItem("Heat Map",
                        tabName = "dashboard",
                        icon = icon("dashboard")
                    ),
                    menuSubItem("Earthquakes Map",
                        tabName = "earthq_map",
                        icon = icon("map")
                    )
                ),
                menuItem("Network Data UK",
                    tabName = "widgets",
                    icon = icon("chart-pie")
                ),
                menuItem("Pollution Map",
                    tabName = "pollution_data",
                    icon = icon("th")
                ),
                menuItem("Weather Map",
                    tabName = "owmr_weather",
                    icon = icon("sun")
                ),
                menuItem("Weather Forecast",
                    tabName = "owmr_forecast",
                    icon = icon("cloud-sun")
                ),
                menuItem("Seacarb Plot",
                    tabName = "seacarb_data",
                    icon = icon("chart-bar")
                ),
                menuItem("Seacarb Plot 2",
                    tabName = "seacarb_data2",
                    icon = icon("chart-bar")
                ),
                menuItem("ARIMA Plot",
                    tabName = "arima_data",
                    icon = icon("chart-area")
                ),
                menuItem("Raw Plot",
                    tabName = "hima_plot",
                    icon = icon("th")
                ),
                menuItem("Secr Plot",
                    tabName = "secr_plot",
                    icon = icon("chart-pie")
                ),
                menuItem("FAQ Page",
                    tabName = "faqpage",
                    icon = icon("question"))
            )
        ),

        # Define dashboard body
        dashboardBody(

            # Define tab items
            tabItems(

                # Define tab item for default map
                tabItem(
                    tabName = "dashboard",
                    leafletOutput("default_map")
                ),

                # Define tab item for earthquakes map
                tabItem(
                    tabName = "earthq_map",
                    tags$style(
                        type = "text/css",
                        "#earthquakes {height: calc(100vh - 200px) !important;}"
                    ),
                    leafletOutput("earthquakes_map")
                ),

                # Define tab item for network map
                tabItem(
                    tabName = "widgets",
                    leafletOutput("network_map")
                ),

                # Define tab item for pollution map
                tabItem(
                    tabName = "pollution_data",
                    leafletOutput("pollution_plot")
                ),

                # Define tab item for weather map
                tabItem(
                    tabName = "owmr_weather",
                    leafletOutput("owmr_map")
                ),

                # Define tab item for weather forecast
                tabItem(
                    tabName = "owmr_forecast",
                    leafletOutput("owmr_plot")
                ),

                # Define tab item for seacarb plot
                tabItem(
                    tabName = "seacarb_data",
                    plotOutput("seacarb_plot")
                ),

                # Define tab item for seacarb plot 2
                tabItem(
                    tabName = "seacarb_data2",
                    plotOutput("seacarb_plot2")
                ),

                # Define tab item for ARIMA plot
                tabItem(
                    tabName = "arima_data",
                    plotOutput("arima_plot")
                ),

                # Define tab item for raw plot
                tabItem(
                    tabName = "hima_plot",
                    plotOutput("hima_plot")
                ),

                # Define tab item for secr plot
                tabItem(
                    tabName = "secr_plot",
                    plotOutput("secr_plot")
                ),
                tabItem(
                    tabName = "faqpage",
                    textOutput("faqpage"))
            )
        ),
        title = "Hello!",
        skin = c()
    )

    server <- function(input, output, session) {
        # Load necessary libraries
        library(shiny)
        library(leaflet)
        library(leaflet.extras)
        library(openair)
        library(openairmaps)
        library(vctrs)
        library(tidyverse)
        library(ggplot2)
        library(lubridate)
        library(worldmet)
        library(owmr) # OpenWeatherMap package
        library(seacarb)
        library(fpp2)
        library(xts)
        library(lubridate)
        library(forecast)
        library(Rcapture)
        library(secr)
        library(COVID19)
        library(worldmet)

        Sys.setenv("WORLD_WEATHER_ONLINE_KEY" = "8262e3dd60684520b30180657233005")

        # Set the API key for the weather map
        owmr_settings("9030bc31f41f8fdc2196b38c4757fec2")
        data(quakes)

        # Render the default map
        output$default_map <- renderLeaflet({
            trajMap(traj_data, colour = "nox")
        })

        output$earthquakes_map <- renderLeaflet({
            pal <- colorNumeric("Blues", quakes$mag)

            leaflet(data = quakes) %>%
                addProviderTiles(providers$Esri.WorldStreetMap,
                    options = tileOptions(minZoom = 0, maxZoom = 7),
                    group = "Esri.WorldStreetMap"
                ) %>%
                addProviderTiles(providers$Esri.WorldImagery,
                    options = tileOptions(minZoom = 7, maxZoom = 13),
                    group = "Esri.WorldImagery"
                ) %>%
                addTiles(group = "OpenStreetMap") %>%
                addCircles(
                    radius = ~ 10^mag / 10, weight = 1, color = ~ pal(mag),
                    fillColor = ~ pal(mag), fillOpacity = 0.6,
                    popup = ~ as.character(mag), label = ~ as.character(mag),
                    group = "Points"
                ) %>%
                addMarkers(
                    lng = ~long, lat = ~lat,
                    popup = ~ as.character(mag),
                    label = ~ as.character(mag),
                    group = "Markers"
                ) %>%
                addLayersControl(
                    baseGroups = c("OpenStreetMap", "Esri.WorldStreetMap", "Esri.WorldImagery"),
                    overlayGroups = c("Markers", "Points"),
                    options = layersControlOptions(collapsed = TRUE)
                )
        })



        # Render the network map
        output$network_map <- renderLeaflet({
            networkMap(
                source = c("waqn", "aqe", "ni", "local"),
                control = c("network"),
                provider = c("OpenStreetMap"),
                collapse.control = TRUE
            )
        })

        # Render the pollution plot
        output$pollution_plot <- renderLeaflet({
            networkMap(
                source = c("europe"),
                control = c("network"),
                provider = c("OpenStreetMap"),
                collapse.control = TRUE
            )
        })

        # Render the weather map
        output$owmr_map <- renderLeaflet({
            owm_data <- find_city(city = "Krasnoyarsk", units = "metric") %>%
                owmr_as_tibble()
            map <- leaflet() %>%
                addProviderTiles(providers$CartoDB.DarkMatter,
                    options = tileOptions(
                        minZoom = 0,
                        maxZoom = 13
                    )
                ) %>%
                add_weather(
                    owm_data,
                    template = "<b>{{name}}</b>, {{temp}}°C",
                    icon = owm_data$weather_icon
                )
        })

        # Render the weather forecast plot
        output$owmr_plot <- renderLeaflet({
            # get forecast
            # 9 day forecast
            result <- get_forecast_daily("London", cnt = 9) %>%
                owmr_as_tibble()
            map <- leaflet() %>%
                addProviderTiles(providers$CartoDB.DarkMatter,
                    options = tileOptions(
                        minZoom = 0,
                        maxZoom = 13
                    )
                )
            forecast_frame <- result$list
        })

        # Render the seacarb plot
        output$seacarb_plot <- renderPlot({
            ## Plot the bjerrum plot for the carbonate system using the default values
            bjerrum(K1(), K2(), 
                main = "DIC speciation", 
                lwd = 2)
            abline(v = -log10(K1()), col = "grey")
            mtext(side = 3, 
                at = -log10(K1()), 
                "pK1")
            abline(v = -log10(K2()), col = "grey")
            mtext(side = 3, 
                at = -log10(K2()), 
                "pK2")
            legend("left", 
                lty = 1:3, 
                lwd = 2, 
                legend = c(
                expression(CO[2]), 
                expression(HCO[3]^"-"),
                expression(CO[3]^"2-")
            ))
        })

        # Render the seacarb plot2
        output$seacarb_plot2 <- renderPlot({
            bjerrum(K1p(), K2p(), K3p(), main = "phosphate speciation", lwd = 2)
            legend("left", lty = 1:4, lwd = 2, legend = c(
                expression(H[3] ~ PO[4]),
                expression(H[2] ~ PO[4]^"-"),
                expression(HPO[4]^"2-"), expression(PO[4]^"3-")
            ))
        })

        # Read in the raw data
        raw_data <- read.csv("ardl_data.csv")

        # Render the ARIMA plot
        output$arima_plot <- renderPlot({
            data <- data.frame(raw_data[, c("Year", "CO2", "GDP", "ENC")])
            data$lnCO2 <- log(data$CO2)
            data$lnGDP <- log(data$GDP)
            data$lnENC <- log(data$ENC)
            data_corr <- data[, c("lnCO2", "lnGDP", "lnENC")]
            corr_mat <- cor(data_corr)
            corr_mat
            plot(data$Year, data$lnCO2, main = "The CO2 consumption over the years")
        })

        # Render the raw plot
        output$hima_plot <- renderPlot({
            waqn_data <- importWAQN(
                year = 2023,
                pollutant = "all"
            )

            polarPlot(waqn_data,
                pollutant = c(
                    "nox",
                    "no2",
                    "so2"
                ),
                statistic = "cpf",
                percentile = 90,
                cols = "YlGnBu"
            )
            # polarPlots by year on same scale
            # polarPlot(waqn_data, pollutant = "so2", type = "year", main = "polarPlot of so2")
        })

        # Render the secr plot
        output$secr_plot <- renderPlot({
            detectors <- make.grid(
                nx = 10, ny = 10, spacing = 20,
                detector = "multi"
            )
            plot(detectors, label = TRUE, border = 0, gridspace = 20)
            detections <- sim.capthist(detectors,
                noccasions = 5,
                popn = list(D = 5, buffer = 100),
                detectpar = list(g0 = 0.2, sigma = 25)
            )
            session(detections) <- "Simulated data"
            plot(detections, border = 20, tracks = TRUE, varycol = TRUE)
            ## generate habitat mask
            mask <- make.mask(detectors, buffer = 100, nx = 48)
            ## fit model and display results
            secr.model <- secr.fit(detections, model = g0 ~ b, mask = mask)
            secr.model
        })

        output$faqpage <- renderText({
            "faqpagefaqpagefaqpagefaqpagefaqpagefaqpagefaqpage"
        })
    }

    shinyApp(ui, server)