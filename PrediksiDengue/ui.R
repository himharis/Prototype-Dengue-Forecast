library(shiny)
# library(shinylogs)

# setwd("~/Downloads/dengue phlc/PrediksiDengue")
# model <- readRDS("dengue_svr.rds")


shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "/favicon-16x16.png")
  ),
  headerPanel(
    list("Dengue Forecast"),
    windowTitle="Dengue Forecast"
  ),
  p(em("oleh Kemenkes P2PM Arbo, Malaria Consortium, PHLC")),
  sidebarLayout(
    sidebarPanel(
      textOutput("userOutput")
      # textInput("userInput", 'Isi teks:'),
      # selectInput("kolompilih", "Pilih Kolom:",
      #             choices = names(USArrests)
      # ),
      # sliderInput('binpilih', 'Pilih Angka Bin:',
      #             min=2, max=12, value=6, step=2)
      # #               ),
      # # # sliderInput('slider 1', 'Filter angka Murder',
      # #             min = 0, max = max(USArrests$Murder),
      # #             value = c(0,10)
      # #             )
    ),
    mainPanel(img(src='logo.png',  height="40%", width="40%", align = "right"), sliderInput("Tavg", "Suhu(°C):",
                  min = 20, max = 40, value = 20, step = 0.5),
      sliderInput("RH_avg", "Kelembaban Rata-Rata:",
                  min = 1, max = 100, value = 1, step = 0.5),
      sliderInput("RR", "Curah Hujan:",
                  min = 1, max = 20, value = 1, step = 0.5),
      sliderInput("ss", "Penyinaran Matahari:",
                  min = 1, max = 10, value = 1, step = 0.5),
      sliderInput("ff_avg", "Kecepatan Angin Rata-Rata:",
                  min = 1, max = 10, value = 1, step = 0.5),
      selectInput("ABJ", label = "ABJ:",
                  choices = list("Berisiko", "Tidak Berisiko")),
      #sliderInput("kasus", "Kasus:",
                  #min = 0, max = 1000, value = 1),
      submitButton("Prediksi!")
      
    )
    # plotOutput('plotOutput')
    #tableOutput("table1")
  )
)
)
# use_tracking()

