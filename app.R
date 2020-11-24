library(shiny)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Define UI for application that draws a histogram

ui <- fluidPage(
    theme = shinythemes::shinytheme("cosmo"),
    # Application title
    h1(strong("Smart grid module"), "by Simon Pierre", align = 'center'),
    #inputs for Hydropower module
    img(src = 'logo.gif', align = 'center'),
    #image of Wascal
    tabsetPanel(
       tabPanel( h4("The hydropower module"),
        sidebarLayout(
            sidebarPanel(
            numericInput("n", "Enter the dimensionless efficiency of the turbine", value = 0.85),
            numericInput("p", "Enter the density of water in kilograms per cubic meter", value = 998),
            numericInput("Q", "Enter the flow in cubic meters per seconds", value = 79.3),
            numericInput("g", "Enter the acceleration due to gravity", value = 9.80),
            numericInput("h", "Enter the height difference between inlet and outlet", value = 146.3)
            ),
            mainPanel(
                p(h3('The power of your hydromodule is', em('(in MegaWatts):'), strong(textOutput("P"))))
            )
        )
        ),
       tabPanel(
           h4("The Wind turbine module"),
           sidebarLayout(
               sidebarPanel(
                   numericInput("x", "Enter the air density", value = 1),
                   numericInput("A", "Enter the swept area of the turbines blades", value = 1),
                   numericInput("v", "Enter the wind speed", value = 1)
               ),
               mainPanel(
                   p(h3('The power of your wind turbine module is', em('(in MegaWatts):'), strong(textOutput("Pw"))))
               )
           )
       ),
       tabPanel(
           h4("The photovoltaic module"),
                   tabsetPanel(
                       tabPanel(
                           h5("The Reverse saturation content"),
                           sidebarLayout(
                               sidebarPanel(
                                   numericInput("Isc", "Enter the cell's short circuit current at a 25^0 C and 1  kW⁄m^2", 9.31),
                                   numericInput("Voc", "Enter the cell open current voltage", 36.6),
                                   numericInput("Fif", "Enter the cell idealising factor",1,5),
                                   numericInput("Tc", "Enter the cell' s absolute temperature",51.27)
                               ),
                               mainPanel(
                                   p(h3("The cell' s reverse saturation current at a solar radiation & reference temperature: Ioa is :")),
                                   textOutput("Ioa")
                               )
                           )
                       ),
                    tabPanel(
                        h5("The dark saturation current"),
                        sidebarLayout(
                            sidebarPanel(
                                numericInput("Ioad", "Enter the The dark saturation current (Ioa) value", 1),
                                numericInput("Tr", "Enter the cell' s reference temperature", 1),
                                numericInput("Vg", "Enter the band-gap energy of the semiconductor used in the cell", 1)
                                
                            ),
                            mainPanel(
                            p(h3("The dark saturation current Io is :")),
                            textOutput("Io")
                        )
                    )
                 ),
                 tabPanel(
                     h5("The light generated current (photocurrent)"),
                     sidebarLayout(
                         sidebarPanel(
                             numericInput("Msc", "Enter the temperature coefficient of the cell's short circuit", 1),
                             numericInput("G", "Enter the solar irradiation in  kW⁄m^2 ", 1)
                         ),
                         mainPanel(
                             p(h3("The light generated current (photocurrent) :")),
                             textOutput("Igc")
                         )
                     )
                 ),
                 tabPanel(
                     h5("The photovoltaic module"),
                     sidebarLayout(
                         sidebarPanel(
                             numericInput("Iod", "Enter dark saturation current (Io) value", 1),
                             numericInput("Igcd", "Enter The light generated current (photocurrent) value (Igc) ", 1),
                             numericInput("Msc", "Enter the temperature coefficient of the cell's short circuit", 1),
                             numericInput("G", "Enter the solar irradiation in  kW⁄m^2 ", 1),
                             numericInput("Rp", "Enter the parallel resistance value", 1)
                         ),
                         mainPanel(
                             p(h3("The photovoltaic module Ipv is:")),
                             textOutput("Ipv")
                         )
                     )
                 )
       )
    )
)
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$P <- renderText({
        (input$n*input$p*input$Q*input$g*input$h)/1000000
    })
    output$Pw <- renderText({
        ((input$x * input$A * (input$v)^3)/2)/1000000
    })
    output$Ioa <- renderText({
         input$Isc/exp((input$Voc * 1.6 * 10^(-19))/(input$Fif * input$Tc * 1.38 * 10 ^ (-23)))
    })
    output$Io <- renderText({
        input$Ioad * ((input$Tc/input$Tr)^3) * exp(((1/input$Tr - input$Tc) * (1.6 * 10^(-19) * input$Vg)/(1.38 * 10 ^ (-23) * input$Fif)))
    })
    output$Igc <- renderText({
        (input$Msc * (input$Tc - input$Tr) + input$Isc) * input$G
    })
    output$Ipv <- renderText({
        input$Igcd - input$Iod * (exp((1.6 * 10^(-19) * input$Voc)/(1.38 * 10 ^ (-23) * input$Fif * input$Tc)) - 1) - input$Vd/input$Rp
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
