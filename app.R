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
                   numericInput("x", "Enter the air density", value = 1.225),
                   numericInput("A", "Enter the swept area of the turbines blades", value = 2826),
                   numericInput("v", "Enter the wind speed", value = 20)
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
                                   numericInput("Isc", "Enter the cell's short circuit current at a 25^0 C and 1  kW⁄m^2", 9.06),
                                   numericInput("Voc", "Enter the cell open current voltage", 20),
                                   numericInput("Fif", "Enter the cell idealising factor",1.3),
                                   numericInput("Tc", "Enter the cell' s absolute temperature",293.15)
                               ),
                               mainPanel(
                                   p(h3("The cell' s reverse saturation current at a solar radiation & reference temperature: Ioa is :", strong(textOutput("Ioa")))),
                               )
                           )
                       ),
                    tabPanel(
                        h5("The dark saturation current"),
                        sidebarLayout(
                            sidebarPanel(
                                numericInput("Tr", "Enter the cell' s reference temperature", 298),
                                numericInput("Vg", "Enter the band-gap energy of the semiconductor used in the cell", 1.1)
                                
                            ),
                            mainPanel(
                            p(h3("The dark saturation current Io is :", strong( textOutput("Io")))),
                           
                        )
                    )
                 ),
                 tabPanel(
                     h5("The light generated current (photocurrent)"),
                     sidebarLayout(
                         sidebarPanel(
                             numericInput("Msc", "Enter the temperature coefficient of the cell's short circuit", 0.00058),
                             numericInput("G", "Enter the solar irradiation in  kW⁄m^2 ", 1000)
                         ),
                         mainPanel(
                             p(h3("The light generated current (photocurrent) :", strong(textOutput("Igc"))))
                         )
                     )
                 ),
                 tabPanel(
                     h5("The photovoltaic module"),
                     sidebarLayout(
                         sidebarPanel(
                             numericInput("Vd", "Enter the band-gap energy of the semiconductor used in the cell", 1),
                             numericInput("Rp", "Enter the parallel resistance value", 1)
                         ),
                         mainPanel(
                             p(h3("The photovoltaic module Ipv is:", strong(textOutput("IPV"))))
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
        (input$Isc/exp((input$Voc * 1.6 * 10^(-19))/(input$Fif * input$Tc * 1.38 * 10 ^ (-23)))) * ((input$Tc/input$Tr)^3) * exp((((1/input$Tr) - (1/input$Tc)) * (1.6 * 10^(-19) * input$Vg)/(1.38 * 10 ^ (-23) * input$Fif)))
    })
    output$Igc <- renderText({
        (input$Msc * (input$Tc - input$Tr) + input$Isc) * input$G
    })
    output$IPV <- renderText({
        ((input$Msc * (input$Tc - input$Tr) + input$Isc) * input$G) - (((input$Isc/exp((input$Voc * 1.6 * 10^(-19))/(input$Fif * input$Tc * 1.38 * 10 ^ (-23)))) * ((input$Tc/input$Tr)^3) * exp((((1/input$Tr) - (1/input$Tc)) * (1.6 * 10^(-19) * input$Vg)/(1.38 * 10 ^ (-23) * input$Fif)))) * (exp((1.6 * 10^(-19) * input$Voc)/(1.38 * 10 ^(-23) * input$Fif * input$Tc)) - 1)) - (input$Vd/input$Rp)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
