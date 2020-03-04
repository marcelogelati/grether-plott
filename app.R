library(shiny) 
library(ggraph) ## Para plotar os gráficos
library(igraph) ## Idem
library(tidyverse) ## Estou usando, pelo menos, o ggplot2
# library(googlesheets) ## Para exportar os resultados

## Criando vetores de payoff
payoff1 <- c(-1, 4)
payoff2 <- c(-1.5, 16)
payoff3 <- c(-0.5, 9)
payoff4 <- c(-1, 2)

## Criando vetores de probabilidade
prob1 <- c("1/36", "35/36")
prob2 <- c("25/36", "11/36")
prob3 <- c("29/36", "7/36")
prob4 <- c("7/36", "29/36")

## Criando valores do gráfico
edges1 <- data.frame(from = "origin", to = payoff1)
edges2 <- data.frame(from = "origin", to = payoff2)
edges3 <- data.frame(from = "origin", to = payoff3)
edges4 <- data.frame(from = "origin", to = payoff4)
edges_loteriacerta <- data.frame(from = "origin", to = "---")

## Criando objeto do tipo gráfico
mygraph1 <- graph_from_data_frame(edges1)
mygraph2 <- graph_from_data_frame(edges2)
mygraph3 <- graph_from_data_frame(edges3)
mygraph4 <- graph_from_data_frame(edges4)
mygraphcerto <- graph_from_data_frame(edges_loteriacerta)

## Criando árvores
lot1 <- ggraph(mygraph1, layout = 'dendrogram', circular = FALSE) + 
    geom_edge_link(aes(label = prob1), hjust = 1.8) +
    geom_node_point() +
    geom_node_text(aes(label = name, filter = leaf), hjust = 0.3, vjust = 2) + 
    ylim(-0.1, NA) + 
    ggtitle("Loteria A") +
    theme_void() + coord_flip() + scale_y_reverse()

lot2 <- ggraph(mygraph2, layout = 'dendrogram', circular = FALSE) + 
    geom_edge_link(aes(label = prob2), hjust = 1.8) +
    geom_node_point() +
    geom_node_text(aes(label = name, filter = leaf), hjust = 0.3, vjust = 2) + 
    ylim(-0.1, NA) + 
    ggtitle("Loteria B") +
    theme_void() + coord_flip() + scale_y_reverse()

lot3 <- ggraph(mygraph3, layout = 'dendrogram', circular = FALSE) + 
    geom_edge_link(aes(label = prob3), hjust = 1.8) +
    geom_node_point() +
    geom_node_text(aes(label = name, filter = leaf), hjust = 0.3, vjust = 2) + 
    ylim(-0.1, NA) + 
    ggtitle("Loteria C") +
    theme_void() + coord_flip() + scale_y_reverse()

lot4 <- ggraph(mygraph4, layout = 'dendrogram', circular = FALSE) + 
    geom_edge_link(aes(label = prob4), hjust = 1.8) +
    geom_node_point() +
    geom_node_text(aes(label = name, filter = leaf), hjust = 0.3, vjust = 2) + 
    ylim(-0.1, NA) + 
    ggtitle("Loteria D") +
    theme_void() + coord_flip() + scale_y_reverse()

ec <- ggraph(mygraphcerto, layout = 'dendrogram', circular = FALSE) + 
    geom_edge_link(aes(label = 1), hjust = 1.8, vjust = -1) +
    geom_node_point() +
    geom_node_text(aes(label = name, filter = leaf), hjust = 0.3, vjust = 2) + 
    ylim(-0.1, NA) + 
    theme_void() + coord_flip() + scale_y_reverse()

## Criando caixas modais para o app
modal_confirm <- modalDialog(
    "Suas respostas foram enviadas!",
    title = "Confirmação",
    footer = list(
        actionButton("ok", "Ok")
    )
)

## Começando user interface
ui <- fluidPage(
    navbarPage("Navegação",
               
               ## Página de introdução
               tabPanel("Introdução",
                        titlePanel(title = "", windowTitle = "Grether-Plott"),
                        h1("Comparação de loterias", align = "center"),
                        textInput("user", "Digite seu nome")
               ),
               
               ## Primeira comparação de loterias
               tabPanel("Pergunta 1",
                        fluidRow(
                            column(6, plotOutput("loteria1")),
                            column(6, plotOutput("loteria2"))
                        ), 
                        radioButtons("escolha1", "Qual loteria você prefere?", c("A", "B"))
               ),
               
               ## Segunda comparação de loterias
               tabPanel("Pergunta 2",
                        fluidRow(
                            column(6, plotOutput("loteria3")),
                            column(6, plotOutput("loteria4"))
                        ),
                        radioButtons("escolha2", "Qual loteria você prefere?", c("C", "D"))
               ),
               
               ## Primeira comparação com equivalente certo
               tabPanel("Pergunta 3",
                        fluidRow(
                            column(6, plotOutput("lottery1")),
                            column(6, plotOutput("loteriacerta1"))
                        ),
                        numericInput("ec1", "O equivalente certo (ec_A)", value = 0, step = 1)
               ),
               
               ## Segunda comparação com equivalente certo
               tabPanel("Pergunta 4",
                        fluidRow(
                            column(6, plotOutput("lottery2")),
                            column(6, plotOutput("loteriacerta2"))
                        ),
                        numericInput("ec2", "O equivalente certo (ec_B)", value = 0, step = 1)
               ),
               
               ## Terceira comparação com equivalente certo
               tabPanel("Pergunta 5",
                        fluidRow(
                            column(6, plotOutput("lottery3")),
                            column(6, plotOutput("loteriacerta3"))
                        ),
                        numericInput("ec3", "O equivalente certo (ec_C)", value = 0, step = 1)
               ),
               
               ## Quarta comparação com equivalente certo
               tabPanel("Pergunta 6",
                        fluidRow(
                            column(6, plotOutput("lottery4")),
                            column(6, plotOutput("loteriacerta4"))
                        ),
                        numericInput("ec4", "O equivalente certo (ec_D)", value = 0, step = 1)
               ),
               
               ## Você é racional?
               tabPanel("Resultados",
                        textOutput("racional1"),
                        textOutput("racional2"),
                        actionButton("submit", "Submeta suas respostas"))
    )
)

## Abrindo server
server <- function(input, output, session) {
    
    ## Output da primeira comparação de loterias
    output$loteria1 <- renderPlot(lot1)
    output$loteria2 <- renderPlot(lot2)
    
    ## Output da segunda comparação de loterias
    output$loteria3 <- renderPlot(lot3)
    output$loteria4 <- renderPlot(lot4)
    
    ## Output da primeira comparação com equivalente certo
    output$lottery1 <- renderPlot(lot1)
    output$loteriacerta1 <- renderPlot(ec)
    
    ## Output da segunda comparação com equivalente certo
    output$lottery2 <- renderPlot(lot2)
    output$loteriacerta2 <- renderPlot(ec)
    
    ## Output da terceira comparação com equivalente certo
    output$lottery3 <- renderPlot(lot3)
    output$loteriacerta3 <- renderPlot(ec)
    
    ## Output da quarta comparação com equivalente certo
    output$lottery4 <- renderPlot(lot4)
    output$loteriacerta4 <- renderPlot(ec)
    
    ## Calculando se você é racional
    output$racional1 <- renderText({
        if (input$escolha1 == "A") {
            A <- "Você prefere a loteria A em relação à loteria B"
        } else {
            B <- "Você prefere a loteria B em relação à loteria A"
        }
        
        if (input$ec1 > input$ec2) {
            ec_A <- paste("e o equivalente certo de A ec_A =", input$ec1, "é maior que o 
            equivalente certo de B ec_B =", input$ec2, sep = " ")
        } else {
            ec_B <- paste("e o equivalente certo de B ec_B =", input$ec2, "é maior que o 
            equivalente certo de A ec_A =", input$ec1, sep = " ")
        }
        
        if (input$escolha1 == "A" & input$ec1 > input$ec2) {
            paste(A, ec_A, "logo você é racional", sep = " ")
        } else if (input$escolha1 == "A" & input$ec2 > input$ec1) {
            paste(A, ec_B, "logo você é irracional", sep = " ") 
        } else if (input$escolha1 == "B" & input$ec2 > input$ec1) {
            paste(B, ec_B, "logo você é racional", sep = " ")
        } else {
            paste(B, ec_A, "logo você é irracional")
        }
    })
    
    output$racional2 <- renderText({
        if (input$escolha2 == "C") {
            C <- "Você prefere a loteria C em relação à loteria D"
        } else {
            D <- "Você prefere a loteria D em relação à loteria C"
        }
        
        if (input$ec3 > input$ec4) {
            ec_C <- paste("e o equivalente certo de C ec_C =", input$ec3, "é maior que o 
            equivalente certo de D ec_D =", input$ec4, sep = " ")
        } else {
            ec_D <- paste("e o equivalente certo de D ec_D =", input$ec4, "é maior que o 
            equivalente certo de C ec_C =", input$ec3, sep = " ")
        }
        
        if (input$escolha2 == "C" & input$ec3 > input$ec4) {
            paste(C, ec_C, "logo você é racional", sep = " ")
        } else if (input$escolha2 == "C" & input$ec4 > input$ec3) {
            paste(C, ec_D, "logo você é irracional", sep = " ") 
        } else if (input$escolha2 == "D" & input$ec4 > input$ec3) {
            paste(D, ec_D, "logo você é racional", sep = " ")
        } else {
            paste(D, ec_C, "logo você é irracional")
        }
    })
    
    ## Mandando pro Google Sheets  
    observeEvent(input$submit, showModal(modal_confirm))
    observeEvent(input$ok, removeModal())
}
## Rodando app
shinyApp(ui, server)