library(shiny)
library(corrgram)
library(plot3D)

graphics = c("Barras","Pizza","Linha", "Dispersão", "3D")
ui <- fluidPage(
    
    tags$head(HTML("<title>Gráficos Simples</title>")),
    titlePanel(h2("Gerador de Gráficos Simples", align = "center")),
    a(href="https://github.com/danttis", "Meu GitHub"),
    br(a(href="https://github.com/danttis/chart-generator/blob/main/csv_teste.csv", "Tabela para testes")),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file","Selecione seu arquivo", multiple = FALSE), 
            helpText("Tabelas muito grandes não serão lidas"),
            uiOutput("selectOpition")
        ),
        mainPanel(column(12,
                         uiOutput("tb")
                         
        ))
        
    )
)



server <- function(input, output) {
    
    output$table <- renderTable({ 
        if(is.null(input$file)){return()}
        read.table(file=input$file$datapath, sep=input$sep, header = TRUE , stringsAsFactors = input$stringAsFactors)
    })
    
    output$selectOpition <- renderUI({
        if(is.null(input$file)) {return()}
        list(hr(), 
             radioButtons(inputId = 'sep', label = 'Qual separador do seu dataset?', choices = c(Virgula=',',Ponto_e_Virgula=';',Tab='\t', Espaço=''), selected = ','),
             checkboxInput(inputId = "stringAsFactors", "Importar as strings como Fator?", FALSE),
             textInput('vector', 'Entre com as colunas que deseja plotar (separe por vírgulas se forem mais de uma):', "1,2"),
             selectInput("Select", "Tipo de gráfico", choices=graphics)
        )
        
    })
    
    output$plotg <- renderPlot({
        if(is.null(input$file)){return()}
        data <- read.table(file=input$file$datapath, sep=input$sep, header = TRUE , stringsAsFactors = input$stringAsFactors)
        l <- read.table(file=input$file$datapath, sep=input$sep, header = FALSE , stringsAsFactors = input$stringAsFactors)
        labelsc <- l[1,]
        x <- as.numeric(unlist(strsplit(input$vector,",")))
        if(input$Select==graphics[1]){
            if(length(x)>1){
                barplot(c(data[,x[1]],data[,x[2]]),xlab=labelsc[x[1]],ylab=labelsc[x[2]])}
            else{
                barplot(data[,x[1]], xlab=labelsc[x[1]])}
        }
        else if(input$Select==graphics[2]){
            pie(data[,x[1]])}
        else if(input$Select==graphics[3]){
            if(length(x)>1){
                plot(data[,x[1]],data[,x[2]], type = "o", col = "blue", xlab=labelsc[x[1]],ylab=labelsc[x[2]])}
            else{
                plot(data[,x[1]], type = "o", col = "blue", xlab=labelsc[x[1]], ylab=" ")
            }
        }
        else if(input$Select==graphics[4]){
            if(length(x)>1){
                plot(data[,x[1]],data[,x[2]],  xlab=labelsc[x[1]],ylab=labelsc[x[2]])}
            else{
                plot(data[,x[1]],  xlab=labelsc[x[1]], ylab=" ")
            }
        }
        
        else if(input$Select==graphics[5]){
            scatter3D(data[,x[1]], data[,x[2]], data[,x[3]], bty = "g", pch = 18, col = gg.col(100))
        }
    }
    )
    
    output$summ <- renderPrint({
        if(is.null(input$file)){return()}
        x <- as.numeric(unlist(strsplit(input$vector,",")))
        data <- read.table(file=input$file$datapath, sep=input$sep, header = TRUE , stringsAsFactors = input$stringAsFactors)
        summary(data[,x])
    })
    
    output$tb <- renderUI({
        if(is.null(input$file)) {return()}
        else{
            tabsetPanel(
                tabPanel("Tabela:", tableOutput("table")),
                tabPanel("Gráfico:", plotOutput("plotg")),
                tabPanel("Sumário:", verbatimTextOutput("summ"))
                
            )
        }
    })
}

shinyApp(ui = ui, server = server)
