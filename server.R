
# Define server logic required to draw a histogram
server <- function(input, output) {
    ################### INPUT ####################
    select_state <- eventReactive(input$go, {
        
        state_name <- input$state
        twin <- input$true_date

        df_state <- master_df %>% 
            filter(X >= as.POSIXct(twin[1]) & X <= as.POSIXct(twin[2]))
        
        return(df_state)
    })
    
    select_state_comp <- eventReactive(input$go_comp, {
        
        twin_comp <- input$true_date_comp

        df_state_comp <- master_df %>% 
            filter(X >= as.POSIXct(twin_comp[1]) & X <= as.POSIXct(twin_comp[2]))
        
        return(df_state_comp)
    })
    
    output$timedate <- renderUI({
        
        state_name <- input$state
        
        df <- master_df 

        min_time <- min(df$X)
        max_time <- max(df$X)
        dateRangeInput("true_date", "Período de análise",
                       end = max_time,
                       start = min_time,
                       min  = min_time,
                       max  = max_time,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language='pt-BR')
    })
    
    output$timedate_comp <- renderUI({
        
        state_name <- input$state_comp
        
        df <- master_df## %>% 
        
        min_time <- min(df$X)
        max_time <- max(df$X)
        
        
        dateRangeInput("true_date_comp", "Período de análise",
                       end = max_time,
                       start = min_time,
                       min    = min_time,
                       max    = max_time,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language='pt-BR')
    })
    
    ################ OUTPUT #####################
    #Tabela 1ª aba
    Info_DataTable <- eventReactive(input$go,{
        df <- select_state()
        state_name <- input$state
        
        mean <- df %>% select(state_name) %>% colMeans()
        Moda <- as.numeric(names(table(df[, state_name]))[which.max(table(df[, state_name]))])
        Desvio_Padrao <- sd(df[, state_name])
        Mediana <- median(df[, state_name])
        Maximo <- max(df[, state_name])
        Minimo <- min(df[, state_name])

        Media <- mean[[1]]
        
        Estado <- input$state
        
        df_tb <-  data.frame(Estado, Media, Moda, Mediana, Desvio_Padrao, Maximo, Minimo)
        
        df_tb <- as.data.frame(t(df_tb))
        names(df_tb)[1] <- "Informações"
        row.names(df_tb)[5] = "Desvio Padrão"
        row.names(df_tb)[6] = "Ponto Máximo"
        row.names(df_tb)[7] = "Ponto Mínimo"
        
        return(df_tb)
    })
    #Tabela 1ª aba
    output$info <- renderDT({
        Info_DataTable() %>%
            as.data.frame() %>% 
            DT::datatable(options=list(
                language=list(
                    url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                ),
                pageLength = 5
            ))
    })
    #Grafico em linha 1ª aba
    output$sh <- renderPlot({
        df <- select_state()
        
        state_name <- input$state
        
        aux <- df[, state_name] %>% na.omit() %>% as.numeric()
        aux1 <- min(aux)
        aux2 <- max(aux)
        
        df$X <- ymd(df$X)

        a <- df %>% 
            ggplot(aes(X, df[, state_name], group=1)) +
            geom_path(color="#b3427e", alpha=0.9) +
            ylab('Eletricidade consumida em MU(Mega Units)') +
            xlab('Datas')+
            coord_cartesian(ylim = c(aux1, aux2)) +
            theme_bw() +
            scale_x_date(date_labels = "%Y-%m-%d")+
            ggtitle(state_name)
        
        a
        
    })
    #Histograma 1ª aba
    output$histo <- renderPlot({
        df <- select_state()
        state_name <- input$state
        tam <-length(df[, state_name])
        titulo <- paste("Intervalo de tamanho: ",
                              as.integer(sqrt(tam)) )
        b <- df %>% #69b3a2
            ggplot(aes(df[, state_name])) +
            geom_histogram( binwidth= sqrt(tam), fill="#b3427e", color="#e9ecef", alpha=0.9) +
            ggtitle(paste0(titulo, "\n", state_name)) +
            xlab('Energia em MU')+ ylab('Frequência') +
            theme_ipsum() +
            theme(
                plot.title = element_text(face = "bold", size = (15))
            )
        b
    })
    #Boxplot 1ª aba
    output$box <- renderPlot({
        df <- select_state()
        state_name <- input$state
        
        c <- df %>% 
            ggplot(aes(x = cut(X, breaks="quarter"), y = df[, state_name])) +    
            geom_boxplot() + 
            labs(x = "Datas do começo do quartil", y = 'Energia em MU') +
            ggtitle(state_name) +
            theme_bw()
            
        c    
    })
    #Grafico em linha 2ª aba
    output$sh_comp <- renderPlot({
        df <- select_state_comp()
        
        Estados <- input$state_comp[1]
        Estados2 <- input$state_comp[2]
        
        
        df$X <- ymd(df$X)
        
       
        cores <- c("#82b342", viridis::plasma(n = 6))
        

        p <- df %>% ggplot(aes(X) ) + 
            geom_line(aes(y =  df[, Estados],   colour= Estados)) +
            geom_line(aes(y =  df[, Estados2], colour = Estados2)) +
            theme_ipsum() + 
            ylab('Eletricidade consumida em MU(Mega Units)') +
            xlab('Datas') +
            scale_x_date(date_labels = "%Y-%m-%d") +
            scale_color_manual(values = c("#82b342", "#7142b3"))
            
        p
        
        
    })
    #Grafico em barra 2ª aba
    output$bar <- renderPlot({
        df <- select_state_comp()
        state_name1 <- input$state_comp[1]
        state_name2 <- input$state_comp[2]
        df$X <- ymd(df$X)
        
        Media1 <- mean(df[, state_name1])
        Media2 <- mean(df[, state_name2])
        
        df_m <- data.frame(
            valor <- c(Media1, Media2),
            nomes <- c(state_name1, state_name2)
        )
        Medias <- as.factor(valor)
        p <- df_m %>%
            ggplot(aes(x = nomes ,y = valor, fill= Medias )) +
            geom_bar(stat = 'identity')+
            ylab('Média do gasto de enrgia em MU')+
            xlab('Estados')+
            theme_ipsum() +
            scale_fill_manual(values = c("#82b342", "#b3427e") )
        
        p
    })
    #Scatterplot 2ª aba
    output$scatter <- renderPlot({
        df <- select_state_comp()
        state_name1 <- input$state_comp[1]
        state_name2 <- input$state_comp[2]
        
        p <- df %>%
            ggplot(aes(x= df[, state_name1], y=df[, state_name2])) +
            ylab(state_name2)+
            xlab(state_name1)+
            theme_ipsum() +
            geom_point()
        p
        
    })
    
    #Tabela 2ª aba
    Info_DataTable_comp <- eventReactive(input$go_comp,{
        df <- select_state_comp()

        state_name_1 <- input$state_comp[1]
        state_name_2 <- input$state_comp[2]
        correlacao = cor(df[, state_name_1], df[, state_name_2])
        
        
        df_tb <-  data.frame(correlacao)
        
        df_tb <- as.data.frame(t(df_tb))
        names(df_tb)[1] <- "Informações"
        row.names(df_tb)[1] = "Correlação"

        return(df_tb)
    })
    #Tabela 2ª aba
    output$info_comp <- renderDT({
        Info_DataTable_comp() %>%
            as.data.frame() %>% 
            DT::datatable(options=list(
                language=list(
                    url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                ),
                pageLength = 5
            ))
    })
}
