
# Define server logic required to draw a histogram
server <- function(input, output) {
    ################### INPUT ####################
    select_state <- eventReactive(input$go, {
        
        state_name <- input$state
        twin <- input$true_date
        ##print(twin[1])
        
        df_state <- master_df %>% 
            filter(X >= as.POSIXct(twin[1]) & X <= as.POSIXct(twin[2]))
        #df_state <- master_df %>% 
        #    filter(Index == state_name, Date >= as.POSIXct(twin[1]) & Date <= as.POSIXct(twin[2]))
            
        ##filter(master_df, Date >= (twin[1]) & Date <= (twin[2]))
       
        ##s = subset(master_df, master_df$Date >= (input$true_date[1]) & master_df$Date <= (input$true_date[2]))
        
        ##df_statedate <- master_df$Date %>%
          ##  filter(Date >= (input$true_date[1]) & Date <= (input$true_date[2]))
        
        
        ## FALTA -> FILTRAR O DF POR DATA!!
        
        return(df_state)
    })
    
    select_state_comp <- eventReactive(input$go_comp, {
        
        #state_name <- input$state_comp[1]
        twin_comp <- input$true_date_comp
        #print(state_name)
        
        df_state_comp <- master_df %>% 
            filter(X >= as.POSIXct(twin_comp[1]) & X <= as.POSIXct(twin_comp[2]))
        
        return(df_state_comp)
    })
    
    output$timedate <- renderUI({
        
        state_name <- input$state
        
        df <- master_df ##%>% 
        ##    filter(Punjab)
        
        min_time <- min(df$X)
        max_time <- max(df$X)
        ##print(min_time)
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
        ##    filter(Index %in% state_name)
        
        # maxmin_time <- df %>% 
        #     #group_by(Index) %>%
        #     summarise(MD = min(Date)) %>% 
        #     .$MD %>% 
        #     max()
        # 
        # minmax_time <- df %>% 
        #     group_by(Index) %>% 
        #     summarise(MD = max(Date)) %>% 
        #     .$MD %>% 
        #     min()
        # 
        # min_time <- maxmin_time
        # max_time <- minmax_time
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
    Info_DataTable <- eventReactive(input$go,{
        df <- select_state()
        state_name <- input$state
        
        mean <- df %>% select(state_name) %>% colMeans()
        ##Moda <- as.numeric(mode(df[, state_name]))
        Moda <- as.numeric(names(table(df[, state_name]))[which.max(table(df[, state_name]))])
        ##print(Moda)
        ##print(outramedia)
        Desvio_Padrao <- sd(df[, state_name])
        Mediana <- median(df[, state_name])
        Maximo <- max(df[, state_name])
        Minimo <- min(df[, state_name])
        ##print(Maximo)
        
        ##mean <- df %>% select(Close) %>% colMeans()
        Media <- mean[[1]]
        ##Moda <- mode[[1]]
        ##print(mean)
        
        Estado <- input$state
        
        df_tb <-  data.frame(Estado, Media, Moda, Mediana, Desvio_Padrao, Maximo, Minimo)
        
        df_tb <- as.data.frame(t(df_tb))
        names(df_tb)[1] <- "Informações"
        row.names(df_tb)[5] = "Desvio Padrão"
        row.names(df_tb)[6] = "Ponto Máximo"
        row.names(df_tb)[7] = "Ponto Mínimo"
        
        print(df_tb)
        
         # tb  <- as_tibble(cbind(nms = names(df_tb), t(df_tb)))
         # tb <- tb %>%
         #     rename('Informações' = nms,
         #            'Valores' = V1)
         
        return(df_tb)
    })
    
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
    
    output$sh <- renderPlot({
        # All the inputs
        df <- select_state()
        
        state_name <- input$state
        ##if(state_name == names(master_df))
        ##naame <- as.double(master_df[, state_name])
    
        
        aux <- df[, state_name] %>% na.omit() %>% as.numeric()
        ##aux <- df$Close %>% na.omit() %>% as.numeric()
        ##print(aux)
        aux1 <- min(aux)
        aux2 <- max(aux)
        
        df$X <- ymd(df$X)
        ##s = subset(master_df, master_df$Date >= (input$true_date[1]) & master_df$Date <= (input$true_date[2]))
        
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
    output$histo <- renderPlot({
        df <- select_state()
        state_name <- input$state
        
        b <- df %>% #69b3a2
            ggplot(aes(df[, state_name])) +
            geom_histogram( binwidth=3, fill="#b3427e", color="#e9ecef", alpha=0.9) +
            ggtitle("Intervalo de tamanho 3", state_name) +
            xlab('Energia em MU')+ ylab('Frequência') +
            theme_ipsum() +
            theme(
                plot.title = element_text(size=15)
            )
        b
    })
    
    output$box <- renderPlot({
        df <- select_state()
        state_name <- input$state
        
        c <- df %>% 
            ggplot(aes(x = cut(X, breaks="quarter"), y = df[, state_name])) +    
            geom_boxplot() + 
            labs(x = "Datas do começo do quartil", y = 'Energia em MU') +
            theme_bw()
            
        c    
    })
    
    output$sh_comp <- renderPlot({
        # All the inputs
        df <- select_state_comp()
        
        Estados <- input$state_comp[1]
        Estados2 <- input$state_comp[2]
        
        ##if(state_name == names(master_df))
        ##naame <- as.double(master_df[, state_name])
        
        
        # aux <- df[, state_name] %>% na.omit() %>% as.numeric()
        # ##aux <- df$Close %>% na.omit() %>% as.numeric()
        # ##print(aux)
        # aux1 <- min(aux)
        # aux2 <- max(aux)
        
        df$X <- ymd(df$X)
        
       
        cores <- c("#82b342", viridis::plasma(n = 6))
        

        p <- df %>% ggplot(aes(X) ) + 
            geom_line(aes(y =  df[, Estados],   colour= Estados)) +
            geom_line(aes(y =  df[, Estados2], colour = Estados2)) +
            theme_ipsum() + 
            #scale_color_viridis(discrete = TRUE, option = 'C') +
            #scale_color_manual(values = cores) +
            ylab('Eletricidade consumida em MU(Mega Units)') +
            xlab('Datas') +
            scale_x_date(date_labels = "%Y-%m-%d") +
            scale_color_manual(values = c("#82b342", "#7142b3"))
        

            
        p
        
        
    })
    
    output$bar <- renderPlot({
        df <- select_state_comp()
        state_name1 <- input$state_comp[1]
        state_name2 <- input$state_comp[2]
        df$X <- ymd(df$X)
        
        #mean <- df %>% select(Estados) %>% colMeans()
        #mean <- df %>% select(Estados2) %>% colMeans()
        Media1 <- mean(df[, state_name1])
        Media2 <- mean(df[, state_name2])
        
        
        # valor <- c(Media1, Media2)
        # print(valor)
        # nomes <- c(state_name1, state_name2)
        
        df_m <- data.frame(
            valor <- c(Media1, Media2),
            nomes <- c(state_name1, state_name2)
        )
        print(df_m)
        Medias <- as.factor(valor)
        p <- df_m %>%
            ggplot(aes(x = nomes ,y = valor, fill= Medias )) +
            #geom_bar(aes(y = df[, Estados]))+
            geom_bar(stat = 'identity')+
            ylab('Média do gasto de enrgia em MU')+
            xlab('Estados')+
            theme_ipsum() +
            scale_fill_manual(values = c("#82b342", "#b3427e") )
        
            
        p
    })
    
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
}
