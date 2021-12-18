

header <- dashboardHeader(title = "Projeto de Estatística")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Métricas", tabName = "m", icon = icon("chart-line")),
        menuItem('Comparando Ações', tabName = 'comp', icon = icon('chart-bar'))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = 'm',
                fluidRow(
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                        selectInput('state', 'Estado', states_list, multiple=FALSE),
                        uiOutput("timedate"),
                        actionButton('go', 'Submeter')
                        )
                ),
                fluidRow(
                    box(title = "Informações sobre o gasto de energia", width = 12, solidHeader = TRUE,
                        DTOutput('info')
                    )
                ),
                fluidRow(
                    box(title = "Série de Gasto de Energia", width = 12, solidHeader = TRUE,
                        plotOutput('sh')
                    )
                ),
                fluidRow(
                    box(title = "Histograma do Gasto de Energia", width = 12, solidHeader = TRUE,
                        plotOutput('histo')
                    )
                ),
                fluidRow(
                    box(title = "Boxplot do Gasto de Energia", width = 12, solidHeader = TRUE,
                        plotOutput('box')
                    )
                ),
        ),
        tabItem(tabName = 'comp',
                fluidRow(
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                        selectizeInput('state_comp', 'Estados', states_list, multiple=TRUE, options = list(maxItems = 2)),
                        uiOutput("timedate_comp"), 
                        actionButton('go_comp', 'Submeter')
                    )
                ),
                fluidRow(
                    box(title = "Informações sobre o gasto de energia", width = 12, solidHeader = TRUE,
                        DTOutput('info_comp')
                    )
                ),
                fluidRow(
                    box(title = "Série dos Gastos de Energia", width = 12, solidHeader = TRUE,
                        plotOutput('sh_comp')
                    )
                ),
                fluidRow(
                    box(title = "Barplot dos Gastos de Energia", width = 12, solidHeader = TRUE,
                        plotOutput('bar')
                    )
                ),
                fluidRow(
                    box(title = "Correlação dos Gastos de Energia", width = 12, solidHeader = TRUE,
                        plotOutput('scatter')
                    )
                ),
        )
    )
)

ui <- dashboardPage(
    skin = 'blue',
    header, sidebar, body)
