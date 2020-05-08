shinyUI(pageWithSidebar(
  headerPanel("Analiza wpływu reguł nietypowych na jakość grupowania metryką Gowera"),
  
  sidebarPanel(
    selectInput(inputId = "algorytm",
                label = "Wybierz algorytm wykrywania reguł nietypowych",
                choices = c("LOF", "COF", "KMEANS","SMALLCLUSTER"),
                selected = "LOF"),
    selectInput(inputId = "metoda",
                label = "Wybierz metodę łączenia grup",
                choices = c("ward.D", "ward.D2", "single","complete", "average","mcquitty", "median", "centroid"),
                selected = "Ward.D"),
    
    sliderInput("setSeed", "Dobierz seed dla KMEANS",
                min = 1, max = 200, value = c(1,200), step= 1),
  
    sliderInput("kSasiadow", "Dobierz k-odległość obiektu p dla LOF lub COF",
                min = 1, max = 150, value = c(1,150), step= 1),
  
  sliderInput("procentRegulNietypowych", "Dobierz % reguł nietypowych do wykrycia",
              min = 1, max = 10, value = c(1,10), step= 1),
  
  sliderInput("liczbaGrup", "Dobierz liczbę grup na jaką chcesz podzielić wczytany zbiór",
              min = 2, max = 10, value = c(1,10), step= 1),
  
  sliderInput("procentZbioru", "Jaki % zbioru to mała grupa? (SMALLCLUSTER)",
              min = 0.01, max = 0.1, value = 0.05, step= 0.01),
  
  sliderInput("smallCluster", "Przesuń suwak, aby wyodrębnić małe grupy, jako reguły nietypowe (SMALLCLUSTER)",
              min = 2, max = 12, value = c(1,12), step = 1)),
  
  mainPanel(
    tabsetPanel(
      
      tabPanel("Dendrogramy", plotOutput("wyjscieDendrogram"),  plotOutput("plot2"),  actionButton("show", "Kliknij i odśwież stronę po wczytaniu zbioru z regułami, 
                                                                            aby zobaczyć aktualny Dendrogram.")),
      tabPanel("Wyniki badań", tableOutput("wyjscieTabela")),  #verbatimTextOutput
      
      tabPanel("Wczytaj zbiór z regułami", sidebarPanel(fileInput("file1", "Wybierz plik rul",multiple = FALSE,
                                                      accept = c("text/rul","text/comma-separated-values,text/plain",".rul")),
               
                                            tags$hr(),
                                            
                                            # Input: Checkbox if file has header ----
                                            checkboxInput("header", "Nagłówek (dla plików rul pozostaw bez Header)", FALSE),
                                            
                                            # Input: Select separator ----
                                            radioButtons("sep", "Separator (dla plików rul pozostaw Przecinek)",
                                                         choices = c(Przecinek = ",",
                                                                     Średnik = ";",
                                                                     Tabulator = "\t"),
                                                         selected= ","),
                                                         
                                                         # Input: Select quotes ----
                                                         radioButtons("quote", "Uzupełnij brakujące jako: NA lub 0",
                                                                      choices = c(
                                                                                  "NA" = 'NA',
                                                                                  "0 (zero)" = '0'),
                                                                      selected = 'NA'),
                                                         
                                            
                                            # Horizontal line ----
                                            tags$hr(),
                                            
                                            # Input: Select number of rows to display ----
                                            radioButtons("disp", "Pokaż",
                                                         choices = c(NagłówekZbioru = "head",
                                                                     Wszystko = "all"),
                                                         selected = "head"),
                                            
                                            # Input: Select quotes ----
                                            radioButtons("quote2", "Jak zaczytać plik? - dla libra.rul zaznacz Double Quote (wtedy wszystkie kolumny będą jakościowe)",
                                                         choices = c(None = "\"",
                                                                     "Double Quote" = "",
                                                                     "Single Quote" = ''),
                                                         selected = "\""))),
      
      
      
      # Main panel for displaying outputs ----
      tabPanel("Podgląd wczytanego zbioru", actionButton("show", "Po wczytaniu zbioru Odśwież (zaktualizuj) Dendrogram. Uwaga! (Podgląd zniknie, jeśli potrzebujesz znów go zobaczyć
                                                     wczytaj zbiór ponownie)."),
        
        # Output: Data file ----
        tableOutput("contents")
        
      )#,
      
     # tabPanel("Podsumowanie", verbatimTextOutput("wyjscieTabela")) #,
     #tabPanel("Dendrogram", plotOutput("wyjscieDendrogram"),   actionButton("show", "Kliknij i Odswiez strone po wczytaniu zbioru z regulami, 
                                                                           # aby zobaczyc aktualny Dendrogram"))
                                            
))
))
#tableOutput("contents")

