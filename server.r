#if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
#BiocManager::install("genefilter", version = "3.8")

library (data.table)
library(tableHTML)
library(DT)
library(dplyr)
library(shinyjs)
library(dmdScheme)
library(shinydashboard)
library(formattable)
library(xlsx)
library(readxl)
library(DDoutlier)

library(formattable)

library(stringr)


library(DMwR)
library(lattice)
library(grid)
library(Rlof)
library(doParallel)
library(foreach)
library(iterators)
library(parallel)
library(cluster)

library(StatMatch)
library(clusterSim)
library(clValid)


library(stats)
library(factoextra)

server <- function(input, output, session) {
  
  # overwriting R's version (not advised)
  `<=` <- function(x, y) (isTRUE(all.equal(x, y)) | (x < y))
  `>=` <- function(x, y) (isTRUE(all.equal(x, y)) | (x > y))
 
  Parametry<- c("Indeks sylwetki","Indeks Dunna","Indeks Daviesa-Bouldina","Pseudo F", "Indeks Huberta", "Miara Gamma", "Liczba grup", "k-sąsiadów lub set.seed", "% reguł nietypowych jako liczba", "Współczynnik CPCC", "Metoda łączenia", "Dataset / Reguły nietypowe" )
  Z_Regułami_Nietypowymi <- 1:12#c("NA", "NA", "NA", "NA","NA","NA","NA","NA","NA","NA","NA")#1:11
  Bez_Reguł_Nietypowych <- 1:12#c("NA", "NA", "NA", "NA","NA","NA","NA","NA","NA","NA","NA")#1:11
  wyniki<-data.frame(Parametry,Z_Regułami_Nietypowymi,Bez_Reguł_Nietypowych)
  
  
  output$contents  <- renderTable({
    
    
    #Define the file name that will be deleted
    #  fn <- "wczytanyZbior.csv"
    #Check its existence
    # if (file.exists(fn)) 
    #Delete file if it exists
     # file.remove(fn)
    
    # plik wej??ciowy $ plik1 pocz??tkowo b??dzie mia?? warto???? NULL. Po wybraniu przez u??ytkownika
    # i przesy??a plik, domy??lnie g????wny plik danych,
    # lub wszystkie wiersze, je??li s?? zaznaczone, zostan?? wy??wietlone.
    
    req(input$file1)
    
    # podczas odczytywania plik??w rozdzielonych ??rednikami,
    # posiadanie separatora przecinkowego powoduje b????d `read.csv`
    tryCatch(
      {
        collection<- fread(input$file1$datapath,
                            header = input$header,
                            sep = input$sep)
                          #quote = input$quote)
        file1 = input$file1
        saveRDS(file1 , file = "file1.rds")
        quote2 = input$quote2
       # nazwa <-input$file1$datapath                   
        saveRDS(quote2, file = "quote2.rds")
        
        ATTRIBUTES <- str_sub(string=collection[2], start=12)
        ATTRIBUTES1<- as.numeric(ATTRIBUTES)
        DECISION_VALUES<- str_sub(string=collection[ATTRIBUTES1+3], start=17)
        DECISION_VALUES1<-as.numeric(DECISION_VALUES)
        saveRDS(DECISION_VALUES1, file = "RULES1.rds")
        RULES<- str_sub(string=collection[ATTRIBUTES1+3+DECISION_VALUES1+1], start=7)
        RULES1<-as.numeric(RULES)
        REGULY_START<-(ATTRIBUTES1+3+DECISION_VALUES1+1+1)
        REGULY_KONIEC<-(REGULY_START+RULES1-1)
        collection1<-collection[(REGULY_START:REGULY_KONIEC),]
        collection2<-str_split(string=collection1$V1, pattern = "=>")
        collection3<-data.table(Reduce(rbind, collection2))
        collection4<-collection3[,1]
        m <- as.data.frame(matrix(0, ncol = ATTRIBUTES1, nrow = RULES1))
        q <- ATTRIBUTES1+2
        k <- c(3:q)
        l<-c(1:RULES1)
        
        for (j in l){
          for (i in k){
            
            collection_3 = unlist(gregexpr(pattern =' ',collection[i]))
            collection_4<-str_sub(string=collection[i], start = 1, end = collection_3[1]-1)
            collection5 <- str_extract(string=collection4[j], pattern = collection_4)
            
            if(is.na(collection5) == TRUE ||  length(unlist(gregexpr(pattern=collection5, collection4[j])))>1) {
              m[j,i-2] = input$quote } else {   
                collection6=unlist(gregexpr(pattern=collection5, collection4[j]))
                collection7 = nchar(collection5)+1
                collection_start<- str_sub(string = collection4[j], start = collection6)
                collection7a<-gregexpr(pattern =')', collection_start)
                ostatni_skladnik = collection7a[[1]][1]
                ostatni_skladnik1=ostatni_skladnik-2
                collection8<-str_sub(string = collection4[j], start = (collection6 + collection7), end =  collection6 + ostatni_skladnik1)
                collection9=unlist(gregexpr(pattern="=",  collection8))
                if (collection9==-1)
                {m[j,i-2] = collection8} else { 
                  m[j,i-2] = input$quote
                }
              }
          }
        }
        
    
        p<-(ATTRIBUTES1-1)
        d <- m[ ,1:p]
        
       write.csv(d,"wczytanyZbior.csv", row.names = FALSE)
        
     
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(d))
    }
    else {
      return(d)
    }
    
  
    
  }) 
    
 
  output$wyjscieTabela <-render_tableHTML({   #renderPrint  render_tableHTML  renderTable
   
   #input$algorytm
   #input$metoda
   #input$setSeed[1]
   #input$kSasiadow[1]
   #input$procentRegulNietypowych[1]
   #input$liczbaGrup[1]
    
   qoute2 <- readRDS(file = "quote2.rds")
    
    g <- fread ("wczytanyZbior.csv", sep=',', header = TRUE, stringsAsFactors = FALSE, quote = qoute2)
    g[g=="NA"] <- NA
    #g[g=="0"] <- 0
    
    #g<-as.data.frame(g)
   #g<-as.numeric(g)
    
    gower_mat1 <- gower.dist (g ,  rngs = NA ,  KR.corr = TRUE ,  var.weights  =  NULL)
    gower_mat1[is.nan(gower_mat1)] <- 0
    gower_mat <-as.dist(gower_mat1) 
    clusters <- hclust(gower_mat, method = input$metoda)    
    wyniki[11,2] <- input$metoda
    
    #Cophenetic Distances for a Hierarchical Clustering
    d1 <- gower_mat
    hc <- clusters
    d2 <- cophenetic(hc)
    wyniki[10,2] <- cor(d1, d2)
    
    # Ocena jako??ci grupowania - cluster validity (prawdziwo????, wiarygodno????, jako???? grupowania)
    cutree_k <- input$liczbaGrup[1]
    wyniki[7,2]<-cutree_k
    
     wyniki[7,3]<-cutree_k
     cutree_k_LOF <- input$liczbaGrup[1]
     cutree_k_COF <- input$liczbaGrup[1]
     cutree_k_kmeans <- input$liczbaGrup[1]
     cutree_k_SMALLCLUSTER <- input$liczbaGrup[1]
    
    #cutree(clusters, k = cutree_k) - wektor liczb ca??kowitych dodatnich informuj??cy 
    #o przynale??no??ci obiekt??w do klasy
    cutree_wektor <- cutree(clusters, k = cutree_k) # k liczba klas
    
    #1 Oparte o odleg??o???? mi??dzy jednostkami w skupieniu i mi??dzy skupieniami 
    
    #silhouette # im wiekszy tym lepiej
    #wska??nik sylwetkowy
    #Kaufman, L., Rousseeuw, P.J. (1990), Finding groups in data: an introduction to cluster analysis,
    #Wiley, New York, pp. 83-88. ISBN: 978-0-471-73578-6.
    
    #icq_sylwetki <- index.S(gower_mat,cutree_wektor)
    #wyniki[1,2]<-icq_sylwetki
     si3 <- silhouette(cutree_wektor, gower_mat) # im wiekszy tym lepiej
     icq_sylwetki <- mean(si3[,"sil_width"])
      wyniki[1,2]<-icq_sylwetki
    #Indeks Dunna. Ten wska??nik jako??ci klastra zosta?? zaproponowany przez Dunna (1973).
    #Wska??nik Dunna to stosunek najmniejszej odleg??o??ci mi??dzy obserwacjami spoza tej samej grupy
    #do najwi??kszej odleg??o??ci wewn??trz gromady. Indeks Dunna ma warto???? od zera do niesko??czono??ci 
    #i nale??y go zmaksymalizowa??. Aby uzyska?? szczeg????owe informacje, patrz winieta na opakowaniu.
    #dunn(gower_mat, cutree_wektor)  # im wi??kszy tym lepiej
    wyniki[2,2]<-dunn(gower_mat, cutree_wektor)
    #2 Oparte o rozproszenie jednostek w skupieniu i odleg??o??ci mi??dzy skupieniami
    #??clusterSim
    
    # Davies-Bouldin???s (DB???s) Index. Czym mniejszy tym lepiej
    #print(index.DB(gower_mat, cutree_wektor, centrotypes="centroids", p = 2, q = 2))
    index_DB <- index.DB(gower_mat, cutree_wektor, centrotypes="centroids", p = 2, q = 2)
    wyniki[3,2]<-index_DB[1]
    #3 Oparte na sumie kwadrat??w wewn??trz skupie?? i mi??dzy skupieniami
    
    #Indeks Pseudo F. Pseudo-statystyka F opisuje stosunek wariancji mi??dzy klastrami
    #do wariancji skupie?? (Calinski i Harabasz, 1974):
    icq_index_G1 <- index.G1(gower_mat1, cutree_wektor, d = NULL, centrotypes = "centroids")
    wyniki[4,2]<-icq_index_G1
    #Gordon, AD (1999), Classification , Chapman & Hall / CRC, Londyn, str. 62. ISBN 9781584880134.
    #Oblicza wska??nik jako??ci wewn??trznego klastra G3 #Huberta i Levine'a
    icq_index_G3 <- index.G3(gower_mat, cutree_wektor)
    wyniki[5,2]<-icq_index_G3
    #4 Inne - stosunek r??znicy do sumy liczby par odleg??o??ci zgodnych i liczba par odleg??o??ci niezgodnych
    #Adaptacja G2 Baker i Hubert statystyki gamma Goodmana-Kruskala 
    icq_index_G2 <- index.G2(gower_mat, cutree_wektor)
    wyniki[6,2]<-icq_index_G2
    # Wyznaczenie charakterystyk poszczeg??lnych klas
    #cluster.Description(gower_mat, cutree_wektor, sdType = "sample")
  #  s <- readRDS(file = "nazwa.rds")
    wyniki[12,2] <-readRDS(file = "file1.rds") #str_sub(string = s , start = 80)#NA
    
    
    if (input$algorytm == "LOF")
    {
    #  d <- fread ("wczytanyZbior.csv", sep=',', header = TRUE)
    #  gower_mat1 <- gower.dist (d[,])
      # Regu??y nietypowe
      qoute2 <- readRDS(file = "quote2.rds")
      
      e <- fread ("wczytanyZbior.csv", sep=',', header = TRUE,stringsAsFactors = FALSE, quote = qoute2)
      e[e=="NA"] <- NA
     #e[e=="0"] <- 0
      
      RULES1<-nrow(e)
      howmany <-(input$procentRegulNietypowych[1]/100)*RULES1
      howmany<-ceiling(howmany)
      wyniki[9,2]<-0
      wyniki[9,3]<-howmany
      set_k <- input$kSasiadow[1] 
      wyniki[8,2]<-NA
      wyniki[8,3]<-set_k
      
      #LOF k = 3 [Breunig i in., 2000] ocena stopnia oddalenia danej obserwacji 
      #od pozosta??ych przy uwzgl??dnieniu zag??szczenia obiekt??w z k-elementowego s??siedztwa 
      #d<- as.data.frame(d, stringsAsFactors = FALSE)
      outlier.scores <- lofactor(gower_mat1, k = set_k)
      outliers_LOF <- order(outlier.scores, decreasing = T) [1:howmany]
      wyniki[12,3] <- toString(outliers_LOF)
      #print(outliers_LOF)
      #print(gower_mat1 [outliers_LOF,])
      
      #Usuni??cie i ponowne grupowanie
      
      # usuni??cie outliers_LOF
      p <- outliers_LOF
      data_inf1 <- e[-p, ]
      #data_inf1
      
      # dendrogram AHC klastrowanie hierarchiczne po usuni??ciu odchyle?? - outliers_LOF
      gower_mat1 <- gower.dist ( data_inf1 ,  rngs = NA ,  KR.corr = TRUE ,  var.weights  =  NULL )
        gower_mat1[is.nan(gower_mat1)] <- 0
      #gower_mat -macierz danych (odleg??o??ci gowera, ale teraz tak wygl??daj?? moje dane, ju?? nie s?? jako??ciowe)
      #gower_mat
      #gower_mat1
      gower_mat <-as.dist(gower_mat1) 
      #gower_mat 
      #dist.metoda oblicza dist_gower_mat
      #dist_gower_mat<-dist(gower_mat, method = "euclidean", diag = TRUE, upper = FALSE, p = 2)  # to mo??na zmienia??!!!!!
      #"euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
      #dist_gower_mat
      
      
      clusters <- hclust(gower_mat,method = input$metoda) #grupowanie najdalszych s??siad??w  # to mo??na zmienia??
      #"ward.D", "ward.D2", "single", "complete", "average","mcquitty", "median", "centroid"
      #saveRDS(clusters, file = "clustersLOF.rds")
      wyniki[11,3] <- input$metoda
      
      #fviz_dend(clusters, k = 4, # Cut in two groups  !!!!!!! ustawi?? cutree_k <- 2
      #         cex = 0.5, # label size
      #        k_colors = c("#2E9FDF", "#FC4E07", "#07fc4e","#E7B800", "#00AFBB"),
      #       color_labels_by_k = TRUE, # color labels by groups
      #      rect = TRUE # Add rectangle around groups 
      #)
      # plot(clusters)
      
      #Cophenetic Distances for a Hierarchical Clustering
      #require(graphics)
      d1 <- gower_mat
      hc <- clusters
      d2 <- cophenetic(hc)
      #cor(d1, d2) 
      wyniki[10,3] <- cor(d1, d2)
      
      # Ocena jako??ci grupowania - cluster validity (prawdziwo????, wiarygodno????, jako???? grupowania)
      
      # na ile skupie?? podzielono, ??eby zbada?? jako????
      # !!!!! cutree_k_LOF <- 2
      
      #cutree(clusters, k = cutree_k) - wektor liczb ca??kowitych dodatnich informuj??cy 
      #o przynale??no??ci obiekt??w do klasy
      cutree_wektor <- cutree(clusters, k = cutree_k_LOF)
      # cutree_wektor
      #1 Oparte o odleg??o???? mi??dzy jednostakami w skupieniu i mi??dzy skupieniami 
      
      #silhouette z wykresem
      #wska??nik sylwetkowy 
      #Kaufman, L., Rousseeuw, P.J. (1990), Finding groups in data: an introduction to cluster analysis,
      #Wiley, New York, pp. 83-88. ISBN: 978-0-471-73578-6.
      
      # si3 <- silhouette(cutree_wektor, gower_mat) # im wiekszy tym lepiej
      #  plot(si3, nmax = 80, cex.names = 0.5)
      
      #silhouette 2 tylko warto???? # im wiekszy tym lepiej
      #wska??nik sylwetkowy
      #Kaufman, L., Rousseeuw, P.J. (1990), Finding groups in data: an introduction to cluster analysis,
      #Wiley, New York, pp. 83-88. ISBN: 978-0-471-73578-6.
     
     #icq <- index.S(gower_mat,cutree_wektor)
      # print(icq)
      #wyniki[1,3]<-icq

     si3 <- silhouette(cutree_wektor, gower_mat) # im wiekszy tym lepiej
     icq1 <- mean(si3[,"sil_width"])
      wyniki[1,3]<-icq1


      #Indeks Dunna. Ten wska??nik jako??ci klastra zosta?? zaproponowany przez Dunna (1973).
      #Wska??nik Dunna to stosunek najmniejszej odleg??o??ci mi??dzy obserwacjami spoza tej samej grupy
      #do najwi??kszej odleg??o??ci wewn??trz gromady. Indeks Dunna ma warto???? od zera do niesko??czono??ci 
      #i nale??y go zmaksymalizowa??. Aby uzyska?? szczeg????owe informacje, patrz winieta na opakowaniu.
      #dunn(gower_mat, cutree_wektor)  # im wi??kszy tym lepiej
      wyniki[2,3]<-dunn(gower_mat, cutree_wektor)
      #2 Oparte o rozproszenie jednostek w skupieniu i odleg??o??ci mi??dzy skupieniami
      #??clusterSim
      
      # Davies-Bouldin???s (DB???s) Index. Czym mniejszy tym lepiej
      #print(index.DB(gower_mat, cutree_wektor, centrotypes="centroids", p = 2, q = 2))
      index_DB <- index.DB(gower_mat, cutree_wektor, centrotypes="centroids", p = 2, q = 2)
      # index_DB[1]
      wyniki[3,3]<-index_DB[1]
      #3 Oparte na sumie kwadrat??w wewn??trz skupie?? i mi??dzy skupieniami
      
      #Indeks Pseudo F. Pseudo-statystyka F opisuje stosunek wariancji mi??dzy klastrami
      #do wariancji skupie?? (Calinski i Harabasz, 1974):
      icq_index_G1 <- index.G1(gower_mat1, cutree_wektor, d = NULL, centrotypes = "centroids")
      #print(icq_index_G1) # Du??e warto??ci Pseudo F wskazuj?? na zwarte i oddzielone klastry.
      wyniki[4,3]<-icq_index_G1
      #Gordon, AD (1999), Classification , Chapman & Hall / CRC, Londyn, str. 62. ISBN 9781584880134.
      #Oblicza wska??nik jako??ci wewn??trznego klastra G3 #Huberta i Levine'a
      icq_index_G3 <- index.G3(gower_mat, cutree_wektor)
      # print(icq_index_G3) # im mniejszy to lepiej je??li to #Huberta i Levine'a
      wyniki[5,3]<-icq_index_G3
      #4 Inne - stosunek r??znicy do sumy liczby par odleg??o??ci zgodnych i liczba par odleg??o??ci niezgodnych
      #Adaptacja G2 Baker i Hubert statystyki gamma Goodmana-Kruskala 
      icq_index_G2 <- index.G2(gower_mat, cutree_wektor)
      wyniki[6,3]<-icq_index_G2
      
    }
    
    if (input$algorytm == "COF")
    {
      
      qoute2 <- readRDS(file = "quote2.rds")
   #   d <- fread ("wczytanyZbior.csv", sep=',', header = TRUE)
    #  gower_mat1 <- gower.dist (d[,])
      
      #Regu??y nietypowe
      f <- fread ("wczytanyZbior.csv", sep=',', header = TRUE,stringsAsFactors = FALSE, quote = qoute2)
      f[f=="NA"] <- NA
      #f[f=="0"] <- 0
      RULES1<-nrow(f)
      howmany <-(input$procentRegulNietypowych[1]/100)*RULES1
      howmany<-ceiling(howmany)
      wyniki[9,2]<-0
      wyniki[9,3]<-howmany
      set_k <- input$kSasiadow[1] 
      wyniki[8,2]<-NA
      wyniki[8,3]<-set_k
      
      
      #COF Funkcja do obliczania opartego na ????czno??ci wsp????czynnika odstaj??cego jako wyniku 
      #odstaj??cego dla obserwacji. Sugerowana przez Tang, J., Chen, Z., Fu, A. W. C. i Cheung, D. W. (2002)
      outlier.scores <- COF(gower_mat1, k = set_k)
      outliers_COF <- order(outlier.scores, decreasing = T) [1:howmany]
      wyniki[12,3] <- toString(outliers_COF)
      #print(outliers_COF)
      #print(gower_mat1 [outliers_COF,])
      
      
      #Usuni??cie i ponowne grupowanie
      
      # usuni??cie outliers_COF
      q <- outliers_COF
      data_inf2 <- f[-q, ]
      #data_inf2
      
      
      
      # dendrogram AHC klastrowanie hierarchiczne po usuni??ciu odchyle?? - outliers_COF
      gower_mat1 <- gower.dist ( data_inf2 ,  rngs = NA ,  KR.corr = TRUE ,  var.weights  =  NULL )
      gower_mat1[is.nan(gower_mat1)] <- 0
      #gower_mat -macierz danych (odleg??o??ci gowera, ale teraz tak wygl??daj?? moje dane, ju?? nie s?? jako??ciowe)
      #gower_mat
      gower_mat <-as.dist(gower_mat1) 
      
      #dist.metoda oblicza dist_gower_mat
      #dist_gower_mat<-dist(gower_mat, method = "euclidean", diag = TRUE, upper = FALSE, p = 2)  #to mo??na zmieni??!!!!
      #"euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
      #dist_gower_mat
      
      
      clusters <- hclust(gower_mat,method = input$metoda) #grupowanie najdalszych s??siad??w   #to mozna zmieni??
      #saveRDS(clusters, file = "clustersCOF.rds")
      
      #"ward.D", "ward.D2", "single", "complete", "average","mcquitty", "median", "centroid"
      wyniki[11,3] <- input$metoda
      
      #   fviz_dend(clusters, k = 2, # Cut in two groups  !!!!!!! ustawi?? cutree_k <- 2
      #            cex = 0.5, # label size
      #           k_colors = c("#2E9FDF", "#FC4E07"),
      #          color_labels_by_k = TRUE, # color labels by groups
      #         rect = TRUE # Add rectangle around groups 
      #)
      
      #plot(clusters)
      
      #Cophenetic Distances for a Hierarchical Clustering
      #require(graphics)
      d1 <- gower_mat
      hc <- clusters
      d2 <- cophenetic(hc)
      # cor(d1, d2) 
      wyniki[10,3] <- cor(d1, d2)
      
      # Ocena jako??ci grupowania - cluster validity (prawdziwo????, wiarygodno????, jako???? grupowania)
      
      # na ile skupie?? podzielono, ??eby zbada?? jako????
      # !!!!! cutree_k_COF <- 2
      
      #cutree(clusters, k = cutree_k) - wektor liczb ca??kowitych dodatnich informuj??cy 
      #o przynale??no??ci obiekt??w do klasy
      cutree_wektor <- cutree(clusters, k = cutree_k_COF)
      # cutree_wektor
      #1 Oparte o odleg??o???? mi??dzy jednostakami w skupieniu i mi??dzy skupieniami 
      
      #silhouette z wykresem
      #wska??nik sylwetkowy 
      #Kaufman, L., Rousseeuw, P.J. (1990), Finding groups in data: an introduction to cluster analysis,
      #Wiley, New York, pp. 83-88. ISBN: 978-0-471-73578-6.
      
      # si3 <- silhouette(cutree_wektor, gower_mat) # im wiekszy tym lepiej
      #  plot(si3, nmax = 80, cex.names = 0.5)
      
      #silhouette 2 tylko warto???? # im wiekszy tym lepiej
      #wska??nik sylwetkowy
      #Kaufman, L., Rousseeuw, P.J. (1990), Finding groups in data: an introduction to cluster analysis,
      #Wiley, New York, pp. 83-88. ISBN: 978-0-471-73578-6.
      #icq <- index.S(gower_mat,cutree_wektor)
      # print(icq)
      #wyniki[1,3]<-icq
      si3 <- silhouette(cutree_wektor, gower_mat) # im wiekszy tym lepiej
      icq2 <- mean(si3[,"sil_width"])
      wyniki[1,3]<-icq2
      #Indeks Dunna. Ten wska??nik jako??ci klastra zosta?? zaproponowany przez Dunna (1973).
      #Wska??nik Dunna to stosunek najmniejszej odleg??o??ci mi??dzy obserwacjami spoza tej samej grupy
      #do najwi??kszej odleg??o??ci wewn??trz gromady. Indeks Dunna ma warto???? od zera do niesko??czono??ci 
      #i nale??y go zmaksymalizowa??. Aby uzyska?? szczeg????owe informacje, patrz winieta na opakowaniu.
      #  dunn(gower_mat, cutree_wektor)  # im wi??kszy tym lepiej
      wyniki[2,3]<-dunn(gower_mat, cutree_wektor)
      #2 Oparte o rozproszenie jednostek w skupieniu i odleg??o??ci mi??dzy skupieniami
      #??clusterSim
      
      # Davies-Bouldin???s (DB???s) Index. Czym mniejszy tym lepiej
      #print(index.DB(gower_mat, cutree_wektor, centrotypes="centroids", p = 2, q = 2))
      index_DB <- index.DB(gower_mat, cutree_wektor, centrotypes="centroids", p = 2, q = 2)
      # index_DB[1]
      wyniki[3,3]<-index_DB[1]
      #3 Oparte na sumie kwadrat??w wewn??trz skupie?? i mi??dzy skupieniami
      
      #Indeks Pseudo F. Pseudo-statystyka F opisuje stosunek wariancji mi??dzy klastrami
      #do wariancji skupie?? (Calinski i Harabasz, 1974):
      icq_index_G1 <- index.G1(gower_mat1, cutree_wektor, d = NULL, centrotypes = "centroids")
      # print(icq_index_G1) # Du??e warto??ci Pseudo F wskazuj?? na zwarte i oddzielone klastry.
      wyniki[4,3]<-icq_index_G1
      #Gordon, AD (1999), Classification , Chapman & Hall / CRC, Londyn, str. 62. ISBN 9781584880134.
      #Oblicza wska??nik jako??ci wewn??trznego klastra G3 #Huberta i Levine'a
      icq_index_G3 <- index.G3(gower_mat, cutree_wektor)
      # print(icq_index_G3) # im mniejszy to lepiej je??li to #Huberta i Levine'a
      wyniki[5,3]<-icq_index_G3
      #4 Inne - stosunek r??znicy do sumy liczby par odleg??o??ci zgodnych i liczba par odleg??o??ci niezgodnych
      #Adaptacja G2 Baker i Hubert statystyki gamma Goodmana-Kruskala 
      icq_index_G2 <- index.G2(gower_mat, cutree_wektor)
      #  print(icq_index_G2) # czym wiekszy tym lepiej
      wyniki[6,3]<-icq_index_G2
      
      # Wyznaczenie charakterystyk poszczeg??lnych klas
      #cluster.Description(gower_mat, cutree_wektor, sdType = "sample")
      
      #wyniki
      # print(outliers_COF)
      
    }
      
    if (input$algorytm == "KMEANS")
      
    {
      qoute2 <- readRDS(file = "quote2.rds")
      # Reguly nietypowe
      h <- fread ("wczytanyZbior.csv", sep=',', header = TRUE,stringsAsFactors = FALSE, quote = qoute2)
      h[h=="NA"] <- NA
      #h[h=="0"] <- 0
      RULES1<-nrow(h)
      howmany <-(input$procentRegulNietypowych[1]/100)*RULES1
      howmany<-ceiling(howmany)
      wyniki[9,2]<-0
      wyniki[9,3]<-howmany
      #howmany
      
      set_k <- input$setSeed[1] 
      wyniki[8,2]<-NA
      wyniki[8,3]<-set_k
      #Analiza skupien metoda k-means(k-srednich), k = DECISION_VALUES1
      #metoda zak??ada, ??e obserwacj?? odstaj??c?? jest obiekt niedaj??cy si?? w????czy?? do ??adnej grupy
      #b??d?? na przyk??ad jest z ma??o licznej grupy
      #DECISION_VALUES1
      RNGkind(sample.kind = "Rounding")
      set.seed(set_k)  #7 #8
      #gower_mat1
      DECISION_VALUES1<-readRDS(file = "RULES1.rds")
      kmeans.result <- kmeans(gower_mat1 , centers = DECISION_VALUES1)
      centers <- kmeans.result$centers[kmeans.result$cluster, ]
      distances <- sqrt(rowSums((as.numeric(unlist(gower_mat1)) - centers)^2))
      outliers_kmeans <- order( distances, decreasing = T ) [1:howmany]
      wyniki[12,3] <- toString(outliers_kmeans)
     # print(outliers_kmeans)
      #print(gower_mat1 [outliers_kmeans,])
      
      
      #Usuni??cie i ponowne grupowanie
      
      # usuni??cie outliers_kmeans
      r <- outliers_kmeans
      #data_inf3 <- d[-c(282,266), ]
      data_inf3 <- h[-r, ]
      #data_inf3
      
      
      # dendrogram AHC klastrowanie hierarchiczne po usuni??ciu odchyle?? - outliers_kmeans
      gower_mat1 <- gower.dist ( data_inf3 ,  rngs = NA ,  KR.corr = TRUE ,  var.weights  =  NULL )
      #gower_mat -macierz danych (odleg??o??ci gowera, ale teraz tak wygl??daj?? moje dane, ju?? nie s?? jako??ciowe)
      #gower_mat
     # gower_mat1
      gower_mat1[is.nan(gower_mat1)] <- 0
      #gower_mat1
      gower_mat <-as.dist(gower_mat1) 
      #gower_mat 
      
      #dist.metoda oblicza dist_gower_mat
      #dist_gower_mat<-dist(gower_mat, method = "euclidean", diag = TRUE, upper = FALSE, p = 2)  # to mo??na zmienia?? !!!
      #"euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
      #dist_gower_mat
      
      
      clusters <- hclust(gower_mat, method =  input$metoda) #grupowanie najdalszych s??siad??w  # to mo??na zmienia??!!!
      #"ward.D", "ward.D2", "single", "complete", "average","mcquitty", "median", "centroid"
      #saveRDS(clusters, file = "clustersKMEANS.rds")
      wyniki[11,3] <-  input$metoda
      
      # cex: label size
      #fviz_dend(clusters, cex = 0.5)
      #fviz_dend(clusters, k = 7, # Cut in two groups  !!!!!!! ustawi?? cutree_k <- 2
      #         cex = 0.5, # label size
      #        k_colors = c("#2E9FDF", "#FC4E07", "#07fc4e","#E7B800", "#00AFBB", "#065535","#7157c4"),
      #       color_labels_by_k = TRUE, # color labels by groups
      #      rect = TRUE # Add rectangle around groups 
      #)
     # plot(clusters)
      
      #Cophenetic Distances for a Hierarchical Clustering
      #require(graphics)
      d1 <- gower_mat
      hc <- clusters
      d2 <- cophenetic(hc)
     # cor(d1, d2) 
      wyniki[10,3] <- cor(d1, d2)
      
      # Ocena jako??ci grupowania - cluster validity (prawdziwo????, wiarygodno????, jako???? grupowania)
      
      # na ile skupie?? podzielono, ??eby zbada?? jako????
      # !!!!! cutree_k_kmeans <- 2
      
      #cutree(clusters, k = cutree_k) - wektor liczb ca??kowitych dodatnich informuj??cy 
      #o przynale??no??ci obiekt??w do klasy
      cutree_wektor <- cutree(clusters, k = cutree_k_kmeans)
     # cutree_wektor
      #1 Oparte o odleg??o???? mi??dzy jednostakami w skupieniu i mi??dzy skupieniami 
      
      #silhouette z wykresem
      #wska??nik sylwetkowy 
      #Kaufman, L., Rousseeuw, P.J. (1990), Finding groups in data: an introduction to cluster analysis,
      #Wiley, New York, pp. 83-88. ISBN: 978-0-471-73578-6.
      
     # si3 <- silhouette(cutree_wektor, gower_mat) # im wiekszy tym lepiej
    #  plot(si3, nmax = 80, cex.names = 0.5)
      
      #silhouette 2 tylko warto???? # im wiekszy tym lepiej
      #wska??nik sylwetkowy
      #Kaufman, L., Rousseeuw, P.J. (1990), Finding groups in data: an introduction to cluster analysis,
      #Wiley, New York, pp. 83-88. ISBN: 978-0-471-73578-6.
    #  icq <- index.S(gower_mat,cutree_wektor)
    #  print(icq)
    #  wyniki[1,3]<-icq
      si3 <- silhouette(cutree_wektor, gower_mat) # im wiekszy tym lepiej
      icq3 <- mean(si3[,"sil_width"])
      wyniki[1,3]<-icq3
      #Indeks Dunna. Ten wska??nik jako??ci klastra zosta?? zaproponowany przez Dunna (1973).
      #Wska??nik Dunna to stosunek najmniejszej odleg??o??ci mi??dzy obserwacjami spoza tej samej grupy
      #do najwi??kszej odleg??o??ci wewn??trz gromady. Indeks Dunna ma warto???? od zera do niesko??czono??ci 
      #i nale??y go zmaksymalizowa??. Aby uzyska?? szczeg????owe informacje, patrz winieta na opakowaniu.
     # dunn(gower_mat, cutree_wektor)  # im wi??kszy tym lepiej
      wyniki[2,3]<-dunn(gower_mat, cutree_wektor)
      #2 Oparte o rozproszenie jednostek w skupieniu i odleg??o??ci mi??dzy skupieniami
      #??clusterSim
      
      # Davies-Bouldin???s (DB???s) Index. Czym mniejszy tym lepiej
      #print(index.DB(gower_mat, cutree_wektor, centrotypes="centroids", p = 2, q = 2))
      index_DB<-index.DB(gower_mat, cutree_wektor, centrotypes="centroids", p = 2, q = 2)
     # index_DB[1]
      wyniki[3,3]<-index_DB[1]
      #3 Oparte na sumie kwadrat??w wewn??trz skupie?? i mi??dzy skupieniami
      
      #Indeks Pseudo F. Pseudo-statystyka F opisuje stosunek wariancji mi??dzy klastrami
      #do wariancji skupie?? (Calinski i Harabasz, 1974):
      icq_index_G1 <- index.G1(gower_mat1, cutree_wektor, d = NULL, centrotypes = "centroids")
     # print(icq_index_G1) # Du??e warto??ci Pseudo F wskazuj?? na zwarte i oddzielone klastry.
      wyniki[4,3]<-icq_index_G1
      #Gordon, AD (1999), Classification , Chapman & Hall / CRC, Londyn, str. 62. ISBN 9781584880134.
      #Oblicza wska??nik jako??ci wewn??trznego klastra G3 #Huberta i Levine'a
      icq_index_G3 <- index.G3(gower_mat, cutree_wektor)
    #  print(icq_index_G3) # im mniejszy to lepiej je??li to #Huberta i Levine'a
      wyniki[5,3]<-icq_index_G3
      #4 Inne - stosunek r??znicy do sumy liczby par odleg??o??ci zgodnych i liczba par odleg??o??ci niezgodnych
      #Adaptacja G2 Baker i Hubert statystyki gamma Goodmana-Kruskala 
      icq_index_G2 <- index.G2(gower_mat, cutree_wektor)
     # print(icq_index_G2) # czym wiekszy tym lepiej
      wyniki[6,3]<-icq_index_G2
      
      # Wyznaczenie charakterystyk poszczeg??lnych klas
      #cluster.Description(gower_mat, cutree_wektor, sdType = "sample")
      
     # wyniki
      #print(outliers_kmeans)
      
      
    }

   # Mydata <- c(Parametry)  #c(Parametry,zRegulamiNietypowymi,BezRegulNietypowych)
    
# colnames(wyniki) <- c(Parametry,zRegulamiNietypowymi,BezRegulNietypowych)
    
    
    # (as.numeric(wyniki[2,2]) <= as.numeric(wyniki[2,3])) &
    #if (as.numeric(wyniki[3,2]) <= as.numeric(wyniki[3,3]))
    #if(as.numeric(wyniki[4,2]) >= as.numeric(wyniki[4,3]))
    #if(as.numeric(wyniki[5,2]) <= as.numeric(wyniki[5,3]))
    #if( as.numeric(wyniki[5,2]) >= as.numeric(wyniki[5,3]))
    #if(as.numeric(wyniki[10,2]) >= as.numeric(wyniki[10,3]))
    
    if (input$algorytm == "SMALLCLUSTER")
    {
      #  d <- fread ("wczytanyZbior.csv", sep=',', header = TRUE)
      #  gower_mat1 <- gower.dist (d[,])
      # Regu??y nietypowe
      qoute2 <- readRDS(file = "quote2.rds")
  
      j <- fread ("wczytanyZbior.csv", sep=',', header = TRUE,stringsAsFactors = FALSE, quote = qoute2)
      j[j=="NA"] <- NA
      #e[e=="0"] <- 0
      
      cutree_k_SMALLCLUSTER <- input$liczbaGrup[1]
      cutree_k_SMALLCLUSTER_outliers <- input$smallCluster[1]
      
      if (cutree_k_SMALLCLUSTER_outliers > nrow(j))
      { return("Wybrana liczba klastrów jest większa niż liczba obserwacji") }
      
     # RULES1<-nrow(j)
    #  howmany <-(input$procentRegulNietypowych[1]/100)*RULES1
     # howmany<-ceiling(howmany)
      wyniki[9,2]<-0
      wyniki[9,3]<-"smallest clusters"
      #set_k <- input$kSasiadow[1] 
      wyniki[8,2]<-NA
      wyniki[8,3]<-NA
      
      #LOF k = 3 [Breunig i in., 2000] ocena stopnia oddalenia danej obserwacji 
      #od pozosta??ych przy uwzgl??dnieniu zag??szczenia obiekt??w z k-elementowego s??siedztwa 
      #d<- as.data.frame(d, stringsAsFactors = FALSE)
      gower_mat1 <- gower.dist ( j ,  rngs = NA ,  KR.corr = TRUE ,  var.weights  =  NULL )
      gower_mat1[is.nan(gower_mat1)] <- 0
      #gower_mat -macierz danych (odleg??o??ci gowera, ale teraz tak wygl??daj?? moje dane, ju?? nie s?? jako??ciowe)
      #gower_mat
      #gower_mat1
      gower_mat <-as.dist(gower_mat1) 
      #gower_mat 
      #dist.metoda oblicza dist_gower_mat
      #dist_gower_mat<-dist(gower_mat, method = "euclidean", diag = TRUE, upper = FALSE, p = 2)  # to mo??na zmienia??!!!!!
      #"euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
      #dist_gower_mat
      clusters <- hclust(gower_mat,method = input$metoda)
      cutree_wektor <- cutree(clusters, k = cutree_k_SMALLCLUSTER_outliers)
      
      
     # outlier.scores <- lofactor(gower_mat1, k = set_k)
    #  outliers_LOF <- order(outlier.scores, decreasing = T) [1:howmany]
      ilosc_obserwacji <-length(cutree_wektor) 
      ilosc_obserwacji_1 <-length(which(cutree_wektor == 1))
      ilosc_obserwacji_2 <-length(which(cutree_wektor == 2))
      ilosc_obserwacji_3 <-length(which(cutree_wektor == 3))
      ilosc_obserwacji_4 <-length(which(cutree_wektor == 4))
      ilosc_obserwacji_5 <-length(which(cutree_wektor == 5))
      ilosc_obserwacji_6 <-length(which(cutree_wektor == 6))
      ilosc_obserwacji_7 <-length(which(cutree_wektor == 7))
      ilosc_obserwacji_8 <-length(which(cutree_wektor == 8))
      ilosc_obserwacji_9 <-length(which(cutree_wektor == 9))
      ilosc_obserwacji_10 <-length(which(cutree_wektor == 10))
      
      
      if ((ilosc_obserwacji_1/ilosc_obserwacji) > 0 & (ilosc_obserwacji_1/ilosc_obserwacji) <= input$procentZbioru)
      {
        a1<-TRUE
      }
      else
      {
        a1<-FALSE
      }
      
      
      if ((ilosc_obserwacji_2/ilosc_obserwacji) > 0 & (ilosc_obserwacji_2/ilosc_obserwacji) <= input$procentZbioru)
      {
        b2<-TRUE
      }
      else
      {
        b2<-FALSE
      }
      
      if ((ilosc_obserwacji_3/ilosc_obserwacji) > 0 & (ilosc_obserwacji_3/ilosc_obserwacji) <= input$procentZbioru)
      {
        c3<-TRUE
      }
      else
      {
        c3<-FALSE
      }
      
      if ((ilosc_obserwacji_4/ilosc_obserwacji) > 0 & (ilosc_obserwacji_4/ilosc_obserwacji) <= input$procentZbioru)
      {
        d4<-TRUE
      }
      else
      {
        d4<-FALSE
      }
      
      if ((ilosc_obserwacji_5/ilosc_obserwacji) > 0 & (ilosc_obserwacji_5/ilosc_obserwacji) <= input$procentZbioru)
      {
        e5<-TRUE
      }
      else
      {
        e5<-FALSE
      }
      
      if ((ilosc_obserwacji_6/ilosc_obserwacji) > 0 & (ilosc_obserwacji_6/ilosc_obserwacji) <= input$procentZbioru)
      {
        f6<-TRUE
      }
      else
      {
        f6<-FALSE
      }
      
      if ((ilosc_obserwacji_7/ilosc_obserwacji) > 0 & (ilosc_obserwacji_7/ilosc_obserwacji) <= input$procentZbioru)
      {
        g7<-TRUE
      }
      else
      {
        g7<-FALSE
      }
      
      if ((ilosc_obserwacji_8/ilosc_obserwacji) > 0 & (ilosc_obserwacji_8/ilosc_obserwacji) <= input$procentZbioru)
      {
        h8<-TRUE
      }
      else
      {
        h8<-FALSE
      }
      
      if ((ilosc_obserwacji_9/ilosc_obserwacji) > 0 & (ilosc_obserwacji_9/ilosc_obserwacji) <= input$procentZbioru)
      {
        i9<-TRUE
      }
      else
      {
        i9<-FALSE
      }
      
      if ((ilosc_obserwacji_10/ilosc_obserwacji) > 0 & (ilosc_obserwacji_10/ilosc_obserwacji) <= input$procentZbioru)
      {
        j10<-TRUE
      }
      else
      {
        j10<-FALSE
      }
      
      
###########################################################################################################3      
      
      #a1 b2 c3 d4
      
      if (a1 == TRUE & b2 == TRUE & c3 == TRUE & d4 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4)
      }
      
      #a1 b2 c3 e5
      
      else if (a1 == TRUE & b2 == TRUE & c3 == TRUE & e5 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5)
      }
      
      
      #a1 b2 c3 f6
      
      else if (a1 == TRUE & b2 == TRUE & c3 == TRUE & f6 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6)
      }
      
      
      #a1 b2 c3 g7
      
      else if (a1 == TRUE & b2 == TRUE & c3 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7)
      }
      
      
      #a1 b2 c3 h8
      
      else if (a1 == TRUE & b2 == TRUE & c3 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8)
      }
      
      
      #a1 b2 c3 i9
      
      else if (a1 == TRUE & b2 == TRUE & c3 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_9)
      }
      
      
      #a1 b2 c3 j10
      
      else if (a1 == TRUE & b2 == TRUE & c3 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_10)
      }
      
      #a1 b2 d4 e5
      
      else if (a1 == TRUE & b2 == TRUE & d4 == TRUE & e5 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5)
      }
      
      #a1 b2 d4 f6
      
      else if (a1 == TRUE & b2 == TRUE & d4 == TRUE & f6 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6)
      }
      
      #a1 b2 d4 g7
      
      else if (a1 == TRUE & b2 == TRUE & d4 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7)
      }
      
      #a1 b2 d4 h8
      
      else if (a1 == TRUE & b2 == TRUE & d4 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8)
      }
      
      #a1 b2 d4 i9
      
      else if (a1 == TRUE & b2 == TRUE & d4 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9)
      }
      
      #a1 b2 d4 j10
      
      else if (a1 == TRUE & b2 == TRUE & d4 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_10)
      }
      
      #a1 b2 e5 f6
      
      else if (a1 == TRUE & b2 == TRUE & e5 == TRUE & f6 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
      }
      
      #a1 b2 e5 g7
      
      else if (a1 == TRUE & b2 == TRUE & e5 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
      }
      
      #a1 b2 e5 h8
      
      else if (a1 == TRUE & b2 == TRUE & e5 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
      }
      
      #a1 b2 e5 i9
      
      else if (a1 == TRUE & b2 == TRUE & e5 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
      }
      
      #a1 b2 e5 j10
      
      else if (a1 == TRUE & b2 == TRUE & e5 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
      }
      
      #a1 b2 f6 g7
      
      else if (a1 == TRUE & b2 == TRUE & f6 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      #a1 b2 f6 h8
      
      else if (a1 == TRUE & b2 == TRUE & f6 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      #a1 b2 f6 i9
      
      else if (a1 == TRUE & b2 == TRUE & f6 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      #a1 b2 f6 j10
      
      else if (a1 == TRUE & b2 == TRUE & f6 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      #a1 b2 g7 h8
      
      else if (a1 == TRUE & b2 == TRUE & g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      #a1 b2 g7 i9
      
      else if (a1 == TRUE & b2 == TRUE & g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      #a1 b2 g7 j10
      
      else if (a1 == TRUE & b2 == TRUE & g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #a1 b2 h8 i9
      
      else if (a1 == TRUE & b2 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #a1 b2 h8 j10
      
      else if (a1 == TRUE & b2 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #a1 b2 i9 j10
      
      else if (a1 == TRUE & b2 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #a1 c3 d4 e5
      
      else if (a1 == TRUE & c3 == TRUE & d4 == TRUE & e5 == TRUE)
      {
        outliers_SMALLCLUSTER_1 <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5)
      }
      
      #a1 c3 d4 f6
      
      else if (a1 == TRUE & c3 == TRUE & d4 == TRUE & f6 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6)
      }
      
      #a1 c3 d4 g7
      
      else if (a1 == TRUE & c3 == TRUE & d4 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7)
      }
      
      #a1 c3 d4 h8
      
      else if (a1 == TRUE & c3 == TRUE & d4 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8)
      }
      
      #a1 c3 d4 i9
      
      else if (a1 == TRUE & c3 == TRUE & d4 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9)
      }
      
      #a1 c3 d4 j10
      
      else if (a1 == TRUE & c3 == TRUE & d4 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_10)
      }
      
      #a1 c3 e5 f6
      
      else if (a1 == TRUE & c3 == TRUE & e5 == TRUE & f6 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
      }
      
      #a1 c3 e5 g7
      
      else if (a1 == TRUE & c3 == TRUE & e5 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
      }
      
      #a1 c3 e5 h8
      
      else if (a1 == TRUE & c3 == TRUE & e5 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
      }
      
      #a1 c3 e5 i9
      
      else if (a1 == TRUE & c3 == TRUE & e5 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
      }
      
      #a1 c3 e5 j10
      
      else if (a1 == TRUE & c3 == TRUE & e5 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
      }
      
      #a1 c3 f6 g7
      
      else if (a1 == TRUE & c3 == TRUE & f6 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      #a1 c3 f6 h8
      
      else if (a1 == TRUE & c3 == TRUE & f6 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      #a1 c3 f6 i9
      
      else if (a1 == TRUE & c3 == TRUE & f6 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      #a1 c3 f6 j10
      
      else if (a1 == TRUE & c3 == TRUE & f6 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      #a1 c3 g7 h8
      
      else if (a1 == TRUE & c3 == TRUE & g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      #a1 c3 g7 i9
      
      else if (a1 == TRUE & c3 == TRUE & g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      #a1 c3 g7 j10
      
      else if (a1 == TRUE & c3 == TRUE & g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #a1 c3 h8 i9
      
      else if (a1 == TRUE & c3== TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #a1 c3 h8 j10
      
      else if (a1 == TRUE & c3 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #a1 c3 i9 j10
      
      else if (a1 == TRUE & c3 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #a1 d4 e5 f6
      
      else if (a1 == TRUE & d4 == TRUE & e5 == TRUE & f6 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
      }
      
      #a1 d4 e5 g7
      
      else if (a1 == TRUE & d4 == TRUE & e5 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
      }
      
      #a1 d4 e5 h8
      
      else if (a1 == TRUE & d4 == TRUE & e5 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
      }
      
      #a1 d4 e5 i9
      
      else if (a1 == TRUE & d4 == TRUE & e5 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
      }
      
      #a1 d4 e5 j10
      
      else if (a1 == TRUE & d4 == TRUE & e5 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
      }
      
      #a1 d4 f6 g7
      
      else if (a1 == TRUE & d4 == TRUE & f6 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      #a1 d4 f6 h8
      
      else if (a1 == TRUE & d4 == TRUE & f6 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      #a1 d4 f6 i9
      
      else if (a1 == TRUE & d4 == TRUE & f6 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      #a1 d4 f6 j10
      
      else if (a1 == TRUE & d4 == TRUE & f6 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      #a1 d4 g7 h8
      
      else if (a1 == TRUE & d4 == TRUE & g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      #a1 d4 g7 i9
      
      else if (a1 == TRUE & d4 == TRUE & g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      #a1 d4 g7 j10
      
      else if (a1 == TRUE & d4 == TRUE & g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #a1 d4 h8 i9
      
      else if (a1 == TRUE & d4 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #a1 d4 h8 j10
      
      else if (a1 == TRUE & d4 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #a1 d4 i9 j10
      
      else if (a1 == TRUE & d4 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #a1 e5 f6 g7
      
      else if (a1 == TRUE & e5 == TRUE & f6 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      #a1 e5 f6 h8
      
      else if (a1 == TRUE & e5 == TRUE & f6 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      #a1 e5 f6 i9
      
      else if (a1 == TRUE & e5 == TRUE & f6 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      #a1 e5 f6 j10
      
      else if (a1 == TRUE & e5 == TRUE & f6 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      #a1 e5 g7 h8
      
      else if (a1 == TRUE & e5 == TRUE & g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      #a1 e5 g7 i9
      
      else if (a1 == TRUE & e5 == TRUE & g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      #a1 e5 g7 j10
      
      else if (a1 == TRUE & e5 == TRUE & g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #a1 e5 h8 i9
      
      else if (a1 == TRUE & e5 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #a1 e5 h8 j10
      
      else if (a1 == TRUE & e5 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #a1 e5 i9 j10
      
      else if (a1 == TRUE & e5 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #a1 f6 g7 h8
      
      else if (a1 == TRUE & f6 == TRUE & g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      #a1 f6 g7 i9
      
      else if (a1 == TRUE & f6 == TRUE & g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      #a1 f6 g7 j10
      
      else if (a1 == TRUE & f6 == TRUE & g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #a1 f6 h8 i9
      
      else if (a1 == TRUE & f6 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #a1 f6 h8 j10
      
      else if (a1 == TRUE & f6 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #a1 f6 i9 j10
      
      else if (a1 == TRUE & f6 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #a1 g7 h8 i9
      
      else if (a1 == TRUE & g7 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #a1 g7 h8 j10
      
      else if (a1 == TRUE & g7 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #a1 g7 i9 j10
      
      else if (a1 == TRUE & g7 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #a1 h8 i9 j10
      
      else if (a1 == TRUE & h8 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #b2 c3 d4 e5
      
      else if (b2 == TRUE & c3 == TRUE & d4 == TRUE & e5 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5)
      }
      
      #b2 c3 d4 f6
      
      else if (b2 == TRUE & c3 == TRUE & d4 == TRUE & f6 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6)
      }
      
      #b2 c3 d4 g7
      
      else if (b2 == TRUE & c3 == TRUE & d4 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7)
      }
      
      #b2 c3 d4 h8
      
      else if (b2 == TRUE & c3 == TRUE & d4 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8)
      }
      
      #b2 c3 d4 i9
      
      else if (b2 == TRUE & c3 == TRUE & d4 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9)
      }
      
      #b2 c3 d4 j10
      
      else if (b2 == TRUE & c3 == TRUE & d4 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_10)
      }
      
      #b2 c3 e5 f6
      
      else if (b2 == TRUE & c3 == TRUE & e5 == TRUE & f6 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
      }
      
      #b2 c3 e5 g7
      
      else if (b2 == TRUE & c3 == TRUE & e5 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
      }
      
      #b2 c3 e5 h8
      
      else if (b2 == TRUE & c3 == TRUE & e5 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
      }
      
      #b2 c3 e5 i9
      
      else if (b2 == TRUE & c3 == TRUE & e5 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
      }
      
      #b2 c3 e5 j10
      
      else if (b2 == TRUE & c3 == TRUE & e5 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
      }
      
      #b2 c3 f6 g7
      
      else if (b2 == TRUE & c3 == TRUE & f6 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      #b2 c3 f6 h8
      
      else if (b2 == TRUE & c3 == TRUE & f6 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      #b2 c3 f6 i9
      
      else if (b2 == TRUE & c3 == TRUE & f6 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      #b2 c3 f6 j10
      
      else if (b2 == TRUE & c3 == TRUE & f6 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      #b2 c3 g7 h8
      
      else if (b2 == TRUE & c3 == TRUE & g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      #b2 c3 g7 i9
      
      else if (b2 == TRUE & c3 == TRUE & g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      #b2 c3 g7 j10
      
      else if (b2 == TRUE & c3 == TRUE & g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #b2 c3 h8 i9
      
      else if (b2 == TRUE & c3 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #b2 c3 h8 j10
      
      else if (b2 == TRUE & c3 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #b2 c3 i9 j10
      
      else if (b2 == TRUE & c3 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #b2 d4 e5 f6
      
      else if (b2 == TRUE & d4 == TRUE & e5 == TRUE & f6 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
      }
      
      #b2 d4 e5 g7
      
      else if (b2 == TRUE & d4 == TRUE & e5 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
      }
      
      #b2 d4 e5 h8
      
      else if (b2 == TRUE & d4 == TRUE & e5 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
      }
      
      #b2 d4 e5 i9
      
      else if (b2 == TRUE & d4 == TRUE & e5 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
      }
      
      #b2 d4 e5 j10
      
      else if (b2 == TRUE & d4 == TRUE & e5 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
      }
      
      #b2 d4 f6 g7
      
      else if (b2 == TRUE & d4 == TRUE & f6 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      #b2 d4 f6 h8
      
      else if (b2 == TRUE & d4 == TRUE & f6 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      #b2 d4 f6 i9
      
      else if (b2 == TRUE & d4 == TRUE & f6 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      #b2 d4 f6 j10
      
      else if (b2 == TRUE & d4 == TRUE & f6 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      #b2 d4 g7 h8
      
      else if (b2 == TRUE & d4 == TRUE & g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      #b2 d4 g7 i9
      
      else if (b2 == TRUE & d4 == TRUE & g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      #b2 d4 g7 j10
      
      else if (b2 == TRUE & d4 == TRUE & g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #b2 d4 h8 i9
      
      else if (b2 == TRUE & d4 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #b2 d4 h8 j10
      
      else if (b2 == TRUE & d4 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #b2 d4 i9 j10
      
      else if (b2 == TRUE & d4 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #b2 e5 f6 g7
      
      else if (b2 == TRUE & e5 == TRUE & f6 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      #b2 e5 f6 h8
      
      else if (b2 == TRUE & e5 == TRUE & f6 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      #b2 e5 f6 i9
      
      else if (b2 == TRUE & e5 == TRUE & f6 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      #b2 e5 f6 j10
      
      else if (b2 == TRUE & e5 == TRUE & f6 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      #b2 e5 g7 h8
      
      else if (b2 == TRUE & e5 == TRUE & g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      #b2 e5 g7 i9
      
      else if (b2 == TRUE & e5 == TRUE & g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      #b2 e5 g7 j10
      
      else if (b2 == TRUE & e5 == TRUE & g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #b2 e5 h8 i9
      
      else if (b2 == TRUE & e5 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #b2 e5 h8 j10
      
      else if (b2 == TRUE & e5 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #b2 e5 i9 j10
      
      else if (b2 == TRUE & e5 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #b2 f6 g7 h8
      
      else if (b2 == TRUE & f6 == TRUE & g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      #b2 f6 g7 i9
      
      else if (b2 == TRUE & f6 == TRUE & g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      #b2 f6 g7 j10
      
      else if (b2 == TRUE & f6 == TRUE & g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #b2 f6 h8 i9
      
      else if (b2 == TRUE & f6 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #b2 f6 h8 j10
      
      else if (b2 == TRUE & f6 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #b2 f6 i9 j10
      
      else if (b2 == TRUE & f6 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #b2 g7 h8 i9
      
      else if (b2 == TRUE & g7 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #b2 g7 h8 j10
      
      else if (b2 == TRUE & g7 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #b2 g7 i9 j10
      
      else if (b2 == TRUE & g7 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #b2 h8 i9 j10
      
      else if (b2 == TRUE & h8 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #c3 d4 e5 f6
      
      else if (c3 == TRUE & d4 == TRUE & e5 == TRUE & f6 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
      }
      
      #c3 d4 e5 g7
      
      else if (c3 == TRUE & d4 == TRUE & e5 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
      }
      
      #c3 d4 e5 h8
      
      else if (c3 == TRUE & d4 == TRUE & e5 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
      }
      
      #c3 d4 e5 i9
      
      else if (c3 == TRUE & d4 == TRUE & e5 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
      }
      
      #c3 d4 e5 j10
      
      else if (c3 == TRUE & d4 == TRUE & e5 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
      }
      
      #c3 d4 f6 g7
      
      else if (c3 == TRUE & d4 == TRUE & f6 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      #c3 d4 f6 h8
      
      else if (c3 == TRUE & d4 == TRUE & f6 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      #c3 d4 f6 i9
      
      else if (c3 == TRUE & d4 == TRUE & f6 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      #c3 d4 f6 j10
      
      else if (c3 == TRUE & d4 == TRUE & f6 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      #c3 d4 g7 h8
      
      else if (c3 == TRUE & d4 == TRUE & g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      #c3 d4 g7 i9
      
      else if (c3 == TRUE & d4 == TRUE & g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      #c3 d4 g7 j10
      
      else if (c3 == TRUE & d4 == TRUE & g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #c3 d4 h8 i9
      
      else if (c3 == TRUE & d4 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #c3 d4 h8 j10
      
      else if (c3 == TRUE & d4 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #c3 d4 i9 j10
      
      else if (c3 == TRUE & d4 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #c3 e5 f6 g7
      
      else if (c3 == TRUE & e5 == TRUE & f6 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      #c3 e5 f6 h8
      
      else if (c3 == TRUE & e5 == TRUE & f6 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      #c3 e5 f6 i9
      
      else if (c3 == TRUE & e5 == TRUE & f6 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      #c3 e5 f6 j10
      
      else if (c3 == TRUE & e5 == TRUE & f6 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      #c3 e5 g7 h8
      
      else if (c3 == TRUE & e5 == TRUE & g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      #c3 e5 g7 i9
      
      else if (c3 == TRUE & e5 == TRUE & g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      #c3 e5 g7 j10
      
      else if (c3 == TRUE & e5 == TRUE & g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #c3 e5 h8 i9
      
      else if (c3 == TRUE & e5 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #c3 e5 h8 j10
      
      else if (c3 == TRUE & e5 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #c3 e5 i9 j10
      
      else if (c3 == TRUE & e5 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #c3 f6 g7 h8
      
      else if (c3 == TRUE & f6 == TRUE & g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      #c3 f6 g7 i9
      
      else if (c3 == TRUE & f6 == TRUE & g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      #c3 f6 g7 j10
      
      else if (c3 == TRUE & f6 == TRUE & g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #c3 f6 h8 i9
      
      else if (c3 == TRUE & f6 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #c3 f6 h8 j10
      
      else if (c3 == TRUE & f6 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #c3 f6 i9 j10
      
      else if (c3 == TRUE & f6 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #c3 g7 h8 i9
      
      else if (c3 == TRUE & g7 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #c3 g7 h8 j10
      
      else if (c3 == TRUE & g7 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #c3 g7 i9 j10
      
      else if (c3 == TRUE & g7 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #c3 h8 i9 j10
      
      else if (c3 == TRUE & h8 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #d4 e5 f6 g7
      
      else if (d4 == TRUE & e5 == TRUE & f6 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      #d4 e5 f6 h8
      
      else if (d4 == TRUE & e5 == TRUE & f6 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      #d4 e5 f6 i9
      
      else if (d4 == TRUE & e5 == TRUE & f6 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      #d4 e5 f6 j10
      
      else if (d4 == TRUE & e5 == TRUE & f6 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      #d4 e5 g7 h8
      
      else if (d4 == TRUE & e5 == TRUE & g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      #d4 e5 g7 i9
      
      else if (d4 == TRUE & e5 == TRUE & g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      #d4 e5 g7 j10
      
      else if (d4 == TRUE & e5 == TRUE & g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #d4 e5 h8 i9
      
      else if (d4 == TRUE & e5 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #d4 e5 h8 j10
      
      else if (d4 == TRUE & e5 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #d4 e5 i9 j10
      
      else if (d4 == TRUE & e5 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #d4 f6 g7 h8
      
      else if (d4 == TRUE & f6 == TRUE & g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      #d4 f6 g7 i9
      
      else if (d4 == TRUE & f6 == TRUE & g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      #d4 f6 g7 j10
      
      else if (d4 == TRUE & f6 == TRUE & g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #d4 f6 h8 i9
      
      else if (d4 == TRUE & f6 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #d4 f6 h8 j10
      
      else if (d4 == TRUE & f6 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #d4 f6 i9 j10
      
      else if (d4 == TRUE & f6 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #d4 g7 h8 i9
      
      else if (d4 == TRUE & g7 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #d4 g7 h8 j10
      
      else if (d4 == TRUE & g7 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #d4 g7 i9 j10
      
      else if (d4 == TRUE & g7 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #d4 h8 i9 j10
      
      else if (d4 == TRUE & h8 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #e5 f6 g7 h8
      
      else if (e5 == TRUE & f6 == TRUE & g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      #e5 f6 g7 i9
      
      else if (e5 == TRUE & f6 == TRUE & g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      #e5 f6 g7 j10
      
      else if (e5 == TRUE & f6 == TRUE & g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #e5 f6 h8 i9
      
      else if (e5 == TRUE & f6 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #e5 f6 h8 j10
      
      else if (e5 == TRUE & f6 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #e5 f6 i9 j10
      
      else if (e5 == TRUE & f6 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #e5 g7 h8 i9
      
      else if (e5 == TRUE & g7 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #e5 g7 h8 j10
      
      else if (e5 == TRUE & g7 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #e5 g7 i9 j10
      
      else if (e5 == TRUE & g7 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #e5 h8 i9 j10
      
      else if (e5 == TRUE & h8 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #f6 g7 h8 i9
      
      else if (f6 == TRUE & g7 == TRUE & h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      #f6 g7 h8 j10
      
      else if (f6 == TRUE & g7 == TRUE & h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      #f6 g7 i9 j10
      
      else if (f6 == TRUE & g7 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #f6 h8 i9 j10
      
      else if (f6 == TRUE & h8 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      #g7 h8 i9 j10
      
      else if (g7 == TRUE & h8 == TRUE & i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      
      
      
      
      
      
      #a1 b2 c3
      
      
      else if (a1 == TRUE & b2 == TRUE & c3 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3)
      }
      
      
      
      #a1 b2 d4
      
      else if (a1 == TRUE & b2 == TRUE & d4 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4)
      }
      
      
      #a1 b2 e5
      
      else if (a1 == TRUE & b2 == TRUE & e5 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5)
      }
      
      
      #a1 b2 f6
      
      
      else if (a1 == TRUE & b2 == TRUE & f6 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6)
      }
      
      
      #a1 b2 g7
      
      else if (a1 == TRUE & b2 == TRUE & g7 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7)
      }
      
      
      #a1 b2 h8
      
      else if (a1 == TRUE & b2 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_8)
      }
      
      
      #a1 b2 i9
      
      else if (a1 == TRUE & b2 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_9)
      }
      
      
      #a1 b2 j10
      
      else if (a1 == TRUE & b2 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_10)
      }
      
      #a1 c3 d4
      
      else if (a1 == TRUE & c3 == TRUE & d4 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4)
      }
      
      
      #a1 c3 e5
      
      else if (a1 == TRUE & c3 == TRUE & e5 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5)
      }
      
      
      #a1 c3 f6
      
      else if (a1 == TRUE & c3 == TRUE & f6 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6)
      }
      
      
      #a1 c3 g7
      
      else if (a1 == TRUE & c3 == TRUE & g7 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7)
      }
      
      #a1 c3 h8
      
      else if (a1 == TRUE & c3 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8)
      }
      
      
      #a1 c3 i9
      
      else if (a1 == TRUE & c3 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_9)
      }
      
      
      #a1 c3 j10
      
      else if (a1 == TRUE & c3 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_10)
      }
      
      
      #a1 d4 e5
      
      else if (a1 == TRUE & d4 == TRUE & e5 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5)
      }
      
      
      #a1 d4 f6
      
      else if (a1 == TRUE & d4 == TRUE & f6 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6)
      }
      
      
      #a1 d4 g7
      
      else if (a1 == TRUE & d4 == TRUE & g7 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7)
      }
      
      
      #a1 d4 h8
      
      else if (a1 == TRUE & d4 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8)
      }
      
      
      #a1 d4 i9
      
      else if (a1 == TRUE & d4 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9)
      }
      
      
      #a1 d4 j10
      
      else if (a1 == TRUE & d4 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_10)
      }
      
      
      #a1 e5 f6
      
      else if (a1 == TRUE & e5 == TRUE & f6 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
      }
      
      
      #a1 e5 g7
      
      else if (a1 == TRUE & e5 == TRUE & g7 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
      }
      
      
      #a1 e5 h8
      
      else if (a1 == TRUE & e5 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
      }
      
      
      #a1 e5 i9
      
      else if (a1 == TRUE & e5 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
      }
      
      
      #a1 e5 j10
      
      else if (a1 == TRUE & e5 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
      }
      
      #a1 f6 g7
      
      else if (a1 == TRUE & f6 == TRUE & g7 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      
      #a1 f6 h8
      
      else if (a1 == TRUE & f6 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      
      #a1 f6 i9
      
      else if (a1 == TRUE & f6 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      
      #a1 f6 j10
      
      else if (a1 == TRUE & f6 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      
      #a1 g7 h8
      
      else if (a1 == TRUE & g7 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      
      #a1 g7 i9
      
      else if (a1 == TRUE & g7 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      
      #a1 g7 j10
      
      else if (a1 == TRUE & g7 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      #a1 h8 i9
      
      else if (a1 == TRUE & h8 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      
      #a1 h8 j10
      
      else if (a1 == TRUE & h8 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      
      #a1 i9 j10
      
      else if (a1 == TRUE & i9 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      
      #b2 c3 d4
      
      else if (b2 == TRUE & c3 == TRUE & d4 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4)
      }
      
      
      #b2 c3 e5
      
      else if (b2 == TRUE & c3 == TRUE & e5 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5)
      }
      
      
      #b2 c3 f6
      
      else if (b2 == TRUE & c3 == TRUE & f6 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6)
      }
      
      
      #b2 c3 g7
      
      else if (b2 == TRUE & c3 == TRUE & g7 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7)
      }
      
      
      #b2 c3 h8
      
      else if (b2 == TRUE & c3 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8)
      }
      
      
      #b2 c3 i9
      
      else if (b2 == TRUE & c3 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_9)
      }
      
      
      #b2 c3 j10
      
      else if (b2 == TRUE & c3 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_10)
      }
      
      
      #b2 d4 e5
      
      else if (b2 == TRUE & d4 == TRUE & e5 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5)
      }
      
      
      #b2 d4 f6
      
      else if (b2 == TRUE & d4 == TRUE & f6 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6)
      }
      
      
      #b2 d4 g7
      
      else if (b2 == TRUE & d4 == TRUE & g7 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7)
      }
      
      
      #b2 d4 h8
      
      else if (b2 == TRUE & d4 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8)
      }
      
      
      #b2 d4 i9
      
      else if (b2 == TRUE & d4 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9)
      }
      
      
      #b2 d4 j10
      
      else if (b2 == TRUE & d4 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_10)
      }
      
      
      #b2 e5 f6
      
      else if (b2 == TRUE & e5 == TRUE & f6 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
      }
      
      
      #b2 e5 g7
      
      else if (b2 == TRUE & e5 == TRUE & g7 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
      }
      
      
      #b2 e5 h8
      
      else if (b2 == TRUE & e5 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
      }
      
      
      #b2 e5 i9
      
      else if (b2 == TRUE & e5 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
      }
      
      
      #b2 e5 j10
      
      else if (b2 == TRUE & e5 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
      }
      
      
      #b2 f6 g7
      
      else if (b2 == TRUE & f6 == TRUE & g7 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      
      #b2 f6 h8
      
      else if (b2 == TRUE & f6 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      
      #b2 f6 i9
      
      else if (b2 == TRUE & f6 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      
      #b2 f6 j10
      
      else if (b2 == TRUE & f6 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      
      #b2 g7 h8
      
      else if (b2 == TRUE & g7 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      
      #b2 g7 i9
      
      else if (b2 == TRUE & g7 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      
      #b2 g7 j10
      
      else if (b2 == TRUE & g7 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      
      #b2 h8 i9
      
      else if (b2 == TRUE & h8 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      
      #b2 h8 j10
      
      else if (b2 == TRUE & h8 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      
      #b2 i9 j10
      
      else if (b2 == TRUE & i9 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      
      #c3 d4 e5
      
      else if (c3 == TRUE & d4 == TRUE & e5 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5)
      }
      
      
      #c3 d4 f6
      
      else if (c3 == TRUE & d4 == TRUE & f6 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6)
      }
      
      
      #c3 d4 g7
      
      else if (c3 == TRUE & d4 == TRUE & g7 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7)
      }
      
      
      #c3 d4 h8
      
      else if (c3 == TRUE & d4 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8)
      }
      
      
      #c3 d4 i9
      
      else if (c3 == TRUE & d4 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9)
      }
      
      
      #c3 d4 j10
      
      else if (c3 == TRUE & d4 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_10)
      }
      
      
      #c3 e5 f6
      
      else if (c3 == TRUE & e5 == TRUE & f6 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
      }
      
      
      #c3 e5 g7
      
      else if (c3 == TRUE & e5 == TRUE & g7 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
      }
      
      
      #c3 e5 h8
      
      else if (c3 == TRUE & e5 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
      }
      
      
      #c3 e5 i9
      
      else if (c3 == TRUE & e5 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
      }
      
      
      #c3 e5 j10
      
      else if (c3 == TRUE & e5 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
      }
      
      
      #c3 f6 g7
      
      else if (c3 == TRUE & f6 == TRUE & g7 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      
      #c3 f6 h8
      
      else if (c3 == TRUE & f6 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      
      #c3 f6 i9
      
      else if (c3 == TRUE & f6 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      
      #c3 f6 j10
      
      else if (c3 == TRUE & f6 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      
      #c3 g7 h8
      
      else if (c3 == TRUE & g7 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      
      #c3 g7 i9
      
      else if (c3 == TRUE & g7 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      
      #c3 g7 j10
      
      else if (c3 == TRUE & g7 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      
      #c3 h8 i9
      
      else if (c3 == TRUE & h8 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      
      #c3 h8 j10
      
      else if (c3 == TRUE & h8 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      
      #c3 i9 j10
      
      else if (c3 == TRUE & i9 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      
      #d4 e5 f6
      
      else if (d4 == TRUE & e5 == TRUE & f6 == TRUE )
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
      }
      
      
      #d4 e5 g7
      
      else if (d4 == TRUE & e5 == TRUE & g7 == TRUE )
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
      }
      
      
      #d4 e5 h8
      
      else if (d4 == TRUE & e5 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
      }
      
      
      #d4 e5 i9
      
      else if (d4 == TRUE & e5 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
      }
      
      
      #d4 e5 j10
      
      else if (d4 == TRUE & e5 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
      }
      
      
      #d4 f6 g7
      
      else if (d4 == TRUE & f6 == TRUE & g7 == TRUE )
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      
      #d4 f6 h8
      
      else if (d4 == TRUE & f6 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      
      #d4 f6 i9
      
      else if (d4 == TRUE & f6 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      
      #d4 f6 j10
      
      else if (d4 == TRUE & f6 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      
      #d4 g7 h8
      
      else if (d4 == TRUE & g7 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      
      #d4 g7 i9
      
      else if (d4 == TRUE & g7 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      
      #d4 g7 j10
      
      else if (d4 == TRUE & g7 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      
      #d4 h8 i9
      
      else if (d4 == TRUE & h8 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      
      #d4 h8 j10
      
      else if (d4 == TRUE & h8 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      
      #d4 i9 j10
      
      else if (d4 == TRUE & i9 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      
      #e5 f6 g7
      
      else if (e5 == TRUE & f6 == TRUE & g7 == TRUE )
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      
      #e5 f6 h8
      
      else if (e5 == TRUE & f6 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      
      #e5 f6 i9
      
      else if (e5 == TRUE & f6 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      
      #e5 f6 j10
      
      else if (e5 == TRUE & f6 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      
      #e5 g7 h8
      
      else if (e5 == TRUE & g7 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      
      #e5 g7 i9
      
      else if (e5 == TRUE & g7 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      
      #e5 g7 j10
      
      else if (e5 == TRUE & g7 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      
      #e5 h8 i9
      
      else if (e5 == TRUE & h8 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      
      #e5 h8 j10
      
      else if (e5 == TRUE & h8 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      
      #e5 i9 j10
      
      else if (e5 == TRUE & i9 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      
      #f6 g7 h8
      
      else if (f6 == TRUE & g7 == TRUE & h8 == TRUE )
      {
        outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      
      #f6 g7 i9
      
      else if (f6 == TRUE & g7 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      
      #f6 g7 j10
      
      else if (f6 == TRUE & g7 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      
      #f6 h8 i9
      
      else if (f6 == TRUE & h8 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      
      #f6 h8 j10
      
      else if (f6 == TRUE & h8 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      
      #f6 i9 j10
      
      else if (f6 == TRUE & i9 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      
      #g7 h8 i9
      
      else if (g7 == TRUE & h8 == TRUE & i9 == TRUE )
      {
        outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      
      #g7 h8 j10
      
      else if (g7 == TRUE & h8 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      
      #g7 i9 j10
      
      else if (g7 == TRUE & i9 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      
      #h8 i9 j10
      
      else if (h8 == TRUE & i9 == TRUE & j10 == TRUE )
      {
        outliers_SMALLCLUSTER_8  <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      
      
      
      
      
      
      #a1 b2
      
      
      else if (a1 == TRUE & b2 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2)
      }
      
      
      #a1 c3
      
      
      else if (a1 == TRUE & c3 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3)
      }
      
      
      
      #a1 d4
      
      else if (a1 == TRUE & d4 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4)
      }
      
      
      #a1 e5
      
      else if (a1 == TRUE & e5 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5)
      }
      
      
      #a1 f6
      
      else if (a1 == TRUE & f6 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6)
      }
      
      
      #a1 g7
      
      else if (a1 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_7)
      }
      
      
      #a1 h8
      
      else if (a1 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_8)
      }
      
      
      #a1 i9
      
      else if (a1 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_9)
      }
      
      
      #a1 j10
      
      else if (a1 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_10)
      }
      
      
      #b2 c3
      
      else if (b2 == TRUE & c3 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3)
      }
      
      
      #b2 d4
      
      else if (b2 == TRUE & d4 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4)
      }
      
      
      #b2 e5
      
      else if (b2 == TRUE & e5 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5)
      }
      
      
      #b2 f6
      
      else if (b2 == TRUE & f6 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6)
      }
      
      
      #b2 g7
      
      else if (b2 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7)
      }
      
      
      #b2 h8
      
      else if (b2 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_8)
      }
      
      
      #b2 i9
      
      else if (b2 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_9)
      }
      
      
      #b2 j10
      
      else if (b2 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_10)
      }
      
      
      #c3 d4
      
      else if (c3 == TRUE & d4 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4)
      }
      
      
      #c3 e5
      
      else if (c3 == TRUE & e5 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5)
      }
      
      
      #c3 f6
      
      else if (c3 == TRUE & f6 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6)
      }
      
      
      #c3 g7
      
      else if (c3 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7)
      }
      
      
      #c3 h8
      
      else if (c3 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8)
      }
      
      
      #c3 i9
      
      else if (c3 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_9)
      }
      
      
      #c3 j10
      
      else if (c3 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_10)
      }
      
      
      #d4 e5
      
      else if (d4 == TRUE & e5 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5)
      }
      
      
      #d4 f6
      
      else if (d4 == TRUE & f6 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6)
      }
      
      
      #d4 g7
      
      else if (d4 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7)
      }
      
      
      #d4 h8
      
      else if (d4 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8)
      }
      
      
      #d4 i9
      
      else if (d4 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9)
      }
      
      
      #d4 j10
      
      else if (d4 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_10)
      }
      
      
      #e5 f6
      
      else if (e5 == TRUE & f6 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
      }
      
      
      #e5 g7
      
      else if (e5 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
      }
      
      
      #e5 h8
      
      else if (e5 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
      }
      
      
      #e5 i9
      
      else if (e5 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
      }
      
      
      #e5 j10
      
      else if (e5 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
      }
      
      
      #f6 g7
      
      else if (f6 == TRUE & g7 == TRUE)
      {
        outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
      }
      
      
      #f6 h8
      
      else if (f6 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
      }
      
      
      #f6 i9
      
      else if (f6 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
      }
      
      
      #f6 j10
      
      else if (f6 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
      }
      
      
      #g7 h8
      
      else if (g7 == TRUE & h8 == TRUE)
      {
        outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
      }
      
      
      #g7 i9
      
      else if (g7 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
      }
      
      
      #g7 j10
      
      else if (g7 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
      }
      
      
      #h8 i9
      
      else if (h8 == TRUE & i9 == TRUE)
      {
        outliers_SMALLCLUSTER_8  <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
      }
      
      
      #h8 j10
      
      else if (h8 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_8  <-which(cutree_wektor == 8)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
      }
      
      
      #i9 j10
      
      else if (i9 == TRUE & j10 == TRUE)
      {
        outliers_SMALLCLUSTER_9  <-which(cutree_wektor == 9)
        outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
      }
      
      
      #a1
      
      else if (a1 == TRUE)
      {
        outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1)
      }
      
      #b2
      
      else if (b2 == TRUE)
      {
        outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2)
      }
      
      #c3
      
      else if (c3 == TRUE)
      {
        outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3)
      }
      
      #d4
      
      else if (d4 == TRUE)
      {
        outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4)
      }
      
      #e5
      
      else if (e5 == TRUE)
      {
        outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5)
      }
      
      #f6
      
      else if (f6 == TRUE)
      {
        outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6)
      }
      
      #g7
      
      else if (g7 == TRUE)
      {
        outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7)
      }
      
      #h8
      
      else if (h8 == TRUE)
      {
        outliers_SMALLCLUSTER_8  <-which(cutree_wektor == 8)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_8)
      }
      
      #i9
      
      else if (i9 == TRUE)
      {
        outliers_SMALLCLUSTER_9  <-which(cutree_wektor == 9)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_9)
      }
      
      
      #j10
      
      else if (j10 == TRUE)
      {
        outliers_SMALLCLUSTER_10  <-which(cutree_wektor == 10)
        
        outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_10)
      }
      
        else
        {
          outliers_SMALLCLUSTER <- "t"
        }
        
       
###############################################################################

     # outliers_SMALLCLUSTER  <-which(cutree_wektor == 10)
    #  outliers_SMALLCLUSTER  <-which(cutree_wektor == 11)
     # outliers_SMALLCLUSTER  <-which(cutree_wektor == 20)
   
  if ( outliers_SMALLCLUSTER == "t")
  {    wyniki[12,3] <- "not small clusters"}
else
       {wyniki[12,3] <- toString(outliers_SMALLCLUSTER)}
                  #toString(outliers_SMALLCLUSTER)
    
      
      #print(outliers_LOF)
      #print(gower_mat1 [outliers_LOF,])
      
      #Usuni??cie i ponowne grupowanie
      
      # usuni??cie outliers_LOF
      
        if (outliers_SMALLCLUSTER == "t")
        { data_inf4 <- j }
     else
     {
    x <- outliers_SMALLCLUSTER
      data_inf4 <- j[-x, ]
   }   #data_inf1
      
      # dendrogram AHC klastrowanie hierarchiczne po usuni??ciu odchyle?? - outliers_LOF
      gower_mat1 <- gower.dist ( data_inf4 ,  rngs = NA ,  KR.corr = TRUE ,  var.weights  =  NULL )
      gower_mat1[is.nan(gower_mat1)] <- 0
      #gower_mat -macierz danych (odleg??o??ci gowera, ale teraz tak wygl??daj?? moje dane, ju?? nie s?? jako??ciowe)
      #gower_mat
      #gower_mat1
      gower_mat <-as.dist(gower_mat1) 
      #gower_mat 
      #dist.metoda oblicza dist_gower_mat
      #dist_gower_mat<-dist(gower_mat, method = "euclidean", diag = TRUE, upper = FALSE, p = 2)  # to mo??na zmienia??!!!!!
      #"euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
      #dist_gower_mat
      
      
      clusters <- hclust(gower_mat,method = input$metoda) #grupowanie najdalszych s??siad??w  # to mo??na zmienia??
      #"ward.D", "ward.D2", "single", "complete", "average","mcquitty", "median", "centroid"
      #saveRDS(clusters, file = "clustersLOF.rds")
      wyniki[11,3] <- input$metoda
      
      #fviz_dend(clusters, k = 4, # Cut in two groups  !!!!!!! ustawi?? cutree_k <- 2
      #         cex = 0.5, # label size
      #        k_colors = c("#2E9FDF", "#FC4E07", "#07fc4e","#E7B800", "#00AFBB"),
      #       color_labels_by_k = TRUE, # color labels by groups
      #      rect = TRUE # Add rectangle around groups 
      #)
      # plot(clusters)
      
      #Cophenetic Distances for a Hierarchical Clustering
      #require(graphics)
      d1 <- gower_mat
      hc <- clusters
      d2 <- cophenetic(hc)
      #cor(d1, d2) 
      wyniki[10,3] <- cor(d1, d2)
      
      # Ocena jako??ci grupowania - cluster validity (prawdziwo????, wiarygodno????, jako???? grupowania)
      
      # na ile skupie?? podzielono, ??eby zbada?? jako????
      # !!!!! cutree_k_LOF <- 2
      
      #cutree(clusters, k = cutree_k) - wektor liczb ca??kowitych dodatnich informuj??cy 
      #o przynale??no??ci obiekt??w do klasy
      cutree_wektor <- cutree(clusters, k = cutree_k_SMALLCLUSTER)
      # cutree_wektor
      #1 Oparte o odleg??o???? mi??dzy jednostakami w skupieniu i mi??dzy skupieniami 
      
      #silhouette z wykresem
      #wska??nik sylwetkowy 
      #Kaufman, L., Rousseeuw, P.J. (1990), Finding groups in data: an introduction to cluster analysis,
      #Wiley, New York, pp. 83-88. ISBN: 978-0-471-73578-6.
      
      # si3 <- silhouette(cutree_wektor, gower_mat) # im wiekszy tym lepiej
      #  plot(si3, nmax = 80, cex.names = 0.5)
      
      #silhouette 2 tylko warto???? # im wiekszy tym lepiej
      #wska??nik sylwetkowy
      #Kaufman, L., Rousseeuw, P.J. (1990), Finding groups in data: an introduction to cluster analysis,
      #Wiley, New York, pp. 83-88. ISBN: 978-0-471-73578-6.
      
      #icq <- index.S(gower_mat,cutree_wektor)
      # print(icq)
      #wyniki[1,3]<-icq
      
      si3 <- silhouette(cutree_wektor, gower_mat) # im wiekszy tym lepiej
      icq1 <- mean(si3[,"sil_width"])
      wyniki[1,3]<-icq1
      
      
      #Indeks Dunna. Ten wska??nik jako??ci klastra zosta?? zaproponowany przez Dunna (1973).
      #Wska??nik Dunna to stosunek najmniejszej odleg??o??ci mi??dzy obserwacjami spoza tej samej grupy
      #do najwi??kszej odleg??o??ci wewn??trz gromady. Indeks Dunna ma warto???? od zera do niesko??czono??ci 
      #i nale??y go zmaksymalizowa??. Aby uzyska?? szczeg????owe informacje, patrz winieta na opakowaniu.
      #dunn(gower_mat, cutree_wektor)  # im wi??kszy tym lepiej
      wyniki[2,3]<-dunn(gower_mat, cutree_wektor)
      #2 Oparte o rozproszenie jednostek w skupieniu i odleg??o??ci mi??dzy skupieniami
      #??clusterSim
      
      # Davies-Bouldin???s (DB???s) Index. Czym mniejszy tym lepiej
      #print(index.DB(gower_mat, cutree_wektor, centrotypes="centroids", p = 2, q = 2))
      index_DB <- index.DB(gower_mat, cutree_wektor, centrotypes="centroids", p = 2, q = 2)
      # index_DB[1]
      wyniki[3,3]<-index_DB[1]
      #3 Oparte na sumie kwadrat??w wewn??trz skupie?? i mi??dzy skupieniami
      
      #Indeks Pseudo F. Pseudo-statystyka F opisuje stosunek wariancji mi??dzy klastrami
      #do wariancji skupie?? (Calinski i Harabasz, 1974):
      icq_index_G1 <- index.G1(gower_mat1, cutree_wektor, d = NULL, centrotypes = "centroids")
      #print(icq_index_G1) # Du??e warto??ci Pseudo F wskazuj?? na zwarte i oddzielone klastry.
      wyniki[4,3]<-icq_index_G1
      #Gordon, AD (1999), Classification , Chapman & Hall / CRC, Londyn, str. 62. ISBN 9781584880134.
      #Oblicza wska??nik jako??ci wewn??trznego klastra G3 #Huberta i Levine'a
      icq_index_G3 <- index.G3(gower_mat, cutree_wektor)
      # print(icq_index_G3) # im mniejszy to lepiej je??li to #Huberta i Levine'a
      wyniki[5,3]<-icq_index_G3
      #4 Inne - stosunek r??znicy do sumy liczby par odleg??o??ci zgodnych i liczba par odleg??o??ci niezgodnych
      #Adaptacja G2 Baker i Hubert statystyki gamma Goodmana-Kruskala 
      icq_index_G2 <- index.G2(gower_mat, cutree_wektor)
      wyniki[6,3]<-icq_index_G2
      
    }
    
  
       
       if ( ((as.numeric(wyniki[1,2])) < (as.numeric((wyniki[1,3])))) &
            ((as.numeric(wyniki[2,2])) <= (as.numeric((wyniki[2,3])))) & 
            ((as.numeric(wyniki[3,2])) > (as.numeric((wyniki[3,3])))) & 
            ((as.numeric(wyniki[4,2])) < (as.numeric((wyniki[4,3])))) & 
            ((as.numeric(wyniki[5,2])) >= (as.numeric((wyniki[5,3])))) & 
            ((as.numeric(wyniki[6,2])) <= (as.numeric((wyniki[6,3])))) & 
           ((as.numeric(wyniki[10,2])) < (as.numeric((wyniki[10,3])))) )
  {
      tableHTML(wyniki, caption = 'Analiza wpływu reguł nietypowych na jakość grupowania:', footer = '*Spróbuj dobrać pola wyboru i paski wyszukiwania tak, aby niebieski nagłówek tabeli zmienił się na zielony',  
                           collapse = 'separate_shiny', spacing = '6px 5px', escape = TRUE, rownames = FALSE) %>%
        add_css_row(css = list('background-color', '#e6f0ff'),
                    rows = odd(2:13)) %>%
         add_css_row(css = list('background-color', '#ffffff'),
                                  rows = even(2:13))  %>%
        
        add_css_row(css = list('background-color', '#90EE90'),  ##C6D9F6
                    rows = odd(1:1))
      }
    
    else 
    {
      tableHTML(wyniki, caption = 'Analiza wpływu reguł nietypowych na jakość grupowania:', footer = '*Spróbuj dobrać pola wyboru i paski wyszukiwania tak, aby niebieski nagłówek tabeli zmienił się na zielony',  
                collapse = 'separate_shiny', spacing = '6px 5px', escape = TRUE, rownames = FALSE) %>%
        add_css_row(css = list('background-color', '#e6f0ff'),
                    rows = odd(2:13)) %>%
        add_css_row(css = list('background-color', '#ffffff'),
                    rows = even(2:13))  %>%
        add_css_row(css = list('background-color', '#C6D9F6'),
                    rows = odd(1:1))
    }  
      
                             
  })
  
  
  output$wyjscieDendrogram <- renderPlot({
  
    observeEvent(input$show, {
      showNotification("Message text",
                       action = session$reload(), "Reload page")
      
    })
    
    fn <- "wczytanyZbior.csv"
    #Check its existence
    if (!file.exists(fn)) 
     return(NULL)
    qoute2 <- readRDS(file = "quote2.rds")
    my_data <- fread ("wczytanyZbior.csv", sep=',', header = TRUE, stringsAsFactors = FALSE, quote = qoute2)
    my_data[my_data=="NA"] <- NA
   # my_data <- output$contents
    
   # gower_mat1 <- gower.dist(my_data)
    gower_mat1 <- gower.dist ( my_data ,  rngs = NA ,  KR.corr = TRUE ,  var.weights  =  NULL )
    gower_mat1[is.nan(gower_mat1)] <- 0
    gower_mat <-as.dist(gower_mat1) 
    clusters <- hclust(gower_mat, method = input$metoda)
    
    J<-readRDS(file = "file1.rds")
    
    collection_30 = unlist(gregexpr(pattern =",",toString(J)))
    collection_40<-str_sub(string=toString(J), start = 1, end = collection_30[1]-1)
    
   # plot(clusters, hang = -1, main = paste("Dendrogram prezentujący podział zbioru danych na grupy przed usunięciem reguł nietypowych, dataset = ",collection_40))
    
    
  plot(fviz_dend(clusters, k = input$liczbaGrup[1],font.main = 20, main = paste("Dendrogram przed usunięciem reguł nietypowych, dataset = ",collection_40), # Cut in two groups  !!!!!!! ustawić cutree_k <- 2
                   xlab = paste("Metoda łączenia grup:",input$metoda,"|", "Miara odległości między regułami: Gower " ),cex = 0.5,  # label size
                 k_colors = c("#2E9FDF", "#FC4E07", "#07fc4e", "#3339FF", "#00AFBB", "#FF33F0", "#E7B800", "#C0C0C0","#800080", "#008080", "#808000", "#2F4F4F", "#8B0000", "#FF1493","#BDB76B", "#FFD700", "#483D8B", "#00FF7F","#00CED1","#0000CD" ), 
           color_labels_by_k = TRUE, # color labels by groups
          rect = FALSE # Add rectangle around groups 
    ))
    
    
    
    
    output$plot2 <- renderPlot({
      
      
      if (input$algorytm == "LOF")
        
      {
        #clustersLOF <- readRDS(file = "clustersLOF.rds")
        
        qoute2 <- readRDS(file = "quote2.rds")
        
        g <- fread ("wczytanyZbior.csv", sep=',', header = TRUE, stringsAsFactors = FALSE, quote = qoute2)
        g[g=="NA"] <- NA
        #g[g=="0"] <- 0
        
        #g<-as.data.frame(g)
        #g<-as.numeric(g)
        
        RULES1<-nrow(g)
        howmany <-(input$procentRegulNietypowych[1]/100)*RULES1
        howmany<-ceiling(howmany)
       # wyniki[9,2]<-howmany
      #  wyniki[9,3]<-howmany
        set_k <- input$kSasiadow[1] 
       # wyniki[8,2]<-set_k
      #  wyniki[8,3]<-set_k
        
        #LOF k = 3 [Breunig i in., 2000] ocena stopnia oddalenia danej obserwacji 
        #od pozosta??ych przy uwzgl??dnieniu zag??szczenia obiekt??w z k-elementowego s??siedztwa 
        #d<- as.data.frame(d, stringsAsFactors = FALSE)
        outlier.scores <- lofactor(gower_mat1, k = set_k)
        outliers_LOF <- order(outlier.scores, decreasing = T) [1:howmany]
        #wyniki[12,3] <- toString(outliers_LOF)
        #print(outliers_LOF)
        #print(gower_mat1 [outliers_LOF,])
        
        #Usuni??cie i ponowne grupowanie
        
        # usuni??cie outliers_LOF
        p <- outliers_LOF
        data_inf1 <- g[-p, ]
        
        
        
        gower_mat1 <- gower.dist (data_inf1 ,  rngs = NA ,  KR.corr = TRUE ,  var.weights  =  NULL)
        gower_mat1[is.nan(gower_mat1)] <- 0
        gower_mat <-as.dist(gower_mat1) 
        clusters <- hclust(gower_mat, method = input$metoda)   
       
       # plot(clusters, hang = -1, main = "Dendrogram prezentujący podział zbioru danych na grupy po usunięciu reguł nietypowych - metoda LOF")
        
        plot(fviz_dend(clusters, k = input$liczbaGrup[1],font.main = 20, main = paste("Dendrogram po usunięciu reguł nietypowych - metoda LOF, dataset = ",collection_40), # Cut in two groups  !!!!!!! ustawić cutree_k <- 2
                       xlab = paste("Metoda łączenia grup:",input$metoda,"|", "Miara odległości między regułami: Gower " ),cex = 0.5,   # label size
                       k_colors = c("#2E9FDF", "#FC4E07", "#07fc4e", "#3339FF", "#00AFBB", "#FF33F0", "#E7B800", "#C0C0C0","#800080", "#008080", "#808000", "#2F4F4F", "#8B0000", "#FF1493","#BDB76B", "#FFD700", "#483D8B", "#00FF7F","#00CED1","#0000CD" ),
                       color_labels_by_k = TRUE, # color labels by groups
                       rect = FALSE # Add rectangle around groups 
        ))
        
      }
      
      
      
      if (input$algorytm == "COF")
        
      {
        
        qoute2 <- readRDS(file = "quote2.rds")
        #   d <- fread ("wczytanyZbior.csv", sep=',', header = TRUE)
        #  gower_mat1 <- gower.dist (d[,])
        
        #Regu??y nietypowe
        f <- fread ("wczytanyZbior.csv", sep=',', header = TRUE,stringsAsFactors = FALSE, quote = qoute2)
        f[f=="NA"] <- NA
        #f[f=="0"] <- 0
        RULES1<-nrow(f)
        howmany <-(input$procentRegulNietypowych[1]/100)*RULES1
        howmany<-ceiling(howmany)
       # wyniki[9,2]<-howmany
      #  wyniki[9,3]<-howmany
        set_k <- input$kSasiadow[1] 
       # wyniki[8,2]<-set_k
      #  wyniki[8,3]<-set_k
        
        
        #COF Funkcja do obliczania opartego na ????czno??ci wsp????czynnika odstaj??cego jako wyniku 
        #odstaj??cego dla obserwacji. Sugerowana przez Tang, J., Chen, Z., Fu, A. W. C. i Cheung, D. W. (2002)
        outlier.scores <- COF(gower_mat1, k = set_k)
        outliers_COF <- order(outlier.scores, decreasing = T) [1:howmany]
       # wyniki[12,3] <- toString(outliers_COF)
        #print(outliers_COF)
        #print(gower_mat1 [outliers_COF,])
        
        
        #Usuni??cie i ponowne grupowanie
        
        # usuni??cie outliers_COF
        q <- outliers_COF
        data_inf2 <- f[-q, ]
        #data_inf2
        
        
        
        # dendrogram AHC klastrowanie hierarchiczne po usuni??ciu odchyle?? - outliers_COF
        gower_mat1 <- gower.dist ( data_inf2 ,  rngs = NA ,  KR.corr = TRUE ,  var.weights  =  NULL )
        gower_mat1[is.nan(gower_mat1)] <- 0
        #gower_mat -macierz danych (odleg??o??ci gowera, ale teraz tak wygl??daj?? moje dane, ju?? nie s?? jako??ciowe)
        #gower_mat
        gower_mat <-as.dist(gower_mat1) 
        #rysowanie macierzy odleglosci!
       # fviz_dist(gower_mat, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
        #dist.metoda oblicza dist_gower_mat
        #dist_gower_mat<-dist(gower_mat, method = "euclidean", diag = TRUE, upper = FALSE, p = 2)  #to mo??na zmieni??!!!!
        #"euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
        #dist_gower_mat
        
        
        clusters <- hclust(gower_mat,method = input$metoda) #grupowanie najdalszych s??siad??w   #to mozna zmieni??
        #saveRDS(clusters, file = "clustersCOF.rds")
        
        
        
        
        #clustersCOF <- readRDS(file = "clustersCOF.rds")
       # plot(clusters, hang = -1, main = "Dendrogram prezentujący podział zbioru danych na grupy po usunięciu reguł nietypowych - metoda COF")
        
        plot(fviz_dend(clusters, k = input$liczbaGrup[1],font.main = 20, main = paste("Dendrogram po usunięciu reguł nietypowych - metoda COF, dataset = ",collection_40), # Cut in two groups  !!!!!!! ustawić cutree_k <- 2
                       xlab = paste("Metoda łączenia grup:",input$metoda,"|", "Miara odległości między regułami: Gower " ),cex = 0.5,   # label size
                       k_colors = c("#2E9FDF", "#FC4E07", "#07fc4e", "#3339FF", "#00AFBB", "#FF33F0", "#E7B800", "#C0C0C0","#800080", "#008080", "#808000", "#2F4F4F", "#8B0000", "#FF1493","#BDB76B", "#FFD700", "#483D8B", "#00FF7F","#00CED1","#0000CD" ),
                       color_labels_by_k = TRUE, # color labels by groups
                       rect = FALSE # Add rectangle around groups 
        ))
        
      }
      
      if (input$algorytm == "KMEANS")
        
      {
        
        qoute2 <- readRDS(file = "quote2.rds")
        # Reguly nietypowe
        h <- fread ("wczytanyZbior.csv", sep=',', header = TRUE,stringsAsFactors = FALSE, quote = qoute2)
        h[h=="NA"] <- NA
        #h[h=="0"] <- 0
        RULES1<-nrow(h)
        howmany <-(input$procentRegulNietypowych[1]/100)*RULES1
        howmany<-ceiling(howmany)
       # wyniki[9,2]<-howmany
      #  wyniki[9,3]<-howmany
        #howmany
      
        set_k <- input$setSeed[1] 
       # wyniki[8,2]<-set_k
      #  wyniki[8,3]<-set_k
        #Analiza skupien metoda k-means(k-srednich), k = DECISION_VALUES1
        #metoda zak??ada, ??e obserwacj?? odstaj??c?? jest obiekt niedaj??cy si?? w????czy?? do ??adnej grupy
        #b??d?? na przyk??ad jest z ma??o licznej grupy
        #DECISION_VALUES1
        RNGkind(sample.kind = "Rounding")
        set.seed(set_k)  #7 #8
        #gower_mat1
        DECISION_VALUES1<-readRDS(file = "RULES1.rds")
        kmeans.result <- kmeans(gower_mat1 , centers = DECISION_VALUES1)
        centers <- kmeans.result$centers[kmeans.result$cluster, ]
        distances <- sqrt(rowSums((as.numeric(unlist(gower_mat1)) - centers)^2))
        outliers_kmeans <- order( distances, decreasing = T ) [1:howmany]
       # wyniki[12,3] <- toString(outliers_kmeans)
        # print(outliers_kmeans)
        #print(gower_mat1 [outliers_kmeans,])
        
        #(PCA)
        #fviz_cluster(kmeans.result, data = gower_mat1)
        
        #Usuni??cie i ponowne grupowanie
        
        # usuni??cie outliers_kmeans
        r <- outliers_kmeans
        #data_inf3 <- d[-c(282,266), ]
        data_inf3 <- h[-r, ]
        #data_inf3
        
        
        # dendrogram AHC klastrowanie hierarchiczne po usuni??ciu odchyle?? - outliers_kmeans
        gower_mat1 <- gower.dist ( data_inf3 ,  rngs = NA ,  KR.corr = TRUE ,  var.weights  =  NULL )
        #gower_mat -macierz danych (odleg??o??ci gowera, ale teraz tak wygl??daj?? moje dane, ju?? nie s?? jako??ciowe)
        #gower_mat
        # gower_mat1
        gower_mat1[is.nan(gower_mat1)] <- 0
        #gower_mat1
        gower_mat <-as.dist(gower_mat1) 
        #gower_mat 
        
        #dist.metoda oblicza dist_gower_mat
        #dist_gower_mat<-dist(gower_mat, method = "euclidean", diag = TRUE, upper = FALSE, p = 2)  # to mo??na zmienia?? !!!
        #"euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
        #dist_gower_mat
        
        
        clusters <- hclust(gower_mat, method =  input$metoda) #grupowanie najdalszych s??siad??w  # to mo??na zmienia??!!!
        
        #clustersKMEANS <- readRDS(file = "clustersKMEANS.rds")
       #plot(clusters, hang = -1, main = "Dendrogram prezentujący podział zbioru danych na grupy po usunięciu reguł nietypowych - metoda KMEANS")
        
        plot(fviz_dend(clusters, k = input$liczbaGrup[1],font.main = 20, main = paste("Dendrogram po usunięciu reguł nietypowych - metoda KMEANS, dataset = ",collection_40), # Cut in two groups  !!!!!!! ustawić cutree_k <- 2
                       xlab = paste("Metoda łączenia grup:",input$metoda,"|", "Miara odległości między regułami: Gower " ),cex = 0.5,   # label size
                       k_colors = c("#2E9FDF", "#FC4E07", "#07fc4e", "#3339FF", "#00AFBB", "#FF33F0", "#E7B800", "#C0C0C0","#800080", "#008080", "#808000", "#2F4F4F", "#8B0000", "#FF1493","#BDB76B", "#FFD700", "#483D8B", "#00FF7F","#00CED1","#0000CD" ),
                       color_labels_by_k = TRUE, # color labels by groups
                       rect = FALSE # Add rectangle around groups 
        ))
        
        
      }
      
      
      if (input$algorytm == "SMALLCLUSTER")
        
      {
        #clustersLOF <- readRDS(file = "clustersLOF.rds")
        
        qoute2 <- readRDS(file = "quote2.rds")
        
        t <- fread ("wczytanyZbior.csv", sep=',', header = TRUE, stringsAsFactors = FALSE, quote = qoute2)
        t[t=="NA"] <- NA
        #g[g=="0"] <- 0
        cutree_k_SMALLCLUSTER <- input$liczbaGrup[1]
        cutree_k_SMALLCLUSTER_outliers <- input$smallCluster[1]
        
        if (cutree_k_SMALLCLUSTER_outliers > nrow(t))
          { return("Wybrana liczba klastrów jest większa niż liczba obserwacji") }
        #g<-as.data.frame(g)
        #g<-as.numeric(g)
        
      #  RULES1<-nrow(t)
       # howmany <-(input$procentRegulNietypowych[1]/100)*RULES1
        #howmany<-ceiling(howmany)
        # wyniki[9,2]<-howmany
        #  wyniki[9,3]<-howmany
      #  set_k <- input$kSasiadow[1] 
        # wyniki[8,2]<-set_k
        #  wyniki[8,3]<-set_k
        
        #LOF k = 3 [Breunig i in., 2000] ocena stopnia oddalenia danej obserwacji 
        #od pozosta??ych przy uwzgl??dnieniu zag??szczenia obiekt??w z k-elementowego s??siedztwa 
        #d<- as.data.frame(d, stringsAsFactors = FALSE)
        
        #outlier.scores <- lofactor(gower_mat1, k = set_k)
        #outliers_LOF <- order(outlier.scores, decreasing = T) [1:howmany]
        
        
        #wyniki[12,3] <- toString(outliers_LOF)
        #print(outliers_LOF)
        #print(gower_mat1 [outliers_LOF,])
        
        #Usuni??cie i ponowne grupowanie
        gower_mat1 <- gower.dist ( t ,  rngs = NA ,  KR.corr = TRUE ,  var.weights  =  NULL )
        gower_mat1[is.nan(gower_mat1)] <- 0
        #gower_mat -macierz danych (odleg??o??ci gowera, ale teraz tak wygl??daj?? moje dane, ju?? nie s?? jako??ciowe)
        #gower_mat
        #gower_mat1
        gower_mat <-as.dist(gower_mat1) 
        #gower_mat 
        #dist.metoda oblicza dist_gower_mat
        #dist_gower_mat<-dist(gower_mat, method = "euclidean", diag = TRUE, upper = FALSE, p = 2)  # to mo??na zmienia??!!!!!
        #"euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
        #dist_gower_mat
        clusters <- hclust(gower_mat,method = input$metoda)
        cutree_wektor <- cutree(clusters, k = cutree_k_SMALLCLUSTER_outliers)
        
        
        # outlier.scores <- lofactor(gower_mat1, k = set_k)
        #  outliers_LOF <- order(outlier.scores, decreasing = T) [1:howmany]
        #outliers_SMALLCLUSTER  <-which(cutree_wektor == 10)
        
        
        
        # usuni??cie outliers_SMALLCLUSTER
      #  l <- outliers_SMALLCLUSTER
       # data_inf4 <- t[-l, ]
        # outlier.scores <- lofactor(gower_mat1, k = set_k)
        #  outliers_LOF <- order(outlier.scores, decreasing = T) [1:howmany]
        ilosc_obserwacji <-length(cutree_wektor) 
        ilosc_obserwacji_1 <-length(which(cutree_wektor == 1))
        ilosc_obserwacji_2 <-length(which(cutree_wektor == 2))
        ilosc_obserwacji_3 <-length(which(cutree_wektor == 3))
        ilosc_obserwacji_4 <-length(which(cutree_wektor == 4))
        ilosc_obserwacji_5 <-length(which(cutree_wektor == 5))
        ilosc_obserwacji_6 <-length(which(cutree_wektor == 6))
        ilosc_obserwacji_7 <-length(which(cutree_wektor == 7))
        ilosc_obserwacji_8 <-length(which(cutree_wektor == 8))
        ilosc_obserwacji_9 <-length(which(cutree_wektor == 9))
        ilosc_obserwacji_10 <-length(which(cutree_wektor == 10))
        
        
        
        
        if ((ilosc_obserwacji_1/ilosc_obserwacji) > 0 & (ilosc_obserwacji_1/ilosc_obserwacji) <= input$procentZbioru)
        {
          a1<-TRUE
        }
        else
        {
          a1<-FALSE
        }
        
        
        if ((ilosc_obserwacji_2/ilosc_obserwacji) > 0 & (ilosc_obserwacji_2/ilosc_obserwacji) <= input$procentZbioru)
        {
          b2<-TRUE
        }
        else
        {
          b2<-FALSE
        }
        
        if ((ilosc_obserwacji_3/ilosc_obserwacji) > 0 & (ilosc_obserwacji_3/ilosc_obserwacji) <= input$procentZbioru)
        {
          c3<-TRUE
        }
        else
        {
          c3<-FALSE
        }
        
        if ((ilosc_obserwacji_4/ilosc_obserwacji) > 0 & (ilosc_obserwacji_4/ilosc_obserwacji) <= input$procentZbioru)
        {
          d4<-TRUE
        }
        else
        {
          d4<-FALSE
        }
        
        if ((ilosc_obserwacji_5/ilosc_obserwacji) > 0 & (ilosc_obserwacji_5/ilosc_obserwacji) <= input$procentZbioru)
        {
          e5<-TRUE
        }
        else
        {
          e5<-FALSE
        }
        
        if ((ilosc_obserwacji_6/ilosc_obserwacji) > 0 & (ilosc_obserwacji_6/ilosc_obserwacji) <= input$procentZbioru)
        {
          f6<-TRUE
        }
        else
        {
          f6<-FALSE
        }
        
        if ((ilosc_obserwacji_7/ilosc_obserwacji) > 0 & (ilosc_obserwacji_7/ilosc_obserwacji) <= input$procentZbioru)
        {
          g7<-TRUE
        }
        else
        {
          g7<-FALSE
        }
        
        if ((ilosc_obserwacji_8/ilosc_obserwacji) > 0 & (ilosc_obserwacji_8/ilosc_obserwacji) <= input$procentZbioru)
        {
          h8<-TRUE
        }
        else
        {
          h8<-FALSE
        }
        
        if ((ilosc_obserwacji_9/ilosc_obserwacji) > 0 & (ilosc_obserwacji_9/ilosc_obserwacji) <= input$procentZbioru)
        {
          i9<-TRUE
        }
        else
        {
          i9<-FALSE
        }
        
        if ((ilosc_obserwacji_10/ilosc_obserwacji) > 0 & (ilosc_obserwacji_10/ilosc_obserwacji) <= input$procentZbioru)
        {
          j10<-TRUE
        }
        else
        {
          j10<-FALSE
        }
        
        
        ################################################################################################################3
        
        #a1 b2 c3 d4
        
        if (a1 == TRUE & b2 == TRUE & c3 == TRUE & d4 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4)
        }
        
        #a1 b2 c3 e5
        
        else if (a1 == TRUE & b2 == TRUE & c3 == TRUE & e5 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5)
        }
        
        
        #a1 b2 c3 f6
        
        else if (a1 == TRUE & b2 == TRUE & c3 == TRUE & f6 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6)
        }
        
        
        #a1 b2 c3 g7
        
        else if (a1 == TRUE & b2 == TRUE & c3 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7)
        }
        
        
        #a1 b2 c3 h8
        
        else if (a1 == TRUE & b2 == TRUE & c3 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8)
        }
        
        
        #a1 b2 c3 i9
        
        else if (a1 == TRUE & b2 == TRUE & c3 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_9)
        }
        
        
        #a1 b2 c3 j10
        
        else if (a1 == TRUE & b2 == TRUE & c3 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_10)
        }
        
        #a1 b2 d4 e5
        
        else if (a1 == TRUE & b2 == TRUE & d4 == TRUE & e5 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5)
        }
        
        #a1 b2 d4 f6
        
        else if (a1 == TRUE & b2 == TRUE & d4 == TRUE & f6 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6)
        }
        
        #a1 b2 d4 g7
        
        else if (a1 == TRUE & b2 == TRUE & d4 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7)
        }
        
        #a1 b2 d4 h8
        
        else if (a1 == TRUE & b2 == TRUE & d4 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8)
        }
        
        #a1 b2 d4 i9
        
        else if (a1 == TRUE & b2 == TRUE & d4 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9)
        }
        
        #a1 b2 d4 j10
        
        else if (a1 == TRUE & b2 == TRUE & d4 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_10)
        }
        
        #a1 b2 e5 f6
        
        else if (a1 == TRUE & b2 == TRUE & e5 == TRUE & f6 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
        }
        
        #a1 b2 e5 g7
        
        else if (a1 == TRUE & b2 == TRUE & e5 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
        }
        
        #a1 b2 e5 h8
        
        else if (a1 == TRUE & b2 == TRUE & e5 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
        }
        
        #a1 b2 e5 i9
        
        else if (a1 == TRUE & b2 == TRUE & e5 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
        }
        
        #a1 b2 e5 j10
        
        else if (a1 == TRUE & b2 == TRUE & e5 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
        }
        
        #a1 b2 f6 g7
        
        else if (a1 == TRUE & b2 == TRUE & f6 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        #a1 b2 f6 h8
        
        else if (a1 == TRUE & b2 == TRUE & f6 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        #a1 b2 f6 i9
        
        else if (a1 == TRUE & b2 == TRUE & f6 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        #a1 b2 f6 j10
        
        else if (a1 == TRUE & b2 == TRUE & f6 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        #a1 b2 g7 h8
        
        else if (a1 == TRUE & b2 == TRUE & g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        #a1 b2 g7 i9
        
        else if (a1 == TRUE & b2 == TRUE & g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        #a1 b2 g7 j10
        
        else if (a1 == TRUE & b2 == TRUE & g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #a1 b2 h8 i9
        
        else if (a1 == TRUE & b2 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #a1 b2 h8 j10
        
        else if (a1 == TRUE & b2 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #a1 b2 i9 j10
        
        else if (a1 == TRUE & b2 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #a1 c3 d4 e5
        
        else if (a1 == TRUE & c3 == TRUE & d4 == TRUE & e5 == TRUE)
        {
          outliers_SMALLCLUSTER_1 <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5)
        }
        
        #a1 c3 d4 f6
        
        else if (a1 == TRUE & c3 == TRUE & d4 == TRUE & f6 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6)
        }
        
        #a1 c3 d4 g7
        
        else if (a1 == TRUE & c3 == TRUE & d4 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7)
        }
        
        #a1 c3 d4 h8
        
        else if (a1 == TRUE & c3 == TRUE & d4 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8)
        }
        
        #a1 c3 d4 i9
        
        else if (a1 == TRUE & c3 == TRUE & d4 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9)
        }
        
        #a1 c3 d4 j10
        
        else if (a1 == TRUE & c3 == TRUE & d4 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_10)
        }
        
        #a1 c3 e5 f6
        
        else if (a1 == TRUE & c3 == TRUE & e5 == TRUE & f6 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
        }
        
        #a1 c3 e5 g7
        
        else if (a1 == TRUE & c3 == TRUE & e5 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
        }
        
        #a1 c3 e5 h8
        
        else if (a1 == TRUE & c3 == TRUE & e5 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
        }
        
        #a1 c3 e5 i9
        
        else if (a1 == TRUE & c3 == TRUE & e5 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
        }
        
        #a1 c3 e5 j10
        
        else if (a1 == TRUE & c3 == TRUE & e5 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
        }
        
        #a1 c3 f6 g7
        
        else if (a1 == TRUE & c3 == TRUE & f6 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        #a1 c3 f6 h8
        
        else if (a1 == TRUE & c3 == TRUE & f6 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        #a1 c3 f6 i9
        
        else if (a1 == TRUE & c3 == TRUE & f6 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        #a1 c3 f6 j10
        
        else if (a1 == TRUE & c3 == TRUE & f6 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        #a1 c3 g7 h8
        
        else if (a1 == TRUE & c3 == TRUE & g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        #a1 c3 g7 i9
        
        else if (a1 == TRUE & c3 == TRUE & g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        #a1 c3 g7 j10
        
        else if (a1 == TRUE & c3 == TRUE & g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #a1 c3 h8 i9
        
        else if (a1 == TRUE & c3== TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #a1 c3 h8 j10
        
        else if (a1 == TRUE & c3 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #a1 c3 i9 j10
        
        else if (a1 == TRUE & c3 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #a1 d4 e5 f6
        
        else if (a1 == TRUE & d4 == TRUE & e5 == TRUE & f6 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
        }
        
        #a1 d4 e5 g7
        
        else if (a1 == TRUE & d4 == TRUE & e5 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
        }
        
        #a1 d4 e5 h8
        
        else if (a1 == TRUE & d4 == TRUE & e5 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
        }
        
        #a1 d4 e5 i9
        
        else if (a1 == TRUE & d4 == TRUE & e5 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
        }
        
        #a1 d4 e5 j10
        
        else if (a1 == TRUE & d4 == TRUE & e5 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
        }
        
        #a1 d4 f6 g7
        
        else if (a1 == TRUE & d4 == TRUE & f6 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        #a1 d4 f6 h8
        
        else if (a1 == TRUE & d4 == TRUE & f6 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        #a1 d4 f6 i9
        
        else if (a1 == TRUE & d4 == TRUE & f6 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        #a1 d4 f6 j10
        
        else if (a1 == TRUE & d4 == TRUE & f6 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        #a1 d4 g7 h8
        
        else if (a1 == TRUE & d4 == TRUE & g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        #a1 d4 g7 i9
        
        else if (a1 == TRUE & d4 == TRUE & g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        #a1 d4 g7 j10
        
        else if (a1 == TRUE & d4 == TRUE & g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #a1 d4 h8 i9
        
        else if (a1 == TRUE & d4 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #a1 d4 h8 j10
        
        else if (a1 == TRUE & d4 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #a1 d4 i9 j10
        
        else if (a1 == TRUE & d4 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #a1 e5 f6 g7
        
        else if (a1 == TRUE & e5 == TRUE & f6 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        #a1 e5 f6 h8
        
        else if (a1 == TRUE & e5 == TRUE & f6 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        #a1 e5 f6 i9
        
        else if (a1 == TRUE & e5 == TRUE & f6 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        #a1 e5 f6 j10
        
        else if (a1 == TRUE & e5 == TRUE & f6 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        #a1 e5 g7 h8
        
        else if (a1 == TRUE & e5 == TRUE & g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        #a1 e5 g7 i9
        
        else if (a1 == TRUE & e5 == TRUE & g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        #a1 e5 g7 j10
        
        else if (a1 == TRUE & e5 == TRUE & g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #a1 e5 h8 i9
        
        else if (a1 == TRUE & e5 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #a1 e5 h8 j10
        
        else if (a1 == TRUE & e5 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #a1 e5 i9 j10
        
        else if (a1 == TRUE & e5 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #a1 f6 g7 h8
        
        else if (a1 == TRUE & f6 == TRUE & g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        #a1 f6 g7 i9
        
        else if (a1 == TRUE & f6 == TRUE & g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        #a1 f6 g7 j10
        
        else if (a1 == TRUE & f6 == TRUE & g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #a1 f6 h8 i9
        
        else if (a1 == TRUE & f6 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #a1 f6 h8 j10
        
        else if (a1 == TRUE & f6 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #a1 f6 i9 j10
        
        else if (a1 == TRUE & f6 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #a1 g7 h8 i9
        
        else if (a1 == TRUE & g7 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #a1 g7 h8 j10
        
        else if (a1 == TRUE & g7 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #a1 g7 i9 j10
        
        else if (a1 == TRUE & g7 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #a1 h8 i9 j10
        
        else if (a1 == TRUE & h8 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #b2 c3 d4 e5
        
        else if (b2 == TRUE & c3 == TRUE & d4 == TRUE & e5 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5)
        }
        
        #b2 c3 d4 f6
        
        else if (b2 == TRUE & c3 == TRUE & d4 == TRUE & f6 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6)
        }
        
        #b2 c3 d4 g7
        
        else if (b2 == TRUE & c3 == TRUE & d4 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7)
        }
        
        #b2 c3 d4 h8
        
        else if (b2 == TRUE & c3 == TRUE & d4 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8)
        }
        
        #b2 c3 d4 i9
        
        else if (b2 == TRUE & c3 == TRUE & d4 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9)
        }
        
        #b2 c3 d4 j10
        
        else if (b2 == TRUE & c3 == TRUE & d4 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_10)
        }
        
        #b2 c3 e5 f6
        
        else if (b2 == TRUE & c3 == TRUE & e5 == TRUE & f6 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
        }
        
        #b2 c3 e5 g7
        
        else if (b2 == TRUE & c3 == TRUE & e5 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
        }
        
        #b2 c3 e5 h8
        
        else if (b2 == TRUE & c3 == TRUE & e5 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
        }
        
        #b2 c3 e5 i9
        
        else if (b2 == TRUE & c3 == TRUE & e5 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
        }
        
        #b2 c3 e5 j10
        
        else if (b2 == TRUE & c3 == TRUE & e5 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
        }
        
        #b2 c3 f6 g7
        
        else if (b2 == TRUE & c3 == TRUE & f6 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        #b2 c3 f6 h8
        
        else if (b2 == TRUE & c3 == TRUE & f6 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        #b2 c3 f6 i9
        
        else if (b2 == TRUE & c3 == TRUE & f6 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        #b2 c3 f6 j10
        
        else if (b2 == TRUE & c3 == TRUE & f6 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        #b2 c3 g7 h8
        
        else if (b2 == TRUE & c3 == TRUE & g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        #b2 c3 g7 i9
        
        else if (b2 == TRUE & c3 == TRUE & g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        #b2 c3 g7 j10
        
        else if (b2 == TRUE & c3 == TRUE & g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #b2 c3 h8 i9
        
        else if (b2 == TRUE & c3 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #b2 c3 h8 j10
        
        else if (b2 == TRUE & c3 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #b2 c3 i9 j10
        
        else if (b2 == TRUE & c3 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #b2 d4 e5 f6
        
        else if (b2 == TRUE & d4 == TRUE & e5 == TRUE & f6 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
        }
        
        #b2 d4 e5 g7
        
        else if (b2 == TRUE & d4 == TRUE & e5 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
        }
        
        #b2 d4 e5 h8
        
        else if (b2 == TRUE & d4 == TRUE & e5 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
        }
        
        #b2 d4 e5 i9
        
        else if (b2 == TRUE & d4 == TRUE & e5 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
        }
        
        #b2 d4 e5 j10
        
        else if (b2 == TRUE & d4 == TRUE & e5 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
        }
        
        #b2 d4 f6 g7
        
        else if (b2 == TRUE & d4 == TRUE & f6 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        #b2 d4 f6 h8
        
        else if (b2 == TRUE & d4 == TRUE & f6 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        #b2 d4 f6 i9
        
        else if (b2 == TRUE & d4 == TRUE & f6 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        #b2 d4 f6 j10
        
        else if (b2 == TRUE & d4 == TRUE & f6 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        #b2 d4 g7 h8
        
        else if (b2 == TRUE & d4 == TRUE & g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        #b2 d4 g7 i9
        
        else if (b2 == TRUE & d4 == TRUE & g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        #b2 d4 g7 j10
        
        else if (b2 == TRUE & d4 == TRUE & g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #b2 d4 h8 i9
        
        else if (b2 == TRUE & d4 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #b2 d4 h8 j10
        
        else if (b2 == TRUE & d4 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #b2 d4 i9 j10
        
        else if (b2 == TRUE & d4 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #b2 e5 f6 g7
        
        else if (b2 == TRUE & e5 == TRUE & f6 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        #b2 e5 f6 h8
        
        else if (b2 == TRUE & e5 == TRUE & f6 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        #b2 e5 f6 i9
        
        else if (b2 == TRUE & e5 == TRUE & f6 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        #b2 e5 f6 j10
        
        else if (b2 == TRUE & e5 == TRUE & f6 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        #b2 e5 g7 h8
        
        else if (b2 == TRUE & e5 == TRUE & g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        #b2 e5 g7 i9
        
        else if (b2 == TRUE & e5 == TRUE & g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        #b2 e5 g7 j10
        
        else if (b2 == TRUE & e5 == TRUE & g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #b2 e5 h8 i9
        
        else if (b2 == TRUE & e5 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #b2 e5 h8 j10
        
        else if (b2 == TRUE & e5 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #b2 e5 i9 j10
        
        else if (b2 == TRUE & e5 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #b2 f6 g7 h8
        
        else if (b2 == TRUE & f6 == TRUE & g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        #b2 f6 g7 i9
        
        else if (b2 == TRUE & f6 == TRUE & g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        #b2 f6 g7 j10
        
        else if (b2 == TRUE & f6 == TRUE & g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #b2 f6 h8 i9
        
        else if (b2 == TRUE & f6 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #b2 f6 h8 j10
        
        else if (b2 == TRUE & f6 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #b2 f6 i9 j10
        
        else if (b2 == TRUE & f6 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #b2 g7 h8 i9
        
        else if (b2 == TRUE & g7 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #b2 g7 h8 j10
        
        else if (b2 == TRUE & g7 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #b2 g7 i9 j10
        
        else if (b2 == TRUE & g7 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #b2 h8 i9 j10
        
        else if (b2 == TRUE & h8 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #c3 d4 e5 f6
        
        else if (c3 == TRUE & d4 == TRUE & e5 == TRUE & f6 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
        }
        
        #c3 d4 e5 g7
        
        else if (c3 == TRUE & d4 == TRUE & e5 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
        }
        
        #c3 d4 e5 h8
        
        else if (c3 == TRUE & d4 == TRUE & e5 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
        }
        
        #c3 d4 e5 i9
        
        else if (c3 == TRUE & d4 == TRUE & e5 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
        }
        
        #c3 d4 e5 j10
        
        else if (c3 == TRUE & d4 == TRUE & e5 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
        }
        
        #c3 d4 f6 g7
        
        else if (c3 == TRUE & d4 == TRUE & f6 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        #c3 d4 f6 h8
        
        else if (c3 == TRUE & d4 == TRUE & f6 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        #c3 d4 f6 i9
        
        else if (c3 == TRUE & d4 == TRUE & f6 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        #c3 d4 f6 j10
        
        else if (c3 == TRUE & d4 == TRUE & f6 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        #c3 d4 g7 h8
        
        else if (c3 == TRUE & d4 == TRUE & g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        #c3 d4 g7 i9
        
        else if (c3 == TRUE & d4 == TRUE & g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        #c3 d4 g7 j10
        
        else if (c3 == TRUE & d4 == TRUE & g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #c3 d4 h8 i9
        
        else if (c3 == TRUE & d4 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #c3 d4 h8 j10
        
        else if (c3 == TRUE & d4 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #c3 d4 i9 j10
        
        else if (c3 == TRUE & d4 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #c3 e5 f6 g7
        
        else if (c3 == TRUE & e5 == TRUE & f6 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        #c3 e5 f6 h8
        
        else if (c3 == TRUE & e5 == TRUE & f6 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        #c3 e5 f6 i9
        
        else if (c3 == TRUE & e5 == TRUE & f6 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        #c3 e5 f6 j10
        
        else if (c3 == TRUE & e5 == TRUE & f6 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        #c3 e5 g7 h8
        
        else if (c3 == TRUE & e5 == TRUE & g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        #c3 e5 g7 i9
        
        else if (c3 == TRUE & e5 == TRUE & g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        #c3 e5 g7 j10
        
        else if (c3 == TRUE & e5 == TRUE & g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #c3 e5 h8 i9
        
        else if (c3 == TRUE & e5 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #c3 e5 h8 j10
        
        else if (c3 == TRUE & e5 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #c3 e5 i9 j10
        
        else if (c3 == TRUE & e5 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #c3 f6 g7 h8
        
        else if (c3 == TRUE & f6 == TRUE & g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        #c3 f6 g7 i9
        
        else if (c3 == TRUE & f6 == TRUE & g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        #c3 f6 g7 j10
        
        else if (c3 == TRUE & f6 == TRUE & g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #c3 f6 h8 i9
        
        else if (c3 == TRUE & f6 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #c3 f6 h8 j10
        
        else if (c3 == TRUE & f6 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #c3 f6 i9 j10
        
        else if (c3 == TRUE & f6 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #c3 g7 h8 i9
        
        else if (c3 == TRUE & g7 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #c3 g7 h8 j10
        
        else if (c3 == TRUE & g7 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #c3 g7 i9 j10
        
        else if (c3 == TRUE & g7 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #c3 h8 i9 j10
        
        else if (c3 == TRUE & h8 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #d4 e5 f6 g7
        
        else if (d4 == TRUE & e5 == TRUE & f6 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        #d4 e5 f6 h8
        
        else if (d4 == TRUE & e5 == TRUE & f6 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        #d4 e5 f6 i9
        
        else if (d4 == TRUE & e5 == TRUE & f6 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        #d4 e5 f6 j10
        
        else if (d4 == TRUE & e5 == TRUE & f6 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        #d4 e5 g7 h8
        
        else if (d4 == TRUE & e5 == TRUE & g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        #d4 e5 g7 i9
        
        else if (d4 == TRUE & e5 == TRUE & g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        #d4 e5 g7 j10
        
        else if (d4 == TRUE & e5 == TRUE & g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #d4 e5 h8 i9
        
        else if (d4 == TRUE & e5 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #d4 e5 h8 j10
        
        else if (d4 == TRUE & e5 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #d4 e5 i9 j10
        
        else if (d4 == TRUE & e5 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #d4 f6 g7 h8
        
        else if (d4 == TRUE & f6 == TRUE & g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        #d4 f6 g7 i9
        
        else if (d4 == TRUE & f6 == TRUE & g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        #d4 f6 g7 j10
        
        else if (d4 == TRUE & f6 == TRUE & g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #d4 f6 h8 i9
        
        else if (d4 == TRUE & f6 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #d4 f6 h8 j10
        
        else if (d4 == TRUE & f6 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #d4 f6 i9 j10
        
        else if (d4 == TRUE & f6 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #d4 g7 h8 i9
        
        else if (d4 == TRUE & g7 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #d4 g7 h8 j10
        
        else if (d4 == TRUE & g7 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #d4 g7 i9 j10
        
        else if (d4 == TRUE & g7 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #d4 h8 i9 j10
        
        else if (d4 == TRUE & h8 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #e5 f6 g7 h8
        
        else if (e5 == TRUE & f6 == TRUE & g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        #e5 f6 g7 i9
        
        else if (e5 == TRUE & f6 == TRUE & g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        #e5 f6 g7 j10
        
        else if (e5 == TRUE & f6 == TRUE & g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #e5 f6 h8 i9
        
        else if (e5 == TRUE & f6 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #e5 f6 h8 j10
        
        else if (e5 == TRUE & f6 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #e5 f6 i9 j10
        
        else if (e5 == TRUE & f6 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #e5 g7 h8 i9
        
        else if (e5 == TRUE & g7 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #e5 g7 h8 j10
        
        else if (e5 == TRUE & g7 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #e5 g7 i9 j10
        
        else if (e5 == TRUE & g7 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #e5 h8 i9 j10
        
        else if (e5 == TRUE & h8 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #f6 g7 h8 i9
        
        else if (f6 == TRUE & g7 == TRUE & h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        #f6 g7 h8 j10
        
        else if (f6 == TRUE & g7 == TRUE & h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        #f6 g7 i9 j10
        
        else if (f6 == TRUE & g7 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #f6 h8 i9 j10
        
        else if (f6 == TRUE & h8 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        #g7 h8 i9 j10
        
        else if (g7 == TRUE & h8 == TRUE & i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        
        
        
        
        
        
        #a1 b2 c3
        
        
        else if (a1 == TRUE & b2 == TRUE & c3 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3)
        }
        
        
        
        #a1 b2 d4
        
        else if (a1 == TRUE & b2 == TRUE & d4 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4)
        }
        
        
        #a1 b2 e5
        
        else if (a1 == TRUE & b2 == TRUE & e5 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5)
        }
        
        
        #a1 b2 f6
        
        
        else if (a1 == TRUE & b2 == TRUE & f6 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6)
        }
        
        
        #a1 b2 g7
        
        else if (a1 == TRUE & b2 == TRUE & g7 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7)
        }
        
        
        #a1 b2 h8
        
        else if (a1 == TRUE & b2 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_8)
        }
        
        
        #a1 b2 i9
        
        else if (a1 == TRUE & b2 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_9)
        }
        
        
        #a1 b2 j10
        
        else if (a1 == TRUE & b2 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_10)
        }
        
        #a1 c3 d4
        
        else if (a1 == TRUE & c3 == TRUE & d4 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4)
        }
        
        
        #a1 c3 e5
        
        else if (a1 == TRUE & c3 == TRUE & e5 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5)
        }
        
        
        #a1 c3 f6
        
        else if (a1 == TRUE & c3 == TRUE & f6 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6)
        }
        
        
        #a1 c3 g7
        
        else if (a1 == TRUE & c3 == TRUE & g7 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7)
        }
        
        #a1 c3 h8
        
        else if (a1 == TRUE & c3 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8)
        }
        
        
        #a1 c3 i9
        
        else if (a1 == TRUE & c3 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_9)
        }
        
        
        #a1 c3 j10
        
        else if (a1 == TRUE & c3 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_10)
        }
        
        
        #a1 d4 e5
        
        else if (a1 == TRUE & d4 == TRUE & e5 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5)
        }
        
        
        #a1 d4 f6
        
        else if (a1 == TRUE & d4 == TRUE & f6 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6)
        }
        
        
        #a1 d4 g7
        
        else if (a1 == TRUE & d4 == TRUE & g7 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7)
        }
        
        
        #a1 d4 h8
        
        else if (a1 == TRUE & d4 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8)
        }
        
        
        #a1 d4 i9
        
        else if (a1 == TRUE & d4 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9)
        }
        
        
        #a1 d4 j10
        
        else if (a1 == TRUE & d4 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_10)
        }
        
        
        #a1 e5 f6
        
        else if (a1 == TRUE & e5 == TRUE & f6 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
        }
        
        
        #a1 e5 g7
        
        else if (a1 == TRUE & e5 == TRUE & g7 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
        }
        
        
        #a1 e5 h8
        
        else if (a1 == TRUE & e5 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
        }
        
        
        #a1 e5 i9
        
        else if (a1 == TRUE & e5 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
        }
        
        
        #a1 e5 j10
        
        else if (a1 == TRUE & e5 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
        }
        
        #a1 f6 g7
        
        else if (a1 == TRUE & f6 == TRUE & g7 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        
        #a1 f6 h8
        
        else if (a1 == TRUE & f6 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        
        #a1 f6 i9
        
        else if (a1 == TRUE & f6 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        
        #a1 f6 j10
        
        else if (a1 == TRUE & f6 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        
        #a1 g7 h8
        
        else if (a1 == TRUE & g7 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        
        #a1 g7 i9
        
        else if (a1 == TRUE & g7 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        
        #a1 g7 j10
        
        else if (a1 == TRUE & g7 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        #a1 h8 i9
        
        else if (a1 == TRUE & h8 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        
        #a1 h8 j10
        
        else if (a1 == TRUE & h8 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        
        #a1 i9 j10
        
        else if (a1 == TRUE & i9 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        
        #b2 c3 d4
        
        else if (b2 == TRUE & c3 == TRUE & d4 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4)
        }
        
        
        #b2 c3 e5
        
        else if (b2 == TRUE & c3 == TRUE & e5 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5)
        }
        
        
        #b2 c3 f6
        
        else if (b2 == TRUE & c3 == TRUE & f6 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6)
        }
        
        
        #b2 c3 g7
        
        else if (b2 == TRUE & c3 == TRUE & g7 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7)
        }
        
        
        #b2 c3 h8
        
        else if (b2 == TRUE & c3 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8)
        }
        
        
        #b2 c3 i9
        
        else if (b2 == TRUE & c3 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_9)
        }
        
        
        #b2 c3 j10
        
        else if (b2 == TRUE & c3 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_10)
        }
        
        
        #b2 d4 e5
        
        else if (b2 == TRUE & d4 == TRUE & e5 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5)
        }
        
        
        #b2 d4 f6
        
        else if (b2 == TRUE & d4 == TRUE & f6 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6)
        }
        
        
        #b2 d4 g7
        
        else if (b2 == TRUE & d4 == TRUE & g7 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7)
        }
        
        
        #b2 d4 h8
        
        else if (b2 == TRUE & d4 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8)
        }
        
        
        #b2 d4 i9
        
        else if (b2 == TRUE & d4 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9)
        }
        
        
        #b2 d4 j10
        
        else if (b2 == TRUE & d4 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_10)
        }
        
        
        #b2 e5 f6
        
        else if (b2 == TRUE & e5 == TRUE & f6 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
        }
        
        
        #b2 e5 g7
        
        else if (b2 == TRUE & e5 == TRUE & g7 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
        }
        
        
        #b2 e5 h8
        
        else if (b2 == TRUE & e5 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
        }
        
        
        #b2 e5 i9
        
        else if (b2 == TRUE & e5 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
        }
        
        
        #b2 e5 j10
        
        else if (b2 == TRUE & e5 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
        }
        
        
        #b2 f6 g7
        
        else if (b2 == TRUE & f6 == TRUE & g7 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        
        #b2 f6 h8
        
        else if (b2 == TRUE & f6 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        
        #b2 f6 i9
        
        else if (b2 == TRUE & f6 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        
        #b2 f6 j10
        
        else if (b2 == TRUE & f6 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        
        #b2 g7 h8
        
        else if (b2 == TRUE & g7 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        
        #b2 g7 i9
        
        else if (b2 == TRUE & g7 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        
        #b2 g7 j10
        
        else if (b2 == TRUE & g7 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        
        #b2 h8 i9
        
        else if (b2 == TRUE & h8 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        
        #b2 h8 j10
        
        else if (b2 == TRUE & h8 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        
        #b2 i9 j10
        
        else if (b2 == TRUE & i9 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        
        #c3 d4 e5
        
        else if (c3 == TRUE & d4 == TRUE & e5 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5)
        }
        
        
        #c3 d4 f6
        
        else if (c3 == TRUE & d4 == TRUE & f6 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6)
        }
        
        
        #c3 d4 g7
        
        else if (c3 == TRUE & d4 == TRUE & g7 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7)
        }
        
        
        #c3 d4 h8
        
        else if (c3 == TRUE & d4 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8)
        }
        
        
        #c3 d4 i9
        
        else if (c3 == TRUE & d4 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9)
        }
        
        
        #c3 d4 j10
        
        else if (c3 == TRUE & d4 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_10)
        }
        
        
        #c3 e5 f6
        
        else if (c3 == TRUE & e5 == TRUE & f6 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
        }
        
        
        #c3 e5 g7
        
        else if (c3 == TRUE & e5 == TRUE & g7 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
        }
        
        
        #c3 e5 h8
        
        else if (c3 == TRUE & e5 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
        }
        
        
        #c3 e5 i9
        
        else if (c3 == TRUE & e5 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
        }
        
        
        #c3 e5 j10
        
        else if (c3 == TRUE & e5 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
        }
        
        
        #c3 f6 g7
        
        else if (c3 == TRUE & f6 == TRUE & g7 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        
        #c3 f6 h8
        
        else if (c3 == TRUE & f6 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        
        #c3 f6 i9
        
        else if (c3 == TRUE & f6 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        
        #c3 f6 j10
        
        else if (c3 == TRUE & f6 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        
        #c3 g7 h8
        
        else if (c3 == TRUE & g7 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        
        #c3 g7 i9
        
        else if (c3 == TRUE & g7 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        
        #c3 g7 j10
        
        else if (c3 == TRUE & g7 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        
        #c3 h8 i9
        
        else if (c3 == TRUE & h8 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        
        #c3 h8 j10
        
        else if (c3 == TRUE & h8 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        
        #c3 i9 j10
        
        else if (c3 == TRUE & i9 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        
        #d4 e5 f6
        
        else if (d4 == TRUE & e5 == TRUE & f6 == TRUE )
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
        }
        
        
        #d4 e5 g7
        
        else if (d4 == TRUE & e5 == TRUE & g7 == TRUE )
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
        }
        
        
        #d4 e5 h8
        
        else if (d4 == TRUE & e5 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
        }
        
        
        #d4 e5 i9
        
        else if (d4 == TRUE & e5 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
        }
        
        
        #d4 e5 j10
        
        else if (d4 == TRUE & e5 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
        }
        
        
        #d4 f6 g7
        
        else if (d4 == TRUE & f6 == TRUE & g7 == TRUE )
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        
        #d4 f6 h8
        
        else if (d4 == TRUE & f6 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        
        #d4 f6 i9
        
        else if (d4 == TRUE & f6 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        
        #d4 f6 j10
        
        else if (d4 == TRUE & f6 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        
        #d4 g7 h8
        
        else if (d4 == TRUE & g7 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        
        #d4 g7 i9
        
        else if (d4 == TRUE & g7 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        
        #d4 g7 j10
        
        else if (d4 == TRUE & g7 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        
        #d4 h8 i9
        
        else if (d4 == TRUE & h8 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        
        #d4 h8 j10
        
        else if (d4 == TRUE & h8 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        
        #d4 i9 j10
        
        else if (d4 == TRUE & i9 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        
        #e5 f6 g7
        
        else if (e5 == TRUE & f6 == TRUE & g7 == TRUE )
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        
        #e5 f6 h8
        
        else if (e5 == TRUE & f6 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        
        #e5 f6 i9
        
        else if (e5 == TRUE & f6 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        
        #e5 f6 j10
        
        else if (e5 == TRUE & f6 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        
        #e5 g7 h8
        
        else if (e5 == TRUE & g7 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        
        #e5 g7 i9
        
        else if (e5 == TRUE & g7 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        
        #e5 g7 j10
        
        else if (e5 == TRUE & g7 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        
        #e5 h8 i9
        
        else if (e5 == TRUE & h8 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        
        #e5 h8 j10
        
        else if (e5 == TRUE & h8 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        
        #e5 i9 j10
        
        else if (e5 == TRUE & i9 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        
        #f6 g7 h8
        
        else if (f6 == TRUE & g7 == TRUE & h8 == TRUE )
        {
          outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        
        #f6 g7 i9
        
        else if (f6 == TRUE & g7 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        
        #f6 g7 j10
        
        else if (f6 == TRUE & g7 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        
        #f6 h8 i9
        
        else if (f6 == TRUE & h8 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        
        #f6 h8 j10
        
        else if (f6 == TRUE & h8 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        
        #f6 i9 j10
        
        else if (f6 == TRUE & i9 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        
        #g7 h8 i9
        
        else if (g7 == TRUE & h8 == TRUE & i9 == TRUE )
        {
          outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        
        #g7 h8 j10
        
        else if (g7 == TRUE & h8 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        
        #g7 i9 j10
        
        else if (g7 == TRUE & i9 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        
        #h8 i9 j10
        
        else if (h8 == TRUE & i9 == TRUE & j10 == TRUE )
        {
          outliers_SMALLCLUSTER_8  <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        
        
        
        
        
        
        #a1 b2
        
        
        else if (a1 == TRUE & b2 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_2 <-which(cutree_wektor == 2)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_2)
        }
        
        
        #a1 c3
        
        
        else if (a1 == TRUE & c3 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_3)
        }
        
        
        
        #a1 d4
        
        else if (a1 == TRUE & d4 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_4)
        }
        
        
        #a1 e5
        
        else if (a1 == TRUE & e5 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_5)
        }
        
        
        #a1 f6
        
        else if (a1 == TRUE & f6 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_6)
        }
        
        
        #a1 g7
        
        else if (a1 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_7)
        }
        
        
        #a1 h8
        
        else if (a1 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_8)
        }
        
        
        #a1 i9
        
        else if (a1 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_9)
        }
        
        
        #a1 j10
        
        else if (a1 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1,outliers_SMALLCLUSTER_10)
        }
        
        
        #b2 c3
        
        else if (b2 == TRUE & c3 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_3 <-which(cutree_wektor == 3)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_3)
        }
        
        
        #b2 d4
        
        else if (b2 == TRUE & d4 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_4)
        }
        
        
        #b2 e5
        
        else if (b2 == TRUE & e5 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_5)
        }
        
        
        #b2 f6
        
        else if (b2 == TRUE & f6 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_6)
        }
        
        
        #b2 g7
        
        else if (b2 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_7)
        }
        
        
        #b2 h8
        
        else if (b2 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_8)
        }
        
        
        #b2 i9
        
        else if (b2 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_9)
        }
        
        
        #b2 j10
        
        else if (b2 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2,outliers_SMALLCLUSTER_10)
        }
        
        
        #c3 d4
        
        else if (c3 == TRUE & d4 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_4 <-which(cutree_wektor == 4)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_4)
        }
        
        
        #c3 e5
        
        else if (c3 == TRUE & e5 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_5)
        }
        
        
        #c3 f6
        
        else if (c3 == TRUE & f6 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_6)
        }
        
        
        #c3 g7
        
        else if (c3 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_7)
        }
        
        
        #c3 h8
        
        else if (c3 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_8)
        }
        
        
        #c3 i9
        
        else if (c3 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_9)
        }
        
        
        #c3 j10
        
        else if (c3 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3,outliers_SMALLCLUSTER_10)
        }
        
        
        #d4 e5
        
        else if (d4 == TRUE & e5 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_5 <-which(cutree_wektor == 5)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_5)
        }
        
        
        #d4 f6
        
        else if (d4 == TRUE & f6 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_6)
        }
        
        
        #d4 g7
        
        else if (d4 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_7)
        }
        
        
        #d4 h8
        
        else if (d4 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_8)
        }
        
        
        #d4 i9
        
        else if (d4 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_9)
        }
        
        
        #d4 j10
        
        else if (d4 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4,outliers_SMALLCLUSTER_10)
        }
        
        
        #e5 f6
        
        else if (e5 == TRUE & f6 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_6 <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_6)
        }
        
        
        #e5 g7
        
        else if (e5 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_7)
        }
        
        
        #e5 h8
        
        else if (e5 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_8)
        }
        
        
        #e5 i9
        
        else if (e5 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_9)
        }
        
        
        #e5 j10
        
        else if (e5 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5,outliers_SMALLCLUSTER_10)
        }
        
        
        #f6 g7
        
        else if (f6 == TRUE & g7 == TRUE)
        {
          outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_7 <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_7)
        }
        
        
        #f6 h8
        
        else if (f6 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_8)
        }
        
        
        #f6 i9
        
        else if (f6 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_9)
        }
        
        
        #f6 j10
        
        else if (f6 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6,outliers_SMALLCLUSTER_10)
        }
        
        
        #g7 h8
        
        else if (g7 == TRUE & h8 == TRUE)
        {
          outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_8 <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_8)
        }
        
        
        #g7 i9
        
        else if (g7 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_9)
        }
        
        
        #g7 j10
        
        else if (g7 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7,outliers_SMALLCLUSTER_10)
        }
        
        
        #h8 i9
        
        else if (h8 == TRUE & i9 == TRUE)
        {
          outliers_SMALLCLUSTER_8  <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_9 <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_9)
        }
        
        
        #h8 j10
        
        else if (h8 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_8  <-which(cutree_wektor == 8)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_8,outliers_SMALLCLUSTER_10)
        }
        
        
        #i9 j10
        
        else if (i9 == TRUE & j10 == TRUE)
        {
          outliers_SMALLCLUSTER_9  <-which(cutree_wektor == 9)
          outliers_SMALLCLUSTER_10 <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_9,outliers_SMALLCLUSTER_10)
        }
        
        
        #a1
        
        else if (a1 == TRUE)
        {
          outliers_SMALLCLUSTER_1  <-which(cutree_wektor == 1)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_1)
        }
        
        #b2
        
        else if (b2 == TRUE)
        {
          outliers_SMALLCLUSTER_2  <-which(cutree_wektor == 2)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_2)
        }
        
        #c3
        
        else if (c3 == TRUE)
        {
          outliers_SMALLCLUSTER_3  <-which(cutree_wektor == 3)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_3)
        }
        
        #d4
        
        else if (d4 == TRUE)
        {
          outliers_SMALLCLUSTER_4  <-which(cutree_wektor == 4)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_4)
        }
        
        #e5
        
        else if (e5 == TRUE)
        {
          outliers_SMALLCLUSTER_5  <-which(cutree_wektor == 5)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_5)
        }
        
        #f6
        
        else if (f6 == TRUE)
        {
          outliers_SMALLCLUSTER_6  <-which(cutree_wektor == 6)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_6)
        }
        
        #g7
        
        else if (g7 == TRUE)
        {
          outliers_SMALLCLUSTER_7  <-which(cutree_wektor == 7)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_7)
        }
        
        #h8
        
        else if (h8 == TRUE)
        {
          outliers_SMALLCLUSTER_8  <-which(cutree_wektor == 8)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_8)
        }
        
        #i9
        
        else if (i9 == TRUE)
        {
          outliers_SMALLCLUSTER_9  <-which(cutree_wektor == 9)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_9)
        }
        
        
        #j10
        
        else if (j10 == TRUE)
        {
          outliers_SMALLCLUSTER_10  <-which(cutree_wektor == 10)
          
          outliers_SMALLCLUSTER <- c(outliers_SMALLCLUSTER_10)
        }
        
        else
        {
          outliers_SMALLCLUSTER <- "t"
        }
        
################################################################################      
        # outliers_SMALLCLUSTER  <-which(cutree_wektor == 10)
        #  outliers_SMALLCLUSTER  <-which(cutree_wektor == 11)
        # outliers_SMALLCLUSTER  <-which(cutree_wektor == 20)
        
        if ( outliers_SMALLCLUSTER == "t")
        {    wyniki[12,3] <- "not small clusters"}
        else
        {wyniki[12,3] <- toString(outliers_SMALLCLUSTER)}
        #toString(outliers_SMALLCLUSTER)
        
        
        #print(outliers_LOF)
        #print(gower_mat1 [outliers_LOF,])
        
        #Usuni??cie i ponowne grupowanie
        
        # usuni??cie outliers_LOF
        
        if (outliers_SMALLCLUSTER == "t")
        { data_inf4 <- t }
        else
        {
          xx <- outliers_SMALLCLUSTER
          data_inf4 <- t[-xx, ]
        }   #data_inf1
        
        gower_mat1 <- gower.dist (data_inf4 ,  rngs = NA ,  KR.corr = TRUE ,  var.weights  =  NULL)
        gower_mat1[is.nan(gower_mat1)] <- 0
        gower_mat <-as.dist(gower_mat1) 
        clusters <- hclust(gower_mat, method = input$metoda)   
        
        #plot(clusters, hang = -1, main = "Dendrogram prezentujący podział zbioru danych na grupy po usunięciu reguł nietypowych - metoda SMALLCLUSTER")

        plot(fviz_dend(clusters, k = input$liczbaGrup[1],font.main = 20, main = paste("Dendrogram po usunięciu reguł nietypowych - metoda SMALLCLUSTER, dataset = ",collection_40), # Cut in two groups  !!!!!!! ustawić cutree_k <- 2
                       xlab = paste("Metoda łączenia grup:",input$metoda,"|", "Miara odległości między regułami: Gower " ),cex = 0.5,   # label size
                       k_colors = c("#2E9FDF", "#FC4E07", "#07fc4e", "#3339FF", "#00AFBB", "#FF33F0", "#E7B800", "#C0C0C0","#800080", "#008080", "#808000", "#2F4F4F", "#8B0000", "#FF1493","#BDB76B", "#FFD700", "#483D8B", "#00FF7F","#00CED1","#0000CD" ),
                       color_labels_by_k = TRUE, # color labels by groups
                       rect = FALSE # Add rectangle around groups 
        ))
        
      }
      
      
      
      
    })
    
  
     #Define the file name that will be deleted
    #fn <- "wczytanyZbior.csv"
    #Check its existence
    #if (file.exists(fn)) 
      #Delete file if it exists
     # file.remove(fn)

  })
  
  
}

