#############################_biblioteki_##########################
if (!("here" %in% rownames(installed.packages()))) install.packages("here")
if (!("dplyr" %in% rownames(installed.packages()))) install.packages("dplyr")
library(here)
library(dplyr)

setwd(here()) # sciezka do pliku
getwd()
Sys.setenv(LANG = "en")

########################_generowanie_danych_########################
# maszyna_big <- c(1:80)
# maszyna_big <- paste('x', as.character(maszyna_big), sep='')
# 
# ceny_1 <- sample(15:25, 15, replace=T)
# ceny_2 <- sample(35:45, 30, replace=T)
# ceny_3 <- sample(55:65, 20, replace=T)
# ceny_4 <- sample(75:85, 15, replace=T)
# 
# ceny_big <- c(ceny_1, ceny_2, ceny_3, ceny_4)
# ceny_big
# 
# df <- data.frame(maszyna_big, ceny_big)
# colnames(df) <- c('Maszyna', 'Cena')
# 
# jobs <-data.frame(matrix(0, 80, 100))
# jobs_names <- c(1:100)
# jobs_names <- paste('j', as.character(jobs_names), sep='')
# colnames(jobs) <- jobs_names
# 
# df <- cbind(df, jobs)
# write.csv(df, 'dane.csv', row.names = FALSE)

dane <- read.table("dane_do_wczytania.txt", sep = ";", header = TRUE) # wczytanie danych

suma_jedynek <- sample(0:0, 80, replace=T) # generowanie 80 zer (suma jedynek)
iloraz <- sample(0:0, 80, replace=T)  # generowanie 80 zer (iloraz)
dane_2 <- cbind(dane, suma_jedynek) # dodanie sumy jedynek na koncu tabeli
dane_2 <- cbind(dane_2, iloraz) # dodanie ilorazu na koncu tabeli
remove(suma_jedynek, iloraz)  # usuniecie wygenerowanych wektorow zeby zachowac przejrzystosc 

dane_3 <- dane_2 %>% relocate(suma_jedynek) # stworzenie tabeli blizniaczej do dane_2 z suma jedynek na poczatku
dane_3 <- dane_3 %>% relocate(iloraz) # przeniesienie ilorazu na poczatek powyzszej tabeli

########################_algorytm_########################
vector_match <- list()  # stworzenie pustych list
ilorazy <- list()
koszt <- 0
nr <- 1


while (ncol(dane_3) > 4){  # petla wykonuje sie dopoki pozostaja niewykonane zadania
  colnames(dane_3) <- c(1:ncol(dane_3)) # zmiana nazw kolumn
  
  suma <- 0
  for (j in 1:nrow(dane_3)){  # przejscie po wszystkich wierszach
    for (i in 5 : ncol(dane_3)){  # przejscie po wszystkich kolumnach we wczesniej wybranych wierszu
      suma <- dane_3[j,i] + suma  # sumowanie jedynek w wierszu
    }
    dane_3[j,2] <- suma  # wpisanie policzonej sumy do kolumny "suma jednynek"
    dane_3[j,1] <- dane_3[j,4]/suma  # wpisanie ilorazu ceny przez sume jedynek do kolumny "iloraz"
    suma <- 0  # wyzerowanie sumy w celu przygotowania jej do nastepnego przejscia petli
  }
  
  match <- match(min(dane_3[,1]),dane_3[,1])  # wyszukanie pozycji najmniejszej wartosci z kolumny "iloraz"
  print(paste0("Wybor nr ",nr,", Iloraz: ",dane_3[match,1]))  # wyswietlenie najmniejszej wartosci z kolumny "iloraz"
  ilorazy[nr] <- dane_3[match,1]
  nr<-nr+1
  
  sum_usun <- 0  # suma usunietych zadan
  for (k in 5 : ncol(dane_3)){  # przejscie po kolumnach z zadaniami
    if (dane_3[match,k - sum_usun] == 1 && sum_usun != dane_3[match,2]) {  # jesli zadanie bylo wykonywane przez wybrana maszyne z aktualnie 
                                                                           # najmiejszym ilroazem i suma usunietych nie jest rowna sumie jedynek 
                                                                           # wybranej maszyny
      dane_3[,k - sum_usun] <- NULL  # usuniecie kolumn, ktorych zadania sa wykonane
      sum_usun <- sum_usun + 1
     
    }
  }
  
  koszt <- dane_3[match,4] + koszt  # sumuje koszt wybranych juz maszyn
  vector_match <- append(vector_match, dane_3[match,3])  # dodaje do listy nazwe wybranej maszyny
  
  dane_3 <- dane_3[-match,] # usuwa wiersz z wybrana maszyne z aktualnie najmiejszym ilroazem
  row.names(dane_3) <- c(1:nrow(dane_3))  # nadanie nazw wierszy
}

vector_match_2 <- setdiff(dane_2[,1], dane_3[,3])  # sprawdza ktore maszyny sa wybrane porownujac z oryginalna tabela

remove(i,j,k,match,sum_usun,suma, dane_2) # usuniecie niepotrzebnych wartosci

plot(1:length(ilorazy), ilorazy, type="l", main="Wykres kolejnych wartosci ilorazow", xlab="Numer wyboru", ylab="Wartosc ilorazu")  # wykres kolejnych wartosci ilorazow
