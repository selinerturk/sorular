
# Soru 1a ----
#  
[Github Repo](https://github.com/selinerturk/Final.git)

# -------------------------------------------------------------------------- ###
# Soru 2a ----
# library(dplyr)

# Titanic veri setini yükleme
titanic <- read.csv("https://bit.ly/3vTgDjZ")

# Cinsiyet ve bilet ücretine göre gruplama ve ortalama hesaplama
titanic %>%
  group_by(sex) %>%
  summarize(mean_fare = mean(fare)) 

-------------------------------------------------------------------------- ###
  
  
  # -------------------------------------------------------------------------- ###
  # Soru 2b ----
# library(tidyverse)

# Kayıp olmayan gözlemleri içeren yeni bir veri seti oluşturma
clean_titanic <- titanic %>%
  drop_na(sex, age)

# Kutu grafiği çizme
ggplot(clean_titanic, aes(x = sex, y = age)) +
  geom_boxplot() +
  labs(x = "Cinsiyet", y = "Yaş") 

------------------------------------------------------------------------- ###
  
  
  # -------------------------------------------------------------------------- ###
  # Soru 2c ----
library(ggplot2)

# Histogram grafiğini çizme
ggplot(titanic, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(x = "Yaş", y = "Frekans", title = "Titanic Yolcularının Yaş Dağılımı")

-------------------------------------------------------------------------- ###
  
  
  # -------------------------------------------------------------------------- ###
  # Soru 3a ----
# x <- 10:20
x[seq(1, 5, by = 3)]

------------------------------------------------------------------------- ###
  
  
  # -------------------------------------------------------------------------- ###
  # Soru 3b ----
# dat3 <- inner_join(dat1, dat2)

-------------------------------------------------------------------------- ###
  
  
  # -------------------------------------------------------------------------- ###
  # Soru 3c ----
# library(ggplot2)

# Veri setini yükleme
titanic <- read.csv("https://bit.ly/3vTgDjZ")

# Saçılım grafiği çizimi
ggplot(data = titanic, aes(x = age, y = fare)) +
  geom_point() +
  xlab("Yaş") +
  ylab("Bilet Ücreti") +
  ggtitle("Titanic Veri Seti: Yaş ve Bilet Ücreti Saçılım Grafiği")
-------------------------------------------------------------------------- ###
  
  
  # -------------------------------------------------------------------------- ###
  # Soru 3d
  ----
  # mylist <- list(1:3, c(3:5, NA))
  myresult <- map(mylist, ~ mean(.x, na.rm = TRUE)) %>% unlist()

-------------------------------------------------------------------------- ###
  
  
  # -------------------------------------------------------------------------- ###
  # Soru 3e ----
# # Örnek verileri oluşturma
set.seed(123)  # Rastgelelik için seed değerini ayarlıyoruz
n <- 25  # Gözlem sayısı
X <- rnorm(n, mean = µ, sd = σ)  # N(µ, σ) dağılımına göre rastgele örnekler oluşturuyoruz

# Z istatistiğini hesaplama
X¯ <- mean(X)
S <- sd(X)
Z <- 5 * (X¯ - µ) / S

# P(Z ≤ 1) olasılığını hesaplama
p_value <- pnorm(1, mean = 0, sd = 1)  # Z dağılımının standart normal dağılıma göre olasılığını hesaplıyoruz

-------------------------------------------------------------------------- ###
  
  
  # -------------------------------------------------------------------------- ###
  # Soru 3f ----
# 
tavla_zar_at <- function() {
  zar1 <- sample(1:6, 1, replace = TRUE)  # 1 ile 6 arasından rastgele bir sayı seçiyoruz (zar 1)
  zar2 <- sample(1:6, 1, replace = TRUE)  # 1 ile 6 arasından rastgele bir sayı seçiyoruz (zar 2)
  
  cat("Zar 1:", zar1, "\n")
  cat("Zar 2:", zar2, "\n")
  
  return(list(zar1 = zar1, zar2 = zar2))
}

------------------------------------------------------------------------- ###
  
  
  # -------------------------------------------------------------------------- ###
  # Soru 3g ----
# # Kurtulan yolcuların yaş ortalaması
age_survived <- mean(titanic$titanic$age[titanic$titanic$survived == 1], na.rm = TRUE)

# Kurtulamayan yolcuların yaş ortalaması
age_not_survived <- mean(titanic$titanic$age[titanic$titanic$survived == 0], na.rm = TRUE)

# T-testi
t_test <- t.test(titanic$titanic$age[titanic$titanic$survived == 1], titanic$titanic$age[titanic$titanic$survived == 0], 
                 alternative = "two.sided", var.equal = TRUE)

# Test sonucunu kontrol etmek
print(t_test)
------------------------------------------ ###
  
  
  # -------------------------------------------------------------------------- ###
  # Soru 4a ----
# library(tidyr)

dat2 <- dat %>%
  pivot_longer(cols = -country, names_to = "year", values_to = "gdp") %>%
  mutate(year = as.integer(year))
-------------------------------------------------------------------------- ###
  
  
  # -------------------------------------------------------------------------- ###
  # Soru 5a ----
# install.packages("ggally")
library(ggally)
ggpairs(dat, columns = c("price", "cut", "depth", "color"))
-------------------------------------------------------------------------- ###
  