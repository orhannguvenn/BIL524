# Adı-Soyadı: Orhan Guven
# Çıktı: R Dökümanı
# Başlık: Keşifsel Veri Analizi, RStudio, Uygulama 1

# Çalışmanın Amacı: Bu çalışmanın amacı, RStudio kullanarak keşifsel 
# veri analizi yapmak ve ardından bu analizleri temel alarak çoklu 
# doğrusal regresyon modeli oluşturmak ve değerlendirmektir. Veri seti, 
# 17 Nisan 2023 ile 17 Nisan 2024 tarihleri arasında Apple Inc. (AAPL) 
# hisse senedi için elde edilen historical data'dan elde edilmiştir.
# Analizler veri setinin kalitesini kontrol etmek, eksik veri analizi 
# yapmak, aykırı değerleri tespit etmek, dağılımları ve korelasyonları 
# keşfetmekten oluşur. Bu analizlerin ardından, hisse senedi fiyatlarıyla 
# hacim arasındaki ilişki grafiklerle gösterilir ve bağımsız değişkenlerin 
# Close fiyatını ne kadar iyi açıkladığı değerlendirilir. Son olarak, veri 
# seti eğitim ve test alt kümelerine bölünerek modelin performansı 
# değerlendirilir.

# Kaynakça: Çalışma için kullanılan verilere aşağıdaki bağlantıdan 
# erişilebilir: https://finance.yahoo.com/quote/AAPL/history?period1=1681689600&period2=1713312000

# Veri Setini Okuma
AAPL_data <- read.csv("AAPL.csv")

# Veri Kalitesi Kontrolü
str(AAPL_data)

# Eksik Veri Analizi
missing_values <- sum(is.na(AAPL_data))
print(paste("Toplam Eksik Veri Sayısı:", missing_values))

# Eksik Verileri ortalama ile Doldurma (Eksik değer olmamasına rağmen, 
# fonksiyonu kullanmak için yazdım.)
numeric_columns <- sapply(AAPL_data, is.numeric)
AAPL_data_filled_mean <- AAPL_data
for (col in names(AAPL_data_filled_mean)[numeric_columns]) {
  AAPL_data_filled_mean[[col]][is.na(AAPL_data_filled_mean[[col]])] <- mean(AAPL_data_filled_mean[[col]], na.rm = TRUE)
}

# Eksik değerlerin doldurulmasından sonra aykırı değer tespiti
# Aykırı Değer Tespiti
AAPL_boxplot <- boxplot(AAPL_data_filled_mean$Close)
# Z-puanı Hesaplama
z_scores <- scale(AAPL_data_filled_mean$Close)
# Aykırı Değerleri Belirleme
outliers <- abs(z_scores) > 3

# Alt ve Üst Çeyreklikleri Hesaplama
Q1 <- quantile(AAPL_data_filled_mean$Close, 0.25)
Q3 <- quantile(AAPL_data_filled_mean$Close, 0.75)

# Alt ve Üst Sınırı Hesaplama
lower_bound <- Q1 - 1.5 * IQR(AAPL_data_filled_mean$Close)
upper_bound <- Q3 + 1.5 * IQR(AAPL_data_filled_mean$Close)

# Aykırı Değerleri Belirleme
outliers <- AAPL_data_filled_mean$Close < lower_bound | AAPL_data_filled_mean$Close > upper_bound

# Dağılımları Keşif
par(mfrow=c(1,1))
hist(AAPL_data_filled_mean$Close, main="Histogram", xlab="Close Price")
boxplot(AAPL_data_filled_mean$Close, main="Boxplot", ylab="Close Price")
qqnorm(AAPL_data_filled_mean$Close, main="Q-Q Plot")
qqline(AAPL_data_filled_mean$Close)

# Kantillerden Yararlanma
quantile_values <- quantile(AAPL_data$Close, na.rm = TRUE)

# Korelasyon Katsayısını Hesaplama
correlation <- cor(AAPL_data[, c("Open", "High", "Low", "Close", "Volume")])

# Korelasyon Katsayısını Ekrana Yazdırma
print(correlation)

# Çoklu Doğrusal Regresyon Modelini Oluşturma
model <- lm(Close ~ Open + High + Low + Volume, data = AAPL_data)

# Model Özetini Alma
print(summary(model))

# Bağımsız Değişkenler Arasındaki Korelasyonu Hesaplama
correlation_matrix <- cor(AAPL_data[, c("Open", "High", "Low", "Volume")])

# Korelasyon Matrisini Görselleştirme
library(corrplot)
corrplot(correlation_matrix, method = "color")

# Bağımsız Değişkenlerin Grafiğini Çizme

plot(AAPL_data$Open, AAPL_data$Close, col = "red", main = "Open vs Close")
plot(AAPL_data$High, AAPL_data$Close, col = "blue", main = "High vs Close")
plot(AAPL_data$Low, AAPL_data$Close, col = "green", main = "Low vs Close")
plot(AAPL_data$Volume, AAPL_data$Close, col = "orange", main = "Volume vs Close")


# Veriyi Standartlaştırma
scaled_data <- scale(AAPL_data[, c("Open", "High", "Low", "Close", "Volume")])

# Standartlaştırılmış Veriyi Kontrol Etme
summary(scaled_data)

# caTools Paketini Yükleme
install.packages("caTools")
library(caTools)

# Veriyi Test ve Eğitim Alt Kümelerine Bölme
set.seed(123)
split <- sample.split(AAPL_data$Close, SplitRatio = 0.7)
train_data <- subset(AAPL_data, split == TRUE)
test_data <- subset(AAPL_data, split == FALSE)

# Verinin Boyutlarını Kontrol Etme
print(paste("Eğitim Veri Boyutu:", dim(train_data)))
print(paste("Test Veri Boyutu:", dim(test_data)))

#-------------------------------------------------------------------

# Adı Soyadı: Orhan Güven
# Çıktı: R Dökümanı
# Başlık: Keşifsel Veri Analizi, RStudio, Uygulama 2

# Çalışmanın Amacı: Bu çalışmanın amacı, RStudio kullanarak keşifsel 
# veri analizi yapmak ve ardından bu analizleri temel alarak hisse 
# senedi fiyatlarının kapanış değerini açıklamak için bir regresyon 
# modeli oluşturmak ve değerlendirmektir. Veri seti, 17 Nisan 2023 ile 
# 17 Nisan 2024 tarihleri arasında Xiaomi Corporation (1810.HK) hisse 
# senedi için elde edilen historical data'dan elde edilmiştir. Analizler 
# veri setinin kalitesini kontrol etmek, eksik veri analizi yapmak, 
# aykırı değerleri tespit etmek, dağılımları ve korelasyonları keşfetmekten 
# oluşur. Bu analizlerin ardından, hisse senedi fiyatlarıyla hacim arasındaki 
# ilişki grafiklerle gösterilir ve bağımsız değişkenlerin Close fiyatını 
# ne kadar iyi açıkladığı değerlendirilir. Son olarak, veri seti eğitim ve 
# test alt kümelerine bölünerek modelin performansı değerlendirilir.

# Kaynakça: Çalışma için kullanılan verilere aşağıdaki bağlantıdan 
# erişilebilir: https://finance.yahoo.com/quote/1810.HK/history?period1=1681689600&period2=1713312000


# Veri setini okuma
xiaomi_data <- read.csv("1810.HK.csv")

# Veri setinin yapısını gösterme
str(xiaomi_data)

# Eksik veri tespiti
missing_values <- sum(is.na(xiaomi_data))
print(paste("Toplam Eksik Veri Sayısı:", missing_values))

# Aykırı Değer Tespiti
xiaomi_boxplot <- boxplot(xiaomi_data$Close)

# Z-puanı Hesaplama
z_scores <- scale(xiaomi_data$Close)

# Aykırı Değerleri Belirleme
outliers <- abs(z_scores) > 3

# Alt ve Üst Çeyreklikleri Hesaplama
Q1 <- quantile(xiaomi_data$Close, 0.25)
Q3 <- quantile(xiaomi_data$Close, 0.75)

# Alt ve Üst Sınırı Hesaplama
lower_bound <- Q1 - 1.5 * IQR(xiaomi_data$Close)
upper_bound <- Q3 + 1.5 * IQR(xiaomi_data$Close)

# Aykırı Değerleri Belirleme
outliers <- xiaomi_data$Close < lower_bound | xiaomi_data$Close > upper_bound

# Dağılımları Keşif
par(mfrow=c(1,1))
hist(xiaomi_data$Close, main="Histogram", xlab="Close Price")
boxplot(xiaomi_data$Close, main="Boxplot", ylab="Close Price")
qqnorm(xiaomi_data$Close, main="Q-Q Plot")
qqline(xiaomi_data$Close)

# Kantillerden Yararlanma
quantile_values <- quantile(xiaomi_data$Close, na.rm = TRUE)

# Korelasyon Katsayısını Hesaplama
correlation <- cor(xiaomi_data[, c("Open", "High", "Low", "Close", "Volume")])

# Korelasyon Katsayısını Ekrana Yazdırma
print(correlation)

# caTools Paketini Yükleme
install.packages("caTools")
library(caTools)

# Test-Train
set.seed(123)
split <- sample.split(xiaomi_data$Close, SplitRatio = 0.7)
train_data <- subset(xiaomi_data, split == TRUE)
test_data <- subset(xiaomi_data, split == FALSE)

# Verinin Boyutlarını Kontrol Etme
print(paste("Eğitim Veri Boyutu:", dim(train_data)))
print(paste("Test Veri Boyutu:", dim(test_data)))
