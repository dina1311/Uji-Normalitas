#lokasi kerja
setwd("")

#add data
data <- read.delim("clipboard")
View(data)

#uji shapiro wilk
shapiro.test(data$Jumlah.Pengeluaran.Rumah.Tangga.Per.Kepala..Rupiah.)

#Transformasi
library(car)
transformasi <- powerTransform(data$Jumlah.Pengeluaran.Rumah.Tangga.Per.Kepala..Rupiah.)
transformasi

#inisialisasi
hasiltransform <- (data$Jumlah.Pengeluaran.Rumah.Tangga.Per.Kepala..Rupiah.^-2.080853)
View(hasiltransform)

#pengujian Normalitas
library(normtest)
library(nortest)
jb.norm.test(hasiltransform)
sf.test(hasiltransform)
ad.test(hasiltransform)

#plot
qqnorm(hasiltransform)
qqline(hasiltransform)


#plotqqplotr
jumRT <- data.frame(hasiltransform)
colnames(jumRT)<- "jumlah_pengeluaran"
attach(jumRT)
View(jumRT)

library(qqplotr)
plotqq <- ggplot(data = jumRT, mapping = aes(sample = jumlah_pengeluaran)) +
  geom_qq_band(bandType = "ts", mapping = aes(fill = "TS")) +
  geom_qq_band(bandType = "normal", mapping = aes(fill = "Normal")) +
  geom_qq_band(bandType = "boot", mapping = aes(fill = "Bootstrap")) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_fill_discrete("Bandtype")
plotqq
