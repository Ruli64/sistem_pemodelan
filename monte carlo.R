
# =========================================================================
# SIMULASI MONTE CARLO:
# ANTREAN KENDARAAN DI SPBU MENGGUNAKAN METODE MONTE CARLO
# Model antrean M/M/1 (satu pompa pelayanan)
# =========================================================================

# 1. Mengunci angka acak agar hasil konsisten
set.seed(20261024)

# 2. Parameter sistem SPBU
jumlah_kendaraan <- 100
rata_antar_kedatangan <- 4.5  # Rata-rata kedatangan tiap 4,5 menit
rata_durasi_pengisian <- 3.5  # Rata-rata pengisian BBM 3,5 menit

# 3. Membangkitkan variabel acak berdistribusi eksponensial
selisih_kedatangan <- round(
  rexp(jumlah_kendaraan, rate = 1 / rata_antar_kedatangan), 2
)

durasi_pengisian <- round(
  rexp(jumlah_kendaraan, rate = 1 / rata_durasi_pengisian), 2
)

# 4. Inisialisasi vektor hasil simulasi
waktu_kedatangan <- numeric(jumlah_kendaraan)
mulai_pelayanan <- numeric(jumlah_kendaraan)
waktu_tunggu <- numeric(jumlah_kendaraan)
selesai_pelayanan <- numeric(jumlah_kendaraan)
total_waktu_sistem <- numeric(jumlah_kendaraan)
waktu_menganggur <- numeric(jumlah_kendaraan)

# 5. Proses simulasi antrean kendaraan
for (i in 1:jumlah_kendaraan) {
  
  if (i == 1) {
    waktu_kedatangan[i] <- selisih_kedatangan[i]
    mulai_pelayanan[i] <- waktu_kedatangan[i]
    waktu_menganggur[i] <- waktu_kedatangan[i]
    
  } else {
    waktu_kedatangan[i] <-
      waktu_kedatangan[i - 1] + selisih_kedatangan[i]
    
    mulai_pelayanan[i] <- max(
      waktu_kedatangan[i],
      selesai_pelayanan[i - 1]
    )
    
    waktu_menganggur[i] <- max(
      0,
      waktu_kedatangan[i] - selesai_pelayanan[i - 1]
    )
  }
  
  waktu_tunggu[i] <- round(
    mulai_pelayanan[i] - waktu_kedatangan[i], 2
  )
  
  selesai_pelayanan[i] <- round(
    mulai_pelayanan[i] + durasi_pengisian[i], 2
  )
  
  total_waktu_sistem[i] <- round(
    selesai_pelayanan[i] - waktu_kedatangan[i], 2
  )
}

# 6. Menyusun tabel hasil simulasi
tabel_simulasi <- data.frame(
  Kendaraan = 1:jumlah_kendaraan,
  Selisih_Kedatangan = selisih_kedatangan,
  Waktu_Kedatangan = waktu_kedatangan,
  Mulai_Pengisian = mulai_pelayanan,
  Durasi_Pengisian = durasi_pengisian,
  Waktu_Tunggu = waktu_tunggu,
  Selesai_Pengisian = selesai_pelayanan,
  Total_Waktu_Sistem = total_waktu_sistem
)

cat("\n================ TABEL HASIL SIMULASI =================\n")
print(tabel_simulasi, row.names = FALSE)

# 7. Evaluasi kinerja antrean SPBU
total_waktu_simulasi <- selesai_pelayanan[jumlah_kendaraan]

total_durasi_pengisian <- sum(durasi_pengisian)

utilisasi_pompa <- (
  total_durasi_pengisian / total_waktu_simulasi
) * 100

kendaraan_mengantre <- sum(waktu_tunggu > 0)

persentase_mengantre <- (
  kendaraan_mengantre / jumlah_kendaraan
) * 100

rata_rata_tunggu <- mean(waktu_tunggu)
maksimum_tunggu <- max(waktu_tunggu)
rata_rata_sistem <- mean(total_waktu_sistem)

cat("\n================ RINGKASAN KINERJA SPBU =================\n")
cat(sprintf(
  "1. Rata-rata waktu tunggu       : %.2f menit\n",
  rata_rata_tunggu
))
cat(sprintf(
  "2. Waktu tunggu maksimum       : %.2f menit\n",
  maksimum_tunggu
))
cat(sprintf(
  "3. Rata-rata waktu dalam sistem: %.2f menit\n",
  rata_rata_sistem
))
cat(sprintf(
  "4. Kendaraan yang mengantre    : %d dari %d (%.1f%%)\n",
  kendaraan_mengantre,
  jumlah_kendaraan,
  persentase_mengantre
))
cat(sprintf(
  "5. Utilisasi pompa pelayanan   : %.2f%%\n",
  utilisasi_pompa
))
cat(sprintf(
  "6. Total waktu simulasi        : %.2f menit\n",
  total_waktu_simulasi
))

# 8. Visualisasi grafik di RStudio
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Grafik 1: Waktu tunggu setiap kendaraan
plot(
  tabel_simulasi$Kendaraan,
  tabel_simulasi$Waktu_Tunggu,
  type = "b",
  pch = 19,
  col = "darkred",
  main = "Waktu Tunggu Kendaraan",
  xlab = "Urutan Kendaraan",
  ylab = "Waktu Tunggu (menit)"
)
grid()

# Grafik 2: Durasi pengisian setiap kendaraan
plot(
  tabel_simulasi$Kendaraan,
  tabel_simulasi$Durasi_Pengisian,
  type = "h",
  lwd = 3,
  col = "steelblue",
  main = "Durasi Pengisian BBM",
  xlab = "Urutan Kendaraan",
  ylab = "Durasi (menit)"
)
grid()

# Grafik 3: Total waktu kendaraan di sistem
plot(
  tabel_simulasi$Kendaraan,
  tabel_simulasi$Total_Waktu_Sistem,
  type = "h",
  lwd = 3,
  col = "darkgreen",
  main = "Total Waktu dalam Sistem",
  xlab = "Urutan Kendaraan",
  ylab = "Waktu (menit)"
)
grid()

# Grafik 4: Distribusi waktu tunggu
hist(
  tabel_simulasi$Waktu_Tunggu,
  breaks = "Sturges",
  col = "orange",
  border = "white",
  main = "Distribusi Waktu Tunggu",
  xlab = "Waktu Tunggu (menit)",
  ylab = "Jumlah Kendaraan"
)
grid()

# Mengembalikan layout grafik ke satu panel
par(mfrow = c(1, 1))

# 9. Menyimpan hasil simulasi ke file CSV
write.csv(
  tabel_simulasi,
  "hasil_simulasi_spbu.csv",
  row.names = FALSE
)

cat("\nHasil simulasi disimpan ke: hasil_simulasi_spbu.csv\n")

