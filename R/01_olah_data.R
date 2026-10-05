# =====================================================================
# R/01_olah_data.R
# Web story UAS Visualisasi Data dan Informasi 2026
# "Sebelum Usia Lima: Peta Kematian Balita Indonesia"
#
# Skrip ini HANYA menyiapkan data final. Visualisasi dibuat di index.qmd
# yang cukup membaca file di data/olahan/.
#
# U5MR = Angka Kematian Balita per 1.000 kelahiran hidup.
#
# Cara menjalankan (dari akar proyek / RStudio Project):
#   Rscript R/01_olah_data.R
#   atau di Console: source("R/01_olah_data.R")
#
# File masuk (data/mentah/, tidak diubah):
#   - angka_kematian_dan_jumlah_balita.xlsx  (sheet "Data 1" dan "Data 2")
#   - data_multivariat.xlsx
#   - batas_kabkota_2025_bps.geojson
#
# Kunci penggabungan: NAMA kabupaten/kota (bukan kode wilayah).
# =====================================================================

options(warn = 1)
set.seed(2026)

# ---------------------------------------------------------------------
# T0. Paket dan jalur
# ---------------------------------------------------------------------
paket_wajib <- c("tidyverse", "readxl", "sf", "cluster", "classInt")
paket_hilang <- paket_wajib[!vapply(paket_wajib, requireNamespace,
                                    logical(1), quietly = TRUE)]
if (length(paket_hilang) > 0) {
  stop("Paket belum terpasang: ", paste(paket_hilang, collapse = ", "),
       "\nPasang dengan: install.packages(c(",
       paste0("\"", paket_hilang, "\"", collapse = ", "), "))", call. = FALSE)
}
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(sf)
  library(cluster)
  library(classInt)
})

# rmapshaper dianjurkan untuk menyederhanakan geometri; bila tidak ada,
# dipakai sf::st_simplify sebagai cadangan (hasilnya tetap sah).
ada_rmapshaper <- requireNamespace("rmapshaper", quietly = TRUE)
if (!ada_rmapshaper) {
  message("Catatan: paket 'rmapshaper' tidak ditemukan; memakai sf::st_simplify ",
          "sebagai cadangan. Pasang rmapshaper untuk hasil yang lebih baik.")
}

DIR_MENTAH <- "data/mentah"
DIR_OLAHAN <- "data/olahan"
F_BALITA   <- file.path(DIR_MENTAH, "angka_kematian_dan_jumlah_balita.xlsx")
F_MULTI    <- file.path(DIR_MENTAH, "data_multivariat.xlsx")
F_GEO      <- file.path(DIR_MENTAH, "batas_kabkota_2025_bps.geojson")

for (f in c(F_BALITA, F_MULTI, F_GEO)) {
  if (!file.exists(f)) {
    stop("File tidak ditemukan: ", f,
         "\nPastikan skrip dijalankan dari akar proyek dan file mentah ada di ",
         DIR_MENTAH, "/", call. = FALSE)
  }
}
dir.create(DIR_OLAHAN, recursive = TRUE, showWarnings = FALSE)

# Batas ukuran geojson hasil penyederhanaan (byte)
BATAS_UKURAN_GEO <- 1.2e6

# ---------------------------------------------------------------------
# Fungsi bantu
# ---------------------------------------------------------------------

# Menyeragamkan nama: huruf kecil, rapikan spasi (termasuk spasi tak-putus)
nama_norm <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[\u00a0\u200b]", " ")
  str_squish(str_to_lower(x))
}

# Kunci nama untuk geojson: "Kab " -> "Kabupaten "; awalan "Kabupaten " dibuang
# (kabupaten tanpa awalan), awalan "Kota " dipertahankan. Sejajar dengan Data 1.
geo_key <- function(x) {
  k <- nama_norm(x)
  k <- str_replace(k, "^kab ", "kabupaten ")
  str_replace(k, "^kabupaten ", "")
}

# Membaca sheet BPS (dua kolom: nama wilayah, nilai) dan menandai provinsi.
# Awal data dicari dari baris "ACEH" (bukan nomor baris tetap).
baca_sheet_bps <- function(path, sheet) {
  mentah <- read_excel(path, sheet = sheet, col_names = FALSE,
                       col_types = "text", .name_repair = "minimal")
  mentah <- mentah[, 1:2]
  names(mentah) <- c("nama", "nilai")
  mentah <- mentah %>% mutate(nama = str_squish(nama))

  mulai <- which(str_to_upper(mentah$nama) == "ACEH")[1]
  if (is.na(mulai)) stop("Baris 'ACEH' tidak ditemukan pada sheet ", sheet)

  d <- mentah[mulai:nrow(mentah), ] %>%
    filter(!is.na(nama), nama != "Catatan") %>%
    mutate(
      nilai    = suppressWarnings(as.numeric(nilai)),
      is_indo  = nama == "INDONESIA",
      is_prov  = nama == str_to_upper(nama) & !str_detect(nama, "^KOTA ") & !is_indo,
      provinsi = if_else(is_prov, nama, NA_character_)
    ) %>%
    fill(provinsi, .direction = "down")

  if (any(is.na(d$nilai))) {
    stop("Ada nilai non-numerik pada sheet ", sheet, ": ",
         paste(head(d$nama[is.na(d$nilai)], 10), collapse = ", "))
  }
  list(
    mentah = mentah,
    kk     = d %>% filter(!is_prov, !is_indo) %>% select(provinsi, nama, nilai),
    prov   = d %>% filter(is_prov) %>% select(provinsi, nilai),
    indo   = d %>% filter(is_indo) %>% pull(nilai)
  )
}

# Pencatat validasi
hasil_validasi <- tibble(cek = character(), status = character(), detail = character())
catat <- function(cek, lulus, detail = "") {
  hasil_validasi <<- bind_rows(
    hasil_validasi,
    tibble(cek = cek, status = if_else(isTRUE(lulus), "LULUS", "GAGAL"),
           detail = as.character(detail))
  )
  invisible(lulus)
}

# Pencatat log struktur
log_baris <- character()
catat_struktur <- function(judul, x) {
  log_baris <<- c(log_baris, paste0("===== ", judul, " ====="),
                  capture.output(str(x, give.attr = FALSE)),
                  capture.output(print(head(x, 5))),
                  "")
}

# =====================================================================
# T1. IMPORT
# =====================================================================
message("\n[T1] Import data ...")
d1 <- baca_sheet_bps(F_BALITA, "Data 1")   # U5MR kab/kota 2025
d2 <- baca_sheet_bps(F_BALITA, "Data 2")   # jumlah penduduk 0-4 tahun 2025
multi_mentah <- read_excel(F_MULTI)
geo_mentah   <- st_read(F_GEO, quiet = TRUE)

# =====================================================================
# T2. CEK STRUKTUR DAN TIPE DATA
# =====================================================================
message("[T2] Cek struktur ...")
catat_struktur("Data 1 (mentah, 2 kolom)", d1$mentah)
catat_struktur("Data 1 kab/kota", d1$kk)
catat_struktur("Data 1 provinsi", d1$prov)
catat_struktur("Data 2 (mentah, 2 kolom)", d2$mentah)
catat_struktur("Data 2 kab/kota", d2$kk)
catat_struktur("Data 2 provinsi", d2$prov)
catat_struktur("data_multivariat.xlsx", multi_mentah)
catat_struktur("geojson (tanpa geometri)", st_drop_geometry(geo_mentah))
log_baris <- c(log_baris,
               paste("Data 1: kab/kota =", nrow(d1$kk), "| provinsi =", nrow(d1$prov)),
               paste("Data 2: kab/kota =", nrow(d2$kk), "| provinsi =", nrow(d2$prov),
                     "| INDONESIA =", d2$indo),
               paste("multivariat:", nrow(multi_mentah), "baris x", ncol(multi_mentah), "kolom"),
               paste("geojson:", nrow(geo_mentah), "fitur | CRS:", st_crs(geo_mentah)$input))
writeLines(log_baris, file.path(DIR_OLAHAN, "log_struktur.txt"))

# =====================================================================
# T3. CLEANING
#   - nama wilayah dirapikan lewat nama_norm()/geo_key()
#   - nilai numerik sudah dikonversi aman di baca_sheet_bps()
#   - tidak ada imputasi; missing/duplikat diperiksa di bawah
# =====================================================================
message("[T3] Cleaning ...")

# Tabel pulau: pengelompokan geografis buatan (7 kelompok) untuk memenuhi 3
# level hierarki (Pulau > Provinsi > Kab/Kota); BUKAN klasifikasi resmi BPS.
tabel_pulau <- tribble(
  ~pulau, ~provinsi,
  "Sumatera", "ACEH", "Sumatera", "SUMATERA UTARA", "Sumatera", "SUMATERA BARAT",
  "Sumatera", "RIAU", "Sumatera", "JAMBI", "Sumatera", "SUMATERA SELATAN",
  "Sumatera", "BENGKULU", "Sumatera", "LAMPUNG", "Sumatera", "KEP. BANGKA BELITUNG",
  "Sumatera", "KEPULAUAN RIAU",
  "Jawa", "DKI JAKARTA", "Jawa", "JAWA BARAT", "Jawa", "JAWA TENGAH",
  "Jawa", "D I YOGYAKARTA", "Jawa", "JAWA TIMUR", "Jawa", "BANTEN",
  "Bali & Nusa Tenggara", "BALI", "Bali & Nusa Tenggara", "NUSA TENGGARA BARAT",
  "Bali & Nusa Tenggara", "NUSA TENGGARA TIMUR",
  "Kalimantan", "KALIMANTAN BARAT", "Kalimantan", "KALIMANTAN TENGAH",
  "Kalimantan", "KALIMANTAN SELATAN", "Kalimantan", "KALIMANTAN TIMUR",
  "Kalimantan", "KALIMANTAN UTARA",
  "Sulawesi", "SULAWESI UTARA", "Sulawesi", "SULAWESI TENGAH",
  "Sulawesi", "SULAWESI SELATAN", "Sulawesi", "SULAWESI TENGGARA",
  "Sulawesi", "GORONTALO", "Sulawesi", "SULAWESI BARAT",
  "Maluku", "MALUKU", "Maluku", "MALUKU UTARA",
  "Papua", "PAPUA BARAT", "Papua", "PAPUA BARAT DAYA", "Papua", "PAPUA",
  "Papua", "PAPUA SELATAN", "Papua", "PAPUA TENGAH", "Papua", "PAPUA PEGUNUNGAN"
)
urutan_pulau <- c("Sumatera", "Jawa", "Bali & Nusa Tenggara", "Kalimantan",
                  "Sulawesi", "Maluku", "Papua")

if (anyDuplicated(tabel_pulau$provinsi) > 0) stop("Tabel pulau: provinsi ganda")
prov_tanpa_pulau <- setdiff(d1$prov$provinsi, tabel_pulau$provinsi)
if (length(prov_tanpa_pulau) > 0) {
  stop("Provinsi tanpa kelompok pulau: ", paste(prov_tanpa_pulau, collapse = ", "))
}

# Basis = Data 1 (514 kab/kota)
dasar <- d1$kk %>%
  transmute(provinsi, kabkota = nama, key = nama_norm(nama), u5mr = nilai)
if (anyDuplicated(dasar$key) > 0) {
  stop("Nama kab/kota ganda di Data 1: ",
       paste(dasar$kabkota[duplicated(dasar$key)], collapse = ", "))
}
if (anyNA(dasar$u5mr)) stop("Ada NA pada U5MR Data 1")

# =====================================================================
# T4. MERGE (kunci: nama)
# =====================================================================
message("[T4] Merge data ...")

# --- 4a. Data 2 -> Data 1 ---------------------------------------------
# Pemetaan nama Data 2 -> nama Data 1 (beda penulisan saja)
peta_d2 <- tribble(
  ~dari,                                          ~ke,
  "Toba Samosir / Toba",                          "Toba Samosir",
  "Kep. Seribu",                                  "Kepulauan Seribu",
  "Kotabaru",                                     "Kota Baru",
  "Kota Makasar",                                 "Kota Makassar",
  "Mamuju Utara / Pasangkayu",                    "Mamuju Utara",
  "Maluku Tenggara Barat / Kepulauan Tanimbar",   "Maluku Tenggara Barat"
) %>% mutate(dari_key = nama_norm(dari), ke_key = nama_norm(ke))

kk2 <- d2$kk %>%
  transmute(provinsi_d2 = provinsi, nama_d2 = nama, key0 = nama_norm(nama),
            balita = nilai) %>%
  mutate(key = coalesce(peta_d2$ke_key[match(key0, peta_d2$dari_key)], key0))

tak_cocok_d2 <- anti_join(kk2, dasar, by = "key")
if (nrow(tak_cocok_d2) > 0) {
  stop("Nama Data 2 yang tidak cocok dengan Data 1: ",
       paste(tak_cocok_d2$nama_d2, collapse = "; "))
}

# Wilayah Papua tercantum ganda (provinsi lama dan baru): pertahankan baris
# yang provinsinya SAMA dengan provinsi di Data 1.
n_kk2_awal <- nrow(kk2)
kk2 <- kk2 %>%
  inner_join(dasar %>% select(key, provinsi_d1 = provinsi), by = "key",
             relationship = "many-to-one") %>%
  filter(provinsi_d2 == provinsi_d1)
n_ganda_dibuang <- n_kk2_awal - nrow(kk2)
message("    Baris Data 2 ganda (batas provinsi lama) dibuang: ", n_ganda_dibuang)
if (anyDuplicated(kk2$key) > 0) {
  stop("Masih ada duplikat di Data 2 setelah penyaringan provinsi: ",
       paste(kk2$nama_d2[duplicated(kk2$key)], collapse = ", "))
}

hier <- dasar %>%
  left_join(kk2 %>% select(key, balita), by = "key") %>%
  left_join(tabel_pulau, by = "provinsi") %>%
  mutate(
    # "Kota Baru" adalah Kabupaten Kotabaru (Kalimantan Selatan), bukan kota
    jenis = if_else(str_starts(key, "kota ") & key != "kota baru", "Kota", "Kabupaten"),
    label = if_else(jenis == "Kabupaten", paste("Kab.", kabkota), kabkota),
    pulau = factor(pulau, levels = urutan_pulau)
  ) %>%
  mutate(peringkat_nasional = min_rank(desc(u5mr))) %>%
  group_by(provinsi) %>%
  mutate(peringkat_provinsi = min_rank(desc(u5mr))) %>%
  ungroup()

# Kelas U5MR: natural breaks (Jenks) dan kuantil, 5 kelas
brks_jenks <- classIntervals(hier$u5mr, n = 5, style = "jenks")$brks
brks_kuant <- unique(classIntervals(hier$u5mr, n = 5, style = "quantile")$brks)
if (length(brks_kuant) != 6) stop("Batas kuantil tidak unik; periksa sebaran U5MR")
beri_kelas <- function(x, brks) cut(x, breaks = brks, labels = FALSE, include.lowest = TRUE)
hier <- hier %>%
  mutate(kelas_jenks  = beri_kelas(u5mr, brks_jenks),
         kelas_kuantil = beri_kelas(u5mr, brks_kuant))
tabel_kelas <- function(brks, nama) {
  tibble(metode = nama, kelas = 1:5,
         batas_bawah = brks[1:5], batas_atas = brks[2:6]) %>%
    mutate(label = sprintf("%.1f - %.1f", batas_bawah, batas_atas))
}
kelas_breaks <- bind_rows(tabel_kelas(brks_jenks, "jenks"),
                          tabel_kelas(brks_kuant, "kuantil"))
saveRDS(kelas_breaks, file.path(DIR_OLAHAN, "kelas_breaks.rds"))

# --- 4b. Data 1 -> geojson --------------------------------------------
# Pemetaan nama Data 1 -> nama pada geojson (beda ejaan/penamaan)
peta_geo <- tribble(
  ~dari,                       ~ke,
  "Toba Samosir",              "Kabupaten Toba",
  "Labuhan Batu",              "Kabupaten Labuhanbatu",
  "Labuhan Batu Selatan",      "Kabupaten Labuhanbatu Selatan",
  "Labuhan Batu Utara",        "Kabupaten Labuhanbatu Utara",
  "Kota Tanjung Balai",        "Kota Tanjungbalai",
  "Kota Pematang Siantar",     "Kota Pematangsiantar",
  "Kota Sawah Lunto",          "Kota Sawahlunto",
  "Batang Hari",               "Kabupaten Batanghari",
  "Banyu Asin",                "Kabupaten Banyuasin",
  "Kota Lubuklinggau",         "Kota Lubuk Linggau",
  "Tulangbawang",              "Kabupaten Tulang Bawang",
  "Kepulauan Seribu",          "Kabupaten Administrasi Kepulauan Seribu",
  "Kota Jakarta Selatan",      "Kota Administrasi Jakarta Selatan",
  "Kota Jakarta Timur",        "Kota Administrasi Jakarta Timur",
  "Kota Jakarta Pusat",        "Kota Administrasi Jakarta Pusat",
  "Kota Jakarta Barat",        "Kota Administrasi Jakarta Barat",
  "Kota Jakarta Utara",        "Kota Administrasi Jakarta Utara",
  "Gunung Kidul",              "Kabupaten Gunungkidul",
  "Kota Palangka Raya",        "Kota Palangkaraya",
  "Kota Baru",                 "Kabupaten Kotabaru",
  "Kota Banjar Baru",          "Kota Banjarbaru",
  "Siau Tagulandang Biaro",    "Kabupaten Kep. Siau Tagulandang Biaro",
  "Tojo Una-Una",              "Kabupaten Tojo Una Una",
  "Kota Baubau",               "Kota Bau Bau",
  "Mamuju Utara",              "Kabupaten Pasangkayu",
  "Maluku Tenggara Barat",     "Kabupaten Kepulauan Tanimbar",
  "Fakfak",                    "Kabupaten Fak Fak"
) %>% mutate(dari_key = nama_norm(dari), ke_key = geo_key(ke))

hier <- hier %>%
  mutate(geo_key = coalesce(peta_geo$ke_key[match(key, peta_geo$dari_key)], key))

kol_prov_geo <- names(geo_mentah)[str_detect(names(geo_mentah), "Field6")]
kol_kode_geo <- names(geo_mentah)[str_detect(names(geo_mentah), "Field2")]
if (length(kol_prov_geo) != 1 || length(kol_kode_geo) != 1) {
  stop("Kolom Field6/Field2 pada geojson tidak ditemukan unik")
}

geo <- geo_mentah
if (is.na(st_crs(geo))) geo <- st_set_crs(geo, 4326)
geo <- geo %>%
  mutate(
    geo_key          = geo_key(name),
    jenis_geo        = if_else(str_starts(nama_norm(name), "kota "), "Kota", "Kabupaten"),
    provinsi_geojson = str_squish(.data[[kol_prov_geo]]),
    kode_bps         = as.character(.data[[kol_kode_geo]])
  ) %>%
  select(kode_bps, name_geojson = name, geo_key, jenis_geo, provinsi_geojson)
if (anyDuplicated(geo$geo_key) > 0) {
  stop("Kunci nama ganda pada geojson: ",
       paste(geo$name_geojson[duplicated(geo$geo_key)], collapse = ", "))
}

tak_cocok_hier <- setdiff(hier$geo_key, geo$geo_key)
tak_cocok_geo  <- setdiff(geo$geo_key, hier$geo_key)
if (length(tak_cocok_hier) + length(tak_cocok_geo) > 0) {
  stop("Nama belum cocok. Data 1 tanpa geojson: ",
       paste(tak_cocok_hier, collapse = "; "),
       " | geojson tanpa Data 1: ", paste(tak_cocok_geo, collapse = "; "))
}

geo_gabung <- geo %>%
  left_join(hier %>% select(geo_key, pulau, provinsi, kabkota, label, jenis,
                            u5mr, balita, peringkat_nasional, peringkat_provinsi,
                            kelas_jenks, kelas_kuantil),
            by = "geo_key")

# --- 4c. Simpan semua pemetaan yang dipakai ----------------------------
pemetaan_nama <- bind_rows(
  peta_d2  %>% transmute(sumber = "Data 2 -> Data 1", nama_asal = dari, nama_tujuan = ke),
  peta_geo %>% transmute(sumber = "Data 1 -> geojson", nama_asal = dari, nama_tujuan = ke)
)

# Kunci provinsi antar sumber (untuk menautkan multivariat, hierarki, peta)
ganti_prov_multi <- c("DI YOGYAKARTA" = "D I YOGYAKARTA", "KEP. RIAU" = "KEPULAUAN RIAU")
kunci_provinsi <- geo_gabung %>%
  st_drop_geometry() %>%
  distinct(provinsi_data1 = provinsi, provinsi_geojson) %>%
  arrange(provinsi_data1)
pemetaan_nama <- bind_rows(
  pemetaan_nama,
  tibble(sumber = "Provinsi multivariat -> Data 1",
         nama_asal = names(ganti_prov_multi), nama_tujuan = unname(ganti_prov_multi))
)
write_csv(pemetaan_nama, file.path(DIR_OLAHAN, "pemetaan_nama.csv"))

# =====================================================================
# T6 (bagian 1). DATA FINAL HIERARKI
# =====================================================================
message("[T6] Menyimpan data hierarki ...")
hier_final <- hier %>%
  select(pulau, provinsi, kabkota, label, jenis, key, u5mr, balita,
         peringkat_nasional, peringkat_provinsi, kelas_jenks, kelas_kuantil)

hier_provinsi <- hier %>%
  group_by(pulau, provinsi) %>%
  summarise(balita_provinsi = sum(balita), n_kabkota = n(), .groups = "drop") %>%
  left_join(d1$prov %>% rename(u5mr_resmi = nilai), by = "provinsi") %>%
  select(pulau, provinsi, u5mr_resmi, balita_provinsi, n_kabkota)

saveRDS(hier_final, file.path(DIR_OLAHAN, "hierarki_final.rds"))
write_csv(hier_final, file.path(DIR_OLAHAN, "hierarki_final.csv"))
saveRDS(hier_provinsi, file.path(DIR_OLAHAN, "hierarki_provinsi.rds"))

# =====================================================================
# T6 (bagian 2). GEOJSON: perbaiki, sederhanakan, simpan
# =====================================================================
message("[T6] Menyederhanakan dan menyimpan geojson ...")
geo_gabung <- st_make_valid(geo_gabung) %>% st_transform(4326)

# Tingkat penyederhanaan dicoba berurutan sampai ukuran <= BATAS_UKURAN_GEO
tingkat <- if (ada_rmapshaper) c(0.05, 0.04, 0.03, 0.02, 0.015, 0.01, 0.007, 0.005) else
  c(500, 1000, 2000, 3000, 5000, 8000, 12000, 20000)   # toleransi (meter)

sederhanakan <- function(x, nilai) {
  hasil <- if (ada_rmapshaper) {
    rmapshaper::ms_simplify(x, keep = nilai, keep_shapes = TRUE)
  } else {
    x %>% st_transform(3857) %>%
      st_simplify(dTolerance = nilai, preserveTopology = TRUE) %>%
      st_transform(4326)
  }
  st_make_valid(hasil)
}
tulis_geo <- function(x, path) {
  suppressMessages(st_write(x, path, delete_dsn = TRUE, quiet = TRUE,
                            layer_options = "COORDINATE_PRECISION=4"))
}

F_GEO_OUT <- file.path(DIR_OLAHAN, "kabkota_geo.geojson")
geo_final <- NULL
for (i in seq_along(tingkat)) {
  kandidat <- sederhanakan(geo_gabung, tingkat[i])
  if (any(st_is_empty(kandidat))) next          # jangan hilangkan wilayah
  tulis_geo(kandidat, F_GEO_OUT)
  ukuran <- file.size(F_GEO_OUT)
  geo_final <- kandidat
  message(sprintf("    tingkat %s -> %.2f MB",
                  format(tingkat[i]), ukuran / 1e6))
  if (ukuran <= BATAS_UKURAN_GEO) break
}
if (is.null(geo_final)) stop("Penyederhanaan geojson gagal (geometri kosong)")
ukuran_geo <- file.size(F_GEO_OUT)
if (ukuran_geo > BATAS_UKURAN_GEO) {
  warning("Ukuran geojson masih ", round(ukuran_geo / 1e6, 2),
          " MB (> batas). Pertimbangkan menambah tingkat penyederhanaan.")
}

# Titik pusat (di dalam poligon) untuk peta simbol proporsional
pusat <- geo_final %>%
  st_transform(3857) %>%
  { suppressWarnings(st_point_on_surface(.)) } %>%
  st_transform(4326)
xy <- st_coordinates(pusat)
pusat$lon <- xy[, 1]
pusat$lat <- xy[, 2]
pusat <- pusat %>%
  select(kabkota, label, jenis, provinsi, pulau, u5mr, balita,
         peringkat_nasional, peringkat_provinsi, kelas_jenks, kelas_kuantil,
         lon, lat)
saveRDS(pusat, file.path(DIR_OLAHAN, "centroid_kabkota.rds"))

# =====================================================================
# T7. MULTIVARIAT
# =====================================================================
message("[T7] Analisis multivariat ...")
multi <- multi_mentah
names(multi) <- str_squish(names(multi))      # nama kolom punya spasi di akhir
multi$Provinsi <- str_squish(multi$Provinsi)

info_var <- tibble(nama_asli = setdiff(names(multi), "Provinsi")) %>%
  mutate(
    nama_pendek = case_when(
      str_detect(nama_asli, "Kematian Kasar")      ~ "AKK",
      str_detect(nama_asli, "Kematian Bayi")       ~ "AKB",
      str_detect(nama_asli, "Kematian Balita")     ~ "AKABA",
      str_detect(nama_asli, "Kematian Anak")       ~ "AKA",
      str_detect(nama_asli, "Miskin")              ~ "Kemiskinan",
      str_detect(nama_asli, "Pengangguran")        ~ "TPT",
      str_detect(nama_asli, "Sanitasi")            ~ "Sanitasi",
      str_detect(nama_asli, "Air Minum")           ~ "AirMinum",
      TRUE ~ NA_character_
    ),
    kelompok = if_else(str_detect(nama_asli, "Kematian"), "Kematian", "Sosial-ekonomi")
  )
if (anyNA(info_var$nama_pendek) || anyDuplicated(info_var$nama_pendek) > 0 ||
    nrow(info_var) != 8) {
  stop("Pengenalan 8 variabel multivariat gagal; periksa nama kolom: ",
       paste(info_var$nama_asli, collapse = " | "))
}
names(multi)[match(info_var$nama_asli, names(multi))] <- info_var$nama_pendek

multi <- multi %>%
  mutate(provinsi_data1 = recode(Provinsi, !!!ganti_prov_multi)) %>%
  left_join(tabel_pulau, by = c("provinsi_data1" = "provinsi")) %>%
  mutate(pulau = factor(pulau, levels = urutan_pulau)) %>%
  rename(provinsi = Provinsi) %>%
  select(provinsi, provinsi_data1, pulau, all_of(info_var$nama_pendek))

# Kunci provinsi lengkap (multivariat, Data 1, geojson)
kunci_provinsi <- kunci_provinsi %>%
  left_join(multi %>% select(provinsi_multivariat = provinsi, provinsi_data1),
            by = "provinsi_data1") %>%
  left_join(tabel_pulau, by = c("provinsi_data1" = "provinsi")) %>%
  select(provinsi_data1, provinsi_multivariat, provinsi_geojson, pulau)
saveRDS(kunci_provinsi, file.path(DIR_OLAHAN, "kunci_provinsi.rds"))
write_csv(kunci_provinsi, file.path(DIR_OLAHAN, "kunci_provinsi.csv"))

vars <- info_var$nama_pendek
if (anyNA(multi[vars])) stop("Ada NA pada data multivariat")
z <- scale(as.matrix(multi[vars]))            # z-score (rata-rata 0, sd 1)
rownames(z) <- multi$provinsi

# --- PCA utama (8 variabel) -------------------------------------------
pca <- prcomp(multi[vars], center = TRUE, scale. = TRUE)
# Orientasikan PC1 agar positif = kematian balita lebih tinggi
if (cor(pca$x[, 1], z[, "AKABA"]) < 0) {
  pca$x[, 1] <- -pca$x[, 1]
  pca$rotation[, 1] <- -pca$rotation[, 1]
}
skor <- as_tibble(pca$x) %>%
  mutate(provinsi = multi$provinsi, .before = 1)
loading <- as_tibble(pca$rotation, rownames = "variabel") %>%
  left_join(info_var, by = c("variabel" = "nama_pendek"))
varians <- tibble(
  pc = paste0("PC", seq_along(pca$sdev)),
  sd = pca$sdev,
  proporsi = pca$sdev^2 / sum(pca$sdev^2)
) %>% mutate(kumulatif = cumsum(proporsi))
message("    Varians terjelaskan PC1 = ", round(varians$proporsi[1] * 100, 1),
        "%, PC2 = ", round(varians$proporsi[2] * 100, 1), "%")

# --- Uji sensitivitas: tanpa AKA dan AKABA -----------------------------
vars_sens <- setdiff(vars, c("AKA", "AKABA"))
pca_s <- prcomp(multi[vars_sens], center = TRUE, scale. = TRUE)
pc1_s <- pca_s$x[, 1]
if (cor(pc1_s, pca$x[, 1]) < 0) pc1_s <- -pc1_s   # samakan tanda PC1
rho <- suppressWarnings(cor(pca$x[, 1], pc1_s, method = "spearman"))
sensitivitas <- tibble(
  provinsi = multi$provinsi,
  pc1_utama = pca$x[, 1],
  pc1_tanpa_AKA_AKABA = pc1_s,
  urutan_utama = rank(-pca$x[, 1]),
  urutan_sens  = rank(-pc1_s)
)
message("    Sensitivitas PC1 (tanpa AKA & AKABA): Spearman rho = ", round(rho, 3))

# --- Klaster k-means (k = 2..6), pilih rata-rata silhouette terbesar ----
jarak <- dist(z)
tabel_sil <- map_dfr(2:6, function(k) {
  set.seed(2026)
  km <- kmeans(z, centers = k, nstart = 50)
  tibble(k = k, silhouette_rata2 = mean(silhouette(km$cluster, jarak)[, "sil_width"]),
         tot_withinss = km$tot.withinss)
})
k_pilih <- tabel_sil$k[which.max(tabel_sil$silhouette_rata2)]
message("    k terpilih (silhouette): ", k_pilih)
print(tabel_sil)

set.seed(2026)
km <- kmeans(z, centers = k_pilih, nstart = 50)
# Beri nomor klaster berurutan menurut rata-rata AKABA (1 = terendah)
urut_klaster <- tibble(klaster_asli = km$cluster, AKABA = multi$AKABA) %>%
  group_by(klaster_asli) %>% summarise(m = mean(AKABA), .groups = "drop") %>%
  arrange(m) %>% mutate(klaster = row_number())
klaster <- urut_klaster$klaster[match(km$cluster, urut_klaster$klaster_asli)]

# Urutan hierarkis (Ward.D2) untuk heatmap terklaster
hc_prov <- hclust(jarak, method = "ward.D2")
hc_var  <- hclust(dist(t(z)), method = "ward.D2")

# Pencilan: |skor-z| > 2 pada PC1 atau PC2
z_pc1 <- as.numeric(scale(pca$x[, 1]))
z_pc2 <- as.numeric(scale(pca$x[, 2]))
tabel_klaster <- tibble(
  provinsi = multi$provinsi, provinsi_data1 = multi$provinsi_data1,
  pulau = multi$pulau, klaster = factor(klaster),
  PC1 = pca$x[, 1], PC2 = pca$x[, 2],
  z_PC1 = z_pc1, z_PC2 = z_pc2,
  pencilan = abs(z_pc1) > 2 | abs(z_pc2) > 2
)
ringkas_klaster <- multi %>%
  mutate(klaster = klaster) %>%
  group_by(klaster) %>%
  summarise(n = n(), across(all_of(vars), mean), .groups = "drop")

saveRDS(multi, file.path(DIR_OLAHAN, "provinsi_multivariat.rds"))
saveRDS(list(skor = skor, loading = loading, varians = varians,
             info_var = info_var, sensitivitas = sensitivitas,
             spearman_pc1 = rho, z = z),
        file.path(DIR_OLAHAN, "hasil_pca.rds"))
saveRDS(list(tabel = tabel_klaster, ringkas = ringkas_klaster,
             silhouette = tabel_sil, k = k_pilih,
             urutan_provinsi = hc_prov$labels[hc_prov$order],
             urutan_variabel = hc_var$labels[hc_var$order],
             hc_provinsi = hc_prov, hc_variabel = hc_var),
        file.path(DIR_OLAHAN, "hasil_klaster.rds"))

# =====================================================================
# T5. VALIDASI
# =====================================================================
message("[T5] Validasi ...")

# 1. Hierarki final
catat("hierarki_final = 514 baris", nrow(hier_final) == 514, nrow(hier_final))
catat("tanpa duplikat kunci kab/kota", anyDuplicated(hier_final$key) == 0)
catat("tanpa NA pada u5mr dan balita",
      !anyNA(hier_final$u5mr) && !anyNA(hier_final$balita))
catat("38 provinsi", n_distinct(hier_final$provinsi) == 38,
      n_distinct(hier_final$provinsi))
catat("7 kelompok pulau", n_distinct(hier_final$pulau) == 7,
      n_distinct(hier_final$pulau))
catat("tiap kab/kota punya pulau", !anyNA(hier_final$pulau))

# 2. Total balita nasional
catat("sum(balita) == baris INDONESIA (Data 2)",
      isTRUE(all.equal(sum(hier_final$balita), d2$indo)),
      paste(sum(hier_final$balita), "vs", d2$indo))

# 3. Total balita per provinsi vs baris provinsi Data 2
#    (pengecualian: PAPUA dan PAPUA BARAT, karena batas provinsi lama)
cek_prov <- hier_provinsi %>%
  select(provinsi, balita_provinsi) %>%
  left_join(d2$prov %>% rename(balita_d2 = nilai), by = "provinsi") %>%
  mutate(selisih = balita_provinsi - balita_d2,
         dikecualikan = provinsi %in% c("PAPUA", "PAPUA BARAT"))
catat("balita per provinsi == Data 2 (kecuali PAPUA, PAPUA BARAT)",
      all(cek_prov$selisih[!cek_prov$dikecualikan] == 0),
      paste(sum(!cek_prov$dikecualikan & cek_prov$selisih != 0), "provinsi berselisih"))
catat("pengecualian dicatat: PAPUA & PAPUA BARAT memakai batas provinsi lama di Data 2",
      TRUE,
      paste(cek_prov %>% filter(dikecualikan) %>%
              transmute(x = paste0(provinsi, " (selisih ", selisih, ")")) %>% pull(x),
            collapse = "; "))

# 4. U5MR provinsi Data 1 == "Angka Kematian Balita" data_multivariat
cek_multi <- d1$prov %>%
  inner_join(multi %>% select(provinsi_data1, AKABA), by = c("provinsi" = "provinsi_data1"))
catat("U5MR provinsi Data 1 identik dengan multivariat (38 provinsi)",
      nrow(cek_multi) == 38 && all(abs(cek_multi$nilai - cek_multi$AKABA) < 0.005),
      paste(nrow(cek_multi), "provinsi dibandingkan"))
catat("multivariat: 38 provinsi, 8 variabel, tanpa NA",
      nrow(multi) == 38 && length(vars) == 8 && !anyNA(multi[vars]))
catat("semua provinsi multivariat terpetakan ke Data 1",
      all(multi$provinsi_data1 %in% d1$prov$provinsi))

# 5. Geojson
geo_baca <- st_read(F_GEO_OUT, quiet = TRUE)
catat("geojson final = 514 fitur", nrow(geo_baca) == 514, nrow(geo_baca))
catat("tiap kab/kota tergabung tepat sekali ke geojson",
      !anyNA(geo_gabung$u5mr) && anyDuplicated(geo_gabung$geo_key) == 0 &&
        setequal(geo_gabung$geo_key, hier$geo_key))
cek_nilai <- st_drop_geometry(geo_baca) %>%
  select(kabkota, u5mr_geo = u5mr, balita_geo = balita) %>%
  inner_join(hier_final %>% select(kabkota, u5mr, balita), by = "kabkota")
catat("u5mr & balita di geojson identik dengan hierarki_final",
      nrow(cek_nilai) == 514 &&
        all(abs(cek_nilai$u5mr_geo - cek_nilai$u5mr) < 1e-6) &&
        all(abs(cek_nilai$balita_geo - cek_nilai$balita) < 1e-6))
catat("jenis (Kota/Kabupaten) konsisten antara Data 1 dan geojson",
      all(st_drop_geometry(geo_gabung)$jenis == geo$jenis_geo[match(geo_gabung$geo_key, geo$geo_key)]),
      paste(sum(hier_final$jenis == "Kota"), "kota"))
catat("tidak ada geometri kosong", !any(st_is_empty(geo_final)))
catat(sprintf("ukuran geojson <= %.1f MB", BATAS_UKURAN_GEO / 1e6),
      ukuran_geo <= BATAS_UKURAN_GEO, sprintf("%.2f MB", ukuran_geo / 1e6))
catat("centroid: 514 titik", nrow(pusat) == 514)

# 6. Cek 10 baris acak
cat("\n--- 10 baris acak hasil merge ---\n")
set.seed(2026)
print(hier_final %>%
        select(pulau, provinsi, kabkota, jenis, u5mr, balita, peringkat_nasional) %>%
        slice_sample(n = 10), width = Inf)

cat("\n--- Hasil validasi ---\n")
print(hasil_validasi, n = Inf, width = Inf)
write_csv(hasil_validasi, file.path(DIR_OLAHAN, "log_validasi.csv"))

# =====================================================================
# T8. METADATA SUMBER (URL dan tanggal akses DIISI MANUAL)
# =====================================================================
metadata <- tribble(
  ~judul_tabel,                                                         ~tahun, ~url, ~tanggal_akses,
  "Angka Kematian Balita Menurut Kabupaten/Kota, Hasil SUPAS",          "2025",  "",   "",
  "Jumlah Penduduk menurut Kabupaten/Kota dan Kelompok Umur (0-4)",     "2025",  "",   "",
  "Data multivariat provinsi (8 variabel; isi judul tabel masing-masing)", "",    "",   "",
  "Batas wilayah kab/kota 2025 (geojson; isi asal data)",               "2025",  "",   ""
)
write_csv(metadata, file.path(DIR_OLAHAN, "metadata_sumber.csv"), na = "")

# ---------------------------------------------------------------------
# Hentikan bila ada validasi yang gagal
# ---------------------------------------------------------------------
gagal <- hasil_validasi %>% filter(status == "GAGAL")
if (nrow(gagal) > 0) {
  stop("VALIDASI GAGAL pada: ", paste(gagal$cek, collapse = " | "),
       "\nLihat ", file.path(DIR_OLAHAN, "log_validasi.csv"), call. = FALSE)
}

message("\nSelesai. Semua validasi LULUS. Keluaran ada di ", DIR_OLAHAN, "/:")
print(list.files(DIR_OLAHAN))
