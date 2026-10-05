# Sebelum Usia Lima: Peta Kematian Balita Indonesia

Web story interaktif (R + Quarto) untuk UAS Visualisasi Data dan Informasi 2026, Politeknik Statistika STIS.

Lima tahun pertama seharusnya menjadi awal kehidupan, tetapi tidak semua anak memiliki peluang yang sama untuk mencapainya. Web story ini menelusuri **di mana anak-anak paling rentan, dan mengapa**, dari tingkat provinsi hingga kabupaten/kota, menggunakan Angka Kematian Balita (U5MR) per 1.000 kelahiran hidup.

| | |
|---|---|
| **Web story** | https://lisafajrianti0.github.io/UAS_VISDAT/ |
| **Repositori** | https://github.com/lisafajrianti0/UAS_VISDAT |
| **Penulis** | Lisa Fajrianti (222313174) |
| **Mata kuliah** | Visualisasi Data dan Informasi, Politeknik Statistika STIS, 2026 |

## Alur cerita

| Bagian | Pertanyaan yang dijawab |
|---|---|
| 00 · Titik awal | Seberapa lebar kesenjangan U5MR di dalam satu provinsi? |
| 01 · Peta risiko | Di mana risiko tertinggi berada, dan apakah polanya mengelompok secara spasial? |
| 02 · Tempat anak-anak tinggal | Di mana sebagian besar balita Indonesia tinggal? |
| 03 · Risiko bertemu jumlah | Apakah risiko tertinggi dan jumlah anak terbanyak berada di wilayah yang sama? |
| 04 · Teman seperjalanan | Faktor sosial-ekonomi apa yang menyertai kematian balita? |
| 05 · Senasib sepenanggungan | Provinsi mana yang memiliki profil serupa? |
| 06 · Dari angka ke langkah | Daerah mana yang layak diprioritaskan lebih dulu? |
| 07 · Penutup | Apa yang tertinggal setelah semua angka ini? |

## Topik visualisasi (3 dari 6)

| Topik | Teknik | Data |
|---|---|---|
| Geospasial | Choropleth (Jenks vs kuantil) dan proportional symbol; peta LISA dan diagram sebar Moran; tooltip, zoom/pan, legenda, kontrol layer | 514 kab/kota |
| Hierarki | Treemap dan sunburst, 3 tingkat (pulau, provinsi, kab/kota), drill-down dengan breadcrumb | 514 kab/kota |
| Multivariat | Biplot PCA, koordinat paralel, heatmap terklaster; brushing & linking | 38 provinsi x 8 variabel |

## Temuan utama

- **Kesenjangan terlebar ada di dalam provinsi.** Di Papua Barat Daya, U5MR kab/kota berkisar dari 21,6 (Kota Sorong) hingga 57,1 (Kab. Tambrauw), selisih 35,5.
- **121 dari 514 kab/kota** masih berada di atas target SDG 3.2 (U5MR ≤ 25).
- **Risiko ekstrem mengelompok secara spasial** (Moran's I = 0,74; p ≤ 0,001). 57% daerah kelas U5MR tertinggi berada di tiga provinsi Papua.
- **Jumlah anak terpusat di Jawa dan Sumatera** (75,2% balita), sedangkan dua kelas U5MR tertinggi hanya memuat 3,8% balita.
- **Kematian balita beriringan dengan kemiskinan dan keterbatasan sanitasi.** PC1 menjelaskan 61,7% variasi antarprovinsi (kemiskinan r = 0,86; sanitasi layak r = -0,73). Korelasi tidak menunjukkan sebab-akibat.
- **Klaster U5MR tertinggi** terdiri dari 6 provinsi di Pulau Papua, dengan rerata U5MR 2,2 kali klaster terendah.
- **Daftar prioritas bersifat konservatif:** hanya 5 daerah yang berisiko di atas target SDG sekaligus berbalita banyak.

## Struktur repositori

```
UAS_VISDAT/
├── index.qmd                  # web story (kode visualisasi)
├── custom.scss                # tema gelap
├── _story.html                # animasi scroll, counter, navigasi
├── _quarto.yml                # konfigurasi proyek, output ke docs/
├── README.md
├── Soal UAS Visualisasi Data dan Informasi 2026.pdf
│
├── R/
│   └── 01_olah_data.R         # skrip pengolahan data
│
├── data/
│   │  # Data mentah dan acuan
│   ├── data geospasial.xlsx           # data U5MR dan balita kab/kota
│   ├── Kode_Wilayah_BPS.xlsx          # kode wilayah BPS
│   ├── uas visdat_ batas wilayah kabkot.qgz   # proyek QGIS batas wilayah
│   ├── kabkota_geo.geojson            # batas wilayah digital kab/kota
│   ├── metadata_sumber.csv            # judul tabel, tahun, URL, tanggal akses
│   │
│   │  # Pemetaan dan kunci
│   ├── pemetaan_nama.csv              # pemetaan nama wilayah
│   ├── kunci_provinsi.csv / .rds      # kunci penghubung provinsi
│   │
│   │  # Hasil pengolahan
│   ├── centroid_kabkota.rds           # sentroid kab/kota (bobot spasial)
│   ├── kelas_breaks.rds               # batas kelas Jenks dan kuantil
│   ├── hierarki_final.csv / .rds      # data hierarki kab/kota (treemap, sunburst)
│   ├── hierarki_provinsi.rds          # data hierarki tingkat provinsi
│   ├── provinsi_multivariat.rds       # 38 provinsi x 8 variabel
│   ├── hasil_pca.rds                  # hasil PCA
│   ├── hasil_klaster.rds              # hasil k-means
│   │
│   │  # Log
│   ├── log_struktur.txt               # log pengecekan struktur data
│   └── log_validasi.csv               # log validasi data
│
└── docs/                      # hasil render untuk GitHub Pages
    ├── index.html
    ├── index_files/libs/
    └── .nojekyll
```

## Reproduksi

Prasyarat: [R](https://www.r-project.org/) dan [Quarto](https://quarto.org/docs/get-started/).

Install paket R:

```r
install.packages(c(
  "dplyr", "tidyr", "readr", "tibble",
  "sf", "spdep",
  "leaflet", "plotly", "crosstalk", "DT",
  "htmltools", "htmlwidgets"
))
```

Render web story:

```bash
quarto render
```

Hasil render tersimpan di `docs/` (`index.html` beserta folder `index_files/libs`).

## Deploy

Web story dipublikasikan dengan GitHub Pages.
File `docs/.nojekyll` diperlukan agar folder `index_files` terbaca dengan benar. Setiap kali kode diubah, jalankan `quarto render` lalu unggah ulang isi `docs/`.

## Data

Data utama berasal dari BPS. Rincian judul tabel, tahun, URL, dan tanggal akses ada di `data/metadata_sumber.csv`.

| Data | Sumber | Tingkat |
|---|---|---|
| Angka Kematian Balita (SUPAS 2025) | BPS | Kab/kota |
| Jumlah Penduduk Kelompok Umur 0-4 (2025) | BPS | Kab/kota |
| Indikator kematian (AKK, AKB, AKABA, AKA), kemiskinan, TPT, sanitasi layak, air minum layak (2025) | BPS | Provinsi |
| Batas wilayah digital kab/kota 2025 | Data pendukung, non-BPS | Kab/kota |
| Target SDG 3.2 (U5MR ≤ 25) | PBB, non-BPS | Acuan |

Pengolahan (PCA, k-means, kelas Jenks/kuantil, Moran's I dan LISA) dilakukan oleh penulis dan hasilnya tersimpan di `data/*.rds`. Skrip pengolahan ada di folder `R/`.

## Catatan metode

- **Klasifikasi peta:** natural breaks (Jenks) dan kuantil disediakan berdampingan karena pola peta bergantung pada metode klasifikasi.
- **Autokorelasi spasial:** bobot 5 tetangga terdekat berdasarkan jarak antar-sentroid (baris distandarkan), signifikansi diuji dengan 999 permutasi. LISA memakai α = 0,05 tanpa koreksi uji berganda sehingga bersifat eksploratif.
- **U5MR pulau dan provinsi** dihitung sebagai rerata tertimbang jumlah balita dari kab/kota, sehingga dapat berbeda dari angka resmi provinsi.
- **Klaster:** k-means pada data terstandar, k = 2 dipilih berdasarkan silhouette tertinggi (0,51).
- **Aksesibilitas warna:** palet ColorBrewer (sekuensial dan divergen) serta Okabe-Ito (klaster) yang aman bagi penderita buta warna.

## Deklarasi penggunaan AI

Kerangka kode Quarto/CSS/JS disusun dengan bantuan alat AI (Claude). Pemilihan tema, data, analisis, dan interpretasi menjadi tanggung jawab penulis.
