# Prediksi Kasus Dengue

Program ini dipublikasikan menggunakan library [Shiny.io](https://pages.github.com/](https://shiny.posit.co/)https://shiny.posit.co/).
Program ini dikembangkan menggunakan aplikasi [R](https://cran.r-project.org/) dan merupakan hasil peneilitian yang dipresentasikan pada [the 6th Asean Dengue Summit](https://www.asiadenguesummit.org/wp-content/uploads/6th-Asia-Dengue-Summit-Programme-Book.pdf) di Thailand pada Juni 2023. Hasil ini merupakan kolaborasi antara [Kementerian Kesehatan Republik Indonesia](https://www.kemkes.go.id/), [Malaria Consortium](https://www.malariaconsortium.org/where-we-work/thailand.htm), dan [Public Health Litearture Club](https://www.instagram.com/publichealth.literatureclub/).

## Sumber Data:
- [Data keseluruhan cuaca BMKG](https://dataonline.bmkg.go.id/) dan dipilih per stasiun, lalu data diekstrak ke excel per bulan dari Januari 2018 sampai Desember 2022.
- Data Kasus Dengue
- Data Larva Free Index

## Trained Model
- Regression Tree
- Support Vector Regression
- Random Forest
- Lasso Regression
- Ridge Regression
- Linear Regression

## Used Model
Model dipilih berdasarkan RMSE dan MAE. Sehingga model yang diaplikasin pada program ini adalah Support Vector Regression.

Untuk mengakses lama website prediksi, silahkan klik [disini](https://himhariss.shinyapps.io/PrediksiDengue/)

### Tampilan website sebagaimana berikut:
![Screenshot web](https://github.com/himharis/himharis_web/blob/main/PrediksiDengue/www/Prototype%20Prediksi%20Dengue.png)
