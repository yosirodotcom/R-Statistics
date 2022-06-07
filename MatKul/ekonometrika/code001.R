# ═══════════• ೋ•✧๑♡๑✧•ೋ •═════════════
#               Persiapan Data
# ═══════════• ೋ•✧๑♡๑✧•ೋ •═════════════


# Install dan load package yang dibutuhkan
install.packages("pacman")
pacman::p_load(pacman, dplyr, ggplot2, AER, MASS, plotly)

p_data(MASS) # untuk melihat contoh tabel data yang terinstall bersamaan dengan package MASS
data(survey) # Load contoh tabel data dari package MASS

glimpse(survey) # untuk melihat struktur data
?survey # untuk melihat keterangan lebih lanjut tentang data "survey"
View(survey)

p <- ggplot(survey, aes(x = Height, y = Wr.Hnd)) + 
        geom_point(shape = 1) +
        geom_smooth(method=lm)
ggplotly(p)


plotly