library(readr)
# This is a shortcut to download data directly from the course website.
# To work offline, download the file to a folder on your computer
# and edit the path variable to point to that folder. Or you
# can use RStudio's import tool like in the tutorials in HW 0
path = "https://jaredsmurray.github.io/sta371g_s19/data/"
waite = read_csv(paste0(path, 'waite.csv'))
