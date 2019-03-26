# Run this code to load the data into your R session.
# Or visit the website and download the code, and then import. This
# is only necessary if you want to work offline. You'll see a bunch of red text 
# after running the code, but if it matches what you see below 
# (starting from ## Parsed with column specification... )
# it's nothing to worry about.
library(readr)
path = "https://jaredsmurray.github.io/sta371g_s19/data/"
milk = read_csv(paste0(path, 'milk.csv'))

