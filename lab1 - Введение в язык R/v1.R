g <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)
g

g[1]

g[length(g)]

g[3:5]

g[!is.na(g) & g == 2]

g[!is.na(g) & g > 4]

g[!is.na(g) & g %% 3 == 0]

g[!is.na(g) & g > 4 & g %% 3 == 0]

g[!is.na(g) & (g < 1 | g > 5)]

which(g == 0)

which(g >= 2 & g <= 8) 



