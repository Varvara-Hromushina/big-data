n1 <- (1:50)

sum1 <- sum(1 / (n1 * (n1 + 1)))
sum1

n2 <- (0:20)

sum2 <- sum(1 / 2 ^ n2)
sum2

numerators <- seq(from = 1, to = 28, by = 3)
denominators <- 3 ^ (0:(length(numerators) - 1))
sequence <- numerators / denominators

sum3 <- sum(sequence)
sum3

count_gt_05 <- sum(sequence > 0.5)
count_gt_05

sequence


