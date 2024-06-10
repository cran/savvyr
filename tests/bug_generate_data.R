

haz <- c(0.2, 0.3, 0.5)
haz
haz_all <- sum(haz)
#new code, produces correct outputs
table(sample(1:3, size = 10^6, prob = haz / haz_all, replace = TRUE))/10^6
#old code, probs. dont match hazards
table(1L + stats::rbinom(n = 10^6, size = 2, prob = haz/haz_all))/10^6
