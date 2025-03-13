kordat <- read.table("variants1.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE, strip.white = TRUE, row.names=1,dec=",")



factor_columns <- names(kordat)[9:ncol(kordat)]
kordat[factor_columns] <- lapply(kordat[factor_columns], as.factor)

sink(file = "results.txt", append=FALSE)

for (i in 9:ncol(kordat)) {
  cat("Faktors:", colnames(kordat)[i], "\n")
  print(table(kordat[, i]))     
  cat("\n")
}
sink()

sl.by.b <- split(kordat$Slope, kordat$b)
sink(file = "results.txt", append = TRUE)
cat("\nSadalītā Slope vērtības pēc b faktora:\n")
print(sl.by.b)
sink()


num_columns <- c("Slope", "Intercept", "adj.r.squared")
kordat[num_columns] <- lapply(kordat[num_columns], function(x) as.numeric(as.character(x)))
kordat$Average <- rowMeans(kordat[, num_columns], na.rm = TRUE)

cat("Standartnovirze pēc f faktora līmeņiem:\n")
print(tapply(kordat$Slope, kordat$f, sd, na.rm = TRUE))

prockordat <- kordat[kordat$adj.r.squared > 0.7, ]

prockordat$Slope <- 1 - 1 / prockordat$Slope

sink("results.txt", append = TRUE)
cat("\nprockordat dati:\n")
print(prockordat)
sink()

svg("scatter.svg") 
plot(kordat$MAD, kordat$Average, xlab = "MAD", ylab = "Average", main = "Izkliedes grafiks", pch = 19, col = "violet")  
dev.off()

svg("boxplot.svg")  
boxplot(Intercept ~ factor(f), data = kordat, xlab = "F faktors", ylab = "Intercept", main = "Sagrupēts kastes grafiks", col = rainbow(length(unique(kordat$f))))
dev.off()


factor_levels <- unlist(kordat[, factor_columns])
most_frequent_level <- names(sort(table(factor_levels), decreasing = TRUE))[1]
cat("Visbiežāk sastopamais faktora līmenis:", most_frequent_level, "\n")

filtered_prockordat <- prockordat[grep(most_frequent_level, rownames(prockordat)), ]
print(filtered_prockordat)