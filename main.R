# library for data frame melting
library(reshape2)
# to work with data.frame easily
library(data.table)
# to plot the data
library(ggplot2)

# import data
d <- read.csv("AllLipids.csv", stringsAsFactors = F)

##### PREPARE DATA FOR ANOVA
d <- d[2:611, c(2,3,5,7,9,11,13,15,17,19,21,23,25)]
# melt wide data.frame to make a long data.frame
d <- melt(d, id = "X")
# convert factors to characters
d$variable <- as.character(d$variable)
d$X <- as.character(d$X)
d$value <- as.numeric(d$value)
# make a data table
d <- as.data.table(d)
# rename conditions
d[grepl("DRAL", variable),variable:="DRAL"]
d[grepl("ALDR", variable),variable:="ALDR"]
d[grepl("DR1", variable),variable:="DR"]
d[grepl("AL1", variable),variable:="AL"]

##### PERFORM ANOVA
aov.res <- d[, list(pval = summary(aov(value ~ variable))[[1]]$`Pr(>F)`[1]), by = "X"]
aov.res$padj <- p.adjust(aov.res$pval, method = "BH")
d.sig <- aov.res[padj<0.05]
setkey(d.sig, "X")

##### EXPORT AND PLOT THE DATA
plot <- d
setkey(plot, "X")
plot <- plot[d.sig]
plot <- plot[order(padj)]
plot$X <- factor(plot$X, levels = unique(plot$X))
# export to csv
write.csv(plot, file = "sig-lipids.csv", quote = F, row.names = F)
# use ggplot2 to plot boxplots to a file
p <- ggplot(plot, aes(variable, value)) +
	geom_boxplot() +
	facet_wrap(~ X, scale="free") +
	theme_bw()
ggsave(p, file = "sig-lipids-boxplots.pdf", w = 25, h = 25)
# find mean and sd of the replicates and plot them instead of boxplots
plot.mean <- plot[,list(mean = mean(value), sd = sd(value)), by = c("X", "variable", "pval", "padj")]
p <- ggplot(plot.mean, aes(variable, mean)) +
	geom_boxplot() +
	geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), width = 0.25) +
	facet_wrap(~ X, scale="free") +
	theme_bw()
ggsave(p, file = "sig-lipids-means.pdf", w = 25, h = 25)
