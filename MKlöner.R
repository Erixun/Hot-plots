

library(ggplot2)
library(reshape2)

#setwd("~/Documents/R stuff/MKlöner")
MKlöner <- read.csv("MKlöner.csv") # data från Statistiska Centralbyrån (SCB)

# Skapar df i lämpligt format
mMKlöner <- melt(data = MKlöner, value.name = "Lön", id.vars = 1, variable.name="År")
rm(MKlöner)
mMKlöner$År <- as.factor(gsub("X", "", mMKlöner$År))
mMKlöner$kon <- factor(as.character(mMKlöner$kon), levels = c("man", "kvinnor") )
names(mMKlöner)[1] <- "Kön"
levels(mMKlöner$Kön) <- c("Mäns", "Kvinnors")

# Beräknar andelar i % av mäns snittlöner och skapar df:
Mandel <- MKlöner[1,2:29]/MKlöner[1,2:29]*100
Kandel <- MKlöner[2,2:29]/MKlöner[1,2:29]*100
MKprop <- as.data.frame(t(rbind(Mandel, Kandel)))
rm(Kandel, Mandel)
names(MKprop) <- c("Mäns", "Kvinnors")
rownames(MKprop) <- gsub("X", "",rownames(MKprop))
MKprop$År <- as.factor(rownames(MKprop))
mMKprop <- melt(MKprop, id.vars = "År"); rm(MKprop)
names(mMKprop) <- c("År", "Kön", "%")

# Lägger till "tom" fejkdata:
fejkDataf <- as.data.frame(list(Mäns = rep(100, 32), Kvinnors = rep(0, 32), År = as.character(seq(2019, 2050)) ) )
framtidf <- rbind(MKprop, fejkDataf)
rm(fejkDataf, framtidf)
framtidfm <- melt(framtidf[10:60,], id.vars = "År")
framtidfm$År <- factor(framtidfm$År, levels = framtidfm$År[1:51])
names(framtidfm) <- c("År", "Kön", "%")
framtidKv <- framtidfm[52:70,] # trendlinje-data (% för kvinnor år 2000-2018)

# Plottar snittlön per kön som % av männens, år 2000-2018 (-2050)
gpt <- ggplot(framtidfm, aes(x=År, y = `%`, fill = Kön )) + geom_col(position="identity", alpha=0.5) + ggtitle("Snittlön per kön i % av männens, år 2000-2018 (-2050)")
gpt <- gpt + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size=10, face="bold"), legend.title = element_blank() )
gpt <- gpt + stat_smooth(data=framtidKv, method="lm", aes(y=`%`, group=1), fullrange = T, se=T, show.legend = F, colour="black",alpha = 0.5, size=0.1, linetype="dashed")
gpt + labs(tag = "Datakälla: SCB") + theme(plot.tag.position = c(0.95, 0.005), plot.tag = element_text(size=7), axis.title.y = element_text(), axis.text.y = element_text(size=9, face = "bold"), panel.background = element_blank(), panel.grid.major.x = element_blank(), panel.spacing.x = unit(c(0,0,0,0), "mm"), axis.line = element_line(colour = "black")) + scale_y_continuous(expand=c(0,0), breaks=seq(0, 100, 10)) + scale_x_discrete(breaks=levels(framtidfm$År)[seq(1, 51, by=10)]  ) +
scale_fill_manual(values = c("LightSteelBlue", "Tan")) 

# Plottar snittlön per kön i Sverige, år 1991-2018
ggplot(mMKlöner, aes(x=År, y=Lön, group=Kön, fill = Kön)) + ylab("Lön (kr)") + ggtitle("Snittlön per kön i Sverige, år 1991-2018") + geom_area(position = "dodge", alpha=0.5) +
  scale_y_continuous(expand=c(0,0), breaks=seq(0, 40000, 2000)) + scale_x_discrete(expand = c(0,0), breaks=levels(mMKlöner$År)[seq(4, 26, by=3)]  ) +
  labs(tag = "Datakälla: SCB") + theme(plot.tag.position = c(0.95, 0.005), plot.tag = element_text(size=7), legend.title = element_blank(), plot.title = element_text(hjust=0.5)) + scale_fill_manual(values = c("LightSteelBlue", "Tan"))
