list.of.packages <- c("ggplot2", "showtext", "reshape2", "plyr", "stringr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for (package in list.of.packages) {
    library(package, character.only=TRUE)
}

monophthongs <- c()
ipa <- {}

map <- "heed /i:/ hid /ɪ/ head /e/ heard /ɜ:/ had /æ/ hud /ʌ/ hard /ɑ:/ hod /ɒ/ hoard /ɔ:/ whod /u:/ hood /ʊ/"

matches <- as.data.frame(str_match_all(map, "([a-z]+)\\s+/([^/]{1,2})/")[[1]])
invisible(apply(matches, 1, FUN=function(row){
    ipa[row["V2"]] <<- row["V3"]
    monophthongs <<- c(monophthongs, row["V2"])
}))

vwl.df <- rbind(
    read.csv("vowels-pre-LV.csv"),
    read.csv("vowels-post-LV.csv"),
    read.csv("vowels-pre-HV.csv"),
    read.csv("vowels-post-HV.csv"))
vwl.df <- vwl.df[vwl.df$vowel %in% monophthongs,]
vwl.df$speaker <- sapply(vwl.df$participant, FUN=function(x) sub("(C[0-9]+).*?([LH]V)", "\\1\\2", x))

names(vwl.df)[names(vwl.df) == "test"] <- "Test"
names(vwl.df)[names(vwl.df) == "group"] <- "Group"

lob.df <- ddply(vwl.df, c("Group", "Test", "speaker"), function(speaker) {
    f1s <- speaker$f1.50
    f2s <- speaker$f2.50
    mean.f1s <- mean(f1s)
    mean.f2s <- mean(f2s)
    sd.f1s <- sd(f1s)
    sd.f2s <- sd(f2s)
    ddply(speaker, c("vowel"), function(vowel) {
        f1v <- vowel$f1.50
        f2v <- vowel$f2.50

        norm.f1v <- (f1v - mean.f1s) / sd.f1s
        norm.f2v <- (f2v - mean.f2s) / sd.f2s

        data.frame(f1=mean(norm.f1v), f2=mean(norm.f2v))
    })
})

write.csv(lob.df, "vowels-lob.csv", row.names=FALSE, quote=FALSE)

font_add("Cabin", "../fonts/Cabin/Cabin-Regular.ttf")
font_add("DejaVuSans", "../fonts/dejavu-fonts-ttf-2.37/ttf/DejaVuSans.ttf")
showtext_auto()

lob.mn.df <- ddply(lob.df, c("Group", "Test", "vowel"), function(subset) {
    data.frame(f1=mean(subset$f1), f2=mean(subset$f2))
})

# Do plot
df.melt <- melt(lob.mn.df,  id.var = c("vowel", "Group", "Test"))
df <- dcast(df.melt, Group+vowel~variable+Test)

# Roughtly Position labels
df$angle <- apply(df, 1, FUN=function(d) {
    atan2(as.numeric(d["f1_post"]) - as.numeric(d["f1_pre"]),
          as.numeric(d["f2_post"]) - as.numeric(d["f2_pre"])) / pi * 180 })
df$angle <- floor(df$angle / 90) * 90 + 90
df$angle[df$vowel == "hud" & df$Group == "LV"] <- 180
df$angle[df$vowel == "hard" & df$Group == "LV"] <- 45
df$angle[df$vowel == "hoard" & df$Group == "LV"] <- 135

df$angle[df$vowel == "heed" & df$Group == "HV"] <- 45
df$angle[df$vowel == "hud" & df$Group == "HV"] <- 45
df$angle[df$vowel == "hard" & df$Group == "HV"] <- 0
df$angle[df$vowel == "heard" & df$Group == "HV"] <- 180

s <- 0.1875
df$lab.x <- df$f2_pre - cos(df$angle / 180 * pi) * s
df$lab.y <- df$f1_pre - sin(df$angle / 180 * pi) * s
df$ipa <- ipa[as.character(df$vowel)]

colors <- {}
colors$pre <-  "#F8BBD0"
colors$post <- "#E91E63"
colors$arrow <- "#444444"
colors$ipa <- "#0288D1"

dpi = 1200
fontSize <- dpi * 80 / 600

width = 7
height = 4.5

p <- ggplot(data=) + theme(
    text=element_text(family="Cabin", size=fontSize),
    panel.background=element_rect(fill="#eeeeee"),
    panel.grid.major=element_line(
        color="#ffffff",
        linetype="13",
        lineend="round"),
    axis.ticks=element_blank(),
    legend.position="bottom",
    legend.key=element_rect(
      fill="transparent",
      colour="transparent"))
p <- p + scale_y_reverse(
    expand=c(0.02,0.02),
    minor_breaks=c(),
    breaks=seq(-2,2,1),
    labels=seq(-2,2,1))
p <- p + scale_x_reverse(
    expand=c(0.02,0.02),
    minor_breaks=c(),
    breaks=seq(-2,2,1),
    labels=seq(-2,2,1))
p <- p + coord_cartesian(xlim=c(2,-2), ylim=c(2, -2))

p <- p + geom_point(
    data=lob.mn.df,
    aes(x=f2, y=f1, color=Test),
    size=3)
p <- p + scale_fill_manual(values=c(colors$pre, colors$post))
p <- p + scale_color_manual(values=c(colors$pre, colors$post))
p <- p + geom_segment(
    data=df,
    aes(x=f2_pre, xend=f2_post, y=f1_pre, yend=f1_post),
    arrow=arrow(type="closed", length=unit(0.075, "in")),
    inherit.aes=FALSE,
    colour=colors$arrow)
p <- p + geom_text(
    data=df,
    aes(x=lab.x, y=lab.y, label=ipa),
    colour=colors$ipa,
    family="DejaVuSans",
    size=fontSize*0.4)
p <- p +  ylab("Normalised F1")
p <- p +  xlab("Normalised F2")
p <- p + facet_grid(.~Group)

options(repr.plot.width=width, repr.plot.height=height)
ggsave("vowels-plot.png", width=width, height=height, units="in", dpi=dpi)


