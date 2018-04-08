source(file.path("R", "settings.R"))

# Ensure packages are installed
list.of.packages <- c("ggplot2", "showtext", "plyr", "reshape2", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for (package in list.of.packages) {
  library(package, character.only=TRUE)
}

all_vowels = "
beat   i:   ɪ   bit
poot   u:  ʊ    put
bot    ɒ   ɑ:   bart
bot    ɒ   ʌ    but
bert   ɜ:  ɑ:   bart
but    ʌ   ɑ:   bart
bird   ɜ:  eə   bared
boat   əʊ   aʊ  bout
bait   eɪ  e    bet
bait   eɪ  aɪ   bite
bite   aɪ  ɪ    bit
bat    æ   ʌ    but
beard  ɪə  eə   bared
buoyed  ɔɪ  ɔ:   board
"
matches <- as.data.frame(str_match_all(all_vowels, "([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)")[[1]])
words <- {}
invisible(apply(matches, 1, FUN=function(row){
  words[row["V2"]] <<- row["V3"]
  words[row["V5"]] <<- row["V4"]
}))

# Set up custom font
font_add("Cabin", "../fonts/Cabin/Cabin-Regular.ttf")
font_add("Cabin-Italic", "../fonts/Cabin/Cabin-Italic.ttf")
font_add("DejaVuSans", "../fonts/dejavu-fonts-ttf-2.37/ttf/DejaVuSans.ttf")

showtext_auto()

axb.file <- file.path("data", "AXB-data.csv")
axb.df <- read.csv(file=axb.file, header=TRUE, sep=",")

axb.df$participant <- apply(axb.df, 1, function(x){
    paste(c(x["participant"], x["group"]), collapse="-")
})
axb.df$Pair <- apply(axb.df, 1, function(row) {
    stimuli <- row[c("stim1", "stim2", "stim3")]
    pair <- sort(unique(stimuli))
    #label <- sprintf('%s-%s', pair[1], pair[2])#
    label <- sprintf('/%s/ - /%s/', words[[pair[1]]], words[[pair[2]]])
    return (label)
})

axb.df$test <- factor(axb.df$test, levels=c("pre", "post"))
axb.df$group <- factor(axb.df$group)
axb.df$Pair <- factor(axb.df$Pair)

bx.df <- ddply(axb.df, c("group", "test", "participant", "Pair"), function(subset) {
    Trials <- nrow(subset)
    Correct = sum(subset$correct.response)
    Accuracy = Correct / Trials * 100
    data.frame(Trials, Correct, Accuracy)
})
names(bx.df)[names(bx.df) == "test"] <- "Test"
names(bx.df)[names(bx.df) == "group"] <- "Group"

bx.df$Group <- factor(bx.df$Group, levels=c("LV", "HV"))
width <- 8
height <- 3.5
dpi <- DPI

showtext_opts(dpi=dpi)
fontSize <- 12

POST <- "#F8BBD0"
PRE <- "#E91E63"
MD <- "#FCE4EC"
IPA <- "#176FC1"
# Do the plot
dodge <- position_dodge(0.875)
p <- ggplot(bx.df, aes(x=Pair, y=Accuracy, fill=Test, color=Test)) +
    geom_boxplot(position=dodge, aes(fill=Test, color=Test))

# Axis stuff
p <- p + scale_y_continuous(expand=c(0,5), limit=c(0, 100),
                                minor_breaks=c(),
                                breaks=seq(0,100,25),
                                labels=seq(0,100,25))
# Theme stuff
p <- p + theme(
    legend.position="right",
    legend.key=element_rect(
      fill="transparent",
      colour="transparent"),
    panel.background=element_rect(fill="#eeeeee"),
    panel.grid.major=element_line(color="#ffffff", linetype="13", lineend="round"),
    axis.ticks = element_blank(),
    text=element_text(family="Cabin", size=fontSize),
    axis.text.x = element_text(angle=45, hjust = 1, vjust=1, family="DejaVuSans", color=IPA),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0))) +
    ylab("Accuracy (%)")
# Manually set colors
p <- p + scale_fill_manual(values=c(POST, PRE))
p <- p + scale_color_manual(values=c(POST, PRE))

# Add the facet variable
p <- p + facet_grid(Group~.)

# Now add the medians. This must come after setting the facets.
dat <- ggplot_build(p)$data[[1]]

dat$Test <- rep(levels(bx.df$Test), times=nrow(dat) / 2)
dat$Group <- rep(levels(bx.df$Group), each=nrow(dat) / 2)
datGroup <- factor(dat$Group, levels=c("LV", "HV"))
p <- p + geom_segment(
  data=dat,
  aes(x=xmin, xend=xmax, y=middle, yend=middle), lineend="square", inherit.aes=FALSE, colour=MD)
#
ggsave(file.path(outDir, "axb-boxplot.png"), width=width, height=height, units="in", dpi=dpi)
