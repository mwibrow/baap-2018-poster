# Ensure packages are installed
list.of.packages <- c("ggplot2", "showtext", "plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(showtext)
library(ggplot2)
library(plyr)

# Set up custom font
font_add("Cabin", "../fonts/Cabin/Cabin-Regular.ttf")
font_add("Cabin-Italic", "../fonts/Cabin/Cabin-Italic.ttf")
showtext_auto()

axb.file <- file.path("data", "AXB-data.csv")
axb.df <- read.csv(file=axb.file, header=TRUE, sep=",")

axb.df$participant <- apply(axb.df, 1, function(x){
    paste(c(x["participant"], x["group"]), collapse="-")
})
axb.df$Pair <- apply(axb.df, 1, function(row) {
    stimuli <- row[c("stim1", "stim2", "stim3")]
    pair <- sort(unique(stimuli))
    label <- paste(pair, collapse="-")
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

width <- 7
height <- 5
dpi <- 1200

if (Sys.info()["sysname"] == "Darwin") {
  fontSize <- 12
} else {
  fontSize <- dpi * 80 / 600
}

POST <- "#F8BBD0"
PRE <- "#E91E63"
MD <- "#FCE4EC"

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
    legend.position="bottom",
    legend.key=element_rect(
      fill="transparent",
      colour="transparent"),
    panel.background=element_rect(fill="#eeeeee"),
    panel.grid.major=element_line(color="#ffffff", linetype="13", lineend="round"),
    axis.ticks = element_blank(),
    text=element_text(family="Cabin", size=fontSize),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5, family="Cabin-Italic"),
    axis.title.x = element_text(margin = margin(t = -5, r = 0, b = -10, l = 0))) +
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
p <- p + geom_segment(
  data=dat,
  aes(x=xmin, xend=xmax, y=middle, yend=middle), lineend="square", inherit.aes=FALSE, colour=MD)
#
ggsave("axb-boxplot.png", width=width, height=height, units="in", dpi=dpi)
