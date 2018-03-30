library(showtext)
font_add("Cabin", "../fonts/Cabin/Cabin-Regular.ttf")
showtext_auto()

library(ggplot2)
library(plyr)

oddity <- read.csv(file="AXB-data.csv", header=TRUE, sep=",")
oddity$Pair <- apply(oddity, 1, function(row) {
    stimuli <- row[c('stim1', 'stim2', 'stim3')]
    pair <- sort(unique(stimuli))
    label <- paste(pair, collapse='-')
    return (label)
})
oddity$test <- factor(oddity$test, levels=c('pre', 'post'))
oddity$group <- factor(oddity$group)
oddity$Pair <- factor(oddity$Pair)

bx.df <- ddply(oddity, c('group', 'test', 'participant', 'Pair'), function(subset) {
    Trials <- nrow(subset)
    Correct = sum(subset$correct.response)
    Accuracy = Correct / Trials * 100
    data.frame(Trials, Correct, Accuracy)
})
names(bx.df)[names(bx.df) == 'test'] <- 'Test'
names(bx.df)[names(bx.df) == 'group'] <- 'Group'

width = 7
height = 5
HV <- "#F8BBD0"
LV <- "#E91E63"
p <- ggplot(bx.df, aes(x=Pair, y=Accuracy, fill=Test, color=Test)) +
    geom_boxplot(position=position_dodge(0.875)) + theme(legend.position="bottom")
p <- p + theme(
    text=element_text(family="Cabin", size=40),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
    axis.title.x = element_text(margin = margin(t = -15, r = 0, b = 0, l = 0)))
p <- p + scale_fill_manual(values=c(HV, LV))
p <- p + scale_color_manual(values=c(HV, LV))
p <- p + facet_grid(Group~.)
ggsave("axb-boxplot.png", width=width, height=height)
