
source(file.path("R", "settings.R"))
library(nlme)

dataDir <- "data"

vwl.df <- rbind(
    read.csv(file.path(dataDir, "pre-LV.csv")),
    read.csv(file.path(dataDir, "post-LV.csv")),
    read.csv(file.path(dataDir, "pre-HV.csv")),
    read.csv(file.path(dataDir, "post-HV.csv")))
vwl.df$speaker <- sapply(vwl.df$participant, FUN=function(x) sub("(C[0-9]+).*?([LH]V)", "\\1\\2", x))


pre.rows <- nrow(vwl.df)
vwl.df <- ddply(vwl.df, c("group", "test", "vowel"), function(d){

  duration.mad <- median(abs(d$duration - median(d$duration)))
  duration.dev <- abs(d$duration - median(d$duration)) / duration.mad
  cbind(d,
    data.frame(
      out=duration.dev> 3
    )
  )
})
vwl.df <- subset(vwl.df, !out)
post.rows <- nrow(vwl.df)
cat(sprintf("Removed %d outliers\n", pre.rows - post.rows))


model <- lme(duration~test*group, random=list(~1+test|vowel, ~1+test|participant), data=vwl.df)

int <- intervals(model, which="fixed")[[1]]

cf.df <- data.frame(
    group=c("LV", "LV", "HV", "HV"),
    test=c("pre", "post", "pre", "post"),
    lower=c(int[1,1], int[1,1] + int[2,1], int[1,1] + int[3,1], int[1,1] + int[2,1] + int[3,1] + int[4,1]),
    estimate=c(int[1,2], int[1,2] + int[2,2], int[1,2] + int[3,2], int[1,2] + int[2,2] + int[3,2] + int[4,2]),
    upper=c(int[1,3], int[1,3] + int[2,3], int[1,3] + int[3,3], int[1,3] + int[2,3] + int[3,3] + int[4,3])
)
cf.df$test <- factor(cf.df$test, levels=c("pre", "post"))
cf.df$group <- factor(cf.df$group, levels=c("LV", "HV"))

dpi <- DPI
width <- 2
height <- 2.75


showtext_opts(dpi=dpi)
fontSize <- 12

options(repr.plot.width=width, repr.plot.height=height)
p <- ggplot(data=cf.df)
p <- p + theme(
    legend.position="none",
    legend.key=element_rect(
      fill="transparent",
      colour="transparent"),
    panel.background=element_rect(fill="#eeeeee"),
    panel.grid.major=element_line(color="#ffffff", linetype="13", lineend="round"),
    axis.ticks = element_blank(),
    text=element_text(family="Cabin", size=fontSize),
    axis.text.x = element_text(size=fontSize*0.75),
    axis.text.y = element_text(size=fontSize*0.75),

    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0))) +
    xlab("Test") +
    ylab("Duration (ms)")

p <- p + scale_y_continuous(
  minor_breaks=c(),
  breaks=seq(0, 350, 100),
  labels=seq(0, 350, 100))

# Add limits here to prevent filtering of data
p <- p + coord_cartesian(ylim=c(0, 350))

p <- p + geom_segment(
    aes(x=test, xend=test, y=lower, yend=upper), color="#444444")

p <- p + geom_point(
    aes(x=test, y=estimate, color=test), size=3)
p <- p + scale_color_manual(
  values=c(pre=colors$pre, post=colors$post),
  name="Test")
p <- p + scale_fill_manual(
  values=c(pre=colors$pre, post=colors$post),
  name="Test")

p <- p + facet_grid(.~group)

ggsave(file.path(outDir, "duration-model.png"),width=width, height=height, units="in", dpi=dpi)


