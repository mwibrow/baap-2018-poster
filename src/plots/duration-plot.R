
source(file.path("R", "settings.R"))


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
vwl.df <- subset(vwl.df)#, !out)
post.rows <- nrow(vwl.df)
cat(sprintf("Removed %d outliers\n", pre.rows - post.rows))


dpi <- DPI
width <- 2
height <- 2.75
options(repr.plot.width=width, repr.plot.height=height)

showtext_opts(dpi=dpi)
fontSize <- 12

p <- ggplot() + theme(
  panel.background=element_rect(fill=colors$panel.background),
  panel.grid.major=element_line(
    color=colors$panel.grid,
    linetype="13",
    lineend="round"),
   text=element_text(family="Cabin", size=fontSize),
  axis.ticks=element_blank(),
   axis.text.x = element_text(size=fontSize*0.75),
    axis.text.y = element_text(size=fontSize*0.75),
  legend.position="none",
  legend.key=element_rect(
    fill="transparent",
    colour="transparent"))

p <- p + geom_boxplot(data=vwl.df, aes(x=test, y=duration, color=test, fill=test))
p <- p + facet_grid(.~group)
p <- p + scale_color_manual(
  values=c(pre=colors$pre, post=colors$post),
  name="Test")
p <- p + scale_fill_manual(
  values=c(pre=colors$pre, post=colors$post),
  name="Test")
p <- p + xlab("Test") +
    ylab("Duration (ms)")

p <- p + scale_y_continuous(
  expand=c(0.02,0.02),
  minor_breaks=c(),
  breaks=seq(0,600,150),
  labels=seq(0,600,150))

# Add limits here to prevent filtering of data
p <- p + coord_cartesian(ylim=c(0,600))

dat <- ggplot_build(p)$data[[1]]
dat$test <- rep(levels(vwl.df$test), times=nrow(dat) / 2)
dat$group <- rep(levels(vwl.df$group), each=nrow(dat) / 2)

dat$group <- factor(dat$group, levels=c("LV", "HV"))
p <- p + geom_segment(
  data=dat,
  aes(x=xmin, xend=xmax, y=middle, yend=middle), lineend="square", inherit.aes=FALSE, colour="white")

ggsave(file.path(outDir, "duration-plot.png"),width=width, height=height, units="in", dpi=dpi)


