
source(file.path("R", "settings.R"))

cov2d <- function(x, y) {
    cov.xy <- cov(x, y)
    return (matrix(c(var(x), cov.xy, cov.xy, var(y)), ncol=2, nrow=2))
}

bhatt.dist <- function(x, y) {
    mn.x <- rowMeans(x)
    mn.y <- rowMeans(y)
    mat.x <- cov2d(x[1,], x[2,])
    mat.y <- cov2d(y[1,], y[2,])
    mat <- (mat.x + mat.y) / 2
    bht <- 1 / 8 * t(mn.x - mn.y) %*% solve(mat) %*% (mn.x - mn.y) + 0.5 * log(det(mat) / sqrt(det(mat.x) * det(mat.y)))
    return (bht)
}

dataDir <- "data"

hm.df <- read.csv(file.path(dataDir, "hawkins_midgely_2005.csv"))
hm.lob.df <- lobanov(hm.df, group=c("speaker"))

vwl.df <- rbind(
    read.csv(file.path(dataDir, "pre-LV.csv")),
    read.csv(file.path(dataDir, "post-LV.csv")),
    read.csv(file.path(dataDir, "pre-HV.csv")),
    read.csv(file.path(dataDir, "post-HV.csv")))
vwl.df$speaker <- sapply(vwl.df$participant, FUN=function(x) sub("(C[0-9]+).*?([LH]V)", "\\1\\2", x))


lob.df <- lobanov(vwl.df, group=c('group', 'test', 'speaker'))
lob.df <- subset(lob.df, vowel %in% monophthongs)

pre.rows <- nrow(lob.df)
lob.df <- ddply(lob.df, c("group", "test", "vowel"), function(d){
  f1.z <- scale(d$f1, center=TRUE, scale=TRUE)
  f2.z <- scale(d$f2, center=TRUE, scale=TRUE)
  f1.mad <- median(abs(d$f1 - median(d$f1)))
  f2.mad <- median(abs(d$f2 - median(d$f2)))
  f1.mad.dev <- abs(d$f1 - median(d$f1)) / f1.mad
  f2.mad.dev <- abs(d$f2 - median(d$f2)) / f1.mad
  cbind(d,
    data.frame(
      f1.z,
      f2.z,
      f1.z.out=abs(f1.z) > 2.5,
      f2.z.out=abs(f2.z) > 2.5,
      f1.mad.out=f1.mad.dev > 3,
      f2.mad.out=f2.mad.dev > 3
    )
  )
})
lob.df <- subset(lob.df)#, !f1.mad.out & !f2.mad.out)
post.rows <- nrow(lob.df)
cat(sprintf("Removed %d outliers\n", pre.rows - post.rows))

ks.df <- ddply(lob.df, c("group", "test"), function(d) {
    ddply(d, "vowel", function(e) {
        vwl = as.character(unique(e$vowel))
        tdf <- subset(hm.lob.df, vowel==vwl)
        f1 <- ks.test(e$f1,tdf$f1)
        f2 <- ks.test(e$f2,tdf$f2)
        rbind(
            data.frame(
                formant='f1',
                distance=f1$statistic,
                p.value=f1$p.value),
            data.frame(
                formant='f2',
                distance=f2$statistic,
                p.value=f2$p.value))
    })
})

dpi <- DPI


fontSize <- 40
width <- 4
height <- 2.5
options(repr.plot.width=width, repr.plot.height=height)

p <- ggplot() + theme(
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
    ylab("K-S distance")

p <- p + geom_boxplot(data=ks.df, aes(x=test, y=distance, color=test, fill=test))

p <- p + scale_y_continuous(
  minor_breaks=c(),
  breaks=seq(0,1,.2),
  labels=seq(0,1,.2))

# Add limits here to prevent filtering of data
p <- p + coord_cartesian(ylim=c(0,.85))

p <- p + scale_color_manual(
  values=c(pre=colors$pre, post=colors$post),
  name="Test")
p <- p + scale_fill_manual(
  values=c(pre=colors$pre, post=colors$post),
  name="Test")
p <- p + facet_grid(.~formant+group)

dat <- ggplot_build(p)$data[[1]]

# dat$test <- rep(levels(ks.df$test), times=nrow(dat) / 2)
dat$formant <- rep(levels(ks.df$formant), each=nrow(dat) / 2)
dat$test <- rep(levels(ks.df$test), times=nrow(dat) / 2)
dat$group <- rep(rep(levels(ks.df$group), each=nrow(dat) / 4), times=nrow(dat) / 4)

dat$group <- factor(dat$group, levels=c("LV", "HV"))
p <- p + geom_segment(
  data=dat,
  aes(x=xmin, xend=xmax, y=middle, yend=middle), lineend="square", inherit.aes=FALSE, colour="white")

ggsave(file.path(outDir, "ks-plot.png"), width=width, height=height, units="in", dpi=dpi)

