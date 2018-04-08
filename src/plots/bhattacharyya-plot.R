
source(file.path("R", "settings.R"))

cov2d <- function(x, y) {
    cov.xy <- cov(x, y)
    return (matrix(
      rbind(
        cbind(c(var(x), cov.xy)),
        cbind(c(cov.xy, var(y)))), ncol=2, nrow=2))
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

bhat.df <- ddply(lob.df, c("group", "test"), function(d) {
    ddply(d, "vowel", function(e) {
        vwl = as.character(unique(e$vowel))
        tdf <- subset(hm.lob.df, vowel==vwl)
        data.frame(
            bhatt=bhatt.dist(matrix(rbind(e$f1, e$f2), nrow=2),
            matrix(rbind(tdf$f1, tdf$f2), nrow=2)))
    })
})

dpi <- DPI
width <- 3
height <- 3.5
options(repr.plot.width=width, repr.plot.height=height)

fontSize <- 40

bhat.df$test <- relevel(factor(bhat.df$test), ref="pre")
bhat.df$group <- factor(bhat.df$group, levels=c("LV", "HV"))


p <- ggplot() + theme(
    legend.position="none",
    legend.key=element_rect(
      fill="transparent",
      colour="transparent"),
    panel.background=element_rect(fill="#eeeeee"),
    panel.grid.major=element_line(color="#ffffff", linetype="13", lineend="round"),
    axis.ticks = element_blank(),
    text=element_text(family="Cabin", size=fontSize),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0))) +
    xlab("Test") +
    ylab("Bhattacharyya distance")

p <- p + scale_y_continuous(
  minor_breaks=c(),
  breaks=seq(0,1.5,.5),
  labels=seq(0,1.5,.5))

# Add limits here to prevent filtering of data
p <- p + coord_cartesian(ylim=c(0,1))

p <- p + geom_boxplot(data=bhat.df, aes(x=test, y=bhatt, color=test, fill=test))
p <- p + scale_color_manual(
  values=c(pre=colors$pre, post=colors$post),
  name="Test")
p <- p + scale_fill_manual(
  values=c(pre=colors$pre, post=colors$post),
  name="Test")
p <- p + facet_grid(.~group)

dat <- ggplot_build(p)$data[[1]]
dat$test <- rep(levels(bhat.df$test), times=nrow(dat) / 2)
dat$group <- rep(levels(bhat.df$group), each=nrow(dat) / 2)

dat$group <- factor(dat$group, levels=c("LV", "HV"))
p <- p + geom_segment(
  data=dat,
  aes(x=xmin, xend=xmax, y=middle, yend=middle), lineend="square", inherit.aes=FALSE, color="white")


ggsave(file.path(outDir, "bhattacharyya-plot.png"), width=width, height=height, units="in", dpi=dpi)

