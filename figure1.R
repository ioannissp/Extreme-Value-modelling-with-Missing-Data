x<-seq(from=0.0001,to=5, length.out=1000000)
x3<-seq(from=0.0001, to=5, length.out=1000000)
y1<-pgpd(x, location = 0, scale = 1, shape=0.5)
y2<-pgpd(x, location = 0, scale = 1, shape=0)
y3<-pgpd(x3, location = 0, scale = 1, shape=-0.5)

pdf("gpd_dist_plot2.pdf", width = 8, height = 5)
par(mfrow = c(1, 3),  # 3 rows, 1 column
    mar = c(4, 4, 2, 1),
    oma = c(0, 0, 6, 0)# Adjust margins if needed
)

par(cex.main = 2, cex.lab = 1.7)

plot(x,y1, xlab="y", ylab="H(y)", main="Shape=0.5", type="l", pch=16, col="purple", cex=0.5, ylim=c(0,1))
plot(x,y2, xlab="y", ylab="H(y)", main="Shape=0", type="l", pch=16, col="purple", cex=0.5, ylim=c(0,1))
plot(x3,y3, xlab="y", ylab="H(y)", main="Shape=-0.5", type="l", pch=16, col="purple", cex=0.5, ylim=c(0,1))

#mtext("Distribution function of GPD with scale=1", line = 1, font = 2, cex = 1.2, outer = TRUE)
title(main = "Distribution function of GPD with scale=1", line = 4, outer = TRUE, cex.main = 2.5)

dev.off()