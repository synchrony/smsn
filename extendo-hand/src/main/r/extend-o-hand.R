


#install.packages("rgl")
library(rgl)

claps <- read.csv(file("/tmp/claps.csv"), header=FALSE)
handshakes <- read.csv(file("/tmp/handshakes.csv"), header=FALSE)
open.palms <- read.csv(file("/tmp/open-palms.csv"), header=FALSE)
points <- read.csv(file("/tmp/points.csv"), header=FALSE)
taps <- read.csv(file("/tmp/taps.csv"), header=FALSE)
waves <- read.csv(file("/tmp/waves.csv"), header=FALSE)

normed <- function(sample) {
    data.frame(t=sample$V1, x=sample$V3/sample$V2, y=sample$V4/sample$V2, z=sample$V5/sample$V2)
}

vector.mean <- function(a) {
    c(mean(a$x), mean(a$y), mean(a$z))
}

mag <- function(v) {
    sqrt(v[1]^2 + v[2]^2 + v[3]^2)
}

norm <- function(v) {
    v / mag(v)
}

dist.a.v <- function(a, v) {
    sqrt((a$x-v[1])^2 + (a$y-v[2])^2 + (a$z-v[3])^2)
}

tsteps <- function(t) {
    t1 <- t[1:length(t)-1]
    t2 <- t[2:length(t)]
    t2 - t1
}

plot.normed <- function(a, color) {
    plot3d(a$x, a$y, a$z, col=color, size=3)
}
points.normed <- function(a, color) {
    points3d(a$x, a$y, a$z, col=color, size=3)
}

plot.normed(normed(claps), "gray")
points.normed(normed(handshakes), "blue")
points.normed(normed(open.palms), "green")
points.normed(normed(points), "red")
#points.normed(normed(taps), "orange")
points.normed(normed(waves), "cyan")


########################################
# waves

a <- normed(waves)
a1 <- subset(a, a$x < 0)
a2 <- subset(a, a$x >= 0)

# c(-0.8356163, 0.4055072, 0.3705529)
a1.m <- norm(vector.mean(a1))
# c(0.9125032, -0.1936484, -0.3603307)
a2.m <- norm(vector.mean(a2))

points3d(a1.m[1], a1.m[2], a1.m[3], col="black", size=4)
points3d(a2.m[1], a2.m[2], a2.m[3], col="black", size=4)

a1.d <- dist.a.v(a1, a1.m)
a2.d <- dist.a.v(a2, a2.m)

# 0.5 seems to be a good cutoff value for both halves of the wave
hist(a1.d)
hist(a2.d)

tmp1 <- data.frame(t=a1$t, x=a1$x, y=a1$y, z=a1$z, d=a1.d)
tmp2 <- data.frame(t=a2$t, x=a2$x, y=a2$y, z=a2$z, d=a2.d)

tmp1.in <- subset(tmp1, d < 0.5)
tmp1.out <- subset(tmp1, d >= 0.5)
tmp2.in <- subset(tmp2, d < 0.5)
tmp2.out <- subset(tmp2, d >= 0.5)
tmp.in <- rbind(tmp1.in, tmp2.in)
tmp.out <- rbind(tmp1.out, tmp2.out)

# show waves recognized vs. not recognized
plot.normed(tmp.in, "red")
points.normed(tmp.out, "blue")

t <- waves$V1

# most waves are less than 0.3s apart, with various longer gaps between sets of waves
hist(tsteps(t) / 1000000, breaks=20)

# note: left and right may well be inverted
c.ONE <- data.frame(t=tmp1.in$t, c="LEFT")
c.one <- data.frame(t=tmp1.out$t, c="L")
c.TWO <- data.frame(t=tmp2.in$t, c="RIGHT")
c.two <- data.frame(t=tmp2.out$t, c="R")
tmp <- rbind(c.ONE, c.one, c.TWO, c.two)
t2 <- tmp[with(tmp, order(t)),]
data.frame(t2$c[2:nrow(t2)], tsteps(t2$t) / 1000000)



########################################
# open palm gesture

a <- normed(open.palms)

# c(0.3093581, -0.6281864, -0.7139183)
m <- norm(vector.mean(a))

d <- dist.a.v(a, m)

# these points are very localised; all within 0.3 of the mean,
# with a fairly even distribution around the mean
hist(d)

tmp <- data.frame(x=a$x, y=a$y, z=a$z, d=d)

t <- open.palms$V1

# I never produced gestures less than 0.6s apart
hist(tsteps(t) / 1000000)



