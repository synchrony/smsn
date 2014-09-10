################################################################################
# collect data

cd /tmp
script

DEVICE=/dev/tty.usbmodemfa141
#DEVICE=/dev/tty.RN42-C342-SPP
screen -S arduino $DEVICE 115200
# exit screen with C-a k, enter 'y' at the prompt

kill `ps | grep script | grep "[0-9] script$" | sed 's/^[^0-9]*//' | sed 's/ .*//'`

# edit to remove leading and trailing garbage
vim /tmp/typescript

cat /tmp/typescript | sed 's/^[^,]*[^0-9,a-z]//' | grep "," | sed 's/[^0-9A-Za-z]*$//' > /tmp/hand.csv

# if you typed in contexts while collecting data, grep them out
cat /tmp/hand.csv | grep s12


################################################################################
# grep out samples

cat hand.csv | grep s12 > samples/1-2.csv
cat hand.csv | grep s13 > samples/1-3.csv
cat hand.csv | grep s14 > samples/1-4.csv
cat hand.csv | grep s15 > samples/1-5.csv
cat hand.csv | grep s16 > samples/1-6.csv
cat hand.csv | grep s17 > samples/1-7.csv
cat hand.csv | grep s18 > samples/1-8.csv
cat hand.csv | grep s19 > samples/1-9.csv
cat hand.csv | grep s34 > samples/3-4.csv
cat hand.csv | grep s35 > samples/3-5.csv
cat hand.csv | grep s37 > samples/3-7.csv
cat hand.csv | grep s38 > samples/3-8.csv
cat hand.csv | grep s55 > samples/5-5.csv
cat hand.csv | grep s72 > samples/7-2.csv
cat hand.csv | grep s73 > samples/7-3.csv
cat hand.csv | grep s75 > samples/7-5.csv
cat hand.csv | grep s76 > samples/7-6.csv
cat hand.csv | grep s91 > samples/9-1.csv
cat hand.csv | grep s92 > samples/9-2.csv
cat hand.csv | grep s93 > samples/9-3.csv
cat hand.csv | grep s94 > samples/9-4.csv
cat hand.csv | grep s95 > samples/9-5.csv
cat hand.csv | grep s96 > samples/9-6.csv
cat hand.csv | grep s97 > samples/9-7.csv
cat hand.csv | grep s98 > samples/9-8.csv

# make sure all are present
wc -l samples/*


################################################################################
# study a new gesture

#install.packages("rgl")
library(rgl)

data <- read.csv(file("/tmp/hand.csv"), header=FALSE)

ax <- data$V3; ay <- data$V4; az <- data$V5
a <- sqrt(ax*ax + ay*ay + az*az)

df <- data.frame(x=ax, y=ay, z=az)


mag <- function(p) {
    sqrt(p$x^2 + p$y^2 + p$z^2)
}

norm <- function(p) {
    p / mag(p)
}

dist <- function(a, b) {
    sqrt((a$x-b$x)^2 + (a$y-b$y)^2 + (a$z-b$z)^2)
}

mean.p <- function(p) {
    data.frame(x=mean(p$x), y=mean(p$y), z=mean(p$z))
}

plot.normed <- function(p, color) {
    plot3d(p$x, p$y, p$z, col=color, size=3)
}
points.normed <- function(p, color) {
    points3d(p$x, p$y, p$z, col=color, size=3)
}

# iterate on df2 <--> center until you reach a fixed point or reasonable solution
df2 <- data.frame(x=1.36,y=0.27,z=-0.33)
#df2 <- data.frame(x=0.22, y=0.1200, z=1.10)
#df2 <- data.frame(x=0.24, y=0.21, z=0.93)
#df2 <- data.frame(x=0.4167566, y=0.1955685, z=0.8481532)
#df2 <- data.frame(x=0.5237636, y=0.1837714, z=0.7915894)

d <- dist(norm(df), norm(df2))
tmp <- data.frame(x=df$x, y=df$y, z=df$z, d=d)

tmp.near <- norm(subset(tmp, d < 0.5))
tmp.far <- norm(subset(tmp, d >= 0.5))

plot.normed(tmp.far, "black")
points.normed(tmp.near, "red")


# tmp.near looks like it contains the right points

# for the "flip" gesture: 0.9266487 0.1260582 -0.303386
# for the "toss" gesture: 0.5662381 0.1734404 0.7668633
center <- data.frame(x=mean(tmp.near$x), y=mean(tmp.near$y), z=mean(tmp.near$z))


########################################
# find the mean of a set of gestures

pos <- data.frame(x=gesture$V5, y=gesture$V6, z=gesture$V7)
np <- norm(pos)
plot3d(np$x, np$y, np$z, size=3)
center <- norm(data.frame(x=mean(np$x), y=mean(np$y), z=mean(np$z)))
d <- dist(np, center)
df <- data.frame(d=d, np)
nrow(subset(df, d < 0.7))/nrow(df)


################################################################################
# calibrate sensors

# calibrating the ADXL345 accelerometer
# the sensor was made to visit each of the six major axis orientations w.r.t. the vertical,
# with slow transitions and slow exploration of the immediate neighborhood of each axis

cat /tmp/typescript | sed 's/.*acceleration.//' | sed 's/[^0-9]+$//' | grep "," > /tmp/accel.csv

data <- read.csv(file("/tmp/accel.csv"), header=FALSE)

# -250 and 250 look to be pretty good approximations of -g and g for x, without any calibration
plot(data$V1, type="l")
abline(col="red", v=150)
abline(col="red", v=210)
x1 <- data[150:210,]
abline(col="red", v=290)
abline(col="red", v=390)
x2 <- data[290:390,]
plot(x1$V1, type="l")
abline(col="red", h=250)
plot(x2$V1, type="l")
abline(col="red", h=-250)

x: -255, 250
y: -245, 265
z: -285, 235


################################################################################
# study 9-axis data

cat hand.csv |grep s98 > /tmp/data.csv
cat /tmp/data.csv | grep magnet > /tmp/motion.csv
cat /tmp/data.csv | grep -v magnet > /tmp/gesture.csv

motion <- read.csv(file("/tmp/motion.csv"), header=FALSE)
gesture <- read.csv(file("/tmp/gesture.csv"), header=FALSE)
gestures <- unique(gesture$V1)

# for stick #1. See gyro.mean derived below
gyro.cal <- data.frame(x=-110.996, y=105.5632, z=-37.2404)


a <- data.frame(x=motion$V3, y=motion$V4, z=motion$V5)
accel <- data.frame(x=motion$V7, y=motion$V8, z=motion$V9)
gyro <- data.frame(x=(motion$V11 - gyro.cal$x[1]), y=(motion$V12 - gyro.cal$y[1]), z=(motion$V13 - gyro.cal$z[1]))
magnet <- data.frame(x=motion$V15, y=motion$V16, z=motion$V17)



plot(gyro$x, type="l")
lines(gyro$y, col="blue")
lines(gyro$z, col="red")


g <- sqrt(gyro$x*gyro$x + gyro$y*gyro$y + gyro$z*gyro$z)
plot(g, type="l")
ac <- sqrt(accel$x*accel$x + accel$y*accel$y + accel$z*accel$z)
lines(ac*4, col="red")


m <- sqrt(magnet$x*magnet$x + magnet$y*magnet$y + magnet$z*magnet$z)

m2 <- 50*(m - min(m))
lines(m2, col="blue")

# period of m and magnet$z is a pair of taps; period of magnet$x and magnet$y is one tap
# m varies only by a tiny amount
plot(m, type="l")
plot(m$x, type="l")
plot(m$y, type="l")
plot(m$z, type="l")



lines.normed <- function(p, color) {
    lines3d(p$x, p$y, p$z, col=color, size=3)
}

lines.normed(accel, "black"); points3d(accel$x[1], accel$y[1], accel$z[1], col="blue")
lines.normed(gyro, "black"); points3d(gyro$x[1], gyro$y[1], gyro$z[1], col="blue")

points3d(0,0,0, col="red")


########################################
#

# when computed over a large sample,
# this is very close to the natural ready position of the hand
a.mean <- data.frame(x=mean(a$x), y=mean(a$y), z=mean(a$z))
accel.mean <- data.frame(x=mean(accel$x), y=mean(accel$y), z=mean(accel$z))

# to calibrate the gyroscope, take a long sample while the sensor is motionless
# initial value of gyro.mean for stick #1 was (-110.996,105.5632,-37.2404)
# the standard deviation appears to be around 2 units in each dimension, due to jitter
gyro.mean <- data.frame(x=mean(gyro$x), y=mean(gyro$y), z=mean(gyro$z))
gyro.sd <- data.frame(x=sd(gyro$x), y=sd(gyro$y), z=sd(gyro$z))

magnet.mean <- data.frame(x=mean(magnet$x), y=mean(magnet$y), z=mean(magnet$z))


########################################
# compare accelerometers

# the two accelerometers are very closely aligned,
# only slightly offset in one dimension or another
plot(a$x, type="l")
lines(accel$x/230 - 0.05, col="red")
plot(a$y, type="l")
lines(accel$y/230, col="red")
plot(a$z, type="l")
lines(accel$z/230, col="red")


########################################
# draw time series in 3D
#
# accelerometer is a comet-like ellipsoid with a tail in the direction of the taps
# gyro is the superposition of a sort of ellipsoid (major axis in z) and oblate spheroid
#     (axis of symmetry in z) at right angles to each other.
#     The ellipsoid corresponds to rotation within a plane perpendicular to the typing surface,
#     while the spheroid (which is biased to -x) seems to represent the tendency of the left hand
#     to suddenly turn counterclockwise, more slowly returning to a palm-down orientation.
# magnetometer is a shallow dish in which the sensor rolls around.
#     Should be relatively easy to calculate the plane of the typing surface from this.

#series <- accel; origin <- accel.mean
#series <- gyro; origin <- data.frame(x=0,y=0,z=0)
series <- magnet; origin <- magnet.mean

m <- series
plot3d(m$x, m$y, m$z, type="p", size=1)
points3d(origin$x,origin$y,origin$z,col="green")


########################################
# animate time series in 3D

#install.packages("rgl")
library(rgl)

#totalTime <- 60
totalTime <- nrow(motion)*((motion$V2[1001]-motion$V2[1])/(1000*1000*1000))

#series <- accel; origin <- accel.mean
#series <- gyro; origin <- gyro.cal
#series <- gyro; origin <- data.frame(x=0,y=0,z=0)
series <- magnet; origin <- magnet.mean
color <- "black"
xmin <- min(series$x); xmax <- max(series$x)
ymin <- min(series$y); ymax <- max(series$y)
zmin <- min(series$z); zmax <- max(series$z)

wave <- function(time) {
   n <- nrow(motion)
   steps <- n * (time / totalTime)
   sub <- series[1:steps,]

   if (0 == time) {
       plot3d(series$x[1], series$y[1], series$z[1], col="blue", size=6, xlim=c(xmin, xmax), ylim=c(ymin, ymax), zlim=c(zmin, zmax))
       points3d(series$x[nrow(series)], series$y[nrow(series)], series$z[nrow(series)], col="red")
       points3d(origin$x,origin$y,origin$z,col="green")
       #points3d(accel.mean$x, accel.mean$y, accel.mean$z, col="purple")
   } else {
       lines3d(sub$x, sub$y, sub$z, col=color, size=6)
   }

   #c(list(skipRedraw = FALSE), spin(time))
   c(list(skipRedraw = FALSE))
}
#spin <- spin3d(rpm=6,axis=c(0,0,1))

open3d()

if (!rgl.useNULL()) {
   play3d(wave, totalTime)
}


########################################
# cumulative average sensor values
#

#series <- accel; origin <- accel.mean
series <- gyro; origin <- data.frame(x=0,y=0,z=0)
#series <- magnet; origin <- magnet.mean

# accelerometer wanders erratically, slowly settling into a disc-shaped neighborhood
# gyro "wobbles" at first, then, quickly converges just beyond the origin
# magnetometer immediately settles into a plane, and slowly approaches a vector within that plane
m <- cumsum(series) / 1:nrow(series)

plot3d(m$x, m$y, m$z, type="l", size=3)
points3d(m$x[1], m$y[1], m$z[1], col="red")
for (i in 1:10) {
    index <- nrow(motion)*i/10
    points3d(m$x[index], m$y[index], m$z[index], col="red")
}
points3d(origin$x,origin$y,origin$z,col="green")


################################################################################
# classify key pairs

g <- gesture
g1 <- rbind(g[1,], g, g[nrow(g),])
l <- g1$V8[1:(nrow(g1)-1)]
n <- g1$V8[2:nrow(g1)]
g2 <- data.frame(g1[2:(nrow(g1)-1),], l=l[1:(length(l)-1)], n=n[2:length(n)])
taps <- subset(g2, (V8=="tap-1" & n=="tap-2") | (V8=="tap-2" & l=="tap-1"))


norm.timestamp <- function(t) {
    if (t < 0) {
       2^31 - t
    } else t;
}

magnet.new <- data.frame(t=sapply(motion$V2, norm.timestamp), magnet)

t1 <- sapply(taps$V2, norm.timestamp)
t2 <- sapply(taps$V3, norm.timestamp)
taps.new <- data.frame(context=taps$V1, t1=t1, t2=t2, gesture=taps$V8)

motion.time <- motion$V2

latest.motion <- function(time) {
    max(subset(magnet.new, t < time)$t)
}

lm <- sapply(taps.new$t1, latest.motion)
taps.newest <- merge(data.frame(taps.new, t=lm), magnet.new, by="t")

tn <- taps.newest
d <- dist(tn[1:(nrow(tn)-1),], tn[2:nrow(tn),])
tmp <- subset(data.frame(tn[1:(nrow(tn)-1),], dist=d), gesture=="tap-1")

mean(tmp$dist)
sd(tmp$dist)

mean(subset(tmp, context=="s12")$dist)
sd(subset(tmp, context=="s12")$dist)


################################################################################
# find direction of gravity

l = 400  # around 4s seems to be a good balance
cx <- cumsum(a$x)
ax.g <- (cx[(l+1):length(cx)] - cx[1:(length(cx)-l)]) / l
plot(ax.g, type="l")



cz <- cumsum(a$z)
az.g <- (cz[(l+1):length(cz)] - cz[1:(length(cz)-l)]) / l
plot(az.g, type="l")


