################################################################################
# collect data

cd /tmp
script

DEVICE=/dev/tty.usbmodemfd131
#DEVICE=/dev/tty.RN42-C342-SPP
screen -S arduino $DEVICE 115200
# exit screen with C-a k, enter 'y' at the prompt

kill `ps | grep script | grep "[0-9] script$" | sed 's/^[^0-9]*//' | sed 's/ .*//'`

# edit to remove leading and trailing garbage
vim /tmp/typescript

cat /tmp/typescript | sed 's/^[^,]*[^0-9,tsx]//' | grep "," | sed 's/[^0-9A-Za-z]*$//' > /tmp/hand.csv

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
# only slightly offset (especially in z)
plot(accel$x, type="l")
lines(a$x*300, col="red")
plot(accel$y, type="l")
lines(a$y*300, col="red")
plot(accel$z, type="l")
lines(a$z*300, col="red")


########################################
# animate time series in 3D (note: choose a small sub-series)

#install.packages("rgl")
library(rgl)

totalTime <- 50
#series <- accel; origin <- accel.mean
series <- gyro; origin <- gyro.cal
#series <- magnet; origin <- magnet.mean
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
