/*
  MMA7361.cpp
  Created by Joshua Shinavier, 2013-2016
  Released into the public domain.
*/

#include "MMA7361.h"

MMA7361::MMA7361(uint8_t xPin, uint8_t yPin, uint8_t zPin) {
    _xPin = xPin;
    _yPin = yPin;
    _zPin = zPin;
}

void MMA7361::calibrateX(int min, int max) {
    xrange = max - min;
    xmid = (min + max) / 2.0;
}

void MMA7361::calibrateY(int min, int max) {
    yrange = max - min;
    ymid = (min + max) / 2.0;
}

void MMA7361::calibrateZ(int min, int max) {
    zrange = max - min;
    zmid = (min + max) / 2.0;
}

double MMA7361::accelX() {
    return 2 * (analogRead(_xPin) - xmid) / xrange;
}

double MMA7361::accelY() {
    return 2 * (analogRead(_yPin) - ymid) / yrange;
}

double MMA7361::accelZ() {
    return 2 * (analogRead(_zPin) - zmid) / zrange;
}

int MMA7361::rawX() {
    return analogRead(_xPin);
}

int MMA7361::rawY() {
    return analogRead(_yPin);
}

int MMA7361::rawZ() {
    return analogRead(_zPin);
}

