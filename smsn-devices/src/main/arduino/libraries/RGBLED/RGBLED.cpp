/*
  RGBLED.cpp
  Created by Joshua Shinavier, 2012-2016
  Released into the public domain.
*/

#include "RGBLED.h"

// prevents red (which otherwise requires a higher resistance) from dominating
const unsigned int RED_FACTOR = 100;

RGBLED::RGBLED(uint8_t redPin, uint8_t greenPin, uint8_t bluePin)
{
    _redPin = redPin;
    _greenPin = greenPin;
    _bluePin = bluePin;
}

void RGBLED::setup()
{
    pinMode(_redPin, OUTPUT);
    pinMode(_greenPin, OUTPUT);
    pinMode(_bluePin, OUTPUT);
    
    setColor(RGB_BLACK);
}

boolean RGBLED::setColor(unsigned long color) {
    if (color > 0xffffff) {
        return false;
    }

    unsigned long red = (color & RGB_RED) >> 16;
    unsigned long green = (color & RGB_GREEN) >> 8;
    unsigned long blue = (color & RGB_BLUE);

    red = (red * RED_FACTOR) / 255;

    analogWrite(_redPin, 255 - (unsigned int) red);
    analogWrite(_greenPin, 255 - (unsigned int) green);
    analogWrite(_bluePin, 255 - (unsigned int) blue);

    return true;
}
