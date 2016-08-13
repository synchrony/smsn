/*
  RGBLED.h - simple tri-color LED control
  Created by Joshua Shinavier, 2012-2016
  Released into the public domain.
*/

#ifndef RGBLED_h
#define RGBLED_h

#include "Arduino.h"

const unsigned long
    RGB_WHITE = 0xffffff,
    RGB_RED = 0xff0000,
    RGB_ORANGE = 0xff8000,
    RGB_YELLOW = 0xffff00,
    RGB_GREEN = 0x00ff00,
    RGB_CYAN = 0x00ffff,
    RGB_BLUE = 0x0000ff,
    RGB_PURPLE = 0xff00ff,
    RGB_BLACK = 0x000000;

class RGBLED
{
  public:
    RGBLED(uint8_t redPin, uint8_t greenPin, uint8_t bluePin);
    void setup();
    // returns true if the operation succeeded, false otherwise (i.e. if the given color is invalid)
    boolean setColor(unsigned long color);

  private:
    uint8_t _redPin, _greenPin, _bluePin;
};

#endif // RGBLED_h
