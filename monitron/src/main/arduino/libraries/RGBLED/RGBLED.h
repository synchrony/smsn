/*
  RGBLED.h - tri-color LED control
  Created by Joshua Shinavier, 2012-2014
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
    void writeColor();
    void pushColor(unsigned long color);
    void popColor();
    void replaceColor(unsigned long color);
    void testColorSequence();

  private:
    uint8_t _redPin, _greenPin, _bluePin;
};

#endif // RGBLED_h
