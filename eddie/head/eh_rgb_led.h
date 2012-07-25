#include <stdlib.h>

const int redPin = 11;
const int greenPin = 9;
const int bluePin = 10;

const unsigned long WHITE = 0xffffff;
const unsigned long RED = 0xff0000;
const unsigned long YELLOW = 0xffff00;
const unsigned long GREEN = 0x00ff00;
const unsigned long CYAN = 0x00ffff;
const unsigned long BLUE = 0x0000ff;
const unsigned long PURPLE = 0xff00ff;
const unsigned long BLACK = 0x000000;

// prevents red (which otherwise requires a higher resistance) from dominating
const unsigned int RED_FACTOR = 60;

////////////////////////////////////////

void rgb_led_setup()
{
  pinMode(redPin, OUTPUT);
  pinMode(greenPin, OUTPUT);
  pinMode(bluePin, OUTPUT);
}

void writeColor(unsigned long color)
{
  unsigned long red = (color & RED) >> 16;
  unsigned long green = (color & GREEN) >> 8;
  unsigned long blue = (color & BLUE);
  
  red = (red * RED_FACTOR) / 255;
  
  analogWrite(redPin, 255 - (unsigned int) red);
  analogWrite(greenPin, 255 - (unsigned int) green);
  analogWrite(bluePin, 255 - (unsigned int) blue);
}
