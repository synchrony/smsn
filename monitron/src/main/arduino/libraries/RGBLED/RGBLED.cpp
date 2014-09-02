/*
  RGBLED.cpp
  Created by Joshua Shinavier, 2012-2014
  Released into the public domain.
*/

#include <stdlib.h>

#include "RGBLED.h"

// prevents red (which otherwise requires a higher resistance) from dominating
const unsigned int RED_FACTOR = 100;

unsigned long colorStack[10];
int colorStackIndex = -1;

RGBLED::RGBLED(uint8_t redPin, uint8_t greenPin, uint8_t bluePin, void (*sendError)(const char*))
{
    _redPin = redPin;
    _greenPin = greenPin;
    _bluePin = bluePin;

    _sendError = sendError;
}

void RGBLED::setup()
{
    pinMode(_redPin, OUTPUT);
    pinMode(_greenPin, OUTPUT);
    pinMode(_bluePin, OUTPUT);
    
    pushColor(RGB_BLACK);
    //testSequence();
    //pushColor(RGB_YELLOW);
    //delay(3000);
}

void RGBLED::writeColor()
{
  unsigned long color = colorStack[colorStackIndex];
  
  unsigned long red = (color & RGB_RED) >> 16;
  unsigned long green = (color & RGB_GREEN) >> 8;
  unsigned long blue = (color & RGB_BLUE);
  
  red = (red * RED_FACTOR) / 255;
  
  analogWrite(_redPin, 255 - (unsigned int) red);
  analogWrite(_greenPin, 255 - (unsigned int) green);
  analogWrite(_bluePin, 255 - (unsigned int) blue);
}

void RGBLED::pushColor(unsigned long color)
{
    checkColor(color);

    colorStackIndex++;
    colorStack[colorStackIndex] = color;  
    writeColor();  
}

void RGBLED::popColor()
{
    if (colorStackIndex > 0) {
        colorStackIndex--;
    }
    writeColor();
}

void RGBLED::replaceColor(unsigned long color)
{
    popColor();
    pushColor(color);
}

void RGBLED::testColorSequence()
{
  for (int i=0; i < 10; i++) {
    replaceColor(RGB_WHITE);
    delay(50);
    replaceColor(RGB_BLACK);
    delay(50);
  }

  replaceColor(RGB_WHITE);
  delay(1000);
  replaceColor(RGB_RED);
  delay(1000);
  replaceColor(RGB_YELLOW);
  delay(1000);
  replaceColor(RGB_GREEN);
  delay(1000);
  replaceColor(RGB_CYAN);
  delay(1000);
  replaceColor(RGB_BLUE);
  delay(1000);
  replaceColor(RGB_PURPLE);
  delay(1000);
  replaceColor(RGB_BLACK);
  delay(1000);
}

void RGBLED::checkColor(unsigned long color) {
     if (color > 0xffffff) {
         _sendError("invalid color");
     }
}

