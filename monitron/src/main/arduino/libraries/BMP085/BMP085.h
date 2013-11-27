/*
  BMP085.h - wrapper for the BMP085 Extended Example Code by Jeff Lindblom
  Created by Joshua Shinavier, 2012
  Released into the public domain.
*/

#ifndef BMP085_h
#define BMP085_h

#include "Arduino.h"

class BMP085
{
  public:
    void setup();
    
    void sample();
    
    short getLastTemperature();
    long getLastPressure();
    
  private:
    //int ac1, ac2, ac3;
    //unsigned int ac4, ac5, ac6;
    //int b1, b2;
    //int mb, mc, md;
    //long b5; 
    //short temperature;
    //long pressure;
};

#endif

