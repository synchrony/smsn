/*
  analog_sampler.h - sampling of Arduino analog input pins
  Created by Joshua Shinavier, 2012
  Released into the public domain.
*/

#ifndef ANALOG_SAMPLER_h
#define ANALOG_SAMPLER_h

#include "Arduino.h"

class AnalogSampler
{
  public:
    AnalogSampler(uint8_t pin);
    
    void sample();
    void reset();
    
    double getMinValue();
    double getMaxValue();
    
  private:
    uint8_t _pin;
    unsigned int _minValue;
    unsigned int _maxValue;
};

#endif

