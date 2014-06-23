/*
  AnalogSampler.h - sampling of Arduino analog input pins
  Created by Joshua Shinavier, 2012-2014
  Released into the public domain.
*/

#ifndef AnalogSampler_h
#define AnalogSampler_h

#include "Arduino.h"

class AnalogSampler
{
  public:
    AnalogSampler(uint8_t pin);
    
    void beginSample();
    void endSample();
    void measure();
    void addMeasurement(double v);
    void reset();
    
    unsigned long getStartTime();  // in milliseconds
    unsigned long getEndTime();    // in milliseconds
    unsigned long getNumberOfMeasurements();
    
    double getMinValue();
    double getMaxValue();
    double getMean();
    double getVariance();
    
  private:
    uint8_t _pin;
    unsigned long _startTime;
    unsigned long _endTime;
    unsigned long _n;
    double _minValue;
    double _maxValue;
    double _sumOfValues;
    double _sumOfSquares;
};

#endif // AnalogSampler_h

