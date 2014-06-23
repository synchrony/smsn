/*
  AnalogSampler.cpp
  Created by Joshua Shinavier, 2012-2014
  Released into the public domain.
*/

#include "AnalogSampler.h"

AnalogSampler::AnalogSampler(uint8_t pin)
{
    _pin = pin;
    reset();
}

void AnalogSampler::beginSample()
{
    if (0 == _startTime)
    {
        _startTime = millis();
    }
}

void AnalogSampler::endSample()
{
    _endTime = millis();
}

void AnalogSampler::measure()
{
    double v = analogRead(_pin) / 1024.0;
    addMeasurement(v);
}

void AnalogSampler::addMeasurement(double v)
{
    _n++;
    
    if (v < _minValue)
    {
        _minValue = v;
    }
    
    if (v > _maxValue)
    {
        _maxValue = v;
    }
    
    _sumOfValues += v;
    _sumOfSquares += (v * v);	
}

void AnalogSampler::reset()
{
    _minValue = 1.0;
    _maxValue = 0.0;
    _sumOfValues = 0;
    _sumOfSquares = 0;
    _n = 0;
    _startTime = 0;
    _endTime = 0;
}
    
unsigned long AnalogSampler::getStartTime() 
{
	return _startTime;
}

unsigned long AnalogSampler::getEndTime()
{
	return _endTime;
}

unsigned long AnalogSampler::getNumberOfMeasurements()
{
	return _n;
}

double AnalogSampler::getMinValue()
{
    return _minValue;
}

double AnalogSampler::getMaxValue()
{
    return _maxValue;
}

double AnalogSampler::getMean()
{
    return (_n > 0) ? _sumOfValues / _n : 0;
}

double AnalogSampler::getVariance()
{
    if (_n < 2) {
        return 0;
    } else {
	double m = getMean();
	return (_sumOfSquares - (_n * m * m)) / (_n - 1);
    }	    
}

