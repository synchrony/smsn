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

    // See Knuth TAOCP vol 2, 3rd edition, page 232
    // See also http://www.johndcook.com/blog/standard_deviation
    if (1 == _n)
    {
        _minValue = v;
        _maxValue = v;

        _oldM = _newM = v;
        _oldS = 0.0;
    }
    else
    {
        if (v < _minValue)
        {
            _minValue = v;
        }

        if (v > _maxValue)
        {
            _maxValue = v;
        }

        _newM = _oldM + (v - _oldM)/_n;
        _newS = _oldS + (v - _oldM)*(v - _newM);
        _oldM = _newM;
        _oldS = _newS;
    }
}

void AnalogSampler::reset()
{
    _n = 0;

    _minValue = 0.0;
    _maxValue = 0.0;
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
    return (_n > 0) ? _newM : 0.0;
}

double AnalogSampler::getVariance()
{
    return ((_n > 1) ? _newS/(_n - 1) : 0.0);
}

double AnalogSampler::getStandardDeviation()
{
    return sqrt(getVariance());
}
