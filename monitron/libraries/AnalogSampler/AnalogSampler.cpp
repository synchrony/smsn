#include "AnalogSampler.h"

AnalogSampler::AnalogSampler(uint8_t pin)
{
    _pin = pin;
    reset();
}

void AnalogSampler::sample()
{
    unsigned int v = analogRead(_pin);
    
    if (v < _minValue)
    {
        _minValue = v;
    }
    
    else if (v > _maxValue)
    {
        _maxValue = v;
    }
}

void AnalogSampler::reset()
{
    _minValue = 1023;
    _maxValue = 0;
}
    
double AnalogSampler::getMinValue()
{
    return _minValue / 1024.0;
}

double AnalogSampler::getMaxValue()
{
    return _maxValue / 1024.0;
}

