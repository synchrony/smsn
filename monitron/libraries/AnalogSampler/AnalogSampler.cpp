#include "AnalogSampler.h"

AnalogSampler::AnalogSampler(uint8_t pin)
{
    _pin = pin;
    reset();
}

void AnalogSampler::sample()
{
    _n++;

    double v = analogRead(_pin) / 1024.0;
    
    if (v < _minValue)
    {
        _minValue = v;
    }
    
    else if (v > _maxValue)
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
    return _sumOfValues / _n;
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

