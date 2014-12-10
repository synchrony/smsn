/*
  BooleanFilter.h - abstract base class of filters for time-series data which produce boolean values
  See: http://github.com/joshsh/extendo

  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#ifndef BooleanFilter_h
#define BooleanFilter_h

class BooleanFilter
{
  public:
    // return: the time of the recognized feature, or 0 for no feature
    virtual unsigned long processNext(unsigned long time, double value) = 0;
};

#endif // BooleanFilter_h
