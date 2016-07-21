/*
  VectorFilter.h - a three-dimensional filter
  See: http://github.com/joshsh/smsn

  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#ifndef VectorFilter_h
#define VectorFilter_h

#include <ScalarFilter.h>
#include <Vector3D.h>

class VectorFilter
{
  public:
    VectorFilter(ScalarFilter &xFilter, ScalarFilter &yFilter, ScalarFilter &zFilter);

    Vector3D processNext(unsigned long time, Vector3D &v);

    void updateTimestep(double timestep);

  private:
    ScalarFilter *xFilter, *yFilter, *zFilter;
};

#endif // VectorFilter_h
