/*
  DirectionFilter.h - a filter applying a criterion of Euclidean distance from a reference vector
  See: http://github.com/joshsh/extendo

  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#ifndef DirectionFilter_h
#define DirectionFilter_h

#include <Vector3D.h>

class DirectionFilter
{
  public:
    DirectionFilter(Vector3D &center, double radius);

    bool process(Vector3D &v);

    // return:
    //   0 -- perfect match; normed vector is exactly the center of the filter
    //   -1 -- fail; normed vector is beyond radius from center
    //   (0, 1] -- normed vector is a corresponding distance from center
    double processFuzzy(Vector3D &v);

  private:
      double refx, refy, refz;
      double radius;
};

#endif // DirectionFilter_h
