/*
  ExtendoHand.h - Arduino library for the Extend-o-Hand gestural glove
  See: http://github.com/joshsh/extendo
  
  Created by Joshua Shinavier, 2012-2014
  Released into the public domain.
*/

#ifndef ExtendoHand_h
#define ExtendoHand_h

#include <ExtendoDevice.h>

#define OSC_EXO_HAND     "/exo/hand"
#define OSC_GESTURE      "/gesture"
#define OSC_MOTION       "/motion"

class ExtendoHand : public ExtendoDevice
{
  public:
    ExtendoHand();

    void setColor(unsigned long color);
    void vibrate(unsigned long durationMs);
    void playTone(unsigned int frequency, unsigned long durationMs);

    // true by default.  Set to false if using the MMA7361 3-axis accelerometer
    void useNineAxisSensors(boolean b);

    void getAcceleration(double *x, double *y, double *z);
    void getRotation(double *x, double *y, double *z);
    void getHeading(double *x, double *y, double *z);

  protected:
    //Morse *createMorse();
    Droidspeak *createDroidspeak();
    void setupPins();
    void setupOther();
    bool handleOSCBundle(class OSCBundle &bundle);
    void onBeginLoop(unsigned long now);
    void onLoopTimeUpdated(double loopTime);

    boolean nineAxis;
};

#endif // ExtendoHand_h
