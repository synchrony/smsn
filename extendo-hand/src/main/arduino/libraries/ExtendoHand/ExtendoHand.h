/*
  ExtendoHand.h - Arduino library for the Extend-o-Hand gestural glove
  See: http://github.com/joshsh/extendo
  
  Created by Joshua Shinavier, 2012-2015
  Released into the public domain.
*/

#ifndef ExtendoHand_h
#define ExtendoHand_h

#include <ExtendoDevice.h>
#include <Vector3D.h>

#define OSC_EXO_HAND     "/exo/hand"
#define OSC_GESTURE      "/gesture"
#define OSC_MOTION       "/motion"
#define OSC_ALERT        "/alert"

class ExtendoHand : public ExtendoDevice
{
  public:
    ExtendoHand();

    void setColor(unsigned long color);
    void vibrate(unsigned long durationMs);
    void vibrateNonBlocking(unsigned long durationMs);
    void playTone(unsigned int frequency, unsigned long durationMs);

    // analogous to the Typeatron's laser feedback;
    // it causes a brief flicker of the RGB LED in red, as well as a vibration
    void alert();

    // true by default.  Set to false if using the MMA7361 3-axis accelerometer
    void useNineAxisSensors(boolean b);

    void getAcceleration(Vector3D &a);
    void getRotation(Vector3D &g);
    void getHeading(Vector3D &m);

  protected:
    //Morse *createMorse();
    //Droidspeak *createDroidspeak();
    void setupPins();
    void setupOther();
    bool handleOSCBundle(class OSCBundle &bundle);
    void onBeginLoop(unsigned long now);
    void onLoopTimeUpdated(double loopTime);

    boolean nineAxis;

    unsigned long vibrateStart;
    unsigned long vibrateLength;

    unsigned long alertStart;
    bool alertFlickerHigh;
};

#endif // ExtendoHand_h
