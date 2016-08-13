/*
  ExtendoHand.h - Arduino library for the Extend-o-Hand gestural glove
  See: http://github.com/joshsh/smsn
  
  Created by Joshua Shinavier, 2012-2016
  Released into the public domain.
*/

#ifndef ExtendoHand_h
#define ExtendoHand_h

#include <ExtendoDevice.h>
#include <Vector3D.h>

#define OSC_EXO_HAND     "/exo/hand"
#define OSC_ALERT        "/alert"
#define OSC_GESTURE      "/gesture"
#define OSC_MOTION       "/motion"
#define OSC_MULTI        "/multi"

class ExtendoHand : public ExtendoDevice
{
  public:
    ExtendoHand();

    void setColor(unsigned long color);
    void vibrate(unsigned long durationMs);
    void vibrateNonBlocking(unsigned long durationMs);
    void playTone(unsigned int frequency, unsigned long durationMs);

    // combination of a tone accompanied by a color (blocking, followed by a short pause)
    // and a haptic cue (non-blocking)
    // If toneDurationMs is 0, no tone will be played, and if vibrateDurationMs is 0, no haptic cue will be produced.
    void multiCue(
        unsigned int toneFrequency, unsigned long toneDurationMs,
        unsigned long color,
        unsigned long vibrateDurationMs);

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
