/*
  Typeatron.h - Arduino library for the Monomanual Typeatron chorded keyer
  See: http://github.com/joshsh/extendo and the Typeatron Mark 1 EAGLE schematic
  
  Created by Joshua Shinavier, 2013-2014
  Released into the public domain.
*/

#ifndef Typeatron_h
#define Typeatron_h

#include <ExtendoDevice.h>
#include <AnalogSampler.h>
#include <RGBLED.h>
#include <Morse.h>

#define OSC_EXO_TT         "/exo/tt"
#define OSC_KEYS           "/keys"
#define OSC_LASER_EVENT    "/laser/event"
#define OSC_LASER_FEEDBACK "/laser/feedback"
#define OSC_LASER_OFF      "/laser/off"
#define OSC_LASER_ON       "/laser/on"
#define OSC_LASER_TRIGGER  "/laser/trigger"
#define OSC_MORSE          "/morse"
#define OSC_PHOTO_DATA     "/photo/data"
#define OSC_PHOTO_GET      "/photo/get"

typedef enum {
    Normal = 0,
    LaserTrigger,
    LaserPointer
} Mode;

class Typeatron : public ExtendoDevice
{
  public:
    Typeatron();

    void setColor(unsigned long color);
    void vibrate(unsigned long durationMs);
    void playTone(unsigned int frequency, unsigned long durationMs);
    Morse *getMorse();
    Droidspeak *getDroidspeak();

    Mode getMode();
    AnalogSampler *getPhotoSampler();

    void updateKeys();
    unsigned int getKeyState();
    int getTotalKeysPressed();

    void laserOn();
    void laserOff();
    // causes an already-on laser to flicker briefly
    void laserFeedback();

    void setMode(int m);

    void sendKeyState();
    void sendLightLevel();
    void sendLaserEvent();

  protected:
    Morse *createMorse();
    Droidspeak *createDroidspeak();
    void setupPins();
    void setupOther();
    bool handleOSCBundle(class OSCBundle &bundle);
    void onBeginLoop(unsigned long now);
    void onLoopTimeUpdated(double loopTime);

  private:
    AnalogSampler *photoSampler;
    Morse *morse;
    Droidspeak *droidspeak;

    RGBLED rgbled;

    Mode mode;

    unsigned int keyState;
    unsigned int keys[5];
    unsigned int totalKeysPressed;

    void resetLaser();

    bool laserModeHigh;
    bool laserFlickerHigh;
    unsigned long laserFlickerStart;
};

#endif // Typeatron_h
