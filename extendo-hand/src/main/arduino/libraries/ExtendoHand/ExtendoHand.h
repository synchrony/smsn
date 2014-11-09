/*
  ExtendoHand.h - Arduino library for the Extend-o-Hand gestural glove
  See: http://github.com/joshsh/extendo
  
  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#ifndef ExtendoHand_h
#define ExtendoHand_h

#include <OSCBundle.h>
#include <ExtendOSC.h>

// OSC addresses
#define EXO_HAND              "/exo/hand"
#define EXO_HAND_AUDIO_TONE   "/exo/hand/audio/tone"
#define EXO_HAND_CONTEXT_SET  "/exo/hand/context/set"
#define EXO_HAND_GESTURE      "/exo/hand/gesture"
#define EXO_HAND_HEARTBEAT    "/exo/hand/heartbeat"
#define EXO_HAND_INFO         "/exo/hand/info"
#define EXO_HAND_MORSE        "/exo/hand/morse"
#define EXO_HAND_MOTION       "/exo/hand/motion"
#define EXO_HAND_PING         "/exo/hand/ping"
#define EXO_HAND_PING_REPLY   "/exo/hand/ping/reply"
#define EXO_HAND_RGB_SET      "/exo/hand/rgb/set"
#define EXO_HAND_VIBRO        "/exo/hand/vibro"

class ExtendoHand
{
  public:
    ExtendoHand();

    void setLoopTimeHandler(void (*handler)(double));

    // if true (the default), listen for incoming OSC messages
    // if false, do not wait for input, permitting a higher sampling and output rate
    void enableInput(boolean b);

    // true by default.  Set to false if using the MMA7361 3-axis accelerometer
    void useNineAxisSensors(boolean b);

    double getLoopTime();

    void setup();
    unsigned long beginLoop();
    void setColor(unsigned long color);
    void vibrate(unsigned long durationMs);
    void playTone(unsigned int frequency, unsigned long durationMs);
    void sendPingReply();

    void getAcceleration(double *x, double *y, double *z);
    void getRotation(double *x, double *y, double *z);
    void getHeading(double *x, double *y, double *z);

    const char* getContext();
    ExtendOSC *getOSC();

  private:
    boolean inputEnabled;
    boolean nineAxis;
    void handleOSCBundle(class OSCBundle &bundle);
    void sendHeartbeatMessage(unsigned long now);
    void updateSamplingRate();

    ExtendOSC osc;
    OSCBundle *bundleIn;

    unsigned long lastHeartbeat;

    // for estimation of sampling rate
    double loopTime;
    int loopCount;
    int loopThreshold;
    unsigned long loopStartTime;
    void (*loopTimeHandler)(double);
};

#endif // ExtendoHand_h
