/*
  SmSnOsc.h - utility for use with OSC (see opensoundcountrol.org) and Semantic Synchrony
  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#ifndef SmSnOsc_h
#define SmSnOsc_h

#include <OSCMessage.h>
#include <OSCBundle.h>
#include "Arduino.h"

//#define BAUD_RATE 9600
#define BAUD_RATE 115200

class SmSnOsc
{
  public:
    SmSnOsc(const char* prefix);

    void beginSerial();
    const char *getPrefix();

    int receiveOSC(class OSCMessage &messageIn);
    int receiveOSCBundle(class OSCBundle &bundleIn);

    void sendOSC(class OSCMessage &messageOut);
    void sendInfo(const char *msg, ...);
    void sendError(const char *msg, ...);
    void sendOSCMessageError(class OSCMessage &message);
    void sendOSCBundleError(class OSCBundle &bundle);

    int validArgs(class OSCMessage &m, int totalExpected);

  private:
    const char* _addressPrefix;
    char _addressBuffer[32];
    char _messageBuffer[128];

    void sendOSCError(OSCErrorCode code);
};

#endif // SmSnOsc_h
