/*
  SmSnOsc.cpp
  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#include "SmSn.h"
#include "SmSnOsc.h"


#ifdef USE_SERIAL1
#include <SLIPEncodedSerial.h>
SLIPEncodedSerial SLIPSerial(Serial1);
#else
#ifdef BOARD_HAS_USB_SERIAL
#include <SLIPEncodedUSBSerial.h>
SLIPEncodedUSBSerial SLIPSerial(thisBoardsSerialUSB);
#else
#include <SLIPEncodedSerial.h>
SLIPEncodedSerial SLIPSerial(Serial);
#endif // BOARD_HAS_USB_SERIAL
#endif // USE_SERIAL1


SmSnOsc::SmSnOsc(const char *prefix)
{
    _addressPrefix = prefix;
}

const char *SmSnOsc::getPrefix() {
    return _addressPrefix;
}

void SmSnOsc::beginSerial() {
    // OSCuino: begin SLIPSerial just like Serial
    // set this as high as you can reliably run on your platform
    SLIPSerial.begin(BAUD_RATE);
/*
#if ARDUINO >= 100
    while(!Serial); // for Arduino Leonardo
#endif
*/
#if ARDUINO >= 100
#ifdef USE_SERIAL1
    while(!Serial1);
#else
#ifdef BOARD_HAS_USB_SERIAL
    while(!thisBoardsSerialUSB);
#else
    while(!Serial);
#endif
#endif
#endif
}

int SmSnOsc::receiveOSC(class OSCMessage &messageIn) {
    int done = SLIPSerial.endofPacket();
    int size;
    while ((size = SLIPSerial.available()) > 0) {
        while (size--) {
            int c = SLIPSerial.read();
            messageIn.fill(c);
            //sprintf(errstr, "received a byte: %d. bytes: %d, size: %d, hasError: %d", c, messageIn2.bytes(), messageIn2.size(), messageIn2.hasError());
            //sendInfo(errstr);
        }
    }
    return done || SLIPSerial.endofPacket();
}

int SmSnOsc::receiveOSCBundle(class OSCBundle &bundleIn) {
    int done = SLIPSerial.endofPacket();
    int size;
    while ((size = SLIPSerial.available()) > 0) {
        while (size--) {
            int c = SLIPSerial.read();
            bundleIn.fill(c);
        }
    }
    return done || SLIPSerial.endofPacket();
}

void SmSnOsc::sendOSC(class OSCMessage &messageOut) {
    SLIPSerial.beginPacket();
    messageOut.send(SLIPSerial); // send the bytes to the SLIP stream
    SLIPSerial.endPacket(); // mark the end of the OSC Packet
    messageOut.empty(); // free the space occupied by the message
    SLIPSerial.flush(); // insofar as it is possible, don't proceed until we have completely sent the message
}

void SmSnOsc::sendInfo(const char *msg, ...) {
    va_list argptr;
    va_start(argptr, msg);
    vsprintf(_messageBuffer, msg, argptr);
    va_end(argptr);

    sprintf(_addressBuffer, "%s/info", _addressPrefix);

    OSCMessage m(_addressBuffer);
    m.add(_messageBuffer);
    sendOSC(m);
}

void SmSnOsc::sendError(const char *msg, ...) {
    va_list argptr;
    va_start(argptr, msg);
    vsprintf(_messageBuffer, msg, argptr);
    va_end(argptr);

    sprintf(_addressBuffer, "%s/error", _addressPrefix);

    OSCMessage m(_addressBuffer);
    m.add(_messageBuffer);
    sendOSC(m);
}

// TODO: restore error code strings.  Removing them is temporary, for conserving flash memory on Extend-o-Hand + Arduino Nano
/*
const char *OSC_OK_msg = "OSC_OK",
    *BUFFER_FULL_msg = "BUFFER_FULL",
    *INVALID_OSC_msg = "INVALID_OSC",
    *ALLOCFAILED_msg = "ALLOCFAILED",
    *INDEX_OUT_OF_BOUNDS_msg = "INDEX_OUT_OF_BOUNDS",
    *unknown_msg = "unknown";
*/

void SmSnOsc::sendOSCError(OSCErrorCode code) {
    /*
    const char *name;

    switch(code) {
      case OSC_OK:
          name = OSC_OK_msg;
          break;
      case BUFFER_FULL:
          name = BUFFER_FULL_msg;
          break;
      case INVALID_OSC:
         name = INVALID_OSC_msg;
         break;
      case ALLOCFAILED:
         name = ALLOCFAILED_msg;
         break;
      case INDEX_OUT_OF_BOUNDS:
         name = INDEX_OUT_OF_BOUNDS_msg;
         break;
      default:
         name = unknown_msg;
    }

    sendError("OSC message hasError (error type: %s)", name);
    */
    sendError("OSC message hasError %d", code);
}

void SmSnOsc::sendOSCMessageError(class OSCMessage &message) {
    OSCErrorCode code = message.getError();
    sendOSCError(code);
}

void SmSnOsc::sendOSCBundleError(class OSCBundle &bundle) {
    OSCErrorCode code = bundle.getError();
    sendOSCError(code);
}

int SmSnOsc::validArgs(class OSCMessage &m, int totalExpected) {
    if (totalExpected != m.size()) {
        sendError("got %d argument(s), expected %d", m.size(), totalExpected);
        return 0;
    } else {
        return 1;
    }
}
