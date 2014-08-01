/*
  ExtendOSC.cpp
  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#include "ExtendOSC.h"

#ifdef BOARD_HAS_USB_SERIAL
#include <SLIPEncodedUSBSerial.h>
SLIPEncodedUSBSerial SLIPSerial( thisBoardsSerialUSB );
#else
#include <SLIPEncodedSerial.h>
SLIPEncodedSerial SLIPSerial(Serial);
#endif

ExtendOSC::ExtendOSC(const char *prefix)
{
    _addressPrefix = prefix;
}

void ExtendOSC::beginSerial() {
    // OSCuino: begin SLIPSerial just like Serial
    // set this as high as you can reliably run on your platform
    // BlueSMiRF Silver is compatible with any baud rate from 2400-115200
    SLIPSerial.begin(115200);
#if ARDUINO >= 100
    while(!Serial); // Leonardo "feature"
#endif
}

int ExtendOSC::receiveOSC(class OSCMessage &messageIn) {
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

void ExtendOSC::sendOSC(class OSCMessage &messageOut) {
    SLIPSerial.beginPacket();
    messageOut.send(SLIPSerial); // send the bytes to the SLIP stream
    SLIPSerial.endPacket(); // mark the end of the OSC Packet
    messageOut.empty(); // free the space occupied by the message
}

void ExtendOSC::sendInfo(const char *msg, ...)
{
    va_list argptr;
    va_start(argptr, msg);
    vsprintf(_messageBuffer, msg, argptr);
    va_end(argptr);

    sprintf(_addressBuffer, "%s/info", _addressPrefix);

    OSCMessage m(_addressBuffer);
    m.add(_messageBuffer);
    sendOSC(m);
}

void ExtendOSC::sendError(const char *msg, ...)
{
    va_list argptr;
    va_start(argptr, msg);
    vsprintf(_messageBuffer, msg, argptr);
    va_end(argptr);

    sprintf(_addressBuffer, "%s/error", _addressPrefix);

    OSCMessage m(_addressBuffer);
    m.add(_messageBuffer);
    sendOSC(m);
}
