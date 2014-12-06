/*
  ExtendoDevice.cpp
  Created by Joshua Shinavier, 2014
  Released into the public domain.
*/

#include "ExtendoDevice.h"
#include <RGBLED.h>

#define OSC_CONTEXT_SET  "/context/set"
#define OSC_ERROR        "/error"
#define OSC_HEARTBEAT    "/heartbeat"
#define OSC_INFO         "/info"
#define OSC_MORSE        "/morse"
#define OSC_OK           "/ok"
#define OSC_PING         "/ping"
#define OSC_PING_REPLY   "/ping/reply"
#define OSC_READY        "/ready"
#define OSC_RGB_SET      "/rgb/set"
#define OSC_TONE         "/tone"
#define OSC_VIBRO        "/vibro"
#define OSC_WARNING      "/warning"

// allows OSC to dispatch messages to non-member functions which call member functions
ExtendoDevice *instance;

ExtendoDevice::ExtendoDevice(const char *oscPrefix): osc(oscPrefix) {
    instance = this;
    ledCueLength = 0;
    ledCueSince = 0;
}

const char* ExtendoDevice::getContext() {
    return contextName;
}

void ExtendoDevice::setContext(const char *context) {
    strcpy(contextName, context);
}


////////////////////////////////////////////////////////////////////////////////
// setup

void ExtendoDevice::setup() {
    setupPins();

    droidspeak = createDroidspeak();
    droidspeak->speakPowerUpPhrase();

    setColor(RGB_YELLOW);

    osc.beginSerial();

    bundleIn = new OSCBundle();

    setupOther();

    // delay the serial open phrase until the random number generator has been seeded
    setColor(RGB_GREEN);
    droidspeak->speakSerialOpenPhrase();

    setColor(RGB_BLACK);

    vibrate(500);
    osc.sendInfo("%s is ready", osc.getPrefix());

    loopCount = 0;
    loopThreshold = 100;
    // initial value for loop time gets us through the first half second or so
    loopTime = 4.0/1000;
    loopStartTime = millis();
}


////////////////////////////////////////////////////////////////////////////////
// looping

void ExtendoDevice::setLoopTimeHandler(void (*handler)(double)) {
    loopTimeHandler = handler;
}

double ExtendoDevice::getLoopTime() {
    return loopTime;
}

unsigned long ExtendoDevice::beginLoop() {
    if (inputEnabled) {
        if (osc.receiveOSCBundle(*bundleIn)) {
            handleOSCBundleInternal(*bundleIn);
            bundleIn->empty();
            delete bundleIn;
            bundleIn = new OSCBundle();
        }
    }

    // no need for micros() here, and it's simpler to avoid overflow
    unsigned long now = millis();

    checkLedStatus(now);

// TODO: never applies
#ifdef HEARTBEAT_MS
    if (now - lastHeartbeat > HEARTBEAT_MS) {
        sendHeartbeatMessage(now);
        lastHeartbeat = now;
    }
#endif // HEARTBEAT_MS

    // periodically adjust the loop time used in band-pass filtering (among potentially other applications)
    if (++loopCount >= loopThreshold) {
        loopTime = (now - loopStartTime)/1000.0/loopThreshold;

        handleLoopTime();

        if (loopTimeHandler) {
            loopTimeHandler(loopTime);
        }

        loopCount = 0;
        loopStartTime = now;
    }

    return now;
}


////////////////////////////////////////////////////////////////////////////////
// cues

void ExtendoDevice::checkLedStatus(unsigned long now) {
    if (ledCueSince > 0 && now - ledCueSince > ledCueLength) {
        setColor(RGB_BLACK);
        ledCueSince = 0;
    }
}

void ExtendoDevice::errorCue() {
    setColor(RGB_RED);
    ledCueLength = errorCueVisualDurationMs;
    ledCueSince = millis();
}

void ExtendoDevice::infoCue() {
    setColor(RGB_BLUE);
    ledCueLength = infoCueVisualDurationMs;
    ledCueSince = millis();
}

void ExtendoDevice::okCue() {
    setColor(RGB_GREEN);
    ledCueLength = okCueVisualDurationMs;
    ledCueSince = millis();
}

void ExtendoDevice::readyCue() {
    setColor(RGB_WHITE);
    ledCueLength = readyCueVisualDurationMs;
    ledCueSince = millis();
}

void ExtendoDevice::warningCue() {
    setColor(RGB_YELLOW);
    ledCueLength = warningCueVisualDurationMs;
    ledCueSince = millis();
}


////////////////////////////////////////////////////////////////////////////////
// OSC

void ExtendoDevice::enableInput(boolean b) {
    inputEnabled = b;
}

ExtendOSC *ExtendoDevice::getOSC() {
    return &osc;
}

const char *ExtendoDevice::address(const char *suffix) {
    // note: redundant writing of osc.getPrefix() to the buffer
    sprintf(oscAddressBuffer, "%s%s", osc.getPrefix(), suffix);
    return oscAddressBuffer;
}

void ExtendoDevice::sendPingReply() {
    OSCMessage m(address(OSC_PING_REPLY));
    m.add((uint64_t) micros());

    osc.sendOSC(m);
}

// TODO: never applies
#ifdef HEARTBEAT_MS
void ExtendoDevice::sendHeartbeatMessage(unsigned long now) {
    OSCMessage m(address(OSC_HEARTBEAT));
    m.add((uint64_t) now);
    osc.sendOSC(m);
}
#endif


////////////////////////////////////////////////////////////////////////////////
// non-member OSC handler functions

void handleContextSetMessage(class OSCMessage &m) {
    if (!instance->getOSC()->validArgs(m, 1)) return;

    char buffer[32];
    m.getString(0, buffer, m.getDataLength(0) + 1);
    instance->setContext(buffer);
}

void handleErrorMessage(class OSCMessage &m) {
    instance->errorCue();
    instance->vibrate(errorCueHapticDurationMs);
}

void handleInfoMessage(class OSCMessage &m) {
    instance->infoCue();
    instance->vibrate(infoCueHapticDurationMs);
}

/*
void ExtendoDevice::handleMorseMessage(class OSCMessage &m) {
    if (!osc.validArgs(m, 1)) return;

    int length = m.getDataLength(0);
    char buffer[length+1];
    m.getString(0, buffer, length+1);

    morse.playMorseString(buffer);
}
*/

void handleOkMessage(class OSCMessage &m) {
    instance->okCue();
}

void handlePingMessage(class OSCMessage &m) {
    instance->sendPingReply();
}

void handleReadyMessage(class OSCMessage &m) {
    instance->readyCue();
}

void handleRGBSetMessage(class OSCMessage &m) {
    if (!instance->getOSC()->validArgs(m, 1)) return;

    int32_t color = m.getInt(0);

    if (color < 0 || color > 0xffffff) {
        instance->getOSC()->sendError("color out of range: %d", (long) color);
    } else {
        instance->setColor(color);
    }
}

void handleToneMessage(class OSCMessage &m) {
    if (!instance->getOSC()->validArgs(m, 2)) return;

    int32_t frequency = m.getInt(0);
    int32_t duration = m.getInt(1);

    if (frequency <= 0 || frequency > 20000) {
        instance->getOSC()->sendError("frequency out of range: %d", (int) frequency);
    } else if (duration <= 0) {
        instance->getOSC()->sendError("duration must be a positive number");
    } else if (duration > 60000) {
        instance->getOSC()->sendError("duration too long");
    } else {
        instance->playTone((unsigned int) frequency, (unsigned long) duration);
    }
}

void handleVibroMessage(class OSCMessage &m) {
    if (!instance->getOSC()->validArgs(m, 1)) return;

    int32_t d = m.getInt(0);

    instance->vibrate((unsigned long) d);

    if (d <= 0) {
        instance->getOSC()->sendError("duration must be a positive number");
    } else if (d > 60000) {
        instance->getOSC()->sendError("duration too long");
    } else {
        instance->vibrate((unsigned long) d);
    }
}

void handleWarningMessage(class OSCMessage &m) {
    instance->warningCue();
    instance->vibrate(warningCueHapticDurationMs);
}


////////////////////////////////////////////////////////////////////////////////

void ExtendoDevice::handleOSCBundleInternal(class OSCBundle &bundle) {
    if (bundle.hasError()) {
        osc.sendOSCBundleError(bundle);
    } else if (!(handleOSCBundle(bundle)
        // TODO: copying addresses into buffers on the fly (via address()), one by one, is inefficient
        || bundle.dispatch(address(OSC_CONTEXT_SET), handleContextSetMessage)
        || bundle.dispatch(address(OSC_ERROR), handleErrorMessage)
        || bundle.dispatch(address(OSC_INFO), handleInfoMessage)
        //|| bundle.dispatch(address(OSC_MORSE), handleMorseMessage)
        || bundle.dispatch(address(OSC_OK), handleOkMessage)
        || bundle.dispatch(address(OSC_PING), handlePingMessage)
        || bundle.dispatch(address(OSC_RGB_SET), handleRGBSetMessage)
        || bundle.dispatch(address(OSC_READY), handleReadyMessage)
        || bundle.dispatch(address(OSC_TONE), handleToneMessage)
        || bundle.dispatch(address(OSC_VIBRO), handleVibroMessage)
        || bundle.dispatch(address(OSC_WARNING), handleWarningMessage)
        )) {
        osc.sendError("no messages dispatched");
    }
}


