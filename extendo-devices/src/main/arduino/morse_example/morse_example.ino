#include <Morse.h>

const int ledPin = 13;

int morseStopTest() {
    // no need to abort
    return 0;
}

Morse morse(ledPin, morseStopTest);

void setup() {
    pinMode(speakerPin, OUTPUT);
}

void loop() {
    morse.playMorseString("foo");
    delay(1000); 
}

