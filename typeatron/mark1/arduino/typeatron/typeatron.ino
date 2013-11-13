/*
 * Monomanual Typeatron breadboard prototyping sketch
 * 
 * Connect the push button switches to pins 2-6 in a pulldown configuration
 * Connect the piezo buzzer to pin 7 and GND
 */

#include <MeetAndroid.h>

MeetAndroid meetAndroid;

const int keyPin1 = 2;
const int keyPin2 = 3;
const int keyPin3 = 4;
const int keyPin4 = 5;
const int keyPin5 = 6;

const int speakerPin = 7;  
const int ledPin =  13;

void setup() {
    pinMode(keyPin1, INPUT);     
    pinMode(keyPin2, INPUT);     
    pinMode(keyPin3, INPUT);     
    pinMode(keyPin4, INPUT);     
    pinMode(keyPin5, INPUT);     

    pinMode(speakerPin, OUTPUT); 
    pinMode(ledPin, OUTPUT);
  
    // BlueSMiRF Silver is compatible with any baud rate from 2400-115200
    // Note: the Amarino receiver appears to be compatible with a variety baud rates, as well
    Serial.begin(115200);  
}

unsigned int lastInputState = 0;

char print_str[100];

// TODO: tailor the bounce interval to the switch being used.
// This 2ms value is a conservative estimate based on an average over many kinds of switches.
// See "A Guide to Debouncing" by Jack G. Ganssle
unsigned int debounceMicros = 2000;

void loop() {  
    unsigned int input[5];
    unsigned inputState = 0;
      
    input[0] = digitalRead(keyPin1);
    input[1] = digitalRead(keyPin2);
    input[2] = digitalRead(keyPin3);
    input[3] = digitalRead(keyPin4);
    input[4] = digitalRead(keyPin5);
  
    for (int i = 0; i < 5; i++) {
      inputState |= input[i] << i;  
    }
    
    // bells and whistles
    if (input[0] == HIGH) {     
        digitalWrite(ledPin, HIGH); 
        tone(speakerPin, 440); 
    } else {
        digitalWrite(ledPin, LOW);
        noTone(speakerPin);
    }
    
    if (inputState != lastInputState) {
        unsigned int before = micros();
        
        sprintf(print_str, "/exo/tt/keys %c%c%c%c%c",
            input[0] + 48, input[1] + 48, input[2] + 48, input[3] + 48, input[4] + 48, input[5] + 48);

        //Serial.println(print_str);    

        meetAndroid.receive(); meetAndroid.send(print_str);

        unsigned int after = micros();
        
        if (after - before < debounceMicros) {
            delayMicroseconds(debounceMicros - (after - before));
        }
        
        /*
        Serial.print("/exo/tt/keys ");
        for (int i = 0; i < 5; i++) {
            Serial.print(input[i] ? "1" : "0");
        } 
        Serial.println("");
        */
    }
  
    lastInputState = inputState;
}
