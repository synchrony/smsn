#include <stdlib.h>

const unsigned long RGB_WHITE = 0xffffff;
const unsigned long RGB_RED = 0xff0000;
const unsigned long RGB_YELLOW = 0xffff00;
const unsigned long RGB_GREEN = 0x00ff00;
const unsigned long RGB_CYAN = 0x00ffff;
const unsigned long RGB_BLUE = 0x0000ff;
const unsigned long RGB_PURPLE = 0xff00ff;
const unsigned long RGB_BLACK = 0x000000;

// prevents red (which otherwise requires a higher resistance) from dominating
const unsigned int RED_FACTOR = 100;

unsigned long color_stack[10];
int color_stack_index = -1;

////////////////////////////////////////

void writeColor()
{
  //Serial.println("writeColor");
  //tone(SPEAKER_PIN, 440);
  //delay(50);
  //noTone(SPEAKER_PIN);
  //delay(50);
  
  unsigned long color = color_stack[color_stack_index];
  
  unsigned long red = (color & RGB_RED) >> 16;
  unsigned long green = (color & RGB_GREEN) >> 8;
  unsigned long blue = (color & RGB_BLUE);
  
  red = (red * RED_FACTOR) / 255;
  
  analogWrite(RGB_LED_RED_PIN, 255 - (unsigned int) red);
  analogWrite(RGB_LED_GREEN_PIN, 255 - (unsigned int) green);
  analogWrite(RGB_LED_BLUE_PIN, 255 - (unsigned int) blue);
}

void pushColor(unsigned long color)
{
    color_stack_index++;
    color_stack[color_stack_index] = color;  
    writeColor();  
}

void popColor()
{
    color_stack_index--;
    writeColor();
}

void replaceColor(unsigned long color)
{
    popColor();
    pushColor(color);
}

void tmp()
{      
  for (int i=0; i < 10; i++) {
    replaceColor(RGB_WHITE);
    delay(50);
    replaceColor(RGB_BLACK);
    delay(50);
  }
  
  replaceColor(RGB_WHITE);
  delay(1000);
  replaceColor(RGB_RED);
  delay(1000);
  replaceColor(RGB_YELLOW);
  delay(1000);
  replaceColor(RGB_GREEN);
  delay(1000);
  replaceColor(RGB_CYAN);
  delay(1000);
  replaceColor(RGB_BLUE);
  delay(1000);
  replaceColor(RGB_PURPLE);
  delay(1000);
  replaceColor(RGB_BLACK);
  delay(1000);
}

void rgb_led_setup()
{
    pinMode(RGB_LED_RED_PIN, OUTPUT);
    pinMode(RGB_LED_GREEN_PIN, OUTPUT);
    pinMode(RGB_LED_BLUE_PIN, OUTPUT);
    
    pushColor(RGB_BLACK);
    pushColor(RGB_YELLOW);
    //tmp();
    //pushColor(RGB_YELLOW);
    //delay(3000);
}

