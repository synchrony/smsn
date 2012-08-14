#include <stdlib.h>

const unsigned long WHITE = 0xffffff;
const unsigned long RED = 0xff0000;
const unsigned long YELLOW = 0xffff00;
const unsigned long GREEN = 0x00ff00;
const unsigned long CYAN = 0x00ffff;
const unsigned long BLUE = 0x0000ff;
const unsigned long PURPLE = 0xff00ff;
const unsigned long BLACK = 0x000000;

// prevents red (which otherwise requires a higher resistance) from dominating
const unsigned int RED_FACTOR = 60;

int color_stack[3];
int color_stack_index = 0;

////////////////////////////////////////

void writeColor()
{
  unsigned long color = color_stack[color_stack_index];
  
  unsigned long red = (color & RED) >> 16;
  unsigned long green = (color & GREEN) >> 8;
  unsigned long blue = (color & BLUE);
  
  red = (red * RED_FACTOR) / 255;
  
  analogWrite(RGB_LED_RED_PIN, 255 - (unsigned int) red);
  analogWrite(RGB_LED_GREEN_PIN, 255 - (unsigned int) green);
  analogWrite(RGB_LED_BLUE_PIN, 255 - (unsigned int) blue);
}

void pushColor(unsigned long color)
{
    color_stack[++color_stack_index] = color;  
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

void rgb_led_setup()
{
    pinMode(RGB_LED_RED_PIN, OUTPUT);
    pinMode(RGB_LED_GREEN_PIN, OUTPUT);
    pinMode(RGB_LED_BLUE_PIN, OUTPUT);
}

