#ifndef _DHT22_H_
#define _DHT22_H_

#include <inttypes.h>

#define DHT22_ERROR_VALUE -99.5

typedef enum
{
  DHT_ERROR_NONE = 0,
  DHT_BUS_HUNG,
  DHT_ERROR_NOT_PRESENT,
  DHT_ERROR_ACK_TOO_LONG,
  DHT_ERROR_SYNC_TIMEOUT,
  DHT_ERROR_DATA_TIMEOUT,
  DHT_ERROR_CHECKSUM,
  DHT_ERROR_TOOQUICK
} DHT22_ERROR_t;

class DHT22
{
  private:
    uint8_t _bitmask;
    volatile uint8_t *_baseReg;
    unsigned long _lastReadTime;
    float _lastHumidity;
    float _lastTemperature;

  public:
    DHT22(uint8_t pin);
    DHT22_ERROR_t readData(void);
    float getHumidity();
    float getTemperatureC();
    void clockReset();
};

#endif /*_DHT22_H_*/
