#include "config.h"
#include <libusb.h>

MOSHEXPORT
int
musb_init(void){
    return libusb_init(NULL);
}
