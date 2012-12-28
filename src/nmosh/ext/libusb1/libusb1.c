#include "config.h"
#include <nmosh/plugin-if.h>
#include <libusb.h>

static uintptr_t
waitloop(uintptr_t in0, uintptr_t in1, uintptr_t* out0, uintptr_t* out1){
    int r;
    libusb_context* ctx = (libusb_context*)in0;
    r = libusb_handle_events(ctx);
    if(r){ /* Error */
        return 0;
    }
    return 1;
}

/* Request xfer */


/* Device claim/release/set-config (O/S Interface) */
/*   in libusb, we should follow this order:
 *     open => claim I/F => set config => XFER => rel I/F => close */
MOSHEXPORT
int
musb_configuration_set(void* p, int c){
    int r;
    r = libusb_set_configuration((libusb_device_handle)p,c);
    return r;
}
MOSHEXPORT
int
musb_interface_claim(void* p, int ifnum){
    int r;
    r = libusb_claim_interface((libusb_device_handle)p, ifnum);
    return r;
}
MOSHEXPORT
int
musb_interface_release(void* p, int ifnum){
    int r;
    r = libusb_release_interface((libusb_device_handle)p, ifnum);
    return r;
}

/* Device enum/open/close */
MOSHEXPORT
void
musb_device_close(void* p){
    libusb_close((libusb_device_handle*)p);
}

MOSHEXPORT
int
musb_device_open(void* p, void** out_handle){
    libusb_device_handle* h;
    int r;
    r = libusb_open(dev,&h);
    *out_handle = h;
    return r;
}

MOSHEXPORT
int
musb_device_list_query(void* p, int* out_vid, int* out_pid, 
                       int* out_class, int* out_subclass, int* out_protocol,
                       int* out_ep0size,
                       int* out_path/* len = 8 */, int* out_pathlen){
    libusb_device* dev = (libusb_device *)p;
    struct libusb_device_descriptor desc;
    char pathbuf[8];
    int pathlen;
    int busnum;
    int i;
    busnum = libusb_get_bus_number(dev);
    out_path[0] = busnum;
    pathlen = libusb_get_port_path(NULL, dev, pathbuf, 8);
    if(pathlen>0){
        *out_pathlen = pathlen+1; /* +1 for bus id */
        for(i=0;i!=pathlen;i++){
            out_path[i+1] = pathbuf[i];
        }
    }else{
        *out_pathlen = 0;
        return -1;
    }
    i = libusb_get_deivce_descriptor(dev, &desc);
    if(i == 0){
        *out_vid = desc.idVendor;
        *out_pid = desc.idProduct;
        *out_class = desc.bDeviceClass;
        *out_subclass = desc.bDeviceSubClass;
        *out_proto = desc.bDeviceProtocol;
        *out_ep0size = desc.bMacPacketSize0;
    }else{
        return -1;
    }
    return 0;
}

MOSHEXPORT
int /* = err */
musb_device_list_get(void* dest){
    ssize_t count;
    count = libusb_get_device_list(NULL, (libusb_device ***)dest);
    return count;
}
MOSHEXPORT
int
musb_device_list_dispose(void* p){
    libusb_free_device_list((libusb_device **)p,1);
}

/* adm */

MOSHEXPORT
void*
musb_get_waitloop_func(void){
    return &waitloop;
}

MOSHEXPORT
int
musb_init(void){
    return libusb_init(NULL);
}
