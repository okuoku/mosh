#include <nmosh/plugin-if.h>
#include <pcap.h>

static void*
reterr(const char* errbuf){
    NMOSH_EXPORT_BEGIN(param)
        NMOSH_EXPORT_CSTRING("pcap_error", errbuf)
    NMOSH_EXPORT_END()
    return NMOSH_EXPORT(param);
}

#define MAYBENULL(x) (x?x:"")

static void*
mpcap_findalldevs_itr(void** ctx, const void* bogus){
    const pcap_if_t* pcapif = (const pcap_if_t*)(*ctx);
    if(pcapif){
        NMOSH_EXPORT_BEGIN(param)
            NMOSH_EXPORT_CSTRING(NULL, MAYBENULL(pcapif->name))
            NMOSH_EXPORT_CSTRING(NULL, MAYBENULL(pcapif->description))
            // FIXME: Addr
            NMOSH_EXPORT_INT(NULL, pcapif->flags)
        NMOSH_EXPORT_END()
        *ctx = pcapif->next;
        return NMOSH_EXPORT(param);
    }else{
        return NULL;
    }
}

MOSHEXPORT
void*
mpcap_findalldevs(void){
    void* obj;
    int r;
    pcap_if_t* alldevs;
    char errbuf[PCAP_ERRBUF_SIZE];
    r = pcap_findalldevs(&alldevs, errbuf);
    if(r){ /* Error */
        return reterr(errbuf);
    }else{
        void* cur = alldevs;
        obj = NMOSH_EXPORT_MAP(mpcap_findalldevs_itr, &cur, NULL);
        pcap_freealldevs(alldevs);
        return obj;
    }
}

NMOSH_CONSTANT_BEGIN(mosh_pcap)
NMOSH_CONSTANT_END()
NMOSH_PLUGIN_DEFINE_WITH_CONSTANTS(mosh_pcap);
